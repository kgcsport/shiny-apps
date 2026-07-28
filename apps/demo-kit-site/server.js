import express from 'express';
import { createServer } from 'http';
import { WebSocketServer } from 'ws';
import Database from 'better-sqlite3';
import session from 'express-session';
import SQLiteStore from 'connect-sqlite3';
import crypto from 'crypto';
import { mkdirSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

const PORT        = parseInt(process.env.PORT || '3000');
const DATA_DIR    = process.env.DATA_DIR || join(__dirname, 'data');
const DB_PATH     = process.env.DB_PATH  || join(DATA_DIR, 'demo_kit.sqlite');
const BASE_URL    = (process.env.BASE_URL || `http://localhost:${PORT}`).replace(/\/$/, '');
const ANTHROPIC_MODEL   = process.env.ANTHROPIC_MODEL   || 'claude-sonnet-5';
const GOOGLE_CLIENT_ID  = process.env.GOOGLE_CLIENT_ID  || '';
const GOOGLE_CLIENT_SECRET = process.env.GOOGLE_CLIENT_SECRET || '';
const SESSION_SECRET = process.env.SESSION_SECRET || crypto.randomBytes(32).toString('hex');
const DAILY_LIMIT   = parseInt(process.env.DEMO_KIT_DAILY_LIMIT || '25');
const EDU_ONLY      = (process.env.DEMO_KIT_EDU_ONLY || 'true') !== 'false';

mkdirSync(DATA_DIR, { recursive: true });

// ── Database ──────────────────────────────────────────────────────────────────
const db = new Database(DB_PATH);
db.pragma('journal_mode = WAL');
db.exec(`
  CREATE TABLE IF NOT EXISTS users (
    email      TEXT PRIMARY KEY,
    name       TEXT,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
  );
  CREATE TABLE IF NOT EXISTS rate_limits (
    email TEXT,
    date  TEXT,
    count INTEGER DEFAULT 0,
    PRIMARY KEY (email, date)
  );
  CREATE TABLE IF NOT EXISTS games (
    room_code      TEXT PRIMARY KEY,
    html           TEXT NOT NULL,
    title          TEXT,
    created_by     TEXT,
    is_multiplayer INTEGER DEFAULT 0,
    created_at     TEXT DEFAULT CURRENT_TIMESTAMP
  );
  CREATE TABLE IF NOT EXISTS oauth_states (
    state      TEXT PRIMARY KEY,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
  );
`);

// ── Express ───────────────────────────────────────────────────────────────────
const app = express();
const httpServer = createServer(app);
const SQLiteSessionStore = SQLiteStore(session);

app.set('trust proxy', 1);
app.use(express.json({ limit: '2mb' }));
app.use(express.static(join(__dirname, 'public')));
app.use(session({
  store: new SQLiteSessionStore({ db: 'sessions.sqlite', dir: DATA_DIR }),
  secret: SESSION_SECRET,
  resave: false,
  saveUninitialized: false,
  cookie: { maxAge: 7 * 24 * 60 * 60 * 1000, sameSite: 'lax', secure: BASE_URL.startsWith('https') }
}));

// ── Middleware helpers ────────────────────────────────────────────────────────
function requireAuth(req, res, next) {
  if (req.session?.user) return next();
  if (req.path.startsWith('/api/')) return res.status(401).json({ error: 'Not authenticated' });
  res.redirect('/');
}

function checkRateLimit(email) {
  const today = new Date().toISOString().slice(0, 10);
  const row = db.prepare('SELECT count FROM rate_limits WHERE email=? AND date=?').get(email, today);
  const count = row?.count || 0;
  if (count >= DAILY_LIMIT) throw Object.assign(new Error(`Daily limit of ${DAILY_LIMIT} generations reached.`), { code: 429 });
  db.prepare(`
    INSERT INTO rate_limits(email,date,count) VALUES(?,?,1)
    ON CONFLICT(email,date) DO UPDATE SET count=count+1
  `).run(email, today);
  return DAILY_LIMIT - count - 1;
}

// ── Auth: /api/me, /auth/google, /auth/callback, /auth/logout ────────────────
app.get('/api/me', (req, res) => {
  if (!req.session?.user) return res.status(401).json({ error: 'Not authenticated' });
  const today = new Date().toISOString().slice(0, 10);
  const row = db.prepare('SELECT count FROM rate_limits WHERE email=? AND date=?').get(req.session.user.email, today);
  res.json({ ...req.session.user, usedToday: row?.count || 0, dailyLimit: DAILY_LIMIT });
});

app.get('/auth/google', (req, res) => {
  if (!GOOGLE_CLIENT_ID) return res.status(503).send('Google OAuth not configured (GOOGLE_CLIENT_ID missing).');
  const state = crypto.randomBytes(16).toString('hex');
  db.prepare('INSERT INTO oauth_states(state) VALUES(?)').run(state);
  const params = new URLSearchParams({
    client_id: GOOGLE_CLIENT_ID,
    redirect_uri: `${BASE_URL}/auth/callback`,
    response_type: 'code',
    scope: 'openid email profile',
    state,
    prompt: 'select_account'
  });
  res.redirect(`https://accounts.google.com/o/oauth2/v2/auth?${params}`);
});

app.get('/auth/callback', async (req, res) => {
  const { code, state, error } = req.query;
  if (error) return res.status(400).send(`Google returned error: ${error}`);

  const stateRow = db.prepare('DELETE FROM oauth_states WHERE state=? RETURNING state').get(state);
  if (!stateRow) return res.status(400).send('Invalid or expired OAuth state. <a href="/">Try again</a>');

  try {
    const tokenRes = await fetch('https://oauth2.googleapis.com/token', {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        code, client_id: GOOGLE_CLIENT_ID, client_secret: GOOGLE_CLIENT_SECRET,
        redirect_uri: `${BASE_URL}/auth/callback`, grant_type: 'authorization_code'
      })
    });
    const tokens = await tokenRes.json();
    if (!tokenRes.ok) throw new Error(tokens.error_description || 'Token exchange failed');

    const userRes = await fetch('https://www.googleapis.com/oauth2/v3/userinfo', {
      headers: { Authorization: `Bearer ${tokens.access_token}` }
    });
    const profile = await userRes.json();

    if (EDU_ONLY && !profile.email?.endsWith('.edu')) {
      return res.status(403).send(`
        <html><body style="font-family:system-ui;max-width:480px;margin:80px auto;padding:0 1rem">
          <h2>Access restricted</h2>
          <p>This tool requires a <strong>.edu</strong> email address.<br>
             You signed in as <strong>${profile.email}</strong>.</p>
          <a href="/auth/google" style="display:inline-block;margin-top:1rem;padding:.5rem 1.2rem;background:#2563eb;color:#fff;border-radius:6px;text-decoration:none">Try a different account</a>
        </body></html>
      `);
    }

    db.prepare('INSERT OR REPLACE INTO users(email,name) VALUES(?,?)').run(profile.email, profile.name);
    req.session.user = { email: profile.email, name: profile.name };
    res.redirect('/');
  } catch (e) {
    console.error('OAuth callback error:', e);
    res.status(500).send(`Authentication failed: ${e.message}. <a href="/">Try again</a>`);
  }
});

app.get('/auth/logout', (req, res) => {
  req.session.destroy(() => res.redirect('/'));
});

// ── LLM system prompts ────────────────────────────────────────────────────────
const SYSTEM_SOLO = `You are an expert at building interactive classroom teaching games as standalone, self-contained HTML files.

Generate a complete, single-file HTML game for classroom use.

Rules:
- No external dependencies whatsoever (no CDN links, no remote scripts or fonts)
- All CSS and JS must be inline in the single file
- Mobile-friendly: large tap targets (min 44px), readable font sizes (min 16px body)
- Include clear student-facing instructions within the game UI
- Include a visible Reset button
- Use a clean, pleasant color scheme
- Return ONLY the HTML content, starting with <!DOCTYPE html> and ending with </html>
- No explanation before or after the HTML`;

const GAMESYNC_API_DOC = `MULTIPLAYER STATE SYNC API (GameSync library — pre-injected, do not redefine):

\`\`\`javascript
// Room code and player ID come from the URL / localStorage (set up for you):
const room = window.GAME_ROOM || new URLSearchParams(location.search).get('room') || 'default';
const playerId = localStorage.getItem('_pid') || ('p' + Math.random().toString(36).slice(2,8));
localStorage.setItem('_pid', playerId);

const sync = new GameSync(room, playerId);

// Called whenever game state changes (including your own updates):
sync.onState((state, players) => {
  // state: plain object, each player's data lives under their playerId key
  //        e.g. { alice123: { bid: 5 }, bob456: { bid: 3 } }
  // players: array of currently connected player IDs
  renderGame(state, players);
});

// Submit this player's data:
function submitAction(data) {
  sync.set({ [playerId]: data });  // merges into shared state
}

// Reset the entire game state (instructor):
function resetGame() {
  sync.reset({});
}
\`\`\`

IMPORTANT: The GameSync class is injected into the page before your script runs. Use it directly — do NOT redeclare or import it. Always read the room using window.GAME_ROOM first (the preview host sets this before loading your game).`;

const SYSTEM_MULTI = `You are an expert at building interactive classroom teaching games as standalone HTML files with real-time multiplayer support.

Generate a complete, single-file HTML multiplayer game for classroom use.

Rules:
- Include <script src="/gamesync.js"></script> in your <head> — this is the ONLY external script allowed
- No other external dependencies (no CDN, no remote fonts)
- All other CSS and JS must be inline
- Mobile-friendly: large tap targets (min 44px), readable font sizes (min 16px body)
- Include clear student-facing instructions within the game UI
- Show a live list of connected players
- Include a Reset button (can be hidden behind ?admin=1 in URL)
- State is PUBLIC — all players see everything. Do not use this for private bids or hidden hands
- Return ONLY the HTML content, starting with <!DOCTYPE html> and ending with </html>
- No explanation before or after the HTML

${GAMESYNC_API_DOC}`;

// ── POST /api/chat — streaming Anthropic proxy ────────────────────────────────
app.post('/api/chat', requireAuth, async (req, res) => {
  const { messages, multiplayer } = req.body;
  if (!Array.isArray(messages) || !messages.length)
    return res.status(400).json({ error: 'messages array required' });

  // Key comes from the client on every request — never stored server-side.
  const apiKey = (req.headers['x-api-key'] || '').trim();
  if (!apiKey || !apiKey.startsWith('sk-'))
    return res.status(400).json({ error: 'No Anthropic API key provided. Enter your key in the sidebar.' });

  let remaining;
  try {
    remaining = checkRateLimit(req.session.user.email);
  } catch (e) {
    return res.status(e.code || 429).json({ error: e.message });
  }

  res.setHeader('Content-Type', 'text/event-stream');
  res.setHeader('Cache-Control', 'no-cache');
  res.setHeader('Connection', 'keep-alive');
  res.setHeader('X-Remaining-Today', remaining);

  try {
    const upstream = await fetch('https://api.anthropic.com/v1/messages', {
      method: 'POST',
      headers: {
        'x-api-key': apiKey,
        'anthropic-version': '2023-06-01',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        model: ANTHROPIC_MODEL,
        max_tokens: 8192,
        stream: true,
        system: multiplayer ? SYSTEM_MULTI : SYSTEM_SOLO,
        messages
      })
    });

    if (!upstream.ok) {
      const err = await upstream.text();
      res.write(`data: ${JSON.stringify({ type: 'error', error: err })}\n\n`);
      return res.end();
    }

    // Pass Anthropic SSE stream through to client
    for await (const chunk of upstream.body) {
      if (res.writableEnded) break;
      res.write(chunk);
    }
  } catch (e) {
    if (!res.writableEnded) {
      res.write(`data: ${JSON.stringify({ type: 'error', error: e.message })}\n\n`);
    }
  }
  res.end();
});

// ── POST /api/game — save a game, get a room code ────────────────────────────
app.post('/api/game', requireAuth, (req, res) => {
  const { html, title, is_multiplayer } = req.body;
  if (!html) return res.status(400).json({ error: 'html required' });

  const code = crypto.randomBytes(3).toString('hex').toUpperCase();

  db.prepare(`
    INSERT OR REPLACE INTO games(room_code, html, title, created_by, is_multiplayer)
    VALUES(?,?,?,?,?)
  `).run(code, html, title || 'Untitled Game', req.session.user.email, is_multiplayer ? 1 : 0);

  res.json({ room_code: code, url: `${BASE_URL}/play?room=${code}` });
});

// ── GET /play — serve saved game (auth required) ─────────────────────────────
app.get('/play', requireAuth, (req, res) => {
  const { room } = req.query;
  if (!room) return res.status(400).send('room parameter required');

  const game = db.prepare('SELECT * FROM games WHERE room_code=?').get(room);
  if (!game) return res.status(404).send(`Game room "${room}" not found.`);

  res.setHeader('Content-Type', 'text/html; charset=utf-8');
  res.send(game.html);
});

// ── GET /health ───────────────────────────────────────────────────────────────
app.get('/health', (_, res) => res.json({ ok: true }));

// ── WebSocket: shared game state ──────────────────────────────────────────────
// Protocol:
//   client→server: {type:'join', player:'id'} | {type:'set', player:'id', data:{...}} | {type:'reset', state:{...}}
//   server→client: {type:'state', state:{...}, players:['id',...]}

const wss = new WebSocketServer({ server: httpServer, path: '/ws' });
const rooms = new Map(); // roomCode → { state:{}, players: Map<playerId, ws> }

wss.on('connection', (ws, req) => {
  const params = new URLSearchParams((req.url || '').split('?')[1] || '');
  const roomCode = params.get('room') || 'default';

  if (!rooms.has(roomCode)) rooms.set(roomCode, { state: {}, players: new Map() });
  const room = rooms.get(roomCode);
  let myId = null;

  function broadcast() {
    const msg = JSON.stringify({
      type: 'state',
      state: room.state,
      players: [...room.players.keys()]
    });
    for (const client of room.players.values()) {
      if (client.readyState === 1) client.send(msg);
    }
  }

  ws.on('message', raw => {
    let msg;
    try { msg = JSON.parse(raw.toString()); } catch { return; }

    if (msg.type === 'join') {
      myId = String(msg.player || 'anon').slice(0, 64);
      room.players.set(myId, ws);
      // Send current state to the joining player
      ws.send(JSON.stringify({ type: 'state', state: room.state, players: [...room.players.keys()] }));
      broadcast();

    } else if (msg.type === 'set' && msg.data && typeof msg.data === 'object') {
      // Shallow merge: each top-level key is player-owned
      Object.assign(room.state, msg.data);
      broadcast();

    } else if (msg.type === 'reset') {
      room.state = (msg.state && typeof msg.state === 'object') ? msg.state : {};
      broadcast();
    }
  });

  ws.on('close', () => {
    if (myId) {
      room.players.delete(myId);
      if (room.players.size === 0) {
        rooms.delete(roomCode);
      } else {
        broadcast();
      }
    }
  });

  ws.on('error', () => {}); // suppress uncaught errors per ws docs
});

// ── Housekeeping ──────────────────────────────────────────────────────────────
setInterval(() => {
  db.prepare("DELETE FROM oauth_states WHERE created_at < datetime('now','-1 hour')").run();
}, 60 * 60 * 1000);

httpServer.listen(PORT, () => {
  console.log(`Demo Kit running on port ${PORT}`);
  console.log(`  BASE_URL   : ${BASE_URL}`);
  console.log(`  EDU_ONLY   : ${EDU_ONLY}`);
  console.log(`  Model      : ${ANTHROPIC_MODEL}`);
  console.log(`  API key    : user-supplied per request`);
  console.log(`  Google     : ${GOOGLE_CLIENT_ID ? '✓ configured' : '✗ not configured'}`);
});
