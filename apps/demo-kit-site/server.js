import express from 'express';
import { createServer } from 'http';
import { WebSocketServer } from 'ws';
import Database from 'better-sqlite3';
import session from 'express-session';
import crypto from 'crypto';
import { mkdirSync, readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import os from 'os';

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
const AUTH_DISABLED = process.env.DEMO_KIT_AUTH_DISABLED === 'true';
const DEMO_USER     = process.env.DEMO_KIT_DEMO_USER || '';
const DEMO_PASS     = process.env.DEMO_KIT_DEMO_PASS || '';
const SHINY_BASE_URL  = (process.env.SHINY_BASE_URL || '').replace(/\/$/, '');
const SHINY_DB_PATH   = process.env.SHINY_DB_PATH || join(DATA_DIR, 'data', 'class-job-market.sqlite');
const DEMO_OPENAI_KEY    = process.env.DEMO_KIT_OPENAI_KEY  || '';
const ADMIN_EMAIL        = process.env.DEMO_KIT_ADMIN_EMAIL || 'kcoombs@vassar.edu';
const SHINY_DEMO_STUDENT = 'demo-student@classroom.demo';
const SHINY_DEMO_TEACHER = 'demo-teacher@classroom.demo';

mkdirSync(DATA_DIR, { recursive: true });

// ── Pre-built game templates ──────────────────────────────────────────────────
const TEMPLATE_META = [
  {
    id:          'tax-incidence',
    title:       'Tax Incidence Explorer',
    description: 'Interactive supply & demand chart showing how a per-unit tax is split between buyers and sellers. Adjust demand and supply slope sliders — see consumer vs. producer burden shift in real time.',
    is_multiplayer: 0,
    source:      'tax-incidence',
  },
  {
    id:          'airplanes-game',
    title:       'Airplane Production Experiment',
    description: 'Live data-entry tool for the in-class airplane-folding production experiment. Edit a table of firms, worker counts, and output during class — a scatter plot updates automatically.',
    is_multiplayer: 0,
    source:      'airplanes-game',
  },
  {
    id:          'sloman-trading-game',
    title:       'Sloman Trading Game',
    description: 'Multi-round trading game for up to 8 teams producing three goods (Square, Triangle, Rectangle). Prices fall as total output rises. Includes a countdown timer, live leaderboard, and price-trend chart.',
    is_multiplayer: 0,
    source:      'sloman-trading-game',
  },
  {
    id:          'theory-of-firm',
    title:       'Theory of the Firm',
    description: 'Interactive cost-curve diagram with adjustable demand (A, B) and cubic MC polynomial parameters. Finds the profit-maximizing Q*, shows MC/MR/ATC/AVC/AFC curves, and a TR vs. TC profit rectangle.',
    is_multiplayer: 0,
    source:      'theory-of-firm',
  },
  {
    id:          'restricted-seller',
    title:       'The Restricted Seller',
    description: 'Phased classroom experiment contrasting competitive market (Round 1) with monopoly (Round 2). Configurable WTP list generates a step-function demand curve; shows CS, PS, and deadweight loss.',
    is_multiplayer: 0,
    source:      'restricted-seller',
  },
  {
    id:          'indifference-to-demand',
    title:       'Indifference Curves & Demand Derivation',
    description: 'Plots indifference curves + budget constraint for four utility types (Cobb-Douglas, Perfect Substitutes, Perfect Complements, Quasilinear), then derives the demand curve by sweeping the price of good X.',
    is_multiplayer: 0,
    source:      'indifference-to-demand',
  },
];

const TEMPLATES = TEMPLATE_META; // HTML loaded on demand — see /api/template/:id

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
  CREATE TABLE IF NOT EXISTS sessions (
    sid     TEXT PRIMARY KEY,
    sess    TEXT NOT NULL,
    expired TEXT
  );
  CREATE TABLE IF NOT EXISTS hidden_templates (
    template_id TEXT PRIMARY KEY,
    hidden_at   TEXT DEFAULT CURRENT_TIMESTAMP
  );
`);

// ── Inline SQLite session store (uses the same better-sqlite3 db) ─────────────
function makeSQLiteStore(Store) {
  return class SQLiteSessionStore extends Store {
    get(sid, cb) {
      try {
        const row = db.prepare('SELECT sess, expired FROM sessions WHERE sid=?').get(sid);
        if (!row) return cb(null, null);
        if (row.expired && new Date(row.expired) < new Date()) {
          this.destroy(sid, () => {});
          return cb(null, null);
        }
        cb(null, JSON.parse(row.sess));
      } catch (e) { cb(e); }
    }
    set(sid, sess, cb) {
      try {
        const exp = sess?.cookie?.expires ? new Date(sess.cookie.expires).toISOString() : null;
        db.prepare('INSERT OR REPLACE INTO sessions(sid,sess,expired) VALUES(?,?,?)').run(sid, JSON.stringify(sess), exp);
        cb(null);
      } catch (e) { cb(e); }
    }
    destroy(sid, cb) {
      try { db.prepare('DELETE FROM sessions WHERE sid=?').run(sid); cb(null); } catch (e) { cb(e); }
    }
    touch(sid, sess, cb) { this.set(sid, sess, cb); }
  };
}

// ── Express ───────────────────────────────────────────────────────────────────
const app = express();
const httpServer = createServer(app);

app.set('trust proxy', 1);
app.use(express.json({ limit: '2mb' }));
app.use(express.static(join(__dirname, 'public')));
app.use(session({
  store: new (makeSQLiteStore(session.Store))(),
  secret: SESSION_SECRET,
  resave: false,
  saveUninitialized: false,
  cookie: { maxAge: 7 * 24 * 60 * 60 * 1000, sameSite: 'lax', secure: BASE_URL.startsWith('https') }
}));

// ── Overnight maintenance window (10 PM – 8 AM ET) ───────────────────────────
const MAINTENANCE_HTML = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Site offline overnight</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:system-ui,sans-serif;background:#f0f2f6;color:#1a1f2e;
  display:flex;align-items:center;justify-content:center;min-height:100vh;padding:2rem}
.card{background:#fff;border:1px solid #e2e6ee;border-radius:12px;
  padding:2.5rem 2rem;max-width:420px;width:100%;text-align:center;
  box-shadow:0 1px 3px rgba(0,0,0,.08)}
h1{font-size:1.2rem;font-weight:700;margin-bottom:.75rem}
p{color:#6b7280;font-size:.92rem;line-height:1.7}
</style>
</head>
<body>
<div class="card">
  <h1>🌙 Site offline overnight</h1>
  <p>This site is offline between <strong>10 PM and 8 AM ET</strong> to refresh and save on server costs.<br><br>
     Come back in the morning — everything will be ready to go.</p>
</div>
</body>
</html>`;

app.use((req, res, next) => {
  if (req.path === '/health') return next(); // let monitors through
  const h = parseInt(new Date().toLocaleString('en-US', { timeZone: 'America/New_York', hour: 'numeric', hour12: false }));
  if (h >= 22 || h < 8) return res.status(503).send(MAINTENANCE_HTML);
  next();
});

// ── Middleware helpers ────────────────────────────────────────────────────────
function requireAuth(req, res, next) {
  if (AUTH_DISABLED || req.session?.user) return next();
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
function isDemoUser(email) {
  return email?.endsWith('@demo.local') || false;
}

function getLocalIps() {
  const ips = [];
  for (const ifaces of Object.values(os.networkInterfaces())) {
    for (const iface of ifaces) {
      if (iface.family === 'IPv4' && !iface.internal) ips.push(iface.address);
    }
  }
  return ips;
}

app.get('/api/me', (req, res) => {
  const user = AUTH_DISABLED
    ? { email: 'dev@localhost', name: 'Dev (auth disabled)' }
    : req.session?.user;
  if (!user) return res.status(401).json({ error: 'Not authenticated' });
  const today = new Date().toISOString().slice(0, 10);
  const row = db.prepare('SELECT count FROM rate_limits WHERE email=? AND date=?').get(user.email, today);
  const localIps = AUTH_DISABLED ? getLocalIps() : [];
  const isStrictAdmin = user.email === ADMIN_EMAIL;
  res.json({ ...user, usedToday: row?.count || 0, dailyLimit: DAILY_LIMIT, isDemo: isDemoUser(user.email), hasServerKey: !!DEMO_OPENAI_KEY, isAdmin: AUTH_DISABLED || isStrictAdmin, isStrictAdmin, localIps });
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
    req.session.save(err => {
      if (err) { console.error('Session save error:', err); return res.status(500).send('Session error. <a href="/">Try again</a>'); }
      res.redirect('/');
    });
  } catch (e) {
    console.error('OAuth callback error:', e);
    res.status(500).send(`Authentication failed: ${e.message}. <a href="/">Try again</a>`);
  }
});

app.post('/auth/demo-login', (req, res) => {
  if (!DEMO_USER || !DEMO_PASS) return res.status(404).json({ error: 'Not found' });
  const { username, password } = req.body;
  if (username !== DEMO_USER || password !== DEMO_PASS)
    return res.status(401).json({ error: 'Invalid credentials' });
  req.session.user = { email: `${DEMO_USER}@demo.local`, name: 'Demo Teacher' };
  req.session.save(err => {
    if (err) return res.status(500).json({ error: 'Session error' });
    res.json({ ok: true });
  });
});

app.get('/auth/logout', (req, res) => {
  req.session.destroy(() => res.redirect('/'));
});

// ── Shiny OAuth relay: /shiny-auth/login + /shiny-auth/callback ───────────────
// nginx routes shiny.kylecoombs.com/auth/* → demo-kit:3000/shiny-auth/*
// so the session cookie appears to the browser as the shiny domain.
app.get('/shiny-auth/login', (req, res) => {
  if (!GOOGLE_CLIENT_ID || !SHINY_BASE_URL) {
    return res.status(503).send('Google OAuth or SHINY_BASE_URL not configured.');
  }
  const state = crypto.randomBytes(16).toString('hex');
  db.prepare('INSERT INTO oauth_states(state) VALUES(?)').run(state);
  const params = new URLSearchParams({
    client_id: GOOGLE_CLIENT_ID,
    redirect_uri: `${SHINY_BASE_URL}/auth/callback`,
    response_type: 'code',
    scope: 'openid email profile',
    state,
    prompt: 'select_account'
  });
  res.redirect(`https://accounts.google.com/o/oauth2/v2/auth?${params}`);
});

app.get('/shiny-auth/callback', async (req, res) => {
  const { code, state, error } = req.query;
  if (error) return res.status(400).send(`Google returned error: ${error}`);

  const stateRow = db.prepare('DELETE FROM oauth_states WHERE state=? RETURNING state').get(state);
  if (!stateRow) return res.status(400).send('Invalid or expired OAuth state. <a href="/auth/login">Try again</a>');

  try {
    const tokenRes = await fetch('https://oauth2.googleapis.com/token', {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        code, client_id: GOOGLE_CLIENT_ID, client_secret: GOOGLE_CLIENT_SECRET,
        redirect_uri: `${SHINY_BASE_URL}/auth/callback`, grant_type: 'authorization_code'
      })
    });
    const tokens = await tokenRes.json();
    if (!tokenRes.ok) throw new Error(tokens.error_description || 'Token exchange failed');

    const userRes = await fetch('https://www.googleapis.com/oauth2/v3/userinfo', {
      headers: { Authorization: `Bearer ${tokens.access_token}` }
    });
    const profile = await userRes.json();

    // Check this email is enrolled in class-job-market
    let shinyDb;
    try {
      shinyDb = new Database(SHINY_DB_PATH, { readonly: true });
    } catch (e) {
      console.error('Cannot open Shiny DB:', e.message);
      return res.status(500).send('Authentication unavailable (cannot read course database).');
    }
    const enrolled = shinyDb.prepare('SELECT user_id FROM users WHERE user_id=?').get(profile.email);
    shinyDb.close();
    if (!enrolled) {
      return res.status(403).send(`
        <html><body style="font-family:system-ui;max-width:480px;margin:80px auto;padding:0 1rem">
          <h2>Access restricted</h2>
          <p>Your account (<strong>${profile.email}</strong>) is not enrolled in this course.</p>
          <a href="/auth/login" style="display:inline-block;margin-top:1rem;padding:.5rem 1.2rem;background:#2563eb;color:#fff;border-radius:6px;text-decoration:none">Try a different account</a>
        </body></html>
      `);
    }

    // Create arcade_session token matching R's make_token() — 48-char alphanumeric
    const chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    const token = Array.from(crypto.randomBytes(48), b => chars[b % chars.length]).join('');
    const expiresAt = new Date(Date.now() + 14 * 24 * 60 * 60 * 1000).toISOString().replace('T', ' ').slice(0, 19);

    let writeDb;
    try {
      writeDb = new Database(SHINY_DB_PATH);
      writeDb.prepare(`
        INSERT OR REPLACE INTO arcade_sessions(token, user_id, expires_at)
        VALUES (?, ?, ?)
      `).run(token, profile.email, expiresAt);
      writeDb.close();
    } catch (e) {
      console.error('Cannot write arcade_session:', e.message);
      return res.status(500).send('Authentication failed (session write error).');
    }

    const cookieExpires = new Date(Date.now() + 14 * 24 * 60 * 60 * 1000).toUTCString();
    // Not httpOnly — Shiny JS reads arcade_token via document.cookie
    res.setHeader('Set-Cookie', `arcade_token=${encodeURIComponent(token)}; Expires=${cookieExpires}; Path=/; SameSite=Lax`);
    res.redirect('/class-job-market/');
  } catch (e) {
    console.error('Shiny OAuth callback error:', e);
    res.status(500).send(`Authentication failed: ${e.message}. <a href="/auth/login">Try again</a>`);
  }
});

// ── GET /shiny-auth/demo-login — instant demo access (no Google OAuth) ────────
// nginx routes shiny.kylecoombs.com/auth/demo-login → demo-kit:3000/shiny-auth/demo-login
// Sets the same arcade_token cookie as the real OAuth callback.
// Requires SHINY_DEMO_STUDENT and SHINY_DEMO_TEACHER env vars AND those emails
// enrolled in the Shiny DB (class-job-market.sqlite users table).
app.get('/shiny-auth/demo-login', (req, res) => {
  if (!SHINY_BASE_URL) return res.status(503).send('Shiny integration not configured.');
  const role  = req.query.role;
  const email = role === 'teacher' ? SHINY_DEMO_TEACHER
              : role === 'student' ? SHINY_DEMO_STUDENT
              : '';
  if (!email) return res.status(400).send('Invalid role. Use ?role=student or ?role=teacher.');

  const chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  const token = Array.from(crypto.randomBytes(48), b => chars[b % chars.length]).join('');
  const expiresAt = new Date(Date.now() + 4 * 60 * 60 * 1000).toISOString().replace('T', ' ').slice(0, 19);

  let writeDb;
  try {
    writeDb = new Database(SHINY_DB_PATH);
    // Auto-create the demo user if not already in the roster
    writeDb.prepare('INSERT OR IGNORE INTO users(user_id) VALUES(?)').run(email);
    writeDb.prepare('INSERT OR REPLACE INTO arcade_sessions(token, user_id, expires_at) VALUES (?, ?, ?)')
      .run(token, email, expiresAt);
    writeDb.close();
  } catch (e) {
    console.error('Demo login session write error:', e);
    return res.status(500).send('Session creation failed — the Shiny database may not be reachable.');
  }

  const cookieExpires = new Date(Date.now() + 4 * 60 * 60 * 1000).toUTCString();
  res.setHeader('Set-Cookie', `arcade_token=${encodeURIComponent(token)}; Expires=${cookieExpires}; Path=/; SameSite=Lax`);
  res.redirect('/class-job-market/');
});

// ── LLM system prompts ────────────────────────────────────────────────────────
const SYSTEM_SOLO = `You are an expert educational game designer who builds interactive teaching tools as standalone, self-contained HTML files for economics and social science classrooms.

When given a concept, scenario, or request, always generate a complete, working HTML game or demo immediately — never ask clarifying questions or describe what you are about to build. If the request is ambiguous, make a reasonable choice and build it.

Required structure — every output must include ALL of these:
1. <title> tag: a concise 2-5 word game name (used in the browser tab and UI)
2. A visible title or header at the top of the page showing the game name
3. A concept box or brief paragraph (1-3 sentences) explaining what economic principle this demonstrates — pitched at undergraduates, not experts
4. Clear on-screen instructions explaining what to do
5. The interactive game area
6. A Reset button, always visible (never buried in a menu)

Quality criteria:
- Directly illustrate or teach the requested concept through hands-on interaction
- Be suitable for projecting in a classroom or sharing a link with students
- Use a clean, engaging visual design with good color contrast
- Show real-time feedback after each interaction (what changed, what it means)
- If the concept has a theoretical prediction (Nash equilibrium, competitive equilibrium, etc.), show it alongside actual results

Technical rules:
- No external dependencies whatsoever (no CDN links, no remote scripts or fonts)
- All CSS and JS must be inline in the single file
- Mobile-friendly: large tap targets (min 44px), readable font sizes (min 16px body)
- Return ONLY the HTML content, starting with <!DOCTYPE html> and ending with </html>
- No explanation, preamble, or commentary before or after the HTML`;

// ── Structured-output system prompt variants ──────────────────────────────────

// Option 1 — "Tagged": same as SOLO but AI embeds a GAME_META comment in <head>
const SYSTEM_SOLO_METADATA = SYSTEM_SOLO
  + `\n\nMETADATA COMMENT — place this as the very first line inside <head>, with real values, JSON on a single line (escape inner quotes as \\\\"):
<!-- GAME_META {"title":"YOUR_TITLE","concept":"YOUR_CONCEPT_1_TO_2_SENTENCES","theory":"THEORETICAL_PREDICTION_OR_BENCHMARK"} -->`;

// Option 2 — "Framed": AI returns JSON {title, concept, theory, html}; server injects GAME_META and emits assembled HTML
const SYSTEM_SOLO_JSON = `You are an expert educational game designer for economics classrooms.

Return a SINGLE valid JSON object — NO markdown, NO code fences, NO other text. Exactly these keys:

{
  "title":   "2-5 word game name",
  "concept": "1-3 sentences explaining the economic principle for undergraduates",
  "theory":  "The theoretical prediction, Nash equilibrium, or competitive benchmark",
  "html":    "COMPLETE self-contained HTML game (<!DOCTYPE html>...</html>), fully JSON-escaped"
}

Requirements for the "html" field:
- All CSS and JS inline, zero external dependencies
- Mobile-friendly: 44px+ tap targets, 16px+ body text
- Always-visible Reset button
- Show theoretical prediction alongside actual results
- Clean visual design with good color contrast

JSON escaping inside "html": newlines → \\n   double-quotes → \\"   backslashes → \\\\.
Return ONLY the JSON object. Invalid JSON will cause a visible error.`;

// Option 3 — "Template fill": AI fills slots; server assembles a fixed layout skeleton
const SYSTEM_SOLO_SLOTS = `You are an expert educational game designer for economics classrooms.

Return a SINGLE valid JSON object — NO markdown, NO code fences, NO other text. Exactly these keys:

{
  "title":         "2-5 word game name",
  "concept":       "1-3 sentences explaining the economic principle for undergraduates",
  "theory":        "Theoretical prediction or benchmark (shown in a collapsible footer note)",
  "instructions":  "1-3 sentence student-facing instructions",
  "css":           "Additional CSS rules only — no <style> tags. May be empty string.",
  "controls_html": "HTML for student input: buttons, sliders, forms. No <script> tags.",
  "display_html":  "HTML for results/charts/output. No <script> tags.",
  "js":            "JavaScript only — no <script> tags. MUST define: function resetGame() {}"
}

Slot rules:
- Use unique element IDs in controls_html and display_html; reference them in js
- js runs at end of <body>; DOM is ready. Use document.getElementById() for access.
- css may also target #controls-section, #display-section, #app, header
- No external dependencies — use Canvas API, SVG, vanilla JS for charts
- resetGame() must clear all state and reset display to initial condition
- Show theoretical prediction in display_html alongside actual results

Return ONLY the JSON object. Invalid JSON causes a visible error.`;

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

const SYSTEM_MULTI = `You are an expert educational game designer who builds interactive multiplayer teaching tools as standalone HTML files for economics and social science classrooms.

When given a concept, scenario, or request, always generate a complete, working HTML multiplayer game immediately — never ask clarifying questions or describe what you are about to build. If the request is ambiguous, make a reasonable choice and build it.

Required structure — every output must include ALL of these:
1. <title> tag: a concise 2-5 word game name
2. A visible title or header at the top of the page showing the game name
3. A concept box or brief paragraph (1-3 sentences) explaining what economic principle this demonstrates — pitched at undergraduates
4. Clear on-screen instructions explaining what students should do
5. The interactive game area with real-time state visible to all players
6. A live list of connected players
7. A Reset button (can be hidden behind ?admin=1 in URL if appropriate)

Quality criteria:
- Directly illustrate or teach the requested concept through real-time interaction between students
- Be suitable for students joining on their own devices via a shared link
- State is PUBLIC — all players see everything; do not use this for private bids or hidden hands
- Show aggregate results clearly (totals, averages, distributions) as players submit
- If the concept has a theoretical prediction, display it alongside live results

Technical rules:
- Include <script src="/gamesync.js"></script> in your <head> — this is the ONLY external script allowed
- No other external dependencies (no CDN, no remote fonts)
- All other CSS and JS must be inline
- Mobile-friendly: large tap targets (min 44px), readable font sizes (min 16px body)
- Return ONLY the HTML content, starting with <!DOCTYPE html> and ending with </html>
- No explanation, preamble, or commentary before or after the HTML

${GAMESYNC_API_DOC}`;

// ── Provider detection + streaming helpers ────────────────────────────────────
function detectProvider(key) {
  if (key.startsWith('sk-ant-')) return 'anthropic';
  if (key.startsWith('sk-or-'))  return 'openrouter';
  return 'openai';
}

const DEFAULT_MODELS = {
  anthropic:   'claude-sonnet-5',
  openai:      'gpt-4o',
  openrouter:  'anthropic/claude-sonnet-5'
};

async function* normalizeStream(provider, response) {
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let buffer = '';
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    buffer += decoder.decode(value, { stream: true });
    const lines = buffer.split('\n');
    buffer = lines.pop();
    for (const line of lines) {
      if (!line.startsWith('data: ')) continue;
      const data = line.slice(6).trim();
      if (!data || data === '[DONE]') continue;
      try {
        const event = JSON.parse(data);
        if (provider === 'anthropic') {
          if (event.type === 'content_block_delta' && event.delta?.type === 'text_delta')
            yield event.delta.text;
          else if (event.type === 'error')
            throw new Error(event.error?.message || JSON.stringify(event.error));
        } else {
          // OpenAI-compatible (OpenAI + OpenRouter)
          const text = event.choices?.[0]?.delta?.content;
          if (text) yield text;
        }
      } catch (e) { if (e.message) throw e; }
    }
  }
}

// ── Slots-mode HTML assembly ──────────────────────────────────────────────────
function escXml(s) {
  return String(s || '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}

function assembleFromSlots(s) {
  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>${escXml(s.title)}</title>
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
html{font-size:16px}
body{font-family:system-ui,-apple-system,BlinkMacSystemFont,sans-serif;background:#f8fafc;color:#0f172a;min-height:100vh;display:flex;flex-direction:column;align-items:center;padding:1rem 1rem 2rem}
#app{width:100%;max-width:740px;display:flex;flex-direction:column;gap:.9rem}
header{background:#fff;border:1px solid #e2e8f0;border-radius:.75rem;padding:1rem 1.25rem;box-shadow:0 1px 3px rgba(0,0,0,.06)}
h1{font-size:1.3rem;font-weight:700;margin-bottom:.35rem;line-height:1.2}
#concept-box{font-size:.875rem;color:#475569;line-height:1.55}
#instructions-section{background:#eff6ff;border:1px solid #bfdbfe;border-radius:.6rem;padding:.7rem 1rem}
#instructions-section h2{font-size:.75rem;font-weight:700;text-transform:uppercase;letter-spacing:.06em;color:#1d4ed8;margin-bottom:.25rem}
#instructions-section p{font-size:.875rem;color:#1e40af;line-height:1.5}
#controls-section,#display-section{background:#fff;border:1px solid #e2e8f0;border-radius:.75rem;padding:1rem 1.25rem;box-shadow:0 1px 3px rgba(0,0,0,.06)}
footer{display:flex;align-items:flex-start;justify-content:space-between;gap:1rem;flex-wrap:wrap;padding:.25rem 0}
#reset-btn{padding:.42rem .9rem;background:#6366f1;color:#fff;border:none;border-radius:.5rem;font-size:.85rem;font-weight:600;cursor:pointer;white-space:nowrap;flex-shrink:0}
#reset-btn:hover{background:#4f46e5}
details#theory-note{font-size:.8rem;color:#64748b;flex:1;min-width:160px}
details#theory-note summary{cursor:pointer;color:#6366f1;font-weight:500;user-select:none}
details#theory-note[open] summary{margin-bottom:.3rem}
details#theory-note p{line-height:1.5;color:#475569}
@media(prefers-color-scheme:dark){
  body{background:#0f172a;color:#e2e8f0}
  header,#controls-section,#display-section{background:#1e293b;border-color:#334155}
  #concept-box,details#theory-note p{color:#94a3b8}
  #instructions-section{background:#1e3a5f;border-color:#2563eb}
  #instructions-section h2{color:#60a5fa}
  #instructions-section p{color:#93c5fd}
  details#theory-note{color:#94a3b8}
}
${s.css || ''}
</style>
</head>
<body>
<div id="app">
  <header>
    <h1>${escXml(s.title)}</h1>
    <div id="concept-box">${escXml(s.concept)}</div>
  </header>
  <div id="instructions-section">
    <h2>How to Play</h2>
    <p>${escXml(s.instructions)}</p>
  </div>
  <div id="controls-section">
    ${s.controls_html || ''}
  </div>
  <div id="display-section">
    ${s.display_html || ''}
  </div>
  <footer>
    <button id="reset-btn" onclick="resetGame()">&#x21BA; Reset</button>
    <details id="theory-note">
      <summary>Theory &amp; prediction</summary>
      <p>${escXml(s.theory)}</p>
    </details>
  </footer>
</div>
<script>
${s.js || ''}
</script>
</body>
</html>`;
}

// ── POST /api/chat — multi-provider streaming proxy ───────────────────────────
app.post('/api/chat', requireAuth, async (req, res) => {
  const { messages, multiplayer, structureMode = 'freeform' } = req.body;
  if (!Array.isArray(messages) || !messages.length)
    return res.status(400).json({ error: 'messages array required' });

  // Use the client's own key if provided; fall back to the server's key for everyone else.
  const clientKey    = (req.headers['x-api-key'] || '').trim();
  const usingOwnKey  = clientKey.startsWith('sk-');
  const apiKey       = usingOwnKey ? clientKey : DEMO_OPENAI_KEY;
  if (!apiKey || !apiKey.startsWith('sk-'))
    return res.status(400).json({ error: 'Enter an API key (Anthropic, OpenAI, or OpenRouter) in the bar above.' });

  const provider = detectProvider(apiKey);
  const model    = (!usingOwnKey && provider === 'openai') ? 'gpt-4o-mini' : DEFAULT_MODELS[provider];

  // Select system prompt: multiplayer always uses SYSTEM_MULTI; solo uses variant by structureMode
  const system = multiplayer ? SYSTEM_MULTI : ({
    freeform: SYSTEM_SOLO,
    metadata: SYSTEM_SOLO_METADATA,
    json:     SYSTEM_SOLO_JSON,
    slots:    SYSTEM_SOLO_SLOTS,
  }[structureMode] || SYSTEM_SOLO);

  // json/slots modes buffer the full response, parse JSON, then emit assembled HTML
  const needsBuffer = !multiplayer && (structureMode === 'json' || structureMode === 'slots');

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

  const emit = text => res.write(`data: ${JSON.stringify({ type: 'text', text })}\n\n`);

  try {
    let upstream;

    if (provider === 'anthropic') {
      upstream = await fetch('https://api.anthropic.com/v1/messages', {
        method: 'POST',
        headers: { 'x-api-key': apiKey, 'anthropic-version': '2023-06-01', 'Content-Type': 'application/json' },
        body: JSON.stringify({ model, max_tokens: 8192, stream: true, system, messages })
      });
    } else {
      const baseUrl = provider === 'openrouter'
        ? 'https://openrouter.ai/api/v1'
        : 'https://api.openai.com/v1';
      upstream = await fetch(`${baseUrl}/chat/completions`, {
        method: 'POST',
        headers: { 'Authorization': `Bearer ${apiKey}`, 'Content-Type': 'application/json' },
        body: JSON.stringify({
          model, max_tokens: 8192, stream: true,
          messages: [{ role: 'system', content: system }, ...messages]
        })
      });
    }

    if (!upstream.ok) {
      const err = await upstream.text();
      res.write(`data: ${JSON.stringify({ type: 'error', error: err })}\n\n`);
      return res.end();
    }

    if (needsBuffer) {
      // Buffer full response, parse JSON, assemble HTML, emit as one chunk
      let raw = '';
      for await (const text of normalizeStream(provider, upstream)) raw += text;

      // Strip markdown code fences if the model adds them
      const cleaned = raw.replace(/^```(?:json)?\s*/i, '').replace(/\s*```$/i, '').trim();
      let parsed;
      try {
        parsed = JSON.parse(cleaned);
      } catch (e) {
        res.write(`data: ${JSON.stringify({ type: 'error', error:
          'AI returned invalid JSON — try again or switch to "Open canvas" mode. ' + e.message })}\n\n`);
        return res.end();
      }

      let html;
      if (structureMode === 'slots') {
        html = assembleFromSlots(parsed);
      } else {
        // json mode: use the html field directly
        html = parsed.html || '';
      }

      // Inject GAME_META comment into <head> for downstream parsing
      const meta = { title: parsed.title || '', concept: parsed.concept || '', theory: parsed.theory || '' };
      const metaComment = `<!-- GAME_META ${JSON.stringify(meta)} -->`;
      html = html.replace(/(<head[^>]*>)/i, `$1\n${metaComment}`);

      emit(html);
    } else {
      for await (const text of normalizeStream(provider, upstream)) {
        if (res.writableEnded) break;
        emit(text);
      }
    }
  } catch (e) {
    if (!res.writableEnded)
      res.write(`data: ${JSON.stringify({ type: 'error', error: e.message })}\n\n`);
  }
  res.write('data: [DONE]\n\n');
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

// ── GET /api/games — list recent saved games (metadata only) ─────────────────
app.get('/api/games', requireAuth, (req, res) => {
  const limit = Math.min(parseInt(req.query.limit) || 50, 100);
  const rows = db.prepare(`
    SELECT room_code, title, is_multiplayer, created_by, created_at
    FROM games ORDER BY rowid DESC LIMIT ?
  `).all(limit);
  res.json(rows);
});

// ── GET /api/game/:code — fetch a saved game (full record) ────────────────────
app.get('/api/game/:code', requireAuth, (req, res) => {
  const game = db.prepare('SELECT * FROM games WHERE room_code=?').get(req.params.code);
  if (!game) return res.status(404).json({ error: 'Game not found' });
  res.json(game);
});

// ── PATCH /api/game/:code — update HTML (and optionally title) of a saved game ─
app.patch('/api/game/:code', requireAuth, (req, res) => {
  const { html, title } = req.body;
  if (!html) return res.status(400).json({ error: 'html required' });

  const game = db.prepare('SELECT * FROM games WHERE room_code=?').get(req.params.code);
  if (!game) return res.status(404).json({ error: 'Game not found' });

  const email = req.session.user.email;
  if (!AUTH_DISABLED && email !== game.created_by && email !== ADMIN_EMAIL)
    return res.status(403).json({ error: 'Only the game creator can update this game' });

  db.prepare('UPDATE games SET html=?, title=? WHERE room_code=?')
    .run(html, title || game.title, req.params.code);

  res.json({ ok: true, room_code: req.params.code });
});

// ── DELETE /api/game/:code — admin-only game removal ─────────────────────────
app.delete('/api/game/:code', requireAuth, (req, res) => {
  if (!AUTH_DISABLED && req.session.user.email !== ADMIN_EMAIL)
    return res.status(403).json({ error: 'Admin only' });
  const result = db.prepare('DELETE FROM games WHERE room_code=?').run(req.params.code);
  if (!result.changes) return res.status(404).json({ error: 'Not found' });
  res.json({ ok: true });
});

// ── GET /api/templates — list available pre-built templates ───────────────────
app.get('/api/templates', requireAuth, (req, res) => {
  const hidden = new Set(
    db.prepare('SELECT template_id FROM hidden_templates').all().map(r => r.template_id)
  );
  res.json(TEMPLATES.filter(t => !hidden.has(t.id)).map(({ source: _src, ...meta }) => meta));
});

// ── GET /api/template/:id — fetch a specific template (includes HTML) ─────────
app.get('/api/template/:id', requireAuth, (req, res) => {
  const t = TEMPLATES.find(x => x.id === req.params.id);
  if (!t) return res.status(404).json({ error: 'Template not found' });
  try {
    const html = readFileSync(join(__dirname, 'templates', `${t.source}.html`), 'utf8');
    res.json({ ...t, html });
  } catch {
    res.status(404).json({ error: `Template file "${t.source}.html" not found on server.` });
  }
});

// ── DELETE /api/template/:id — admin-only template hide ───────────────────────
app.delete('/api/template/:id', requireAuth, (req, res) => {
  if (req.session.user?.email !== ADMIN_EMAIL)
    return res.status(403).json({ error: 'Admin only' });
  const t = TEMPLATES.find(x => x.id === req.params.id);
  if (!t) return res.status(404).json({ error: 'Template not found' });
  db.prepare('INSERT OR IGNORE INTO hidden_templates(template_id) VALUES(?)').run(req.params.id);
  res.json({ ok: true });
});

// ── POST /api/export — convert game HTML to R Shiny or Python Streamlit ───────
app.post('/api/export', requireAuth, async (req, res) => {
  const { html, language } = req.body;
  if (!html) return res.status(400).json({ error: 'html required' });
  if (!['r_shiny', 'python_streamlit', 'stata'].includes(language))
    return res.status(400).json({ error: 'language must be r_shiny, python_streamlit, or stata' });

  const clientKey   = (req.headers['x-api-key'] || '').trim();
  const usingOwnKey = clientKey.startsWith('sk-');
  const apiKey      = usingOwnKey ? clientKey : DEMO_OPENAI_KEY;
  if (!apiKey || !apiKey.startsWith('sk-'))
    return res.status(400).json({ error: 'API key required for export. Add your key via "+ Use your own key".' });

  let remaining;
  try { remaining = checkRateLimit(req.session.user.email); }
  catch (e) { return res.status(e.code || 429).json({ error: e.message }); }

  const provider = detectProvider(apiKey);
  const model    = (!usingOwnKey && provider === 'openai') ? 'gpt-4o-mini' : DEFAULT_MODELS[provider];
  const isR      = language === 'r_shiny';
  const isStata  = language === 'stata';

  const systemPrompt = isR
    ? `You are an expert R programmer converting interactive HTML/JavaScript classroom economics tools to self-contained R Shiny apps.

Convert the provided HTML to a single app.R file using shinyApp(ui, server). Rules:
- Reproduce every interactive control: sliders → sliderInput, number inputs → numericInput, selects → selectInput, buttons → actionButton
- Reproduce all charts using base R graphics (plot/lines/polygon/points/rect/mtext) inside renderPlot — no ggplot2, no plotly
- Preserve all the mathematics and logic from the JavaScript exactly
- Return ONLY valid R code starting with library(shiny). No markdown fences, no explanation.`
    : isStata
    ? `You are an expert Stata programmer converting interactive HTML/JavaScript classroom economics tools to self-contained Stata do-files.

Convert the provided HTML to a single analysis.do file. Rules:
- Put every tunable parameter (anything that was a slider or numeric input) as a named scalar or local macro at the top of the file, clearly commented, so the user can change values in one place
- Reproduce all charts using Stata twoway graphics (twoway line, twoway area, twoway scatter, etc.); use graph twoway with appropriate options for axes, titles, and colors; export to analysis.png with "graph export analysis.png, replace"
- Reproduce every calculation and table from the JavaScript using Stata commands (generate, replace, summarize, display, etc.)
- Use "clear" at the start and build any needed datasets with "set obs N" + generate commands — no external data files required
- Add a brief comment block at the top explaining what the tool does and how to change the parameters
- Return ONLY valid Stata code starting with "* ". No markdown fences, no explanation.`
    : `You are an expert Python programmer converting interactive HTML/JavaScript classroom economics tools to self-contained Python Streamlit apps.

Convert the provided HTML to a single app.py file. Rules:
- Reproduce every interactive control: sliders → st.slider, number inputs → st.number_input, selects → st.selectbox, buttons → st.button
- Reproduce all charts using matplotlib (fig, ax = plt.subplots() then st.pyplot(fig)) — no plotly, no altair
- Preserve all the mathematics and logic from the JavaScript exactly
- Return ONLY valid Python code starting with import streamlit as st. No markdown fences, no explanation.`;

  try {
    let responseText;

    if (provider === 'anthropic') {
      const upstream = await fetch('https://api.anthropic.com/v1/messages', {
        method: 'POST',
        headers: { 'x-api-key': apiKey, 'anthropic-version': '2023-06-01', 'Content-Type': 'application/json' },
        body: JSON.stringify({
          model, max_tokens: 8192, stream: false,
          system: systemPrompt,
          messages: [{ role: 'user', content: `Convert this to ${isR ? 'R Shiny' : isStata ? 'Stata' : 'Python Streamlit'}:\n\n${html}` }]
        })
      });
      if (!upstream.ok) {
        const err = await upstream.text();
        return res.status(502).json({ error: `API error: ${err.slice(0, 300)}` });
      }
      const data = await upstream.json();
      responseText = data.content?.[0]?.text || '';
    } else {
      const baseUrl = provider === 'openrouter' ? 'https://openrouter.ai/api/v1' : 'https://api.openai.com/v1';
      const upstream = await fetch(`${baseUrl}/chat/completions`, {
        method: 'POST',
        headers: { 'Authorization': `Bearer ${apiKey}`, 'Content-Type': 'application/json' },
        body: JSON.stringify({
          model, max_tokens: 8192, stream: false,
          messages: [
            { role: 'system', content: systemPrompt },
            { role: 'user', content: `Convert this to ${isR ? 'R Shiny' : isStata ? 'Stata' : 'Python Streamlit'}:\n\n${html}` }
          ]
        })
      });
      if (!upstream.ok) {
        const err = await upstream.text();
        return res.status(502).json({ error: `API error: ${err.slice(0, 300)}` });
      }
      const data = await upstream.json();
      responseText = data.choices?.[0]?.message?.content || '';
    }

    // Strip any code fences the model adds despite instructions
    const code = responseText.replace(/^```[a-z]*\r?\n?/i, '').replace(/\r?\n?```$/i, '').trim();
    const filename = isR ? 'app.R' : isStata ? 'analysis.do' : 'app.py';
    res.json({ code, filename, remaining });
  } catch (e) {
    res.status(500).json({ error: e.message });
  }
});

// ── GET /play — serve saved game (auth required) ─────────────────────────────
app.get('/play', (req, res) => {
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
