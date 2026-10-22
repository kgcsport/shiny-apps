import { cloudCounts, normalizePollId, publicState, validateResponse } from "./live-wordcloud-core.js";

const json = (data, status = 200) => Response.json(data, { status, headers: { "cache-control": "no-store" } });
const error = (message, status = 400) => json({ error: message }, status);

export class LiveWordcloudRoom {
  constructor(ctx) { this.ctx = ctx; }

  async poll() { return await this.ctx.storage.get("poll"); }
  async save(poll) { await this.ctx.storage.put("poll", poll); }
  async body(request) { try { return await request.json(); } catch { return {}; } }
  authorized(poll, code) { return Boolean(poll && code && code === poll.adminCode); }

  async fetch(request) {
    const url = new URL(request.url);
    const method = request.method;
    if (url.pathname.endsWith("/connect")) return this.connect(request);
    const poll = await this.poll();
    if (method === "POST" && url.pathname.endsWith("/create")) return this.create(request, poll);
    if (!poll) return error("This poll has not been created yet.", 404);
    if (method === "GET" && url.pathname.endsWith("/state")) return json(publicState(poll));
    if (method === "POST" && url.pathname.endsWith("/submit")) return this.submit(request, poll);
    if (method === "POST" && url.pathname.endsWith("/configure")) return this.configure(request, poll);
    if (method === "POST" && url.pathname.endsWith("/clear")) return this.clear(request, poll);
    if (method === "GET" && url.pathname.endsWith("/export")) {
      if (!this.authorized(poll, url.searchParams.get("code"))) return error("Incorrect instructor code.", 403);
      return json({ ...publicState(poll), responses: Object.values(poll.responses) });
    }
    return error("Unknown poll action.", 404);
  }

  async create(request, current) {
    if (current) return error("This stable poll URL already exists.", 409);
    const data = await this.body(request);
    const pollId = normalizePollId(data.pollId);
    const prompt = String(data.prompt ?? "").trim();
    const adminCode = String(data.adminCode ?? "").trim();
    if (!pollId) return error("Use a short URL key.");
    if (!prompt) return error("A question is required.");
    if (adminCode.length < 4) return error("Use an instructor code of at least four characters.");
    const poll = { pollId, prompt, instructions: String(data.instructions ?? "Respond in a few words.").trim(), maxWords: clamp(data.maxWords, 1, 8, 4), isOpen: true, adminCode, responses: {} };
    await this.save(poll); await this.broadcast(poll);
    return json(publicState(poll), 201);
  }

  async submit(request, poll) {
    if (!poll.isOpen) return error("Submissions are closed.", 409);
    const data = await this.body(request);
    const response = String(data.response ?? "").trim();
    const problem = validateResponse(response, poll.maxWords);
    if (problem) return error(problem);
    const clientId = String(data.clientId ?? "").trim();
    if (!clientId || clientId.length > 150) return error("Your browser session could not be identified.");
    poll.responses[clientId] = response;
    await this.save(poll); await this.broadcast(poll);
    return json(publicState(poll));
  }

  async configure(request, poll) {
    const data = await this.body(request);
    if (!this.authorized(poll, data.adminCode)) return error("Incorrect instructor code.", 403);
    const prompt = String(data.prompt ?? poll.prompt).trim();
    if (!prompt) return error("A question is required.");
    poll.prompt = prompt;
    poll.instructions = String(data.instructions ?? poll.instructions).trim();
    poll.maxWords = clamp(data.maxWords, 1, 8, poll.maxWords);
    poll.isOpen = Boolean(data.isOpen);
    await this.save(poll); await this.broadcast(poll);
    return json(publicState(poll));
  }

  async clear(request, poll) {
    const data = await this.body(request);
    if (!this.authorized(poll, data.adminCode)) return error("Incorrect instructor code.", 403);
    poll.responses = {};
    await this.save(poll); await this.broadcast(poll);
    return json(publicState(poll));
  }

  connect(request) {
    if (request.headers.get("Upgrade") !== "websocket") return error("WebSocket upgrade required.", 426);
    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);
    this.ctx.acceptWebSocket(server);
    this.poll().then(poll => poll && server.send(JSON.stringify({ type: "state", state: publicState(poll) })));
    return new Response(null, { status: 101, webSocket: client });
  }
  webSocketMessage() {}
  webSocketClose(ws) { try { ws.close(1000, "Closed"); } catch {} }
  async broadcast(poll) {
    const message = JSON.stringify({ type: "state", state: publicState(poll) });
    for (const socket of this.ctx.getWebSockets()) { try { socket.send(message); } catch {} }
  }
}

function clamp(value, min, max, fallback) {
  const number = Number(value);
  return Number.isFinite(number) ? Math.max(min, Math.min(max, Math.round(number))) : fallback;
}
