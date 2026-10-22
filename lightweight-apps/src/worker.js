import { LiveWordcloudRoom } from "./live-wordcloud-room.js";

export default {
  fetch(request, env) {
    const url = new URL(request.url);
    if (url.pathname === "/healthz") {
      return Response.json({ ok: true, service: "lightweight-teaching-apps", mode: "shared-room-pilot" });
    }
    const match = url.pathname.match(/^\/api\/live-wordcloud\/([a-z0-9-]+)\/(.+)$/);
    if (match) return env.LIVE_WORDCLOUD.get(env.LIVE_WORDCLOUD.idFromName(match[1])).fetch(request);
    return env.ASSETS.fetch(request);
  }
};

export { LiveWordcloudRoom };
