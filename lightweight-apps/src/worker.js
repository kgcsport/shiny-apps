// Static-only Phase 0 Worker. Shared-room Durable Objects will be added only
// after the static pilots have passed classroom parity checks.
export default {
  fetch(request, env) {
    if (new URL(request.url).pathname === "/healthz") {
      return Response.json({ ok: true, service: "lightweight-teaching-apps", mode: "static-pilot" });
    }
    return env.ASSETS.fetch(request);
  }
};
