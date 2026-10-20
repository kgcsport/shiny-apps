import assert from "node:assert/strict";
import test from "node:test";
import worker from "../src/worker.js";

test("health route is independent of assets and exposes static-pilot mode", async () => {
  const response = await worker.fetch(new Request("https://preview.example/healthz"), {});
  assert.equal(response.status, 200);
  assert.deepEqual(await response.json(), {
    ok: true,
    service: "lightweight-teaching-apps",
    mode: "static-pilot"
  });
});
