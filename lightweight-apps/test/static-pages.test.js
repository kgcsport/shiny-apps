import assert from "node:assert/strict";
import test from "node:test";
import { readFile } from "node:fs/promises";

test("static pilots reference their local calculation modules and accessible charts", async () => {
  const [tax, airplanes] = await Promise.all([
    readFile(new URL("../public/tax-incidence/index.html", import.meta.url), "utf8"),
    readFile(new URL("../public/airplanes-game/index.html", import.meta.url), "utf8")
  ]);
  assert.match(tax, /type="module"/);
  assert.match(tax, /\/modules\/tax-incidence\.js/);
  assert.match(tax, /role="img"/);
  assert.match(airplanes, /\/modules\/airplanes-game\.js/);
  assert.match(airplanes, /localStorage/);
  assert.match(airplanes, /Download CSV/);
});
