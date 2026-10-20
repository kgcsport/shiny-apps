import assert from "node:assert/strict";
import test from "node:test";
import { readFile } from "node:fs/promises";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";

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

test("inline page modules remain valid JavaScript", async () => {
  const directory = await mkdtemp(join(tmpdir(), "lightweight-pages-"));
  for (const page of ["tax-incidence", "airplanes-game"]) {
    const html = await readFile(new URL(`../public/${page}/index.html`, import.meta.url), "utf8");
    const script = html.match(/<script type="module">([\s\S]*?)<\/script>/)?.[1];
    assert.ok(script, `${page} needs a module script`);
    const file = join(directory, `${page}.mjs`);
    await writeFile(file, script);
    const check = spawnSync(process.execPath, ["--check", file], { encoding: "utf8" });
    assert.equal(check.status, 0, check.stderr);
  }
});
