import assert from "node:assert/strict";
import test from "node:test";
import { readFile } from "node:fs/promises";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

test("static pilots reference their local calculation modules and accessible charts", async () => {
  const [tax, airplanes, consumer] = await Promise.all([
    readFile(new URL("../public/tax-incidence/index.html", import.meta.url), "utf8"),
    readFile(new URL("../public/airplanes-game/index.html", import.meta.url), "utf8"),
    readFile(new URL("../public/indifference-to-demand/index.html", import.meta.url), "utf8")
  ]);
  assert.match(tax, /type="module"/);
  assert.match(tax, /\/modules\/tax-incidence\.js/);
  assert.match(tax, /role="img"/);
  assert.match(airplanes, /\/modules\/airplanes-page\.js/);
  assert.match(airplanes, /Advance round/);
  assert.match(airplanes, /Download all rounds/);
  assert.match(consumer, /\/modules\/indifference-page\.js/);
  assert.match(consumer, /role="img"/);
  assert.match(consumer, /id="formula-explainer"/);
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
  const pageModule = fileURLToPath(new URL("../public/modules/airplanes-page.js", import.meta.url));
  const moduleCheck = spawnSync(process.execPath, ["--check", pageModule], { encoding: "utf8" });
  assert.equal(moduleCheck.status, 0, moduleCheck.stderr);
  const consumerModule = fileURLToPath(new URL("../public/modules/indifference-page.js", import.meta.url));
  const consumerCheck = spawnSync(process.execPath, ["--check", consumerModule], { encoding: "utf8" });
  assert.equal(consumerCheck.status, 0, consumerCheck.stderr);
  const consumerSource = await readFile(consumerModule, "utf8");
  assert.match(consumerSource, /clip-path/);
  for (const formula of ["X<sup>α</sup>Y<sup>1−α</sup>", "αX + (1−α)Y", "min(X, Y)", "X + α ln(Y)", "Y + aX − (b\/2)X²"]) assert.match(consumerSource, new RegExp(formula.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
});

test("airplane live entry does not rebuild the focused table on every keystroke", async () => {
  const source = await readFile(new URL("../public/modules/airplanes-page.js", import.meta.url), "utf8");
  const updateRow = source.match(/function updateRow\([\s\S]*?\n  \}/)?.[0] || "";
  assert.match(updateRow, /renderChart\(\)/);
  assert.doesNotMatch(updateRow, /render\(/);
});
