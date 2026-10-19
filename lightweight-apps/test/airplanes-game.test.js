import assert from "node:assert/strict";
import test from "node:test";
import { airplanesCsv, chartPoints, normalizeRows } from "../public/modules/airplanes-game.js";

test("airplane rows preserve blank outputs but chart only uses completed observations", () => {
  const rows = normalizeRows([{ firm: "A", workers: 1, output: "" }, { firm: "B", workers: 2, output: 9 }]);
  assert.equal(rows[0].output, "");
  assert.deepEqual(chartPoints(rows), [{ firm: "B", workers: 2, output: 9 }]);
});

test("airplane CSV quotes data safely", () => {
  assert.equal(airplanesCsv([{ firm: "A, team", workers: 2, output: 9 }]), "Firm,Workers,Planes_produced\n\"A, team\",2,9\n");
});
