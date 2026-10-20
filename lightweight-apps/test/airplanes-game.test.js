import assert from "node:assert/strict";
import test from "node:test";
import { advanceRound, airplanesCsv, chartPoints, normalizeExperimentState, normalizeRows } from "../public/modules/airplanes-game.js";

test("airplane rows preserve blank outputs but chart only uses completed observations", () => {
  const rows = normalizeRows([{ firm: "A", workers: 1, output: "" }, { firm: "B", workers: 2, output: 9 }]);
  assert.equal(rows[0].output, "");
  assert.deepEqual(chartPoints(rows), [{ firm: "B", workers: 2, output: 9 }]);
});

test("airplane CSV quotes data safely", () => {
  assert.equal(airplanesCsv([{ firm: "A, team", workers: 2, output: 9 }]), "Firm,Workers,Planes_produced\n\"A, team\",2,9\n");
});

test("advance round archives completed work and retains each firm's workers", () => {
  const next = advanceRound(normalizeExperimentState({ round: 2, rows: [{ firm: "A", workers: 3, output: 11 }] }), "2026-10-20T12:00:00.000Z");
  assert.equal(next.round, 3);
  assert.deepEqual(next.rows.map(({ firm, workers, output }) => ({ firm, workers, output })), [{ firm: "A", workers: 3, output: "" }]);
  assert.equal(next.history[0].round, 2);
  assert.equal(next.history[0].rows[0].output, 11);
});
