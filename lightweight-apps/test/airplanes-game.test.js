import assert from "node:assert/strict";
import test from "node:test";
import { advanceRound, airplanesCsv, chartPoints, forwardRound, normalizeExperimentState, normalizeRows, rewindRound, roundChartPoints } from "../public/modules/airplanes-game.js";

test("airplane rows preserve blank outputs but chart only uses completed observations", () => {
  const rows = normalizeRows([{ firm: "A", workers: 1, output: "" }, { firm: "B", workers: 2, output: 9 }]);
  assert.equal(rows[0].output, "");
  assert.deepEqual(chartPoints(rows), [{ firm: "B", workers: 2, output: 9 }]);
});

test("legacy row-only local saves gain empty round histories safely", () => {
  const state = normalizeExperimentState([{ firm: "A", workers: 1, output: 2 }]);
  assert.equal(state.round, 1);
  assert.deepEqual(state.history, []);
  assert.deepEqual(state.future, []);
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

test("chart points include completed rounds as well as the current round", () => {
  const state = {
    round: 2,
    history: [{ round: 1, rows: [{ firm: "A", workers: 1, output: 4 }] }],
    rows: [{ firm: "A", workers: 2, output: 9 }]
  };
  assert.deepEqual(roundChartPoints(state), [
    { firm: "A", workers: 1, output: 4, round: 1, current: false },
    { firm: "A", workers: 2, output: 9, round: 2, current: true }
  ]);
});

test("rewind restores the prior round and saves the active round as a forward draft", () => {
  const rewound = rewindRound({
    round: 3,
    history: [{ round: 1, rows: [{ firm: "A", workers: 1, output: 3 }] }, { round: 2, rows: [{ firm: "A", workers: 2, output: 8 }] }],
    rows: [{ firm: "A", workers: 4, output: 15 }]
  });
  assert.equal(rewound.round, 2);
  assert.equal(rewound.history.length, 1);
  assert.equal(rewound.future.length, 1);
  assert.equal(rewound.future[0].rows[0].output, 15);
  assert.deepEqual(rewound.rows.map(({ firm, workers, output }) => ({ firm, workers, output })), [{ firm: "A", workers: 2, output: 8 }]);
});

test("forward restores the saved draft and retains any edits to the round revisited", () => {
  const forward = forwardRound({
    round: 1,
    rows: [{ firm: "A", workers: 1, output: 5 }],
    history: [],
    future: [{ round: 2, rows: [{ firm: "A", workers: 2, output: 10 }] }]
  }, "2026-10-20T12:00:00.000Z");
  assert.equal(forward.round, 2);
  assert.equal(forward.rows[0].output, 10);
  assert.equal(forward.history[0].rows[0].output, 5);
  assert.equal(forward.future.length, 0);
});
