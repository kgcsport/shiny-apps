import assert from "node:assert/strict";
import test from "node:test";
import { calculateOptimum, demandSchedule, UTILITY_TYPES } from "../public/modules/indifference-to-demand.js";

test("consumer-choice optima match the Shiny reference models", () => {
  const input = { income: 100, px: 2, py: 4, alpha: .5 };
  assert.deepEqual(calculateOptimum({ ...input, type: UTILITY_TYPES.COBB_DOUGLAS }), { x: 25, y: 12.5, utility: Math.sqrt(312.5) });
  assert.deepEqual(calculateOptimum({ ...input, type: UTILITY_TYPES.PERFECT_COMPLEMENTS }), { x: 100 / 6, y: 100 / 6, utility: 100 / 6 });
  const substitutes = calculateOptimum({ ...input, type: UTILITY_TYPES.PERFECT_SUBSTITUTES });
  assert.equal(substitutes.x, 50); assert.equal(substitutes.y, 0);
  const quasilinear = calculateOptimum({ ...input, type: UTILITY_TYPES.QUASILINEAR });
  assert.equal(quasilinear.x, 49.75); assert.equal(quasilinear.y, .125);
});

test("demand schedules follow the selected good and preference model", () => {
  const base = { income: 100, px: 2, py: 4, alpha: .5, priceMin: 1, priceMax: 5, nPoints: 3 };
  assert.deepEqual(demandSchedule({ ...base, type: UTILITY_TYPES.COBB_DOUGLAS, demandFor: "x" }).map(row => row.quantity), [50, 100 / 6, 10]);
  assert.deepEqual(demandSchedule({ ...base, type: UTILITY_TYPES.PERFECT_COMPLEMENTS, demandFor: "x" }).map(row => row.quantity), [20, 100 / 7, 100 / 9]);
  assert.deepEqual(demandSchedule({ ...base, type: UTILITY_TYPES.QUASILINEAR, demandFor: "y" }).map(row => row.quantity), [.5, 1 / 6, .1]);
});
