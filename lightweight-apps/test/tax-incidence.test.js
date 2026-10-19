import assert from "node:assert/strict";
import test from "node:test";
import { calculateTaxIncidence, taxIncidenceCsv } from "../public/modules/tax-incidence.js";

test("tax incidence matches the fixed-equilibrium Shiny formulas", () => {
  const value = calculateTaxIncidence({ demandSlope: 1, supplySlope: 1, tax: 4 });
  assert.deepEqual(value.qStar, 6);
  assert.equal(value.qTax, 4);
  assert.equal(value.buyerPrice, 10);
  assert.equal(value.sellerPrice, 6);
  assert.equal(value.revenue, 16);
  assert.equal(value.deadweightLoss, 4);
});

test("incidence shares follow relative slopes and output is nonnegative", () => {
  const value = calculateTaxIncidence({ demandSlope: 4, supplySlope: 1, tax: 10 });
  assert.equal(value.qTax, 4);
  assert.equal(value.buyerShare, 0.8);
  assert.equal(value.sellerShare, 0.2);
  assert.match(taxIncidenceCsv({ demandSlope: 1, supplySlope: 1, tax: 4 }), /deadweight_loss/);
});
