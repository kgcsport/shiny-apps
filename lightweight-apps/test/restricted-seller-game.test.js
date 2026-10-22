import assert from "node:assert/strict";
import test from "node:test";
import { addTrade, competitiveBenchmark, parseWtp, roundOneSummary, roundTwoSummary, undoTrade } from "../public/modules/restricted-seller-game.js";
test("WTP parsing sorts values and repeats each set",()=>assert.deepEqual(parseWtp("8, 20, 12",2),[20,12,8,20,12,8]));
test("competitive benchmark counts profitable trades and surplus",()=>assert.deepEqual(competitiveBenchmark([20,18,4,2],4),{quantity:2,surplus:30}));
test("trade entry removes a buyer and undo restores it",()=>{const initial={available:[20,18],trades:[]};const entered=addTrade(initial,20,8);assert.deepEqual(entered.available,[18]);assert.equal(roundOneSummary(entered.trades,4).totalSurplus,16);assert.deepEqual(undoTrade(entered),initial);});
test("monopoly summary caps accepted buyers and calculates deadweight loss",()=>{const result=roundTwoSummary([20,18,16,14,12,10,8,6],4,14,9);assert.equal(result.quantity,4);assert.equal(result.producerSurplus,40);assert.equal(result.deadweightLoss,20);});
