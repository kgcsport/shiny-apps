import assert from "node:assert/strict";
import test from "node:test";
import { evaluateFirm, firmCsv } from "../public/modules/theory-of-firm.js";
test("firm solution maximizes profit at MR equals MC for linear costs",()=>{const e=evaluateFirm({A:80,B:2,F:20,a:20,b:0,c:0,d:0,qMax:40});assert.ok(Math.abs(e.q-15)<.03);assert.ok(Math.abs(e.mr-e.mcAtQ)<.15);assert.equal(Math.round(e.price),50);assert.equal(Math.round(e.profit),430);});
test("firm CSV includes solved outcome",()=>assert.match(firmCsv({A:80,B:2,F:20,a:20,b:0,c:0,d:0,qMax:40}),/marginal_revenue/));
