import assert from "node:assert/strict";
import test from "node:test";
import { cloudCounts, normalizePollId, normalizeResponse, publicState, validateResponse } from "../src/live-wordcloud-core.js";

test("wordcloud normalizes stable poll keys and groups equivalent responses", () => {
  assert.equal(normalizePollId(" Tax base! "), "tax-base");
  assert.equal(normalizeResponse("Sales-tax!!"), "sales tax");
  assert.deepEqual(cloudCounts({ one: "Sales tax", two: "sales-tax", three: "Land value" }), [
    { normalized: "sales tax", label: "Sales tax", n: 2 },
    { normalized: "land value", label: "Land value", n: 1 }
  ]);
});

test("wordcloud preserves one current response per browser and validates the original limits", () => {
  assert.equal(validateResponse("", 4), "Enter a response before submitting.");
  assert.equal(validateResponse("one two three four five", 4), "Use 4 words or fewer.");
  const state = publicState({ pollId: "tax-base", prompt: "Recommend a base", instructions: "1–4 words", maxWords: 4, isOpen: true, responses: { a: "Land value", b: "land-value" } });
  assert.equal(state.responseCount, 2);
  assert.equal(state.counts[0].n, 2);
});
