import test from 'node:test';
import assert from 'node:assert/strict';
import { safeShinyReturnPath } from '../lib/shiny-oauth.js';

const base = 'https://shiny.kylecoombs.com';

test('preserves allowlisted course-app return paths', () => {
  assert.equal(safeShinyReturnPath('/class-job-market/?view=cold-call-slide', base),
               '/class-job-market/?view=cold-call-slide');
  assert.equal(safeShinyReturnPath('https://shiny.kylecoombs.com/class-job-market?view=cold-call-slide', base),
               '/class-job-market/?view=cold-call-slide');
  assert.equal(safeShinyReturnPath('/review-quiz/', base), '/review-quiz/');
  assert.equal(safeShinyReturnPath('https://shiny.kylecoombs.com/review-quiz', base), '/review-quiz/');
  assert.equal(safeShinyReturnPath('/coordination-games/', base), '/coordination-games/');
  assert.equal(safeShinyReturnPath('https://shiny.kylecoombs.com/coordination-games', base), '/coordination-games/');
});

test('rejects external, unrelated, and unrecognized return routes', () => {
  assert.equal(safeShinyReturnPath('https://example.com', base), '/class-job-market/');
  assert.equal(safeShinyReturnPath('/live-wordcloud/', base), '/class-job-market/');
  assert.equal(safeShinyReturnPath('/class-job-market/?view=admin', base), '/class-job-market/');
});
