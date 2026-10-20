import test from 'node:test';
import assert from 'node:assert/strict';
import {
  extractOpenAIResponseText,
  extractOpenAIStreamDelta,
  makeOpenAIResponseBody,
  selectOpenAIModel,
} from '../lib/openai-api.js';

test('builds a privacy-preserving Responses API request', () => {
  assert.deepEqual(makeOpenAIResponseBody({
    model: 'gpt-6-sol',
    instructions: 'Be concise.',
    input: [{ role: 'user', content: 'Hello' }],
    stream: true,
  }), {
    model: 'gpt-6-sol',
    instructions: 'Be concise.',
    input: [{ role: 'user', content: 'Hello' }],
    max_output_tokens: 8192,
    store: false,
    stream: true,
  });
});

test('extracts Responses API output and streaming deltas', () => {
  assert.equal(extractOpenAIStreamDelta({
    type: 'response.output_text.delta', delta: 'Hello',
  }), 'Hello');
  assert.equal(extractOpenAIStreamDelta({ type: 'response.completed' }), '');
  assert.equal(extractOpenAIResponseText({
    output: [{ type: 'message', content: [
      { type: 'output_text', text: 'Hello ' },
      { type: 'output_text', text: 'world' },
    ] }],
  }), 'Hello world');
});

test('uses the economical demo model for server-funded requests', () => {
  const oldDemo = process.env.OPENAI_DEMO_MODEL;
  const oldDefault = process.env.OPENAI_DEFAULT_MODEL;
  process.env.OPENAI_DEMO_MODEL = 'demo-model';
  process.env.OPENAI_DEFAULT_MODEL = 'personal-model';
  assert.equal(selectOpenAIModel(false), 'demo-model');
  assert.equal(selectOpenAIModel(true), 'personal-model');
  if (oldDemo === undefined) delete process.env.OPENAI_DEMO_MODEL;
  else process.env.OPENAI_DEMO_MODEL = oldDemo;
  if (oldDefault === undefined) delete process.env.OPENAI_DEFAULT_MODEL;
  else process.env.OPENAI_DEFAULT_MODEL = oldDefault;
});
