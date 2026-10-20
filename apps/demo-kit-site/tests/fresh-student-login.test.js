import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const appDir = join(dirname(fileURLToPath(import.meta.url)), '..');
const serverSource = readFileSync(join(appDir, 'server.js'), 'utf8');
const dockerfile = readFileSync(join(appDir, 'Dockerfile'), 'utf8');

test('fresh student Google login has its OAuth helper in the runtime image', () => {
  // An already authenticated instructor can enter Shiny using arcade_token
  // without visiting /auth/login. A new student cannot: this import must both
  // exist in source and be copied into the image that handles /auth/login.
  const helper = join(appDir, 'lib', 'shiny-oauth.js');
  assert.match(serverSource, /from ['"]\.\/lib\/shiny-oauth\.js['"]/);
  assert.ok(existsSync(helper), 'the OAuth helper must exist in source');
  assert.match(dockerfile, /^COPY\s+lib\/\s+\.\/lib\/$/m,
    'the runtime image must copy the OAuth helper directory');
});
