export function normalizePollId(value) {
  return String(value ?? "").trim().toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 80);
}

export function normalizeResponse(value) {
  return String(value ?? "").trim().replace(/[^\p{L}\p{N}\s]+/gu, " ").replace(/\s+/g, " ").trim().toLocaleLowerCase();
}

export function validateResponse(value, maxWords = 4, maxChars = 42) {
  const text = String(value ?? "").trim();
  if (!text) return "Enter a response before submitting.";
  if (!normalizeResponse(text)) return "Enter words rather than punctuation.";
  if (text.length > maxChars) return `Keep the response under ${maxChars} characters.`;
  if (text.split(/\s+/).length > maxWords) return `Use ${maxWords} words or fewer.`;
  return null;
}

export function cloudCounts(responses) {
  const groups = new Map();
  for (const response of Object.values(responses || {})) {
    const normalized = normalizeResponse(response);
    if (!normalized) continue;
    const previous = groups.get(normalized) || { label: response, n: 0 };
    previous.n += 1;
    groups.set(normalized, previous);
  }
  return [...groups.entries()].map(([normalized, value]) => ({ normalized, ...value }))
    .sort((a, b) => b.n - a.n || a.normalized.localeCompare(b.normalized));
}

export function publicState(poll) {
  return {
    pollId: poll.pollId,
    prompt: poll.prompt,
    instructions: poll.instructions,
    maxWords: poll.maxWords,
    isOpen: poll.isOpen,
    counts: cloudCounts(poll.responses),
    responseCount: Object.keys(poll.responses || {}).length
  };
}
