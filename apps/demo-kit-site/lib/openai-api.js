const configured = (name, fallback) => process.env[name] || fallback;

export function selectOpenAIModel(usingOwnKey) {
  return usingOwnKey
    ? configured('OPENAI_DEFAULT_MODEL', 'gpt-6-sol')
    : configured('OPENAI_DEMO_MODEL', 'gpt-6-luna');
}

export function makeOpenAIResponseBody({ model, instructions, input, stream = false }) {
  return {
    model,
    instructions,
    input,
    max_output_tokens: 8192,
    store: false,
    ...(stream ? { stream: true } : {}),
  };
}

export function extractOpenAIStreamDelta(event) {
  return event?.type === 'response.output_text.delta' ? event.delta || '' : '';
}

export function extractOpenAIResponseText(response) {
  if (typeof response?.output_text === 'string') return response.output_text;
  return (response?.output || [])
    .filter(item => item?.type === 'message')
    .flatMap(item => item.content || [])
    .filter(part => part?.type === 'output_text')
    .map(part => part.text || '')
    .join('');
}
