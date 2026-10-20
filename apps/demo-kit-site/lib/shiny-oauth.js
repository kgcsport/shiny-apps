export const SHINY_APP_PATH = '/class-job-market/';

// OAuth may return only to the course app, optionally restoring its compact
// slide panel. Never reflect arbitrary URLs through the callback.
export function safeShinyReturnPath(value, shinyBaseUrl) {
  if (typeof value !== 'string' || value.length > 300 || !shinyBaseUrl) return SHINY_APP_PATH;
  try {
    const appUrl = new URL(shinyBaseUrl);
    const requested = new URL(value, shinyBaseUrl);
    const appPath = SHINY_APP_PATH.replace(/\/$/, '');
    const requestedPath = requested.pathname.replace(/\/$/, '');
    if (requested.origin !== appUrl.origin || requestedPath !== appPath) return SHINY_APP_PATH;
    return requested.searchParams.get('view') === 'cold-call-slide'
      ? `${SHINY_APP_PATH}?view=cold-call-slide`
      : SHINY_APP_PATH;
  } catch {
    return SHINY_APP_PATH;
  }
}
