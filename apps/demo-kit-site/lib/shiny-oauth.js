export const SHINY_APP_PATH = '/class-job-market/';
export const REVIEW_QUIZ_PATH = '/review-quiz/';

export const COORDINATION_GAMES_PATH = '/coordination-games/';
// OAuth may return only to an explicitly enrolled course app. Never reflect
// arbitrary URLs through the callback: this is both the Class Job Market login
// gateway and the shared session entry point for course games.
export function safeShinyReturnPath(value, shinyBaseUrl) {
  if (typeof value !== 'string' || value.length > 300 || !shinyBaseUrl) return SHINY_APP_PATH;
  try {
    const appUrl = new URL(shinyBaseUrl);
    const requested = new URL(value, shinyBaseUrl);
    const appPath = SHINY_APP_PATH.replace(/\/$/, '');
    const requestedPath = requested.pathname.replace(/\/$/, '');
    if (requested.origin !== appUrl.origin) return SHINY_APP_PATH;
    if (requestedPath === REVIEW_QUIZ_PATH.replace(/\/$/, '')) return REVIEW_QUIZ_PATH;
    if (requestedPath === COORDINATION_GAMES_PATH.replace(/\/$/, "")) return COORDINATION_GAMES_PATH;
    if (requestedPath !== appPath) return SHINY_APP_PATH;
    return requested.searchParams.get('view') === 'cold-call-slide'
      ? `${SHINY_APP_PATH}?view=cold-call-slide`
      : SHINY_APP_PATH;
  } catch {
    return SHINY_APP_PATH;
  }
}
