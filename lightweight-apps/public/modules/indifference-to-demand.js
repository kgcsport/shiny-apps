const EPSILON = 1e-8;

export const UTILITY_TYPES = { COBB_DOUGLAS: "cobb-douglas", PERFECT_SUBSTITUTES: "perfect-substitutes", PERFECT_COMPLEMENTS: "perfect-complements", QUASILINEAR: "quasilinear" };

function positive(value, label) { if (!Number.isFinite(value) || value <= 0) throw new RangeError(`${label} must be greater than zero.`); }

export function calculateOptimum({ type, income, px, py, alpha = .5 }) {
  positive(income, "Income"); positive(px, "Price of X"); positive(py, "Price of Y");
  if (type !== UTILITY_TYPES.PERFECT_COMPLEMENTS && (!Number.isFinite(alpha) || alpha <= 0 || alpha >= 1)) throw new RangeError("Preference parameter must be between zero and one.");
  if (type === UTILITY_TYPES.COBB_DOUGLAS) { const x = alpha * income / px, y = (1 - alpha) * income / py; return { x, y, utility: x ** alpha * y ** (1 - alpha) }; }
  if (type === UTILITY_TYPES.PERFECT_SUBSTITUTES) { const xValue = alpha / px, yValue = (1 - alpha) / py; const x = Math.abs(xValue - yValue) < EPSILON ? income / px * .5 : xValue > yValue ? income / px : 0; const y = Math.abs(xValue - yValue) < EPSILON ? income / py * .5 : xValue > yValue ? 0 : income / py; return { x, y, utility: alpha * x + (1 - alpha) * y }; }
  if (type === UTILITY_TYPES.PERFECT_COMPLEMENTS) { const x = income / (px + py); return { x, y: x, utility: x }; }
  if (type === UTILITY_TYPES.QUASILINEAR) { const targetY = alpha / py, affordable = income >= alpha; const y = affordable ? targetY : income / py, x = affordable ? (income - alpha) / px : 0; return { x, y, utility: x + alpha * Math.log(y + 1e-12) }; }
  throw new RangeError("Unknown utility type.");
}

export function demandSchedule({ type, demandFor, income, px, py, alpha = .5, priceMin, priceMax, nPoints }) {
  positive(priceMin, "Minimum price"); positive(priceMax, "Maximum price"); if (priceMax <= priceMin) throw new RangeError("Maximum price must exceed minimum price."); if (!Number.isInteger(nPoints) || nPoints < 2) throw new RangeError("At least two price points are required.");
  const beta = 1 - alpha;
  return Array.from({ length: nPoints }, (_, index) => { const price = priceMin + (priceMax - priceMin) * index / (nPoints - 1); let quantity; if (type === UTILITY_TYPES.COBB_DOUGLAS) quantity = (demandFor === "x" ? alpha : beta) * income / price; else if (type === UTILITY_TYPES.PERFECT_SUBSTITUTES) quantity = demandFor === "x" ? (alpha / price > beta / py ? income / price : 0) : (beta / price > alpha / px ? income / price : 0); else if (type === UTILITY_TYPES.PERFECT_COMPLEMENTS) quantity = income / (price + (demandFor === "x" ? py : px)); else if (type === UTILITY_TYPES.QUASILINEAR) quantity = demandFor === "x" ? (income >= alpha ? (income - alpha) / price : 0) : alpha / price; else throw new RangeError("Unknown utility type."); return { price, quantity }; });
}

export function indifferenceCurves({ type, optimum, alpha = .5, xMax, yMax, count = 3, increment = 10 }) {
  const levels = Array.from({ length: count }, (_, i) => optimum.utility + (i - (count - 1) / 2) * increment).filter(level => Number.isFinite(level) && (type === UTILITY_TYPES.QUASILINEAR || level > 0)); const samples = 160;
  return levels.map(level => { const points = []; if (type === UTILITY_TYPES.COBB_DOUGLAS) for (let i = 0; i < samples; i++) { const x = Math.max(.01, xMax * i / (samples - 1)); points.push({ x, y: (level / x ** alpha) ** (1 / (1 - alpha)) }); } else if (type === UTILITY_TYPES.PERFECT_SUBSTITUTES) for (let i = 0; i < samples; i++) { const x = xMax * i / (samples - 1); points.push({ x, y: (level - alpha * x) / (1 - alpha) }); } else if (type === UTILITY_TYPES.PERFECT_COMPLEMENTS) points.push({ x: 0, y: level }, { x: level, y: level }, { x: level, y: 0 }); else if (type === UTILITY_TYPES.QUASILINEAR) for (let i = 0; i < samples; i++) { const y = Math.max(.01, yMax * (i + 1) / samples); points.push({ x: level - alpha * Math.log(y), y }); } return { level, points }; });
}
