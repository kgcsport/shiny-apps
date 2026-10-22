const valid = value => Number.isFinite(value) && value > 0;

export function parseWtp(text, sets = 1) {
  const values = String(text).split(",").map(value => Number(value.trim())).filter(valid).sort((a, b) => b - a);
  const base = values.length ? values : [20, 18, 16, 14, 12, 10, 8, 6];
  return Array.from({ length: Math.max(1, Math.min(5, Math.floor(sets) || 1)) }, () => base).flat();
}

export function competitiveBenchmark(wtp, mc) {
  return { quantity: wtp.filter(value => value > mc).length, surplus: wtp.reduce((sum, value) => sum + Math.max(0, value - mc), 0) };
}

export function roundOneSummary(trades, mc) {
  const consumerSurplus = trades.reduce((sum, trade) => sum + Math.max(0, trade.wtp - trade.price), 0);
  const producerSurplus = trades.reduce((sum, trade) => sum + Math.max(0, trade.price - mc), 0);
  return { quantity: trades.length, consumerSurplus, producerSurplus, totalSurplus: consumerSurplus + producerSurplus, averagePrice: trades.length ? trades.reduce((sum, trade) => sum + trade.price, 0) / trades.length : null };
}

export function roundTwoSummary(wtp, mc, price, accepted) {
  const buyers = [...wtp].sort((a, b) => b - a).filter(value => value >= price);
  const traded = buyers.slice(0, Math.max(0, Math.min(Math.floor(accepted) || 0, buyers.length)));
  const consumerSurplus = traded.reduce((sum, value) => sum + Math.max(0, value - price), 0);
  const producerSurplus = (price - mc) * traded.length;
  const competitive = [...wtp].sort((a, b) => b - a).filter(value => value > mc);
  const deadweightLoss = competitive.slice(traded.length).reduce((sum, value) => sum + Math.max(0, value - mc), 0);
  return { quantity: traded.length, consumerSurplus, producerSurplus, totalSurplus: consumerSurplus + producerSurplus, deadweightLoss, eligible: buyers };
}

export function addTrade(state, wtp, price) {
  const index = state.available.indexOf(wtp);
  if (index < 0 || !Number.isFinite(price) || price < 0) throw new RangeError("Choose an available buyer and a nonnegative price.");
  return { ...state, trades: [...state.trades, { wtp, price }], available: state.available.filter((_, current) => current !== index) };
}

export function undoTrade(state) {
  if (!state.trades.length) return state;
  const trade = state.trades.at(-1);
  return { ...state, trades: state.trades.slice(0, -1), available: [...state.available, trade.wtp].sort((a, b) => b - a) };
}
