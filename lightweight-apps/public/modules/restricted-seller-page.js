import { addTrade, competitiveBenchmark, parseWtp, roundOneSummary, roundTwoSummary, undoTrade } from "/modules/restricted-seller-game.js";

const $ = id => document.getElementById(id);
const money = value => `$${Number(value).toFixed(2)}`;
let state = { phase: "setup", wtp: [20, 18, 16, 14, 12, 10, 8, 6], mc: 4, available: [], trades: [], price: 14, quantity: 4, accepted: 0 };

function stats(items) { return `<div class="game-stats">${items.map(([label, value, tone = ""]) => `<div class="game-stat ${tone}"><span>${label}</span><b>${value}</b></div>`).join("")}</div>`; }
function table(headers, rows) { return `<div class="table-wrap"><table class="data-table"><thead><tr>${headers.map(header => `<th>${header}</th>`).join("")}</tr></thead><tbody>${rows.map(row => `<tr>${row.map(cell => `<td>${cell}</td>`).join("")}</tr>`).join("")}</tbody></table></div>`; }
function button(action, label, style = "primary") { return `<button class="button ${style}" data-action="${action}">${label}</button>`; }

function setup() {
  const benchmark = competitiveBenchmark(state.wtp, state.mc);
  return `<section class="panel game-stage"><h2>Game setup</h2><div class="game-two"><div><div class="control stacked-control"><label for="wtp">Buyer WTP values (comma-separated)</label><input id="wtp" value="${state.wtp.join(", ")}"></div><div class="two-controls"><div class="control stacked-control"><label for="mc">Seller marginal cost</label><input id="mc" type="number" min="0" value="${state.mc}"></div><div class="control stacked-control"><label for="sets">WTP sets</label><input id="sets" type="number" min="1" max="5" value="1"></div></div>${button("start-r1", "Start Round 1: Competitive Market")}</div><div class="panel inset"><h3>Competitive benchmark</h3><p class="quiet">WTP values: <b id="wtp-preview">${state.wtp.join(", ")}</b></p>${stats([["Competitive quantity", `${benchmark.quantity} trades`, "blue"], ["Maximum total surplus", money(benchmark.surplus), "green"]])}</div></div></section>`;
}

function r1Trading() { return `<section class="panel game-stage"><h2>Round 1: Competitive market</h2><div class="game-two"><div><div class="control stacked-control"><label for="trade-wtp">Buyer WTP</label><select id="trade-wtp">${state.available.map(value => `<option value="${value}">${money(value)}</option>`).join("")}</select></div><div class="control stacked-control"><label for="trade-price">Agreed price</label><input id="trade-price" type="number" min="0" value="${state.mc}"></div><div class="button-row">${button("record-trade", "Record trade")}${button("undo-trade", "Undo last trade")}</div></div><div><h3>Trades so far</h3>${state.trades.length ? table(["Trade", "Buyer WTP", "Price"], state.trades.map((trade, i) => [i + 1, money(trade.wtp), money(trade.price)])) : `<p class="quiet">No trades recorded yet.</p>`}${button("end-r1", "End Round 1", "")}</div></div></section>`; }

function r1Results() { const summary = roundOneSummary(state.trades, state.mc); const max = competitiveBenchmark(state.wtp, state.mc).surplus; return `<section class="panel game-stage"><h2>Round 1 results: Competition</h2><div class="game-two"><div>${stats([["Trades", summary.quantity, "blue"], ["Consumer surplus", money(summary.consumerSurplus), "blue"], ["Producer surplus", money(summary.producerSurplus), "green"], ["Total surplus", `${money(summary.totalSurplus)} / ${money(max)}`], ["Average price", summary.averagePrice === null ? "—" : money(summary.averagePrice)]])}${button("start-r2", "Start Round 2: Monopoly")}</div><div><svg id="market-chart" class="chart restricted-chart" viewBox="0 0 720 460" role="img" aria-label="Competitive market demand and surplus diagram"></svg></div></div></section>`; }

function r2Setup() { const rows = state.wtp.map((value, i) => [i + 1, money(value), money((value - state.mc) * (i + 1))]); return `<section class="panel game-stage"><h2>Round 2: Monopoly — seller decides</h2><div class="game-two"><div><div class="control stacked-control"><label for="mono-price">Your price</label><input id="mono-price" type="number" min="0.01" value="${state.price}"></div><div class="control stacked-control"><label for="mono-quantity">Units you will sell</label><input id="mono-quantity" type="number" min="1" value="${state.quantity}"></div><p class="hint">Commit before buyers decide.</p>${button("lock-monopoly", "Lock in price and quantity")}</div><div><h3>Demand schedule</h3>${table(["Units sold", "Price", "Total profit"], rows)}</div></div></section>`; }

function r2Trading() { const summary = roundTwoSummary(state.wtp, state.mc, state.price, state.quantity); return `<section class="panel game-stage"><h2>Round 2: Monopoly trading</h2><div class="game-two"><div>${stats([["Committed price", money(state.price), "green"], ["Committed quantity", `${state.quantity} units`, "green"]])}<div class="control stacked-control"><label for="accepted">How many buyers accepted?</label><input id="accepted" type="number" min="0" max="${Math.min(state.quantity, summary.eligible.length)}" value="${state.accepted}"></div><p class="hint">At most ${Math.min(state.quantity, summary.eligible.length)} eligible buyers can buy.</p>${button("end-r2", "End Round 2", "")}</div><div><h3>Eligible buyers</h3>${summary.eligible.length ? table(["Buyer", "WTP", "Surplus if they buy"], summary.eligible.map((value, i) => [i + 1, money(value), money(value - state.price)])) : `<p class="quiet">No buyer is willing to pay this price.</p>`}</div></div></section>`; }

function r2Results() { const summary = roundTwoSummary(state.wtp, state.mc, state.price, state.accepted); return `<section class="panel game-stage"><h2>Round 2 results: Monopoly</h2><div class="game-two"><div>${stats([["Trades", summary.quantity, "blue"], ["Consumer surplus", money(summary.consumerSurplus), "blue"], ["Producer surplus", money(summary.producerSurplus), "green"], ["Total surplus", money(summary.totalSurplus)], ["Deadweight loss", money(summary.deadweightLoss), "red"]])}${button("compare", "Compare both rounds")}</div><div><svg id="market-chart" class="chart restricted-chart" viewBox="0 0 720 460" role="img" aria-label="Monopoly market demand, profit, and deadweight loss diagram"></svg></div></div></section>`; }

function comparison() { const one = roundOneSummary(state.trades, state.mc), two = roundTwoSummary(state.wtp, state.mc, state.price, state.accepted); return `<section class="panel game-stage"><h2>Competition vs. monopoly</h2><div class="compare-charts"><svg id="competition-chart" class="chart restricted-chart" viewBox="0 0 720 460" role="img" aria-label="Competitive market result"></svg><svg id="monopoly-chart" class="chart restricted-chart" viewBox="0 0 720 460" role="img" aria-label="Monopoly market result"></svg></div>${table(["", "Competition", "Monopoly", "Difference"], [["Trades", one.quantity, two.quantity, two.quantity - one.quantity], ["Consumer surplus", money(one.consumerSurplus), money(two.consumerSurplus), money(two.consumerSurplus - one.consumerSurplus)], ["Producer surplus", money(one.producerSurplus), money(two.producerSurplus), money(two.producerSurplus - one.producerSurplus)], ["Total surplus", money(one.totalSurplus), money(two.totalSurplus), money(two.totalSurplus - one.totalSurplus)], ["Deadweight loss", money(0), money(two.deadweightLoss), money(two.deadweightLoss)]])}<p class="dwl-callout">${money(two.deadweightLoss)} in surplus disappeared. Nobody received it: that is deadweight loss.</p>${button("reset", "Reset game", "")}</section>`; }

function render() {
  const stage = $("stage");
  stage.innerHTML = ({ setup: setup(), r1: r1Trading(), r1results: r1Results(), r2setup: r2Setup(), r2: r2Trading(), r2results: r2Results(), compare: comparison() })[state.phase];
  if (state.phase === "r1results") drawMarket($("market-chart"), { trades: state.trades });
  if (state.phase === "r2results") drawMarket($("market-chart"), { monopoly: true });
  if (state.phase === "compare") { drawMarket($("competition-chart"), { trades: state.trades }); drawMarket($("monopoly-chart"), { monopoly: true }); }
}

function drawMarket(svg, options) {
  const width = 720, height = 460, left = 58, right = 20, top = 28, bottom = 55, maxQ = state.wtp.length + 1, maxP = Math.max(...state.wtp) + 4;
  const x = q => left + q / maxQ * (width - left - right), y = p => top + (maxP - p) / maxP * (height - top - bottom);
  svg.replaceChildren();
  const add = (name, attrs, text = "") => { const element = document.createElementNS("http://www.w3.org/2000/svg", name); Object.entries(attrs).forEach(([key, value]) => element.setAttribute(key, value)); element.textContent = text; svg.append(element); return element; };
  for (let i = 0; i <= state.wtp.length; i += 1) add("line", { x1: x(i), y1: y(0), x2: x(i), y2: y(maxP), class: "grid" });
  for (let p = 0; p <= maxP; p += 4) { add("line", { x1: x(0), y1: y(p), x2: x(maxQ), y2: y(p), class: "grid" }); add("text", { x: left - 7, y: y(p) + 4, class: "label", "text-anchor": "end" }, money(p)); }
  add("line", { x1: x(0), y1: y(0), x2: x(maxQ), y2: y(0), class: "axis" }); add("line", { x1: x(0), y1: y(0), x2: x(0), y2: y(maxP), class: "axis" });
  const sorted = [...state.wtp].sort((a, b) => b - a); sorted.forEach((value, i) => { add("line", { x1: x(i), y1: y(value), x2: x(i + 1), y2: y(value), stroke: "#172033", "stroke-width": 2.5 }); if (i < sorted.length - 1) add("line", { x1: x(i + 1), y1: y(value), x2: x(i + 1), y2: y(sorted[i + 1]), stroke: "#172033", "stroke-width": 2.5 }); });
  add("line", { x1: x(0), y1: y(state.mc), x2: x(maxQ), y2: y(state.mc), stroke: "#19723b", "stroke-width": 2.5 }); add("text", { x: x(maxQ) - 4, y: y(state.mc) - 6, fill: "#19723b", class: "curve-label", "text-anchor": "end" }, `MC = ${money(state.mc)}`);
  const compQ = competitiveBenchmark(state.wtp, state.mc).quantity; add("line", { x1: x(compQ), y1: y(0), x2: x(compQ), y2: y(maxP), class: "reference" });
  if (options.trades) options.trades.forEach((trade, i) => { add("rect", { x: x(i) + 2, y: y(trade.wtp), width: x(i + 1) - x(i) - 4, height: y(trade.price) - y(trade.wtp), fill: "#1769aa", opacity: ".25" }); add("rect", { x: x(i) + 2, y: y(trade.price), width: x(i + 1) - x(i) - 4, height: y(state.mc) - y(trade.price), fill: "#19723b", opacity: ".28" }); });
  if (options.monopoly) { const result = roundTwoSummary(state.wtp, state.mc, state.price, state.accepted); add("line", { x1: x(0), y1: y(state.price), x2: x(maxQ), y2: y(state.price), stroke: "#a21c34", "stroke-width": 2.5, "stroke-dasharray": "7 4" }); add("line", { x1: x(result.quantity), y1: y(0), x2: x(result.quantity), y2: y(maxP), stroke: "#a21c34", "stroke-width": 2.5, "stroke-dasharray": "3 4" }); if (result.quantity) add("rect", { x: x(0), y: y(state.price), width: x(result.quantity) - x(0), height: y(state.mc) - y(state.price), fill: "#19723b", opacity: ".32" }); sorted.slice(result.quantity, compQ).forEach((value, index) => add("rect", { x: x(result.quantity + index) + 2, y: y(value), width: x(result.quantity + index + 1) - x(result.quantity + index) - 4, height: y(state.mc) - y(value), fill: "#a21c34", opacity: ".28" })); add("text", { x: x(maxQ) - 4, y: y(state.price) - 6, fill: "#a21c34", class: "curve-label", "text-anchor": "end" }, `Monopoly price = ${money(state.price)}`); }
  add("text", { x: (left + width - right) / 2, y: height - 13, class: "label", "text-anchor": "middle" }, "Quantity (units traded)"); add("text", { x: left, y: 16, class: "label" }, "Price / WTP");
}

$("stage").addEventListener("click", event => {
  const action = event.target.dataset.action; if (!action) return;
  if (action === "start-r1") { state.wtp = parseWtp($("wtp").value, +$("sets").value); state.mc = +$("mc").value; state.available = [...state.wtp]; state.trades = []; state.phase = "r1"; }
  if (action === "record-trade") { try { state = addTrade(state, +$("trade-wtp").value, +$("trade-price").value); } catch (error) { $("status").textContent = error.message; } }
  if (action === "undo-trade") state = undoTrade(state);
  if (action === "end-r1") state.phase = "r1results";
  if (action === "start-r2") state.phase = "r2setup";
  if (action === "lock-monopoly") { state.price = +$("mono-price").value; state.quantity = Math.max(1, Math.floor(+$("mono-quantity").value)); state.accepted = Math.min(state.quantity, roundTwoSummary(state.wtp, state.mc, state.price, state.quantity).eligible.length); state.phase = "r2"; }
  if (action === "end-r2") { state.accepted = Math.max(0, Math.min(state.quantity, Math.floor(+$("accepted").value) || 0)); state.phase = "r2results"; }
  if (action === "compare") state.phase = "compare";
  if (action === "reset") state = { phase: "setup", wtp: [20, 18, 16, 14, 12, 10, 8, 6], mc: 4, available: [], trades: [], price: 14, quantity: 4, accepted: 0 };
  render();
});
render();
