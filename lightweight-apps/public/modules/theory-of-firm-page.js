import { evaluateFirm, firmCsv } from "/modules/theory-of-firm.js";

const $ = id => document.getElementById(id);
const SVG = "http://www.w3.org/2000/svg";
const format = value => Number.isFinite(value) ? value.toFixed(2) : "—";

function add(parent, name, attributes = {}, text = "") {
  const element = document.createElementNS(SVG, name);
  Object.entries(attributes).forEach(([key, value]) => element.setAttribute(key, value));
  element.textContent = text;
  parent.append(element);
  return element;
}

function values() {
  return Object.fromEntries(["A", "B", "F", "a", "b", "c", "d", "qMax"].map(key => [key, +$(key).value]));
}

function linePath(fn, maxQ, x, y) {
  let started = false;
  let d = "";
  for (let i = 0; i < 240; i += 1) {
    const q = maxQ * i / 239;
    const value = fn(q);
    if (!Number.isFinite(value)) {
      started = false;
      continue;
    }
    d += `${started ? "L" : "M"}${x(q)} ${y(value)}`;
    started = true;
  }
  return d;
}

function axes(svg, { x, y, qMax, yMin = 0, yMax, width, height, left, bottom }) {
  for (let i = 0; i <= 4; i += 1) {
    const q = qMax * i / 4;
    const value = yMin + (yMax - yMin) * i / 4;
    add(svg, "line", { x1: x(q), y1: y(yMin), x2: x(q), y2: y(yMax), class: "grid" });
    add(svg, "line", { x1: x(0), y1: y(value), x2: x(qMax), y2: y(value), class: "grid" });
    add(svg, "text", { x: x(q), y: height - bottom + 20, class: "label", "text-anchor": "middle" }, format(q));
    add(svg, "text", { x: left - 8, y: y(value) + 4, class: "label", "text-anchor": "end" }, format(value));
  }
  add(svg, "line", { x1: x(0), y1: y(yMin), x2: x(qMax), y2: y(yMin), class: "axis" });
  add(svg, "line", { x1: x(0), y1: y(yMin), x2: x(0), y2: y(yMax), class: "axis" });
  add(svg, "text", { x: (left + width - 20) / 2, y: height - 12, class: "label", "text-anchor": "middle" }, "Quantity (Q)");
}

function directLabel(svg, curve, maxQ, yMin, yMax, x, y) {
  for (let i = 0; i < 100; i += 1) {
    const q = maxQ * (0.88 - i * 0.0075);
    const value = curve.fn(q);
    if (Number.isFinite(value) && value > yMin + (yMax - yMin) * 0.04 && value < yMax - (yMax - yMin) * 0.04) {
      add(svg, "text", { x: x(q) + 7, y: y(value) - 6 + (curve.labelOffset || 0), fill: curve.color, class: "curve-label" }, curve.name);
      return;
    }
  }
}

function drawMarginalChart(model, input) {
  const svg = $("chart");
  const width = 720, height = 440, left = 64, right = 20, top = 20, bottom = 58;
  const yMax = +$("yMax").value;
  const x = q => left + q / input.qMax * (width - left - right);
  const y = value => top + (1 - value / yMax) * (height - top - bottom);
  svg.replaceChildren();
  axes(svg, { x, y, qMax: input.qMax, yMax, width, height, left, bottom });
  add(svg, "text", { x: left, y: 14, class: "label" }, "Price, revenue, and cost per unit");
  const curves = [
    { name: "Demand", fn: model.demand, color: "#1769aa", width: 3 },
    { name: "MR", fn: model.mrCurve, color: "#7c3aed", width: 3 },
    { name: "MC", fn: model.mc, color: "#c2410c", width: 3 }
  ];
  if ($("atc").checked) curves.push(
    { name: "ATC", fn: q => q > 0.1 ? model.tc(q) / q : NaN, color: "#d97706", width: 2, dash: "6 3", labelOffset: -10 },
    { name: "AVC", fn: q => q > 0.1 ? model.avc(q) : NaN, color: "#0f766e", width: 2, dash: "3 3", labelOffset: 10 }
  );
  curves.forEach(curve => {
    add(svg, "path", { d: linePath(curve.fn, input.qMax, x, y), fill: "none", stroke: curve.color, "stroke-width": curve.width, "stroke-dasharray": curve.dash || "" });
    directLabel(svg, curve, input.qMax, 0, yMax, x, y);
  });
  add(svg, "line", { x1: x(model.q), y1: y(0), x2: x(model.q), y2: y(model.price), class: "reference" });
  add(svg, "circle", { cx: x(model.q), cy: y(model.mcAtQ), r: 5, class: "optimum" });
  add(svg, "text", { x: x(model.q) + 7, y: y(model.mcAtQ) + 18, class: "value-label" }, `Q* = ${format(model.q)}`);
}

function drawTotalsChart(model, input) {
  const svg = $("boxes");
  const width = 720, height = 360, left = 64, right = 20, top = 20, bottom = 58;
  const maxQ = Math.min(input.qMax, input.A / input.B);
  const samples = Array.from({ length: 241 }, (_, i) => maxQ * i / 240);
  const revenue = q => model.tr(q);
  const cost = q => model.tc(q);
  const profit = q => revenue(q) - cost(q);
  const allValues = samples.flatMap(q => [revenue(q), cost(q), profit(q)]).filter(Number.isFinite);
  const yMin = Math.min(0, ...allValues);
  const yMax = Math.max(1, ...allValues);
  const padding = Math.max(1, (yMax - yMin) * 0.08);
  const scaledMin = yMin - padding;
  const scaledMax = yMax + padding;
  const x = q => left + q / maxQ * (width - left - right);
  const y = value => top + (scaledMax - value) / (scaledMax - scaledMin) * (height - top - bottom);
  svg.replaceChildren();
  axes(svg, { x, y, qMax: maxQ, yMin: scaledMin, yMax: scaledMax, width, height, left, bottom });
  add(svg, "text", { x: left, y: 14, class: "label" }, "Total dollars");
  add(svg, "line", { x1: x(0), y1: y(0), x2: x(maxQ), y2: y(0), stroke: "#64748b", "stroke-width": 1.5, "stroke-dasharray": "4 4" });
  add(svg, "text", { x: x(maxQ) - 4, y: y(0) - 5, class: "value-label", "text-anchor": "end" }, "zero profit");
  const profitColor = model.profit < 0 ? "#a21c34" : "#19723b";
  const curves = [
    { name: "Total revenue", fn: revenue, color: "#1769aa", width: 3 },
    { name: "Total cost", fn: cost, color: "#c2410c", width: 3 },
    { name: "Profit", fn: profit, color: profitColor, width: 3, dash: "7 3" }
  ];
  curves.forEach(curve => {
    add(svg, "path", { d: linePath(curve.fn, maxQ, x, y), fill: "none", stroke: curve.color, "stroke-width": curve.width, "stroke-dasharray": curve.dash || "" });
    directLabel(svg, curve, maxQ, scaledMin, scaledMax, x, y);
  });
  add(svg, "line", { x1: x(model.q), y1: y(model.totalRevenue), x2: x(model.q), y2: y(model.totalCost), stroke: model.profit >= 0 ? "#19723b" : "#a21c34", "stroke-width": 7, opacity: ".72" });
  add(svg, "circle", { cx: x(model.q), cy: y(model.totalRevenue), r: 4, fill: "#1769aa" });
  add(svg, "circle", { cx: x(model.q), cy: y(model.totalCost), r: 4, fill: "#c2410c" });
  add(svg, "text", { x: x(model.q) + 8, y: (y(model.totalRevenue) + y(model.totalCost)) / 2 - 6, class: "value-label" }, `${model.profit >= 0 ? "Profit" : "Loss"} at Q*`);
}

function draw() {
  try {
    const input = values();
    const model = evaluateFirm(input);
    drawMarginalChart(model, input);
    drawTotalsChart(model, input);
    $("A-v").value = format(input.A);
    $("B-v").value = format(input.B);
    $("summary").innerHTML = `<strong>Profit-maximizing outcome</strong><span>Q*: <b>${format(model.q)}</b></span><span>P*: <b>${format(model.price)}</b></span><span>TR: <b>${format(model.totalRevenue)}</b></span><span>TC: <b>${format(model.totalCost)}</b></span><span>${model.profit >= 0 ? "Profit" : "Loss"}: <b>${format(Math.abs(model.profit))}</b></span>`;
    $("status").textContent = "";
  } catch (error) {
    $("status").textContent = error.message;
  }
}

document.querySelectorAll("input").forEach(input => input.addEventListener("input", draw));
$("download").onclick = () => {
  const link = document.createElement("a");
  link.href = URL.createObjectURL(new Blob([firmCsv(values())]));
  link.download = "firm-solution.csv";
  link.click();
};
draw();
