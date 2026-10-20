import {
  INITIAL_ROWS,
  advanceRound,
  airplanesCsv,
  normalizeExperimentState,
  normalizeRows,
  roundChartPoints
} from "/modules/airplanes-game.js";

const COLORS = ["#1769aa", "#a21c34", "#19723b", "#8a4dad", "#bd6d16", "#087a91"];
const STORAGE_KEY = "lightweight-airplanes-game-v2";

export function startAirplanesExperiment(documentRef = document, storage = localStorage) {
  const $ = (id) => documentRef.getElementById(id);
  let state = restore();

  function restore() {
    try {
      return normalizeExperimentState(JSON.parse(storage.getItem(STORAGE_KEY)));
    } catch {
      return normalizeExperimentState({ rows: INITIAL_ROWS });
    }
  }

  function save(message = "Saved on this browser.") {
    storage.setItem(STORAGE_KEY, JSON.stringify(state));
    $("round").textContent = `Round ${state.round} · ${state.history.length} completed`;
    $("status").textContent = message;
  }

  function updateRow(index, patch) {
    state.rows[index] = normalizeRows([{ ...state.rows[index], ...patch }])[0];
    render();
  }

  function field(type, value, label, onInput) {
    const element = documentRef.createElement("input");
    element.type = type;
    element.value = value;
    element.setAttribute("aria-label", label);
    if (type === "number") {
      element.min = "0";
      element.step = "1";
    }
    element.addEventListener("input", () => onInput(element.value));
    return element;
  }

  function renderTable() {
    const body = $("rows");
    body.replaceChildren(...state.rows.map((row, index) => {
      const tr = documentRef.createElement("tr");
      const firm = documentRef.createElement("td");
      const workers = documentRef.createElement("td");
      const output = documentRef.createElement("td");
      const remove = documentRef.createElement("td");
      firm.append(field("text", row.firm, `Firm ${index + 1}`, value => updateRow(index, { firm: value })));
      workers.append(field("number", row.workers, `Workers ${index + 1}`, value => updateRow(index, { workers: value })));
      const outputField = field("number", row.output, `Planes ${index + 1}`, value => updateRow(index, { output: value }));
      outputField.placeholder = "Enter output";
      output.append(outputField);
      const button = documentRef.createElement("button");
      button.className = "icon-button";
      button.type = "button";
      button.textContent = "×";
      button.title = "Remove row";
      button.disabled = state.rows.length === 1;
      button.addEventListener("click", () => {
        state.rows.splice(index, 1);
        render("Row removed.");
      });
      remove.append(button);
      tr.append(firm, workers, output, remove);
      return tr;
    }));
  }

  function renderChart() {
    const svg = $("chart");
    const points = roundChartPoints(state);
    const ns = "http://www.w3.org/2000/svg";
    const d = { w: 720, h: 500, l: 78, r: 28, t: 26, b: 64 };
    const xMax = Math.max(4, Math.ceil(Math.max(0, ...points.map(point => point.workers)) * 1.2));
    const yMax = Math.max(4, Math.ceil(Math.max(0, ...points.map(point => point.output)) * 1.2));
    const x = value => d.l + value / xMax * (d.w - d.l - d.r);
    const y = value => d.t + (1 - value / yMax) * (d.h - d.t - d.b);
    const add = (tag, attributes, text = "") => {
      const element = documentRef.createElementNS(ns, tag);
      Object.entries(attributes).forEach(([key, value]) => element.setAttribute(key, value));
      element.textContent = text;
      svg.append(element);
    };
    svg.replaceChildren();
    for (let i = 0; i <= 5; i += 1) {
      const xv = xMax * i / 5;
      const yv = yMax * i / 5;
      add("line", { x1: x(xv), y1: y(0), x2: x(xv), y2: y(yMax), class: "grid" });
      add("line", { x1: x(0), y1: y(yv), x2: x(xMax), y2: y(yv), class: "grid" });
      add("text", { x: x(xv), y: y(0) + 22, class: "label", "text-anchor": "middle" }, xv.toFixed(1));
      add("text", { x: x(0) - 10, y: y(yv) + 4, class: "label", "text-anchor": "end" }, yv.toFixed(1));
    }
    add("line", { x1: x(0), y1: y(0), x2: x(xMax), y2: y(0), class: "axis", "stroke-width": "2" });
    add("line", { x1: x(0), y1: y(0), x2: x(0), y2: y(yMax), class: "axis", "stroke-width": "2" });
    add("text", { x: x(xMax / 2), y: d.h - 18, class: "label", "text-anchor": "middle" }, "Workers");
    add("text", { x: 18, y: y(yMax / 2), class: "label", transform: `rotate(-90 18 ${y(yMax / 2)})`, "text-anchor": "middle" }, "Planes Produced");
    if (!points.length) {
      add("text", { x: x(xMax / 2), y: y(yMax / 2), class: "label", "text-anchor": "middle" }, "Enter output values to see the chart.");
      return;
    }
    points.forEach(point => {
      const color = COLORS[(point.round - 1) % COLORS.length];
      add("circle", { cx: x(point.workers), cy: y(point.output), r: point.current ? "8" : "6", fill: color, opacity: point.current ? "1" : ".48", stroke: "#fff", "stroke-width": "2" });
      add("text", { x: x(point.workers) + 11, y: y(point.output) - 7, fill: color, opacity: point.current ? "1" : ".7", "font-size": "12", "font-weight": "800" }, `R${point.round}: ${point.firm}`);
    });
  }

  function render(message) {
    renderTable();
    renderChart();
    save(message);
  }

  function download() {
    const rows = [
      ...state.history.flatMap(snapshot => snapshot.rows.map(row => ({ ...row, round: snapshot.round }))),
      ...state.rows.map(row => ({ ...row, round: state.round }))
    ];
    const csv = ["Round,Firm,Workers,Planes_produced", ...rows.map(row => `${row.round},${airplanesCsv([row]).trim().split("\n")[1]}`)].join("\n") + "\n";
    const anchor = documentRef.createElement("a");
    anchor.href = URL.createObjectURL(new Blob([csv], { type: "text/csv" }));
    anchor.download = "airplane-production-all-rounds.csv";
    anchor.click();
    URL.revokeObjectURL(anchor.href);
    $("status").textContent = "Downloaded all rounds as CSV.";
  }

  $("add").addEventListener("click", () => {
    state.rows.push(normalizeRows([{ firm: String.fromCharCode(65 + state.rows.length % 26), workers: 1, output: "" }])[0]);
    render("Added a row.");
  });
  $("advance").addEventListener("click", () => {
    const completed = state.round;
    state = advanceRound(state);
    render(`Saved round ${completed}; ready for round ${state.round}.`);
  });
  $("reset").addEventListener("click", () => {
    state = normalizeExperimentState({ rows: INITIAL_ROWS });
    render("Reset all rounds.");
  });
  $("download").addEventListener("click", download);
  render("Ready. Data is stored only in this browser.");
}
