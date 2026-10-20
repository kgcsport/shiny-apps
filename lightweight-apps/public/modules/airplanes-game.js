export const INITIAL_ROWS = Object.freeze([
  { firm: "A", workers: 1, output: "" },
  { firm: "B", workers: 1, output: "" },
  { firm: "C", workers: 1, output: "" }
]);

export function normalizeRows(rows) {
  return rows.map((row, index) => ({
    firm: String(row.firm ?? "").trim(),
    workers: Math.max(0, Number(row.workers) || 0),
    output: row.output === "" || row.output === null || row.output === undefined ? "" : Math.max(0, Number(row.output) || 0),
    id: row.id ?? `${index}-${globalThis.crypto?.randomUUID?.() ?? Math.random().toString(36).slice(2)}`
  }));
}

export function chartPoints(rows) {
  return normalizeRows(rows).filter(row => row.firm && row.output !== "").map(({ firm, workers, output }) => ({ firm, workers, output }));
}

export function normalizeExperimentState(saved) {
  if (Array.isArray(saved)) return { round: 1, rows: normalizeRows(saved), history: [] };
  const rows = Array.isArray(saved?.rows) && saved.rows.length ? saved.rows : INITIAL_ROWS;
  return {
    round: Math.max(1, Number.parseInt(saved?.round, 10) || 1),
    rows: normalizeRows(rows),
    history: Array.isArray(saved?.history) ? saved.history : []
  };
}

export function advanceRound(state, completedAt = new Date().toISOString()) {
  const currentRows = normalizeRows(state.rows);
  return {
    round: Math.max(1, Number(state.round) || 1) + 1,
    rows: currentRows.map(row => ({ ...row, output: "" })),
    history: [...(state.history || []), { round: Math.max(1, Number(state.round) || 1), completedAt, rows: currentRows }]
  };
}

function escapeCsv(value) {
  const string = String(value ?? "");
  return /[",\n]/.test(string) ? `"${string.replaceAll('"', '""')}"` : string;
}

export function airplanesCsv(rows) {
  return ["Firm,Workers,Planes_produced", ...normalizeRows(rows).map(row => [row.firm, row.workers, row.output].map(escapeCsv).join(","))].join("\n") + "\n";
}
