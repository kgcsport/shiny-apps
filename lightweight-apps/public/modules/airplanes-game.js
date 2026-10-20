export const INITIAL_ROWS = Object.freeze([
  { firm: "Team A", workers: 1, output: "" },
  { firm: "Team B", workers: 1, output: "" },
  { firm: "Team C", workers: 1, output: "" }
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

export function roundChartPoints(state) {
  const completed = (state.history || []).flatMap(snapshot =>
    chartPoints(snapshot.rows).map(point => ({ ...point, round: snapshot.round, current: false }))
  );
  const current = chartPoints(state.rows || []).map(point => ({ ...point, round: state.round, current: true }));
  return [...completed, ...current];
}

export function normalizeExperimentState(saved) {
  if (Array.isArray(saved)) return { round: 1, rows: normalizeRows(saved), history: [], future: [] };
  const rows = Array.isArray(saved?.rows) && saved.rows.length ? saved.rows : INITIAL_ROWS;
  return {
    round: Math.max(1, Number.parseInt(saved?.round, 10) || 1),
    rows: normalizeRows(rows),
    history: Array.isArray(saved?.history) ? saved.history : [],
    future: Array.isArray(saved?.future) ? saved.future : []
  };
}

export function advanceRound(state, completedAt = new Date().toISOString()) {
  const currentRows = normalizeRows(state.rows);
  return {
    round: Math.max(1, Number(state.round) || 1) + 1,
    rows: currentRows.map(row => ({ ...row, output: "" })),
    history: [...(state.history || []), { round: Math.max(1, Number(state.round) || 1), completedAt, rows: currentRows }],
    future: []
  };
}

export function rewindRound(state) {
  const history = state.history || [];
  if (!history.length || Number(state.round) <= 1) return normalizeExperimentState(state);
  const previous = history.at(-1);
  return {
    round: previous.round,
    rows: normalizeRows(previous.rows),
    history: history.slice(0, -1),
    future: [...(state.future || []), { round: state.round, rows: normalizeRows(state.rows) }]
  };
}

export function forwardRound(state, completedAt = new Date().toISOString()) {
  const future = state.future || [];
  if (!future.length) return normalizeExperimentState(state);
  const next = future.at(-1);
  return {
    round: next.round,
    rows: normalizeRows(next.rows),
    history: [...(state.history || []), { round: state.round, completedAt, rows: normalizeRows(state.rows) }],
    future: future.slice(0, -1)
  };
}

export function renameTeamAcrossRounds(state, teamId, teamName) {
  const name = String(teamName ?? "").trim();
  const rename = rows => rows.map(row => row.id === teamId ? { ...row, firm: name } : row);
  return {
    ...state,
    rows: rename(state.rows || []),
    history: (state.history || []).map(snapshot => ({ ...snapshot, rows: rename(snapshot.rows || []) })),
    future: (state.future || []).map(snapshot => ({ ...snapshot, rows: rename(snapshot.rows || []) }))
  };
}

function escapeCsv(value) {
  const string = String(value ?? "");
  return /[",\n]/.test(string) ? `"${string.replaceAll('"', '""')}"` : string;
}

export function airplanesCsv(rows) {
  return ["Team,Workers,Planes_produced", ...normalizeRows(rows).map(row => [row.firm, row.workers, row.output].map(escapeCsv).join(","))].join("\n") + "\n";
}
