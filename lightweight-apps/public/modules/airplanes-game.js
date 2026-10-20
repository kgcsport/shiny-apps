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

function escapeCsv(value) {
  const string = String(value ?? "");
  return /[",\n]/.test(string) ? `"${string.replaceAll('"', '""')}"` : string;
}

export function airplanesCsv(rows) {
  return ["Firm,Workers,Planes_produced", ...normalizeRows(rows).map(row => [row.firm, row.workers, row.output].map(escapeCsv).join(","))].join("\n") + "\n";
}
