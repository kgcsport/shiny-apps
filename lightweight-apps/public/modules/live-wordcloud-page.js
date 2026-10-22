const $ = selector => document.querySelector(selector);
const params = new URLSearchParams(location.search);
const clientKey = "lightweight-live-wordcloud-client";
const pollKey = "lightweight-live-wordcloud-admin-code";
let clientId = localStorage.getItem(clientKey);
if (!clientId) { clientId = crypto.randomUUID?.() || `${Math.random()}-${Date.now()}`; localStorage.setItem(clientKey, clientId); }
let pollId = normalize(params.get("poll"));
let state; let socket;
const api = action => `/api/live-wordcloud/${encodeURIComponent(pollId)}/${action}`;
function normalize(value) { return String(value || "").trim().toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, ""); }
function instructorCode() { return localStorage.getItem(`${pollKey}:${pollId}`) || ""; }
function status(message, bad = false) { const el = $("#poll-status"); el.textContent = message; el.classList.toggle("error", bad); }
async function request(action, options = {}) { const response = await fetch(api(action), { headers: { "content-type": "application/json" }, ...options }); const data = await response.json(); if (!response.ok) throw new Error(data.error || "The room could not be updated."); return data; }
function render(next) {
  state = next; $("#landing").hidden = true; $("#poll-view").hidden = false;
  const displayMode = params.get("display") === "1";
  const studentEntryMode = params.get("student") === "1" && !displayMode;
  $("#poll-title").textContent = state.prompt; $("#poll-instructions").textContent = state.instructions;
  const cloud = $("#cloud"); cloud.replaceChildren();
  if (!state.counts.length) cloud.innerHTML = '<span class="cloud-empty">Responses will appear here.</span>';
  else state.counts.forEach((term, index) => { const span = document.createElement("span"); span.className = "cloud-term"; span.title = `${term.n} ${term.n === 1 ? "response" : "responses"}`; span.textContent = term.label; span.style.cssText = `font-size:${Math.min(3.8, 1.1 + .9 * Math.log2(term.n + 1)).toFixed(2)}rem;color:${["#7A1731","#0072B2","#D55E00","#009E73","#6A3D9A","#8C510A"][index % 6]};opacity:${(.72 + .28 * Math.sqrt(term.n / state.counts[0].n)).toFixed(2)}`; cloud.append(span); });
  $("#response-count").textContent = state.responseCount ? `${state.responseCount} ${state.responseCount === 1 ? "response" : "responses"}` : "No responses yet";
  const studentMode = params.get("student") === "1";
  const canAdmin = !studentMode && (Boolean(instructorCode()) || params.get("admin") === "1");
  $("#instructor-area").hidden = !canAdmin;
  $("#cloud-shell").hidden = studentEntryMode;
  $("#response-count").hidden = studentEntryMode;
  $("#response-form").hidden = !studentEntryMode || !state.isOpen;
  if (!state.isOpen && studentEntryMode) status("Submissions are closed.");
  if (canAdmin) { const form = $("#configure-form"); form.prompt.value = state.prompt; form.instructions.value = state.instructions; form.maxWords.value = state.maxWords; form.isOpen.checked = state.isOpen; form.adminCode.value = instructorCode(); }
  const studentUrl = `${location.origin}/live-wordcloud/?poll=${encodeURIComponent(pollId)}&student=1`;
  const displayUrl = `${studentUrl}&display=1`;
  $("#student-url").textContent = studentUrl;
  $("#embed-code").textContent = `<iframe src="${displayUrl}" title="Live class poll results" style="width:100%;height:420px;border:0"></iframe>`;
}
function connect() { socket?.close(); const protocol = location.protocol === "https:" ? "wss:" : "ws:"; socket = new WebSocket(`${protocol}//${location.host}${api("connect")}`); socket.onmessage = event => { const data = JSON.parse(event.data); if (data.type === "state") render(data.state); }; socket.onclose = () => setTimeout(() => pollId && connect(), 1500); }
async function load() {
  try { render(await request("state", { method: "GET", headers: {} })); connect(); }
  catch (error) {
    $("#landing").hidden = false;
    $("#poll-view").hidden = true;
    $("#poll-title").textContent = "Poll not found";
    $("#poll-instructions").textContent = `“${pollId}” has not been created yet. Create it below, or enter a different URL key.`;
    $("#create-form [name=pollId]").value = pollId;
    $("#join-form [name=pollId]").value = pollId;
    const message = $("#create-form .status"); message.textContent = error.message; message.classList.add("error");
  }
}
$("#create-form").addEventListener("submit", async event => { event.preventDefault(); const form = new FormData(event.currentTarget); pollId = normalize(form.get("pollId")); try { const next = await request("create", { method: "POST", body: JSON.stringify(Object.fromEntries(form)) }); localStorage.setItem(`${pollKey}:${pollId}`, form.get("adminCode")); history.replaceState(null, "", `?poll=${encodeURIComponent(pollId)}&admin=1`); render(next); connect(); } catch (error) { const el = $("#create-form .status"); el.textContent = error.message; el.classList.add("error"); } });
$("#join-form").addEventListener("submit", event => { event.preventDefault(); const id = normalize(new FormData(event.currentTarget).get("pollId")); if (id) location.search = `?poll=${encodeURIComponent(id)}`; });
$("#response-form").addEventListener("submit", async event => { event.preventDefault(); const input = $("#response"); try { await request("submit", { method: "POST", body: JSON.stringify({ response: input.value, clientId }) }); input.value = ""; status("Response added."); } catch (error) { status(error.message, true); } });
$("#configure-form").addEventListener("submit", async event => { event.preventDefault(); const data = Object.fromEntries(new FormData(event.currentTarget)); data.isOpen = $("#configure-form [name=isOpen]").checked; try { const next = await request("configure", { method: "POST", body: JSON.stringify(data) }); localStorage.setItem(`${pollKey}:${pollId}`, data.adminCode); render(next); status("Question saved."); } catch (error) { status(error.message, true); } });
$("#clear-button").addEventListener("click", async () => { if (!confirm("Clear every response to this poll?")) return; try { const next = await request("clear", { method: "POST", body: JSON.stringify({ adminCode: instructorCode() || $("#configure-form [name=adminCode]").value }) }); render(next); status("Responses cleared."); } catch (error) { status(error.message, true); } });
$("#export-button").addEventListener("click", async () => { try { const data = await request(`export?code=${encodeURIComponent(instructorCode() || $("#configure-form [name=adminCode]").value)}`, { method: "GET", headers: {} }); const link = document.createElement("a"); link.href = URL.createObjectURL(new Blob([JSON.stringify(data, null, 2)], { type: "application/json" })); link.download = `${pollId}-responses.json`; link.click(); URL.revokeObjectURL(link.href); } catch (error) { status(error.message, true); } });
if (params.get("display") === "1") document.documentElement.classList.add("display-mode"); if (pollId) load();
