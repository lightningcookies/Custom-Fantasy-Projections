const $ = (id) => document.getElementById(id);

let meta = { teams: [], statKeys: [] };
let workbookId = null;
let sheet = null;
let saveTimers = new Map();

async function api(path, opts = {}) {
  const res = await fetch(path, {
    headers: { "Content-Type": "application/json", ...opts.headers },
    ...opts,
  });
  if (!res.ok) {
    const t = await res.text();
    throw new Error(t || res.statusText);
  }
  if (res.status === 204) return null;
  const ct = res.headers.get("content-type");
  if (ct && ct.includes("application/json")) return res.json();
  return res.text();
}

function setMsg(el, text, isErr = false) {
  el.textContent = text;
  el.style.color = isErr ? "var(--danger)" : "var(--muted)";
}

async function refreshMeta() {
  meta = await api("/api/meta");
  const st = $("sel-team");
  st.innerHTML = "";
  for (const t of meta.teams) {
    const o = document.createElement("option");
    o.value = t;
    o.textContent = t;
    st.appendChild(o);
  }

  const wbs = await api("/api/workbooks");
  const sw = $("sel-wb");
  sw.innerHTML = "";
  for (const w of wbs) {
    const o = document.createElement("option");
    o.value = w.id;
    o.textContent = `${w.name} (${w.id.slice(0, 8)}…)`;
    sw.appendChild(o);
  }
  if (wbs.length && !workbookId) workbookId = wbs[0].id;
  if (workbookId) sw.value = workbookId;
}

async function loadSheet() {
  workbookId = $("sel-wb").value;
  const team = $("sel-team").value;
  if (!workbookId) return;
  sheet = await api(`/api/workbooks/${workbookId}/team/${team}`);
  renderSheet();
}

function renderSheet() {
  const q = $("inp-filter").value.trim().toLowerCase();
  const keys = meta.statKeys || [];
  const head = $("tbl-head");
  head.innerHTML = "";
  const hr = document.createElement("tr");
  ["Player", "Pos", ...keys.map((k) => `ref:${k}`), ...keys.map((k) => k), "est"].forEach((h) => {
    const th = document.createElement("th");
    th.textContent = h;
    hr.appendChild(th);
  });
  head.appendChild(hr);

  const body = $("tbl-body");
  body.innerHTML = "";
  for (const p of sheet.players) {
    if (q && !p.name.toLowerCase().includes(q)) continue;
    const tr = document.createElement("tr");
    const nameTd = document.createElement("td");
    nameTd.textContent = p.name;
    tr.appendChild(nameTd);
    const posTd = document.createElement("td");
    posTd.textContent = p.pos;
    tr.appendChild(posTd);

    for (const k of keys) {
      const td = document.createElement("td");
      td.className = "num ref";
      const v = p.reference && p.reference[k] != null ? p.reference[k] : "";
      td.textContent = v === "" ? "" : Number(v).toLocaleString(undefined, { maximumFractionDigits: 2 });
      tr.appendChild(td);
    }
    for (const k of keys) {
      const td = document.createElement("td");
      td.className = "num";
      const inp = document.createElement("input");
      inp.className = "cell";
      inp.type = "text";
      inp.inputMode = "decimal";
      inp.dataset.key = k;
      inp.dataset.player = p.name;
      inp.dataset.pos = p.pos;
      const pv = p.projection[k];
      inp.value = pv === 0 || pv === 0.0 ? "" : String(pv);
      inp.addEventListener("input", () => scheduleSave(inp));
      td.appendChild(inp);
      tr.appendChild(td);
    }
    const est = document.createElement("td");
    est.className = "num";
    est.textContent = p.fantasy_ppr_est.toFixed(1);
    tr.appendChild(est);
    body.appendChild(tr);
  }
}

function scheduleSave(inp) {
  const key = `${inp.dataset.player}|${inp.dataset.pos}|${inp.dataset.key}`;
  if (saveTimers.has(key)) clearTimeout(saveTimers.get(key));
  saveTimers.set(
    key,
    setTimeout(() => saveCell(inp), 450)
  );
}

async function saveCell(inp) {
  const team = $("sel-team").value;
  const player_name = inp.dataset.player;
  const position = inp.dataset.pos;
  const k = inp.dataset.key;
  let v = inp.value.trim();
  if (v === "") v = "0";
  const num = Number(v);
  if (Number.isNaN(num)) {
    setMsg($("wb-msg"), `Invalid number for ${player_name} ${k}`, true);
    return;
  }
  try {
    await api(`/api/workbooks/${workbookId}/team/${team}/player`, {
      method: "PATCH",
      body: JSON.stringify({ player_name, position, stats: { [k]: num } }),
    });
    setMsg($("wb-msg"), "Saved");
  } catch (e) {
    setMsg($("wb-msg"), e.message, true);
  }
}

async function loadCombined(pos) {
  if (!workbookId) return;
  const data = await api(`/api/workbooks/${workbookId}/combined/${pos}`);
  const tb = $("tbl-combined-body");
  tb.innerHTML = "";
  data.players.forEach((r, i) => {
    const tr = document.createElement("tr");
    tr.innerHTML = `<td class="num">${i + 1}</td><td>${r.team}</td><td>${r.player_name}</td><td class="num">${r.fantasy_ppr_est.toFixed(
      2
    )}</td>`;
    tb.appendChild(tr);
  });
}

async function newWorkbook() {
  const name = prompt("Workbook name", "My projections") || "My projections";
  const w = await api("/api/workbooks", { method: "POST", body: JSON.stringify({ name }) });
  workbookId = w.id;
  setMsg($("wb-msg"), `Created ${w.rows_seeded} rows`);
  await refreshMeta();
  $("sel-wb").value = workbookId;
  await loadSheet();
}

async function reseed() {
  workbookId = $("sel-wb").value;
  if (!workbookId) return;
  if (!confirm("Clear all projection edits and re-seed from roster?")) return;
  const r = await api(`/api/workbooks/${workbookId}/reseed`, { method: "POST" });
  setMsg($("wb-msg"), `Reseeded ${r.rows_seeded} rows`);
  await loadSheet();
}

async function delWb() {
  workbookId = $("sel-wb").value;
  if (!workbookId) return;
  if (!confirm("Delete this workbook permanently?")) return;
  await api(`/api/workbooks/${workbookId}`, { method: "DELETE" });
  workbookId = null;
  setMsg($("wb-msg"), "Deleted");
  await refreshMeta();
  await loadSheet().catch(() => {});
}

async function boot() {
  try {
    await refreshMeta();
    await loadSheet();
    await loadCombined("QB");
  } catch (e) {
    setMsg($("wb-msg"), e.message || String(e), true);
  }

  $("btn-new").addEventListener("click", () => newWorkbook().catch((e) => setMsg($("wb-msg"), e.message, true)));
  $("btn-reseed").addEventListener("click", () => reseed().catch((e) => setMsg($("wb-msg"), e.message, true)));
  $("btn-del").addEventListener("click", () => delWb().catch((e) => setMsg($("wb-msg"), e.message, true)));
  $("sel-wb").addEventListener("change", () => loadSheet().catch((e) => setMsg($("wb-msg"), e.message, true)));
  $("sel-team").addEventListener("change", () => loadSheet().catch((e) => setMsg($("wb-msg"), e.message, true)));
  $("inp-filter").addEventListener("input", () => renderSheet());

  document.querySelectorAll(".tab").forEach((b) => {
    b.addEventListener("click", () => {
      loadCombined(b.dataset.pos).catch((e) => setMsg($("wb-msg"), e.message, true));
    });
  });
}

boot();
