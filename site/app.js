const DATA = {
  meta: null,
  rosters: [],
  season: [],
  teamStats: [],
};

const rosterState = {
  team: "ALL",
  pos: null,
  q: "",
  sort: { key: "name", dir: "asc" },
};

const seasonState = {
  pos: null,
  q: "",
  sort: { key: "fantasyPpr", dir: "desc" },
};

const teamState = {
  q: "",
  sort: { key: "offYd", dir: "desc" },
};

function $(sel, root = document) {
  return root.querySelector(sel);
}

function $all(sel, root = document) {
  return [...root.querySelectorAll(sel)];
}

function teamLabel(abbr) {
  const names = DATA.meta?.teamNames ?? {};
  const full = names[abbr];
  return full ? `${full} (${abbr})` : abbr;
}

function escapeHtml(s) {
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

const TEXT_SORT_KEYS = new Set(["name", "pos", "team", "position", "label"]);

function toggleSort(state, key) {
  if (state.sort.key === key) {
    state.sort.dir = state.sort.dir === "asc" ? "desc" : "asc";
  } else {
    state.sort.key = key;
    state.sort.dir = TEXT_SORT_KEYS.has(key) ? "asc" : "desc";
  }
}

function cmp(a, b) {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

function downloadCsv(filename, rows, headers) {
  const esc = (v) => {
    const s = v == null ? "" : String(v);
    if (/[",\n]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
    return s;
  };
  const lines = [headers.map(esc).join(",")];
  for (const row of rows) {
    lines.push(row.map(esc).join(","));
  }
  const blob = new Blob([lines.join("\n")], { type: "text/csv;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  a.click();
  URL.revokeObjectURL(url);
}

async function loadJson(path) {
  const res = await fetch(path, { cache: "no-store" });
  if (!res.ok) throw new Error(`${path} → ${res.status}`);
  return res.json();
}

function rosterYear() {
  return DATA.meta?.rosterSeason ?? 2026;
}

function refYear() {
  return DATA.meta?.referenceSeason ?? 2024;
}

function setPanel(hash) {
  const id = hash === "#rosters" ? "rosters" : hash === "#season" ? "season" : hash === "#teams" ? "teams" : "home";
  $all(".panel").forEach((p) => {
    const show = p.id === `panel-${id}`;
    p.hidden = !show;
    p.classList.toggle("is-visible", show);
  });
  $all(".nav-link").forEach((a) => {
    const href = a.getAttribute("href") ?? "";
    a.classList.toggle("is-active", href === `#${id}` || (id === "home" && href === "#home"));
  });
  document.title =
    id === "home"
      ? "Custom Fantasy Projections"
      : `${id === "rosters" ? `${rosterYear()} rosters` : id === "season" ? `${refYear()} players` : `${refYear()} teams`} · Custom Fantasy Projections`;
}

function onNavClick(e) {
  const t = e.target.closest("[data-nav]");
  if (!t) return;
  const href = t.getAttribute("href");
  if (href?.startsWith("#")) {
    e.preventDefault();
    history.pushState(null, "", href);
    setPanel(href);
    window.scrollTo({ top: 0, behavior: "smooth" });
  }
}

function uniqueTeams(rosters) {
  const s = new Set(rosters.map((r) => r.team).filter(Boolean));
  return [...s].sort();
}

function fillRosterTeamSelect() {
  const sel = $("#roster-team");
  const teams = uniqueTeams(DATA.rosters);
  sel.innerHTML = "";
  const opt0 = document.createElement("option");
  opt0.value = "ALL";
  opt0.textContent = "All teams";
  sel.appendChild(opt0);
  for (const abbr of teams) {
    const o = document.createElement("option");
    o.value = abbr;
    o.textContent = teamLabel(abbr);
    sel.appendChild(o);
  }
}

function renderPosChips(containerId, stateKey) {
  const wrap = $(`#${containerId}`);
  wrap.innerHTML = "";
  const opts = [
    { value: null, label: "All" },
    { value: "QB", label: "QB" },
    { value: "RB", label: "RB" },
    { value: "WR", label: "WR" },
    { value: "TE", label: "TE" },
  ];
  for (const o of opts) {
    const b = document.createElement("button");
    b.type = "button";
    b.className = "chip";
    b.textContent = o.label;
    b.dataset.pos = o.value ?? "";
    if (stateKey === "roster" && rosterState.pos === o.value) b.classList.add("is-on");
    if (stateKey === "season" && seasonState.pos === o.value) b.classList.add("is-on");
    b.addEventListener("click", () => {
      if (stateKey === "roster") {
        rosterState.pos = o.value;
        refreshPosChips("roster-pos", "roster");
        renderRosters();
      } else {
        seasonState.pos = o.value;
        refreshPosChips("season-pos", "season");
        renderSeason();
      }
    });
    wrap.appendChild(b);
  }
}

function refreshPosChips(containerId, stateKey) {
  $all(`#${containerId} .chip`).forEach((b) => {
    const v = b.dataset.pos === "" ? null : b.dataset.pos;
    const on =
      stateKey === "roster" ? rosterState.pos === v : seasonState.pos === v;
    b.classList.toggle("is-on", on);
  });
}

function renderRosters() {
  let rows = DATA.rosters.slice();
  if (rosterState.team !== "ALL") rows = rows.filter((r) => r.team === rosterState.team);
  if (rosterState.pos) rows = rows.filter((r) => r.pos === rosterState.pos);
  const q = rosterState.q.trim().toLowerCase();
  if (q) rows = rows.filter((r) => r.name.toLowerCase().includes(q));

  const { key, dir } = rosterState.sort;
  const sign = dir === "asc" ? 1 : -1;
  rows.sort((a, b) => {
    let va;
    let vb;
    if (key === "age" || key === "exp") {
      va = a[key];
      vb = b[key];
    } else {
      va = String(a[key] ?? "").toLowerCase();
      vb = String(b[key] ?? "").toLowerCase();
    }
    return sign * cmp(va, vb);
  });

  const tb = $("#roster-table tbody");
  tb.innerHTML = rows
    .map(
      (r) => `<tr>
        <td>${escapeHtml(r.name)}</td>
        <td>${escapeHtml(r.pos)}</td>
        <td>${escapeHtml(r.team)}</td>
        <td class="num">${r.age}</td>
        <td class="num">${r.exp}</td>
        <td>${escapeHtml(r.college)}</td>
      </tr>`
    )
    .join("");
  $("#roster-count").textContent = `${rows.length} player${rows.length === 1 ? "" : "s"} shown`;
}

function renderSeason() {
  let rows = DATA.season.slice();
  if (seasonState.pos) rows = rows.filter((r) => r.position === seasonState.pos);
  const q = seasonState.q.trim().toLowerCase();
  if (q) rows = rows.filter((r) => r.name.toLowerCase().includes(q));

  const { key, dir } = seasonState.sort;
  const sign = dir === "asc" ? 1 : -1;
  rows.sort((a, b) => {
    const va = a[key];
    const vb = b[key];
    if (typeof va === "number" && typeof vb === "number") return sign * cmp(va, vb);
    return sign * cmp(String(va ?? "").toLowerCase(), String(vb ?? "").toLowerCase());
  });

  const tb = $("#season-table tbody");
  tb.innerHTML = rows
    .map(
      (r) => `<tr>
        <td>${escapeHtml(r.name)}</td>
        <td>${escapeHtml(r.position)}</td>
        <td>${escapeHtml(r.team)}</td>
        <td class="num">${r.games}</td>
        <td class="num">${r.fantasyPpr}</td>
        <td class="num">${r.fantasy}</td>
        <td class="num">${Math.round(r.passYd)}</td>
        <td class="num">${Math.round(r.rushYd)}</td>
        <td class="num">${Math.round(r.recYd)}</td>
      </tr>`
    )
    .join("");
  $("#season-count").textContent = `${rows.length} player${rows.length === 1 ? "" : "s"} shown`;
}

function renderTeams() {
  const names = DATA.meta?.teamNames ?? {};
  let rows = DATA.teamStats.map((t) => ({
    ...t,
    label: teamLabel(t.team),
    searchBlob: `${t.team} ${(names[t.team] ?? "").toLowerCase()}`,
  }));

  const q = teamState.q.trim().toLowerCase();
  if (q) rows = rows.filter((r) => r.searchBlob.toLowerCase().includes(q));

  const { key, dir } = teamState.sort;
  const sign = dir === "asc" ? 1 : -1;
  rows.sort((a, b) => sign * cmp(a[key], b[key]));

  const tb = $("#team-table tbody");
  tb.innerHTML = rows
    .map(
      (r) => `<tr>
        <td>${escapeHtml(r.label)}</td>
        <td class="num">${Math.round(r.offYd)}</td>
        <td class="num">${Math.round(r.passYd)}</td>
        <td class="num">${Math.round(r.rushYd)}</td>
        <td class="num">${r.passTd}</td>
        <td class="num">${r.rushTd}</td>
        <td class="num">${r.int}</td>
      </tr>`
    )
    .join("");
  $("#team-count").textContent = `${rows.length} team${rows.length === 1 ? "" : "s"} shown`;
}

function wireRoster() {
  $("#roster-team").addEventListener("change", (e) => {
    rosterState.team = e.target.value;
    renderRosters();
  });
  $("#roster-search").addEventListener("input", (e) => {
    rosterState.q = e.target.value;
    renderRosters();
  });
  $all("[data-roster-sort]").forEach((btn) => {
    btn.addEventListener("click", () => {
      toggleSort(rosterState, btn.dataset.rosterSort);
      renderRosters();
    });
  });
  $("#roster-csv").addEventListener("click", () => {
    let rows = DATA.rosters.slice();
    if (rosterState.team !== "ALL") rows = rows.filter((r) => r.team === rosterState.team);
    if (rosterState.pos) rows = rows.filter((r) => r.pos === rosterState.pos);
    const q = rosterState.q.trim().toLowerCase();
    if (q) rows = rows.filter((r) => r.name.toLowerCase().includes(q));
    downloadCsv(
      "rosters_filtered.csv",
      rows.map((r) => [r.name, r.pos, r.team, r.age, r.exp, r.college]),
      ["Name", "POS", "Team", "Age", "Exp", "College"]
    );
  });
}

function wireSeason() {
  $("#season-search").addEventListener("input", (e) => {
    seasonState.q = e.target.value;
    renderSeason();
  });
  $all("[data-season-sort]").forEach((btn) => {
    btn.addEventListener("click", () => {
      toggleSort(seasonState, btn.dataset.seasonSort);
      renderSeason();
    });
  });
  $("#season-csv").addEventListener("click", () => {
    let rows = DATA.season.slice();
    if (seasonState.pos) rows = rows.filter((r) => r.position === seasonState.pos);
    const q = seasonState.q.trim().toLowerCase();
    if (q) rows = rows.filter((r) => r.name.toLowerCase().includes(q));
    downloadCsv(
      `season${refYear()}_filtered.csv`,
      rows.map((r) => [
        r.name,
        r.position,
        r.team,
        r.games,
        r.fantasyPpr,
        r.fantasy,
        Math.round(r.passYd),
        Math.round(r.rushYd),
        Math.round(r.recYd),
      ]),
      ["Player", "Pos", "Team", "G", "PPR", "Std", "Pass Yd", "Rush Yd", "Rec Yd"]
    );
  });
}

function wireTeams() {
  $("#team-search").addEventListener("input", (e) => {
    teamState.q = e.target.value;
    renderTeams();
  });
  $all("[data-team-sort]").forEach((btn) => {
    btn.addEventListener("click", () => {
      toggleSort(teamState, btn.dataset.teamSort);
      renderTeams();
    });
  });
  $("#team-csv").addEventListener("click", () => {
    const names = DATA.meta?.teamNames ?? {};
    let rows = DATA.teamStats.map((t) => ({
      ...t,
      label: teamLabel(t.team),
      searchBlob: `${t.team} ${(names[t.team] ?? "").toLowerCase()}`,
    }));
    const q = teamState.q.trim().toLowerCase();
    if (q) rows = rows.filter((r) => r.searchBlob.toLowerCase().includes(q));
    downloadCsv(
      `team_stats_${refYear()}_filtered.csv`,
      rows.map((r) => [
        r.label,
        Math.round(r.offYd),
        Math.round(r.passYd),
        Math.round(r.rushYd),
        r.passTd,
        r.rushTd,
        r.int,
      ]),
      ["Team", "Off Yd", "Pass Yd", "Rush Yd", "Pass TD", "Rush TD", "INT"]
    );
  });
}

function showError(msg) {
  const main = $(".main");
  const div = document.createElement("div");
  div.className = "error-banner";
  div.setAttribute("role", "alert");
  div.textContent = msg;
  main.prepend(div);
}

async function boot() {
  document.body.addEventListener("click", onNavClick);
  window.addEventListener("popstate", () => setPanel(location.hash || "#home"));

  try {
    const [meta, rosters, season, teamStats] = await Promise.all([
      loadJson("data/meta.json"),
      loadJson("data/rosters.json"),
      loadJson("data/seasonTotals.json"),
      loadJson("data/teamTotals.json"),
    ]);
    DATA.meta = meta;
    DATA.rosters = rosters;
    DATA.season = season;
    DATA.teamStats = teamStats;

    const built = meta.builtAt ? new Date(meta.builtAt).toLocaleString() : "";
    $("#build-meta").textContent = built ? `Data bundle built ${built}.` : "";

    fillRosterTeamSelect();
    renderPosChips("roster-pos", "roster");
    renderPosChips("season-pos", "season");
    wireRoster();
    wireSeason();
    wireTeams();
    renderRosters();
    renderSeason();
    renderTeams();
    setPanel(location.hash || "#home");
  } catch (e) {
    console.error(e);
    showError(
      "Could not load data JSON. Serve this folder over HTTP (for example run npm run serve from the repo root) or run npm run build first."
    );
  }
}

boot();
