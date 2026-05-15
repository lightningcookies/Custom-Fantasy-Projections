import { parse } from "csv-parse/sync";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.join(__dirname, "..");
const outDir = path.join(root, "site", "data");

const TEAM_NAMES = {
  ARI: "Arizona Cardinals",
  ATL: "Atlanta Falcons",
  BAL: "Baltimore Ravens",
  BUF: "Buffalo Bills",
  CAR: "Carolina Panthers",
  CHI: "Chicago Bears",
  CIN: "Cincinnati Bengals",
  CLE: "Cleveland Browns",
  DAL: "Dallas Cowboys",
  DEN: "Denver Broncos",
  DET: "Detroit Lions",
  GB: "Green Bay Packers",
  HOU: "Houston Texans",
  IND: "Indianapolis Colts",
  JAX: "Jacksonville Jaguars",
  KC: "Kansas City Chiefs",
  LV: "Las Vegas Raiders",
  LAC: "Los Angeles Chargers",
  LAR: "Los Angeles Rams",
  MIA: "Miami Dolphins",
  MIN: "Minnesota Vikings",
  NE: "New England Patriots",
  NO: "New Orleans Saints",
  NYG: "New York Giants",
  NYJ: "New York Jets",
  PHI: "Philadelphia Eagles",
  PIT: "Pittsburgh Steelers",
  SF: "San Francisco 49ers",
  SEA: "Seattle Seahawks",
  TB: "Tampa Bay Buccaneers",
  TEN: "Tennessee Titans",
  WAS: "Washington Commanders",
};

function readText(rel) {
  try {
    return fs.readFileSync(path.join(root, rel), "utf8").trim();
  } catch {
    return "";
  }
}

function parseConfigSeasons() {
  const cfg = readText("R/config.R");
  const tmpl = cfg.match(/NFL_TEMPLATE_SEASON\s*<-\s*(\d+)/);
  const rosterSeason = tmpl ? Number(tmpl[1]) : 2026;
  let referenceSeason = Number(readText("R/.effective_stats_season"));
  if (!Number.isFinite(referenceSeason)) {
    const fallback = cfg.match(/NFL_STATS_SEASON\s*<-\s*(\d+)/);
    referenceSeason = fallback ? Number(fallback[1]) : 2024;
  }
  return { rosterSeason, referenceSeason };
}

function readCsv(file) {
  const text = fs.readFileSync(path.join(root, file), "utf8");
  return parse(text, {
    columns: true,
    skip_empty_lines: true,
    relax_quotes: true,
    cast: false,
  });
}

function num(v) {
  if (v === "" || v === undefined || v === null || v === "NA") return 0;
  const n = Number(v);
  return Number.isFinite(n) ? n : 0;
}

function main() {
  fs.mkdirSync(outDir, { recursive: true });

  const { rosterSeason, referenceSeason } = parseConfigSeasons();
  const rosterFile = `${rosterSeason}_rosters.csv`;
  const teamStatsFile = `team_stats_${referenceSeason}.csv`;

  const rosterRows = readCsv(rosterFile);
  const rosters = rosterRows.map((r) => ({
    name: r.Name?.trim() ?? "",
    pos: (r.POS ?? "").trim(),
    age: num(r.Age),
    exp: num(r.Exp),
    college: (r.College ?? "").trim(),
    team: (r.team ?? "").trim().toUpperCase(),
  }));

  const teamStatRows = readCsv(teamStatsFile);
  const teamStats = teamStatRows.map((r) => ({
    team: (r.team ?? "").toUpperCase(),
    offYd: num(r.off_yd),
    passYd: num(r.p_yd),
    carries: num(r.car),
    rushYd: num(r.r_yd),
    rushTd: num(r.r_td),
    passFf: num(r.p_ff),
    passAtt: num(r.p_att),
    cmpPct: num(r.cmp_pct),
    passTd: num(r.p_td),
    int: num(r.int),
    fmb: num(r.fmb),
  }));

  const playerRows = readCsv("player_data.csv");
  const agg = new Map();

  for (const row of playerRows) {
    if (num(row.season) !== referenceSeason) continue;
    if ((row.season_type ?? "").trim() !== "REG") continue;

    const id = (row.player_id ?? "").trim();
    if (!id) continue;

    let a = agg.get(id);
    if (!a) {
      a = {
        playerId: id,
        name: (row.player_display_name ?? row.player_name ?? "").trim(),
        position: (row.position ?? "").trim(),
        team: (row.recent_team ?? "").trim().toUpperCase(),
        games: 0,
        fantasy: 0,
        fantasyPpr: 0,
        passYd: 0,
        passTd: 0,
        int: 0,
        rushYd: 0,
        rushTd: 0,
        rec: 0,
        recYd: 0,
        recTd: 0,
        targets: 0,
      };
      agg.set(id, a);
    }

    a.games += 1;
    a.fantasy += num(row.fantasy_points);
    a.fantasyPpr += num(row.fantasy_points_ppr);
    a.passYd += num(row.passing_yards);
    a.passTd += num(row.passing_tds);
    a.int += num(row.interceptions);
    a.rushYd += num(row.rushing_yards);
    a.rushTd += num(row.rushing_tds);
    a.rec += num(row.receptions);
    a.recYd += num(row.receiving_yards);
    a.recTd += num(row.receiving_tds);
    a.targets += num(row.targets);
    if (a.team !== (row.recent_team ?? "").trim().toUpperCase()) {
      a.team = (row.recent_team ?? "").trim().toUpperCase() || a.team;
    }
  }

  const seasonTotals = [...agg.values()].map((p) => ({
    ...p,
    fantasy: Math.round(p.fantasy * 100) / 100,
    fantasyPpr: Math.round(p.fantasyPpr * 100) / 100,
  }));

  const meta = {
    teamNames: TEAM_NAMES,
    builtAt: new Date().toISOString(),
    rosterSeason,
    referenceSeason,
    rosterCsv: rosterFile,
    teamStatsCsv: teamStatsFile,
  };

  fs.writeFileSync(path.join(outDir, "meta.json"), JSON.stringify(meta));
  fs.writeFileSync(path.join(outDir, "rosters.json"), JSON.stringify(rosters));
  fs.writeFileSync(path.join(outDir, "teamTotals.json"), JSON.stringify(teamStats));
  fs.writeFileSync(path.join(outDir, "seasonTotals.json"), JSON.stringify(seasonTotals));

  console.log(
    `Wrote ${rosters.length} roster rows, ${teamStats.length} team stat rows, ${seasonTotals.length} player season aggregates (ref ${referenceSeason}, roster ${rosterSeason}) → ${outDir}`
  );
}

main();
