#!/usr/bin/env python3
"""
Refresh bundled CSVs from nflverse static release URLs (same files nflreadr uses).
Use when R is not installed locally: run from repo root.

  python3 scripts/nflverse_snapshot.py

Does not scrape HTML. Optional: set NFLVERSE_STATS_SEASON=2025 to prefer that year.
"""
from __future__ import annotations

import csv
import os
import sys
import urllib.request
from datetime import date, datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
R_CONFIG = ROOT / "R" / "config.R"

ROSTER_SEASON = 2026
# Prefer 2025 stats in the 2026 offseason; fall back if GitHub 404s.
STATS_ORDER = [int(os.environ.get("NFLVERSE_STATS_SEASON", "2025")), 2024, 2023]


def read_template_season() -> int:
    text = R_CONFIG.read_text(encoding="utf-8")
    for line in text.splitlines():
        if line.strip().startswith("NFL_TEMPLATE_SEASON"):
            import re

            m = re.search(r"(\d+)", line)
            if m:
                return int(m.group(1))
    return ROSTER_SEASON


def download(url: str, dest: Path) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    print("GET", url)
    urllib.request.urlretrieve(url, dest)


def roster_url(season: int) -> str:
    return (
        f"https://github.com/nflverse/nflverse-data/releases/download/"
        f"rosters/roster_{season}.csv"
    )


def player_stats_url(season: int) -> str:
    return (
        f"https://github.com/nflverse/nflverse-data/releases/download/"
        f"player_stats/player_stats_{season}.csv"
    )


def age_from_birth(birth: str) -> str:
    if not birth or birth.strip() in ("", "NA"):
        return ""
    try:
        bd = datetime.strptime(birth[:10], "%Y-%m-%d").date()
        years = (date.today() - bd).days / 365.25
        return str(int(years))
    except ValueError:
        return ""


def normalize_exp(raw: str) -> str:
    if raw is None or str(raw).strip() in ("", "NA"):
        return "0"
    s = str(raw).strip()
    if s.upper() == "R":
        return "0"
    try:
        return str(int(float(s)))
    except ValueError:
        return "0"


def build_roster_csv(roster_season: int, out_path: Path) -> int:
    src = ROOT / "_tmp_roster.csv"
    download(roster_url(roster_season), src)
    rows_out = []
    seen = set()
    with src.open(newline="", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            pos = (row.get("position") or "").strip()
            if pos not in {"QB", "RB", "WR", "TE"}:
                continue
            gsis = row.get("gsis_id") or ""
            team = (row.get("team") or "").strip()
            key = (gsis, team)
            if key in seen:
                continue
            seen.add(key)
            name = (row.get("full_name") or "").strip() or (
                (row.get("first_name") or "").strip() + " " + (row.get("last_name") or "").strip()
            ).strip()
            college = (row.get("college") or "").strip() or "--"
            rows_out.append(
                {
                    "Name": name,
                    "POS": pos,
                    "Age": age_from_birth(row.get("birth_date") or ""),
                    "Exp": normalize_exp(row.get("years_exp")),
                    "College": college,
                    "team": team,
                }
            )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["Name", "POS", "Age", "Exp", "College", "team"])
        w.writeheader()
        w.writerows(sorted(rows_out, key=lambda x: (x["team"], x["POS"], x["Name"])))
    src.unlink(missing_ok=True)
    print("Wrote", out_path, "rows", len(rows_out))
    return len(rows_out)


def pick_player_stats_season() -> int:
    from urllib.error import HTTPError, URLError

    for y in STATS_ORDER:
        url = player_stats_url(y)
        try:
            with urllib.request.urlopen(url) as resp:  # noqa: S310
                if resp.status == 200:
                    return y
        except (HTTPError, URLError, OSError):
            continue
    print("Could not download player_stats for any of", STATS_ORDER, file=sys.stderr)
    sys.exit(1)


def copy_player_stats(season: int, dest: Path) -> None:
    src = ROOT / "_tmp_player_stats.csv"
    download(player_stats_url(season), src)
    dest.write_bytes(src.read_bytes())
    src.unlink(missing_ok=True)
    print("Wrote", dest, "from season", season)


def write_effective_season(season: int) -> None:
    p = ROOT / "R" / ".effective_stats_season"
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(str(season), encoding="utf-8")
    print("Wrote", p, "→", season)


def aggregate_players_and_teams(stats_season: int) -> None:
    """Mirror generate_data.R using only the Python stdlib."""
    player_csv = ROOT / "player_data.csv"
    nfl_teams = [
        "ARI",
        "ATL",
        "BAL",
        "BUF",
        "CAR",
        "CHI",
        "CIN",
        "CLE",
        "DAL",
        "DEN",
        "DET",
        "GB",
        "HOU",
        "IND",
        "JAX",
        "KC",
        "LAC",
        "LAR",
        "LV",
        "MIA",
        "MIN",
        "NE",
        "NO",
        "NYG",
        "NYJ",
        "PHI",
        "PIT",
        "SEA",
        "SF",
        "TB",
        "TEN",
        "WAS",
    ]

    def fnum(x):
        try:
            return float(x or 0)
        except ValueError:
            return 0.0

    def iint(x):
        return int(round(fnum(x)))

    rows = []
    with player_csv.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row.get("season_type", "").strip() != "REG":
                continue
            if iint(row.get("season")) != stats_season:
                continue
            rows.append(row)

    def grp_key(r):
        return (r.get("position", ""), r.get("player_name", ""), r.get("recent_team", ""))

    from collections import defaultdict

    buckets = defaultdict(list)
    for r in rows:
        buckets[grp_key(r)].append(r)

    out_players = []
    for (position, player_name, recent_team), g in sorted(buckets.items()):
        pos = position
        if pos not in {"QB", "RB", "WR", "TE", "FB"}:
            continue
        rt = "LAR" if recent_team == "LA" else recent_team
        p_att = sum(fnum(x.get("attempts")) for x in g)
        cmp_ = sum(fnum(x.get("completions")) for x in g)
        tgt = sum(fnum(x.get("targets")) for x in g)
        car = sum(fnum(x.get("carries")) for x in g)
        rec = sum(fnum(x.get("receptions")) for x in g)
        sum_p_att = p_att
        tgt_share = (tgt / sum_p_att) if sum_p_att > 0 else ""
        ypc = (sum(fnum(x.get("rushing_yards")) for x in g) / car) if car > 0 else ""
        rec_yd = sum(fnum(x.get("receiving_yards")) for x in g)
        ypr = (rec_yd / rec) if rec > 0 else ""
        cmp_pct = (cmp_ / p_att) if p_att > 0 else ""
        p_td = sum(fnum(x.get("passing_tds")) for x in g)
        td_rate = (p_td / p_att) if p_att > 0 else ""

        out_players.append(
            {
                "player_name": player_name,
                "recent_team": rt,
                "pos": pos,
                "g": len(g),
                "p_att": p_att,
                "cmp": cmp_,
                "p_yd": sum(fnum(x.get("passing_yards")) for x in g),
                "p_td": p_td,
                "int": sum(fnum(x.get("interceptions")) for x in g),
                "car": car,
                "r_yd": sum(fnum(x.get("rushing_yards")) for x in g),
                "r_td": sum(fnum(x.get("rushing_tds")) for x in g),
                "tgt": tgt,
                "rec": rec,
                "rec_yd": rec_yd,
                "rec_td": sum(fnum(x.get("receiving_tds")) for x in g),
                "fmb": sum(
                    fnum(x.get("rushing_fumbles_lost")) + fnum(x.get("sack_fumbles_lost")) for x in g
                ),
                "tp_c": sum(
                    fnum(x.get("passing_2pt_conversions"))
                    + fnum(x.get("rushing_2pt_conversions"))
                    + fnum(x.get("receiving_2pt_conversions"))
                    for x in g
                ),
                "f_ppr": sum(fnum(x.get("fantasy_points_ppr")) for x in g),
                "tgt_share": tgt_share,
                "ypc": ypc,
                "ypr": ypr,
                "cmp_pct": cmp_pct,
                "td_rate": td_rate,
            }
        )

    pout = ROOT / f"players_{stats_season}.csv"
    fields = [
        "player_name",
        "recent_team",
        "pos",
        "g",
        "p_att",
        "cmp",
        "p_yd",
        "p_td",
        "int",
        "car",
        "r_yd",
        "r_td",
        "tgt",
        "rec",
        "rec_yd",
        "rec_td",
        "fmb",
        "tp_c",
        "f_ppr",
        "tgt_share",
        "ypc",
        "ypr",
        "cmp_pct",
        "td_rate",
    ]
    with pout.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for row in sorted(out_players, key=lambda x: (x["recent_team"], x["pos"], x["player_name"])):
            w.writerow(row)
    print("Wrote", pout, "rows", len(out_players))

    by_team = defaultdict(list)
    for row in out_players:
        by_team[row["recent_team"]].append(row)

    team_rows = []
    for tm in nfl_teams:
        d = by_team.get(tm, [])
        p_att = sum(x["p_att"] for x in d)
        cmp_sum = sum(x["cmp"] for x in d)
        team_rows.append(
            {
                "team": tm,
                "off_yd": sum(x["p_yd"] + x["r_yd"] for x in d),
                "p_yd": sum(x["p_yd"] for x in d),
                "car": sum(x["car"] for x in d),
                "r_yd": sum(x["r_yd"] for x in d),
                "r_td": sum(x["r_td"] for x in d),
                "p_ff": sum(x["f_ppr"] for x in d),
                "p_att": p_att,
                "cmp_pct": (cmp_sum / p_att) if p_att > 0 else "",
                "p_td": sum(x["p_td"] for x in d),
                "int": sum(x["int"] for x in d),
                "fmb": sum(x["fmb"] for x in d),
            }
        )
    tout = ROOT / f"team_stats_{stats_season}.csv"
    with tout.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "team",
                "off_yd",
                "p_yd",
                "car",
                "r_yd",
                "r_td",
                "p_ff",
                "p_att",
                "cmp_pct",
                "p_td",
                "int",
                "fmb",
            ],
        )
        w.writeheader()
        w.writerows(team_rows)
    print("Wrote", tout)


def main() -> None:
    tpl = read_template_season()
    roster_out = ROOT / f"{tpl}_rosters.csv"
    build_roster_csv(tpl, roster_out)

    stats_season = pick_player_stats_season()
    copy_player_stats(stats_season, ROOT / "player_data.csv")
    write_effective_season(stats_season)
    aggregate_players_and_teams(stats_season)


if __name__ == "__main__":
    main()
