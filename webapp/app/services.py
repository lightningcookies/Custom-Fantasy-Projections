"""Load roster / reference CSVs and match players for the UI."""

from __future__ import annotations

import csv
from typing import Any

STAT_KEYS = [
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


def default_stats() -> dict[str, float]:
    return {k: 0.0 for k in STAT_KEYS}


def fantasy_ppr_simple(p: dict[str, Any]) -> float:
    """Rough PPR-style total for sorting combined boards (not league-specific)."""
    return (
        float(p.get("p_yd") or 0) * 0.04
        + float(p.get("p_td") or 0) * 4
        + float(p.get("int") or 0) * -1
        + float(p.get("r_yd") or 0) * 0.1
        + float(p.get("r_td") or 0) * 6
        + float(p.get("rec") or 0) * 1.0
        + float(p.get("rec_yd") or 0) * 0.1
        + float(p.get("rec_td") or 0) * 6
        + float(p.get("tp_c") or 0) * 2
        - float(p.get("fmb") or 0) * 2
    )


def _load_csv(path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def load_rosters(path) -> list[dict[str, str]]:
    return _load_csv(path)


def load_reference_players(path) -> list[dict[str, str]]:
    return _load_csv(path)


def _norm(s: str) -> str:
    return s.lower().replace(".", " ").strip()


def reference_team_code(team: str) -> str:
    """Roster CSV uses LA for the Rams; nflverse player stats use LAR."""
    t = team.upper()
    return "LAR" if t == "LA" else t


def match_reference(
    refs: list[dict[str, str]], team: str, pos: str, display_name: str
) -> dict[str, Any] | None:
    """Best-effort link roster display names to nflverse abbreviated player_name."""
    ref_team = reference_team_code(team)
    pos = pos.upper()
    candidates = [
        r
        for r in refs
        if (r.get("recent_team") or "").upper() == ref_team and (r.get("pos") or "").upper() == pos
    ]
    if not candidates:
        return None

    dlast = _norm(display_name).split()[-1] if display_name.strip() else ""
    if not dlast:
        return None

    for r in candidates:
        pn = _norm(r.get("player_name") or "")
        if dlast in pn.split():
            return _reference_stats_row(r)
        if dlast[:5] in pn.replace(" ", ""):
            return _reference_stats_row(r)
    return None


def _reference_stats_row(row: dict[str, str]) -> dict[str, Any]:
    out: dict[str, Any] = {}
    for k in STAT_KEYS:
        v = row.get(k)
        if v is None or v == "":
            out[k] = None
            continue
        try:
            out[k] = float(v) if "." in str(v) else int(v)
        except ValueError:
            out[k] = None
    return out


def roster_by_team(rosters: list[dict[str, str]], team: str) -> list[dict[str, str]]:
    t = team.upper()
    return [r for r in rosters if (r.get("team") or "").upper() == t]


def all_teams(rosters: list[dict[str, str]]) -> list[str]:
    return sorted({(r.get("team") or "").upper() for r in rosters if r.get("team")})
