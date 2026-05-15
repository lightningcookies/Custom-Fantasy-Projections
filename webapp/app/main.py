"""FastAPI backend: editable projections + combined boards (macro replacement)."""

from __future__ import annotations

import json
import sqlite3
import uuid
from collections.abc import Generator
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from fastapi import Depends, FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel
from pydantic_settings import BaseSettings, SettingsConfigDict

from .database import get_conn, init_schema
from .services import (
    STAT_KEYS,
    all_teams,
    default_stats,
    fantasy_ppr_simple,
    load_reference_players,
    load_rosters,
    match_reference,
    roster_by_team,
)


class Settings(BaseSettings):
    model_config = SettingsConfigDict(env_prefix="CFP_", env_file_encoding="utf-8")

    repo_root: Path = Path(__file__).resolve().parents[2]
    roster_csv: str = "2026_rosters.csv"
    reference_csv: str = "players_2024.csv"
    db_path: Path = Path(__file__).resolve().parents[1] / "data" / "projections.db"


settings = Settings()
app = FastAPI(title="Custom Fantasy Projections", version="0.1.0")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

_state: dict[str, Any] = {}


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def row_as_dict(row: sqlite3.Row) -> dict[str, Any]:
    return {k: row[k] for k in row.keys()}


@app.on_event("startup")
def startup() -> None:
    roster_path = settings.repo_root / settings.roster_csv
    ref_path = settings.repo_root / settings.reference_csv
    _state["rosters"] = load_rosters(roster_path)
    _state["reference"] = load_reference_players(ref_path)
    _state["teams"] = all_teams(_state["rosters"])
    with get_conn(settings.db_path) as conn:
        init_schema(conn)
    if not _state["rosters"]:
        print(f"Warning: no roster rows at {roster_path}")
    if not _state["reference"]:
        print(f"Warning: no reference rows at {ref_path}")


def get_db() -> Generator[sqlite3.Connection, None, None]:
    conn = sqlite3.connect(settings.db_path, check_same_thread=False)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA foreign_keys = ON")
    try:
        yield conn
    finally:
        conn.close()


class WorkbookCreate(BaseModel):
    name: str = "My projections"


@app.get("/api/health")
def health() -> dict[str, str]:
    return {"status": "ok"}


@app.get("/api/meta")
def meta() -> dict[str, Any]:
    return {
        "teams": _state.get("teams", []),
        "statKeys": STAT_KEYS,
        "rosterCsv": settings.roster_csv,
        "referenceCsv": settings.reference_csv,
    }


@app.get("/api/workbooks")
def list_workbooks(conn: sqlite3.Connection = Depends(get_db)) -> list[dict[str, Any]]:
    cur = conn.execute("SELECT id, name, created_at FROM workbook ORDER BY created_at DESC")
    return [row_as_dict(row) for row in cur.fetchall()]


def _seed_workbook(conn: sqlite3.Connection, workbook_id: str) -> int:
    rosters: list = _state.get("rosters") or []
    now = utc_now()
    n = 0
    for r in rosters:
        team = (r.get("team") or "").upper()
        name = (r.get("Name") or "").strip()
        pos = (r.get("POS") or "").strip().upper()
        if not team or not name or not pos:
            continue
        stats = default_stats()
        conn.execute(
            """
            INSERT OR IGNORE INTO projection_entry
            (workbook_id, team, player_name, position, stats_json, updated_at)
            VALUES (?, ?, ?, ?, ?, ?)
            """,
            (workbook_id, team, name, pos, json.dumps(stats), now),
        )
        n += 1
    conn.commit()
    return n


@app.post("/api/workbooks")
def create_workbook(body: WorkbookCreate, conn: sqlite3.Connection = Depends(get_db)) -> dict[str, Any]:
    wid = str(uuid.uuid4())
    now = utc_now()
    conn.execute(
        "INSERT INTO workbook (id, name, created_at) VALUES (?, ?, ?)",
        (wid, body.name.strip() or "My projections", now),
    )
    conn.commit()
    inserted = _seed_workbook(conn, wid)
    return {"id": wid, "name": body.name, "created_at": now, "rows_seeded": inserted}


@app.delete("/api/workbooks/{workbook_id}")
def delete_workbook(workbook_id: str, conn: sqlite3.Connection = Depends(get_db)) -> dict[str, str]:
    conn.execute("DELETE FROM workbook WHERE id = ?", (workbook_id,))
    conn.commit()
    return {"deleted": workbook_id}


@app.post("/api/workbooks/{workbook_id}/reseed")
def reseed_workbook(workbook_id: str, conn: sqlite3.Connection = Depends(get_db)) -> dict[str, Any]:
    row = conn.execute("SELECT 1 FROM workbook WHERE id = ?", (workbook_id,)).fetchone()
    if not row:
        raise HTTPException(404, "workbook not found")
    conn.execute("DELETE FROM projection_entry WHERE workbook_id = ?", (workbook_id,))
    conn.commit()
    n = _seed_workbook(conn, workbook_id)
    return {"workbook_id": workbook_id, "rows_seeded": n}


@app.get("/api/workbooks/{workbook_id}/team/{team}")
def get_team_sheet(workbook_id: str, team: str, conn: sqlite3.Connection = Depends(get_db)) -> dict[str, Any]:
    team = team.upper()
    if team not in _state.get("teams", []):
        raise HTTPException(404, "unknown team")
    wb = conn.execute("SELECT id, name FROM workbook WHERE id = ?", (workbook_id,)).fetchone()
    if not wb:
        raise HTTPException(404, "workbook not found")

    refs = _state.get("reference") or []
    cur = conn.execute(
        """
        SELECT player_name, position, stats_json FROM projection_entry
        WHERE workbook_id = ? AND team = ?
        ORDER BY position, player_name
        """,
        (workbook_id, team),
    )
    proj_map: dict[tuple[str, str], dict[str, Any]] = {}
    for r in cur.fetchall():
        proj_map[(r["player_name"], r["position"])] = json.loads(r["stats_json"])

    players_out: list[dict[str, Any]] = []
    for r in roster_by_team(_state.get("rosters") or [], team):
        name = (r.get("Name") or "").strip()
        pos = (r.get("POS") or "").strip().upper()
        ref = match_reference(refs, team, pos, name)
        proj = proj_map.get((name, pos))
        if proj is None:
            proj = default_stats()
        players_out.append(
            {
                "name": name,
                "pos": pos,
                "age": r.get("Age"),
                "exp": r.get("Exp"),
                "college": r.get("College"),
                "reference": ref,
                "projection": proj,
                "fantasy_ppr_est": fantasy_ppr_simple(proj),
            }
        )
    return {"workbook": row_as_dict(wb), "team": team, "players": players_out}


@app.patch("/api/workbooks/{workbook_id}/team/{team}/player")
def patch_player_projection(
    workbook_id: str,
    team: str,
    body: dict[str, Any],
    conn: sqlite3.Connection = Depends(get_db),
) -> dict[str, Any]:
    team = team.upper()
    player_name = (body.get("player_name") or "").strip()
    position = (body.get("position") or "").strip().upper()
    patch_stats = body.get("stats") or {}
    if not player_name or not position:
        raise HTTPException(400, "player_name and position required")

    row = conn.execute(
        """
        SELECT stats_json FROM projection_entry
        WHERE workbook_id = ? AND team = ? AND player_name = ? AND position = ?
        """,
        (workbook_id, team, player_name, position),
    ).fetchone()
    if not row:
        raise HTTPException(404, "projection row not found")

    current = json.loads(row["stats_json"])
    for k, v in patch_stats.items():
        if k in STAT_KEYS:
            try:
                current[k] = float(v)
            except (TypeError, ValueError) as e:
                raise HTTPException(400, f"invalid numeric value for {k}") from e
    now = utc_now()
    conn.execute(
        """
        UPDATE projection_entry
        SET stats_json = ?, updated_at = ?
        WHERE workbook_id = ? AND team = ? AND player_name = ? AND position = ?
        """,
        (json.dumps(current), now, workbook_id, team, player_name, position),
    )
    conn.commit()
    current["fantasy_ppr_est"] = fantasy_ppr_simple(current)
    return {"ok": True, "projection": current, "updated_at": now}


@app.get("/api/workbooks/{workbook_id}/combined/{position}")
def combined_position(
    workbook_id: str,
    position: str,
    conn: sqlite3.Connection = Depends(get_db),
) -> dict[str, Any]:
    position = position.upper()
    if position not in {"QB", "RB", "WR", "TE"}:
        raise HTTPException(400, "position must be QB, RB, WR, or TE")
    cur = conn.execute(
        """
        SELECT team, player_name, position, stats_json FROM projection_entry
        WHERE workbook_id = ? AND position = ?
        """,
        (workbook_id, position),
    )
    rows: list[dict[str, Any]] = []
    for r in cur.fetchall():
        stats = json.loads(r["stats_json"])
        rows.append(
            {
                "team": r["team"],
                "player_name": r["player_name"],
                "position": r["position"],
                "projection": stats,
                "fantasy_ppr_est": fantasy_ppr_simple(stats),
            }
        )
    rows.sort(key=lambda x: x["fantasy_ppr_est"], reverse=True)
    return {"workbook_id": workbook_id, "position": position, "players": rows}


static_dir = Path(__file__).resolve().parent.parent / "static"
if static_dir.exists():
    app.mount("/", StaticFiles(directory=str(static_dir), html=True), name="static")
