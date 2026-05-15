"""SQLite persistence for projection workbooks."""

from __future__ import annotations

import json
import sqlite3
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Generator


def connect(db_path: Path) -> sqlite3.Connection:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path, check_same_thread=False)
    conn.row_factory = sqlite3.Row
    return conn


def init_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS workbook (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL DEFAULT 'My projections',
            created_at TEXT NOT NULL
        );

        CREATE TABLE IF NOT EXISTS projection_entry (
            workbook_id TEXT NOT NULL,
            team TEXT NOT NULL,
            player_name TEXT NOT NULL,
            position TEXT NOT NULL,
            stats_json TEXT NOT NULL,
            updated_at TEXT NOT NULL,
            PRIMARY KEY (workbook_id, team, player_name, position),
            FOREIGN KEY (workbook_id) REFERENCES workbook(id) ON DELETE CASCADE
        );

        CREATE INDEX IF NOT EXISTS idx_projection_workbook_team
            ON projection_entry(workbook_id, team);
        """
    )
    conn.commit()


@contextmanager
def get_conn(db_path: Path) -> Generator[sqlite3.Connection, None, None]:
    conn = connect(db_path)
    try:
        yield conn
    finally:
        conn.close()


def row_to_dict(row: sqlite3.Row) -> dict[str, Any]:
    return {k: row[k] for k in row.keys()}
