# Custom Fantasy Projections

**Full setup, deploy, and self-hosting:** see **[docs/SETUP.md](docs/SETUP.md)**.

R-based pipeline that builds a **team-by-team Excel workbook** for NFL fantasy projections. The 2026 offseason refresh targets the **2026 NFL season** (fall 2026–early 2027) using **nflverse** libraries instead of scraping ESPN HTML.

## What you get

1. **Reference player totals** from `nflfastR::load_player_stats()` (regular season) for the season configured in `R/config.R` (defaults to **2025**; if that release is not in your `player_data.csv` yet, `generate_data.R` falls back to the newest season present and records it in `R/.effective_stats_season`).
2. **Current-season rosters** (QB, RB, WR, TE) from **`nflreadr::load_rosters()`** — the same nflverse release files as the website, not `rvest`.
3. **Team offensive rollups** derived from those player totals.
4. **`custom_projections_2026V1.xlsx`**: one sheet per franchise with last year’s stats, a blank “at a glance” block for your **2026–27** projections, and roster rows zeroed for you to fill in.

## R pipeline (recommended)

Install dependencies (once):

```r
install.packages(c("tidyverse", "nflreadr", "nflfastR", "openxlsx", "readr", "scales"))
```

From the project root:

```r
source("main.R")
```

That runs, in order: `rosters.R` → `load_player_stats` → `generate_data.R` → `spreadsheet_creation.R`.

Seasons and filenames are controlled in **`R/config.R`** (`NFL_TEMPLATE_SEASON`, `NFL_ROSTER_SEASON`, `NFL_STATS_SEASON`).

## Refresh CSVs without R (optional)

If you only have Python 3, you can pull the same nflverse **static release** CSVs and rebuild `player_data.csv`, `2026_rosters.csv`, `players_*.csv`, and `team_stats_*.csv`:

```bash
python3 scripts/nflverse_snapshot.py
```

To prefer a specific stats year when GitHub has it (e.g. after the Super Bowl):

```bash
NFLVERSE_STATS_SEASON=2025 python3 scripts/nflverse_snapshot.py
```

## Excel template and macros

1. Download **`custom_projections_2026V1.xlsx`** from this repo (when you have generated it locally, or once it is attached to a release).
2. Open in Microsoft Excel. Enable macros only if you trust the workbook; the VBA under `VBA macros/` automates consolidation and styling.
3. Fill in projections team by team, then run your **Refresh** workflow as described in the original template instructions.

Useful references while projecting: [DraftKings player props](https://sportsbook.draftkings.com/leagues/football/nfl?category=player-stats) and [FantasyPros projections](https://www.fantasypros.com/nfl/projections/rb.php?week=draft).

## Web app (Excel replacement, MVP)

The **`webapp/`** folder is a **FastAPI + SQLite** service plus a small static UI:

- Editable projection cells per team (same stat columns as `players_*.csv`), saved to SQLite with debounced PATCH requests.
- **Combined QB/RB/WR/TE** board across all teams (replaces the “Combined\_\*” / “Overall” macro consolidation), sorted by a simple estimated PPR.
- **Reference** columns join best-effort from the bundled reference CSV when names match.

Run locally:

```bash
cd webapp
pip install -r requirements.txt
python3 -m uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

Open `http://localhost:8000/`, create a **New workbook**, pick a team, edit cells. Configuration uses env prefix **`CFP_`** (see `webapp/app/main.py`): `CFP_REPO_ROOT`, `CFP_ROSTER_CSV`, `CFP_REFERENCE_CSV`, `CFP_DB_PATH`.

Details: `webapp/HOSTING.txt`.

## Static reference dashboard (`site/`)

The **`site/`** folder is a **static** read-only dashboard (rosters, prior-season fantasy totals, team offense tables from bundled JSON — no projection editing). Build data with `npm install && npm run build`, preview with `npm run serve`. Publish the **`site`** directory to any static host; see **`site/HOSTING.txt`**.

## R package metadata

A minimal **`DESCRIPTION`** lists Imports for `devtools::install_deps()` or `renv` workflows.

---

This started as a 2023 hobby project; the data layer is now aligned with **nflreadr / nflfastR** for reproducibility and easier updates each offseason.
