# Custom Fantasy Projections — complete setup guide

This document is the single place for **installing**, **running**, and **deploying** everything in this repository: the R data pipeline, optional Python CSV refresh, the **read-only static dashboard** (`site/`), and the **editable web app** (`webapp/`).

---

## 1. What lives in this repo

| Piece | Purpose | Needs a server? |
|--------|---------|------------------|
| **R scripts** (`main.R`, `rosters.R`, …) | Regenerate CSVs + `custom_projections_*V1.xlsx` | No (your laptop is fine) |
| **`scripts/nflverse_snapshot.py`** | Refresh CSVs from nflverse URLs without R | No |
| **`site/`** | Static HTML/JS + JSON — browse rosters & reference stats | Only a static file host (or any HTTP server) |
| **`webapp/`** | FastAPI + SQLite — editable projections in the browser | Yes, if you want it always-on |

---

## 2. Prerequisites

**R pipeline**

- R 4.x
- Packages (install once in R):

  ```r
  install.packages(c("tidyverse", "nflreadr", "nflfastR", "openxlsx", "readr", "scales"))
  ```

  Or: `devtools::install_deps()` / `renv::restore()` using root **`DESCRIPTION`**.

**Static site build**

- Node.js 18+ and npm

**Web app**

- Python 3.10+ recommended  
- `pip install -r webapp/requirements.txt`

---

## 3. Configuration: seasons and files

Edit **`R/config.R`**:

- **`NFL_TEMPLATE_SEASON`** — roster / workbook “target” year (e.g. 2026).
- **`NFL_ROSTER_SEASON`** — passed to `nflreadr::load_rosters()`.
- **`NFL_STATS_SEASON`** — year you want for `nflfastR::load_player_stats()`.

If that stats year is not in `player_data.csv` yet, **`generate_data.R`** falls back to the newest regular season in the file and writes the actual year to **`R/.effective_stats_season`**.

The **static site** build reads **`R/config.R`** and **`R/.effective_stats_season`** via **`npm run build`** so JSON stays aligned.

---

## 4. R pipeline (full refresh from nflverse)

From the **repository root**:

```r
source("main.R")
```

Order of operations:

1. `rosters.R` — `nflreadr::load_rosters()` → `*_rosters.csv`
2. `nflfastR::load_player_stats()` → `player_data.csv`
3. `generate_data.R` → `players_*.csv`, `team_stats_*.csv`
4. `spreadsheet_creation.R` → **`custom_projections_<year>V1.xlsx`**

Regenerate the Excel file whenever you change seasons or want fresh nflverse data.

---

## 5. Refresh CSVs without R (Python)

Uses nflverse **release URLs** (same data the R packages use):

```bash
python3 scripts/nflverse_snapshot.py
```

Prefer a specific completed season once GitHub hosts it:

```bash
NFLVERSE_STATS_SEASON=2025 python3 scripts/nflverse_snapshot.py
```

Then run **`generate_data.R`** / **`spreadsheet_creation.R`** in R if you need the workbook updated from the new `player_data.csv`.

---

## 6. Static reference dashboard (`site/`)

**Build** JSON under `site/data/` from root CSVs:

```bash
npm install
npm run build
```

**Preview** (must be over HTTP, not `file://` — browsers block `fetch()` for local JSON):

```bash
npm run serve
```

Open **http://localhost:4173** (or the port `serve` prints).

**Deploy (managed static hosting)** — Netlify, Cloudflare Pages, Vercel, GitHub Pages:

- **Build command:** `npm run build`
- **Publish directory:** `site`
- Or publish only the `site/` folder if it already contains built `site/data/*.json`

---

## 7. Web app — editable projections (`webapp/`)

### Run locally

```bash
cd webapp
pip install -r requirements.txt
python3 -m uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

Open **http://localhost:8000/** — create a workbook, pick a team, edit cells. Data lives in SQLite unless you change **`CFP_DB_PATH`**.

### Environment variables (prefix **`CFP_`**)

| Variable | Meaning | Default |
|----------|---------|---------|
| `CFP_REPO_ROOT` | Path to repo root (parent of `webapp/`) | Auto-detected |
| `CFP_ROSTER_CSV` | Roster filename under repo root | `2026_rosters.csv` |
| `CFP_REFERENCE_CSV` | Reference `players_*.csv` for side-by-side columns | e.g. `players_2024.csv` |
| `CFP_DB_PATH` | SQLite database | `webapp/data/projections.db` |

Example:

```bash
export CFP_DB_PATH=/var/lib/cfp/projections.db
python3 -m uvicorn app.main:app --host 127.0.0.1 --port 8000
```

### HTTP API (for scripting or future mobile client)

- `GET /api/health` — liveness
- `GET /api/meta` — teams, stat keys, configured CSV names
- `GET/POST /api/workbooks` — list / create workbook (seeds rows from roster)
- `DELETE /api/workbooks/{id}`
- `POST /api/workbooks/{id}/reseed` — wipe projections and re-seed from roster
- `GET /api/workbooks/{id}/team/{ARI|BUF|…}`
- `PATCH /api/workbooks/{id}/team/{abbr}/player` — body: `{"player_name":"…","position":"QB","stats":{"p_yd":4100}}`
- `GET /api/workbooks/{id}/combined/{QB|RB|WR|TE}`

**Production notes:** tighten CORS (not `*` on the public internet), add authentication if exposed, back up **`CFP_DB_PATH`**, and run behind a reverse proxy with TLS.

---

## 8. Hosting on your own Linux box + a domain

**Yes, you can host both parts on a home or VPS Linux machine** that has a domain pointed at it.

### Static `site/` only

- Point DNS **A/AAAA** at your server’s public IP (or use a dynamic-DNS provider if your home IP changes).
- Serve `site/` with **nginx**, **Caddy**, or **Apache** as a plain static root, or run **`npx serve site -p 8080`** behind a reverse proxy.
- Get **HTTPS** (Let’s Encrypt **certbot** or **Caddy** automatic HTTPS). Browsers expect TLS for real domains; mixed content and security settings are simpler with HTTPS.

**Worth it vs a managed static host?**

- **Self-host static `site/`:** Fine if you already run a box 24/7; cost is basically domain + power. Downsides: you maintain OS updates, TLS, and (on residential internet) possible **CGNAT** (no inbound connections unless you use a tunnel) or ISP terms.
- **Netlify / Cloudflare Pages / GitHub Pages:** Usually **less work**, with a global CDN and free tiers. For a **read-only** dashboard, managed hosting is often the better **time vs hassle** trade unless you like homelab ops.

### `webapp/` (FastAPI) on your Linux server

Typical pattern:

1. Run **uvicorn** (or **gunicorn** + uvicorn workers) bound to **127.0.0.1:8000**.
2. Put **nginx** or **Caddy** in front: `https://fantasy.example.com` → `proxy_pass http://127.0.0.1:8000`.
3. Use **systemd** (or Docker) to start the app on boot.
4. Persist **`CFP_DB_PATH`** on disk; include it in backups.

**Worth it vs PaaS (Railway, Fly.io, etc.)?**

- **Self-host:** You control data (SQLite on your disk), no vendor for a tiny app, great for **LAN**, **Tailscale**, or **household-only** use. You own patching, monitoring, and security.
- **Managed PaaS:** Less ops, easy HTTPS and deploy hooks; may cost money or use free-tier limits. Often simpler for **public** access without opening home router ports.

**Summary:** Self-hosting on Linux is **absolutely viable** and can be **worth it** if you already run that server and want data local or private access. For a **public** site with minimal maintenance, **managed static hosting** (and optionally a small VPS for the API) is often simpler than exposing your home network.

---

## 9. Quick checklist before “production”

- [ ] Run **`npm run build`** so `site/data/` matches current CSVs and **`R/.effective_stats_season`**
- [ ] Run **`source("main.R")`** when nflverse publishes a new season of `player_stats`
- [ ] Back up **`webapp/data/projections.db`** (or your `CFP_DB_PATH`)
- [ ] Use HTTPS and sensible firewall rules if the web app is on the public internet
- [ ] Replace wide-open CORS and add auth when strangers can reach the API

---

## 10. Where else to look

- Root **`README.md`** — short overview; this file is the long form.
- **`site/HOSTING.txt`** — static deploy reminders.
- **`webapp/HOSTING.txt`** — API list and env vars.
- **`R/config.R`** — season knobs.
- **`VBA macros/`** — legacy Excel automation (if you still use the workbook).
