# Full pipeline: nflreadr rosters + nflfastR player stats + aggregates + workbook.
# Optional CSV-only refresh (no R): python3 scripts/nflverse_snapshot.py
library(nflfastR)
library(readr)

source("R/config.R", local = TRUE)

# 1. Generate rosters (nflreadr → CSV)
source("rosters.R")

# 2. Player-level stats from nflverse (nflfastR)
stats <- load_player_stats(seasons = NFL_STATS_SEASON)
write_csv(stats, "player_data.csv")
message("Wrote player_data.csv (seasons requested: ", NFL_STATS_SEASON, ")")

# 3. Team / player aggregates for the spreadsheet pipeline
source("generate_data.R")

# 4. Generate spreadsheet workbook
source("spreadsheet_creation.R")
