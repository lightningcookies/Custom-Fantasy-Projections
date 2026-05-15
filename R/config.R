# Season configuration — 2026 offseason / 2026–27 NFL league year
# -----------------------------------------------------------------
# * NFL_TEMPLATE_SEASON: workbook + roster export year (upcoming NFL season).
# * NFL_ROSTER_SEASON:    passed to nflreadr::load_rosters() (usually same).
# * NFL_STATS_SEASON:     target year for nflfastR::load_player_stats() (most
#   recent completed regular season). After the Super Bowl, nflverse publishes
#   this file; until then, run generate_data.R and it will fall back to the
#   newest season present in player_data.csv and record it in
#   R/.effective_stats_season.

NFL_TEMPLATE_SEASON <- 2026L
NFL_ROSTER_SEASON <- NFL_TEMPLATE_SEASON
NFL_STATS_SEASON <- 2025L

ROSTER_CSV <- sprintf("%d_rosters.csv", NFL_TEMPLATE_SEASON)
PLAYERS_CSV <- function() {
  sprintf("players_%d.csv", effective_stats_season())
}
TEAM_STATS_CSV <- function() {
  sprintf("team_stats_%d.csv", effective_stats_season())
}

effective_stats_season <- function() {
  path <- file.path("R", ".effective_stats_season")
  if (file.exists(path)) {
    v <- suppressWarnings(as.integer(readLines(path, n = 1)[1]))
    if (!is.na(v)) {
      return(v)
    }
  }
  as.integer(NFL_STATS_SEASON)
}
