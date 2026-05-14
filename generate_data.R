library(tidyverse)
library(scales)
library(nflreadr)

source("R/config.R", local = TRUE)

message("Running generate_data.R")

df <- read.csv("player_data.csv", check.names = FALSE)

available_seasons <- unique(df$season[df$season_type == "REG"])
available_seasons <- available_seasons[!is.na(available_seasons)]

effective <- if (NFL_STATS_SEASON %in% available_seasons) {
  as.integer(NFL_STATS_SEASON)
} else {
  mx <- max(available_seasons, na.rm = TRUE)
  message(
    "NFL_STATS_SEASON (", NFL_STATS_SEASON,
    ") not found in player_data.csv; using newest REG season: ", mx
  )
  as.integer(mx)
}

writeLines(as.character(effective), file.path("R", ".effective_stats_season"))

players_out <- sprintf("players_%d.csv", effective)
team_stats_out <- sprintf("team_stats_%d.csv", effective)

# Canonical 32 abbreviations (LAR not LA; WAS not WSH)
nfl_teams <- nflreadr::teams_colors_logos()$team_abbr
nfl_teams <- sort(unique(nfl_teams))

df_filtered <- df %>%
  filter(season == effective, season_type == "REG") %>%
  select(
    position, player_name, recent_team, targets, target_share, receptions,
    receiving_yards, receiving_tds, rushing_yards, rushing_tds,
    rushing_fumbles_lost, receiving_fumbles_lost,
    passing_yards, passing_tds, attempts, completions,
    interceptions, fantasy_points, fantasy_points_ppr, sack_fumbles_lost,
    passing_2pt_conversions, rushing_2pt_conversions,
    receiving_2pt_conversions, carries
  )

combined_df <- map_dfr(nfl_teams, function(tm) {
  df_filtered %>%
    filter(.data$recent_team == tm) %>%
    group_by(.data$position, .data$player_name, .data$recent_team) %>%
    summarise(
      pos = unique(.data$position),
      g = n(),
      p_att = sum(.data$attempts, na.rm = TRUE),
      cmp = sum(.data$completions, na.rm = TRUE),
      p_yd = sum(.data$passing_yards, na.rm = TRUE),
      p_td = sum(.data$passing_tds, na.rm = TRUE),
      int = sum(.data$interceptions, na.rm = TRUE),
      car = sum(.data$carries, na.rm = TRUE),
      r_yd = sum(.data$rushing_yards, na.rm = TRUE),
      r_td = sum(.data$rushing_tds, na.rm = TRUE),
      tgt = sum(.data$targets, na.rm = TRUE),
      rec = sum(.data$receptions, na.rm = TRUE),
      rec_yd = sum(.data$receiving_yards, na.rm = TRUE),
      rec_td = sum(.data$receiving_tds, na.rm = TRUE),
      fmb = sum(.data$rushing_fumbles_lost + .data$sack_fumbles_lost, na.rm = TRUE),
      tp_c = sum(
        .data$passing_2pt_conversions + .data$rushing_2pt_conversions +
          .data$receiving_2pt_conversions,
        na.rm = TRUE
      ),
      f_ppr = sum(.data$fantasy_points_ppr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(.data$pos %in% c("QB", "RB", "WR", "TE", "FB")) %>%
    mutate(
      tgt_share = if_else(sum(.data$p_att) > 0, .data$tgt / sum(.data$p_att), NA_real_),
      ypc = if_else(.data$car > 0, .data$r_yd / .data$car, NA_real_),
      ypr = if_else(.data$rec > 0, .data$rec_yd / .data$rec, NA_real_),
      cmp_pct = if_else(.data$p_att > 0, .data$cmp / .data$p_att, NA_real_),
      td_rate = if_else(.data$p_att > 0, .data$p_td / .data$p_att, NA_real_)
    )
}) %>%
  select(-"position") %>%
  mutate(recent_team = if_else(.data$recent_team == "LA", "LAR", .data$recent_team))

readr::write_csv(combined_df, players_out)
message("Wrote ", players_out)

team_stats_df <- combined_df

combined_df2 <- map_dfr(nfl_teams, function(tm) {
  d <- team_stats_df %>% filter(.data$recent_team == tm)
  pa <- sum(d$p_att, na.rm = TRUE)
  tibble(
    team = tm,
    off_yd = sum(d$p_yd, na.rm = TRUE) + sum(d$r_yd, na.rm = TRUE),
    p_yd = sum(d$p_yd, na.rm = TRUE),
    car = sum(d$car, na.rm = TRUE),
    r_yd = sum(d$r_yd, na.rm = TRUE),
    r_td = sum(d$r_td, na.rm = TRUE),
    p_ff = sum(d$f_ppr, na.rm = TRUE),
    p_att = pa,
    cmp_pct = if (pa > 0) sum(d$cmp, na.rm = TRUE) / pa else NA_real_,
    p_td = sum(d$p_td, na.rm = TRUE),
    int = sum(d$int, na.rm = TRUE),
    fmb = sum(d$fmb, na.rm = TRUE)
  )
})

readr::write_csv(combined_df2, team_stats_out)
message("Wrote ", team_stats_out)
message("generate_data.R complete (reference season ", effective, ")")
