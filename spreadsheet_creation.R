library(tidyverse)
library(openxlsx)
library(nflreadr)

source("R/config.R", local = TRUE)

message("Running spreadsheet_creation.R")

nfl_teams <- sort(unique(teams_colors_logos()$team_abbr))

players_path <- PLAYERS_CSV()
team_stats_path <- TEAM_STATS_CSV()
roster_path <- ROSTER_CSV

drop_index_col <- function(d) {
  if ("X" %in% names(d)) {
    dplyr::select(d, -"X")
  } else {
    d
  }
}

df <- read.csv(players_path, check.names = FALSE) %>% drop_index_col()
roster <- read.csv(roster_path, check.names = FALSE) %>% drop_index_col()
glance <- read.csv(team_stats_path, check.names = FALSE) %>% drop_index_col()

roster <- roster %>%
  mutate(team = if_else(.data$team == "WSH", "WAS", .data$team))

proj_zero <- function(d) {
  d %>%
    mutate(
      g = 0, p_att = 0, cmp = 0, p_yd = 0, p_td = 0, int = 0,
      car = 0, r_yd = 0, r_td = 0, tgt = 0, rec = 0, rec_yd = 0,
      rec_td = 0, fmb = 0, tp_c = 0, f_ppr = 0, tgt_share = 0,
      ypc = 0, ypr = 0, cmp_pct = 0, td_rate = 0, f_custom = 0
    ) %>%
    drop_index_col()
}

wb <- createWorkbook()
for (tm in nfl_teams) {
  # Prior-year player totals (reference season from pipeline)
  team_df <- df %>%
    filter(.data$recent_team == tm)

  addWorksheet(wb, sheetName = tm)
  writeDataTable(wb, sheet = tm, x = team_df, startRow = 1, startCol = 1)

  # Team offense at a glance (same reference season)
  at_glance <- glance %>%
    filter(.data$team == tm)

  writeDataTable(wb, sheet = tm, x = at_glance, startRow = 30, startCol = 1)

  # Blank template row block for the upcoming season projection
  glance_tpl <- data.frame(
    team = character(), off_yd = numeric(), p_yd = numeric(), car = numeric(),
    r_yd = numeric(), r_td = numeric(), p_ff = numeric(), p_att = numeric(),
    cmp_pct = numeric(), p_td = numeric(), int = numeric(), fmb = numeric()
  )

  writeDataTable(wb, sheet = tm, x = glance_tpl, startRow = 32, startCol = 1)

  qb_roster <- roster %>%
    filter(.data$team == tm, .data$POS == "QB") %>%
    proj_zero()
  writeDataTable(wb, sheet = tm, x = qb_roster, startRow = 35, startCol = 1)

  rb_roster <- roster %>%
    filter(.data$team == tm, .data$POS == "RB") %>%
    proj_zero()
  writeDataTable(wb, sheet = tm, x = rb_roster, startRow = 45, startCol = 1)

  wr_roster <- roster %>%
    filter(.data$team == tm, .data$POS == "WR") %>%
    proj_zero()
  writeDataTable(wb, sheet = tm, x = wr_roster, startRow = 60, startCol = 1)

  te_roster <- roster %>%
    filter(.data$team == tm, .data$POS == "TE") %>%
    proj_zero()
  writeDataTable(wb, sheet = tm, x = te_roster, startRow = 75, startCol = 1)
}

out_xlsx <- sprintf("custom_projections_%dV1.xlsx", NFL_TEMPLATE_SEASON)
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("spreadsheet_creation.R complete → ", out_xlsx)
