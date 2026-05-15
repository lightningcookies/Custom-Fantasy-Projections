# Pull season rosters from nflverse (same backend as nflreadr) — no HTML scraping.
library(dplyr)
library(readr)
library(nflreadr)

source("R/config.R", local = TRUE)

rosters_raw <- load_rosters(seasons = NFL_ROSTER_SEASON)

if (!"full_name" %in% names(rosters_raw)) {
  if ("player_name" %in% names(rosters_raw)) {
    rosters_raw <- rosters_raw %>% mutate(full_name = .data$player_name)
  } else {
    rosters_raw <- rosters_raw %>% mutate(
      full_name = paste(.data$first_name, .data$last_name)
    )
  }
}

out <- rosters_raw %>%
  filter(.data$position %in% c("QB", "RB", "WR", "TE")) %>%
  mutate(
    Exp_raw = suppressWarnings(as.character(.data$years_exp)),
    Exp = dplyr::case_when(
      is.na(.data$Exp_raw) | .data$Exp_raw == "" ~ 0,
      grepl("^[Rr]$", .data$Exp_raw) ~ 0,
      TRUE ~ suppressWarnings(as.numeric(.data$Exp_raw))
    ),
    Exp = dplyr::coalesce(.data$Exp, 0),
    birth_date = suppressWarnings(as.Date(.data$birth_date)),
    Age = dplyr::if_else(
      !is.na(.data$birth_date),
      as.integer(floor(
        as.numeric(difftime(Sys.Date(), .data$birth_date, units = "days")) / 365.25
      )),
      NA_integer_
    ),
    Name = dplyr::coalesce(
      .data$full_name,
      paste(.data$first_name, .data$last_name)
    ),
    POS = .data$position,
    College = dplyr::if_else(
      is.na(.data$college) | .data$college == "",
      "--",
      as.character(.data$college)
    ),
    team = .data$team
  ) %>%
  dplyr::distinct(.data$gsis_id, .data$team, .keep_all = TRUE) %>%
  transmute(
    Name = .data$Name,
    POS = .data$POS,
    Age = .data$Age,
    Exp = as.numeric(.data$Exp),
    College = .data$College,
    team = .data$team
  ) %>%
  arrange(.data$team, .data$POS, .data$Name)

write_csv(out, ROSTER_CSV)
message("Wrote ", nrow(out), " rows to ", ROSTER_CSV)
