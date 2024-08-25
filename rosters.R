library(tidyverse)
library(rvest)

nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WSH")

combined_df <- data.frame()

for(team in nfl_teams){
  # rvest functions to get rosters from ESPN.
  url_base = "https://www.espn.com/nfl/team/roster/_/name/"
  url_full = paste0(url_base, team)
  team_ <- team
  page <- read_html(url_full)
  table_node <- html_nodes(page, "table")
  
  df <- html_table(table_node)[[1]]
  df <- df %>%
    select(-where(function(x) all(is.na(x) | x == "")))%>%
    select(Name, POS, Age, Exp, College)%>%
    mutate(Exp = ifelse(Exp == "R", 0, Exp),
           Age = ifelse(Age == "--", NA, Age))
  
  # Get rid of jersey num and add team. Set as.numeric
  df$Name <- str_replace(df$Name,"\\d+$", "")
  df$team <- team
  df$Age <- as.numeric(df$Age)
  df$Exp <- as.numeric(df$Exp)

  combined_df <- bind_rows(combined_df, df)
}
# Combine and only include QB, RB, TE, WR
combined_df <- combined_df %>%
  filter(POS %in% c("QB", "RB", "TE", "WR"))

# Write to "2023_rosters.csv"
write.csv(combined_df, "2024_rosters.csv")
