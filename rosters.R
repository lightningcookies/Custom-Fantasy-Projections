library(tidyverse)
library(rvest)

nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WSH")

combined_df <- data.frame()

for(team in nfl_teams){
  url_base = "https://www.espn.com/nfl/team/roster/_/name/"
  url_full = paste0(url_base, team)
  team_ <- team
  page <- read_html(url_full)
  table_node <- html_nodes(page, "table")
  df <- html_table(table_node)[[1]]
  df <- df %>%
    select(-where(function(x) all(is.na(x) | x == "")))%>%
    select(Name, POS, Age, Exp, College)
  
  # Get rid of jersey num and add team. 
  df$Name <- str_replace(df$Name,"\\d+$", "")
  df$team <- team

  
  combined_df <- bind_rows(combined_df, df)
}
view(combined_df)
combined_df <- combined_df %>%
  filter(POS %in% c("QB", "RB", "TE", "WR"))

write.csv(combined_df, "2023_rosters.csv")







