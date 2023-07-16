library(tidyverse)
library(openxlsx)
# setup
nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LA", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")

df <- read.csv("players_2022.csv")
roster <- read.csv("2023_rosters.csv")
glance <- read.csv("team_stats_2022.csv")


# start
wb <- createWorkbook()
for(team_ in nfl_teams){
  # 2022 results insertion
  team_df <- df %>% 
    filter(recent_team == team_)%>%
    select(-X)
    
  addWorksheet(wb, sheetName = team_)
  writeDataTable(wb, sheet = team_, x = team_df, startRow = 1,startCol = 1)
  # 2022-23 season at a glance
  at_glance <- glance %>%
    filter(team == team_)%>%
    select(-X)
  
  writeDataTable(wb, sheet = team_, x = at_glance, startRow = 30, startCol = 1)
  #template for 2023-24 at a glance
  glance_23 <- data.frame(
    team = character(),off_yd = numeric(),p_yd = numeric(),car = numeric(),
    r_yd = numeric(),r_td = numeric(),p_ff = numeric(),p_att = numeric(),
    cmp_pct = numeric(),p_td = numeric(),int = numeric(),fmb = numeric())
  
  writeDataTable(wb, sheet = team_, x = glance_23, startRow = 32, startCol = 1)
  # 2023-24 rosters insertion and formatting?
  team_roster <- roster %>%
    filter(team == team_)%>%
    add_column(g = 0, p_att = 0, cmp = 0, p_yd = 0, p_td = 0, int = 0,
               car = 0, r_yd = 0, r_td = 0, tgt = 0, rec = 0, rec_yd = 0,
               rec_td = 0, fmb = 0, tp_c = 0, f_ppr = 0, tgt_share = 0,
               ypc = 0, ypr = 0, cmp_pct = 0, td_rate = 0, f_custom = 0)%>%
    select(-X)
  writeDataTable(wb, sheet = team_, x = team_roster,startRow = 35,startCol = 1)
}

# Saving the workbook to test_xlsx.xlsx
saveWorkbook(wb, "test_xlsx.xlsx",overwrite = T)
