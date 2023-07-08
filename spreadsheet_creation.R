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
    filter(recent_team == team_)
    
  addWorksheet(wb, sheetName = team_)
  writeDataTable(wb, sheet = team_, x = team_df, startRow = 1,startCol = 1)
  
  # 2023-24 rosters insertion and formatting?
  team_roster <- roster %>%
    filter(team == team_)
  
  writeDataTable(wb, sheet = team_, x = team_roster,startRow = 35,startCol = 1)
  
  # 2022-23 season at a glance
  at_glance <- glance %>%
    filter(team == team_)
  
  writeDataTable(wb, sheet = team_, x = at_glance, startRow = 30, startCol = 1)
}
saveWorkbook(wb, "test_xlsx.xlsx",overwrite = T)


view()

# Notes

# *Need to add a second table where I'll eventually fill in my projections. It would be nice if this was automated with the rosters. 
# *Then I'll create a "master" page at the beginning of the sheet that takes all my projections and aggregates them into a top 200
# *Then I can fish all the stats off of that. 
# *Format the tables so that each different position is different color for ease.
# Example

# Write the first table to range A1:Z30
#writeDataTable(wb, sheet = "Sheet1", x = df1, tableStyle = "TableStyleMedium9", startRow = 1, startCol = 1, withHeaders = TRUE)

# Write the second table to range A40:Z70
#writeDataTable(wb, sheet = "Sheet1", x = df2, tableStyle = "TableStyleMedium9", startRow = 40, startCol = 1, withHeaders = TRUE)