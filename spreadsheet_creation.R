library(tidyverse)
library(openxlsx)

nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LA", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")

df <- read.csv("players_2022.csv")

wb <- createWorkbook()

for(team in nfl_teams){
  team_df <- df %>% 
    filter(recent_team == team)
  
  addWorksheet(wb, sheetName = team)
  writeDataTable(wb, sheet = team, x = team_df)
  
}

saveWorkbook(wb, "test_xlsx.xlsx",overwrite = T)

# Notes

# *Need to add a second table where I'll eventually fill in my projections. It would be nice if this was automated with the rosters. 
# *Then I'll create a "master" page at the beginning of the sheet that takes all my projections and aggregates them into a top 200
# *Then I can fish all the stats off of that. 

# Example

# Write the first table to range A1:Z30
#writeDataTable(wb, sheet = "Sheet1", x = df1, tableStyle = "TableStyleMedium9", startRow = 1, startCol = 1, withHeaders = TRUE)

# Write the second table to range A40:Z70
#writeDataTable(wb, sheet = "Sheet1", x = df2, tableStyle = "TableStyleMedium9", startRow = 40, startCol = 1, withHeaders = TRUE)