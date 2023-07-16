library(tidyverse)
library(rvest)
library(openxlsx)

# rosters
nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LA", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")
source("rosters.R")

#Notes
#missing file that creates 2023_rosters.csv

# re-look at the OG dataset, it looks like it might include things like target share

# change names of dataframes to avoid conflicts. 

# need to finish off the excel sheet on PC. Need to use power query. 

# could write script for when you're done with your projections, you can export your
#top 200 a sa csv or something like that. 
