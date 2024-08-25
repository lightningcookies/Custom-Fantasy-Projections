library(tidyverse)
library(rvest)
library(openxlsx)
library(nflfastR)

# 1. Generate rosters
# source("rosters.R")

# 2. Generate player stats
stats <- load_player_stats(seasons = 2023)
df <- write.csv(stats, file = "player_data.csv")

# 3. Generate Data
source("generate_data.R")

# 4. Generate Spreadsheet
source("spreadsheet_creation.R")



# Notes for future
# change names of dataframes to avoid conflicts? 

# Make it year-proof by changing global variable names?

# could write script for when you're done with your projections, you can export your
# top 200 to csv or something like that.

# integrate formats with underdog or sleeper for comparison?
