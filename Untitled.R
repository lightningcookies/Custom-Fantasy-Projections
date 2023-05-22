library(tidyverse)

# By position ranks
for(team in nfl_teams){
  #QB
  #RB
  #WR
}

test_df <- read.csv("team_data/LA.csv")


# Combine all data into one CSV
csv_files <- list.files("./team_data", pattern = "*.csv")
setwd("./team_data")
df_list <- lapply(csv_files, read_csv)
df <- bind_rows(df_list)
setwd("..")
write_csv(df, "combined_data.csv")


# Combined Df
df_c <- read.csv("combined_data.csv")


# By position finish rankings
qb_c <- df_c %>%
  filter(position == "QB")%>%
  group_by(player_name)%>%
  arrange(desc(fantasy_ppr))%>%
  view()

rb_c <- df_c %>%
  filter(position == "RB")%>%
  group_by(player_name)%>%
  arrange(desc(fantasy_ppr))%>%
  view()

wr_c <- df_c %>%
  filter(position == "WR")%>%
  group_by(player_name)%>%
  arrange(desc(fantasy_ppr))%>%
  view()



