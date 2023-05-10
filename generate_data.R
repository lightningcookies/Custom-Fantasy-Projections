library(tidyverse)
library(scales)

# Read player data in
df <- read.csv("player_data.csv")

# team names

nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")


for(team in nfl_teams){
  QB_df <- df %>%
    filter(position == "QB",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(attempts = sum(attempts),
              completions = sum(completions),
              comp_pct = completions/attempts,
              pass_yd = sum(passing_yards),
              pass_td = sum(passing_tds),
              int = sum(interceptions),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              fumbles_lost = sum(rushing_fumbles_lost + sack_fumbles_lost),
              fantasy_points = sum(fantasy_points),
              weeks_played = n())%>%
    mutate(comp_pct = percent(comp_pct, accuracy = 0.01)) %>%
    arrange(desc(fantasy_points))%>%
    mutate(calc_fantasy_unfinished = (rush_td * 6)
           + (pass_td * 6) + (.1 * rush_yd) + (.025 * pass_yd))

  RB_df <- df %>%
    filter(position == "RB",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(carries = sum(carries),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              receptions = sum(receptions),
              receiving_yards = sum(receiving_yards),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_points = sum(fantasy_points),
              weeks_played = n())%>%
    arrange(desc(fantasy_points))%>%
    filter(carries > 5)
  
  TE_df <- df %>%
    filter(position == "TE",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(weeks_played = n(),
              receptions = sum(receptions),
              rec_yds = sum(receiving_yards),
              rec_tds = sum(receiving_tds),
              carries = sum(carries),
              rush_yards = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_ppr = sum(fantasy_points_ppr))%>%
    arrange(desc(fantasy_ppr))

  df_join1 <- full_join(QB_df,RB_df)
  df_join2 <- full_join(df_join1, TE_df)
  write.csv(df_join2, paste0(team,".csv"))
  print(df_join2)
}




# # OG ARIzona  only code
# ARI_QB <- df %>%
#   filter(position == "QB",
#          season == 2022,
#          season_type == "REG",
#          recent_team == "ARI")%>%
#   group_by(player_name)%>%
#   summarise(attempts = sum(attempts),
#             completions = sum(completions),
#             comp_pct = completions/attempts,
#             pass_yd = sum(passing_yards),
#             pass_td = sum(passing_tds),
#             int = sum(interceptions),
#             rush_yd = sum(rushing_yards),
#             rush_td = sum(rushing_tds),
#             fumbles_lost = sum(rushing_fumbles_lost + sack_fumbles_lost),
#             fantasy_points = sum(fantasy_points),
#             weeks_played = n())%>%
#   mutate(comp_pct = percent(comp_pct, accuracy = 0.01)) %>%
#   arrange(desc(fantasy_points))%>%
#   mutate(calc_fantasy_unfinished = (rush_td * 6) + (pass_td * 6) + (.1 * rush_yd) + (.025 * pass_yd))%>%
#   view()
# 
# ARI_RB <- df %>%
#   filter(position == "RB",
#          season == 2022,
#          season_type == "REG",
#          recent_team == "ARI")%>%
#   group_by(player_name)%>%
#   summarise(carries = sum(carries),
#             rush_yd = sum(rushing_yards),
#             rush_td = sum(rushing_tds),
#             receptions = sum(receptions),
#             receiving_yards = sum(receiving_yards),
#             fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
#             fantasy_points = sum(fantasy_points),
#             weeks_played = n())%>%
#   arrange(desc(fantasy_points))%>%
#   filter(carries > 5)%>%
#   view()
# 
# ARI_WR <- df %>%
#   filter(position == "WR",
#          season == 2022,
#          season_type == "REG",
#          recent_team == "ARI")%>%
#   group_by(player_name)%>%
#   summarise(weeks_played = n(),
#             receptions = sum(receptions),
#             rec_yards = sum(receiving_yards),
#             rec_td = sum(receiving_tds),
#             carries = sum(carries),
#             rush_yards = sum(rushing_yards),
#             rush_td = sum(rushing_tds),
#             fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
#             fantasy_ppr = sum(fantasy_points_ppr))%>%
#   arrange(desc(fantasy_ppr))%>%
#   view()
# 
# ARI_TE <- df %>%
#   filter(position == "TE",
#          season == 2022,
#          season_type == "REG",
#          recent_team == "ARI")%>%
#   group_by(player_name)%>%
#   summarise(weeks_played = n(),
#             receptions = sum(receptions),
#             rec_yds = sum(receiving_yards),
#             rec_tds = sum(receiving_tds),
#             carries = sum(carries),
#             rush_yards = sum(rushing_yards),
#             rush_td = sum(rushing_tds),
#             fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
#             fantasy_ppr = sum(fantasy_points_ppr)
#   )%>%
#   arrange(desc(fantasy_ppr))%>%
#   view()
# 
# df_join1 <- full_join(ARI_QB,ARI_RB)
# df_join2 <- full_join(df_join1, ARI_TE)
# write.csv(df_join2, "ARI.csv")
# 
# #view(df_join2)
