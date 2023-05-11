library(tidyverse)
library(scales)

# Read player data in
df <- read.csv("player_data.csv")

# team names

nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")
# scoring parameters
pr <- .5
ptd <- 4
pass_yd_pt <- .04
int_point <- (-1)
tp_conv <- 2
fum_lost <- (-2)

for(team in nfl_teams){
  QB_df <- df %>%
    filter(position == "QB",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              attempts = sum(attempts),
              completions = sum(completions),
              comp_pct = completions/attempts,
              pass_yd = sum(passing_yards),
              pass_td = sum(passing_tds),
              int = sum(interceptions),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              fumbles_lost = sum(rushing_fumbles_lost + sack_fumbles_lost),
              two_point = sum(passing_2pt_conversions + rushing_2pt_conversions),
              fantasy_points = sum(fantasy_points),
              weeks_played = n())%>%
    mutate(comp_pct = percent(comp_pct, accuracy = 0.01)) %>%
    arrange(desc(fantasy_points))%>%
    mutate(calc_fantasy_unfinished = (rush_td * 6) +
             (.1 * rush_yd) + 
             (pass_td * ptd) +
             (pass_yd * pass_yd_pt) +
             (int * int_point)+
             (two_point * tp_conv)+
             (fumbles_lost * fum_lost))

  RB_df <- df %>%
    filter(position == "RB",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              carries = sum(carries),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              trgt = sum(targets),
              receptions = sum(receptions),
              receiving_yards = sum(receiving_yards),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_points = sum(fantasy_points),
              weeks_played = n())%>%
    mutate(trgt_share = trgt/sum(QB_df$attempts))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(fantasy_points))
  
  WR_df <- df %>%
    filter(position == "WR",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              weeks_played = n(),
              trgt = sum(targets),
              receptions = sum(receptions),
              rec_yards = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              carries = sum(carries),
              rush_yards = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_ppr = sum(fantasy_points_ppr))%>%
    mutate(trgt_share = trgt/sum(QB_df$attempts))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(fantasy_ppr))
  
  TE_df <- df %>%
    filter(position == "TE",
           season == 2022,
           season_type == "REG",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              weeks_played = n(),
              trgt = sum(targets),
              receptions = sum(receptions),
              rec_yds = sum(receiving_yards),
              rec_tds = sum(receiving_tds),
              carries = sum(carries),
              rush_yards = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_ppr = sum(fantasy_points_ppr))%>%
    mutate(trgt_share = trgt/sum(QB_df$attempts))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(fantasy_ppr))

  df_join1 <- full_join(QB_df,RB_df)
  df_join2 <- full_join(df_join1, TE_df)
  df_join3 <- full_join(df_join2, WR_df)
  filename <- paste0("team_data/", team, ".csv")
  write.csv(df_join3, filename)
}

# By position ranks
for(team in nfl_teams){
  #QB
  #RB
  #WR
}

test_df <- read.csv("team_data/CIN.csv")

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
#             trgt = sum(targets),
#             receptions = sum(receptions),
#             receiving_yards = sum(receiving_yards),
#             fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
#             fantasy_points = sum(fantasy_points),
#             weeks_played = n())%>%
#   mutate(tgt_share = trgt/sum(ARI_QB$attempts))%>%
#   mutate(tgt_share = percent(tgt_share, accuracy = 0.01))%>%
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
#             trgt = sum(targets),
#             receptions = sum(receptions),
#             rec_yards = sum(receiving_yards),
#             rec_td = sum(receiving_tds),
#             carries = sum(carries),
#             rush_yards = sum(rushing_yards),
#             rush_td = sum(rushing_tds),
#             fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
#             fantasy_ppr = sum(fantasy_points_ppr))%>%
#   mutate(tgt_share = trgt/sum(ARI_QB$attempts))%>%
#   mutate(tgt_share = percent(tgt_share, accuracy = 0.01))%>%
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
