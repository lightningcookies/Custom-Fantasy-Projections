library(tidyverse)
library(scales)

# Read player data in
df <- read.csv("player_data.csv")

# team names

nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
               "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
               "LAC", "LA", "LV", "MIA", "MIN", "NE", "NO", "NYG",
               "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")
# scoring parameters
pr <- .5
ptd <- 4
pass_yd_pt <- .04
int_point <- (-1)
tp_conv <- 2
fum_lost <- (-2)

# filter df for faster performance
df_filtered <- df %>%
  filter(season == 2022,
         season_type == "REG") %>%
  select(position, player_name, recent_team, targets, receptions,
         receiving_yards, receiving_tds, rushing_yards, rushing_tds,
         rushing_fumbles_lost, receiving_fumbles_lost,
         passing_yards, passing_tds, attempts, completions,
         interceptions, fantasy_points, fantasy_points_ppr, sack_fumbles_lost,
         passing_2pt_conversions, rushing_2pt_conversions,
         receiving_2pt_conversions, carries)




# main for loop
for(team in nfl_teams){
  
  #QB
  QB_df <- df_filtered %>%
    filter(position == "QB",
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
              fantasy_ppr = sum(fantasy_points_ppr),
              weeks_played = n())%>%
    mutate(comp_pct = percent(comp_pct, accuracy = 0.01)) %>%
    arrange(desc(fantasy_ppr))%>%
    mutate(calc_fantasy_unfinished = (rush_td * 6) +
             (.1 * rush_yd) + 
             (pass_td * ptd) +
             (pass_yd * pass_yd_pt) +
             (int * int_point)+
             (two_point * tp_conv)+
             (fumbles_lost * fum_lost))
  
# RB
  RB_df <- df_filtered %>%
    filter(position == "RB",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              carries = sum(carries),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              trgt = sum(targets),
              receptions = sum(receptions),
              rec_yd = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              two_point = sum(receiving_2pt_conversions + rushing_2pt_conversions),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_ppr = sum(fantasy_points_ppr),
              weeks_played = n())%>%
    mutate(trgt_share = trgt/sum(QB_df$attempts))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(fantasy_ppr))%>%
    mutate(calc_fantasy_unfinished = (rush_td * 6) +
             (.1 * rush_yd) + 
             (pr * receptions) +
             (.1 * rec_yd) +
             (6 * rec_td) +
             (tp_conv * two_point) +
             (fum_lost * fumbles_lost))
             
  # WR
  WR_df <- df_filtered %>%
    filter(position == "WR",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              weeks_played = n(),
              trgt = sum(targets),
              receptions = sum(receptions),
              rec_yd = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              carries = sum(carries),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              two_point = sum(receiving_2pt_conversions + rushing_2pt_conversions),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_ppr = sum(fantasy_points_ppr))%>%
    mutate(trgt_share = trgt/sum(QB_df$attempts))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(fantasy_ppr))%>%
    mutate(calc_fantasy_unfinished = (rush_td * 6) +
             (.1 * rush_yd) + 
             (pr * receptions) +
             (.1 * rec_yd) +
             (6 * rec_td) +
             (tp_conv * two_point) +
             (fum_lost * fumbles_lost))
    
  #TE
  TE_df <- df_filtered %>%
    filter(position == "TE",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(position = unique(position),
              weeks_played = n(),
              trgt = sum(targets),
              receptions = sum(receptions),
              rec_yd = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              carries = sum(carries),
              rush_yd = sum(rushing_yards),
              rush_td = sum(rushing_tds),
              two_point = sum(receiving_2pt_conversions + rushing_2pt_conversions),
              fumbles_lost = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              fantasy_ppr = sum(fantasy_points_ppr))%>%
    mutate(trgt_share = trgt/sum(QB_df$attempts))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(fantasy_ppr))%>%
    mutate(calc_fantasy_unfinished = (rush_td * 6) +
             (.1 * rush_yd) + 
             (pr * receptions) +
             (.1 * rec_yd) +
             (6 * rec_td) +
             (tp_conv * two_point) +
             (fum_lost * fumbles_lost))
    

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

test_df <- read.csv("team_data/LA.csv")



# Get a list of all the CSV files in the folder
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
