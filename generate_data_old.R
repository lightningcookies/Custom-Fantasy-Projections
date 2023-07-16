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
    summarise(pos = unique(position),
              g = n(),
              p_att = sum(attempts),
              cmp = sum(completions),
              p_yd = sum(passing_yards),
              p_td = sum(passing_tds),
              int = sum(interceptions),
              car = sum(carries),
              r_yd = sum(rushing_yards),
              r_td = sum(rushing_tds),
              fmb = sum(rushing_fumbles_lost + sack_fumbles_lost),
              tp_c = sum(passing_2pt_conversions + rushing_2pt_conversions),
              f_ppr = sum(fantasy_points_ppr))%>%
    mutate(cmp_pct = cmp/p_att,
           int_pct = int/p_att,
           td_pct = p_td/p_att,
           f_custom = (r_td * 6) +
             (.1 * r_yd) + 
             (ptd * p_td) +
             (pass_yd_pt * p_yd) +
             (int_point * int)+
             (tp_conv * tp_c)+
             (fum_lost * fmb)) %>%
    arrange(desc(f_ppr))
  
# RB
  RB_df <- df_filtered %>%
    filter(position == "RB",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(pos = unique(position),
              g = n(),
              car = sum(carries),
              r_yd = sum(rushing_yards),
              r_td = sum(rushing_tds),
              tgt = sum(targets),
              rec = sum(receptions),
              rec_yd = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              tp_c = sum(receiving_2pt_conversions + rushing_2pt_conversions),
              fmb = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              f_ppr = sum(fantasy_points_ppr))%>%
    mutate(tgt_p = tgt/sum(QB_df$p_att))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(f_ppr))%>%
    mutate(f_custom = (r_td * 6) +
             (.1 * r_yd) + 
             (pr * rec) +
             (.1 * rec_yd) +
             (6 * rec_td) +
             (tp_conv * tp_c) +
             (fum_lost * fmb))
             
  # WR
  WR_df <- df_filtered %>%
    filter(position == "WR",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(pos = unique(position),
              g = n(),
              tgt = sum(targets),
              rec = sum(receptions),
              rec_yd = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              car = sum(carries),
              r_yd = sum(rushing_yards),
              r_td = sum(rushing_tds),
              tp_c = sum(receiving_2pt_conversions + rushing_2pt_conversions),
              fmb = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              f_ppr = sum(fantasy_points_ppr))%>%
    mutate(tgt_p = tgt/sum(QB_df$p_att))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(f_ppr))%>%
    mutate(f_custom = (6 * r_td) +
             (.1 * r_yd) + 
             (pr * rec) +
             (.1 * rec_yd) +
             (6 * rec_td) +
             (tp_conv * tp_c) +
             (fum_lost * fmb))
    
  #TE
  TE_df <- df_filtered %>%
    filter(position == "TE",
           recent_team == team)%>%
    group_by(player_name)%>%
    summarise(pos = unique(position),
              g = n(),
              tgt = sum(targets),
              rec = sum(receptions),
              rec_yd = sum(receiving_yards),
              rec_td = sum(receiving_tds),
              car = sum(carries),
              r_yd = sum(rushing_yards),
              r_td = sum(rushing_tds),
              tp_c = sum(receiving_2pt_conversions + rushing_2pt_conversions),
              fmb = sum(rushing_fumbles_lost + receiving_fumbles_lost),
              f_ppr = sum(fantasy_points_ppr))%>%
    mutate(tgt_p = tgt/sum(QB_df$p_att))%>%
    #mutate(tgt_share = percent(as.numeric(tgt_share), accuracy = 0.01))%>%
    arrange(desc(f_ppr))%>%
    mutate(f_custom = (6 * r_td) +
             (.1 * r_yd) + 
             (pr * rec) +
             (.1 * rec_yd) +
             (6 * rec_td) +
             (tp_conv * tp_c) +
             (fum_lost * fmb))
    
  # joins
  df_join1 <- full_join(QB_df,RB_df)
  df_join2 <- full_join(df_join1, TE_df)
  df_join3 <- full_join(df_join2, WR_df)
  
  # Replace NA
  df_join3 <- df_join3 %>% replace(is.na(.), 0)
  
  df_s <- tibble(points = 34,
    pass_att = sum(df_join3$p_att),
    cmp = sum(df_join3$cmp)
                 )%>%
    view()
  
  filename <- paste0("team_data/", team, ".csv")
  write.csv(df_join3, filename)
}