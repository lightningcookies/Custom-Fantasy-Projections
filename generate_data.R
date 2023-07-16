library(tidyverse)
library(scales)

# Read player data in
df <- read.csv("player_data.csv")

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

# Initialize an empty data frame
combined_df <- data.frame()

# Iterate over NFL teams
for (team in nfl_teams) {
  team_df <- df_filtered %>%
    filter(recent_team == team) %>%
    group_by(position, player_name,recent_team) %>%
    summarise(
      pos = unique(position),
      g = n(),
      p_att = sum(attempts),
      cmp = sum(completions),
      p_yd = sum(passing_yards),
      p_td = sum(passing_tds),
      int = sum(interceptions),
      car = sum(carries),
      r_yd = sum(rushing_yards),
      r_td = sum(rushing_tds),
      tgt = sum(targets),
      rec = sum(receptions),
      rec_yd = sum(receiving_yards),
      rec_td = sum(receiving_tds),
      fmb = sum(rushing_fumbles_lost + sack_fumbles_lost),
      tp_c = sum(passing_2pt_conversions + rushing_2pt_conversions),
      f_ppr = sum(fantasy_points_ppr))%>%
    filter(pos %in% c("QB","RB","WR","TE","FB"))%>%
    mutate(tgt_share = tgt/sum(team_df$p_att),
           ypc = r_yd/car,
           ypr = rec_yd/rec,
           cmp_pct = cmp/p_att,
           td_rate = p_td/p_att)%>%
    mutate(
       f_custom = case_when(
         pos == "QB" ~ (r_td * 6) + (.1 * r_yd) + (ptd * p_td)
         + (pass_yd_pt * p_yd) + (int_point * int) + (tp_conv * tp_c) + (fum_lost * fmb),
         pos %in% c("RB","WR","TE","FB") ~ (6 * r_td) + (.1 * r_yd) + (pr * rec)
         + (.1 * rec_yd) + (6 * rec_td) + (tp_conv * tp_c) + (fum_lost * fmb)))
  
  # Append the team's data to the combined data frame
  combined_df <- bind_rows(combined_df, team_df)
}
combined_df <- combined_df %>%
  select(-position)%>%
  view()

# Saving the dataframe 

write.csv(combined_df,"players_2022.csv")


#team stats csv
combined_df2 <- tibble()
for(team in nfl_teams){
  df <- read.csv("players_2022.csv")%>%
    filter(recent_team == team)
  
  off_yd <- sum(df$p_yd) + sum(df$r_yd)
  p_yd <-  sum(df$p_yd)
  car <- sum(df$car)
  r_yd <- sum(df$r_yd)
  r_td <- sum(df$r_td)
  p_ff <- sum(df$f_ppr)
  p_att <- sum(df$p_att)
  cmp_pct <- sum(df$cmp)/sum(df$p_att)
  p_td <- sum(df$p_td)
  int <- sum(df$int)
  fmb <- sum(df$fmb)
  df2 <- tibble(team,off_yd,p_yd,car,r_yd,r_td,p_ff,p_att,cmp_pct,p_td,int,fmb)
  
  combined_df2 <- bind_rows(combined_df2,df2)
}

write.csv(combined_df2,"team_stats_2022.csv")
