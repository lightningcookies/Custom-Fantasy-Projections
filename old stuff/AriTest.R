library(tidyverse)
library(rhandsontable)

pr <- .5
ptd <- 4
pass_yd_pt <- .04
int_point <- (-1)
tp_conv <- 2
fum_lost <- (-2)


ari_players <- read.csv("team_data/ARI.csv")
view(ari_players)

ari_roster <- read.csv("2023_rosters.csv")%>%
  filter(team == "ARI")%>%
  view()

ari_qb <- ari_roster %>%
  filter(POS == "QB")%>%
  mutate(GP = 1,ATT = 1, CMP = 1, PCT = CMP/ATT, YDS = 1, AVG = YDS/ATT,
         TD = 1, TDPCT = TD/ATT, INT = 1, INTPCT = INT/ATT, RYD = 1,RATT = 1,
         RTD = 1, RAVG = RYD/RATT, FUM = 1, TPC = 1, FPTS = (RTD * 6) +
           (.1 * RYD) + 
           (TD * ptd) +
           (YDS * pass_yd_pt) +
           (INT * int_point)+
           (TPC * tp_conv)+
           (FUM * fum_lost))%>%
  view()

ari_rb <- ari_roster %>%
  filter(POS == "RB")%>%
  mutate(GP = 1, CAR = 1, RYD = 1, RTD = 1, REC = 1, RECYD = 1, RTD = 1,
         YPC = 1, )

ari_qb_2022 <- ari_players%>%
  filter(position == "QB")





write.csv(ari_qb, "AriQb.csv")
