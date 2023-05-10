library(shiny)
library(tidyverse)
library(nflfastR)

df <- read.csv("player_data.csv")

# Define the user interface
ui <- navbarPage("Navbar",
                 tabPanel("Test"),
                 navbarMenu("Teams",
                            tabPanel("ARI"), 
                            tabPanel("ATL"),
                            tabPanel("BAL"),
                            tabPanel("BUF"),
                            tabPanel("CAR"),
                            tabPanel("CHI"),
                            tabPanel("CIN"),
                            tabPanel("CLE"),
                            tabPanel("DAL"),
                            tabPanel("DEN"),
                            tabPanel("DET"),
                            tabPanel("GB"),
                            tabPanel("HOU"),
                            tabPanel("IND"),
                            tabPanel("JAX"),
                            tabPanel("KC"),
                            tabPanel("LV"),
                            tabPanel("LAC"),
                            tabPanel("LAR"),
                            tabPanel("MIA"),
                            tabPanel("MIN"),
                            tabPanel("NE"),
                            tabPanel("NO"),
                            tabPanel("NYG"),
                            tabPanel("NYJ"),
                            tabPanel("PHI"),
                            tabPanel("PIT"),
                            tabPanel("SF"),
                            tabPanel("SEA"),
                            tabPanel("TB"),
                            tabPanel("TEN"),
                            tabPanel("WSH")
)
)