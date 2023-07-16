library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(purrr)

# Read player data in
df <- read.csv("player_data.csv")

teams_full <- c(
  "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens",
  "Buffalo Bills", "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals",
  "Cleveland Browns", "Dallas Cowboys", "Denver Broncos", "Detroit Lions",
  "Green Bay Packers", "Houston Texans", "Indianapolis Colts",
  "Jacksonville Jaguars", "Kansas City Chiefs", "Las Vegas Raiders",
  "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
  "Minnesota Vikings", "New England Patriots", "New Orleans Saints",
  "New York Giants", "New York Jets", "Philadelphia Eagles",
  "Pittsburgh Steelers", "San Francisco 49ers", "Seattle Seahawks",
  "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team")

# Arizona Cardinals 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Arizona Cardinals", tabName = "Ari", icon = icon("football")),
    menuItem("Atlanta Falcons", tabName = "Atl", icon = icon("football")),
    menuItem("Baltimore Ravens", tabName = "Bal", icon = icon("football")),
    menuItem("Buffalo Bills", tabName = "Buf", icon = icon("football")),
    menuItem("Carolina Panthers", tabName = "Car", icon = icon("football")),
    menuItem("Chicago Bears", tabName = "Chi", icon = icon("football")),
    menuItem("Cincinnati Bengals", tabName = "Cin", icon = icon("football")),
    menuItem("Cleveland Browns", tabName = "Cle", icon = icon("football")),
    menuItem("Dallas Cowboys", tabName = "Dal", icon = icon("football")),
    menuItem("Denver Broncos", tabName = "Den", icon = icon("football")),
    menuItem("Detroit Lions", tabName = "Det", icon = icon("football")),
    menuItem("Green Bay Packers", tabName = "Gre", icon = icon("football")),
    menuItem("Houston Texans", tabName = "Hou", icon = icon("football")),
    menuItem("Indianapolis Colts", tabName = "Ind", icon = icon("football")),
    menuItem("Jacksonville Jaguars", tabName = "Jac", icon = icon("football")),
    menuItem("Kansas City Chiefs", tabName = "Kan", icon = icon("football")),
    menuItem("Las Vegas Raiders", tabName = "Las", icon = icon("football")),
    menuItem("Los Angeles Chargers", tabName = "Los", icon = icon("football")),
    menuItem("Los Angeles Rams", tabName = "Los", icon = icon("football")),
    menuItem("Miami Dolphins", tabName = "Mia", icon = icon("football")),
    menuItem("Minnesota Vikings", tabName = "Min", icon = icon("football")),
    menuItem("New England Patriots", tabName = "New", icon = icon("football")),
    menuItem("New Orleans Saints", tabName = "New", icon = icon("football")),
    menuItem("New York Giants", tabName = "New", icon = icon("football")),
    menuItem("New York Jets", tabName = "New", icon = icon("football")),
    menuItem("Philadelphia Eagles", tabName = "Phi", icon = icon("football")),
    menuItem("Pittsburgh Steelers", tabName = "Pit", icon = icon("football")),
    menuItem("San Francisco 49ers", tabName = "San", icon = icon("football")),
    menuItem("Seattle Seahawks", tabName = "Sea", icon = icon("football")),
    menuItem("Tampa Bay Buccaneers", tabName = "Tam", icon = icon("football")),
    menuItem("Tennessee Titans", tabName = "Ten", icon = icon("football")),
    menuItem("Washington Football Team", tabName = "Was", icon = icon("football"))
  )
)

body <- dashboardBody(
  tabItem(tabName = "Ari",
          h2("Arizona Cardinals"),
          fluidRow(
            box(title = "Quarterbacks"),
            box(title = "Running Backs"),
            box(title = "Wide Receivers"),
            box(title = "Tight Ends")
          )
  ),
  tabItem(tabName = "Atl",
          h2("Atlanta Falcons")
      )
)

ui <- dashboardPage(
  dashboardHeader(title = "Custom Fantasy Projections"),
  sidebar,
  body
)

server <- function(input, output) { 

}

shinyApp(ui, server)


#ideas section
#file upload and download for changes in rankings with fileInput("upload", NULL)