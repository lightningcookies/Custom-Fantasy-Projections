library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

# Read player data in
df <- read.csv("player_data.csv")

# Arizona Cardinals 


ui <- dashboardPage(
  dashboardHeader(title = "Custom Fantasy Projections"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Arizona", tabName = "Ari", icon = icon("dashboard")),
      menuItem("Page 2", tabName = "page2", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Ari",
              h2("Arizona Cardinals"),
              fluidRow(
                box(
                  title = "Quarterbacks"
                ),
                box(
                  title = "Running Backs"
                )
              )
      ),
      tabItem(tabName = "page2",
              h2("Page 2"),
              plotOutput("plot")
      )
    )
  )
)

server <- function(input, output) { 
  output$plot <- renderPlot({
    plot(cars)
  })
}

shinyApp(ui, server)

