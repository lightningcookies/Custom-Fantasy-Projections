library(purrr)

# Define the list of team names
team_names <- c(
  "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens",
  "Buffalo Bills", "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals",
  "Cleveland Browns", "Dallas Cowboys", "Denver Broncos", "Detroit Lions",
  "Green Bay Packers", "Houston Texans", "Indianapolis Colts",
  "Jacksonville Jaguars", "Kansas City Chiefs", "Las Vegas Raiders",
  "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
  "Minnesota Vikings", "New England Patriots", "New Orleans Saints",
  "New York Giants", "New York Jets", "Philadelphia Eagles",
  "Pittsburgh Steelers", "San Francisco 49ers", "Seattle Seahawks",
  "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team"
)

# Generate the menu items dynamically
menu_items <- map_chr(team_names, function(team) {
  paste0("menuItem(\"", team, "\", tabName = \"", substr(team, 1, 3),
         "\", icon = icon(\"football\"))",",")
})

# Write the menu items to a text file
writeLines(menu_items, "menu_items.txt")

tabItem(tabName = "Ari",
        h2("Arizona Cardinals"),
        fluidRow(
          box(title = "Quarterbacks"),
          box(title = "Running Backs"),
          box(title = "Wide Receivers"),
          box(title = "Tight Ends")
        )
)
tab_items <- map_chr(team_names,function(team){
  paste0("tabItem(tabName = \"", substr(team, 1, 3), "\",\n",
         "\th2(\"", team, "\"),\n",
         "\tfluidRow(\n",
         "\t\tbox(title = \"Quarterbacks\"),\n",
         "\t\tbox(title = \"Running Backs\"),\n",
         "\t\tbox(title = \"Wide Receivers\"),\n",
         "\t\tbox(title = \"Tight Ends\")\n",
         "\t)\n)")
})
writeLines(tab_items,"tab_items.txt")
