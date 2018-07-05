#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(highcharter)
import::from("stringr", str_c)
import::from("purrr", map)

# Data --------------------------------------------------------------------

df <- readr::read_csv("output/data_BRA.csv")

year_list <- 
  df %>% 
  mutate(name = stringr::str_c(year, country, sep = " ")) %>% 
  distinct(name, year) %>%
  split(.$name) %>% 
  map(~.$year) %>% 
  c("All cups" = 0, .)
  
# User Interface ----------------------------------------------------------

ui <- 
  dashboardPage(

    # Header --------------------------------------------------------------
    dashboardHeader(title = "Brazil"),

    # Sidebar -------------------------------------------------------------
    dashboardSidebar(
      selectInput("year",
                  "Year of the world cup:",
                  choices = year_list)
    ),

    # Body ----------------------------------------------------------------
    dashboardBody(
      fluidRow(
        column(width = 8,
               highchartOutput("goals_per_cup")),
        valueBoxOutput("winner_count"),
        valueBoxOutput("perc_win"),
        valueBoxOutput("matches_played")
      )
    )
  )


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Goals per cup ---------------------------------------------------------
  output$goals_per_cup <- renderHighchart({
    df %>% 
      distinct(year, team_total_score) %>% 
      hchart("line", hcaes(x = "year", y = "team_total_score"))
  })
  

  # Counts ----------------------------------------------------------------
  output$winner_count <- renderValueBox({
    win_count <- df %>% 
      distinct(year, winner, team_name) %>% 
      summarise(is_winner = sum(if_else(winner == team_name, 1, 0))) %>% 
      pull()
    valueBox(value = win_count,
             subtitle = "Times champion.")
  })
  output$perc_win <- renderValueBox({
    matches_won <- df %>% 
      distinct(year, match_id, team_initials, match_winner) %>% 
      summarise(
        matches_won = sum(team_initials == match_winner),
        matches_played = n(),
        perc_win = matches_won / matches_played
      ) %>% 
      pull(perc_win)
    valueBox(value = scales::percent(matches_won),
             subtitle = "of matches played won.")
  })
  output$matches_played <- renderValueBox({
    ties <- df %>% 
      distinct(year, match_id, match_winner) %>% 
      summarise(
        ties = sum(match_winner == "Tie"),
        matches_played = n(),
        perc_ties = ties / matches_played
      ) %>% 
      pull(perc_ties)
    valueBox(value = scales::percent(ties),
             subtitle = "of matches played were ties.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

