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
import::from("purrr", map)

# Data --------------------------------------------------------------------

df <- readr::read_csv("output/data_BRA.csv")

wc_list <- c(
  "All cups"          = 0,
  "Uruguay 1930"      = 1930,
  "Italy 1934"        = 1934,
  "France 1938"       = 1938,
  "Brazil 1950"       = 1950,
  "Switzerland 1954"  = 1954,
  "Sweden 1958"       = 1958,
  "Chile 1962"        = 1962,
  "England 1966"      = 1966,
  "Mexico 1970"       = 1970,
  "Germany 1974"      = 1974,
  "Argentina 1978"    = 1978,
  "Spain 1982"        = 1982,
  "Mexico 1986"       = 1986,
  "Italy 1990"        = 1990,
  "USA 1994"          = 1994,
  "France 1998"       = 1998,
  "Korea/Japan 2002"  = 2002,
  "Germany 2006"      = 2006,
  "South Africa 2010" = 2010,
  "Brazil 2014"       = 2014
)
  
# User Interface ----------------------------------------------------------

ui <- 
  dashboardPage(
    
    # Header --------------------------------------------------------------
    dashboardHeader(title = "Brazil"),

    # Sidebar -------------------------------------------------------------
    dashboardSidebar(
      selectInput("wc",
                  "Year of the world cup:",
                  choices = wc_list,
                  selected = "All cups",
                  selectize = FALSE)
    ),

    # Body ----------------------------------------------------------------
    dashboardBody(
      fluidRow(
        column(width = 8,
               highchartOutput("linechart")),
        valueBoxOutput("winner_count"),
        valueBoxOutput("perc_win"),
        valueBoxOutput("matches_played")
      )
    )
  )


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Linechart -------------------------------------------------------------
  output$linechart <- renderHighchart({
  
    # Score per match -----------------------------------------------------
    if (input$wc != 0) {
      linedata <-
        df %>% 
        filter(year == input$wc) %>% 
        mutate(datetime = stringr::str_replace(datetime, "\\s-.*$", ""),
               datetime2 = lubridate::dmy(datetime),
               cupname = paste(country, year)) %>% 
        distinct(datetime, datetime2, match, score, cupname) %>% 
        arrange(datetime2)

      if (nrow(linedata) > 1) {
        cats <- linedata$match
      } else {
        cats <- list(linedata$match)
      }

      highchart() %>% 
        hc_add_series(data = linedata$score,
                      name = "Score",
                      showInLegend = FALSE) %>% 
        hc_xAxis(categories = cats) %>% 
        hc_title(text = first(linedata$cupname)) %>% 
        hc_subtitle(text = "Score per match.")
      

    # Score per cup -------------------------------------------------------
    } else {
      linedata <- distinct(df, year, country, team_total_score)
      
      highchart() %>% 
        hc_xAxis(categories = paste(linedata$country, linedata$year)) %>%
        hc_add_series(data = linedata$team_total_score,
                      name = "Total score",
                      showInLegend = FALSE,
                      events = list(
                        click = JS("function(event) {Shiny.onInputChange('wc_date', [event.point.category.name]);}")
                      )) %>% 
        hc_title(text = "Total goals per cup")
    }
  })

  # Observe clicks to the linechart ---------------------------------------
  observeEvent(input$wc_date != "",
               updateSelectInput(session, "wc", selected = wc_list[input$wc_date]),
               ignoreInit = TRUE)
  
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

