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
library(magrittr)
library(dplyr)
library(highcharter)

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
                  selectize = FALSE),
      actionButton("closetab", "Close current tab", icon("window-close"))
    ),

    # Body ----------------------------------------------------------------
    dashboardBody(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Main panel",
          value = "main_tab",
          fluidRow(
            column(width = 8,
                   highchartOutput("linechart")),
            valueBoxOutput("winner_count"),
            valueBoxOutput("perc_win"),
            valueBoxOutput("matches_played")
          )
        )
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
        distinct(datetime, match, score, cupname) %>% 
        arrange(datetime)

      if (nrow(linedata) > 1) {
        cats <- linedata$match
      } else {
        cats <- list(linedata$match)
      }

      highchart() %>% 
        hc_add_series(data = linedata$score,
                      name = "Score",
                      showInLegend = FALSE,
                      events = list(
                        click = JS("function(event) {Shiny.onInputChange('wc_match', [event.point.x]);}")
                      )) %>% 
        hc_xAxis(categories = cats) %>% 
        hc_title(text = first(linedata$cupname)) %>% 
        hc_subtitle(text = "Score per match.")
      

    # Score per cup -------------------------------------------------------
    } else {
      linedata <- distinct(df, year, country, team_total_score)
      
      highchart() %>% 
        hc_add_series(data = linedata$team_total_score,
                      name = "Total score",
                      showInLegend = FALSE,
                      events = list(
                        click = JS("function(event) {Shiny.onInputChange('wc_date', [event.point.category.name]);}")
                      )) %>% 
        hc_xAxis(categories = paste(linedata$country, linedata$year)) %>%
        hc_title(text = "Total goals per cup")
    }
  })

  # Filter by world cup ---------------------------------------------------
  observeEvent(input$wc_date != "",
               updateSelectInput(session, "wc", selected = wc_list[input$wc_date]),
               ignoreInit = TRUE)

  # Add match tab ---------------------------------------------------------
  observeEvent(input$wc_match, 
               {
                 match_date <- 
                   df %>%
                   filter(year == input$wc) %>%
                   distinct(datetime) %>%
                   pull() %>%
                   magrittr::extract(input$wc_match + 1)
                 
                 match_info <- filter(df, datetime == match_date)
                 
                 cup_name <- match_info[[1, "cupname"]]
                 match_teams <- match_info[[1, "match"]]
                 match_winner <- match_info[[1, "match_winner"]]
                 match_score <- match_info[[1, "match_score"]]
                 match_date <- match_info[[1, "datetime"]]
                 coach <- match_info[[1, "coach_name"]]
                 
                 match_info %<>% 
                   select(player_name, shirt_number, line_up)
                 
                 appendTab(
                   inputId = "tabs",
                   tabPanel(title = paste(cup_name, match_teams),
                            fluidRow(
                              column(width = 6,
                                     valueBox(match_winner, "Winner", width = NULL),
                                     valueBox(match_score, "Final score", width = NULL)),
                              column(width = 6,
                                     valueBox(lubridate::floor_date(match_date, "day"), "Date of the match", width = NULL),
                                     valueBox(coach, "Team coach", width = NULL))
                            ),
                            fluidRow(
                              box(width = 12,
                                  title = "Team lineup",
                                  DT::renderDataTable(match_info))
                            )))
               },
               ignoreInit = TRUE)
  

  # Close current tab -----------------------------------------------------
  observeEvent(input$closetab,
               if (input$tabs != "main_tab") {
                 removeTab("tabs", input$tabs)
               })
  
  # Win count / position in cup -------------------------------------------
  output$winner_count <- renderValueBox({
    win_count <- distinct(df, year, winner, team_name, team_outcome)
    
    if (input$wc != 0) win_count %<>% filter(year == input$wc)
    
    win_count %<>% 
      summarise(is_winner = sum(if_else(winner == team_name, 1, 0)),
                team_outcome = first(team_outcome))
    
    if (input$wc == 0){ 
      valueBox(value = win_count$is_winner,
               subtitle = "Times champion.")
    } else {
      valueBox(value = win_count$team_outcome,
               subtitle = "Position")
    }
  })

  # N matches won ---------------------------------------------------------
  output$perc_win <- renderValueBox({
    matches_won <- distinct(df, year, match_id, team_initials, match_winner)
    
    if (input$wc != 0) matches_won %<>% filter(year == input$wc)
    
    matches_won %<>% 
      summarise(
        matches_won = sum(team_initials == match_winner),
        matches_played = n(),
        perc_win = matches_won / matches_played
      ) %>% 
      pull(perc_win)
    
    valueBox(value = scales::percent(matches_won),
             subtitle = "of matches played won.")
  })
  
  # % ties ----------------------------------------------------------------
  output$matches_played <- renderValueBox({
    ties <- distinct(df, year, match_id, match_winner)
    
    if (input$wc != 0) ties %<>% filter(year == input$wc)
    
    ties %<>% 
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
