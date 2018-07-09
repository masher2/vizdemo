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


# Choices -----------------------------------------------------------------

country_list <-
  readxl::read_excel("data/Dataset.xlsx", sheet = 2) %>% 
  janitor::clean_names() %>% 
  transmute(
    home = paste(home_team_name, home_team_initials, sep = "/"),
    away = paste(away_team_name, away_team_initials, sep = "/")
  ) %>% 
  tidyr::gather() %>% 
  distinct(value) %>% 
  tidyr::separate(value, into = c("name", "initials"), sep ="/") %>% 
  split(.$name) %>% 
  purrr::map(~.$initials)

# User Interface ----------------------------------------------------------

ui <- 
  dashboardPage(
    
    # Header --------------------------------------------------------------
    dashboardHeader(title = textOutput("teamname")),

    # Sidebar -------------------------------------------------------------
    dashboardSidebar(
      selectInput("team",
                  "Select a team:",
                  choices = country_list,
                  selected = "Algeria",
                  selectize = FALSE),
      selectInput("wc",
                  "Year of the world cup:",
                  choices = list("All cups" = 0),
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

  # Data --------------------------------------------------------------------
  df <- reactive({
    readr::read_csv(glue::glue("output/data_{input$team}.csv"))
  })
  
  # Title -----------------------------------------------------------------
  output$teamname <- renderText({
    df() %>% pull(team_name) %>% .[1]
  })

  # Update world cups assisted --------------------------------------------
  wc_assisted <- reactive({
    df() %>% 
      distinct(cupname, year) %>%
      split(.$cupname) %>% 
      purrr::map(~.$year) %>% 
      c("All cups" = 0, .)
  })
  
  observe({
    updateSelectInput(session,
                      "wc",
                      choices = wc_assisted())
  })
  
  # Linechart -------------------------------------------------------------
  output$linechart <- renderHighchart({
  
    # Score per match -----------------------------------------------------
    if (input$wc != 0) {
      linedata <-
        df() %>% 
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
      linedata <- distinct(df(), cupname, team_total_score)
      
      if (nrow(linedata) > 1) {
        cats <- linedata$cupname
      } else {
        cats <- list(linedata$cupname)
      }
      
      highchart() %>% 
        hc_add_series(data = linedata$team_total_score,
                      name = "Total score",
                      showInLegend = FALSE,
                      events = list(
                        click = JS("function(event) {Shiny.onInputChange('wc_date', [event.point.category.name]);}")
                      )) %>% 
        hc_xAxis(categories = cats) %>%
        hc_title(text = "Total goals per cup")
    }
  })

  # Filter by world cup ---------------------------------------------------
  observeEvent(input$wc_date != "",
               updateSelectInput(session, "wc", selected = wc_assisted()[input$wc_date]),
               ignoreInit = TRUE)

  # Add match tab ---------------------------------------------------------
  observeEvent(input$wc_match, 
               {
                 match_date <- 
                   df() %>%
                   filter(year == input$wc) %>%
                   distinct(datetime) %>%
                   pull() %>%
                   magrittr::extract(input$wc_match + 1)
                 
                 match_info <- filter(df(), datetime == match_date)
                 
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
    win_count <- distinct(df(), year, winner, team_name, team_outcome)
    
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
    matches_won <- distinct(df(), year, match_id, team_initials, match_winner)
    
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
    ties <- distinct(df(), year, match_id, match_winner)
    
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
