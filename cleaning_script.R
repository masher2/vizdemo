# Packs -------------------------------------------------------------------

library(magrittr)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# Data --------------------------------------------------------------------

players  <- 
  read_excel("data/Dataset.xlsx", sheet=1) %>% 
  clean_names() %>% 
  mutate(player_name = str_to_title(player_name)) %>% 
  select(-(position:event))

matches  <- 
  read_excel("data/Dataset.xlsx", sheet=2) %>% 
  clean_names() %>% 
  mutate(
    home = str_c(home_team_name, home_team_initials, home_team_goals, sep = "/"),
    away = str_c(away_team_name, away_team_initials, away_team_goals, sep = "/"),
    win_conditions2 = str_match(win_conditions, "^(.+)\\swin") %>% .[, 2],
    match = str_c(home_team_initials, away_team_initials, sep = "-"),
    match_winner = case_when(
      home_team_goals < away_team_goals ~ away_team_initials,
      home_team_goals > away_team_goals ~ home_team_initials,
      (home_team_goals == away_team_goals) & (home_team_name == win_conditions2) ~ home_team_initials,
      (home_team_goals == away_team_goals) & (away_team_name == win_conditions2) ~ away_team_initials,
      (home_team_goals == away_team_goals) & is.na(win_conditions) ~ "Tie"
    )
  ) %>% 
  select(
    -(home_team_name:away_team_name),
    -(home_team_initials:away_team_initials),
    -(attendance:assistant_2)
  ) %>% 
  gather(home_away, value, home, away) %>% 
  separate(value, c("team_name", "team_initials", "score"), sep = "/") %>% 
  group_by(year, team_name) %>% 
  mutate(
    score = as.numeric(score),
    team_total_score = sum(score)
  ) %>% 
  ungroup()

worldcup <- 
  read_excel("data/Dataset.xlsx", sheet = 3) %>% 
  clean_names() %>% 
  mutate(cupname = paste(country, year))

# Joins -------------------------------------------------------------------

data <- 
  players %>% 
  left_join(matches, by = c("round_id", "match_id", "team_initials")) %>% 
  left_join(worldcup, by = c("year"))

# Transformations ---------------------------------------------------------

data %<>% 
  mutate(datetime = lubridate::dmy_hm(datetime)) %>% 
  arrange(datetime) %>% 
  group_by(year, team_initials) %>% 
  mutate(
    team_outcome = case_when(
      team_name == winner ~ "Winner",
      team_name == runners_up ~ "Runner up",
      team_name == third ~ "Third",
      team_name == fourth ~ "Fourth",
      TRUE ~ last(stage)
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    team_outcome = case_when(
      startsWith(team_outcome, "Group") ~ "First round",
      startsWith(team_outcome, "Preli") ~ "First round",
      startsWith(team_outcome, "Round") ~ "Eigth-finals",
      TRUE ~ team_outcome
    )
  )
  

# Output ------------------------------------------------------------------

map(
  data %>% pull(team_initials) %>% unique(),
  function(x) {
    data %>% 
      filter(team_initials == x) %>% 
      write.csv(paste0("output/data_", x, ".csv"))
    print(paste("Done for:", x))
  }
)
