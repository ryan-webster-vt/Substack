
library(cfbfastR)
library(tidyverse)


# Load API Key ------------------------------------------------------------

readRenviron("CFB Clutchness Factor/.Renviron")
Sys.setenv(CFBD_API_KEY = Sys.getenv("API_KEY"))


# Load Team 2024-2025 Records ---------------------------------------------

fbs_teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE) %>% select(school)
team_matrix <- expand.grid(team = fbs_teams$school, week = 1:14)

# Expected Win Probability at 2:00 4th ------------------------------------

pbp <- cfbfastR::load_cfb_pbp() %>% 
  filter(
    home %in% fbs_teams$school | away %in% fbs_teams$school,
    week < 15,
    period == 4
  ) %>% 
  mutate(
    wp_home = ifelse(pos_team == home, wp_before, 1 - wp_before), 
    wp_away = 1 - wp_home
  )

fourth_quarter <- pbp %>% 
  select(
    game_id,
    week,
    home, 
    away, 
    clock.minutes, 
    clock.seconds, 
    wp_home, 
    wp_away,
    period
  ) %>% 
  mutate(seconds_from_two_min = abs((clock.minutes * 60 + clock.seconds) - 120)) %>% 
  group_by(game_id) %>% 
  slice_min(order_by = seconds_from_two_min, n = 1, with_ties = FALSE) %>% 
  ungroup()


# Apply Probabilities to Team Matrix --------------------------------------

home_teams <- inner_join(
  team_matrix, 
  fourth_quarter %>% select(wp_home, home, week), 
  by = c("team" = "home", "week")
) %>% rename(wp = wp_home)

away_teams <- inner_join(
  team_matrix, 
  fourth_quarter %>% select(wp_away, away, week), 
  by = c("team" = "away", "week")
) %>% rename(wp = wp_away)

team_matrix <- rbind(home_teams, away_teams)

# Retrieve Team Records ---------------------------------------------------

records <- cfbfastR::cfbd_game_records(2024) %>% 
  filter(classification == "fbs") %>% 
  select(team, total_wins)


# Debug Bowl Games Mistakenly Added ---------------------------------------

game_count <- team_matrix %>% 
  group_by(team) %>% 
  summarise(count = n())


# Calculate Expected Wins -------------------------------------------------

expected_wins <- team_matrix %>% 
  group_by(team) %>% 
  summarise(expected_wins = sum(wp, na.rm = TRUE))

expected_wins <- inner_join(
  expected_wins, 
  records %>% select(team, total_wins), 
  by = join_by("team")
)

expected_wins <- expected_wins %>% mutate(win_diff = total_wins - expected_wins)

writexl::write_xlsx(expected_wins, "expected_wins.xlsx")
