library(tidyverse)
library(rvest)
library(stringr)

options(scipen = 999)

# Grab Conference Tournament Champions ------------------------------------

seasons <- c(2025:2021, 2019)

get_conf_champs <- function(season) {
  url <- paste0("https://www.sports-reference.com/cbb/seasons/men/", season, "-conference-tournaments.html")
  table <- html_table(html_element(read_html(url), css = "table"))
  
  table %>% 
    mutate(season = season) %>% 
    filter(nchar(Champ) > 0) %>% 
    select(Champ, season)
}

all_conf_champs <- purrr::map_dfr(seasons, get_conf_champs) %>% 
  rename(team = Champ)


# Seeding Data ------------------------------------------------------------

get_seeds <- function(season) {
  if (season == 2024) {
    file_name <- paste0("March Madness At-Large Bids/Data/seed_", season, ".csv")
    seed <- read_csv(file_name, col_types = cols(.default = "c")) %>% 
      rename(Rk = Column1) %>% 
      mutate(season = season)
  } else {
    file_name <- paste0("March Madness At-Large Bids/Data/seed_", season, ".csv")
    seed <- read_csv(file_name, col_types = cols(.default = "c")) %>% 
      mutate(season = season)
  }
}

all_seeds <- purrr::map_dfr(seasons, get_seeds) %>% 
  filter(Rk != "Rk") %>% 
  mutate(
    seed = as.numeric(gsub("\\D*(\\d{1,2}).*", "\\1", Team)),
    team = sub("\\s*\\d.*", "", Team),
    team = str_trim(team),
    team_season = paste0(team, season)
  ) %>% 
  select(-c("Team", "Rk"))


# Metric Data -------------------------------------------------------------

get_metric <- function(season) {
  file_name <- paste0("March Madness At-Large Bids/Data/team_sheets_", season, ".csv")
  if (season == 2024) {
    team_sheets <- read.csv(file_name) %>% 
      mutate(season = 2024, Quality.Sag = NA)
  } else {
    team_sheets <- read.csv(file_name) %>% 
      mutate(season = season, Quality.Sag = NA)
  }
}

all_team_sheets <- purrr::map_dfr(seasons, get_metric) %>% 
  filter(Rk != "Rk") %>% 
  rename(
    net = NET,
    sor = Resumé.SOR,
    kp = Quality.KP,
    team = Team,
    q1 = Current.Quadrant.Records.Q1,
    q2 = Current.Quadrant.Records.Q2,
    q3 = Current.Quadrant.Records.Q3,
    q4 = Current.Quadrant.Records.Q4
  ) %>% 
  mutate(
    team = gsub("F4O", "", team),
    team = gsub("N4O", "", team),
    team = sub("\\d+\\s*", "", team),
    team_season = paste0(team, season)
  ) %>% 
  select(team, season, team_season, net, sor, kp, q1, q2, q3, q4)

all_team_sheets <- all_team_sheets %>%
  left_join(all_seeds, by = "team_season") %>%
  select(-team_season, -season.y, -team.y) %>%
  rename(
    team = team.x,
    season = season.x
  )

all_team_sheets <- all_team_sheets %>%
  separate(q1, into = c("q1_wins", "q1_losses"), sep = "-", convert = TRUE) %>%
  separate(q2, into = c("q2_wins", "q2_losses"), sep = "-", convert = TRUE) %>%
  separate(q3, into = c("q3_wins", "q3_losses"), sep = "-", convert = TRUE) %>%
  separate(q4, into = c("q4_wins", "q4_losses"), sep = "-", convert = TRUE) %>% 
  mutate(
    across(c(
    q1_wins, q1_losses, q2_wins, q2_losses, 
    q3_wins, q3_losses, q4_wins, q4_losses, 
    net, sor, kp, seed), as.numeric
  ),
    q3q4_losses = q3_losses + q4_losses
  )


# Fix Names ---------------------------------------------------------------

team_name_map <- c(
  "St. Mary's" = "Saint Mary's",
  "Northern Ky." = "Northern Kentucky",
  "Charleston" = "College of Charleston",
  "App. St." = "Appalachian St.",
  "Abilene Christ." = "Abilene Christian",
  "UC-Santa Barbara" = "UC Santa Barbara",
  "Cal St Fullerton" = "Cal St. Fullerton",
  "Fla. Atlantic" = "Florida Atlantic",
  "SE Missouri St" = "Southeast Missouri St.",
  "Tx AM-CC" = "Texas A&M Corpus Chris",
  "Louis." = "Louisiana Lafayette",
  "Va Tech" = "Virginia Tech",
  "St. Peter's" = "Saint Peter's",
  "Loyola \\(IL\\)" = "Loyola Chicago",
  "S Dakota St." = "South Dakota St.",
  "Ga Tech" = "Georgia Tech",
  "Eastern Wash." = "Eastern Washington",
  "Mt. Saint Mary's" = "Mount St. Mary's",
  "St. Louis" = "Saint Louis",
  "Gardner-Webb" = "Gardner Webb",
  "UC-Irvine" = "UC Irvine",
  "N.C. Central" = "North Carolina Central",
  "FDU" = "Fairleigh Dickinson",
  "Prairie View" = "Prairie View A&M",
  "NC State" = "North Carolina St.",
  "Western Ky." = "Western Kentucky",
  "Long Beach State" = "Long Beach St.",
  "Grambling" = "Grambling St.",
  "St. Francis (PA)" = "Saint Francis",
  "St. John's (NY)" = "St. John's",
  "UC-San Diego" = "UC San Diego",
  "UNC Wilm." = "UNC Wilmington"
)

all_conf_champs <- all_conf_champs %>%
  mutate(team = str_replace_all(team, team_name_map))


# Assign At-Large Teams ---------------------------------------------------

all_team_sheets <- all_team_sheets %>% 
  group_by(team, season) %>% 
  mutate(
    tournament = !is.na(seed),
    at_large = tournament & 
      !paste0(team, season) %in% 
      paste0(all_conf_champs$team, all_conf_champs$season)
  ) %>% 
  arrange(team) %>% 
  ungroup()


# Fix Auto Bids -----------------------------------------------------------


all_team_sheets <- all_team_sheets %>%
  mutate(
    across("at_large", ~ if_else(
      (team == "Fairleigh Dickinson" & season == 2023) |
      (team == "Jacksonville St." & season == 2022),
      FALSE,
      .
    ))
  )


# At-Large Only -----------------------------------------------------------

at_large_team_sheets <- all_team_sheets %>% 
  filter(!(tournament & !at_large))
