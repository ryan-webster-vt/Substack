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
    team_season = paste(team, season),
    team_season = str_squish(team_season)
  ) %>% 
  select(-c("Team", "Rk"))


# Metric Data -------------------------------------------------------------

get_metric <- function(season) {
  file_name <- paste0("March Madness At-Large Bids/Data/team_sheets_", season, ".xlsx")
  
  if (season == 2024) {
    team_sheets <- readxl::read_xlsx(file_name) %>% 
      mutate(season = 2024, Quality.Sag = NA)
  } else {
    team_sheets <- readxl::read_xlsx(file_name) %>% 
      mutate(season = season, Quality.Sag = NA)
  }
}

all_team_sheets <- purrr::map_dfr(seasons, get_metric) %>% 
  filter(Rk != "Rk") %>% 
  rename(
    net = NET,
    sor = SOR,
    kp = KPI,
    team = Team,
    q1 = Q1,
    q2 = Q2,
    q3 = Q3,
    q4 = Q4
  ) %>% 
  mutate(
    team = gsub("F4O", "", team),
    team = gsub("N4O", "", team),
    team = sub("\\d+\\s*", "", team),
    team_season = paste0(team, season),
    team_season = str_squish(team_season)
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

all_conf_champs$team <- gsub("St. Mary's", "Saint Mary's", all_conf_champs$team)
all_conf_champs$team <- gsub("Northern Ky.", "Northern Kentucky", all_conf_champs$team)
all_conf_champs$team <- gsub("Charleston", "College of Charleston", all_conf_champs$team)
all_conf_champs$team <- gsub("App. St.", "Appalachian St.", all_conf_champs$team)
all_conf_champs$team <- gsub("Abilene Christ.", "Abilene Christian", all_conf_champs$team)
all_conf_champs$team <- gsub("UC-Santa Barbara", "UC Santa Barbara", all_conf_champs$team)
all_conf_champs$team <- gsub("Cal St Fullerton", "Cal St. Fullerton", all_conf_champs$team)
all_conf_champs$team <- gsub("Fla. Atlantic", "Florida Atlantic", all_conf_champs$team)
all_conf_champs$team <- gsub("SE Missouri St", "Southeast Missouri St.", all_conf_champs$team)
all_conf_champs$team <- gsub("Tx AM-CC", "Texas A&M Corpus Chris", all_conf_champs$team)
all_conf_champs$team <- gsub("Louis.", "Louisiana Lafayette", all_conf_champs$team)
all_conf_champs$team <- gsub("Va Tech", "Virginia Tech", all_conf_champs$team)
all_conf_champs$team <- gsub("St. Peter's", "Saint Peter's", all_conf_champs$team)
all_conf_champs$team <- gsub("Loyola \\(IL\\)", "Loyola Chicago", all_conf_champs$team)
all_conf_champs$team <- gsub("S Dakota St.", "South Dakota St.", all_conf_champs$team)
all_conf_champs$team <- gsub("Ga Tech", "Georgia Tech", all_conf_champs$team)
all_conf_champs$team <- gsub("Eastern Wash.", "Eastern Washington", all_conf_champs$team)
all_conf_champs$team <- gsub("Mt. Saint Mary's", "Mount St. Mary's", all_conf_champs$team)
all_conf_champs$team <- gsub("St. Louis", "Saint Louis", all_conf_champs$team)
all_conf_champs$team <- gsub("Gardner-Webb", "Gardner Webb", all_conf_champs$team)
all_conf_champs$team <- gsub("UC-Irvine", "UC Irvine", all_conf_champs$team)
all_conf_champs$team <- gsub("N.C. Central", "North Carolina Central", all_conf_champs$team)
all_conf_champs$team <- gsub("FDU", "Fairleigh Dickinson", all_conf_champs$team)
all_conf_champs$team <- gsub("Prairie View", "Prairie View A&M", all_conf_champs$team)
all_conf_champs$team <- gsub("NC State", "North Carolina St.", all_conf_champs$team)
all_conf_champs$team <- gsub("Western Ky.", "Western Kentucky", all_conf_champs$team)
all_conf_champs$team <- gsub("Long Beach State", "Long Beach St.", all_conf_champs$team)
all_conf_champs$team <- gsub("Grambling", "Grambling St.", all_conf_champs$team)
all_conf_champs$team <- gsub("St. Francis \\(PA\\)", "Saint Francis", all_conf_champs$team)
all_conf_champs$team <- gsub("St. John's \\(NY\\)", "St. John's", all_conf_champs$team)
all_conf_champs$team <- gsub("UC-San Diego", "UC San Diego", all_conf_champs$team)
all_conf_champs$team <- gsub("UNC Wilm.", "UNC Wilmington", all_conf_champs$team)
all_conf_champs$team <- gsub("Omaha", "Nebraska Omaha", all_conf_champs$team)


#  "St. John's (NY)" = "St. John's",
 # "UC-San Diego" = "UC San Diego",
  #"UNC Wilm." = "UNC Wilmington"



# Assign At-Large Teams ---------------------------------------------------

all_team_sheets <- all_team_sheets %>% 
  group_by(team, season) %>% 
  mutate(
    tournament = !is.na(seed),
    at_large = tournament & 
      !str_squish(paste(team, season)) %in% 
      str_squish(paste(all_conf_champs$team, all_conf_champs$season))
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
