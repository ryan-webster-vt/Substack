
library(cfbfastR)
library(tidyverse)


# Load API Key ------------------------------------------------------------

readRenviron("CFB Clutchness Factor/.Renviron")
Sys.setenv(CFBD_API_KEY = Sys.getenv("API_KEY"))

