## code to prepare `dcc` dataset goes here
library(here)
library(dplyr)

dcc.coded <- read.csv(here("inst", "extdata", "dcc-coded.csv"),
                      skip = 6) %>%
  select(Yr, Day, Hr, Mn, Fr, Sig, Code) %>%
  mutate(station = "TEST")

usethis::use_data(dcc.coded, overwrite = TRUE)


dcc.standard <- read.csv(here("inst", "extdata", "dcc-standard.csv"),
                      skip = 6) %>%
  select(Yr, Day, Hr, Mn, Fr, Sig) %>%
  mutate(station = "TEST")

usethis::use_data(dcc.standard, overwrite = TRUE)
