## This script is a place to connect to/pull data from db for direct testing

library(DBI)
library(odbc)

library(tamatoamlr)
library(tidyverse)
# library(lubridate)
# library(readxl)

tableNA <- function(...) table(..., useNA = 'ifany')

con <- dbConnect(odbc(),
                 Driver = "SQL Server", #"SQL Server Native Client 11.0",
                 Server = "swc-***REMOVED***-s",
                 # Database = "***REMOVED***_Test",
                 Database = "***REMOVED***",
                 Trusted_Connection = "True")

# pinniped.season <- tbl(con, "pinniped_season") %>% collect()

###############################################################################
###############################################################################
tr.p <- tbl(con, "tag_resights") %>%
  left_join(tbl(con, "pinnipeds"), by = c("pinniped_id" = "ID")) %>%
  filter(!is.na(resight_date)) %>%
  head(5000) %>%
  select(-ts, -attachments) %>%
  collect() %>%
  mutate(resight_date = as.Date(resight_date),
         pinniped_age = pinniped_age(resight_date, cohort)) %>%
  select(pinniped_age, cohort, everything())


###############################################################################
system.time(vcs <- tbl(con, "vCensus_Season") %>% filter(season_name == "2010/11") %>% collect())
census.phocid <- tbl(con, "vCensus_Season") %>%
  filter(between(as.Date(census_date), as.Date("2016-10-01"), as.Date("2017-04-01")),
         census_type == "phocid") %>%
  collect() %>%
  mutate(census_date = as.Date(census_date)) %>%
  filter(!is.na(Beach)) %>%
  group_by(species, Beach, census_date) %>%
  summarise(across(ad_female_count:unk_unk_count, sum, na.rm = TRUE),
            .groups = "drop") %>%
  complete(species, Beach, census_date) %>%
  mutate(across(ad_female_count:unk_unk_count, ~replace_na(.x, 0))) %>%
  arrange(species)


census.phocid %>%
  filter(species == "Weddell seal") %>%
  ggplot(aes(x = census_date, y = ad_female_count, color = Beach, group = Beach)) +
  geom_point() +
  geom_line()


###############################################################################
# Which census columns are used for which census types?
census <- tbl(con, "census") %>% collect()
tableNA(census$census_type)

census.summ <- census %>%
  select(ID, census_type, species, ad_female_count:unk_unk_count) %>%
  mutate(ID = as.character(ID)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
  filter(!is.na(count_value),
         count_value != 0)

tableNA(census.summ$count_class, census.summ$census_type)
cat(paste(sort(names(select(census, ad_female_count:unk_unk_count))), collapse = "\", \""))
