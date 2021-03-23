## This script is a place to connect to/pull data from db for direct testing

library(DBI)
library(odbc)

library(amlrPinnipeds)
library(dplyr)
# library(lubridate)
# library(readxl)

tableNA <- function(...) table(..., useNA = 'ifany')

con <- dbConnect(odbc(),
                 Driver = "SQL Server", #"SQL Server Native Client 11.0",
                 Server = "swc-***REMOVED***-s",
                 Database = "***REMOVED***_Test",
                 Trusted_Connection = "True")

# pinniped.season <- tbl(con, "pinniped_season") %>% collect()

tr.p <- tbl(con, "tag_resights") %>%
  left_join(tbl(con, "pinnipeds"), by = c("pinniped_id" = "ID")) %>%
  filter(!is.na(resight_date)) %>%
  head(5000) %>%
  select(-ts, -attachments) %>%
  collect() %>%
  mutate(resight_date = as.Date(resight_date),
         pinniped_age = pinniped_age(resight_date, cohort)) %>%
  select(pinniped_age, cohort, everything())

