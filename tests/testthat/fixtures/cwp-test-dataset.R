# Generate CWP datasets for testing

library(odbc)
library(dplyr)
library(testthat)

con <- odbc::dbConnect(odbc(), filedsn = "../dsn/amlr-pinniped-db-prod.dsn")

cwp <- tbl(con, "vCensus_AFS_Capewide_Pup") %>%
  select(season_name, census_id, census_type,
         observer, census_date, time_start, time_end,
         location, beach_id, census_afs_capewide_pup_sort, species,
         pup_count, pup_live_count, pup_dead_count, exclude_count,
         research_program, census_notes, census_created_dt) %>%
  collect()

cwp.sample <- cwp %>% filter(season_name %in% c("2016/17", "2022/23"))
cwp.sample.ballena <- cwp.201617 %>% filter(grepl("Ballena", location))


saveRDS(cwp.sample, file = test_path("fixtures", "cwp_sample.rds"))
saveRDS(cwp.sample.ballena, file = test_path("fixtures", "cwp_agg_sample.rds"))
