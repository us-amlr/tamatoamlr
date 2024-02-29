test_that("csphoc example runs and completes as expected", {
  header.id <- c(1, 1, 2, 2, 3, 2)
  census.dates <- as.Date(c("2000-01-01", "2000-01-08", "2000-01-15"))

  count.df <- data.frame(
    header_id = header.id,
    species = tamatoamlr::pinniped.phocid.sp[c(1, 2, 1, 3, 4, 2)],
    location = "test",
    ad_female_count = c(5, 3, 7, 3, 6, 3),
    ad_male_count = c(NA, 4, 2, NA, 0, 3),
    ad_unk_count = 0,
    juv_female_count = 0,
    juv_male_count = 0,
    juv_unk_count = 0,
    pup_live_count = 0,
    unk_female_count = NA_integer_,
    unk_male_count = NA_integer_,
    unk_unk_count = NA_integer_,
    research_program = "USAMLR",
    census_date_start = census.dates[header.id]
  )

  expect_snapshot(complete_csphoc(count.df))
})
