test_that("cwp_total_by_loc properly removes exclude_count records", {
  x <- readRDS(test_path("fixtures", "cwp_sample.rds"))

  expect_equal(
    nrow(x) - sum(x$exclude_count),
    # The warning printed is good, we just don't care about it for this test
    suppressWarnings(sum(cwp_total_by_loc(x)$num_records))
  )
})


test_that("cwp functions are robust to presence or absence of census_afs_capewide_pup_sort", {
  x <- readRDS(test_path("fixtures", "cwp_sample.rds"))
  x <- x[!x$exclude_count, ]
  y <- subset(x, select = -c(census_afs_capewide_pup_sort))

  expect_no_error(cwp_total(x))
  expect_no_error(cwp_total(y))
  expect_no_error(cwp_review(x))
  expect_no_error(cwp_review(y))
  expect_no_error(cwp_loc_agg(x))
  expect_no_error(cwp_loc_agg(y))
})


test_that("cwp_total and _by_loc summarize as expected: by location within season, then by season", {
  x <- readRDS(test_path("fixtures", "cwp_sample.rds"))
  x <- x[!x$exclude_count, ]

  x.total <- cwp_total(x)
  x.total$count_mean <- round(x.total$count_mean, 1)
  x.total$count_sd <- round(x.total$count_sd, 1)

  # Proofed values
  x.proofed <- tibble(
    season_name = c("2016/17", "2022/23"),
    count_mean = c(1545, 497.5),
    count_sd = c(11.2, 4.1),
    research_program = "USAMLR"
  )

  expect_equal(x.total, x.proofed)
})
