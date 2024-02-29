test_that("dcc_format adheres to strptime format, and expected output columns", {
  x <- data.frame(
    Yr = c(20, 20, 21),
    Day = c(365, 366, 1),
    Hr = 12,
    Mn = 0,
    Fr = 164123,
    Sig = 99,
    station = "test"
  )

  datetimes <- as.POSIXct(
    c("2020-12-30 12:00:00", "2020-12-31 12:00:00", "2021-01-01 12:00:00"),
    tz = "America/Punta_Arenas"
  )
  x.out <- data.frame(
    freq = 164.123,
    sig = 99,
    datetime = datetimes,
    station = "test"
  )

  expect_equal(dcc_format(x), x.out)
})

test_that("dcc_format handles data frames with or without 'code' column. Also checks alternate tiomezone assigment", {
  x <- data.frame(
    Yr = 20,
    Day = c(365, 366),
    Hr = 12,
    Mn = 0,
    Fr = 164123,
    Sig = 99,
    Code = 1,
    station = "test"
  )

  x.out <- data.frame(
    freq = 164.123,
    code = 1,
    sig = 99,
    datetime = as.POSIXct(c("2020-12-30 12:00:00", "2020-12-31 12:00:00"), tz = "UTC"),
    station = "test"
  )

  expect_equal(dcc_format(x, tz = "UTC"), x.out)
  expect_equal(dcc_format(subset(x, select = -c(Code)), tz = "UTC"),
               subset(x.out, select = -c(code)))
})

test_that("dcc_format handles data frames with 4- or 6- digit frequencies, only", {
  x <- data.frame(
    Yr = 20,
    Day = 365,
    Hr = 12,
    Mn = 0,
    Fr = c(164123, 4123),
    Sig = 99,
    station = "test"
  )

  x.out <- data.frame(
    freq = rep(164.123, 2),
    sig = 99,
    datetime = as.POSIXct(c("2020-12-30 12:00:00"), tz = "America/Punta_Arenas"),
    station = "test"
  )

  expect_equal(dcc_format(x), x.out)

  x$Fr[1] <- 64123
  x$Fr[2] <- 123
  x.out$freq <- NA_real_
  expect_equal(dcc_format(x), x.out)
})



test_that("dcc_calc_trips handles data frames with or without 'code' column", {
  x.format <- data.frame(
    Yr = 20, Day = c(365, 366), Hr = 12, Mn = 0,
    Fr = 164123, Sig = 99, Code = 1, station = "test"
  ) %>%
    dcc_format(tz = "UTC")

  x.out <- tibble(
    freq = 164.123,
    code = 1,
    sig = 99,
    datetime = as.POSIXct(c("2020-12-30 12:00:00", "2020-12-31 12:00:00"), tz = "UTC"),
    datetime_prev = as.POSIXct(c(NA, "2020-12-30 12:00:00"), tz = "UTC"),
    time_diff_hr = c(NA_integer_, 24),
    trip_num_completed = c(0, 1),
    station = "test"
  )

  expect_equal(dcc_calc_trips(x.format, trip.hours = 24), x.out)
  expect_equal(dcc_calc_trips(subset(x.format, select = -c(code)),
                              trip.hours = 8),
               subset(x.out, select = -c(code)))
})

test_that("Brief check of dcc_calc_trips math and trip.hours input", {
  x.format <- data.frame(
    Yr = c(20, 20, 21),
    Day = c(365, 366, 1),
    Hr = c(12, 12, 0),
    Mn = 0,
    Fr = 164123,
    Sig = 99,
    station = "test"
  ) %>%
    dcc_format(tz = "UTC")

  datetimes <- as.POSIXct(
    c("2020-12-30 12:00:00", "2020-12-31 12:00:00", "2021-01-01 0:00:00"),
    tz = "UTC"
  )
  x.out <- tibble(
    freq = 164.123,
    sig = 99,
    datetime = datetimes,
    datetime_prev = as.POSIXct(c(NA, head(datetimes, -1)), tz = "UTC"),
    time_diff_hr = c(NA_integer_, 24, 12),
    trip_num_completed = c(0, 1, 1),
    station = "test"
  )

  expect_equal(dcc_calc_trips(x.format, trip.hours = 24), x.out)

  x.out$trip_num_completed[3] <- 2
  expect_equal(dcc_calc_trips(x.format, trip.hours = 8), x.out)
})

test_that("dcc_calc_trips only returns trip rows when trips.only is true", {
  x.format <- data.frame(
    Yr = c(20, 20, 21),
    Day = c(365, 366, 1),
    Hr = c(12, 12, 0),
    Mn = 0,
    Fr = 164123,
    Sig = 99,
    station = "test"
  ) %>%
    dcc_format()

  datetimes <- as.POSIXct(
    c("2020-12-30 12:00:00", "2020-12-31 12:00:00", "2021-01-01 0:00:00"),
    tz = "America/Punta_Arenas"
  )
  x.out <- tibble(
    freq = 164.123,
    sig = 99,
    datetime = datetimes[2],
    datetime_prev = datetimes[1],
    time_diff_hr = 24,
    trip_num_completed = 1,
    station = "test"
  )

  expect_equal(dcc_calc_trips(x.format, trip.hours = 24, trips.only = TRUE),
               x.out)
})
