
test_that("general test case", {
  exposures <- data.frame(id=1, time=5)
  events <- data.frame(id=1, time=8)

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = 15,
    id = "id",
    time = "time",
    risk_period = 3
  )

  expected <- data.table(
    id = c(1, 1, 1),
    start = c(1, 5, 8),
    stop = c(5, 8, 15),
    A = c(FALSE, TRUE, FALSE),
    Y = c(FALSE, TRUE, FALSE)
  )
  setkey(expected, id)

  expect_s3_class(out, "data.table")
  expect_named(out, c("id", "start", "stop", "A", "Y"))
  expect_equal(out, expected)
})

test_that("exposure indicator is FALSE outside the risk period", {
  exposures <- data.frame(
    id = 1,
    time = 5
  )

  events <- data.frame(
    id = 1,
    time = c(1, 4, 6, 8)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = 10,
    id = "id",
    time = "time",
    risk_period = 3
  )

  expect_true(all(!is.na(out$A)))
  exposed <- out$start >= 5 & out$start < 8
  expect_true(any(out$A[exposed]))
  expect_true(all(out$A[!exposed] == FALSE))
})

test_that("numeric event times without events are handled", {
  exposures <- data.frame(
    id = 1,
    time = 5
  )

  events <- data.frame(
    id = integer(2),
    time = numeric(12)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = 10,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_true("Y" %in% names(out))
  expect_true(all(!is.na(out$Y)))
  expect_false(any(out$Y))
})

test_that("scalar observation period is supported", {
  exposures <- data.frame(
    id = c(1, 2),
    time = c(3, 7)
  )

  events <- data.frame(
    id = c(1, 2),
    time = c(5, 9)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = 10,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_equal(sort(unique(out$id)), c(1, 2))
  expect_true(all(out$start >= 1))
  expect_true(all(out$stop <= 10))
})

test_that("individual observation start and end data frames are supported", {
  exposures <- data.frame(
    id = c(1, 2),
    time = c(5, 8)
  )

  events <- data.frame(
    id = c(1, 2),
    time = c(7, 10)
  )

  obs_start <- data.frame(
    id = c(1, 2),
    time = c(1, 3)
  )

  obs_end <- data.frame(
    id = c(1, 2),
    time = c(9, 12)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = obs_start,
    obs_end = obs_end,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_true("Y" %in% names(out))
  expect_true("A" %in% names(out))
  expect_false(".observed" %in% names(out))

  expect_true(all(out$id %in% c(1, 2)))
})

test_that("individual observation start + common obs. end is supported", {
  exposures <- data.frame(
    id = c(1, 2),
    time = c(5, 8)
  )

  events <- data.frame(
    id = c(1, 2),
    time = c(7, 10)
  )

  obs_start <- data.frame(
    id = c(1, 2),
    time = c(1, 3)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = obs_start,
    obs_end = 12,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_true("A" %in% names(out))
  expect_true("Y" %in% names(out))
  expect_false(".observed" %in% names(out))
  expect_true(all(out$id %in% c(1, 2)))
})

test_that("common observation start with individual obs. end is supported", {
  exposures <- data.frame(
    id = c(1, 2),
    time = c(5, 8)
  )

  events <- data.frame(
    id = c(1, 2),
    time = c(7, 10)
  )

  obs_end <- data.frame(
    id = c(1, 2),
    time = c(9, 12)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = obs_end,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_true("A" %in% names(out))
  expect_true("Y" %in% names(out))
  expect_false(".observed" %in% names(out))
  expect_true(all(out$id %in% c(1, 2)))
})

test_that("custom exposure and event names are respected", {
  exposures <- data.frame(
    subject = 1,
    exposure_time = 5
  )

  events <- data.frame(
    subject = 1,
    exposure_time = 7
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = 10,
    id = "subject",
    time = "exposure_time",
    risk_period = 2,
    exposure_name = "Drug",
    event_name = "Outcome"
  )

  expect_true("Drug" %in% names(out))
  expect_true("Outcome" %in% names(out))
  expect_false("A" %in% names(out))
  expect_false("Y" %in% names(out))
})

test_that("multiple individuals retain their IDs", {
  exposures <- data.frame(
    id = c(1, 2, 3),
    time = c(2, 5, 8)
  )

  events <- data.frame(
    id = c(1, 2, 3),
    time = c(4, 7, 10)
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = 1,
    obs_end = 12,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_setequal(unique(out$id), c(1, 2, 3))
})

test_that("input data are not modified in place", {
  exposures <- data.frame(
    id = 1,
    time = 5
  )

  events <- data.frame(
    id = 1,
    time = 7
  )

  obs_start <- data.frame(
    id = 1,
    time = 1
  )

  obs_end <- data.frame(
    id = 1,
    time = 10
  )

  exposures_original <- exposures
  events_original <- events
  obs_start_original <- obs_start
  obs_end_original <- obs_end

  prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = obs_start,
    obs_end = obs_end,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_identical(exposures, exposures_original)
  expect_identical(events, events_original)
  expect_identical(obs_start, obs_start_original)
  expect_identical(obs_end, obs_end_original)
})

test_that("data frame and data.table inputs are both accepted", {
  exposures_df <- data.frame(
    id = 1,
    time = 5
  )

  events_df <- data.frame(
    id = 1,
    time = 7
  )

  exposures_dt <- data.table::as.data.table(exposures_df)
  events_dt <- data.table::as.data.table(events_df)

  out_df <- prepare_spmd_data(
    exposures = exposures_df,
    events = events_df,
    obs_start = 1,
    obs_end = 10,
    id = "id",
    time = "time",
    risk_period = 2
  )

  out_dt <- prepare_spmd_data(
    exposures = exposures_dt,
    events = events_dt,
    obs_start = 1,
    obs_end = 10,
    id = "id",
    time = "time",
    risk_period = 2
  )

  expect_equal(out_df, out_dt)
})

test_that("Date times are handled correctly", {
  exposures <- data.frame(
    id = 1,
    time = as.Date("2024-01-05")
  )

  events <- data.frame(
    id = 1,
    time = as.Date("2024-01-08")
  )

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = as.Date("2024-01-01"),
    obs_end = as.Date("2024-01-15"),
    id = "id",
    time = "time",
    risk_period = 4,
    convert_date = FALSE
  )

  expect_s3_class(out$start, "Date")
  expect_s3_class(out$stop, "Date")
  expect_equal(out$start[1], as.Date("2024-01-01"))
  expect_equal(out$stop[4], as.Date("2024-01-15"))

  out <- prepare_spmd_data(
    exposures = exposures,
    events = events,
    obs_start = as.Date("2024-01-01"),
    obs_end = as.Date("2024-01-15"),
    id = "id",
    time = "time",
    risk_period = 4,
    convert_date = TRUE
  )

  expect_equal(out$start, c(0, 4, 7, 8))
  expect_equal(out$stop, c(4, 7, 8, 14))
})
