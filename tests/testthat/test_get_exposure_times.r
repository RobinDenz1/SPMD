
test_that("extracts exposure starts correctly", {

  data <- data.table(
    .id = c(1, 1, 1, 2, 2, 2),
    .start = c(0, 10, 20, 0, 10, 20),
    .stop = c(10, 20, 30, 10, 20, 30),
    .A = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
    .max_t = c(30, 30, 30, 30, 30, 30)
  )

  out <- get_exposure_times(data)

  expected <- data.table(
    .id = c(1, 2),
    .time = c(10, 20),
    .max_t = c(30, 30)
  )

  expect_equal(out, expected)
})

test_that("identifies only FALSE-to-TRUE transitions", {

  data <- data.table(
    .id = 1,
    .start = c(0, 10, 20, 30, 40),
    .stop = c(10, 20, 30, 40, 50),
    .A = c(FALSE, TRUE, TRUE, FALSE, TRUE),
    .max_t = 50
  )

  out <- get_exposure_times(data)

  expected <- data.table(
    .id = c(1, 1),
    .time = c(10, 40),
    .max_t = c(50, 50)
  )

  expect_equal(out, expected)
})

test_that("handles individuals with no exposure", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .A = c(FALSE, FALSE, FALSE, TRUE),
    .max_t = c(20, 20, 20, 20)
  )

  out <- get_exposure_times(data)

  expected <- data.table(
    .id = 2,
    .time = 10,
    .max_t = 20
  )

  expect_equal(out, expected)
})

test_that("returns empty data when nobody is exposed", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .A = FALSE,
    .max_t = 20
  )

  out <- get_exposure_times(data)

  expect_equal(nrow(out), 0L)

  expect_named(out, c(".id", ".time", ".max_t"))
})

test_that("handles individuals exposed from the first interval", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .A = c(TRUE, TRUE, FALSE, TRUE),
    .max_t = c(20, 20, 20, 20)
  )

  out <- get_exposure_times(data)

  # An individual already exposed at the first observed interval
  # is not identified as a new exposure because shift() is filled
  # with FALSE.
  expected <- data.table(
    .id = c(1, 2),
    .time = c(0, 10),
    .max_t = c(20, 20)
  )

  expect_equal(out, expected)
})

test_that("can identify multiple exposure episodes", {

  data <- data.table(
    .id = 1,
    .start = seq(0, 50, by = 10),
    .stop = seq(10, 60, by = 10),
    .A = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    .max_t = 60
  )

  out <- get_exposure_times(data)

  expected <- data.table(
    .id = c(1, 1, 1),
    .time = c(10, 30, 50),
    .max_t = c(60, 60, 60)
  )

  expect_equal(out, expected)
})

test_that("keeps all exposure episodes for an individual", {

  data <- data.table(
    .id = c(1, 1, 1, 1, 2, 2),
    .start = c(0, 10, 20, 30, 0, 10),
    .stop = c(10, 20, 30, 40, 10, 20),
    .A = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    .max_t = c(40, 40, 40, 40, 20, 20)
  )

  out <- get_exposure_times(data)

  expect_equal(out$.id, c(1, 1, 2))
  expect_equal(out$.time, c(10, 30, 10))
})

test_that("retains .max_t", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .A = c(FALSE, TRUE, FALSE, TRUE),
    .max_t = c(100, 100, 200, 200)
  )

  out <- get_exposure_times(data)

  expect_equal(out$.max_t, c(100, 200))
})

test_that("preserves the input data", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .A = c(FALSE, TRUE, FALSE, TRUE),
    .max_t = c(20, 20, 20, 20)
  )

  data_before <- copy(data)

  get_exposure_times(data)

  expect_identical(data, data_before)
})
