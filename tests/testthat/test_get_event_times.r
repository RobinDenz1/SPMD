
test_that("extracts all events", {

  data <- data.table(
    .id = c(1, 1, 1, 2, 2),
    .start = c(0, 10, 20, 0, 10),
    .stop = c(10, 20, 30, 10, 20),
    .Y = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  out <- get_event_times(data)

  expected <- data.table(
    .id = c(1, 2),
    .time = c(20, 10)
  )

  expect_equal(out, expected)
})

test_that("extracts multiple events for an individual", {

  data <- data.table(
    .id = 1,
    .start = c(0, 10, 20, 30, 40),
    .stop = c(10, 20, 30, 40, 50),
    .Y = c(FALSE, TRUE, FALSE, TRUE, TRUE)
  )

  out <- get_event_times(data)

  expected <- data.table(
    .id = c(1, 1, 1),
    .time = c(20, 40, 50)
  )

  expect_equal(out, expected)
})

test_that("handles individuals with no events", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .Y = FALSE
  )

  out <- get_event_times(data)

  expect_equal(nrow(out), 0L)
  expect_named(out, c(".id", ".time"))
})

test_that("keeps only event rows", {

  data <- data.table(
    .id = c(1, 1, 1),
    .start = c(0, 10, 20),
    .stop = c(10, 20, 30),
    .Y = c(FALSE, TRUE, FALSE),
    extra = c("a", "b", "c")
  )

  out <- get_event_times(data)

  expect_equal(nrow(out), 1L)
  expect_equal(out$.id, 1)
  expect_equal(out$.time, 20)
  expect_named(out, c(".id", ".time"))
})

test_that("uses .stop as the event time", {

  data <- data.table(
    .id = 1,
    .start = 100,
    .stop = 250,
    .Y = TRUE
  )

  out <- get_event_times(data)

  expect_equal(out$.time, 250)
})

test_that("preserves the order of event rows", {

  data <- data.table(
    .id = c(2, 1, 2, 1),
    .start = c(0, 0, 10, 10),
    .stop = c(10, 10, 20, 20),
    .Y = c(TRUE, TRUE, TRUE, TRUE)
  )

  out <- get_event_times(data)

  expect_equal(out$.id, c(2, 1, 2, 1))
  expect_equal(out$.time, c(10, 10, 20, 20))
})

test_that("handles multiple events at the same time", {

  data <- data.table(
    .id = c(1, 1, 1),
    .start = c(0, 0, 0),
    .stop = c(10, 10, 10),
    .Y = TRUE
  )

  out <- get_event_times(data)

  expect_equal(nrow(out), 3L)
  expect_equal(out$.id, c(1, 1, 1))
  expect_equal(out$.time, c(10, 10, 10))
})

test_that("preserves the input data", {

  data <- data.table(
    .id = c(1, 1, 2, 2),
    .start = c(0, 10, 0, 10),
    .stop = c(10, 20, 10, 20),
    .Y = c(FALSE, TRUE, FALSE, TRUE)
  )

  data_before <- copy(data)

  get_event_times(data)

  expect_identical(data, data_before)
})
