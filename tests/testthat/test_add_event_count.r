
test_that("returns the input index rows with event counts", {

  dt_index <- data.table(
    .id = c(1, 2, 3),
    .time = c(10, 20, 30),
    .end_time = c(20, 30, 40)
  )

  dt_events <- data.table(
    .id = c(1, 1, 2, 3),
    .time = c(12, 15, 25, 50)
  )

  out <- add_event_count(
    dt_index = dt_index,
    dt_events = dt_events,
    bounds = "()"
  )

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), nrow(dt_index))
  expect_equal(out$.n_events, c(2L, 1L, 0L))
})

test_that("counts events only for the same individual", {

  dt_index <- data.table(
    .id = c(1, 2),
    .time = c(10, 10),
    .end_time = c(20, 20)
  )

  dt_events <- data.table(
    .id = c(1, 2, 3),
    .time = c(15, 15, 15)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, c(1L, 1L))
})

test_that("returns zero when there are no matching events", {

  dt_index <- data.table(
    .id = c(1, 2, 3),
    .time = c(10, 20, 30),
    .end_time = c(20, 30, 40)
  )

  dt_events <- data.table(
    .id = c(1, 2),
    .time = c(100, 200)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, c(0L, 0L, 0L))
})

test_that("handles an empty events table", {

  dt_index <- data.table(
    .id = c(1, 2, 3),
    .time = c(10, 20, 30),
    .end_time = c(20, 30, 40)
  )

  dt_events <- data.table(
    .id = numeric(),
    .time = numeric()
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, c(0L, 0L, 0L))
})

test_that("handles individuals with no events", {

  dt_index <- data.table(
    .id = c(1, 2, 3),
    .time = c(10, 10, 10),
    .end_time = c(20, 20, 20)
  )

  dt_events <- data.table(
    .id = c(1, 1),
    .time = c(12, 15)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, c(2L, 0L, 0L))
})

test_that("counts multiple events for one individual", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 5),
    .time = c(11, 12, 13, 14, 15)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, 5L)
})

test_that("correctly handles duplicate index rows", {

  dt_index <- data.table(
    .id = c(1, 1, 1),
    .time = c(10, 10, 10),
    .end_time = c(20, 20, 20)
  )

  dt_events <- data.table(
    .id = c(1, 1),
    .time = c(12, 15)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(nrow(out), 3L)
  expect_equal(out$.n_events, c(2L, 2L, 2L))
})

test_that("preserves the order of index rows", {

  dt_index <- data.table(
    .id = c(3, 1, 2),
    .time = c(30, 10, 20),
    .end_time = c(40, 20, 30)
  )

  dt_events <- data.table(
    .id = c(1, 2, 3),
    .time = c(15, 25, 35)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.id, c(3, 1, 2))
  expect_equal(out$.time, c(30, 10, 20))
  expect_equal(out$.n_events, c(1L, 1L, 1L))
})

test_that("adds the .n_events column", {

  dt_index <- data.table(
    .id = c(1, 2),
    .time = c(10, 20),
    .end_time = c(20, 30),
    other = c("a", "b")
  )

  dt_events <- data.table(
    .id = 1,
    .time = 15
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_true(".n_events" %in% names(out))
  expect_equal(out$other, c("a", "b"))
})

test_that("does not leave the temporary row_id column", {

  dt_index <- data.table(
    .id = c(1, 2),
    .time = c(10, 20),
    .end_time = c(20, 30)
  )

  dt_events <- data.table(
    .id = 1,
    .time = 15
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_false("row_id" %in% names(out))
})

test_that("correctly implements open bounds", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 4),
    .time = c(10, 11, 19, 20)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, 2L)
})

test_that("correctly implements left-open right-closed bounds", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 4),
    .time = c(10, 11, 19, 20)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "(]"
  )

  expect_equal(out$.n_events, 3L)
})

test_that("correctly implements left-closed right-open bounds", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 4),
    .time = c(10, 11, 19, 20)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "[)"
  )

  expect_equal(out$.n_events, 3L)
})


test_that("correctly implements closed bounds", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 4),
    .time = c(10, 11, 19, 20)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "[]"
  )

  expect_equal(out$.n_events, 4L)
})


test_that("distinguishes all four boundary conventions", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 4),
    .time = c(10, 11, 19, 20)
  )

  expected <- c(
    "()" = 2L,
    "(]" = 3L,
    "[)" = 3L,
    "[]" = 4L
  )

  for (bounds in names(expected)) {

    out <- add_event_count(
      dt_index,
      dt_events,
      bounds = bounds
    )

    expect_equal(
      out$.n_events,
      expected[[bounds]],
      info = paste("bounds =", bounds)
    )
  }
})

test_that("correctly handles events at multiple identical times", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  # Three separate events at time 15
  dt_events <- data.table(
    .id = c(1, 1, 1),
    .time = c(15, 15, 15)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, 3L)
})


test_that("handles different interval lengths", {

  dt_index <- data.table(
    .id = c(1, 1, 1),
    .time = c(0, 10, 20),
    .end_time = c(5, 25, 50)
  )

  dt_events <- data.table(
    .id = rep(1, 6),
    .time = c(2, 6, 15, 24, 30, 45)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, c(1L, 2L, 3L))
})

test_that("works with non-numeric IDs", {

  dt_index <- data.table(
    .id = c("A", "B"),
    .time = c(10, 10),
    .end_time = c(20, 20)
  )

  dt_events <- data.table(
    .id = c("A", "A", "B"),
    .time = c(12, 15, 15)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, c(2L, 1L))
})

test_that("handles integer times", {

  dt_index <- data.table(
    .id = 1L,
    .time = 10L,
    .end_time = 20L
  )

  dt_events <- data.table(
    .id = c(1L, 1L),
    .time = c(15L, 19L)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, 2L)
})

test_that("handles fractional event times", {

  dt_index <- data.table(
    .id = 1,
    .time = 10,
    .end_time = 20
  )

  dt_events <- data.table(
    .id = rep(1, 4),
    .time = c(10.1, 10.5, 19.5, 19.9)
  )

  out <- add_event_count(
    dt_index,
    dt_events,
    bounds = "()"
  )

  expect_equal(out$.n_events, 4L)
})
