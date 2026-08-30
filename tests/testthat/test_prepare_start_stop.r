
test_that("correctly renames columns", {

  data <- data.frame(
    person = c(2, 1),
    begin = c(10, 0),
    end = c(20, 10),
    treatment = c(1, 0),
    event = c(0, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "begin",
    stop = "end",
    id = "person",
    exposure = "treatment",
    outcome = "event"
  )

  expect_true(all(c(".start", ".stop", ".id", ".A", ".Y", ".max_t") %in%
                    names(out)))
  expect_false(any(c("begin", "end", "person", "treatment", "event") %in%
                     names(out)))
})

test_that("accepts data.frames", {

  data <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 10, 0, 10),
    stop = c(10, 20, 10, 20),
    A = c(0, 1, 0, 1),
    Y = c(0, 1, 0, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_s3_class(out, "data.table")
})

test_that("converts exposure to logical", {

  data <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 10, 0, 10),
    stop = c(10, 20, 10, 20),
    A = c(0, 1, 0, 0),
    Y = c(0, 1, 0, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y",
    remove_unexposed = FALSE
  )

  expect_type(out$.A, "logical")
  expect_equal(out$.A, c(FALSE, TRUE, FALSE, FALSE))
})

test_that("converts outcome to logical", {

  data <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 10, 0, 10),
    stop = c(10, 20, 10, 20),
    A = c(0, 1, 0, 1),
    Y = c(0, 2, 0, 5)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_type(out$.Y, "logical")
  expect_equal(out$.Y, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("maps zero outcomes to FALSE", {

  data <- data.frame(
    id = c(1, 1),
    start = c(0, 10),
    stop = c(10, 20),
    A = c(0, 1),
    Y = c(0, 0)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y",
    remove_noevents = FALSE
  )

  expect_equal(out$.Y, c(FALSE, FALSE))
})

test_that("maps nonzero outcomes to TRUE", {

  data <- data.frame(
    id = c(1, 1, 1),
    start = c(0, 10, 20),
    stop = c(10, 20, 30),
    A = c(0, 1, 1),
    Y = c(-1, 0, 2)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y",
    remove_noevents = FALSE
  )

  expect_equal(out$.Y, c(TRUE, FALSE, TRUE))
})

test_that("sorts by id and start time", {

  data <- data.frame(
    id = c(2, 1, 2, 1),
    start = c(20, 10, 0, 0),
    stop = c(30, 20, 10, 10),
    A = c(1, 1, 0, 0),
    Y = c(0, 0, 1, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(out$.id, c(1, 1, 2, 2))
  expect_equal(out$.start, c(0, 10, 0, 20))
})

test_that("calculates maximum observation time per person", {

  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    start = c(0, 10, 20, 0, 10),
    stop = c(10, 20, 50, 10, 30),
    A = c(0, 1, 1, 0, 1),
    Y = c(0, 0, 1, 0, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(out$.max_t, c(50, 50, 50, 30, 30))
})

test_that("removes unexposed individuals by default", {

  data <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 10, 0, 10),
    stop = c(10, 20, 10, 20),
    A = c(0, 0, 0, 1),
    Y = c(0, 1, 1, 0)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(unique(out$.id), 2)
})

test_that("retains unexposed individuals when requested", {

  data <- data.frame(
    id = c(1, 1, 2, 2),
    start = c(0, 10, 0, 10),
    stop = c(10, 20, 10, 20),
    A = c(0, 0, 0, 1),
    Y = c(0, 1, 1, 0)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y",
    remove_unexposed = FALSE
  )

  expect_setequal(unique(out$.id), c(1, 2))
})

test_that("removes rows with missing values", {

  data <- data.frame(
    id = c(1, 1, 2, 2, 3),
    start = c(0, 10, 0, 10, 0),
    stop = c(10, 20, 10, 20, 10),
    A = c(0, 1, NA, 1, 0),
    Y = c(0, 1, 0, NA, 1)
  )

  expect_warning(
    out <- prepare_start_stop(
      data = data,
      start = "start",
      stop = "stop",
      id = "id",
      exposure = "A",
      outcome = "Y",
      remove_unexposed = FALSE,
      remove_noevents = FALSE
    ),
    "Missing values"
  )

  expect_equal(nrow(out), 3L)
  expect_false(anyNA(out$.id))
})

test_that("preserves maximum observation time after filtering", {

  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    start = c(0, 10, 20, 0, 10),
    stop = c(10, 20, 100, 10, 30),
    A = c(0, 1, 1, 0, 1),
    Y = c(0, 1, 1, 0, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(out[.id == 1, unique(.max_t)], 100)
  expect_equal(out[.id == 2, unique(.max_t)], 30)
})

test_that("does not modify its input", {

  data <- data.table(
    id = c(2, 1),
    start = c(10, 0),
    stop = c(20, 10),
    A = c(1, 0),
    Y = c(1, 0)
  )

  data_before <- data

  prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_identical(data, data_before)
})

test_that("prepare_start_stop handles character IDs", {

  data <- data.frame(
    id = c("b", "b", "a", "a"),
    start = c(10, 20, 0, 10),
    stop = c(20, 30, 10, 20),
    A = c(0, 1, 0, 1),
    Y = c(0, 1, 0, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(out$.id, c("a", "a", "b", "b"))
})

test_that("prepare_start_stop handles repeated events", {

  data <- data.frame(
    id = c(1, 1, 1, 1),
    start = c(0, 10, 20, 30),
    stop = c(10, 20, 30, 40),
    A = c(0, 1, 1, 1),
    Y = c(0, 1, 1, 1)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(sum(out$.Y), 3)
})

test_that("handles a person exposed throughout observation", {

  data <- data.frame(
    id = c(1, 1, 1),
    start = c(0, 10, 20),
    stop = c(10, 20, 30),
    A = c(1, 1, 1),
    Y = c(0, 1, 0)
  )

  out <- prepare_start_stop(
    data = data,
    start = "start",
    stop = "stop",
    id = "id",
    exposure = "A",
    outcome = "Y"
  )

  expect_equal(nrow(out), 3L)
  expect_true(all(out$.A))
})
