
test_that("works with integers", {
  treat <- c(0, 0, 1, 1, 0, 1)
  expected <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  out <- preprocess_treat(treat)
  expect_equal(out, expected)
})

test_that("works with factors", {
  treat <- factor(c("A", "A", "B", "B", "A", "B"), levels=c("A", "B"))
  expected <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  out <- preprocess_treat(treat)
  expect_equal(out, expected)
})

test_that("works with characters", {
  treat <- c("a", "a", "v", "v", "a", "v")
  expected <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  out <- preprocess_treat(treat)
  expect_equal(out, expected)
})

test_that("error with anything else", {
  treat <- c(0.0, 0.1, 0.1, 0.0, 0.1, 0.0)
  expect_error(preprocess_treat(treat))
})
