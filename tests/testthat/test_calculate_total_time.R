
test_that("general test case", {
  vec <- c(25, 28, 48, 140, 143, 1400)
  out <- calculate_total_time(start=vec, stop=vec+40)
  expect_equal(out, 146)
})

test_that("with censored last time", {
  start <- c(25, 28, 48, 140, 143, 1400)
  stop <- c(65, 68, 88, 180, 183, 1402)
  out <- calculate_total_time(start=start, stop=stop)
  expect_equal(out, 108)
})
