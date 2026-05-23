
test_that("general test case", {
  vec <- c(25, 28, 48, 140, 143, 1400)
  out <- calculate_total_time(times=vec, risk_duration=40)
  expect_equal(out, 146)
})
