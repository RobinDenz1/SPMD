
test_that("general test cases", {

  # should work
  out <- is_valid_match(.id=10, .id2=12, .time=10, .time2=50, risk_period=30)
  expect_true(out)

  # risk_periods overlap, so should not work
  out <- is_valid_match(.id=10, .id2=12, .time=10, .time2=50, risk_period=40)
  expect_false(out)

  # same individual does not work either
  out <- is_valid_match(.id=10, .id2=10, .time=10, .time2=50, risk_period=30)
  expect_false(out)
})
