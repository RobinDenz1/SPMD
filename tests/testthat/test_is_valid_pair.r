
test_that("general test cases", {

  # should work
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=30,
                       .max_t=1000, .max_t2=1000, bounds="[]")
  expect_true(out)

  # risk_periods overlap, so should not work
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=40,
                       .max_t=1000, .max_t2=1000, bounds="[]")
  expect_false(out)

  # risk_periods overlap, but bounds specify independency, so should work
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=40,
                       .max_t=1000, .max_t2=1000, bounds="[)")
  expect_true(out)

  # same individual does not work either
  out <- is_valid_pair(.id=10, .id2=10, .time=10, .time2=50, risk_period=30,
                       .max_t=1000, .max_t2=1000, bounds="[]")
  expect_false(out)

  # control period could not be fully observed due to right-censoring
  # but its fine because its after t2
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=30,
                       .max_t=60, .max_t2=1000, bounds="[]")
  expect_true(out)

  # control period could not be observed at all due to right-censoring
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=30,
                       .max_t=40, .max_t2=1000, bounds="[]")
  expect_false(out)

  # same as above, but censoring isn't checked so it should work
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=30,
                       .max_t=40, .max_t2=1000, bounds="[]",
                       check_censoring=FALSE)
  expect_true(out)

  # one control period cannot be observed at all, the risk period only partially
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=30,
                       .max_t=35, .max_t2=70, bounds="[]")
  expect_false(out)

  # two censoring times, both should be fine
  out <- is_valid_pair(.id=10, .id2=12, .time=10, .time2=50, risk_period=30,
                       .max_t=75, .max_t2=70, bounds="[]")
  expect_true(out)

  # allow_overlap works
  out <- is_valid_pair(.id=2, .id2=3, .time=20, .time2=50, risk_period=40,
                       .max_t=1000, .max_t2=1000, bounds="(]",
                       allow_overlap=TRUE)
  expect_true(out)
})

test_that("works with NA", {
  # in .time
  out <- is_valid_pair(.id=10, .id2=20, .time=NA, .time2=50, risk_period=30,
                       .max_t=1000, .max_t2=1000, bounds="[]")
  expect_true(is.na(out))

  # in .time2
  out <- is_valid_pair(.id=10, .id2=20, .time=1000, .time2=NA, risk_period=30,
                       .max_t=10000, .max_t2=10000, bounds="[]")
  expect_true(is.na(out))
})
