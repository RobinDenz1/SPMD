
test_that("general test case", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20))

  expected <- data.table(.id=c(2, 3, 3, 4, 4),
                         .time=c(45, 80, 80, 20, 20),
                         .id2=c(1, 1, 2, 2, 3),
                         .time2=c(20, 20, 45, 45, 80),
                         .id_pair=c(1, 2, 3, 4, 5))

  out <- generate_all_pairs(data, risk_period=20, bounds="[]")
  out2 <- generate_all_pairs(data, risk_period=20, bounds="[)")

  expect_equal(out, expected)
  expect_equal(out2, expected) # bounds make no difference here
})

test_that("works if no matches are possible", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20))

  out <- generate_all_pairs(data, risk_period=200, bounds="()")
  expect_equal(nrow(out), 0)
})

test_that("works with multiple exposures in one person", {

  # pairs possible through id = 2
  data <- data.table(.id=c(1, 2, 4, 4), .time=c(20, 45, 80, 20))

  expected <- data.table(.id=c(4, 4, 2),
                         .time=c(80, 20, 45),
                         .id2=c(2, 2, 1),
                         .time2=c(45, 45, 20),
                         .id_pair=c(1, 2, 3))

  out <- generate_all_pairs(data, risk_period=20, bounds="()")
  expect_equal(out, expected)

  # no pairs possible
  data <- data.table(.id=c(1, 4, 4), .time=c(20, 80, 20))

  out <- generate_all_pairs(data, risk_period=20, bounds="()")
  expect_true(nrow(out)==0)
})

test_that("works with multiple exposures in two persons", {

  data <- data.table(.id=c(1, 1, 4, 4), .time=c(20, 45, 80, 20))

  expected <- data.table(.id=c(4),
                         .time=c(80),
                         .id2=c(1),
                         .time2=c(45),
                         .id_pair=c(1))

  out <- generate_all_pairs(data, risk_period=20, bounds="[]")
  expect_equal(out, expected)

})

test_that("with [], less matches are found than with other bounds", {

  data <- data.table(.id=c(1, 2, 3, 4, 5, 6),
                     .time=c(20, 45, 80, 20, 40, 88))

  out1 <- generate_all_pairs(data, risk_period=20, bounds="[]")
  out2 <- generate_all_pairs(data, risk_period=20, bounds="(]")
  out3 <- generate_all_pairs(data, risk_period=20, bounds="[)")
  out4 <- generate_all_pairs(data, risk_period=20, bounds="()")

  expect_equal(nrow(out1), 10)
  expect_equal(nrow(out2), 12)
  expect_equal(nrow(out3), 12)
  expect_equal(nrow(out4), 12)
})

test_that("warning with very large amount of possible matches", {

  set.seed(3425)
  data <- sim_example_data(n=7000)
  data <- prepare_start_stop(data, start="start", stop="stop", id=".id",
                             exposure="A", outcome="Y")
  d_exp <- get_exposure_times(data)

  expect_warning(generate_all_pairs(d_exp, risk_period=40, bounds="[]"),
                 paste0("The amount of possible matches (although not all ",
                        "of them are going to be valid) is > 10 million ",
                        "(around ~ 10253656). This may be infeasible. ",
                        "Consider using pairs='random' instead."),
                 fixed=TRUE)
})
