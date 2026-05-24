
test_that("general test case", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20))

  expected <- data.table(.id=c(2, 3, 3, 4, 4),
                         .time=c(45, 80, 80, 20, 20),
                         .id2=c(1, 1, 2, 2, 3),
                         .time2=c(20, 20, 45, 45, 80),
                         .id_pair=c(1, 2, 3, 4, 5))

  out <- generate_all_pairs(data, risk_period=20)

  expect_equal(out, expected)
})

test_that("works if no matches are possible", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20))

  out <- generate_all_pairs(data, risk_period=200)
  expect_equal(nrow(out), 0)
})

test_that("warning with very large amount of possible matches", {

  set.seed(3425)
  data <- sim_example_data(n=7000)
  data <- prepare_start_stop(data, start="start", stop="stop", id=".id",
                             exposure="A", outcome="Y")
  d_exp <- get_exposure_times(data)

  expect_warning(generate_all_pairs(d_exp, risk_period=40),
                 paste0("The amount of possible matches (although not all ",
                        "of them are going to be valid) is > 10 million ",
                        "(around ~ 10253656. This may be infeasible. ",
                        "Consider using pairs='random' instead."),
                 fixed=TRUE)
})
