
test_that("general test case", {

  data <- data.table(.id=c(1, 2, 3, 4),
                     .time=c(50, 80, 130, 84),
                     .max_t=1000)

  out <- generate_random_pairs(data, risk_period=20, n_pairs=3, bounds="[]")
  expect_equal(nrow(out), 3)
})

test_that("works if no matches are possible", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20), .max_t=1000)

  out <- suppressWarnings(
    generate_random_pairs(data, risk_period=200, n_pairs=10, bounds="[]")
  )
  expect_equal(nrow(out), 0)
})

test_that("warning if less possible pairs than n_pairs", {

  data <- data.table(.id=c(1, 2, 3, 4),
                     .time=c(50, 80, 130, 84),
                     .max_t=1000)

  expect_warning(out <- generate_random_pairs(data, risk_period=20, n_pairs=30,
                                              bounds="[]"),
                 paste0("Cannot generate 30 pairs, because only 5 possible ",
                        "valid pairs exist. Took all possible ",
                        "valid pairs instead."))
})
