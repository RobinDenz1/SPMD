
test_that("general test case", {

  data <- data.table(.id=c(1, 2, 3, 4),
                     .time=c(50, 80, 130, 84))

  out <- generate_random_pairs(data, risk_period=20, n_pairs=3)
  expect_equal(nrow(out), 3)
})

test_that("works if no matches are possible", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20))

  out <- suppressWarnings(
    generate_random_pairs(data, risk_period=200, n_pairs=10)
  )
  expect_equal(nrow(out), 0)
})

test_that("warning if less possible pairs than n_pairs", {

  data <- data.table(.id=c(1, 2, 3, 4),
                     .time=c(50, 80, 130, 84))

  expect_warning(out <- generate_random_pairs(data, risk_period=20, n_pairs=30),
                 paste0("Cannot generate 30 pairs, because only 5 possible ",
                        "valid pairs exist. Took all possible ",
                        "valid pairs instead."))
})
