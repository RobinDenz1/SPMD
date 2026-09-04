
test_that("general test case", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20),
                     .min_t=0, .max_t=1000)

  expected <- data.table(.id=c(1, 4),
                         .time=c(20, 20),
                         .min_t=0,
                         .max_t=c(1000, 1000),
                         .id2=c(2, 3),
                         .time2=c(45, 80),
                         .min_t2=0,
                         .max_t2=c(1000, 1000),
                         .id_pair=c(1, 2))
  out <- generate_one_pairing(data, risk_period=20, bounds="[]")

  expect_equal(out, expected)
})

test_that("works if no matches are possible", {

  data <- data.table(.id=c(1, 2, 3, 4), .time=c(20, 45, 80, 20),
                     .min_t=0, .max_t=1000)

  expect_error(generate_one_pairing(data, risk_period=200, bounds="[]"),
               paste0("Matching failed. Use random matches ",
                      "(pairs='random1' / pairs='random2') or all matches",
                      " (pairs='all') instead."),
               fixed=TRUE)
})
