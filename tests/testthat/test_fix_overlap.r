
test_that("fixes intervals", {

  d_input <- data.table(.id=c(1, 2, 2, 1), .time=c(40, 40, 60, 60),
                        .max_t=c(1000, 1000, 1000, 1000),
                        .id_pair=c(1, 1, 1, 1), .A=c(FALSE, TRUE, FALSE, TRUE),
                        .group=c(1, 2, 3, 4), .end_time=c(80, 80, 100, 100))

  out <- fix_overlap(copy(d_input), bounds="()", risk_period=40)

  expected <- data.table(.id=c(1, 2, 2, 1), .time=c(40, 40, 80, 80),
                         .max_t=c(1000, 1000, 1000, 1000),
                         .id_pair=c(1, 1, 1, 1), .A=c(FALSE, TRUE, FALSE, TRUE),
                         .group=c(1, 2, 3, 4), .end_time=c(60, 60, 100, 100),
                         .has_overlap=TRUE)
  expect_equal(out, expected)
})

test_that("ignores intervals with no overlap", {

  d_input <- data.table(.id=c(1, 2, 2, 1), .time=c(40, 40, 60, 60),
                        .max_t=c(1000, 1000, 1000, 1000),
                        .id_pair=c(1, 1, 1, 1), .A=c(FALSE, TRUE, FALSE, TRUE),
                        .group=c(1, 2, 3, 4), .end_time=c(80, 80, 100, 100))

  out <- fix_overlap(copy(d_input), bounds="()", risk_period=10)
  out[, .has_overlap := NULL]

  expect_equal(out, d_input)
})

