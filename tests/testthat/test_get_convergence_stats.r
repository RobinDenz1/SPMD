
test_that("single pair has no shared-pair combinations", {

  d_counts <- data.table(.id1=c(1L), .id2=c(2L))

  out <- get_convergence_stats(d_counts)

  expect_equal(out$A_n, 0)
  expect_equal(out$E_n, 1)
  expect_equal(out$ratio, 0)
})

test_that("disjoint pairs have no shared-pair combinations", {

  d_counts <- data.table(.id1=c(1L, 3L), .id2=c(2L, 4L))

  out <- get_convergence_stats(d_counts)

  expect_equal(out$A_n, 0)
  expect_equal(out$E_n, 2)
  expect_equal(out$ratio, 0)
})

test_that("two pairs sharing one individual are counted correctly", {

  d_counts <- data.table(.id1=c(1L, 1L), .id2=c(2L, 3L))

  out <- get_convergence_stats(d_counts)

  expect_equal(out$A_n, 2)
  expect_equal(out$E_n, 2)
  expect_equal(out$ratio, 0.5)
})

test_that("chain of three pairs is counted correctly", {

  d_counts <- data.table(.id1=c(1L, 2L, 3L), .id2=c(2L, 3L, 4L))

  out <- get_convergence_stats(d_counts)

  expect_equal(out$A_n, 4)
  expect_equal(out$E_n, 3)
  expect_equal(out$ratio, 4 / 9)
})

test_that("star graph is counted correctly", {

  d_counts <- data.table(.id1=c(1L, 1L, 1L, 1L),
                         .id2=c(2L, 3L, 4L, 5L))

  out <- get_convergence_stats(d_counts)

  expect_equal(out$A_n, 12)
  expect_equal(out$E_n, 4)
  expect_equal(out$ratio, 0.75)
})

test_that("pair orientation does not affect the result", {

  d_counts1 <- data.table(.id1=c(1L, 1L, 3L), .id2=c(2L, 3L, 4L))

  d_counts2 <- data.table(.id1=c(2L, 3L, 4L), .id2=c(1L, 1L, 3L))

  out1 <- get_convergence_stats(d_counts1)
  out2 <- get_convergence_stats(d_counts2)

  expect_equal(out1, out2)
})
