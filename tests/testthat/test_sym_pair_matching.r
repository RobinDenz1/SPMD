
set.seed(1234)
data <- sim_example_data(n=1000)

test_that("names same as internal names", {

  data2 <- copy(data)
  setnames(data2, old=c("start", "stop", "A", "Y"),
           new=c(".start", ".stop", ".A", ".Y"))

  out <- sym_pair_matching(Surv(.start, .stop, .Y) ~ .A, data=data2,
                           id=".id", pairs="one", estimator="moments",
                           risk_period=40)
  expect_equal(round(out$est, 3), 1.732)
})

test_that("using glmm", {

  # with pairs="one"
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="one", estimator="glmm",
                           risk_period=40)
  expect_equal(round(out$est, 3), 3.212)

  # with pairs="random"
  set.seed(243)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="random", estimator="glmm",
                           risk_period=40, n_pairs=200)
  expect_equal(round(out$est, 3), 1.648)

  # with pairs="all"
  set.seed(3455)
  data2 <- sim_example_data(n=50)

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data2,
                           id=".id", pairs="all", estimator="glmm",
                           risk_period=40)
  expect_equal(round(out$est, 3), 2.088)
})

test_that("using estimating equations based estimator", {

  # with pairs="one"
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="one", estimator="moments",
                           risk_period=40)
  expect_equal(round(out$est, 3), 1.732)

  # with pairs="random"
  set.seed(243)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="random", estimator="moments",
                           risk_period=40, n_pairs=200)
  expect_equal(round(out$est, 3), 2)

  # with pairs="all"
  set.seed(3455)
  data2 <- sim_example_data(n=500)

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data2,
                           id=".id", pairs="all", estimator="moments",
                           risk_period=40)
  expect_equal(round(out$est, 3), 2.717)
})

test_that("not using an estimator", {

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="one", estimator="none",
                           risk_period=40)
  expect_equal(nrow(out$d_matches), 1928)
})

test_that("different risk_period changes results", {

  out1 <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                            id=".id", pairs="one", estimator="moments",
                            risk_period=20)
  out2 <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                            id=".id", pairs="one", estimator="moments",
                            risk_period=50)

  expect_equal(round(out1$est, 3), 1.414)
  expect_equal(round(out2$est, 3), 1.732)
})

test_that("different handling of risk_period", {

  out1 <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                            id=".id", pairs="all", estimator="moments",
                            risk_period=20)
  out2 <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                            id=".id", pairs="all", estimator="moments",
                            risk_period=20, include_exp_time=FALSE)

  expect_equal(round(out1$est, 3), 3.252)
  expect_equal(round(out2$est, 3), 3.457)
})
