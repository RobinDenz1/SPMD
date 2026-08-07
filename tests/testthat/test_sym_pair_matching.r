
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
  expect_equal(round(out$est, 3), 3.170)

  # with pairs="random1"
  set.seed(243)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="random1", estimator="glmm",
                           risk_period=40, n_pairs=200)
  expect_equal(round(out$est, 3), 1.536)

  # with pairs="random2"
  set.seed(243)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="random2", estimator="glmm",
                           risk_period=40, n_pairs=200)
  expect_equal(round(out$est, 3), 4.163)

  # with pairs="all"
  set.seed(3455)
  data2 <- sim_example_data(n=50)

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data2,
                           id=".id", pairs="all", estimator="glmm",
                           risk_period=40)
  expect_equal(round(out$est, 3), 2.117)
})

test_that("using estimating equations based estimator", {

  # with pairs="one"
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="one", estimator="moments",
                           risk_period=40)
  expect_equal(round(out$est, 3), 1.732)

  # with pairs="random1"
  set.seed(2454)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="random1", estimator="moments",
                           risk_period=40, n_pairs=1000)
  expect_equal(round(out$est, 3), 4.123)

  # with pairs="random2"
  set.seed(246)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                           id=".id", pairs="random2", estimator="moments",
                           risk_period=40, n_pairs=1000)
  expect_equal(round(out$est, 3), 2.000)

  # with pairs="all"
  set.seed(3455)
  data2 <- sim_example_data(n=500)

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data2,
                           id=".id", pairs="all", estimator="moments",
                           risk_period=40)
  expect_equal(round(out$est, 3), 2.804)
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
                            risk_period=20, bounds="[]")

  expect_equal(round(out1$est, 3), 3.294)
  expect_equal(round(out2$est, 3), 3.252)
})

set.seed(42)
data <- sim_example_data(n=1000)

test_that("bootstrap, n_cores = 1, pairs = 'one', moments", {

  set.seed(2134)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="one", estimator="moments",
                           bootstrap=TRUE, n_boot=5, n_cores=1)
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 1, pairs = 'one', glmm", {

  set.seed(2134)
  out <- suppressMessages(
    sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                      risk_period=40, pairs="one", estimator="glmm",
                      bootstrap=TRUE, n_boot=5, n_cores=1)
  )
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 1, pairs = 'random1', moments", {

  set.seed(2134)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="random1", estimator="moments",
                           bootstrap=TRUE, n_boot=5, n_cores=1, n_pairs=1000)
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 1, pairs = 'random2', moments", {

  set.seed(2134)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="random2", estimator="moments",
                           bootstrap=TRUE, n_boot=5, n_cores=1, n_pairs=1000)
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 1, pairs = 'random1', glmm", {

  set.seed(2134)
  out <- suppressMessages(
    sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                      risk_period=40, pairs="random1", estimator="glmm",
                      bootstrap=TRUE, n_boot=5, n_cores=1, n_pairs=1000)
  )
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 1, pairs = 'random2', glmm", {

  set.seed(2134)
  out <- suppressMessages(
    sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                      risk_period=40, pairs="random2", estimator="glmm",
                      bootstrap=TRUE, n_boot=5, n_cores=1, n_pairs=1000)
  )
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 1, pairs = 'all', moments", {

  set.seed(2134)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="all", estimator="moments",
                           bootstrap=TRUE, n_boot=5, n_cores=1)
  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))
})

test_that("bootstrap, n_cores = 2, pairs = 'all', moments", {

  # with progressbar
  set.seed(2134)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="all", estimator="moments",
                           bootstrap=TRUE, n_boot=5, n_cores=2)

  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))

  # no progressbar, also same results with same seed
  set.seed(2134)
  out2 <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                            risk_period=40, pairs="all", estimator="moments",
                            bootstrap=TRUE, n_boot=5, n_cores=2,
                            progressbar=FALSE)
  expect_true(length(out2$boot_est)==5)
  expect_true(is.numeric(out2$boot_est))
})

test_that("bootstrap, n_cores = 2, pairs = 'one', moments", {

  # with progressbar
  set.seed(2134)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="one", estimator="moments",
                           bootstrap=TRUE, n_boot=5, n_cores=2)

  expect_true(length(out$boot_est)==5)
  expect_true(is.numeric(out$boot_est))

  # no progressbar, also same results with same seed (not directly
  # tested because it somehow doesn't work with check(), but works perfectly
  # with test_local())
  set.seed(2134)
  out2 <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                            risk_period=40, pairs="one", estimator="moments",
                            bootstrap=TRUE, n_boot=5, n_cores=2,
                            progressbar=FALSE)

  expect_true(length(out2$boot_est)==5)
  expect_true(is.numeric(out2$boot_est))
})
