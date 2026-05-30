
set.seed(1234124)
data <- sim_example_data(n=800)

test_that("pairs='one', estimator='moments'", {

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="one", estimator="moments")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("pairs='one', estimator='none'", {

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="one", estimator="none")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("pairs='random', estimator='moments'", {

  set.seed(1234)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="random", estimator="moments",
                           n_pairs=500)
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("pairs='all', estimator='moments'", {

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="all", estimator="moments")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("pairs='one', estimator='glmm'", {

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="one", estimator="glmm")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("pairs='random', estimator='glmm'", {

  set.seed(2344)
  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="random", estimator="moments",
                           n_pairs=300)
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("pairs='all', estimator='moments', with bootstrap", {

  out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                           risk_period=40, pairs="all", estimator="moments",
                           conf_level=0.99)
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})
