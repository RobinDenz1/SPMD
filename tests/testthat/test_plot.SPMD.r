
set.seed(123452)
data <- sim_example_data(n=300, rr=2.5)

out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="all")

test_that("defaults", {
  expect_snapshot_output(plot(out))
})

test_that("with show_events=TRUE", {
  expect_snapshot_output(plot(out, show_events=TRUE))
})

test_that("changing some cosmetics under defaults", {
  expect_snapshot_output(plot(out, fill_exposed="brown", fill_control="orange",
                              xlab="Time in Years"))
})

test_that("changing some cosmetics with show_events=TRUE", {
  expect_snapshot_output(plot(out, show_events=TRUE, event_size=0.5,
                              event_shape=12, event_color="brown"))
})
