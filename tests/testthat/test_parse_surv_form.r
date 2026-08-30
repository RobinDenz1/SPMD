
test_that("parse_surv_form correctly parses valid formula", {

  f <- Surv(start, stop, outcome) ~ exposure

  expected <- list(
    start = "start",
    stop = "stop",
    outcome = "outcome",
    exposure = "exposure"
  )

  expect_identical(parse_surv_form(f), expected=expected)
})

test_that("parse_surv_form rejects non-formulas", {
  expect_error(parse_surv_form("Surv(start, stop, outcome) ~ exposure"),
               "'formula' must be a formula")
  expect_error(parse_surv_form(list(Surv(start, stop, outcome) ~ exposure)),
               "'formula' must be a formula")
})

test_that("parse_surv_form requires Surv() on the left-hand side", {
  expect_error(parse_surv_form(outcome ~ exposure), "Surv\\(\\) object")
  expect_error(parse_surv_form(start + stop + outcome ~ exposure),
               "Surv\\(\\) object")
})

test_that("parse_surv_form requires exactly three Surv arguments", {
  expect_error(parse_surv_form(Surv(start, stop) ~ exposure))
  expect_error(parse_surv_form(Surv(start, stop, outcome, extra) ~ exposure))
})

test_that("parse_surv_form requires simple Surv variables", {
  expect_error(parse_surv_form(Surv(log(start), stop, outcome) ~ exposure),
               "must each be a variable name")
  expect_error(parse_surv_form(Surv(start, stop + 1, outcome) ~ exposure),
               "must each be a variable name")
  expect_error(parse_surv_form(Surv(start, stop, factor(outcome)) ~ exposure),
               "must each be a variable name")
})

test_that("parse_surv_form requires a single exposure variable", {
  expect_error(parse_surv_form(Surv(start, stop, outcome) ~ factor(exposure)),
               "single variable name")
  expect_error(parse_surv_form(Surv(start, stop, outcome) ~ exposure + age),
               "single variable name")
  expect_error(parse_surv_form(Surv(start, stop, outcome) ~ exposure * age),
               "single variable name")
  expect_error(parse_surv_form(Surv(start, stop, outcome) ~ log(exposure)),
               "single variable name")
})
