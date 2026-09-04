
# NOTE: these tests were mostly AI generated, because it is not crucial
#       functionality and I did not have the patience to write those myself.

test_that("check_inputs_spmd accepts valid inputs", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  expect_silent(
    check_inputs_spmd(
      formula = c("A", "Y", "time", "id"),
      data = data,
      id = "id",
      risk_period = 10,
      pairs = "one",
      n_pairs = NULL,
      estimator = "none",
      bootstrap = FALSE,
      n_boot = 100,
      conf_level = 0.95,
      bounds = "()",
      batch_size = 100,
      rand_max_iter = 1000,
      convergence = TRUE,
      allow_overlap = TRUE
    )
  )
})

test_that("check_inputs_spmd checks data", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  expect_error(
    check_inputs_spmd(
      formula = c("A", "Y", "time", "id"),
      data = list(),
      id = "id",
      risk_period = 10,
      pairs = "one",
      n_pairs = NULL,
      estimator = "none",
      bootstrap = FALSE,
      n_boot = 100,
      conf_level = 0.95,
      bounds = "()",
      batch_size = 100,
      rand_max_iter = 1000,
      convergence = TRUE,
      allow_overlap = TRUE
    ),
    "'data' must be a data.frame"
  )

  expect_error(
    check_inputs_spmd(
      formula = c("A", "Y", "time", "id"),
      data = data.frame(id = 1),
      id = "id",
      risk_period = 10,
      pairs = "one",
      n_pairs = NULL,
      estimator = "none",
      bootstrap = FALSE,
      n_boot = 100,
      conf_level = 0.95,
      bounds = "()",
      batch_size = 100,
      rand_max_iter = 1000,
      convergence = TRUE,
      allow_overlap = TRUE
    ),
    "'data' must contain at least 2 rows"
  )
})

test_that("check_inputs_spmd checks id", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  # id does not exist
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "missing",
      10, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'id' must be a single character string"
  )

  # id is not character
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, 1,
      10, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'id' must be a single character string"
  )

  # id has length > 1
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, c("id", "A"),
      10, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'id' must be a single character string"
  )

  # id column is not numeric
  data_bad <- data
  data_bad$id <- as.character(data_bad$id)

  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data_bad, "id",
      10, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ), "'id' must be a single character string")
})

test_that("check_inputs_spmd checks risk_period", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  # zero
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      0, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ), "'risk_period' must be a single positive number")

  # negative
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      -1, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE),
    "'risk_period' must be a single positive number"
  )

  # non-numeric
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      "10", "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'risk_period' must be a single positive number"
  )

  # length > 1
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      c(10, 20), "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'risk_period' must be a single positive number"
  )
})

test_that("check_inputs_spmd checks pairs", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_pairs in list(
    "random",
    "invalid",
    1,
    c("one", "all"),
    NULL
  )) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, bad_pairs, NULL, "none", FALSE, 100, .95,
        "()", 100, 1000, TRUE, TRUE
      ),
      "'pairs' must be either"
    )
  }
})

test_that("check_inputs_spmd checks n_pairs", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  # n_pairs = NULL is valid
  expect_silent(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      10, "one", NULL, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    )
  )

  # zero
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      10, "one", 0, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'n_pairs' must be either NULL or a positive integer"
  )

  # negative
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      10, "one", -1, "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'n_pairs' must be either NULL or a positive integer"
  )

  # non-numeric
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      10, "one", "10", "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'n_pairs' must be either NULL or a positive integer"
  )

  # length > 1
  expect_error(
    check_inputs_spmd(
      c("A", "Y", "time", "id"), data, "id",
      10, "one", c(1, 2), "none", FALSE, 100, .95,
      "()", 100, 1000, TRUE, TRUE
    ),
    "'n_pairs' must be either NULL or a positive integer"
  )
})

test_that("check_inputs_spmd checks estimator", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_estimator in list(
    "invalid",
    1,
    c("none", "moments"),
    NULL
  )) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, bad_estimator, FALSE, 100, .95,
        "()", 100, 1000, TRUE, TRUE
      ),
      "'estimator' must be either"
    )
  }
})

test_that("check_inputs_spmd checks bootstrap", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_bootstrap in list(1, "TRUE", c(TRUE, FALSE), NULL)) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", bad_bootstrap, 100, .95,
        "()", 100, 1000, TRUE, TRUE
      ),
      "'bootstrap' must be either"
    )}
})

test_that("check_inputs_spmd checks n_boot", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_n_boot in list(0, -1, 1.5, "100", c(100, 200), NULL)) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", FALSE, bad_n_boot, .95,
        "()", 100, 1000, TRUE, TRUE
      ),
      "'n_boot' must be a single integer > 0"
    )
  }
})

test_that("check_inputs_spmd checks conf_level", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_conf_level in list(0, 1, -0.1, 1.1, "0.95", c(.95, .99), NULL)) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", FALSE, 100, bad_conf_level,
        "()", 100, 1000, TRUE, TRUE
      ),
      "'conf_level' must be a single number < 1 and > 0"
    )
  }
})

test_that("check_inputs_spmd checks bounds", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_bounds in list("invalid", "[", "(]", c("()", "[]"), 1, NULL)) {

    # "(]" and "[]" are actually valid, so only test invalid values here
    if (!identical(bad_bounds, "(]")) {
      expect_error(
        check_inputs_spmd(
          c("A", "Y", "time", "id"), data, "id",
          10, "one", NULL, "none", FALSE, 100, .95,
          bad_bounds, 100, 1000, TRUE, TRUE
        ),
        "'bounds' must be one of"
      )
    }
  }

  # Explicitly check all four valid possibilities
  for (valid_bounds in c("()", "(]", "[)", "[]")) {
    expect_silent(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", FALSE, 100, .95,
        valid_bounds, 100, 1000, TRUE, TRUE
      )
    )
  }
})

test_that("check_inputs_spmd checks batch_size", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_batch_size in list(0, -1, 1.5, "100", c(100, 200), NULL)) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", FALSE, 100, .95,
        "()", bad_batch_size, 1000, TRUE, TRUE
      ),
      "'batch_size' must be a single integer > 0"
    )
  }
})

test_that("check_inputs_spmd checks rand_max_iter", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_rand_max_iter in list(0, -1, 1.5, "1000", c(1000, 2000), NULL)) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", FALSE, 100, .95,
        "()", 100, bad_rand_max_iter, TRUE, TRUE
      ),
      "'rand_max_iter' must be a single integer > 0"
    )
  }
})

test_that("check_inputs_spmd checks convergence", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  for (bad_convergence in list(1, 0, "TRUE", c(TRUE, FALSE), NULL)) {

    expect_error(
      check_inputs_spmd(
        c("A", "Y", "time", "id"), data, "id",
        10, "one", NULL, "none", FALSE, 100, .95,
        "()", 100, 1000, bad_convergence, TRUE
      ),
      "'convergence' must be either"
    )
  }
})

test_that("check_inputs_spmd checks formula variables", {

  data <- data.frame(
    id = 1:4,
    A = c(0, 1, 0, 1),
    Y = c(1, 0, 1, 0),
    time = 1:4
  )

  # Each of the four positions in formula[[i]] should be checked.
  for (i in 1:4) {

    bad_formula <- c("A", "Y", "time", "id")
    bad_formula[i] <- "missing"

    expect_error(
      check_inputs_spmd(
        formula = bad_formula,
        data = data,
        id = "id",
        risk_period = 10,
        pairs = "one",
        n_pairs = NULL,
        estimator = "none",
        bootstrap = FALSE,
        n_boot = 100,
        conf_level = .95,
        bounds = "()",
        batch_size = 100,
        rand_max_iter = 1000,
        convergence = TRUE,
        allow_overlap = TRUE
      ),
      "Column ' missing ' not found in 'data'."
    )
  }
})
