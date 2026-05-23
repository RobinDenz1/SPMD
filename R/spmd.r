
## main function to apply the symmetric pair matching design
#' @export
sym_pair_matching <- function(formula, data, id, risk_period, pairs="one",
                              n_pairs=NULL, estimator="moments",
                              include_exp_time=TRUE, ...) {

  # get info from formula
  form_parsed <- parse_surv_form(formula)

  check_inputs_spmd(formula=form_parsed, data=data, id=id,
                    risk_period=risk_period, pairs=pairs, n_pairs=n_pairs,
                    estimator=estimator, include_exp_time=include_exp_time)

  # create matched dataset
  l_data <- get_full_data(data=data,
                          id=id,
                          start=form_parsed$start,
                          stop=form_parsed$stop,
                          exposure=form_parsed$exposure,
                          outcome=form_parsed$outcome,
                          pairs=pairs,
                          n_pairs=n_pairs,
                          risk_period=risk_period,
                          remove_noevents=estimator=="moments",
                          include_exp_time=include_exp_time)

  # initiate output object
  out <- list(d_matches=l_data$d_matches,
              inputs=list(pairs=pairs,
                          n_pairs=n_pairs,
                          estimator=estimator,
                          risk_period=risk_period,
                          formula=formula))
  class(out) <- "SPMD"

  # analyse data
  if (estimator=="moments") {

    l_est <- estimate_moments(data=l_data$d_matches)

    # add to output
    out$est <- l_est$est
    out$d_counts <- l_est$d_counts
    out$l_sums <- l_est$l_sums
    out$model <- NULL

  } else if (estimator=="glmm") {

    l_est <- estimate_glmm(data=l_data$d_matches, ...)

    # add to output
    out$est <- l_est$est
    out$d_counts <- NULL
    out$l_sums <- NULL
    out$model <- l_est$model
  }

  ## calculate some further statistics
  # some numbers describing the sample sizes used
  n_total <- length(unique(data$.id))
  n_exposed <- l_data$n_exposed
  n_exposed_time <- length(unique(l_data$d_exp$.id))
  n_has_event <- length(unique(l_data$d_events$.id))
  n_exposed_and_event <- length(intersect(l_data$d_exp$.id,
                                          l_data$d_events$.id))
  n_exposures <- nrow(l_data$d_exp)
  n_events <- nrow(l_data$d_events)

  # amount of observation time used from included individuals
  d_time_used <- get_times_used(d_matches=l_data$d_matches,
                                data=l_data$data,
                                risk_period=risk_period)

  # add to output
  out$d_time_used <- d_time_used
  out$sizes <- list(n_total=n_total, n_exposed=n_exposed,
                    n_exposed_time=n_exposed_time,
                    n_exposures=n_exposures,
                    n_events=n_events,
                    n_has_event=n_has_event,
                    n_exposed_and_event=n_exposed_and_event)
  return(out)
}

## S3 print method for SPMD objects
#' @export
print.SPMD <- function(x, ...) {

  cat("A SPMD object\n")

  if (x$inputs$pairs=="one") {
    cat(" - using each individual in a single symmetric pair\n")
  } else if (x$inputs$pairs=="all") {
    cat(" - using all possible unique symmetric pairs\n")
  } else if (x$inputs$pairs=="random") {
    cat(" - using", x$inputs$n_pairs, "random unique symmetric pairs\n")
  }

  cat(" - using a risk-period of", x$inputs$risk_period, "time units\n")

  if (x$inputs$estimator=="moments") {
    cat(" - using the estimating equations based estimator\n")
  } else if (x$inputs$estimator=="glmm") {
    cat(" - using the generalized linear model based estimator\n")
  }
}

## S3 summary method for SPMD objects
#' @export
summary.SPMD <- function(object, ...) {

  cat("Symmetric Pair Matching")

  if (object$inputs$estimator=="moments") {
    cat(" using an estimating equation based estimator\n")
  } else if (object$inputs$estimator=="glmm") {
    cat(" using a generalized linear mixed model based estimator\n")
  }

  cat("  Formula:", format(object$inputs$formula), "\n")
  cat("  Risk period:", object$input$risk_period, "\n\n")
  cat("  No. individuals in data =", object$sizes$n_total, "\n")
  cat("  No. exposed individuals =", object$sizes$n_exposed, "\n")
  cat("  No. exposed with event(s) =", object$sizes$n_exposed_and_event, "\n")
  cat(" ", max(object$d_matches$.id_pair), "symmetric pairs were created\n")
  cat("  ", round((sum(object$d_time_used$.time_used) /
                  sum(object$d_time_used$.max_possible_t))*100, 2), "%",
      " of the included observation time was used\n\n", sep="")

  if (object$inputs$estimator=="none") {
    cat("No estimation was done.\n")
  } else {
    cat("Final estimate:", round(object$est, 3), "\n\n")
  }

  if (object$inputs$estimator=="moments") {
    cat("Estimated using: exp(0.5 * log(",
        object$l_sums$X2_X4, "/", object$l_sums$X1_X3, "))\n",
        sep="")
  }
}
