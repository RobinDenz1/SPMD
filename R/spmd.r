
## main function to apply the symmetric pair matching design
#' @export
sym_pair_matching <- function(formula, data, id, risk_period, bounds="[)",
                              estimator="moments", pairs="random2",
                              n_pairs=100000, batch_size=max(5000, n_pairs * 2),
                              rand_max_iter=100, bootstrap=FALSE, n_boot=1000,
                              conf_level=0.95, n_cores=1, progressbar=TRUE,
                              ...) {

  requireNamespace("data.table", quietly=TRUE)

  # get info from formula
  form_parsed <- parse_surv_form(formula)

  check_inputs_spmd(formula=form_parsed, data=data, id=id,
                    risk_period=risk_period, pairs=pairs, n_pairs=n_pairs,
                    estimator=estimator, bootstrap=bootstrap, n_boot=n_boot,
                    conf_level=conf_level, bounds=bounds,
                    rand_max_iter=rand_max_iter, batch_size=batch_size)

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
                          bounds=bounds,
                          rand_max_iter=rand_max_iter,
                          batch_size=batch_size)

  # initiate output object
  out <- list(d_matches=l_data$d_matches,
              d_events=l_data$d_events,
              inputs=list(pairs=pairs,
                          n_pairs=n_pairs,
                          estimator=estimator,
                          risk_period=risk_period,
                          formula=formula,
                          bootstrap=bootstrap,
                          n_boot=n_boot,
                          conf_level=conf_level,
                          rand_max_iter=rand_max_iter,
                          batch_size=batch_size))
  # analyse data
  if (estimator=="moments") {

    l_est <- estimate_moments(data=l_data$d_matches,
                              bootstrap=bootstrap & pairs=="all",
                              n_boot=n_boot, conf_level=conf_level,
                              n_cores=n_cores, progressbar=progressbar)

    # add to output
    out <- c(out, l_est)
    out$boot_est <- l_est$boot_est
    out$model <- NULL

  } else if (estimator=="glmm") {

    l_est <- estimate_glmm(data=l_data$d_matches, ...)

    # add to output
    out <- c(out, l_est)
    out$d_counts <- NULL
    out$l_sums <- NULL
  }

  # perform full bootstrapping
  if (bootstrap && !(estimator=="moments" && pairs=="all")) {

    out_boot <- perform_bootstrapping(
      d_exp=l_data$d_exp,
      d_events=l_data$d_events,
      estimator=estimator,
      pairs=pairs,
      n_pairs=n_pairs,
      risk_period=risk_period,
      bounds=bounds,
      n_boot=n_boot,
      n_cores=n_cores,
      progressbar=progressbar,
      rand_max_iter=rand_max_iter,
      batch_size=batch_size,
      ...
    )

    out$boot_est <- out_boot
    out$se <- stats::sd(out_boot, na.rm=TRUE)
    out$ci <- stats::quantile(
      x=out_boot, probs=c((1-conf_level)/2, conf_level+((1-conf_level)/2)),
      na.rm=TRUE, names=FALSE
    )
    out$p_value <- get_boot_p_value(out_boot)
  }

  ## calculate some further statistics
  # some numbers describing the sample sizes used
  n_total <- length(unique(data$.id))
  n_exposed <- l_data$n_exposed
  n_exposed_time <- length(unique(l_data$d_exp$.id))
  n_has_event <- length(unique(l_data$d_events$.id))
  n_exposed_and_event <- length(intersect(l_data$d_exp$.id,
                                          l_data$d_events$.id))
  n_exposures <- l_data$n_exposures
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
  class(out) <- "SPMD"

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
  } else if (x$inputs$pairs=="random1" || x$inputs$pairs=="random2") {
    cat(" - using", x$inputs$n_pairs, "random unique symmetric pairs\n")
  }

  cat(" - using a risk-period of", x$inputs$risk_period, "time units\n")

  if (x$inputs$estimator=="moments") {
    cat(" - using the estimating equations based estimator\n")
  } else if (x$inputs$estimator=="glmm") {
    cat(" - using the generalized linear model based estimator\n")
  } else if (x$inputs$estimator=="none") {
    cat(" - without performing estimation\n")
  }
}

## S3 summary method for SPMD objects
#' @importFrom data.table uniqueN
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
  cat("  No. unique exposure times =", object$sizes$n_exposures, "\n")
  cat("  No. exposed individuals with event(s) =",
      object$sizes$n_exposed_and_event, "\n")
  cat(" ", uniqueN(object$d_matches$.id_pair), "symmetric pairs were created\n")
  cat("  ", round((sum(object$d_time_used$.time_used) /
                  sum(object$d_time_used$.max_possible_t))*100, 2), "%",
      " of the included observation time was used\n\n", sep="")

  if (object$inputs$estimator=="none") {
    cat("No estimation was done.\n")
  } else {
    cat("Final estimate:", round(object$est, 3), "\n")
  }

  if (!is.null(object$ci)) {
    cat(object$inputs$conf_level * 100, "% CI: [",
        round(object$ci[1], 3), "; ", round(object$ci[2], 3), "]\n", sep="")
    cat("P-Value:", round(object$p_value, 3), "\n\n")
  } else {
    cat("\n")
  }

  if (object$inputs$estimator=="moments") {
    cat("Estimated using: exp(0.5 * log(",
        object$l_sums$X2_X4, "/", object$l_sums$X1_X3, "))\n",
        sep="")
  }

  if (!is.null(object$ci) && object$inputs$estimator!="none") {
    cat("Bootstrap CI based on", object$inputs$n_boot,
        "bootstrap replications\n")
  }
}
