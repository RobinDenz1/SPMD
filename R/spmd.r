
## main function to apply the symmetric pair matching design
#' @importFrom data.table :=
#' @importFrom data.table uniqueN
#' @importFrom data.table fifelse
#' @export
sym_pair_matching <- function(formula, data, id, risk_period, bounds="[)",
                              estimator="moments", pairs="random2",
                              n_pairs=100000, batch_size=max(5000, n_pairs * 2),
                              rand_max_iter=100, allow_overlap=FALSE,
                              conf_type="auto", conf_level=0.95, n_boot=1000,
                              n_cores=1, progressbar=TRUE, convergence=TRUE,
                              ...) {

  . <- .id <- .max_t <- .time <- .censored <- .had_overlap <- V1 <- NULL

  requireNamespace("data.table", quietly=TRUE)

  # get info from formula
  form_parsed <- parse_surv_form(formula)

  check_inputs_spmd(formula=form_parsed, data=data, id=id,
                    risk_period=risk_period, pairs=pairs, n_pairs=n_pairs,
                    estimator=estimator, conf_type=conf_type, n_boot=n_boot,
                    conf_level=conf_level, bounds=bounds,
                    rand_max_iter=rand_max_iter, batch_size=batch_size,
                    convergence=convergence, allow_overlap=allow_overlap)

  # set type of confidence interval automatically if conf_type = "auto"
  if (conf_type=="auto") {
    conf_type <- fifelse(estimator=="moments" & pairs=="all", "jackknife",
                         "none")
  }

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
                          batch_size=batch_size,
                          allow_overlap=allow_overlap)

  # initiate output object
  out <- list(d_matches=l_data$d_matches,
              d_events=l_data$d_events,
              inputs=list(pairs=pairs,
                          n_pairs=n_pairs,
                          estimator=estimator,
                          risk_period=risk_period,
                          formula=formula,
                          conf_type=conf_type,
                          conf_level=conf_level,
                          n_boot=n_boot,
                          rand_max_iter=rand_max_iter,
                          batch_size=batch_size,
                          convergence=convergence,
                          allow_overlap=allow_overlap))
  # analyse data
  if (estimator=="moments") {

    l_est <- estimate_moments(data=l_data$d_matches,
                              bootstrap=(conf_type=="boot.fast"),
                              n_boot=n_boot, conf_level=conf_level,
                              n_cores=n_cores, progressbar=progressbar)

    # add to output
    out <- c(out, l_est)
    out$boot_est <- l_est$boot_est
    out$model <- NULL

    # warn if NA or Inf
    warnifnotm(!(is.na(log(out$est)) || is.infinite(out$est)),
               "The final estimate is NA or not finite. Estimation likely",
               "failed due to rare events.")

  } else if (estimator=="glmm") {

    l_est <- estimate_glmm(data=l_data$d_matches, ...)

    # add to output
    out <- c(out, l_est)
    out$d_counts <- NULL
    out$l_sums <- NULL
  }

  # perform full bootstrapping
  if (conf_type=="boot") {

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
      allow_overlap=allow_overlap,
      ...
    )

    out$boot_est <- out_boot
    out$se <- stats::sd(out_boot, na.rm=TRUE)
    out$ci <- stats::quantile(
      x=out_boot, probs=c((1-conf_level)/2, conf_level+((1-conf_level)/2)),
      na.rm=TRUE, names=FALSE
    )
    out$p_value <- get_boot_p_value(out_boot)
    out$n_boot_na <- sum(is.na(log(out_boot)) | is.infinite(log(out_boot)))
  }

  # warn if any NA or Inf in bootstrap estimates
  if ((conf_type=="boot" | conf_type=="boot.fast") && out$n_boot_na > 0) {
    warning(out$n_boot_na, " bootstrap estimates were",
            " NA or infinite, which may happen with estimator='moments'",
            " if either the denominator or the numerator is 0. With",
            " estimator='glmm' it might be due to failed convergence.",
            " Proceed with caution.", call.=FALSE)
  }

  # if needed, calculate approximate variance
  if (conf_type=="jackknife") {
    jackknife_ci <- get_spm_ci_ijk(out$d_counts, conf_level=conf_level)

    # multiply by IRR to show SE on IRR scale
    out$se <- jackknife_ci$se * out$est
    out$ci <- jackknife_ci$irr_ci
    out$p_value <- jackknife_ci$p_value
  }

  ## calculate some further statistics
  # some numbers describing the sample sizes used
  n_total <- uniqueN(data$.id)
  n_exposed <- l_data$n_exposed_all
  n_exposed_included <- l_data$n_exposed
  n_exposures <- l_data$n_exposures_all
  n_exposures_included <- l_data$n_exposures
  n_has_event <- data[, sum(any(get(form_parsed$outcome))), by=id][, sum(V1)]
  n_exposed_and_event <- length(
    intersect(l_data$d_exp$.id, l_data$d_events$.id)
  )

  n_events <- nrow(l_data$d_events)
  n_had_overlap <- sum(out$d_matches$.had_overlap) / 4

  out$d_matches[, .censored := any((.time + risk_period) > .max_t), by=.id]
  n_censored <- sum(out$d_matches$.censored)

  # amount of observation time used from included individuals
  d_time_used <- get_times_used(d_matches=l_data$d_matches,
                                data=l_data$data,
                                risk_period=risk_period)

  # little cleanup of d_matches
  out$d_matches[, .had_overlap := NULL]
  out$d_matches[, .censored := NULL]

  # convergence stats when re-using pairs
  if (pairs!="one" && estimator=="moments" && convergence==TRUE) {
    est_convergence <- get_convergence_stats(out$d_counts)
  } else if (convergence) {
    est_convergence <- list(A_n=0, E_n=nrow(out$d_counts), ratio=0)
  } else {
    est_convergence <- NULL
  }

  # add to output
  out$d_time_used <- d_time_used
  out$convergence <- est_convergence
  out$sizes <- list(n_total=n_total,
                    n_exposed=n_exposed,
                    n_exposures=n_exposures,
                    n_exposed_included=n_exposed_included,
                    n_exposures_included=n_exposures_included,
                    n_events=n_events,
                    n_has_event=n_has_event,
                    n_exposed_and_event=n_exposed_and_event,
                    n_censored=n_censored,
                    n_had_overlap=n_had_overlap)
  class(out) <- "SPMD"

  return(out)
}
