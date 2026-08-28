
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

  # convergence stats when re-using pairs
  if (pairs!="one" && estimator=="moments") {
    convergence <- get_convergence_stats(out$d_counts)
  } else {
    convergence <- list(A_n=0, E_n=nrow(out$d_counts)^2, ratio=0)
  }

  # add to output
  out$d_time_used <- d_time_used
  out$convergence <- convergence
  out$sizes <- list(n_total=n_total, n_exposed=n_exposed,
                    n_exposed_time=n_exposed_time,
                    n_exposures=n_exposures,
                    n_events=n_events,
                    n_has_event=n_has_event,
                    n_exposed_and_event=n_exposed_and_event)
  class(out) <- "SPMD"

  return(out)
}
