
## main function to apply the symmetric pair matching design
#' @export
sym_pair_matching <- function(formula, data, id, risk_period, pairs="one",
                              n_pairs=NULL, estimator="moments", ...) {

  # get info from formula
  form_parsed <- parse_surv_form(formula)

  # create matched dataset
  d_matches <- get_full_data(data=data,
                             id=id,
                             start=form_parsed$start,
                             stop=form_parsed$stop,
                             exposure=form_parsed$exposure,
                             outcome=form_parsed$outcome,
                             pairs=pairs,
                             n_pairs=n_pairs,
                             risk_period=risk_period)

  # initiate output object
  out <- list(d_matches=d_matches,
              inputs=list(pairs=pairs,
                          n_pairs=n_pairs,
                          estimator=estimator,
                          risk_period=risk_period))
  class(out) <- "SPMD"

  # analyse data
  if (estimator=="moments") {

    l_est <- estimate_moments(data=d_matches)

    # add to output
    out$est <- l_est$est
    out$d_counts <- l_est$d_counts
    out$l_sums <- l_est$l_sums
    out$model <- NULL

  } else if (estimator=="glmm") {

    l_est <- estimate_glmm(data=d_matches, ...)

    # add to output
    out$est <- l_est$est
    out$d_counts <- NULL
    out$l_sums <- NULL
    out$model <- l_est$model
  }

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
  # TODO: put info about supplied data + results here
  #       could include information about how much of the data was used etc.
}
