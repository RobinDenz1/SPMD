
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

  stopifnot(inherits(object, "SPMD"))

  inputs <- object$inputs
  sizes  <- object$sizes

  ## design information
  pairing <- switch(
    inputs$pairs,
    one = "One pair per individual",
    all = "All possible pairs",
    random1 = paste0(
      "Random pairs (", format(inputs$n_pairs, big.mark = ","), ")"
    ),
    random2 = paste0(
      "Random pairs (", format(inputs$n_pairs, big.mark = ","), ")"
    ),
    as.character(inputs$pairs)
  )

  estimator <- switch(
    inputs$estimator,
    moments = "Estimating equations",
    glmm = "Generalized linear mixed model",
    none = "None",
    as.character(inputs$estimator)
  )

  # number of symmetric pairs
  n_pairs <- ifelse(!is.null(object$d_matches) &&
                      ".id_pair" %in% names(object$d_matches),
                    data.table::uniqueN(object$d_matches$.id_pair), NA_integer_)

  # proportion of observation time used
  max_time  <- sum(object$d_time_used$.max_possible_t, na.rm=TRUE)
  used_time <- sum(object$d_time_used$.time_used, na.rm=TRUE)
  time_used <- 100 * used_time / max_time

  # effect estimate
  estimate <- object$est
  se <- object$se
  ci <- object$ci
  p_value <- object$p_value
  log_rr <- ifelse(is.finite(estimate), log(estimate), NA_real_)
  boot_n <- ifelse(!is.null(object$boot_est), length(object$boot_est), 0)

  # estimating equation
  estimating_equation <- NULL

  if (inputs$estimator=="moments") {
    estimating_equation <- paste0(
      "exp{1/2 log(",
      format(object$l_sums$X2_X4, big.mark = ","),
      " / ",
      format(object$l_sums$X1_X3, big.mark = ","),
      ")}"
    )
  }

  # return structured summary object
  out <- list(
    design = list(
      risk_period = inputs$risk_period,
      pairing = pairing,
      estimator = estimator
    ),

    sample = list(
      individuals = sizes$n_total,
      exposed = sizes$n_exposed,
      exposure_episodes = sizes$n_exposures,
      individuals_with_event = sizes$n_has_event,
      exposed_with_event = sizes$n_exposed_and_event,
      symmetric_pairs = n_pairs,
      observation_time_used = time_used
    ),

    estimate = list(
      log_rr = log_rr,
      rr = estimate,
      se = se,
      ci = ci,
      conf_level = inputs$conf_level,
      p_value = p_value
    ),
    convergence = object$convergence,
    bootstrap = list(n = boot_n, n_na=object$n_boot_na),
    estimating_equation = estimating_equation,
    object = object
  )

  class(out) <- "summary.SPMD"
  out
}

## print a symmetric pair matching summary() object
#' @exportS3Method
print.summary.SPMD <- function(x, ...) {

  line <- strrep("\u2500", 62)

  cat(line, sep = "")
  cat("\nSymmetric Pair Matching Design\n")
  cat(line, "\n", sep = "")

  # design
  cat("Design\n")
  cat(sprintf("  %-32s %s\n", "Risk period", x$design$risk_period))
  cat(sprintf("  %-32s %s\n", "Pairing strategy", x$design$pairing))
  cat(sprintf("  %-32s %s\n", "Estimator", x$design$estimator))

  # sample
  cat("\nSample\n")
  sample_rows <- c(
    "Individuals" = x$sample$individuals,
    "Exposed individuals" = x$sample$exposed,
    "Exposure episodes" = x$sample$exposure_episodes,
    "Individuals with >=1 event" = x$sample$individuals_with_event,
    "Exposed + event" = x$sample$exposed_with_event,
    "Symmetric pairs" = x$sample$symmetric_pairs
  )

  for (i in seq_along(sample_rows)) {

    value <- sample_rows[[i]]

    if (is.na(value)) {
      value <- "NA"
    } else {
      value <- format(value, big.mark=",", scientific=FALSE, trim=TRUE)
    }

    cat(sprintf("  %-32s %s\n", names(sample_rows)[i], value))
  }
  cat(sprintf("  %-32s %.2f%%\n", "Observation time used",
              x$sample$observation_time_used))

  # effect estimate
  cat("\nEffect estimate\n")

  if (length(x$estimate$rr) == 0 || !is.finite(x$estimate$rr)) {
    cat("  No finite estimate available.\n")
  } else if (is.null(x$estimate$ci)) {
    cat(sprintf("  %-10s %s\n", "log(RR)", "RR"))
    cat(sprintf("  %-10.3f %.3f\n", x$estimate$log_rr, x$estimate$rr))
  } else {
    ci_label <- paste0(x$estimate$conf_level * 100, "% CI")

    cat(sprintf("  %-10s %-10s %-10s %-15s %s\n", "log(RR)", "RR", "SE",
                ci_label, "P-value"))
    ci_string <- sprintf("%.3f \u2013 %.3f", x$estimate$ci[1],
                         x$estimate$ci[2])
    p_string <- format.pval(x$estimate$p_value, digits=3, eps=0.001)
    cat(sprintf("  %-10.3f %-10.3f %-10.3f %-17s %s\n", x$estimate$log_rr,
                x$estimate$rr, x$estimate$se, ci_string, p_string))
  }

  # Bootstrap
  if (x$bootstrap$n > 0 && x$bootstrap$n_na > 0) {
    cat(sprintf("\nBootstrap: %s replicates %s%s%s\n",
                format(x$bootstrap$n, big.mark = ","), "(",
                format(x$bootstrap$n_na, big.mark = ","), " NA or Inf)"))
  } else if (x$bootstrap$n > 0) {
    cat(sprintf("\nBootstrap: %s replicates\n",
                format(x$bootstrap$n, big.mark = ",")))
  }

  # Estimating equation
  if (!is.null(x$estimating_equation)) {
    cat("\nEstimation\n")
    cat("  Estimating equation: ", x$estimating_equation, "\n", sep="")

    if (x$object$inputs$estimator=="moments" &&
        x$object$inputs$convergence==TRUE) {
      cat("  |A_n| / |E_n|^2: ", x$convergence$ratio, "\n", sep="")
    }
  }

  cat(line, "\n", sep = "")
  invisible(x)
}
