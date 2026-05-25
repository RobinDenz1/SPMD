
## given a sorted vector of unique times and a duration, calculates the
## amount of unique observation time used
calculate_total_time <- function(times, risk_duration) {
  out <- risk_duration + sum(pmin(diff(times), risk_duration))
  return(out)
}

## calculates the amount of observation time that was actually used
## for each individual
get_times_used <- function(d_matches, data, risk_period) {

  .id <- .time <- . <- .start <- .stop <- .max_possible_t <- .max_t <-
    .min_t <- .used_prop <- .time_used <- NULL

  # unique start times per person
  d_times <- unique(d_matches, by=c(".id", ".time"))
  setkey(d_times, .id, .time)

  # total duration used per person
  d_dur <- d_times[, .(.time_used = calculate_total_time(.time, risk_period)),
                   by=.id]

  # maximal duration observed per person
  d_total <- data[, .(.min_t = min(.start), .max_t = max(.stop)), by=.id]
  d_total[, .max_possible_t := .max_t - .min_t]

  # merge together
  d_dur <- merge(d_dur, d_total[, c(".id", ".max_possible_t")],
                 by=".id", all.x=TRUE)
  d_dur[, .used_prop := .time_used / .max_possible_t]

  return(d_dur)
}

## input checks for the sym_pair_matching() function
check_inputs_spmd <- function(formula, data, id, risk_period, pairs, n_pairs,
                              estimator, include_exp_time, bootstrap,
                              n_boot, conf_level) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame like object (tibbles, data.table, etc.).",
         call.=FALSE)
  } else if (nrow(data) < 2) {
    stop("'data' must contain at least 2 rows (for valid results, much more).",
         call.=FALSE)
  } else  if (!(length(id)==1 && is.character(id) && id %in% colnames(data))) {
    stop("'id' must be a single character string, identifying a valid",
         " column in 'data'.", call.=FALSE)
  } else if (!(length(risk_period)==1 && is.numeric(risk_period) &&
               risk_period > 0)) {
    stop("'risk_period' must be a single positive number.", call.=FALSE)
  } else if (!(length(pairs)==1 && is.character(pairs) &&
               pairs %in% c("one", "all", "random"))) {
    stop("'pairs' must be either 'one', 'all' or 'random'.", call.=FALSE)
  } else if (!(is.null(n_pairs) || (length(n_pairs)==1 && is.numeric(n_pairs) &&
                                    n_pairs >= 1))) {
    stop("'n_pairs' must be either NULL or a positive integer.", call.=FALSE)
  } else if (pairs=="random" && is.null(n_pairs)) {
    stop("'n_pairs' must be specified when pairs='random'.", call.=FALSE)
  } else if (!(length(estimator)==1 && is.character(estimator) &&
               estimator %in% c("none", "moments", "glmm"))) {
    stop("'estimator' must be either 'none', 'moments' or 'glmm'.",
         call.=FALSE)
  } else if (!(length(include_exp_time)==1 && is.logical(include_exp_time))) {
    stop("'include_exp_time' must be either TRUE or FALSE.", call.=FALSE)
  } else if (!(length(bootstrap)==1 && is.logical(bootstrap))) {
    stop("'bootstrap' must be either TRUE or FALSE.", call.=FALSE)
  } else if (!(length(n_boot)==1 && is.numeric(n_boot) && n_boot > 0 &&
               round(n_boot)==n_boot)) {
    stop("'n_boot' must be a single integer > 0.", call.=FALSE)
  } else if (!(length(conf_level)==1 && is.numeric(conf_level) &&
               conf_level > 0 && conf_level < 1)) {
    stop("'conf_level' must be a single number < 1 and > 0.", call.=FALSE)
  }

  for (i in seq_len(4)) {
    if (!formula[[i]] %in% colnames(data)) {
      stop("Column '", formula[[i]], "' not found in 'data'.", call.=FALSE)
    }
  }
}
