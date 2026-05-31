
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

## a single iteration of a full bootstrap procedure
one_boot_iter <- function(ids, d_exp, d_events, pairs, n_pairs, risk_period,
                          bounds, estimator, batch_size, rand_max_iter,
                          ...) {

  # get bootstrap sample
  ids_i <- sample(x=ids, size=length(ids), replace=TRUE)
  d_exp_i <- d_exp[data.table(.id=ids_i), on=".id"]

  # perform matching
  d_matches_i <- match_pairs(data=d_exp_i, pairs=pairs, risk_period=risk_period,
                             n_pairs=n_pairs, max_iter=rand_max_iter,
                             batch_size=batch_size)

  if (nrow(d_matches_i)==0) {
    return(NA)
  }

  d_matches_i <- expand_pair_matches(d_matches_i)

  # add outcome event count to it
  d_matches_i <- add_event_count(dt_index=d_matches_i, dt_events=d_events,
                                 risk_period=risk_period,
                                 bounds=bounds)

  if (sum(d_matches_i$.n_events)==0) {
    return(NA)
  }

  # apply estimator
  if (estimator=="moments") {
    est <- estimate_moments(data=d_matches_i, bootstrap=FALSE,
                            n_boot=1000, conf_level=0.95)$est
  } else if (estimator=="glmm") {
    est <- tryCatch({estimate_glmm(data=d_matches_i, ...)$est},
                    error=function(e){return(NA)})
  }

  return(est)
}

## carries out full bootstrapping on the entire procedure
#' @importFrom data.table setDTthreads
perform_bootstrapping <- function(d_exp, d_events, estimator, pairs, n_pairs,
                                  risk_period, bounds, n_boot,
                                  n_cores, progressbar, batch_size,
                                  rand_max_iter, ...) {
  # includable ids
  ids <- unique(d_exp$.id)

  # using a single core
  if (n_cores==1) {
    out <- numeric(n_boot)
    for (i in seq_len(n_boot)) {
      out[i] <- one_boot_iter(ids=ids, d_exp=d_exp, d_events=d_events,
                              pairs=pairs, n_pairs=n_pairs,
                              risk_period=risk_period,
                              bounds=bounds, batch_size=batch_size,
                              rand_max_iter=rand_max_iter,
                              estimator=estimator, ...)
    }
  # using multiple processing cores
  } else {

    requireNamespace("parallel", quietly=TRUE)
    requireNamespace("doRNG", quietly=TRUE)
    requireNamespace("doSNOW", quietly=TRUE)
    requireNamespace("foreach", quietly=TRUE)

    `%dorng%` <- doRNG::`%dorng%`

    # setup clusters
    cl <- parallel::makeCluster(n_cores, outfile="")
    doSNOW::registerDoSNOW(cl)
    pkgs <- c("data.table", "survival", "SPMD", "lme4")

    glob_funs <- ls(envir=.GlobalEnv)[vapply(ls(envir=.GlobalEnv), function(obj)
      "function"==class(eval(parse(text=obj)))[1], FUN.VALUE=logical(1))]

    # progressbar
    if (progressbar) {
      pb <- utils::txtProgressBar(max=n_boot, style=3)
      progress <- function(n) {utils::setTxtProgressBar(pb, n)}
      opts <- list(progress=progress)
    } else {
      opts <- NULL
    }

    # run loop in parallel
    out <- foreach::foreach(i=seq_len(n_boot), .packages=pkgs,
                            .export=glob_funs, .options.snow=opts) %dorng% {
      setDTthreads(1)

      one_boot_iter <- utils::getFromNamespace("one_boot_iter", "SPMD")

      one_boot_iter(ids=ids, d_exp=d_exp, d_events=d_events, pairs=pairs,
                    n_pairs=n_pairs, risk_period=risk_period,
                    bounds=bounds, estimator=estimator, batch_size=batch_size,
                    rand_max_iter=rand_max_iter, ...)
    }
    on.exit(close(pb))
    on.exit(parallel::stopCluster(cl))

    out <- unlist(out)
  }
  return(out)
}

## estimates a p-value from bootstrapped samples
get_boot_p_value <- function(boot_samples, null=1) {
  p_lower <- mean(boot_samples <= null)
  p_upper <- mean(boot_samples >= null)

  p <- 2 * min(p_lower, p_upper)
  p <- min(p, 1)

  # small correction, because p-values cannot actually be 0
  # when derived from bootstrapping
  p <- fifelse(p==0, 1/length(boot_samples), p)

  return(p)
}

## works similar to stopifnot() but allows a custom message in
## a more convenient fashion
stopifnotm <- function(assert, ...) {
  if (!assert) {
    stop(paste(..., collapse=""), call.=FALSE)
  }
}

## similar to stopifnotm() but just returns a warning
warnifnotm <- function(assert, ...) {
  if (!assert) {
    warning(paste(..., collapse=""), call.=FALSE)
  }
}

## input checks for the sym_pair_matching() function
check_inputs_spmd <- function(formula, data, id, risk_period, pairs, n_pairs,
                              estimator, bootstrap, n_boot, conf_level,
                              bounds, batch_size, rand_max_iter) {

  stopifnotm(is.data.frame(data), "'data' must be a data.frame like ",
             "object (tibbles, data.table, etc.).")
  stopifnotm(nrow(data) > 1, "'data' must contain at least 2 rows ",
             "(for valid results, much more).")
  stopifnotm((length(id)==1 && is.character(id) && id %in% colnames(data)),
             "'id' must be a single character string, identifying a valid",
             " column in 'data'.")
  stopifnotm((length(risk_period)==1 && is.numeric(risk_period) &&
                risk_period > 0),
             "'risk_period' must be a single positive number.")
  stopifnotm((length(pairs)==1 && is.character(pairs) &&
                pairs %in% c("one", "all", "random1", "random2")),
             "'pairs' must be either 'one', 'all', 'random1' or 'random2'.")
  stopifnotm((is.null(n_pairs) || (length(n_pairs)==1 && is.numeric(n_pairs) &&
                                     n_pairs >= 1)),
             "'n_pairs' must be either NULL or a positive integer.")
  stopifnotm(!(pairs=="random" && is.null(n_pairs)),
             "'n_pairs' must be specified when pairs='random'.")
  stopifnotm((length(estimator)==1 && is.character(estimator) &&
                estimator %in% c("none", "moments", "glmm")),
             "'estimator' must be either 'none', 'moments' or 'glmm'.")
  stopifnotm((length(bootstrap)==1 && is.logical(bootstrap)),
             "'bootstrap' must be either TRUE or FALSE.")
  stopifnotm((length(n_boot)==1 && is.numeric(n_boot) && n_boot > 0 &&
                round(n_boot)==n_boot),
             "'n_boot' must be a single integer > 0.")
  stopifnotm((length(conf_level)==1 && is.numeric(conf_level) &&
                conf_level > 0 && conf_level < 1),
             "'conf_level' must be a single number < 1 and > 0.")
  stopifnotm((length(bounds)==1 && is.character(bounds) &&
                bounds %in% c("()", "(]", "[)", "[]")),
             "'bounds' must be one of '()', '(]', '[)', or '[]'.")
  stopifnotm((length(batch_size)==1 && is.numeric(batch_size) &&
              round(batch_size)==batch_size && batch_size > 0),
             "'batch_size' must be a single integer > 0.")
  stopifnotm((length(rand_max_iter)==1 && is.numeric(rand_max_iter) &&
                round(rand_max_iter)==rand_max_iter && rand_max_iter > 0),
             "'rand_max_iter' must be a single integer > 0.")

  # check if all variables named in formula are in data
  for (i in seq_len(4)) {
    stopifnotm(formula[[i]] %in% colnames(data),
               "Column '", formula[[i]], "' not found in 'data'.")
  }
}
