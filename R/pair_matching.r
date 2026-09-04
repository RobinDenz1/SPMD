
## create valid pairs from given data
match_pairs <- function(data, pairs, risk_period, bounds, n_pairs=NULL,
                        batch_size=NULL, max_iter=NULL, allow_overlap=FALSE) {

  if (pairs=="one") {
    d_pairs <- generate_one_pairing(data=data, risk_period=risk_period,
                                    bounds=bounds, allow_overlap=allow_overlap)
  } else if (pairs=="all") {
    d_pairs <- generate_all_pairs(data=data, risk_period=risk_period,
                                  bounds=bounds, allow_overlap=allow_overlap)
  } else if (pairs=="random1") {
    d_pairs <- generate_random_pairs_mem(data=data, risk_period=risk_period,
                                         n_pairs=n_pairs, batch_size=batch_size,
                                         max_iter=max_iter, bounds=bounds,
                                         allow_overlap=allow_overlap)
  } else if (pairs=="random2") {
    d_pairs <- generate_random_pairs(data=data, risk_period=risk_period,
                                     n_pairs=n_pairs, bounds=bounds,
                                     allow_overlap=allow_overlap)
  }

  return(d_pairs)
}

## check if two individuals are a valid pairing, e.g. they are not the
## same individual and their risk-periods do not overlap
# NOTE: If either of the bounds is defined using ( or ), intervals
#       that touch each other will have no overlapping information and can
#       thus be used. If both ends are included, however, touching intervals
#       would overlap at the "touch point", thus making them invalid
is_valid_pair <- function(.id, .id2, .time, .time2, .min_t, .min_t2,
                          .max_t, .max_t2, risk_period, bounds,
                          check_censoring=TRUE, check_truncation=TRUE,
                          allow_overlap=FALSE) {

  # check overlap of risk periods
  if (allow_overlap) {
    out <- .id==.id2 | .time==.time2
  } else if (bounds=="[]") {
    out <- .id==.id2 | (abs(.time - .time2) <= risk_period)
  } else {
    out <- .id==.id2 | (abs(.time - .time2) < risk_period)
  }

  # check if follow-up time for each individual is sufficient
  # NOTE: We do not treat this differently depending on the bounds, because
  #       we assume that right-censoring means that we know a person is
  #       event free up to the end of max_t (so effectively we have observed
  #       max_t). It is then sufficient to require that max_t should be
  #       equal to time + risk_period regardless of bounds.
  if (check_censoring) {
    complete_followup <- fifelse(.time <= .time2,
                                 .max_t > .time2 & .max_t2 > .time2,
                                 .max_t > .time & .max_t2 > .time)
  } else {
    complete_followup <- TRUE
  }

  # if observation period starts at different times, this checks if
  # the pair can still be formed
  if (check_truncation) {
    complete_start <- fifelse(.time <= .time2,
                              .min_t <= .time & .min_t2 <= .time,
                              .min_t <= .time2 & .min_t2 <= .time2)
  } else {
    complete_start <- TRUE
  }

  return(!out & complete_followup & complete_start)
}

## generates all possible (and valid) pairings of individuals
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table setnames
generate_all_pairs <- function(data, risk_period, bounds,
                               allow_overlap=FALSE) {

  .idx <- . <- .id <- .id2 <- .time <- .time2 <- .id_pair <- NULL

  # check if there might be too many matches
  n_possible <- (nrow(data) * (nrow(data) - 1)) / 2
  if (n_possible > 10000000) {
    warning("The amount of possible matches (although not all of them",
            " are going to be valid) is > 10 million (around ~ ",
            n_possible, "). This may be infeasible. Consider using",
            " pairs='random' instead.", call.=FALSE, immediate.=TRUE)
  }

  data[, .idx := .I]

  # self-join keeping only i < j to avoid duplicates
  d_pairs <- data[data, on = .(.idx > .idx), allow.cartesian=TRUE, nomatch=0]
  d_pairs[, .idx := NULL]
  setnames(d_pairs,
           old=c("i..id", "i..time", "i..min_t", "i..max_t"),
           new=c(".id2", ".time2", ".min_t2", ".max_t2"))

  # keep only valid pairs
  d_pairs <- remove_invalid_matches(d_pairs=d_pairs, d_exp=data,
                                    risk_period=risk_period, bounds=bounds,
                                    allow_overlap=allow_overlap)
  d_pairs[, .id_pair := seq_len(.N)]

  return(d_pairs)
}

## generates a set of n random (valid) pairs
#' @importFrom data.table .N
generate_random_pairs <- function(data, risk_period, bounds, n_pairs,
                                  allow_overlap=FALSE) {

  d_pairs <- generate_all_pairs(data=data, risk_period=risk_period,
                                bounds=bounds, allow_overlap=allow_overlap)

  if (n_pairs > nrow(d_pairs)) {
    warning("Cannot generate ", format(n_pairs, scientific=FALSE),
            " pairs, because only ", nrow(d_pairs),
            " possible valid pairs exist. Took all possible valid pairs",
            " instead.", call.=FALSE)
  } else {
    inds <- sample.int(n=nrow(d_pairs), size=n_pairs, replace=FALSE)
    d_pairs <- d_pairs[inds]
  }

  return(d_pairs)
}

## generate a set of n random (valid) pairs, without first generating
## all possible pairs, which is more memory efficient but slower
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table .I
generate_random_pairs_mem <- function(data, n_pairs, risk_period, bounds,
                                      batch_size=max(5000L, n_pairs * 2L),
                                      max_iter=100L, allow_overlap=FALSE) {
  . <- .id_pair <- NULL

  n <- nrow(data)

  # don't continue if it is clearly impossible
  if (n_pairs > ((n * (n-1)) / 2)) {
    stop("'n_pairs' is larger than ", ((n * (n-1)) / 2),
         " which is the maximum possible amount of matches (if all possible",
         " matches were valid). Use a smaller value for 'n_pairs' or",
         " use pairs='all' or pairs='random2' instead.", call.=FALSE)
  }

  # stores accepted pairs
  accepted <- data.table(
    .id = integer(),
    .time = numeric(),
    .id2 = integer(),
    .time2 = numeric(),
    .min_t = numeric(),
    .min_t2 = numeric(),
    .max_t = numeric(),
    .max_t2 = numeric()
  )

  # stores unique pair keys
  seen <- new.env(hash=TRUE, parent=emptyenv())

  iter <- 0
  while (nrow(accepted) < n_pairs) {

    # break if maximum iterations reached
    iter <- iter + 1
    if (iter==max_iter) {
      warning("Was unable to generate ", n_pairs, " random matches using",
              " only ", max_iter, " iterations. Proceeding with ",
              nrow(accepted), " randomly matched pairs. Alternatively, trying",
              " pairs='random2' or pairs='all' might help.", call.=FALSE)
      break
    }

    # generate candidate pairs
    i <- sample.int(n=n, size=batch_size, replace=TRUE)
    j <- sample.int(n=n, size=batch_size, replace=TRUE)

    cand <- data.table(i, j)

    # remove self-pairs
    cand <- cand[i != j]

    # canonical ordering => (i,j) == (j,i)
    cand[, c("i", "j") := .(
      pmin(i, j),
      pmax(i, j)
    )]

    # remove duplicates generated in this batch
    cand <- unique(cand)

    if (nrow(cand)==0) {
      next
    }

    # remove already-seen pairs
    keys <- paste0(cand$i, "_", cand$j)

    keep <- !vapply(
      X = keys,
      FUN = exists,
      FUN.VALUE = logical(1),
      envir = seen,
      inherits = FALSE
    )

    cand <- cand[keep]

    if (nrow(cand)==0) {
      next
    }

    keys <- keys[keep]

    # mark as seen immediately
    for (k in keys) {
      assign(k, TRUE, envir=seen)
    }

    # attach pair information
    cand[, `:=`(
      .id = data$.id[i],
      .id2 = data$.id[j],
      .time = data$.time[i],
      .time2 = data$.time[j],
      .min_t = data$.min_t[i],
      .min_t2 = data$.min_t[j],
      .max_t = data$.max_t[i],
      .max_t2 = data$.max_t[j],
      i = NULL,
      j = NULL
    )]

    # remove invalid ones
    cand <- remove_invalid_matches(d_pairs=cand, d_exp=data,
                                   risk_period=risk_period,
                                   bounds=bounds, allow_overlap=allow_overlap)
    if (nrow(cand)==0) {
      next
    }

    accepted <- rbind(accepted, cand, use.names=TRUE)
  }

  accepted <- accepted[seq_len(n_pairs)]
  accepted[, .id_pair := .I]

  return(accepted)
}

## a greedy time sorting algorithm to create a dataset in which every person
## that was exposed at some point in time is in exactly a single pair
#' @importFrom data.table setorder
#' @importFrom data.table setnames
#' @importFrom data.table .N
#' @importFrom data.table :=
generate_one_pairing <- function(data, risk_period, bounds,
                                 allow_overlap=FALSE) {

  .time <- .id_pair <- .max_t <- NULL

  # sort by time
  setorder(data, .time)

  # remove definitely right-censored exposure times
  # NOTE: done here so that this does not later fail the final check, still
  #       need to supply original exposure times to d_exp in checks
  data2 <- subset(data, .time <= .max_t - risk_period)

  # split into two pieces
  d_1 <- data2[seq_len(nrow(data2)/2)]
  d_2 <- data2[seq(nrow(data2)/2+1, nrow(data2))]
  setnames(d_2, old=c(".id", ".time", ".min_t", ".max_t"),
           new=c(".id2", ".time2", ".min_t2", ".max_t2"))

  # put together in one pairwise data.table
  d_pairs <- cbind(d_1, d_2)

  # check if all are valid
  n_prev <- nrow(d_pairs)
  d_pairs <- remove_invalid_matches(d_pairs=d_pairs, d_exp=data,
                                    risk_period=risk_period, bounds=bounds,
                                    allow_overlap=allow_overlap)

  if (n_prev != nrow(d_pairs)) {
    stop("Matching failed. Use random matches (pairs='random1' / ",
         "pairs='random2') or all matches (pairs='all') instead.", call.=FALSE)
  }

  d_pairs[, .id_pair := seq_len(.N)]

  return(d_pairs)
}

## takes a set of pair matches and checks if they are valid
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table setnames
#' @importFrom data.table dcast
remove_invalid_matches <- function(d_pairs, d_exp, risk_period, bounds,
                                   allow_overlap=FALSE) {

  . <- .id <- .id2 <- .time <- .time2 <- .n_exp1 <- .n_exp2 <- .num <-
    is_valid <- .min_t <- .min_t2 <- .max_t <- .max_t2 <- NULL

  # skip more complex processing if only one exposure per person
  if (length(unique(d_exp$.id))==nrow(d_exp)) {

    d_pairs <- subset(d_pairs, is_valid_pair(
      .id=.id, .id2=.id2, .time=.time, .time2=.time2,
      .min_t=.min_t, .min_t2=.min_t2,
      .max_t=.max_t, .max_t2=.max_t2, risk_period=risk_period,
      bounds=bounds, allow_overlap=allow_overlap
      )
    )

  # otherwise, more is needed
  } else {

    # NOTE: Overlap is only allowed in the specific pair we look at,
    #       e.g. if a risk period overlaps with two risk periods at the same
    #       time, it is discarded, even if small parts of it could theoretically
    #       be used

    # apply first check
    d_pairs <- subset(d_pairs, is_valid_pair(.id=.id, .id2=.id2, .time=.time,
                                             .time2=.time2, .max_t=.max_t,
                                             .max_t2=.max_t2,
                                             .min_t=.min_t, .min_t2=.min_t2,
                                             risk_period=risk_period,
                                             bounds=bounds,
                                             allow_overlap=allow_overlap))
    # merge n exposures per id to it
    d_n_exp <- d_exp[, .(.n_exp1 = .N), by=.id]

    d_pairs <- merge(d_pairs, d_n_exp, by=".id", all.x=TRUE)
    setnames(d_n_exp, old=".n_exp1", new=".n_exp2")
    d_pairs <- merge(d_pairs, d_n_exp, by.x=".id2", by.y=".id", all.x=TRUE)

    # those with just one exposure are valid
    d_pairs1 <- subset(d_pairs, .n_exp1==1 & .n_exp2==1)

    ## more complex checking for other cases
    d_pairs <- subset(d_pairs, .n_exp1 > 1 | .n_exp2 > 1)
    d_pairs[, c(".n_exp1", ".n_exp2") := NULL]

    # transform exposure times to long-format
    d_exp[, .num := seq_len(.N), by=.id]
    d_exp_long <- dcast(d_exp, .id ~ .num, value.var=".time")

    # rename columns
    max_exp <- ncol(d_exp_long)
    times_id1 <- paste0("id1_", 1:(max_exp-1))
    colnames(d_exp_long)[2:max_exp] <- times_id1

    # merge to pairs once for .id and once for .id2
    d_pairs <- merge(d_pairs, d_exp_long, by=".id", all.x=TRUE)

    times_id2 <- paste0("id2_", 1:(max_exp-1))
    colnames(d_exp_long)[2:max_exp] <- times_id2
    d_pairs <- merge(d_pairs, d_exp_long, by.x=".id2", by.y=".id", all.x=TRUE)

    # initialize indicator
    d_pairs[, is_valid := TRUE]

    # check if exposure period of .id overlaps with any exposure periods of .id2
    for (k in seq_len(length(times_id2))) {
      d_pairs[!is_valid_pair(.id=.id, .id2=.id2, .time=.time,
                             .time2=get(times_id2[k]),
                             .min_t=.min_t, .min_t2=.min_t2,
                             .max_t=.max_t, .max_t2=.max_t2,
                             risk_period=risk_period,
                             bounds=bounds, check_censoring=FALSE,
                             allow_overlap=FALSE, check_truncation=FALSE),
              is_valid := FALSE]
    }

    # check if exposure period of .id2 overlaps with any exposure periods of .id
    for (k in seq_len(length(times_id1))) {
      d_pairs[!is_valid_pair(.id=.id, .id2=.id2, .time=get(times_id1[k]),
                             .time2=.time2, .max_t=.max_t, .max_t2=.max_t2,
                             .min_t=.min_t, .min_t2=.min_t2,
                             risk_period=risk_period, bounds=bounds,
                             check_censoring=FALSE, allow_overlap=FALSE,
                             check_truncation=FALSE),
              is_valid := FALSE]
    }

    # remove invalid matches
    d_pairs <- subset(d_pairs, is_valid==TRUE)

    # put pairs back together
    cnames <- c(".id", ".time", ".id2", ".time2", ".min_t", ".min_t2",
                ".max_t", ".max_t2")
    d_pairs <- rbind(d_pairs[, cnames, with=FALSE],
                     d_pairs1[, cnames, with=FALSE])
  }
  return(d_pairs)
}
