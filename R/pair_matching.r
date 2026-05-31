
## create valid pairs from given data
match_pairs <- function(data, pairs, risk_period, n_pairs=NULL,
                        batch_size=NULL, max_iter=NULL) {

  if (pairs=="one") {
    d_pairs <- generate_one_pairing(data=data, risk_period=risk_period)
  } else if (pairs=="all") {
    d_pairs <- generate_all_pairs(data=data, risk_period=risk_period)
  } else if (pairs=="random1") {
    d_pairs <- generate_random_pairs_mem(data=data, risk_period=risk_period,
                                         n_pairs=n_pairs, batch_size=batch_size,
                                         max_iter=max_iter)
  } else if (pairs=="random2") {
    d_pairs <- generate_random_pairs(data=data, risk_period=risk_period,
                                     n_pairs=n_pairs)
  }

  return(d_pairs)
}

## check if two individuals are a valid pairing, e.g. they are not the
## same individual and their risk-periods do not overlap
# TODO: this does not work with multiple exposures!
is_valid_match <- function(.id, .id2, .time, .time2, risk_period) {
  !(.id==.id2 | abs(.time - .time2) <= risk_period)
}

## generates all possible (and valid) pairings of individuals
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table setnames
generate_all_pairs <- function(data, risk_period) {

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
  setnames(d_pairs, old=c("i..id", "i..time"), new=c(".id2", ".time2"))

  # keep only valid pairs
  d_pairs <- subset(d_pairs, is_valid_match(.id=.id, .id2=.id2, .time=.time,
                                            .time2=.time2,
                                            risk_period=risk_period))
  d_pairs[, .id_pair := seq_len(.N)]

  return(d_pairs)
}

## generates a set of n random (valid) pairs
#' @importFrom data.table .N
generate_random_pairs <- function(data, risk_period, n_pairs) {

  d_pairs <- generate_all_pairs(data=data, risk_period=risk_period)

  if (n_pairs > nrow(d_pairs)) {
    warning("Cannot generate ", format(n_pairs, scientific=FALSE),
            " pairs, because only ", nrow(d_pairs),
            " possible valid pairs exist. Took all possible valid pairs",
            " instead.", call.=FALSE)
  } else {
    d_pairs <- d_pairs[sample(x=.N, size=n_pairs)]
  }

  return(d_pairs)
}

## generate a set of n random (valid) pairs, without first generating
## all possible pairs, which is more memory efficient but slower
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table .I
generate_random_pairs_mem <- function(data, n_pairs, risk_period,
                                      batch_size=max(5000L, n_pairs * 2L),
                                      max_iter=100L) {
  . <- .id_pair <- NULL

  n <- nrow(data)

  # don't continue if it is clearly impossible
  if (n_pairs > ((n * (n-1)) / 2)) {
    stop("'n_pairs' is larger than ", ((n * (n-1)) / 2),
         " which is the maximum possible amount of matches (if all possible",
         " matches were valid). Use a smaller value for 'n_pairs' or",
         " use pairs='all' or pairs='random2' instead.", .call=FALSE)
  }

  # stores accepted pairs
  accepted <- data.table(
    i = integer(),
    j = integer()
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
      .time2 = data$.time[j]
    )]

    # vectorized validity check
    valid <- is_valid_match(
      .id = cand$.id,
      .id2 = cand$.id2,
      .time = cand$.time,
      .time2 = cand$.time2,
      risk_period = risk_period
    )

    cand <- cand[valid]

    if (nrow(cand)==0) {
      next
    }

    accepted <- rbind(
      accepted,
      cand[, .(i, j)],
      use.names = TRUE
    )
  }

  accepted <- accepted[seq_len(n_pairs)]

  # create output
  out <- data.table(
    .id = data$.id[accepted$i],
    .id2 = data$.id[accepted$j],
    .time = data$.time[accepted$i],
    .time2 = data$.time[accepted$j]
  )[seq_len(min(c(n_pairs, nrow(accepted))))]
  out[, .id_pair := .I]

  return(out)
}

## a greedy time sorting algorithm to create a dataset in which every person
## that was exposed at some point in time is in exactly a single pair
# TODO: this does not always work
#' @importFrom data.table setorder
#' @importFrom data.table setnames
#' @importFrom data.table .N
#' @importFrom data.table :=
generate_one_pairing <- function(data, risk_period) {

  .time <- .id_pair <- NULL

  # sort by time
  setorder(data, .time)

  # split into two pieces
  d_1 <- data[seq_len(nrow(data)/2)]
  d_2 <- data[seq(nrow(data)/2+1, nrow(data))]
  setnames(d_2, old=c(".id", ".time"), new=c(".id2", ".time2"))

  # put together in one pairwise data.table
  d_pairs <- cbind(d_1, d_2)

  # check if all are valid
  is_valid <- is_valid_match(.id=d_pairs$.id, .id2=d_pairs$.id2,
                             .time=d_pairs$.time, .time2=d_pairs$.time2,
                             risk_period=risk_period)

  if (!all(is_valid==TRUE)) {
    stop("Matching failed. Use random matches (pairs='random') or",
         " all matches (pairs='all') instead.", call.=FALSE)
  }

  d_pairs[, .id_pair := seq_len(.N)]

  return(d_pairs)
}
