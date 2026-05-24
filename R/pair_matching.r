
## create valid pairs from given data
match_pairs <- function(data, pairs, risk_period, n_pairs=NULL) {

  if (pairs=="one") {
    d_pairs <- generate_one_pairing(data=data, risk_period=risk_period)
  } else if (pairs=="all") {
    d_pairs <- generate_all_pairs(data=data, risk_period=risk_period)
  } else if (pairs=="random") {
    d_pairs <- generate_random_pairs(data=data, risk_period=risk_period,
                                     n_pairs=n_pairs)
  } else {
    stop("Argument 'pairs' must be either 'one', 'all' or 'random', not ",
         toString(pairs), ".", call.=FALSE)
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
