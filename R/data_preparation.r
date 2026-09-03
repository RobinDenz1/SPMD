
## performs all data preparation tasks, including matching, to get
## the final dataset needed for further analysis
get_full_data <- function(data, start, stop, id, exposure, outcome,
                          pairs, n_pairs, risk_period, remove_noevents,
                          bounds, rand_max_iter, batch_size,
                          allow_overlap) {
  .time <- .max_t <- overlap <- .id <- NULL

  # small preparations
  data <- prepare_start_stop(data=data, start=start, stop=stop, id=id,
                             exposure=exposure, outcome=outcome,
                             remove_unexposed=TRUE,
                             remove_noevents=remove_noevents)

  # all exposure / event times
  d_exp <- get_exposure_times(data)
  d_events <- get_event_times(data)

  stopifnotm(nrow(d_exp)!=0, "There are no exposure instances in the ",
             "supplied 'data'. Estimation is thus impossible.")
  stopifnotm(nrow(d_events)!=0, "There are no events in the supplied",
             " 'data'. Estimation is thus impossible.")

  # check if there are any overlapping risk periods in one individual
  if (bounds=="[]") {
    d_exp[, overlap := (.time - shift(.time)) <= risk_period, by=.id]
  } else {
    d_exp[, overlap := (.time - shift(.time)) < risk_period, by=.id]
  }

  stopifnotm(sum(d_exp$overlap, na.rm=TRUE)==0,
             "Some individuals exhibit multiple exposure periods that overlap ",
             "with each other, which is not allowed.")
  d_exp[, overlap := NULL]

  # perform matching
  d_matches <- match_pairs(data=d_exp, pairs=pairs, risk_period=risk_period,
                           n_pairs=n_pairs, max_iter=rand_max_iter,
                           batch_size=batch_size, bounds=bounds,
                           allow_overlap=allow_overlap)

  stopifnotm(nrow(d_matches)!=0, "Could not create any valid matches.")

  d_matches <- expand_pair_matches(d_matches, risk_period=risk_period)

  # fix overlapping intervals, if needed
  if (allow_overlap) {
    d_matches <- fix_overlap(d_matches, bounds=bounds, risk_period=risk_period)
  }

  # add outcome event count to it
  d_matches <- add_event_count(dt_index=d_matches, dt_events=d_events,
                               bounds=bounds, allow_overlap=allow_overlap)

  stopifnotm(sum(d_matches$.n_events)!=0, "There were no events in the ",
             "risk_period of any of the individuals",
             " included in the matched data (control and exposure periods).",
             " Estimation is thus impossible.")

  out <- list(d_matches=d_matches, data=data, d_exp=d_exp, d_events=d_events,
              n_exposed=length(unique(d_exp$.id)), n_exposures=nrow(d_exp))

  return(out)
}

## extract exposure times for all given individuals from start-stop data
#' @importFrom data.table :=
#' @importFrom data.table setnames
#' @importFrom data.table shift
get_exposure_times <- function(data) {

  .A_shift <- .A <- .id <- NULL

  data[, .A_shift := shift(.A, type="lag", fill=0), by=.id]
  d_exp <- data[.A==TRUE & .A_shift==FALSE][, c(".id", ".start", ".max_t")]
  setnames(d_exp, old=".start", new=".time")
  data[, .A_shift := NULL]

  return(d_exp)
}

## extract event times for all given individuals from start-stop data
#' @importFrom data.table setnames
get_event_times <- function(data) {

  .Y <- NULL

  d_events <- data[.Y==TRUE][, c(".id", ".stop")]
  setnames(d_events, old=".stop", new=".time")

  return(d_events)
}

## re-code integers, factors or characters to TRUE / FALSE treatment
# NOTE: assumes that it has already been checked that treat only contains
#       two values
#' @importFrom data.table fifelse
preprocess_treat <- function(treat) {

  if (is.logical(treat)) {
    # great!
  } else if (is.numeric(treat) && all(treat %in% c(0, 1))) {
    treat <- fifelse(treat==0, FALSE, TRUE)
  } else if (is.factor(treat)) {
    treat <- fifelse(treat==levels(treat)[1], FALSE, TRUE)
  } else if (is.character(treat)) {
    treat <- fifelse(treat==sort(unique(treat))[1], FALSE, TRUE)
  } else {
    stop("The treatment variable specified by the LHS of 'formula'",
         " needs to specify a variable coded as one of:\n ",
         "(1) a logical vector, (2) an integer with only 0/1 values",
         ", (3) a binary factor or (4) a binary character variable.",
         call.=FALSE)
  }
  return(treat)
}

## small data preparations
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table :=
#' @importFrom data.table fifelse
#' @importFrom data.table copy
prepare_start_stop <- function(data, start, stop, id, exposure, outcome,
                               remove_unexposed=TRUE, remove_noevents=TRUE) {

  .A <- .Y <- .id <- .start <- .max_t <- .stop <- .exposed <-
    .has_event <- NULL

  data <- copy(as.data.table(data))

  # rename important columns
  setnames(data, old=c(start, stop, id, exposure, outcome),
           new=c(".start", ".stop", ".id", ".A", ".Y"))

  # remove missing values
  if (anyNA(data[, c(".id", ".start", ".stop", ".A", ".Y")])) {
    warning("Missing values in the 'id', time, exposure or outcome detected.",
            " Rows with such missings will be removed from further",
            " analysis.", call.=FALSE)
    data <- stats::na.omit(data, cols=c(".id", ".start", ".stop", ".A", ".Y"))
  }

  # coerce exposure / outcome to logical from whatever its input was
  data[, .A := preprocess_treat(.A)]
  data[, .Y := fifelse(.Y==0, FALSE, TRUE)]

  # sort by .id and .start
  setkey(data, .id, .start)

  # calculate maximum observation time per person
  data[, .max_t := max(.stop), by=.id]

  if (remove_unexposed) {
    data[, .exposed := sum(.A) > 0, by=.id]
    data <- data[.exposed==TRUE]
    data[, .exposed := NULL]
  }

  if (remove_noevents) {
    data[, .has_event := sum(.Y) > 0, by=.id]
    data <- data[.has_event==TRUE]
    data[, .has_event := NULL]
  }

  return(data)
}

## expand matches to full long-form quadruple matches
#' @importFrom data.table setnames
#' @importFrom data.table setkey
#' @importFrom data.table copy
#' @importFrom data.table :=
expand_pair_matches <- function(data, risk_period) {

  .A <- .time <- .time2 <- .id_pair <- .group <- .max_t <- .end_time <- NULL

  data[, .A := TRUE]

  data2 <- copy(data)
  data2[, .time := data$.time2]
  data2[, .time2 := data$.time]
  data2[, .A := FALSE]

  data <- rbind(data, data2)
  data3 <- data[, c(".id2", ".time2", ".max_t2", ".id_pair", ".A"), with=FALSE]
  setnames(data3, old=c(".id2", ".time2", ".max_t2"),
           new=c(".id", ".time", ".max_t"))
  data <- rbind(data[, -c(".id2", ".time2", ".max_t2")], data3)

  # assign all needed ids
  setkey(data, .id_pair, .time, .A)
  data[, .group := rep(c(1, 2, 3, 4), nrow(data)/4)]
  setkey(data, .id_pair, .group)

  # calculate interval end
  data[, .max_t := min(.max_t), by=c(".id_pair", ".time")]
  data[, .end_time := .time + risk_period]
  data[.end_time > .max_t, .end_time := .max_t]

  return(data)
}

## fix the interval start and endpoints for pairs with overlapping
## risk periods
#' @importFrom data.table :=
fix_overlap <- function(d_pairs, bounds, risk_period) {

  .t1 <- .t2 <- .time <- .id_pair <- .has_overlap <- .group <-
    .end_time <- .any_len_0 <- NULL

  # get exposure times
  d_pairs[, .t1 := .time[1], by=.id_pair]
  d_pairs[, .t2 := .time[3], by=.id_pair]

  # identify overlapping individuals
  if (bounds=="[]") {
    d_pairs[, .has_overlap := (.t2 - .t1) <= risk_period]
  } else {
    d_pairs[, .has_overlap := (.t2 - .t1) < risk_period]
  }

  # for individuals with overlap, fix observation period for the earlier
  # comparison to [t1, t2] and to [t1 + risk_period, t2] for the latter
  d_pairs[.group <= 2 & .has_overlap==TRUE, .end_time := .t2]
  d_pairs[.group >= 3 & .has_overlap==TRUE, .time := .t1 + risk_period]

  # if any length = 0 intervals were produced, remove the pair
  d_pairs[, .any_len_0 := any(.time >= .end_time), by=.id_pair]
  d_pairs <- subset(d_pairs, .any_len_0==FALSE)

  # clean up
  d_pairs[, .t1 := NULL]
  d_pairs[, .t2 := NULL]
  d_pairs[, .any_len_0 := NULL]

  return(d_pairs)
}

## get a data.table containing four columns of event counts for
## each matched pair
#' @importFrom data.table data.table
matches2counts <- function(data, bootstrap) {

  .group <- .n_events <- NULL

  data <- data.table(
    .id1 = data[.group==1]$.id,
    .id2 = data[.group==2]$.id,
    X1 = data[.group==1]$.n_events,
    X2 = data[.group==2]$.n_events,
    X3 = data[.group==3]$.n_events,
    X4 = data[.group==4]$.n_events
  )

  return(data)
}

## adds the actual count of events
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @importFrom data.table .I
#' @importFrom data.table .EACHI
add_event_count <- function(dt_index, dt_events, bounds, allow_overlap=FALSE) {

  row_id <- .end_time <- .time <- .id <- . <- .n_events <- .group <-
    .has_overlap <- NULL

  dt_index <- copy(dt_index)

  # unique row identifier
  dt_index[, row_id := .I]
  setkey(dt_events, .id, .time)

  # count matching events per row and attach counts
  if (allow_overlap) {

    # break into three parts to handle bounds appropriately
    dt_index1 <- subset(dt_index, .has_overlap==FALSE)
    dt_index2 <- subset(dt_index, .has_overlap & .group <= 2)
    dt_index3 <- subset(dt_index, .has_overlap & .group > 2)

    # no overlap problems, handle as usual
    out1 <- count_events(dt_events=dt_events, dt_index=dt_index1, bounds=bounds)
    dt_index1[, .n_events := out1$n_events]

    # handle overlap issues for earlier time period
    # NOTE: If [ is in front, this suggests that the exposure happens before
    #       the event, so when the second exposure time cuts the earlier
    #       risk_period short, events that happen exactly at that time should
    #       be counted as under the second exposure, not as control and
    #       vice versa
    bounds2 <- switch(
      bounds,
      "()" = "(]",
      "(]" = "()",
      "[)" = "[]",
      "[]" = "[)",
    )
    out2 <- count_events(dt_events=dt_events, dt_index=dt_index2,
                         bounds=bounds2)
    dt_index2[, .n_events := out2$n_events]

    # handle overlap issues for later time period
    # NOTE: If bounds end with ], events exactly at the end are counted towards
    #       exposure, so they cannot be also counted towards the control period
    #       that starts immediately afterwards. Similarly, if bounds end with ),
    #       the event must go somewhere -> counted in control afterwards
    bounds3 <- switch(
      bounds,
      "()" = "[)",
      "(]" = "(]",
      "[)" = "[)",
      "[]" = "(]",
    )
    out3 <- count_events(dt_events=dt_events, dt_index=dt_index3,
                         bounds=bounds3)
    dt_index3[, .n_events := out3$n_events]

    # put together
    dt_index <- rbindlist(list(dt_index1, dt_index2, dt_index3))
    dt_index[, .has_overlap := NULL]

  } else {
    out <- count_events(dt_events=dt_events, dt_index=dt_index, bounds=bounds)
    dt_index[, .n_events := out$n_events]
  }

  # rows with no matches get NA -> replace with 0
  dt_index[is.na(.n_events), .n_events := 0L]

  # cleanup
  dt_index[, c("row_id") := NULL]

  return(dt_index)
}

## given a data.table of events and a data.table of time periods,
## count the number of events in the time periods
#' @importFrom data.table :=
#' @importFrom data.table .N
count_events <- function(dt_events, dt_index, bounds) {

  . <- .id <- .time <- .end_time <- NULL

  out <- switch(
    bounds,
    "()" = dt_events[dt_index, on=.(.id, .time > .time, .time < .end_time),
                     .(n_events = .N), by=.EACHI],
    "(]" = dt_events[dt_index, on=.(.id, .time > .time, .time <= .end_time),
                     .(n_events = .N), by=.EACHI],
    "[)" = dt_events[dt_index, on=.(.id, .time >= .time, .time < .end_time),
                     .(n_events = .N), by=.EACHI],
    "[]" = dt_events[dt_index, on=.(.id, .time >= .time, .time <= .end_time),
                     .(n_events = .N), by=.EACHI],
  )
  return(out)
}

## get required information from Surv() formula
parse_surv_form <- function(formula) {

  # check that formula is actually a formula
  stopifnotm(inherits(formula, "formula"),
             "'formula' must be a formula of the form Surv(start, stop, ",
             "outcome) ~ exposure.")

  # extract formula components
  lhs <- formula[[2]]
  rhs <- formula[[3]]

  # check that the left-hand side is Surv(...)
  stopifnotm(is.call(lhs) && identical(lhs[[1]], as.name("Surv")),
    "'formula' must have a Surv() object on the left-hand side, ",
    "i.e. Surv(start, stop, outcome) ~ exposure."
  )

  # check number of arguments to Surv()
  stopifnotm(length(lhs) == 4L, "'Surv()' must contain exactly three ",
             "arguments: start, stop, and outcome.")

  # check that all Surv() arguments are simple variable names
  surv_vars <- lhs[-1]

  stopifnotm(all(vapply(surv_vars, is.name, logical(1))),
    "The start, stop, and outcome arguments of 'Surv()' ",
    "must each be a variable name."
  )

  # Check that the RHS is a single variable
  stopifnotm(is.name(rhs),
    "'exposure' must be a single variable name; ",
    "transformations, interactions, and multiple exposure variables ",
    "are not supported."
  )

  # Return variable names
  out <- list(
    start = as.character(surv_vars[[1]]),
    stop = as.character(surv_vars[[2]]),
    outcome = as.character(surv_vars[[3]]),
    exposure = as.character(rhs)
  )

  return(out)
}
