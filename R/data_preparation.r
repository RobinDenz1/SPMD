
## performs all data preparation tasks, including matching, to get
## the final dataset needed for further analysis
get_full_data <- function(data, start, stop, id, exposure, outcome,
                          pairs, n_pairs, risk_period, remove_noevents,
                          include_exp_time) {

  .time <- .max_t <- NULL

  # small preparations
  data <- prepare_start_stop(data=data, start=start, stop=stop, id=id,
                             exposure=exposure, outcome=outcome,
                             remove_unexposed=TRUE,
                             remove_noevents=remove_noevents)

  # all exposure / event times
  d_exp <- get_exposure_times(data)
  d_events <- get_event_times(data)

  # remove those were we cannot observe the entire risk period
  n_exposed <- length(unique(d_exp$.id))
  d_exp <- subset(d_exp, .time <= .max_t - risk_period)[, c(".id", ".time")]

  # perform matching
  d_matches <- match_pairs(data=d_exp, pairs=pairs, risk_period=risk_period,
                           n_pairs=n_pairs)
  d_matches <- expand_pair_matches(d_matches)

  # add outcome event count to it
  d_matches <- add_event_count(dt_index=d_matches, dt_events=d_events,
                               risk_period=risk_period,
                               include_exp_time=include_exp_time)

  out <- list(d_matches=d_matches, data=data, d_exp=d_exp, d_events=d_events,
              n_exposed=n_exposed)

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
prepare_start_stop <- function(data, start, stop, id, exposure, outcome,
                               remove_unexposed=TRUE, remove_noevents=TRUE) {

  .A <- .Y <- .id <- .start <- .max_t <- .stop <- .exposed <-
    .has_event <- NULL

  data <- as.data.table(data)

  # rename important columns
  setnames(data, old=c(start, stop, id, exposure, outcome),
           new=c(".start", ".stop", ".id", ".A", ".Y"))

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
expand_pair_matches <- function(data) {

  .A <- .time <- .time2 <- .id_pair <- .group <- NULL

  data[, .A := TRUE]

  data2 <- copy(data)
  data2[, .time := data$.time2]
  data2[, .time2 := data$.time]
  data2[, .A := FALSE]

  data <- rbind(data, data2)
  data3 <- data[, c(".id2", ".time2", ".id_pair", ".A"), with=FALSE]
  setnames(data3, old=c(".id2", ".time2"), new=c(".id", ".time"))
  data <- rbind(data[, -c(".id2", ".time2")], data3)

  # assign all needed ids
  setkey(data, .id_pair, .time, .A)
  data[, .group := rep(c(1,2,3,4), nrow(data)/4)]
  setkey(data, .id_pair, .group)

  return(data)
}

## get a data.table containing four columns of event counts for
## each matched pair
#' @importFrom data.table data.table
matches2counts <- function(data) {

  data <- data.table(
    .id_pair = data$.id_pair,
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
add_event_count <- function(dt_index, dt_events, risk_period,
                            include_exp_time=FALSE) {

  row_id <- end_time <- .time <- .id <- . <- .n_events <- NULL

  dt_index <- copy(dt_index)

  # unique row identifier
  dt_index[, row_id := .I]

  # interval end
  dt_index[, end_time := .time + risk_period]

  # key events table
  setkey(dt_events, .id, .time)

  # count matching events per row
  if (include_exp_time) {
    out <- dt_events[
      dt_index,
      on = .(
        .id,
        .time >= .time,
        .time <= end_time
      ),
      .(n_events = .N),
      by = .EACHI
    ]
  } else {
    out <- dt_events[
      dt_index,
      on = .(
        .id,
        .time > .time,
        .time <= end_time
      ),
      .(n_events = .N),
      by = .EACHI
    ]
  }

  # attach counts
  dt_index[, .n_events := out$n_events]

  # rows with no matches get NA -> replace with 0
  dt_index[is.na(.n_events), .n_events := 0L]

  # cleanup
  dt_index[, c("row_id", "end_time") := NULL]

  return(dt_index)
}

## get required information from Surv() formula
parse_surv_form <- function(formula) {

  vars <- all.vars(formula)

  out <- list(start=vars[1], stop=vars[2], outcome=vars[3],
              exposure=vars[4])
  return(out)
}
