
## estimate either standard or spline-based SCCS
estimate_sccs <- function(data, spline=FALSE, cuts=NULL, df) {

  data <- copy(data)

  # keep only individuals with at least one event
  data[, any_Y := any(Y), by=.id]
  data <- subset(data, any_Y == TRUE)

  # extract event times
  d_events <- times_from_start_stop(data, id=".id", name="Y", type="event",
                                    time_name=".time")

  # new start-stop data, ignoring everything but A
  d_sccs <- simplify_start_stop(data, id=".id", cols="A")

  # add time-categories if spline based method is used
  if (spline) {

    # create dataset of cut-points
    d_cuts <- data.table(start=cuts, stop=shift(cuts, -1))
    d_cuts <- subset(d_cuts, !is.na(stop))
    d_cuts[, k := seq_len(.N)]

    # fill with .ids
    ids <- unique(d_sccs$.id)
    d_cuts <- d_cuts[rep(seq_len(length(cuts)-1), each=length(ids))]
    d_cuts[, .id := rep(ids, times=length(cuts)-1)]
    setkey(d_cuts, .id, start)

    d_sccs <- merge_start_stop(d_sccs, d_cuts, by=".id")
  }

  # re-add outcome indicator
  d_sccs[, outcome := FALSE]
  d_sccs[d_events,
         on = ".id",
         outcome := outcome | (start < .time & .time <= stop)]

  # add interval length
  d_sccs[, interval := (stop - start)]

  # fit conditional Poisson model
  if (!spline) {
    sccs_mod <- survival::clogit(outcome ~ A + strata(.id) +
                                   offset(log(interval)), data=d_sccs)
  } else {
    d_sccs[, time_point := (start + stop) / 2]
    sccs_mod <- survival::clogit(outcome ~ A + strata(.id) +
                              offset(log(interval)) + ns(time_point, df=df),
                                 data=d_sccs)
  }

  return(as.vector(exp(coef(sccs_mod)))[1])
}

## function to get data in the format needed to apply the CTC design
get_ctc_data <- function(data, risk_period) {

  data <- copy(data)

  # whether individual was ever exposed / ever had an event
  data[, any_A := any(A), by=.id]
  data[, any_Y := any(Y), by=.id]

  # identify cases, controls and exposure times for both
  d_cases <- subset(data, any_A==TRUE & any_Y==TRUE)
  d_controls <- subset(data, any_A==TRUE & any_Y==FALSE)
  d_exposures <- times_from_start_stop(data, id=".id", name="A", type="var")

  # first event time
  d_cases[, min_Y := min(stop[Y==TRUE]), by=.id]

  # keep only those where both periods can be observed
  d_cases <- subset(d_cases, min_Y >= (2 * risk_period))

  # keep only the event time for cases
  d_cases <- d_cases[stop == min_Y][, c(".id", "stop")]
  setnames(d_cases, old="stop", new=".Y_time")
  d_cases[, group := "1"]

  # sample controls randomly
  ids_control <- sample(unique(d_controls$.id), size=nrow(d_cases),
                        replace=FALSE)

  # assign them the same outcome times
  d_controls <- data.table(.id=ids_control, .Y_time=d_cases$.Y_time,
                           group="0")

  # put together
  d_out <- rbind(d_cases, d_controls)

  # add exposure times
  d_out <- merge(d_out, d_exposures, by=".id", all.x=TRUE, all.y=FALSE)
  d_out[, diff := .Y_time - time]

  # check if exposure in each period
  d_period1 <- d_out[, .(exposure = any(diff >= 0 & diff <= (risk_period - 1)),
                         group = group[1],
                         .Y_time = .Y_time[1]),
                     by=.id]
  d_period1[, period := 1]

  d_period0 <- d_out[, .(exposure = any(diff >= risk_period
                                        & diff <= ((2 * risk_period) - 1)),
                         group = group[1],
                         .Y_time = .Y_time[1]),
                     by=.id]
  d_period0[, period := 0]

  d_out <- rbind(d_period0, d_period1)
  d_out[, exposure := as.numeric(exposure)]
  d_out[is.na(exposure), exposure := 0]
  setkey(d_out, .id, period, group)

  return(d_out)
}

## apply conditional log. reg. to transformed data and extract RR
get_ctc_est <- function(data) {
  model <- clogit(period ~ exposure * group + strata(.id), data=data)
  return(as.vector(exp(coef(model)[3])))
}

## apply CTC design
estimate_ctc <- function(data, risk_period) {
  d_ctc <- get_ctc_data(data=data, risk_period=risk_period)
  out <- get_ctc_est(d_ctc)
  return(out)
}

## function to get data in the format needed to apply the CCO design
get_cco_data <- function(data, risk_period) {

  data <- copy(data)

  # whether individual was ever exposed / ever had an event
  data[, any_A := any(A), by=.id]
  data[, any_Y := any(Y), by=.id]

  # identify cases and exposure times
  d_cases <- subset(data, any_A==TRUE & any_Y==TRUE)
  d_exposures <- times_from_start_stop(data, id=".id", name="A", type="var")

  # first event time
  d_cases[, min_Y := min(stop[Y==TRUE]), by=.id]

  # keep only those where both periods can be observed
  d_cases <- subset(d_cases, min_Y >= (2 * risk_period))

  # keep only the event time for cases
  d_cases <- d_cases[stop == min_Y][, c(".id", "stop")]
  setnames(d_cases, old="stop", new=".Y_time")

  # add exposure times
  d_cases <- merge(d_cases, d_exposures, by=".id", all.x=TRUE, all.y=FALSE)
  d_cases[, diff := .Y_time - time]

  # check if exposure in each period
  d_period1 <- d_cases[, .(exposure = any(diff >= 0 & diff <= (risk_period - 1)),
                           .Y_time = .Y_time[1]), by=.id]
  d_period1[, period := 1]

  d_period0 <- d_cases[, .(exposure = any(diff >= risk_period
                                          & diff <= ((2 * risk_period) - 1)),
                           .Y_time = .Y_time[1]), by=.id]
  d_period0[, period := 0]

  d_out <- rbind(d_period0, d_period1)
  d_out[, exposure := as.numeric(exposure)]
  d_out[is.na(exposure), exposure := 0]
  setkey(d_out, .id, period)

  # keep only those were at least one period has an exposure
  # (only dis-concordant pairs matter)
  d_out[, n_exp := sum(exposure), by=.id]
  d_out <- subset(d_out, n_exp > 0)
  d_out[, n_exp := NULL]

  return(d_out)
}

# function to fit the model
get_cco_est <- function(data) {
  model <- clogit(period ~ exposure + strata(.id), data = data)
  return(as.vector(exp(coef(model))))
}

estimate_cco <- function(data, risk_period) {
  d_cco <- get_cco_data(data=data, risk_period=risk_period)
  out <- get_cco_est(d_cco)
  return(out)
}
