
## convenience function to create a start-stop dataset as required for
## sym_pair_matching() using only information on exposure timings,
## event times and information on the observation period
#' @importFrom data.table copy
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table setnames
#' @export
prepare_spmd_data <- function(exposures, events, obs_start, obs_end, id, time,
                              risk_period, exposure_name="A", event_name="Y",
                              convert_date=TRUE, units="days") {

  start <- .observed <- NULL

  check_inputs_prepare_data(exposures=exposures, events=events,
                            obs_start=obs_start, obs_end=obs_end, id=id,
                            time=time, risk_period=risk_period,
                            exposure_name=exposure_name, event_name=event_name,
                            convert_date=convert_date)

  # prepare exposures
  exposures <- copy(as.data.table(exposures))
  setnames(exposures, old=time, new="start")
  exposures[, stop := start + risk_period]
  exposures[, (exposure_name) := TRUE]

  # prepare events
  events <- copy(as.data.table(events))
  setnames(events, old=time, new="time")

  dlist <- list(exposures)

  # defining the start and end of the observation period
  if (is.data.frame(obs_start) && is.data.frame(obs_end)) {
    obs_start <- copy(as.data.table(obs_start))
    obs_end <- copy(as.data.table(obs_end))

    setnames(obs_start, old=time, new="start")
    setnames(obs_end, old=time, new="stop")

    obs <- merge(obs_start, obs_end, by=id, all=TRUE)
    obs[, .observed := TRUE]
  } else if (is.data.frame(obs_start) && !is.data.frame(obs_end)) {
    obs <- copy(as.data.table(obs_start))
    setnames(obs, old=time, new="start")
    obs[, stop := obs_end]
    obs[, .observed := TRUE]
  } else if (!is.data.frame(obs_start) && is.data.frame(obs_end)) {
    obs <- copy(as.data.table(obs_end))
    setnames(obs, old=time, new="stop")
    obs[, start := obs_start]
    obs[, .observed := TRUE]
  }

  # first_time and last_time arguments are only needed when no individual-
  # specific information is supplied
  if (is.data.frame(obs_start) || is.data.frame(obs_end)) {
    dlist[[length(dlist) + 1]] <- obs
    first_time <- last_time <- NULL
  } else {
    first_time <- obs_start
    last_time <- obs_end
  }

  # when outside risk period, exposure indicator should be FALSE
  defaults <- list(A = FALSE)
  names(defaults) <- exposure_name

  # merge together
  out <- merge_start_stop(
    dlist = dlist,
    by = id,
    all.x = TRUE,
    all.y = FALSE,
    first_time = first_time,
    last_time = last_time,
    event_times = events,
    defaults = defaults
  )
  setnames(out, old="status", new=event_name)

  if (".observed" %in% colnames(out)) {
    out[, .observed := NULL]
  }

  if (convert_date && is_date(out$start)) {
    min_time <- min(out$start)
    out[, start := as.vector(difftime(start, min_time, units=units))]
    out[, stop := as.vector(difftime(stop, min_time, units=units))]
  }

  return(out)
}

## check inputs for the prepare_data() function
#' @importFrom data.table uniqueN
check_inputs_prepare_data <- function(exposures, events, obs_start, obs_end,
                                      id, time, risk_period, exposure_name,
                                      event_name, convert_date) {

  stopifnotm(length(id)==1 && is.character(id),
             "'id' must be a single character string.")
  stopifnotm(length(time)==1 && is.character(time),
             "'time' must be a single character string.")
  stopifnotm(length(exposure_name)==1 && is.character(exposure_name),
             "'exposure_name' must be a single character string.")
  stopifnotm(length(event_name)==1 && is.character(event_name),
             "'event_name' must be a single character string.")
  stopifnotm(length(unique(c(id, event_name, exposure_name)))==3,
             "'id', 'exposure_name' and 'event_name' must be distinct.")
  stopifnotm(is.data.frame(obs_start) ||
             (length(obs_start)==1 &&
                (is.numeric(obs_start) | is_date(obs_start))),
             "'obs_start' must either be a single number / Date or a ",
             "data.frame containing one entry for each 'id' in 'exposures'.")
  stopifnotm(is.data.frame(obs_end) ||
               (length(obs_end)==1 && (is.numeric(obs_end) | is_date(obs_end))),
             "'obs_end' must either be a single number / Date or a data.frame",
             "containing one entry for each 'id' in 'exposures'.")
  stopifnotm(is.data.frame(exposures) && nrow(exposures),
             "'exposures' must be a data.frame with at least 1 row.")
  stopifnotm(is.data.frame(events) && nrow(events),
             "'events' must be a data.frame with at least 1 row.")
  stopifnotm(id %in% colnames(exposures),
             "'id' must be the name of a column in 'exposures'.")
  stopifnotm(id %in% colnames(events),
             "'id' must be the name of a column in 'events'.")
  stopifnotm(time %in% colnames(exposures),
             "'time' must be the name of a column in 'exposures'.")
  stopifnotm(time %in% colnames(events),
             "'time' must be the name of a column in 'events'.")
  stopifnotm(length(risk_period)==1 && is.numeric(risk_period) &&
             risk_period > 0,
             "'risk_period' must be a single number > 0.")
  stopifnotm(length(convert_date)==1 && is.logical(convert_date),
             "'convert_date' must be either TRUE or FALSE.")

  if (is.data.frame(obs_start)) {
    stopifnotm(id %in% colnames(obs_start),
               "'id' must be the name of a column in 'obs_start'.")
    stopifnotm(time %in% colnames(obs_start),
               "'time' must be the name of a column in 'obs_start'.")
    stopifnotm(all(obs_start[[id]] %in% exposures[[id]]) &&
               nrow(obs_start) == uniqueN(exposures[[id]]),
               "If 'obs_start' is supplied as a data.frame, there should",
               "be exactly one entry for every 'id' in 'exposures'.")
  }

  if (is.data.frame(obs_end)) {
    stopifnotm(id %in% colnames(obs_end),
               "'id' must be the name of a column in 'obs_end'.")
    stopifnotm(time %in% colnames(obs_end),
               "'time' must be the name of a column in 'obs_start'.")
    stopifnotm(all(obs_end[[id]] %in% exposures[[id]]) &&
               nrow(obs_end) == uniqueN(exposures[[id]]),
               "If 'obs_end' is supplied as a data.frame, there should",
               "be exactly one entry for every 'id' in 'exposures'.")
  }
}
