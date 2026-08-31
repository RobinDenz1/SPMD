
## function to transform multiple data.tables into a single start-stop
## data.table. Copied as-is from the current version of the MatchTime package.
## Should be removed from this package once MatchTime is on CRAN
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table merge.data.table
#' @importFrom data.table melt.data.table
#' @importFrom data.table setnames
#' @importFrom data.table dcast
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table shift
#' @importFrom data.table uniqueN
#' @importFrom data.table is.data.table
merge_start_stop <- function(x, y, ..., dlist, by, start="start",
                             stop="stop", all=FALSE, all.x=all, all.y=all,
                             first_time=NULL, last_time=NULL,
                             remove_before_first=TRUE, remove_after_last=TRUE,
                             center_on_first=FALSE, units="auto", defaults=NULL,
                             event_times=NULL, time_to_first_event=FALSE,
                             status="status", constant_vars=NULL) {

  . <- .id <- .first_time <- .event_time <- .last_per_id <- .dataset <- NULL

  if (missing(dlist) & missing(y)) {
    dlist <- list(x)
  } else if (missing(dlist)) {
    dlist <- list(x, y, ...)
  }
  dlist <- copy(dlist)

  ## prepare dlist for further processing
  col_types <- list()
  for (i in seq_len(length(dlist))) {

    # must be a data.table
    if (!is.data.table(dlist[[i]])) {
      dlist[[i]] <- as.data.table(dlist[[i]])
    }

    # columns containing the values
    cnames <- colnames(dlist[[i]])[!colnames(dlist[[i]]) %in%
                                     c(by, start, stop)]

    # extract column types
    type_value_i <- extract_col_types(dlist[[i]], cnames)
    col_types <- append(col_types, type_value_i)

    if (!all(unlist(type_value_i) %in% c("logical", "numeric", "character",
                                         "integer"))) {
      stop("All columns containing variables (columns except 'by', 'start' ",
           "and 'stop') must be of type:\n 'logical', 'numeric', 'integer' or ",
           "'character'.", call.=FALSE)
    }

    # get supplied data.tables into long format if it contains more than one
    # variable with actual values
    if (ncol(dlist[[i]])==4) {
      dlist[[i]][, .dataset := cnames]
      setnames(dlist[[i]], old=cnames, new="value")
    } else {
      dlist[[i]] <- suppressWarnings(
        melt.data.table(dlist[[i]], id.vars=c(by, start, stop),
                        variable.name=".dataset",
                        variable.factor=FALSE)
      )
    }

    # check if all values in a dataset are unique
    if (uniqueN(dlist[[i]]) != nrow(dlist[[i]])) {
      stop("Duplicate rows found in dataset ", i, ". Remove duplicates",
           " and re-run the function.", call.=FALSE)
    }
  }

  # put together all datasets in one
  value_dat <- rbindlist(dlist, use.names=TRUE)

  # initial data.table
  data <- data.table(.id=rep(value_dat[[by]], 2),
                     start=c(value_dat[[start]], value_dat[[stop]]))

  # apply all.x and all.y restrictions
  if (length(dlist) > 1) {
    if (!all.x | !all.y) {
      ids_x_y <- get_unique_x_y_ids(dlist=dlist, id=by)
    } else {
      ids_x_y <- NULL
    }

    if (!all.x && length(ids_x_y$only_in_x) > 0) {
      data <- data[!.id %in% ids_x_y$only_in_x]
    }

    if (!all.y && length(ids_x_y$only_in_y) > 0) {
      data <- data[!.id %in% ids_x_y$only_in_y]
    }
  }
  unique_ids <- unique(data$.id)

  # add event times, if specified
  if (!is.null(event_times)) {
    event_times <- copy(event_times)
    setnames(event_times, old=by, new=".id")
    setnames(event_times, old="time", new="start")
    data <- rbind(data, event_times)
    setnames(event_times, old="start", new=".event_time")
  }

  # add first time
  if (!is.null(first_time)) {
    start_rows <- data.table(.id=unique_ids, start=first_time)
    data <- rbind(data, start_rows)
  }

  # add last time
  if (!is.null(last_time)) {
    end_rows <- data.table(.id=unique_ids, start=last_time)
    data <- rbind(data, end_rows)
  }

  # sort by .id and start
  setkey(data, .id, start)

  # create stop
  set_shift_by(data, col_in="start", col_out="stop", type="lead",
               by=".id", fill=NA)

  data <- unique(data)
  data <- data[!is.na(stop) & start!=stop]

  # create column names for later
  var_names <- unique(value_dat$.dataset)
  var_names_stop <- paste0(stop, "_", var_names)
  var_names_value <- paste0("value_", var_names)

  # create one end date for each start + corresponding value
  formula <- stats::as.formula(paste0(by, " + ", start, " ~ .dataset"))
  value_dat <- dcast(value_dat, formula=formula, value.var=c(stop, "value"),
                     drop=TRUE)
  setnames(value_dat, old=c(by, start), new=c(".id", "start"))

  # add this info to start-stop intervals
  data <- merge.data.table(data, value_dat, by=c(".id", "start"),
                           all.x=TRUE, all.y=FALSE)
  rm(value_dat)

  # fill up variable info where appropriate
  for (i in seq_len(length(var_names_stop))) {
    name_stop <- var_names_stop[i]
    name_value <- var_names_value[i]

    # apply LOCF to both stop and value
    set_na_locf_by_id(data=data, name=name_stop)
    set_na_locf_by_id(data=data, name=name_value)

    # for data.table version < 1.14.9, need explicit coercion of
    # general NA to specific type of NA
    if (inherits(data[[name_value]], "character")) {
      def_NA <- NA_character_
    } else  if (inherits(data[[name_value]], "integer")) {
      def_NA <- NA_integer_
    } else if (inherits(data[[name_value]], "numeric")) {
      def_NA <- NA_real_
    } else {
      def_NA <- NA
    }

    # update value accordingly
    data[, (name_value) := fifelse(!is.na(get(name_stop)) &
                                     start < get(name_stop),
                                   get(name_value), def_NA, na=def_NA)]
    data[, (name_stop) := NULL]
  }
  setnames(data, old=var_names_value, new=var_names)

  # remove periods before start
  if (remove_before_first & !is.null(first_time)) {
    data <- data[start >= first_time, ]
  }

  # remove periods after stop
  if (remove_after_last & !is.null(last_time)) {
    data <- data[start < last_time, ]
  }

  # parse columns back to original class
  set_col_classes(data=data, col_types=col_types)

  # apply defaults, if specified
  if (!is.null(defaults)) {
    for (i in seq_len(length(defaults))) {
      name_i <- names(defaults)[i]
      val_i <- defaults[i]
      data[is.na(get(name_i)), (name_i) := eval(val_i)]
    }
  }

  # add event indicator, if event times were supplied
  if (!is.null(event_times)) {

    if (time_to_first_event) {
      event_times <- event_times[, .(.event_time = min(.event_time)), by=.id]
    }

    data <- merge.data.table(data, event_times, by=".id", all.x=TRUE,
                             allow.cartesian=TRUE)
    data[, (status) := any(stop==.event_time), by=c(".id", "start")]
    data[is.na(eval(status)), (status) := FALSE]

    if (time_to_first_event) {
      data <- data[stop <= .event_time | is.na(.event_time)]
      data[, .event_time := NULL]
    } else {
      data[, .event_time := NULL]
      data <- unique(data)
    }
  }

  # center output on first time, if specified
  if (center_on_first) {

    if (is.null(first_time)) {
      data[, first_time := min(start), by=.id]
    }

    if (inherits(data$start, c("Date", "POSIXt", "POSIXct", "POSIXlt"))) {
      data[, start := as.numeric(difftime(start, first_time, units=units))]
      data[, stop := as.numeric(difftime(stop, first_time, units=units))]
    } else {
      data[, start := start - first_time]
      data[, stop := stop - first_time]
    }

    if (is.null(first_time)) {
      data[, first_time := NULL]
    }
  }

  # add constant variables, if specified
  if (!is.null(constant_vars)) {
    setnames(constant_vars, by, ".id")
    data <- merge.data.table(data, constant_vars, by=".id", all.x=TRUE,
                             all.y=FALSE)
  }

  # set names back to user-supplied names
  setnames(data, old=c(".id", "start", "stop"), new=c(by, start, stop))

  return(data)
}

## applies na_locf() by group in a data.table and changes the column in place
## NOTE: this seems weird because we could just use one na_locf() call per .id
##       but it is quite a bit faster this way, because it removes the overhead
##       of calling the na_locf() function over and over again
#' @importFrom data.table :=
#' @importFrom data.table .N
set_na_locf_by_id <- function(data, name) {

  .id <- .leading_na <- NULL

  # identify first non-NA by .id
  data[!is.na(get(name)), .leading_na := seq_len(.N)==1, by=.id]
  data[is.na(.leading_na), .leading_na := FALSE]

  # cumsum() by .id, making everything == 0 a leading NA value
  data[, .leading_na := cumsum(.leading_na), by=.id]

  # call na_locf() a single time
  data[, (name) := na_locf(get(name))]

  # set leading NAs that were wrongly filled up back to NA
  data[.leading_na==0, (name) := NA]
  data[, .leading_na := NULL]
}

## last observation carried forward
na_locf <- function(x) {
  v <- !is.na(x)
  return(c(NA, x[v])[cumsum(v) + 1])
}

## given a data.table and a named list of column classes, parse
## the columns back to that class
set_col_classes <- function(data, col_types) {

  for (i in seq_len(length(col_types))) {
    name_i <- names(col_types)[[i]]
    type_i <- col_types[[i]]

    if (type_i=="logical") {
      if (all(unique(data[[name_i]]) %in% c("0", "1", NA))) {
        data[, (name_i) := as.logical(as.numeric(get(name_i)))]
      } else {
        data[, (name_i) := as.logical(get(name_i))]
      }
    } else if (type_i=="numeric") {
      data[, (name_i) := as.numeric(get(name_i))]
    } else if (type_i=="character") {
      data[, (name_i) := as.character(get(name_i))]
    } else if (type_i=="integer") {
      data[, (name_i) := as.integer(get(name_i))]
    }
  }
}

## same as a shift() call per id, but much faster for large data
## especially if that data has many different values in "by"
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table shift
set_shift_by <- function(data, col_in, col_out, type, by, fill=NA) {

  .incorrect <- NULL

  data[, (col_out) := shift(get(col_in), n=1, type=type)]

  if (type=="lag") {
    data[, .incorrect := seq_len(.N)==1L, by=eval(by)]
  } else if (type=="lead") {
    data[, .incorrect := seq_len(.N)==.N, by=eval(by)]
  }

  data[.incorrect==TRUE, (col_out) := fill]
  data[, .incorrect := NULL]
}

## given a data.table and a list of column names, get a named list
## of the corresponding column types
extract_col_types <- function(data, cnames) {

  out <- vector(mode="list", length=length(cnames))
  for (i in seq_len(length(cnames))) {
    out[[i]] <- class(data[[cnames[i]]])
  }
  names(out) <- cnames
  return(out)
}

## extract ids that are only present in x, y respectively
get_unique_x_y_ids <- function(dlist, id) {

  all_x_ids <- unique(dlist[[1]][[id]])

  all_y_ids <- vector(mode="list", length=length(dlist)-1)
  for (i in seq(2, length(dlist))) {
    all_y_ids[[i-1]] <- dlist[[i]][[id]]
  }
  all_y_ids <- unique(unlist(all_y_ids))

  out <- list(only_in_x=setdiff(all_x_ids, all_y_ids),
              only_in_y=setdiff(all_y_ids, all_x_ids))
  return(out)
}
