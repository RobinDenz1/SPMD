
## given a sorted vector of unique times and a duration, calculates the
## amount of unique observation time used
calculate_total_time <- function(times, risk_duration) {
  out <- risk_duration + sum(pmin(diff(times), risk_duration))
  return(out)
}

## calculates the amount of observation time that was actually used
## for each individual
get_times_used <- function(d_matches, data, risk_period) {

  # unique start times per person
  d_times <- unique(out$d_matches, by=c(".id", ".time"))
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
