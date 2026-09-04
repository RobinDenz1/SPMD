\name{prepare_spmd_data}
\alias{prepare_spmd_data}

\title{
Prepare data for \code{sym_pair_matching()}
}
\description{
This function may be used to transform raw information about exposure, event and observation times into the start-stop format required to use the \code{\link{sym_pair_matching}} function.
}
\usage{
prepare_spmd_data(exposures, events, obs_start, obs_end,
                  id, time, risk_period, exposure_name="A",
                  event_name="Y", convert_date=TRUE,
                  units="days")
}
\arguments{
  \item{exposures}{
A \code{data.frame} like object with two columns: \code{id} (the unique person identifier) and \code{time} (the exposure times of an individual). This object defines when the exposure occured for each individual. If an individual experienced multiple exposure times, one row per exposure time should be included (e.g. the dataset should be in the long-format). The output dataset will only contain individuals that appear in this dataset, because individuals without an exposure are ignored in \code{\link{sym_pair_matching}}.
  }
  \item{events}{
A \code{data.frame} like object with two columns: \code{id} (the unique person identifier) and \code{time} (the event times of an individual). This object defines when the event occured for each individual. Similar to \code{exposures}, multiple events per person can be included by using multiple rows per person.
  }
  \item{obs_start}{
Either a single number or Date specifying the start of the observation period (usually 0), or a \code{data.frame} like object structured similarly as the \code{exposures} and \code{events} arguments. Again, it should have two columns: \code{id} (the unique person identifier) and \code{time} (the start of the observation time for the individual). Contraty to \code{exposures} and \code{events} it needs to have exactly one entry for each \code{id} in \code{exposures}, otherwise an error will be produced.
  }
  \item{obs_end}{
Same as \code{obs_start}, but for the end of the observation period.
  }
  \item{id}{
A single character string specifying the name of the unique person identifier in \code{exposures}, \code{events} and possible \code{obs_start} and \code{obs_end}.
  }
  \item{time}{
A single character string specifying the name of the time related columns in \code{exposures}, \code{events} and possible \code{obs_start} and \code{obs_end}.
  }
  \item{risk_period}{
A single positive number specifying the length of the risk period, essentially equal to the argument of the same name in \code{\link{sym_pair_matching}}.
  }
  \item{exposure_name}{
A single character string specifying how the exposure indicator in the output data should be named.
  }
  \item{event_name}{
A single character string specifying how the event indicator in the output data should be named.
  }
  \item{convert_date}{
Either \code{TRUE} (default) or \code{FALSE}, specifying whether \code{Date} like objects should be converted to numeric values. If \code{TRUE}, the smallest datetime in \code{start} is used as time 0. All other values in both \code{start} and \code{stop} then resemble the time elapsed since this point in time (in \code{units}). We recommend keeping this at \code{TRUE}, because \code{\link{sym_pair_matching}} requires numeric time values. Ignore if the supplied time values are already numeric.
  }
  \item{units}{
A single character string passed to the argument of the same name in \code{\link{difftime}}. It defines the units of time since the artificial time zero. Only used if \code{convert_date=TRUE} and if the supplied time values are \code{Date} like objects, ignored otherwise.
  }
}
\details{
The dataset produced by this function is in the exact format needed for the \code{\link{sym_pair_matching}} function. Exposure times are exactly at \code{start}, event times exactly at \code{stop}. Internally, it uses the \code{merge_start_stop()} function from the \pkg{MatchTime} package to perform the data wrangling.
}
\author{
Robin Denz
}
\value{
Returns a \code{data.table} containing five columns: \code{.id} (the unique person identifier), \code{start} and \code{stop} (defining some durations of individual-specific time in which no variables changed), \code{exposure_name} (a time-dependent logical exposure variable) and \code{event_name} (a recurrent logical event).
}
\seealso{
\code{\link{sym_pair_matching}}
}
\references{
Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld (2026). "The Symmetric Pair Matching Design: A Self-Controlled Method with Automatic Adjustment for Time Effects". arXiv Preprint. doi: 10.48550/arXiv.2608.25979.
}
\examples{
library(data.table)
library(SPMD)

# some exposure timings
exposures <- data.table(.id=c(1, 1, 2, 3, 4),
                        .time=c(14, 299, 234, 33, 903))

# some event timings
events <- data.table(.id=c(1, 3, 3, 4),
                     .time=c(18, 305, 481, 210))

# creating the full start-stop data over a time-period
# of 1000 days from t = 0
data <- prepare_spmd_data(
  exposures = exposures,
  events = events,
  id = ".id",
  time = ".time",
  obs_start = 0,
  obs_end = 1000,
  risk_period = 30
)
head(data)

# some individual-specific right-censoring times
# NOTE: all individuals must be included here!
cens_times <- data.table(.id=c(1, 2, 3, 4),
                         .time=c(400, 510, 1000, 1000))

# creating the full start-stop data over a time-period
# of 1000 days from t = 0, respecting the censoring
data <- prepare_spmd_data(
  exposures = exposures,
  events = events,
  id = ".id",
  time = ".time",
  obs_start = 0,
  obs_end = cens_times,
  risk_period = 30
)
head(data)
}
