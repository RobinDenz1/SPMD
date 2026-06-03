\name{plot.SPMD}
\alias{plot.SPMD}

\title{
Plot a \code{SPMD} object
}
\description{
This function visualizes the time used for the estimation in a symmetric pair matching design. It does so by plotting one row per person, where the time used as control and the time under exposure is colored differently. Time that was not used is white. Individuals are sorted by their first exposure.
}
\usage{
\method{plot}{SPMD}(x, show_events=FALSE, fill_control="steelblue",
     fill_exposed="red", event_size=1, event_shape=8,
     event_color="black", xlab="Time", ...)
}
\arguments{
  \item{x}{
A \code{SPMD} object created using the \code{\link{sym_pair_matching}} function.
  }
  \item{show_events}{
Either \code{TRUE} or \code{FALSE}, indicating whether a dot for each observed event should be added to the plot.
  }
  \item{fill_control}{
A single character string, specifying the color that should be used for times used as control.
  }
  \item{fill_exposed}{
A single character string, specifying the color that should be used for exposure times.
  }
  \item{event_size}{
A single number, specifying the size of the event indicators when using \code{show_events=TRUE}.
  }
  \item{event_shape}{
A single number, specifying the shape of the event indicators when using \code{show_events=TRUE}.
  }
  \item{event_color}{
A single character string, specifying the color of the event indicators when using \code{show_events=TRUE}.
  }
  \item{xlab}{
A single character string, giving the x-axis title.
  }
  \item{...}{
Currently not used.
  }
}
\details{
This function may be used to visually investigate exposure trends, and control time distributions. It may be particularly valuable when using \code{pairs="random1"} or \code{pairs="random2"}.
}
\author{
Robin Denz
}
\value{
Returns a standard \code{ggplot2} object.
}
\seealso{
\code{\link{sym_pair_matching}}
}
\examples{
library(ggplot2)
library(data.table)
library(SPMD)

set.seed(1234)
data <- sim_example_data(n=500, rr=3)

# using each person in one match
out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="one", estimator="moments")
plot(out)

# using each person in multiple match
out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="random2", estimator="moments",
                         n_pairs=2000)
plot(out)

# using all possible and valid match
out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="all", estimator="moments")
plot(out)
}
