\name{summary.SPMD}
\alias{summary.SPMD}

\title{
Summarise the Results of a \code{SPMD} object
}
\description{
This is the summary S3 method for \code{SPMD} objects. It shows the final estimate, as well as some valuable numbers regarding the flow of the data processing.
}
\usage{
\method{summary}{SPMD}(object, ...)
}
\arguments{
  \item{object}{
An \code{SPMD} object created using the \code{\link{sym_pair_matching}} function.
  }
  \item{...}{
Currently not used.
  }
}
\details{
Since symmetric pair matching by design cannot use all data, it is important to understand what data is actually being used. First, all individuals who were never exposed are discarded, since they can never be used. Secondly, all exposures that occur \code{risk_period} time units before the individuals observation end (right-censoring) can also not be used, since then we would be unable to observe the full event count. Furthermore, when using \code{estimator="moments"}, individuals that never experienced an event cannot meaningfully contribute to the estimation, since event counts are multiplied across individuals. Additionally, only individuals who can be matched to someone else can be included, although this is usually not a problem in practice, since we do not match on any covariates directly.

Finally, when an exposure instance of an individual is included in the matched data, only the \code{risk_period} time units after exposure and at the control time directly enter the calculations, the rest of the individuals observation time remains unused. When using \code{pairs="one"}, this means that very little of the actual observation time of included units is used. To check how much is used, the percentage of the total time used amongst the ever included indiviiduals is included in the output. It may be used to get a rough idea how to specify \code{n_pairs} when using \code{pairs="random"}.
}
\author{
Robin Denz
}
\value{
Returns \code{NULL}.
}
\seealso{
\code{\link{sym_pair_matching}}
}
\references{
TODO
}
\examples{
set.seed(1234)

# simulate data where the exposure increases the event probability
data <- sim_example_data(n=100, rr=2.5)
head(data)

out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="all", estimator="moments")
summary(out)
}
