\name{sim_example_data}
\alias{sim_example_data}

\title{
Simulate Some Simple Example Data
}
\description{
This function is mostly used to obtain example data to illustrate the features of the \pkg{SPMD} package. It simulates a dataset with three main variables: \code{X} (a continuous time-fixed confounder), \code{A} (a binary time-dependent exposure variable) and \code{Y} (a recurrent event outcome). The data is created in the start-stop format, as required for the \code{\link{sym_pair_matching}} function. For more details on the data generation process, see details.
}
\usage{
sim_example_data(n, rr=2.5, risk_period=40, max_t=1000)
}
\arguments{
  \item{n}{
A single positive number, specifying the number of individuals that should be simulated.
  }
  \item{rr}{
A single positive number, specifying the true effect of the exposure (\code{A}) on the outcome (\code{Y}) during the \code{risk_period} time units after exposure.
  }
  \item{risk_period}{
A single positive integer, specifying the length of the risk period during which the outcome probability is changed after an exposure occurs.
  }
  \item{max_t}{
A single positive number specifying the amount of points in time that should be generated.
  }
}
\details{
In this data, \code{X} is simulated first using draws from a standard normal distribution. The probability for exposure \code{A} turning \code{TRUE} is dependent on time and \code{X}. The probability for an event in \code{Y} is likewise dependent on time, \code{X} and whether the individual is currently in the risk period after having experienced an exposure \code{A}. Because \code{X} causes both \code{A} and \code{Y}, it is a confounder for the relationship between \code{A} and \code{Y}. Similarly, because the probability for both \code{A} and \code{Y} is time-dependent, time itself acts as a confounding variable. There are no time-dependent confounders and no interactions between \code{X} and \code{A} or time and \code{A}. For simplicity, an individual can only ever be exposed once.

The data is generated using a discrete-event simulation approach with a Gillespie type algorithm as implemented in the \pkg{simDAG} R package. If users want to generate similar data, we highly recommend checking out that package.
}
\author{
Robin Denz
}
\value{
Returns a \code{data.table} containing six columns: \code{.id} (the unique person identifier), \code{start} and \code{stop} (defining some durations of individual-specific time in which no variables changed), \code{X} (a continuous time-fixed confounder), \code{A} (a time-dependent logical exposure variable) and \code{Y} (a recurrent logical event).
}
\seealso{
\code{\link{sym_pair_matching}}, \code{\link[simDAG]{sim_discrete_event}}, \code{\link[simDAG]{node}}
}
\references{
Denz, Robin and Nina Timmesfeld (2026). "Simulating Complex Cross-Sectional and Longitudinal Data using the simDAG R Package". Journal of Statistical Software 116 (2), doi: 10.18637/jss.v116.i02.

Gillespie, Daniel T. 1976. “A General Method for Numerically Simulating the Stochastic Time Evolution of Coupled Chemical Reactions.” Journal of Computational Physics 22 (4): 403–34.

Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld (2026). "The Symmetric Pair Matching Design: A Self-Controlled Method with Automatic Adjustment for Time Effects". arXiv Preprint. doi: 10.48550/arXiv.2608.25979.
}
\examples{
set.seed(1234)

# simulate data with no exposure effect
data <- sim_example_data(n=100, rr=1)
head(data)

# simulate data where the exposure increases the event probability
data <- sim_example_data(n=100, rr=2.5)
head(data)
}
