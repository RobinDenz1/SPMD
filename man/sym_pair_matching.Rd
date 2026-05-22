\name{sym_pair_matching}
\alias{sym_pair_matching}

\title{
Symmetric Pair Matching for Causal Inference
}
\description{
This function can be used to perform symmetric pair matching, which is a self-controlled method that uses symmetric pairings to directly adjust for both time-invariant confounders and time effects. For more information, see details and the associated article and vignette.
}
\usage{
sym_pair_matching(formula, data, id, risk_period, pairs="one",
                  n_pairs=NULL, estimator="moments",
                  include_exp_time=TRUE, ...)
}
\arguments{
  \item{formula}{
A \code{formula} object defining the variables that identify the start and stop of the respective observation periods, the outcome and exposure. It should look like \code{Surv(start, stop, Y) ~ A}, where \code{start}, \code{stop}, \code{Y} and \code{A} should be replaced with the names of the start and stop of the observation period, the outcome and the exposure respectively. The exposure should be binary, ideally coded as a logical variable. However, other codings are allowed as long as it is binary. See details.
  }
  \item{data}{
A single \code{data.frame} like object in the start-stop format. This dataset should contain at least five columns: (1) the start of the observation period, (2) the end of the observation period, (3) the outcome of interest, (4) the exposure of interest and (5) a unique person identifier (see \code{id}). It can contain any further number of columns. For more details on how the start-stop data should be structured, see details.
  }
  \item{id}{
A single character string, specifying the name of the unique person identifier in \code{data}.
  }
  \item{risk_period}{
A single positive number specifying the length of the risk period.
  }
  \item{pairs}{
A single character string specifying what pair matches should be used. If \code{"one"} (default), a dataset is created in which each exposed individual is included in exactly one pair (if possible). If \code{"all"}, all possible valid pairings are created. This may be infeasible with large data. If \code{"random"}, a pre-specified amount of random pairings are created. In this case, the amount of pairs is controlled using the \code{n_pairs} argument.
  }
  \item{n_pairs}{
A single positive number, specifying how many random matches should be created when using \code{pairs="random"}, ignored otherwise.
  }
  \item{estimator}{
A single character string specifying which estimator should be used. If \code{"moments"} (default) the estimating equations based estimator described in the main paper is used. If \code{"glmm"}, a generalized linear model from the Poisson family with a specific random effects structure (\code{(1 | .id_pair:.group)}) is used instead. May also be set to \code{"none"} to only return the matched dataset, without performing any estimation. See details.
  }
  \item{include_exp_time}{
Either \code{TRUE} or \code{FALSE}, specifying whether events that occur exactly at the time of exposure should be counted as events in the \code{risk_period} or not. If \code{TRUE}, those events are counted, if \code{FALSE} they are not. Note that events that occur exactly at \code{exposure_time + risk_period} are always counted.
  }
  \item{...}{
Further arguments passed to \code{\link[lme4]{glmer}} when using \code{estimator="glmm"}, ignored otherwise.
  }
}
\details{

\strong{\emph{How it Works}}:

To be written

}
\author{
Robin Denz
}
\value{
Returns a \code{SPMD} object, containing the following objects:

\itemize{
  \item{\code{d_matches}: A \code{data.table} containing the matched pairs.}
  \item{\code{inputs}: A \code{list} containing the arguments supplied by the user.}
  \item{\code{est}: A single number containing the estimated relative risk.}
  \item{\code{d_counts}: If \code{estimator="moments"}, a \code{data.table} containing the four event counts per matched pair.}
  \item{\code{l_sums}: If \code{estimator="moments"}, a \code{list} containing the two sums used in the estimator.}
  \item{\code{model}: If \code{estimator="glmm"}, an object of class \code{merMod}, which is the generalized linear model used to calculate the estimate.}
}
}
\seealso{
\code{\link{print.SPMD}}, \code{\link{summary.SPMD}}, \code{\link{plot.SPMD}}
}
\references{
TODO
}
\examples{
#TODO
}
