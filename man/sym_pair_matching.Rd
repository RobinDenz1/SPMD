\name{sym_pair_matching}
\alias{sym_pair_matching}

\title{
Symmetric Pair Matching Design for Causal Inference
}
\description{
This function can be used to perform symmetric pair matching, which is a self-controlled method that uses symmetric pairings to directly adjust for both time-invariant confounders and time effects. For more information, see details and the associated article (Denz et al. 2026) and vignette.
}
\usage{
sym_pair_matching(formula, data, id, risk_period, bounds="[)",
                  estimator="moments", pairs="random2",
                  n_pairs=100000, batch_size=max(5000, n_pairs * 2),
                  rand_max_iter=100, allow_overlap=FALSE,
                  bootstrap=FALSE, n_boot=1000, conf_level=0.95,
                  n_cores=1, progressbar=TRUE, convergence=TRUE,
                  ...)
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
A single positive number specifying the length of the risk period (corresponds to \eqn{\tau} in the main paper).
  }
  \item{bounds}{
A single character string with two characters, specifying the bounds on the definition of the \code{risk_period}. Allowed values are \code{"()", "[)", "(]"} and \code{"[]"}. Curved parenthesis indicate open intervals, square parenthesis indicate closed intervals. So for example, using the default of \code{"[)"} events that occur at the exact time of exposure are counted as having occured in the risk-period, but events that occur on exactly \code{exposure_time + risk_period} are not. This also impacts which pairs are valid. If the risk period of two pairs "touches" (for example \code{id = 1} has an exposure at \code{t = 10} and \code{id = 2} has an exposure at \code{t = 20} with \code{risk_period=10}), it is considered valid as long as \code{bounds!="[]"}, because then no information is overlapping.
  }
  \item{estimator}{
A single character string specifying which estimator should be used. If \code{"moments"} (default) the empirical based estimator described in the main paper is used. If \code{"glmm"}, a generalized linear model from the Poisson family with a specific random effects structure (\code{(1 | .id_pair:.group)}) is used instead. May also be set to \code{"none"} to only return the matched dataset, without performing any estimation. See details.
  }
  \item{pairs}{
A single character string specifying what pair matches should be used. If \code{"one"}, a dataset is created in which each exposed individual is included in exactly one pair (if possible). If \code{"all"}, all possible valid pairings are created. This may be infeasible with large data. If \code{"random1"}, a pre-specified amount of random pairings are created, without first creating all possible matches. In this case, the amount of pairs is controlled using the \code{n_pairs} argument. If \code{"random2"}, a pre-specified amount of random pairings is created by random sampling from all possible pairings (default). This is faster and more reliable than \code{"random1"}, but it is much more memory inefficient and not feasible with a large number of unique id/exposure combinations (see details).
  }
  \item{n_pairs}{
A single positive number, specifying how many random matches should be created when using \code{pairs="random1"} or \code{pairs="random2"}, ignored otherwise.
  }
  \item{batch_size}{
A single positive integer, specifying the number of candidate pair matches that should be created each iteration when using \code{pairs="random1"}. Higher numbers may result in faster computation, but require more memory usage. Ignored if \code{pairs!="random1"}.
  }
  \item{rand_max_iter}{
A single positive integer, specifying the maximum number of allowed iterations for generating random matches when \code{pairs="random1"}. Since the algorithm is iterative and does not generate all possible matches, it is possible that \code{n_pairs} is larger than the maximum possible number, or that through chance the algorithm gets stuck. This ensures that it eventually terminates (see details). Ignored if \code{pairs!="random1"}.
  }
  \item{allow_overlap}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether the use of pairs with partially overlapping exposure periods should be allowed. If allowed, only the non-overlapping parts of such pairs are used in any calculations, ensuring that no contaminated time is used. Due to usage of more pairs, this might lead to an increase in efficiency. It does, however, increase computational time and is unlikely to have a large impact if \code{risk_period} is small with respect to the total observation time.
  }
  \item{bootstrap}{
Either \code{TRUE} or \code{FALSE}, specifying whether bootstrapping should be performed to estimate confidence intervals. The bootstrapping is done on an individual level, before forming pairs, so that all parts of the analysis are included. This is necessary for correct estimation, but it might lead to long computation times. Especially when using \code{pairs="all"}, \code{pairs="random1"} or \code{pairs="random2"} with a large \code{n_pairs} value, it might take a long time.
  }
  \item{n_boot}{
A single positive integer, specifying the number of bootstrap replications that should be done for confidence interval estimation. Ignored if \code{bootstrap=FALSE}.
  }
  \item{conf_level}{
A single number between 0 and 1, specifying the confidence level that should be used for the confidence interval estimation. Ignored if \code{bootstrap=FALSE}.
  }
  \item{n_cores}{
A single integer, specifying the number of processing cores that should be used for bootstrapping when \code{bootstrap=TRUE} (ignored otherwise).
  }
  \item{progressbar}{
Either \code{TRUE} or \code{FALSE}, specifying whether a progressbar should be shown when \code{bootstrap=TRUE}. This currently only works if \code{n_cores > 1}.
  }
  \item{convergence}{
Either \code{TRUE} or \code{FALSE}, specifying whether to calculate convergence related statistics. Those are calculated by default, but may be turned off to save some computation time.
  }
  \item{...}{
Further arguments passed to \code{\link[lme4]{glmer}} when using \code{estimator="glmm"}, ignored otherwise.
  }
}
\details{

\strong{\emph{How it Works}}:

In the symmetric pair matching design, we form pairs of individuals who were at some point exposed to the exposure of interest. We then use the individuals as control for each other at their respective exposure times. Pairs are only valid if they permit this sort of matching. For example, if individual \code{b} was exposed only at \eqn{t = 50} and individual \code{a} was exposed at \eqn{t = 100} with \code{risk_period=20}, they could form a valid pair. The pair would then look like this:

\preformatted{
----------------------------------------
| individual | time | exposure | index |
----------------------------------------
|     a      |  50  |     0    |   a1  |
|     b      |  50  |     1    |   b1  |
|     b      |  100 |     0    |   b2  |
|     a      |  100 |     1    |   a2  |
----------------------------------------
}

Here, \code{a} acts as control for the exposure period of \code{b} at \eqn{t = 50} and similarly, \code{b} later acts as control for the exposure period of \code{a} at \eqn{t = 100}. If this sort of pairing is not possible, for example because the two exposures are too close together in time or because of another exposure period in the required control time, the pairing is considered invalid. Under the assumption that individual and time effects are multiplicative, the following equation holds for each valid pair:

\deqn{\displaystyle \theta = \frac{1}{2}\ln\left(\frac{\lambda_{b1} \lambda_{a2}}{\lambda_{a1} \lambda_{b2}}\right),}

where the four \eqn{\lambda}s are the event rates for each row (see \code{index}) of the pairing and \eqn{\theta} is the natural log of the multiplicative effect of interest. This means that, under some mild assumptions, both individual-level effects (e.g. any time-fixed confounders) and time effects cancel out. However, since we do not onserve the rates directly, but only event counts in the \code{risk_period} after \code{time}, we have to use related estimators. All of this is described in more detail in the associated paper.

\strong{\emph{Required Data}}:

This function expects \code{data} to be supplied in the start-stop or counting process format. Essentially, it is expected that the \code{data} is formatted in the same way that would be needed to fit a Cox proportional hazards model using the \pkg{survival} package. Each row should correspond to a time duration (defined by a \code{start} and \code{stop} time value) in which no covariates changed. Covariate changes should occur at \code{start} and events should occur exactly at \code{stop}. Events are expected to be binary and only binary exposures are supported. A simple convenience function to create such data from raw exposure and event times is given in this package through \code{\link{prepare_spmd_data}}.

Ideally, the exposure specified on the RHS of the \code{formula} argument is coded as a logical variable, where \code{TRUE} corresponds to the "exposed" time and \code{FALSE} corresponds to the "unexposed" time. If this is not the case, this function will coerce it to this type internally using the following rules:

1.) if the variable only consists of the numbers \code{0} and \code{1} (coded as numeric), \code{0} will be considered the "unexposed" time and \code{1} the "exposed" time; 2.) otherwise, if the variable is a factor, \code{levels(treat)[1]} will be considered the "exposed" time and the other value the "unexposed" time; 3.) otherwise \code{sort(unique(treat))[1]} will be considered "unexposed" and the other value the exposed. It is safest to ensure that the exposure variable is a logical variable.

\strong{\emph{Forming the Pairs}}:

Before we can estimate anything, we need to form valid pairs. Multiple options are available through the \code{pairs} argument. Suppose for the moment that each individual can only be exposed at most one time (only for explanatory purposes, multiple exposures per person are allowed in this package). The most intuitive way to form pairs is to build a pairing so that each exposure instance in the original data is paired to a single other person. In a dataset in which everyone is at most only exposed once, this would mean that every non-exposed person is discarded and each exposed person gets paired with exactly one other exposed person. Small caveat: If the number of exposed individuals is uneven, at least one person will get no match. This can be done using \code{pairs="one"}.

Although fairly simple, this strategy is also inefficient. It means that for each person, we only use the \code{risk_period} time units after its exposure time and the \code{risk_period} time units after its matched control time. All other observation time gets discarded, regardless of whether there might be some events there. The most efficient strategy would be to build \emph{all} possible pairings instead (\code{pairs="all"}). This, however, may not be feasible computationally, as the number of possible matches grows extremly fast with rising sample sizes. Instead, users may generate a pre-specified number of random valid pairs using \code{pairs="random1"} or \code{pairs="random2"}.

In \code{pairs="random1"}, randomly choosen pairs are generated in batches of size \code{batch_size}. It is then checked, whether these matches are valid and whether they were already included in the data. This continues until all \code{n_pairs} matches have been generated, or until \code{rand_max_iter} number of iterations are reached. This strategy is always feasible memory-wise, but is a bit slower than other strategies. It is also a little unstable, because it is possible that \code{n_pairs} is larger than the maximum number of possible (and valid) pairs. The algorithm then runs needlessly until \code{rand_max_iter} is reached. The option \code{pairs="random2"} also generates \code{n_pairs} random pairs, but does so by first generating all possible and valid pairs. This is generally preferable, whenever it is possible.

We recommend trying \code{pairs="all"} first. If that is infeasible, we recommend switching to \code{pairs="random2"}. If that is still infeasible, use \code{pairs="random1"}. The option \code{pairs="one"} should only be used when using \code{estimator="glmm"}, or when the \code{data} is extremly large.

\strong{\emph{Estimators}}:

This package implements two kinds of estimators. The main one (\code{estimator="moments"}) is simply:

\deqn{\displaystyle \hat{\theta}_n = \frac{1}{2}\ln\left(\frac{\sum_j X_{b1j} X_{a2j}}{\sum_j X_{a1j} X_{b2j}}\right),}

where \eqn{X} represents the event counts observed in the \code{risk_period} time units after \code{time} in each \code{group} (exact definitions of the interval can be changed using the \code{bounds} argument). This estimator requires minimal assumptions, but usually requires lots of pairs to be build. With \code{pairs="one"}, it may not work extremly well. Another option is to use a generalized linear mixed Poisson model (\code{estimator="glmm"}). Here, we estimate one fixed effect per group and assign one random effect per group / pair combination. This requires more assumptions, but may be more efficient with \code{pairs="one"}. However, it is also much slower computationally and often does not converge. We recommend using \code{estimator="moments"}.

\strong{\emph{Confidence Intervals & P-Values}}:

Because of the non-linear nature of the estimator and the potentially non-independent pairs, a non-parametric bootstrap procedure on the individual level is needed to estimate confidence intervals or p-values. In particular, percentile bootstrap confidence intervals are used. The p-value tests whether \eqn{\exp(\hat{\theta}) == 1}. Since all aspects of the estimation, including the pair matching, have to be bootstrapped, this may become fairly slow with large sample sizes and large amounts of \code{n_boot}. The \code{n_cores} option may help here, by allowing the usage of multiple processing cores in parallel. The \pkg{doRNG} package is used internally to ensure that the results are still replicable. Additionally, when using \code{pairs="all"} and \code{estimator="moments"}, a weighting trick is performed internally so that the pairs do not have to be build over and over again.

\strong{\emph{Individuals without Events}}:

Similar to the self-controlled case series (SCCS) method (Farrington 1995), individuals that never experienced an event do not contribute meaningfully to any pair when using \code{estimator="moments"}. The reason for this is that we multiply the number of events of the matched individuals at each of the two points in time. Therefore, if an individual has no events, it will always multiply its paired value by 0, effectively making no contribution to the final ratio. To save computation time, we thus exclude all individuals without any events before creating pairs.

}
\author{
Robin Denz
}
\value{
Returns a \code{SPMD} object, containing the following objects:

\itemize{
  \item{\code{d_matches}: A \code{data.table} containing the matched pairs.}
  \item{\code{d_events}: A \code{data.table} containing the events and associated \code{id}s from all relevant individuals.}
  \item{\code{inputs}: A \code{list} containing the arguments supplied by the user.}
  \item{\code{est}: A single number containing the estimated relative risk.}
  \item{\code{d_counts}: If \code{estimator="moments"}, a \code{data.table} containing the four event counts per matched pair.}
  \item{\code{l_sums}: If \code{estimator="moments"}, a \code{list} containing the two sums used in the estimator.}
  \item{\code{model}: If \code{estimator="glmm"}, an object of class \code{merMod}, which is the generalized linear model used to calculate the estimate.}
  \item{\code{d_time_used}: A \code{data.table} containing the amount of observation time used for each individual included in the matching process.}
  \item{\code{convergence}: A \code{list} containing three numbers: \code{A_n} (the number of pairs of pairs that contain at least one overlapping individual, if ordered pairs are considered), \code{E_n} (the number of possible pairs of pairs, if ordered pairs are considered) and \code{ratio} (A_n / E_n^2). All of these numbers are estimated with respect to the pairs actually used.}
  \item{\code{sizes}: A \code{list} containing various numbers describing the used sample sizes.}
}

If \code{bootstrap=TRUE} was used, the output additionally contains:

\itemize{
  \item{\code{ci}: A numeric vector of length 2, containing the confidence interval.}
  \item{\code{se}: A single number containing the estimated standard error.}
  \item{\code{boot_est}: A numeric vector containing the bootstrapped estimates.}
  \item{\code{p_value}: A single number containing the estimated p-value.}
  \item{\code{n_boot_na}: A single number specifying the number of bootstrap estimates that were either \code{NA} or not finite.}
}

}
\seealso{
\code{\link{summary.SPMD}}, \code{\link{plot.SPMD}}, \code{\link{prepare_spmd_data}}
}
\references{
Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld (2026). "The Symmetric Pair Matching Design: A Self-Controlled Method with Automatic Adjustment for Time Effects". arXiv Preprint. doi: 10.48550/arXiv.2608.25979.

Farrington, C. Paddy (1995). "Relative Incidence Estimation from Case Series for Vaccine Safety Evaluation". In: Biometrics 51.1, pp. 228–235. doi: 10.2307/2533328.
}
\examples{
set.seed(42)

# simulate example data
data <- sim_example_data(n=500)

# apply pair matching using all possible pairs and the
# empirical moments based estimator
out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="all", estimator="moments")
summary(out)
}
