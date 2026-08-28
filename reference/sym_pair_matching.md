# Symmetric Pair Matching Design for Causal Inference

This function can be used to perform symmetric pair matching, which is a
self-controlled method that uses symmetric pairings to directly adjust
for both time-invariant confounders and time effects. For more
information, see details and the associated article (Denz et al. 2026)
and vignette.

## Usage

``` r
sym_pair_matching(formula, data, id, risk_period, bounds="[)",
                  estimator="moments", pairs="random2",
                  n_pairs=100000, batch_size=max(5000, n_pairs * 2),
                  rand_max_iter=100, bootstrap=FALSE, n_boot=1000,
                  conf_level=0.95, n_cores=1, progressbar=TRUE,
                  ...)
```

## Arguments

- formula:

  A `formula` object defining the variables that identify the start and
  stop of the respective observation periods, the outcome and exposure.
  It should look like `Surv(start, stop, Y) ~ A`, where `start`, `stop`,
  `Y` and `A` should be replaced with the names of the start and stop of
  the observation period, the outcome and the exposure respectively. The
  exposure should be binary, ideally coded as a logical variable.
  However, other codings are allowed as long as it is binary. See
  details.

- data:

  A single `data.frame` like object in the start-stop format. This
  dataset should contain at least five columns: (1) the start of the
  observation period, (2) the end of the observation period, (3) the
  outcome of interest, (4) the exposure of interest and (5) a unique
  person identifier (see `id`). It can contain any further number of
  columns. For more details on how the start-stop data should be
  structured, see details.

- id:

  A single character string, specifying the name of the unique person
  identifier in `data`.

- risk_period:

  A single positive number specifying the length of the risk period
  (corresponds to \\\tau\\ in the main paper).

- bounds:

  A single character string with two characters, specifying the bounds
  on the definition of the `risk_period`. Allowed values are
  `"()", "[)", "(]"` and `"[]"`. Curved parenthesis indicate open
  intervals, square parenthesis indicate closed intervals. So for
  example, using the default of `"[)"` events that occur at the exact
  time of exposure are counted as having occured in the risk-period, but
  events that occur on exactly `exposure_time + risk_period` are not.
  This also impacts which pairs are valid. If the risk period of two
  pairs "touches" (for example `id = 1` has an exposure at `t = 10` and
  `id = 2` has an exposure at `t = 20` with `risk_period=10`), it is
  considered valid as long as `bounds!="[]"`, because then no
  information is overlapping.

- estimator:

  A single character string specifying which estimator should be used.
  If `"moments"` (default) the empirical based estimator described in
  the main paper is used. If `"glmm"`, a generalized linear model from
  the Poisson family with a specific random effects structure
  (`(1 | .id_pair:.group)`) is used instead. May also be set to `"none"`
  to only return the matched dataset, without performing any estimation.
  See details.

- pairs:

  A single character string specifying what pair matches should be used.
  If `"one"`, a dataset is created in which each exposed individual is
  included in exactly one pair (if possible). If `"all"`, all possible
  valid pairings are created. This may be infeasible with large data. If
  `"random1"`, a pre-specified amount of random pairings are created,
  without first creating all possible matches. In this case, the amount
  of pairs is controlled using the `n_pairs` argument. If `"random2"`, a
  pre-specified amount of random pairings is created by random sampling
  from all possible pairings (default). This is faster and more reliable
  than `"random1"`, but it is much more memory inefficient and not
  feasible with a large number of unique id/exposure combinations (see
  details).

- n_pairs:

  A single positive number, specifying how many random matches should be
  created when using `pairs="random1"` or `pairs="random2"`, ignored
  otherwise.

- batch_size:

  A single positive integer, specifying the number of candidate pair
  matches that should be created each iteration when using
  `pairs="random1"`. Higher numbers may result in faster computation,
  but require more memory usage. Ignored if `pairs!="random1"`.

- rand_max_iter:

  A single positive integer, specifying the maximum number of allowed
  iterations for generating random matches when `pairs="random1"`. Since
  the algorithm is iterative and does not generate all possible matches,
  it is possible that `n_pairs` is larger than the maximum possible
  number, or that through chance the algorithm gets stuck. This ensures
  that it eventually terminates (see details). Ignored if
  `pairs!="random1"`.

- bootstrap:

  Either `TRUE` or `FALSE`, specifying whether bootstrapping should be
  performed to estimate confidence intervals. The bootstrapping is done
  on an individual level, before forming pairs, so that all parts of the
  analysis are included. This is necessary for correct estimation, but
  it might lead to long computation times. Especially when using
  `pairs="all"`, `pairs="random1"` or `pairs="random2"` with a large
  `n_pairs` value, it might take a long time.

- n_boot:

  A single positive integer, specifying the number of bootstrap
  replications that should be done for confidence interval estimation.
  Ignored if `bootstrap=FALSE`.

- conf_level:

  A single number between 0 and 1, specifying the confidence level that
  should be used for the confidence interval estimation. Ignored if
  `bootstrap=FALSE`.

- n_cores:

  A single integer, specifying the number of processing cores that
  should be used for bootstrapping when `bootstrap=TRUE` (ignored
  otherwise).

- progressbar:

  Either `TRUE` or `FALSE`, specifying whether a progressbar should be
  shown when `bootstrap=TRUE`. This currently only works if
  `n_cores > 1`.

- ...:

  Further arguments passed to
  [`glmer`](https://rdrr.io/pkg/lme4/man/glmer.html) when using
  `estimator="glmm"`, ignored otherwise.

## Details

***How it Works***:

In the symmetric pair matching design, we form pairs of individuals who
were at some point exposed to the exposure of interest. We then use the
individuals as control for each other at their respective exposure
times. Pairs are only valid if they permit this sort of matching. For
example, if individual `b` was exposed only at \\t = 50\\ and individual
`a` was exposed at \\t = 100\\ with `risk_period=20`, they could form a
valid pair. The pair would then look like this:


    ----------------------------------------
    | individual | time | exposure | index |
    ----------------------------------------
    |     a      |  50  |     0    |   a1  |
    |     b      |  50  |     1    |   b1  |
    |     b      |  100 |     0    |   b2  |
    |     a      |  100 |     1    |   a2  |
    ----------------------------------------

Here, `a` acts as control for the exposure period of `b` at \\t = 50\\
and similarly, `b` later acts as control for the exposure period of `a`
at \\t = 100\\. If this sort of pairing is not possible, for example
because the two exposures are too close together in time or because of
another exposure period in the required control time, the pairing is
considered invalid. Under the assumption that individual and time
effects are multiplicative, the following equation holds for each valid
pair:

\$\$\theta = \frac{1}{2}\log\left(\frac{\lambda\_{b1}
\lambda\_{a2}}{\lambda\_{a1} \lambda\_{b2}}\right),\$\$

where the four \\\lambda\\s are the event rates for each row (see
`index`) of the pairing and \\\theta\\ is the natural log of the
multiplicative effect of interest. This means that, under some mild
assumptions, both individual-level effects (e.g. any time-fixed
confounders) and time effects cancel out. However, since we do not
onserve the rates directly, but only event counts in the `risk_period`
after `time`, we have to use related estimators. All of this is
described in more detail in the associated paper.

***Required Data***:

This function expects `data` to be supplied in the start-stop or
counting process format. Essentially, it is expected that the `data` is
formatted in the same way that would be needed to fit a Cox proportional
hazards model using the survival package. Each row should correspond to
a time duration (defined by a `start` and `stop` time value) in which no
covariates changed. Covariate changes should occur at `start` and events
should occur exactly at `stop`. Events are expected to be binary and
only binary exposures are supported.

Ideally, the exposure specified on the RHS of the `formula` argument is
coded as a logical variable, where `TRUE` corresponds to the "exposed"
time and `FALSE` corresponds to the "unexposed" time. If this is not the
case, this function will coerce it to this type internally using the
following rules:

1.) if the variable only consists of the numbers `0` and `1` (coded as
numeric), `0` will be considered the "unexposed" time and `1` the
"exposed" time; 2.) otherwise, if the variable is a factor,
`levels(treat)[1]` will be considered the "exposed" time and the other
value the "unexposed" time; 3.) otherwise `sort(unique(treat))[1]` will
be considered "unexposed" and the other value the exposed. It is safest
to ensure that the exposure variable is a logical variable.

***Forming the Pairs***:

Before we can estimate anything, we need to form valid pairs. Multiple
options are available through the `pairs` argument. Suppose for the
moment that each individual can only be exposed at most one time (only
for explanatory purposes, multiple exposures per person are allowed in
this package). The most intuitive way to form pairs is to build a
pairing so that each exposure instance in the original data is paired to
a single other person. In a dataset in which everyone is at most only
exposed once, this would mean that every non-exposed person is discarded
and each exposed person gets paired with exactly one other exposed
person. Small caveat: If the number of exposed individuals is uneven, at
least one person will get no match. This can be done using
`pairs="one"`.

Although fairly simple, this strategy is also inefficient. It means that
for each person, we only use the `risk_period` time units after its
exposure time and the `risk_period` time units after its matched control
time. All other observation time gets discarded, regardless of whether
there might be some events there. The most efficient strategy would be
to build *all* possible pairings instead (`pairs="all"`). This, however,
may not be feasible computationally, as the number of possible matches
grows extremly fast with rising sample sizes. Instead, users may
generate a pre-specified number of random valid pairs using
`pairs="random1"` or `pairs="random2"`.

In `pairs="random1"`, randomly choosen pairs are generated in batches of
size `batch_size`. It is then checked, whether these matches are valid
and whether they were already included in the data. This continues until
all `n_pairs` matches have been generated, or until `rand_max_iter`
number of iterations are reached. This strategy is always feasible
memory-wise, but is a bit slower than other strategies. It is also a
little unstable, because it is possible that `n_pairs` is larger than
the maximum number of possible (and valid) pairs. The algorithm then
runs needlessly until `rand_max_iter` is reached. The option
`pairs="random2"` also generates `n_pairs` random pairs, but does so by
first generating all possible and valid pairs. This is generally
preferable, whenever it is possible.

We recommend trying `pairs="all"` first. If that is infeasible, we
recommend switching to `pairs="random2"`. If that is still infeasible,
use `pairs="random1"`. The option `pairs="one"` should only be used when
using `estimator="glmm"`, or when the `data` is extremly large.

***Estimators***:

This package implements two kinds of estimators. The main one
(`estimator="moments"`) is simply:

\$\$\hat{\theta}\_n = \frac{1}{2}\log\left(\frac{\sum_i X\_{b1}
X\_{a2}}{\sum_i X\_{a1} X\_{b2}}\right),\$\$

where \\X\\ represents the event counts observed in the `risk_period`
time units after `time` in each `group` (exact definitions of the
interval can be changed using the `bounds` argument). This estimator
requires minimal assumptions, but usually requires lots of pairs to be
build. With `pairs="one"`, it may not work extremly well. Another option
is to use a generalized linear mixed Poisson model (`estimator="glmm"`).
Here, we estimate one fixed effect per group and assign one random
effect per group / pair combination. This requires more assumptions, but
may be more efficient with `pairs="one"`. However, it is also much
slower computationally and often does not converge. We recommend using
`estimator="moments"`.

***Confidence Intervals & P-Values***:

Because of the non-linear nature of the estimator and the potentially
non-independent pairs, a non-parametric bootstrap procedure on the
individual level is needed to estimate confidence intervals or p-values.
In particular, percentile bootstrap confidence intervals are used. The
p-value tests whether \\\exp(\hat{\theta}) == 1\\. Since all aspects of
the estimation, including the pair matching, have to be bootstrapped,
this may become fairly slow with large sample sizes and large amounts of
`n_boot`. The `n_cores` option may help here, by allowing the usage of
multiple processing cores in parallel. The doRNG package is used
internally to ensure that the results are still replicable.
Additionally, when using `pairs="all"` and `estimator="moments"`, a
weighting trick is performed internally so that the pairs do not have to
be build over and over again.

***Individuals without Events***:

Similar to the self-controlled case series (SCCS) method (Farrington
1995), individuals that never experienced an event do not contribute
meaningfully to any pair when using `estimator="moments"`. The reason
for this is that we multiply the number of events of the matched
individuals at each of the two points in time. Therefore, if an
individual has no events, it will always multiply its paired value by 0,
effectively making no contribution to the final ratio. To save
computation time, we thus exclude all individuals without any events
before creating pairs.

## Author

Robin Denz

## Value

Returns a `SPMD` object, containing the following objects:

- `d_matches`: A `data.table` containing the matched pairs.

- `d_events`: A `data.table` containing the events and associated `id`s
  from all relevant individuals.

- `inputs`: A `list` containing the arguments supplied by the user.

- `est`: A single number containing the estimated relative risk.

- `d_counts`: If `estimator="moments"`, a `data.table` containing the
  four event counts per matched pair.

- `l_sums`: If `estimator="moments"`, a `list` containing the two sums
  used in the estimator.

- `model`: If `estimator="glmm"`, an object of class `merMod`, which is
  the generalized linear model used to calculate the estimate.

- `d_time_used`: A `data.table` containing the amount of observation
  time used for each individual included in the matching process.

- `convergence`: A `list` containing three numbers: `A_n` (the number of
  pairs of pairs that contain at least one overlapping individual, if
  ordered pairs are considered), `E_n` (the number of possible pairs of
  pairs, if ordered pairs are considered) and `ratio` (A_n / E_n^2). All
  of these numbers are estimated with respect to the pairs actually
  used.

- `sizes`: A `list` containing various numbers describing the used
  sample sizes.

If `bootstrap=TRUE` was used, the output additionally contains:

- `ci`: A numeric vector of length 2, containing the confidence
  interval.

- `se`: A single number containing the estimated standard error.

- `boot_est`: A numeric vector containing the bootstrapped estimates.

- `p_value`: A single number containing the estimated p-value.

## See also

[`summary.SPMD`](https://robindenz1.github.io/SPMD/reference/summary.SPMD.md),
[`plot.SPMD`](https://robindenz1.github.io/SPMD/reference/plot.SPMD.md)

## References

Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld
(2026). "The Symmetric Pair Matching Design: A Self-Controlled Method
with Automatic Adjustment for Time Effects". arXiv Preprint. doi:
10.48550/arXiv.2608.25979.

Farrington, C. Paddy (1995). "Relative Incidence Estimation from Case
Series for Vaccine Safety Evaluation". In: Biometrics 51.1, pp. 228–235.
doi: 10.2307/2533328.

## Examples

``` r
set.seed(42)

# simulate example data
data <- sim_example_data(n=500)

# apply pair matching using all possible pairs and the
# empirical moments based estimator
out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data, id=".id",
                         risk_period=40, pairs="all", estimator="moments")
summary(out)
#> ──────────────────────────────────────────────────────────────
#> Symmetric Pair Matching Design
#> ──────────────────────────────────────────────────────────────
#> Design
#>   Risk period                      40
#>   Pairing strategy                 All possible pairs
#>   Estimator                        Estimating equations
#> 
#> Sample
#>   Individuals                      500
#>   Exposed individuals              334
#>   Exposure episodes                334
#>   Individuals with >=1 event       334
#>   Exposed + event                  334
#>   Symmetric pairs                  47,252
#>   Observation time used            95.84%
#> 
#> Effect estimate
#>   log(RR)    RR
#>   0.903      2.468
#> 
#> Estimation
#>   Estimating equation: exp{1/2 log(676 / 111)}
#>   |A_n| / |E_n|^2: 5.388323e-12
#> ──────────────────────────────────────────────────────────────
```
