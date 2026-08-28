# Summarise the Results of a `SPMD` object

This is the summary S3 method for `SPMD` objects. It shows the final
estimate, as well as some valuable numbers regarding the flow of the
data processing.

## Usage

``` r
# S3 method for class 'SPMD'
summary(object, ...)
```

## Arguments

- object:

  An `SPMD` object created using the
  [`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md)
  function.

- ...:

  Currently not used.

## Details

Since symmetric pair matching by design cannot use all data, it is
important to understand what data is actually being used. First, all
individuals who were never exposed are discarded, since they can never
be used. Furthermore, when using `estimator="moments"`, individuals that
never experienced an event cannot meaningfully contribute to the
estimation, since event counts are multiplied across individuals.
Additionally, only individuals who can be matched to someone else can be
included, although this is usually not a problem in practice, since we
do not match on any covariates directly.

Finally, when an exposure instance of an individual is included in the
matched data, only the `risk_period` time units after exposure and at
the control time directly enter the calculations, the rest of the
individuals observation time remains unused. When using `pairs="one"`,
this means that very little of the actual observation time of included
units is used. To check how much is used, the percentage of the total
time used amongst the ever included indiviiduals is included in the
output. It may be used to get a rough idea how to specify `n_pairs` when
using `pairs="random"`.

When `estimator="moments"` is used, the output also shows the estimating
equation used, as well as the ratio \\\|A_n\| / \|E_n\|^2\\, where
\\\|A_n\|\\ is the number of pairs of pairs that share at least one
individual and \\\|E_n\|\\ is the number of all possible pairs (both
counting ordered pairs). The ratio should be close to 0, otherwise
pair-dependence might have influenced the estimates. See appendix B of
the main paper.

## Author

Robin Denz

## Value

Returns an object of class `summary.SPMD`. This object stores some of
the information of the original `SPMD` object in a more convenient
fashion.

## See also

[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md),
[`plot.SPMD`](https://robindenz1.github.io/SPMD/reference/plot.SPMD.md)

## References

Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld
(2026). "The Symmetric Pair Matching Design: A Self-Controlled Method
with Automatic Adjustment for Time Effects". arXiv Preprint. doi:
10.48550/arXiv.2608.25979.

## Examples

``` r
set.seed(1234)

# simulate data where the exposure increases the event probability
data <- sim_example_data(n=100, rr=2.5)
head(data)
#> Key: <.id, start>
#>      .id start  stop          X      A      Y
#>    <int> <num> <num>      <num> <lgcl> <lgcl>
#> 1:     1     0   386 -1.2070657  FALSE  FALSE
#> 2:     1   386   426 -1.2070657   TRUE  FALSE
#> 3:     1   426  1000 -1.2070657  FALSE  FALSE
#> 4:     2     0    86  0.2774292  FALSE   TRUE
#> 5:     2    86    87  0.2774292  FALSE  FALSE
#> 6:     2    87   188  0.2774292  FALSE  FALSE

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
#>   Individuals                      100
#>   Exposed individuals              68
#>   Exposure episodes                68
#>   Individuals with >=1 event       68
#>   Exposed + event                  68
#>   Symmetric pairs                  1,931
#>   Observation time used            70.45%
#> 
#> Effect estimate
#>   log(RR)    RR
#>   0.405      1.500
#> 
#> Estimation
#>   Estimating equation: exp{1/2 log(9 / 4)}
#>   |A_n| / |E_n|^2: 1.56274e-08
#> ──────────────────────────────────────────────────────────────
```
