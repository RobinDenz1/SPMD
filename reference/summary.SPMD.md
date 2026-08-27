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
be used. Secondly, all exposures that occur `risk_period` time units
before the individuals observation end (right-censoring) can also not be
used, since then we would be unable to observe the full event count.
Furthermore, when using `estimator="moments"`, individuals that never
experienced an event cannot meaningfully contribute to the estimation,
since event counts are multiplied across individuals. Additionally, only
individuals who can be matched to someone else can be included, although
this is usually not a problem in practice, since we do not match on any
covariates directly.

Finally, when an exposure instance of an individual is included in the
matched data, only the `risk_period` time units after exposure and at
the control time directly enter the calculations, the rest of the
individuals observation time remains unused. When using `pairs="one"`,
this means that very little of the actual observation time of included
units is used. To check how much is used, the percentage of the total
time used amongst the ever included indiviiduals is included in the
output. It may be used to get a rough idea how to specify `n_pairs` when
using `pairs="random"`.

## Author

Robin Denz

## Value

Returns `NULL`.

## See also

[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md)

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
#> Symmetric Pair Matching using an estimating equation based estimator
#>   Formula: Surv(start, stop, Y) ~ A 
#>   Risk period: 40 
#> 
#>   No. individuals in data = 100 
#>   No. exposed individuals = 68 
#>   No. unique exposure times = 68 
#>   No. exposed individuals with event(s) = 68 
#>   1931 symmetric pairs were created
#>   70.45% of the included observation time was used
#> 
#> Final estimate: 1.5 
#> 
#> Estimated using: exp(0.5 * log(9/4))
```
