# Simulate Some Simple Example Data

This function is mostly used to obtain example data to illustrate the
features of the SPMD package. It simulates a dataset with three main
variables: `X` (a continuous time-fixed confounder), `A` (a binary
time-dependent exposure variable) and `Y` (a recurrent event outcome).
The data is created in the start-stop format, as required for the
[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md)
function. For more details on the data generation process, see details.

## Usage

``` r
sim_example_data(n, rr=2.5, risk_period=40, max_t=1000)
```

## Arguments

- n:

  A single positive number, specifying the number of individuals that
  should be simulated.

- rr:

  A single positive number, specifying the true effect of the exposure
  (`A`) on the outcome (`Y`) during the `risk_period` time units after
  exposure.

- risk_period:

  A single positive integer, specifying the length of the risk period
  during which the outcome probability is changed after an exposure
  occurs.

- max_t:

  A single positive number specifying the amount of points in time that
  should be generated.

## Details

In this data, `X` is simulated first using draws from a standard normal
distribution. The probability for exposure `A` turning `TRUE` is
dependent on time and `X`. The probability for an event in `Y` is
likewise dependent on time, `X` and whether the individual is currently
in the risk period after having experienced an exposure `A`. Because `X`
causes both `A` and `Y`, it is a confounder for the relationship between
`A` and `Y`. Similarly, because the probability for both `A` and `Y` is
time-dependent, time itself acts as a confounding variable. There are no
time-dependent confounders and no interactions between `X` and `A` or
time and `A`. For simplicity, an individual can only ever be exposed
once.

The data is generated using a discrete-event simulation approach with a
Gillespie type algorithm as implemented in the simDAG R package. If
users want to generate similar data, we highly recommend checking out
that package.

## Author

Robin Denz

## Value

Returns a `data.table` containing six columns: `.id` (the unique person
identifier), `start` and `stop` (defining some durations of
individual-specific time in which no variables changed), `X` (a
continuous time-fixed confounder), `A` (a time-dependent logical
exposure variable) and `Y` (a recurrent logical event).

## See also

[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md),
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.html),
[`node`](https://robindenz1.github.io/simDAG/reference/node.html)

## References

Denz, Robin and Nina Timmesfeld (2026). "Simulating Complex
Cross-Sectional and Longitudinal Data using the simDAG R Package".
Journal of Statistical Software 116 (2), doi: 10.18637/jss.v116.i02.

Gillespie, Daniel T. 1976. “A General Method for Numerically Simulating
the Stochastic Time Evolution of Coupled Chemical Reactions.” Journal of
Computational Physics 22 (4): 403–34.

Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld
(2026). "The Symmetric Pair Matching Design: A Self-Controlled Method
with Automatic Adjustment for Time Effects". arXiv Preprint. doi:
10.48550/arXiv.2608.25979.

## Examples

``` r
set.seed(1234)

# simulate data with no exposure effect
data <- sim_example_data(n=100, rr=1)
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

# simulate data where the exposure increases the event probability
data <- sim_example_data(n=100, rr=2.5)
head(data)
#> Key: <.id, start>
#>      .id start  stop          X      A      Y
#>    <int> <num> <num>      <num> <lgcl> <lgcl>
#> 1:     1     0   984 -3.3960635  FALSE   TRUE
#> 2:     1   984   985 -3.3960635  FALSE  FALSE
#> 3:     1   985  1000 -3.3960635  FALSE  FALSE
#> 4:     2     0   302 -0.7813523  FALSE  FALSE
#> 5:     2   302   342 -0.7813523   TRUE  FALSE
#> 6:     2   342  1000 -0.7813523  FALSE  FALSE
```
