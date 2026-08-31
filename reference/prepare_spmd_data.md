# Prepare data for `sym_pair_matching()`

This function may be used to transform raw information about exposure,
event and observation times into the start-stop format required to use
the
[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md)
function.

## Usage

``` r
prepare_spmd_data(exposures, events, obs_start, obs_end,
                  id, time, risk_period, exposure_name="A",
                  event_name="Y")
```

## Arguments

- exposures:

  A `data.frame` like object with two columns: `id` (the unique person
  identifier) and `time` (the exposure times of an individual). This
  object defines when the exposure occured for each individual. If an
  individual experienced multiple exposure times, one row per exposure
  time should be included (e.g. the dataset should be in the
  long-format). The output dataset will only contain individuals that
  appear in this dataset, because individuals without an exposure are
  ignored in
  [`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md).

- events:

  A `data.frame` like object with two columns: `id` (the unique person
  identifier) and `time` (the event times of an individual). This object
  defines when the event occured for each individual. Similar to
  `exposures`, multiple events per person can be included by using
  multiple rows per person.

- obs_start:

  Either a single number or Date specifying the start of the observation
  period (usually 0), or a `data.frame` like object structured similarly
  as the `exposures` and `events` arguments. Again, it should have two
  columns: `id` (the unique person identifier) and `time` (the start of
  the observation time for the individual). Contraty to `exposures` and
  `events` it needs to have exactly one entry for each `id` in
  `exposures`, otherwise an error will be produced.

- obs_end:

  Same as `obs_start`, but for the end of the observation period.

- id:

  A single character string specifying the name of the unique person
  identifier in `exposures`, `events` and possible `obs_start` and
  `obs_end`.

- time:

  A single character string specifying the name of the time related
  columns in `exposures`, `events` and possible `obs_start` and
  `obs_end`.

- risk_period:

  A single positive number specifying the length of the risk period,
  essentially equal to the argument of the same name in
  [`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md).

- exposure_name:

  A single character string specifying how the exposure indicator in the
  output data should be named.

- event_name:

  A single character string specifying how the event indicator in the
  output data should be named.

## Details

The dataset produced by this function is in the exact format needed for
the
[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md)
function. Exposure times are exactly at `start`, event times exactly at
`stop`. Internally, it uses the `merge_start_stop()` function from the
MatchTime package to perform the data wrangling.

## Author

Robin Denz

## Value

Returns a `data.table` containing five columns: `.id` (the unique person
identifier), `start` and `stop` (defining some durations of
individual-specific time in which no variables changed), `exposure_name`
(a time-dependent logical exposure variable) and `event_name` (a
recurrent logical event).

## See also

[`sym_pair_matching`](https://robindenz1.github.io/SPMD/reference/sym_pair_matching.md)

## References

Denz, Robin, Filippo Saatkamp, Katharina Meiszl and Nina Timmesfeld
(2026). "The Symmetric Pair Matching Design: A Self-Controlled Method
with Automatic Adjustment for Time Effects". arXiv Preprint. doi:
10.48550/arXiv.2608.25979.

## Examples

``` r
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
#> Key: <.id>
#>      .id start  stop      A      Y
#>    <num> <num> <num> <lgcl> <lgcl>
#> 1:     1     0    14  FALSE  FALSE
#> 2:     1    14    18   TRUE   TRUE
#> 3:     1    18    44   TRUE  FALSE
#> 4:     1    44   299  FALSE  FALSE
#> 5:     1   299   329   TRUE  FALSE
#> 6:     1   329  1000  FALSE  FALSE

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
#> Key: <.id>
#>      .id start  stop      A      Y
#>    <num> <num> <num> <lgcl> <lgcl>
#> 1:     1     0    14  FALSE  FALSE
#> 2:     1    14    18   TRUE   TRUE
#> 3:     1    18    44   TRUE  FALSE
#> 4:     1    44   299  FALSE  FALSE
#> 5:     1   299   329   TRUE  FALSE
#> 6:     1   329   400  FALSE  FALSE
```
