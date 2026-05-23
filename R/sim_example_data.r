
## baseline hazard for A
fbasehaz_A <- function(t) {
  0.0001 + t * 0.00001
}

## baseline hazard for Y
fbasehaz_Y <- function(t) {
  0.00005 + t * 0.000001
}

## simulate some simple example data with a single continuous and
## time-fixed confounder
#' @importFrom simDAG empty_dag
#' @importFrom simDAG node
#' @importFrom simDAG node_td
#' @importFrom simDAG sim_discrete_event
#' @export
sim_example_data <- function(n, rr=2.5, risk_period=40, max_t=1000) {

  dag <- empty_dag() +
    node("X", type="rnorm") +
    node_td("A", type="next_time",
            formula= ~ X*log(3),
            event_duration=risk_period, immunity_duration=Inf,
            model="cox", surv_dist=fbasehaz_A,
            basehaz_grid=seq(1, 10001, 0.5),
            extrapolate=TRUE, as_integer=TRUE) +
    node_td("Y", type="next_time", event_duration=1,
            formula= ~ X*log(2) + A*log(eval(rr)),
            model="cox", surv_dist=fbasehaz_Y,
            basehaz_grid=seq(1, 10001, 0.5),
            extrapolate=TRUE, as_integer=TRUE)

  data <- sim_discrete_event(dag, n_sim=n, max_t=max_t,
                             censor_at_max_t=TRUE, target_event="Y")
  return(data)
}
