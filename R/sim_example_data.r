
## simulate some simple example data with a single continuous and
## time-fixed confounder
#' @importFrom simDAG empty_dag
#' @importFrom simDAG node
#' @importFrom simDAG node_td
#' @importFrom simDAG sim_discrete_event
#' @export
sim_example_data <- function(n, rr=2.5, risk_period=40, max_t=1000) {

  check_inputs_sim(n=n, rr=rr, risk_period=risk_period, max_t=max_t)

  requireNamespace("simDAG", quietly=TRUE)

  # define causal DAG
  dag <- empty_dag() +
    node("X", type="rnorm") +
    node_td("A", type="next_time",
            formula= ~ X*log(3),
            event_duration=risk_period, immunity_duration=Inf,
            model="cox", surv_dist=fbasehaz_A,
            basehaz_grid=seq(1, max_t+10, 0.5),
            extrapolate=TRUE, as_integer=TRUE) +
    node_td("Y", type="next_time", event_duration=1,
            formula= ~ X*log(2) + A*log(eval(rr)),
            model="cox", surv_dist=fbasehaz_Y,
            basehaz_grid=seq(1, max_t+10, 0.5),
            extrapolate=TRUE, as_integer=TRUE)

  # simulate data using discrete-event approach
  data <- sim_discrete_event(dag, n_sim=n, max_t=max_t,
                             censor_at_max_t=TRUE, target_event="Y")
  return(data)
}

## baseline hazard for A
fbasehaz_A <- function(t) {
  0.0001 + t * 0.00001
}

## baseline hazard for Y
fbasehaz_Y <- function(t) {
  0.00005 + t * 0.000001
}

## check inputs for the sim_example_data() function
check_inputs_sim <- function(n, rr, risk_period, max_t) {

  if (!(length(n)==1 && is.numeric(n) && round(n)==n && n > 0)) {
    stop("'n' must be a single integer > 0.", call.=FALSE)
  } else if (!(length(rr)==1 && is.numeric(rr) && rr > 0)) {
    stop("'rr' must be a single number > 0.", call.=FALSE)
  } else if (!(length(risk_period)==1 && is.numeric(risk_period) &&
               risk_period > 0)) {
    stop("'risk_period' must be a single number > 0.", call.=FALSE)
  } else if (!(length(max_t)==1 && is.numeric(max_t) &&
               round(max_t)==max_t && max_t > 0)) {
    stop("'max_t' must be a single integer > 0.", call.=FALSE)
  }
}
