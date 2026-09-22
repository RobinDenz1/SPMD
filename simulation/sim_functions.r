
## flexible function to create a specific "wavy" baseline hazard
fbasehaz <- function(t, base_p, peak_times, wave_widths,
                     wave_heights) {
  p <- 0
  for (i in seq_len(length(peak_times))) {
    p <- p + wave_heights[i] * exp(-((t - peak_times[i])^2) /
                                     (2 * wave_widths[i]^2))
  }

  return(p + base_p)
}

## baseline hazard for outcome
# = mean(fbasehaz_Y2(seq(1, 1000, 0.1)))
fbasehaz_Y1 <- function(t) {
  return(rep(0.000008508646, length(t)))
}

fbasehaz_Y2 <- function(t) {
  fbasehaz(t=t, base_p=0.000001,
           peak_times=c(0, 200, 400, 600, 800, 1000),
           wave_widths=rep(30, 6),
           wave_heights=rep(0.00002, 6))
}

## baseline hazard for exposure
# = mean(fbasehaz_A2(seq(1, 1000, 0.1)))
fbasehaz_A1 <- function(t) {
  return(rep(0.0002503218, length(t)))
}

fbasehaz_A2 <- function(t) {
  fbasehaz(t=t, base_p=0.0001,
           peak_times=c(0, 200, 400, 600, 800, 1000)-100,
           wave_widths=rep(40, 6),
           wave_heights=rep(0.0003, 6))
}

## generate a dataset following the required DGP for both scenarios
create_data <- function(n, scenario, theta, multiple_A=FALSE,
                        multiple_Y=TRUE, beta_L_A=0, beta_L_Y=0,
                        A_time_interact=0, U_time_interact=0,
                        risk_period=30, censor=0) {

  # no time effects
  if (scenario==1) {
    fa <- fbasehaz_A1
    fy <- fbasehaz_Y1
    # with sinus-curve like time effects
  } else if (scenario==2) {
    fa <- fbasehaz_A2
    fy <- fbasehaz_Y2
  } else if (scenario==3) {
    data <- create_data_scenario3(n=n, theta=theta, risk_period=risk_period)
    return(data)
  }

  if (multiple_A) {
    immunity_duration_A <- 31
  } else {
    immunity_duration_A <- Inf
  }

  if (multiple_Y) {
    immunity_duration_Y <- 1
  } else {
    immunity_duration_Y <- Inf
  }

  # define DAG
  if (A_time_interact==0 & U_time_interact==0) {
    dag <- empty_dag() +
      node("U", type="rnorm", mean=0, sd=1) +
      node_td("L", type="next_time", event_duration=50,
              prob_fun=0.001) +
      node_td("A", type="next_time", model="cox", event_duration=risk_period,
              formula= ~ U*log(2) + L*beta_L_A, surv_dist=fa,
              basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_A) +
      node_td("Y", type="next_time", model="cox", event_duration=1,
              formula= ~ U*log(2) + A*eval(theta) + L*beta_L_Y, surv_dist=fy,
              basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_Y)
  } else if (A_time_interact != 0) {
    dag <- empty_dag() +
      node("U", type="rnorm", mean=0, sd=1) +
      node_td("L", type="next_time", event_duration=50,
              prob_fun=0.001) +
      node_td("A", type="next_time", model="cox", event_duration=risk_period,
              formula= ~ U*log(2) + L*beta_L_A, surv_dist=fa,
              basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_A) +
      node_td("Y", type="next_time", model="cox", event_duration=1,
              formula= ~ U*log(2) + ATRUE*eval(theta) + LTRUE*beta_L_Y +
                ATRUE:time_cuts_event_count*eval(A_time_interact),
              surv_dist=fy,
              basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_Y) +
      node_td("time_cuts", type="next_time", prob_fun=1,
              event_duration=0, distr_fun=simDAG:::timecuts,
              distr_fun_args=list(cuts=300), event_count=TRUE)
  } else if (U_time_interact != 0) {
    dag <- empty_dag() +
      node("U", type="rnorm", mean=0, sd=1) +
      node_td("L", type="next_time", event_duration=50,
              prob_fun=0.001) +
      node_td("A", type="next_time", model="cox", event_duration=risk_period,
              formula= ~ U*log(2) + L*beta_L_A, surv_dist=fa,
              basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_A) +
      node_td("Y", type="next_time", model="cox", event_duration=1,
              formula= ~ U*log(2) + ATRUE*eval(theta) + LTRUE*beta_L_Y +
                U:time_cuts_event_count*eval(U_time_interact),
              surv_dist=fy,
              basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_Y) +
      node_td("time_cuts", type="next_time", prob_fun=1,
              event_duration=0, distr_fun=simDAG:::timecuts,
              distr_fun_args=list(cuts=300), event_count=TRUE)
  }

  # define censoring node, if specified
  if (censor==1) {
    dag <- dag + node_td("C", type="next_time", prob_fun=0.0005,
                         event_duration=Inf, event_count=TRUE)
  } else if (censor==2) {
    dag <- dag + node_td("C", type="next_time",
                         formula= ~ log(0.0003) + U*log(2),
                         event_duration=Inf, event_count=TRUE, link="log")
  } else if (censor==3) {
    dag <- dag + node_td("C", type="next_time",
                         formula= ~ log(0.0003) + L*log(5),
                         event_duration=Inf, event_count=TRUE, link="log")
  }

  # generate data
  data <- sim_discrete_event(dag, n_sim=n, max_t=1000, censor_at_max_t=TRUE,
                             target_event="Y")

  # apply censoring, if specified
  if (censor != 0) {
    data <- subset(data, C_event_count==0)
  }

  return(data)
}

## data for scenario3, in which individuals have different baseline hazards
create_data_scenario3 <- function(n, theta, risk_period) {

  dag <- empty_dag() +
    node("U", type="rnorm", mean=0, sd=1) +
    node_td("A", type="next_time", model="cox", event_duration=risk_period,
            formula= ~ U*log(2), surv_dist=fbasehaz_A2,
            basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
            as_integer=FALSE, immunity_duration=Inf)

  # Y with constant baseline hazard
  dag1 <- dag +
    node_td("Y", type="next_time", model="cox", event_duration=1,
            formula= ~ U*log(2) + A*eval(theta), surv_dist=fbasehaz_Y1,
            basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
            as_integer=FALSE, immunity_duration=1)

  # Y with time-varying baseline hazard
  dag2 <- dag +
    node_td("Y", type="next_time", model="cox", event_duration=1,
            formula= ~ U*log(2) + A*eval(theta), surv_dist=fbasehaz_Y2,
            basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
            as_integer=FALSE, immunity_duration=1)

  # simulate half of the data under one and the other half under another
  # baseline hazard function
  data1 <- sim_discrete_event(dag1, n_sim=n/2, max_t=1000, censor_at_max_t=TRUE,
                              target_event="Y")
  data2 <- sim_discrete_event(dag2, n_sim=n/2, max_t=1000, censor_at_max_t=TRUE,
                              target_event="Y")

  # put together
  data2[, .id := .id + (n/2)]
  data <- rbind(data1, data2)

  return(data)
}

## get RR estimate using different methods
apply_method <- function(data, type, include_ci=FALSE, risk_period=30,
                         allow_overlap=FALSE) {

  if (type=="cox") {
    mod <- coxph(Surv(start, stop, Y) ~ A + U, data=data)
    rr_hat <- as.vector(exp(coef(mod)["ATRUE"]))
    n_events <- n_exposed_and_event <- ci_lower <- ci_upper <- NA
  } else if (type=="spmd") {
    out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                             id=".id", pairs="all", risk_period=risk_period,
                             estimator="moments", bounds="(]",
                             convergence=FALSE, allow_overlap=allow_overlap,
                             bootstrap=include_ci)
    rr_hat <- out$est
    n_events <- out$sizes$n_events
    n_exposed_and_event <- out$sizes$n_exposed_and_event

    if (include_ci) {
      ci_lower <- out$ci[1]
      ci_upper <- out$ci[2]
    } else {
      ci_lower <- ci_upper <- NA
    }
  } else if (type=="sccs") {
    rr_hat <- estimate_sccs(data)
    n_events <- n_exposed_and_event <- ci_lower <- ci_upper <- NA
  } else if (type=="sccs_spline_5") {
    rr_hat <- estimate_sccs(data, spline=TRUE, cuts=seq(0, 1000, 50), df=5)
    n_events <- n_exposed_and_event <- ci_lower <- ci_upper <- NA
  } else if (type=="sccs_spline_15") {
    rr_hat <- estimate_sccs(data, spline=TRUE, cuts=seq(0, 1000, 50), df=15)
    n_events <- n_exposed_and_event <- ci_lower <- ci_upper <- NA
  } else if (type=="cco") {
    rr_hat <- estimate_cco(data, risk_period=risk_period)
    n_events <- n_exposed_and_event <- ci_lower <- ci_upper <- NA
  } else if (type=="ctc") {
    rr_hat <- estimate_ctc(data, risk_period=risk_period)
    n_events <- n_exposed_and_event <- ci_lower <- ci_upper <- NA
  }

  out <- list("rr_hat"=rr_hat,
              "n_events"=n_events,
              "n_exposed_and_event"=n_exposed_and_event,
              "ci_lower"=ci_lower,
              "ci_upper"=ci_upper)

  return(out)
}

## main function to run the entire Monte-Carlo simulation study
run_simulation <- function(n_sim, n_repeats, method, scenario, theta,
                           multiple_A, multiple_Y, beta_L_Y=0, beta_L_A=0,
                           U_time_interact=0, A_time_interact=0,
                           conf_int=FALSE, risk_period=30, allow_overlap=FALSE,
                           censor=0, n_cores=8, seed=2134) {

  # annoying needed fix, because otherwise run() fails
  # due to scoping issues
  global_funs <- ls(envir = .GlobalEnv)
  global_funs <- global_funs[
    vapply(global_funs, function(x) is.function(get(x, envir=.GlobalEnv)),
           logical(1))
  ]

  for (name in global_funs) {
    assign(name, get(name, envir=.GlobalEnv), envir=environment())
  }

  # new simulation object
  sim <- new_sim()

  # set main parameters
  sim %<>% set_levels(
    estimator = method,
    scenario = scenario,
    theta = theta,
    n = n_sim,
    multiple_A = multiple_A,
    multiple_Y = multiple_Y,
    beta_L_Y = beta_L_Y,
    beta_L_A = beta_L_A,
    U_time_interact = U_time_interact,
    A_time_interact = A_time_interact,
    conf_int = conf_int,
    risk_period = risk_period,
    allow_overlap = allow_overlap,
    censor = censor
  )

  # define the simulation script
  sim %<>% set_script(function() {
    batch({
      data <- create_data(n=L$n, scenario=L$scenario, theta=L$theta,
                          multiple_Y=L$multiple_Y, multiple_A=L$multiple_A,
                          beta_L_Y=L$beta_L_Y, beta_L_A=L$beta_L_A,
                          U_time_interact=L$U_time_interact,
                          A_time_interact=L$A_time_interact,
                          risk_period=L$risk_period, censor=L$censor)
    })
    out <- apply_method(data=data, type=L$estimator, include_ci=L$conf_int,
                        risk_period=L$risk_period,
                        allow_overlap=L$allow_overlap)

    return(out)
  })

  # set configurations
  sim %<>% set_config(
    num_sim = n_repeats,
    packages = c("data.table", "SPMD", "survival", "simDAG", "MatchTime",
                 "splines"),
    batch_levels = c("n", "scenario", "theta", "multiple_Y", "multiple_A",
                     "beta_L_A", "beta_L_Y", "U_time_interact",
                     "A_time_interact", "risk_period", "censor"),
    parallel = n_cores > 1,
    n_cores = n_cores,
    seed = seed
  )

  # run simulation
  sim %<>% run()

  return(sim)
}
