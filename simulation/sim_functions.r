
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
  return(rep(0.00001701729, length(t)))
}

fbasehaz_Y2 <- function(t) {
  fbasehaz(t=t, base_p=0.000002,
           peak_times=c(0, 200, 400, 600, 800, 1000),
           wave_widths=rep(30, 6),
           wave_heights=rep(0.00004, 6))
}

## baseline hazard for exposure
# = mean(fbasehaz_A2(seq(1, 1000, 0.1)))
fbasehaz_A1 <- function(t) {
  return(rep(0.0005006436, length(t)))
}

fbasehaz_A2 <- function(t) {
  fbasehaz(t=t, base_p=0.0002,
           peak_times=c(0, 200, 400, 600, 800, 1000)-100,
           wave_widths=rep(40, 6),
           wave_heights=rep(0.0006, 6))
}

## generate a dataset following the required DGP for both scenarios
create_data <- function(n, scenario, theta, multiple_A=FALSE,
                        multiple_Y=TRUE, beta_L_A=0, beta_L_Y=0, beta_Y_count=0,
                        A_time_interact=0, U_time_interact=0,
                        risk_period=30, censor=0, max_t=1000) {

  # no time effects
  if (scenario==1) {
    fa <- fbasehaz_A1
    fy <- fbasehaz_Y1
  # with sinus-curve like time effects
  } else if (scenario==2) {
    fa <- fbasehaz_A2
    fy <- fbasehaz_Y2
  # with different baseline hazards across individuals
  } else if (scenario==3) {
    data <- create_data_scenario3(n=n, theta=theta, risk_period=risk_period,
                                  max_t=max_t)
    return(data)
  }

  if (multiple_A) {
    immunity_duration_A <- risk_period + 1
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
              formula= ~ U*log(2) + L*beta_L_A + Y_event_count*beta_Y_count,
              surv_dist=fa,
              basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_A) +
      node_td("Y", type="next_time", model="cox", event_duration=1,
              formula= ~ U*log(2) + A*eval(theta) + L*beta_L_Y, surv_dist=fy,
              basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_Y,
              event_count=TRUE)
  } else if (A_time_interact != 0) {
    dag <- empty_dag() +
      node("U", type="rnorm", mean=0, sd=1) +
      node_td("L", type="next_time", event_duration=50,
              prob_fun=0.001) +
      node_td("A", type="next_time", model="cox", event_duration=risk_period,
              formula= ~ U*log(2) + L*beta_L_A, surv_dist=fa,
              basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_A) +
      node_td("Y", type="next_time", model="cox", event_duration=1,
              formula= ~ U*log(2) + ATRUE*eval(theta) + LTRUE*beta_L_Y +
                ATRUE:time_cuts_event_count*eval(A_time_interact),
              surv_dist=fy,
              basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
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
              basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
              as_integer=FALSE, immunity_duration=immunity_duration_A) +
      node_td("Y", type="next_time", model="cox", event_duration=1,
              formula= ~ U*log(2) + ATRUE*eval(theta) + LTRUE*beta_L_Y +
                U:time_cuts_event_count*eval(U_time_interact),
              surv_dist=fy,
              basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
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
  data <- sim_discrete_event(dag, n_sim=n, max_t=max_t, censor_at_max_t=TRUE,
                             target_event="Y")

  # apply censoring, if specified
  if (censor != 0 & censor < 4) {
    data <- subset(data, C_event_count==0)
  # censor after first event
  } else if (censor == 4) {
    data[, min_time := if (any(Y)) min(stop[Y]) else Inf, by=.id]
    data <- subset(data, stop <= min_time)
  }

  return(data)
}

# baseline hazards for the event in scenario 3
fbasehaz_Y3.1 <- function(t) {
  return(0.000017017292 *
           (1 + 0.95 * tanh((t - 500) / 100)))
}

fbasehaz_Y3.2 <- function(t) {
  return(0.000017017292 *
           (1 - 0.95 * tanh((t - 500) / 100)))
}

## data for scenario3, in which individuals have different baseline hazards
create_data_scenario3 <- function(n, theta, risk_period, max_t) {

  dag <- empty_dag() +
    node("U", type="rnorm", mean=0, sd=1) +
    node_td("A", type="next_time", model="cox", event_duration=risk_period,
            formula= ~ U*log(2), surv_dist=fbasehaz_A2,
            basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
            as_integer=FALSE, immunity_duration=Inf)

  # Y with constant baseline hazard
  dag1 <- dag +
    node_td("Y", type="next_time", model="cox", event_duration=1,
            formula= ~ U*log(2) + A*eval(theta), surv_dist=fbasehaz_Y3.1,
            basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
            as_integer=FALSE, immunity_duration=1)

  # Y with time-varying baseline hazard
  dag2 <- dag +
    node_td("Y", type="next_time", model="cox", event_duration=1,
            formula= ~ U*log(2) + A*eval(theta), surv_dist=fbasehaz_Y3.2,
            basehaz_grid=seq(0.5, (max_t + 100), 0.5), extrapolate=TRUE,
            as_integer=FALSE, immunity_duration=1)

  # simulate half of the data under one and the other half under another
  # baseline hazard function
  data1 <- sim_discrete_event(dag1, n_sim=n/2, max_t=max_t,
                              censor_at_max_t=TRUE, target_event="Y")
  data2 <- sim_discrete_event(dag2, n_sim=n/2, max_t=max_t,
                              censor_at_max_t=TRUE, target_event="Y")

  # put together
  data2[, .id := .id + (n/2)]
  data <- rbind(data1, data2)

  return(data)
}

## get RR estimate using different methods
apply_method <- function(data, type, conf_int="none", risk_period=30,
                         allow_overlap=FALSE, pairs="all", n_pairs=100000) {

  if (type=="cox") {
    mod <- coxph(Surv(start, stop, Y) ~ A + U, data=data)
    rr_hat <- as.vector(exp(coef(mod)["ATRUE"]))

    out <- list(
      "rr_hat"=rr_hat,
      "ci_lower"=NA,
      "ci_upper"=NA,
      "n_events"=NA,
      "n_exposed_and_event"=NA,
      "boot_na"=0
    )

  } else if (type=="spmd") {
    spm <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                             id=".id", pairs=pairs, risk_period=risk_period,
                             estimator="moments", bounds="(]",
                             convergence=FALSE, allow_overlap=allow_overlap,
                             allow_partial=allow_overlap,
                             conf_type=conf_int, n_pairs=n_pairs)
    out <- list(
      "rr_hat"=spm$est,
      "ci_lower"=NA,
      "ci_upper"=NA,
      "n_events"=spm$sizes$n_events,
      "n_exposed_and_event"=spm$sizes$n_exposed_and_event,
      "boot_na"=0
    )

    if (conf_int!="none") {
      out$ci_lower <- spm$ci[1]
      out$ci_upper <- spm$ci[2]
      out$boot_na <- ifelse(is.null(spm$n_boot_na), 0, spm$n_boot_na)
    }
  } else if (type=="sccs") {
    out <- estimate_sccs(data, conf_int=conf_int!="none")
  } else if (type=="sccs_spline_5") {
    out <- estimate_sccs(data, spline=TRUE, cuts=seq(0, 1000, 50), df=6,
                         conf_int=conf_int!="none")
  } else if (type=="sccs_spline_15") {
    out <- estimate_sccs(data, spline=TRUE, cuts=seq(0, 1000, 50), df=16,
                         conf_int=conf_int!="none")
  } else if (type=="cco") {
    out <- estimate_cco(data, risk_period=risk_period,
                        conf_int=conf_int!="none")
  } else if (type=="ctc") {
    out <- estimate_ctc(data, risk_period=risk_period,
                        conf_int=conf_int!="none")
  }

  return(out)
}

## main function to run the entire Monte-Carlo simulation study
run_simulation <- function(n_sim, n_repeats, method, scenario, theta,
                           multiple_A, multiple_Y, beta_L_Y=0, beta_L_A=0,
                           beta_Y_count=0, U_time_interact=0, A_time_interact=0,
                           conf_int="none", risk_period=30, allow_overlap=FALSE,
                           censor=0, pairs="all", n_pairs=1000000, max_t=1000,
                           n_cores=8, seed=2134) {

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
    beta_Y_count = beta_Y_count,
    U_time_interact = U_time_interact,
    A_time_interact = A_time_interact,
    conf_int = conf_int,
    risk_period = risk_period,
    allow_overlap = allow_overlap,
    censor = censor,
    pairs = pairs,
    n_pairs = n_pairs,
    max_t = max_t
  )

  # define the simulation script
  sim %<>% set_script(function() {
    batch({
      data <- create_data(n=L$n, scenario=L$scenario, theta=L$theta,
                          multiple_Y=L$multiple_Y, multiple_A=L$multiple_A,
                          beta_L_Y=L$beta_L_Y, beta_L_A=L$beta_L_A,
                          U_time_interact=L$U_time_interact,
                          A_time_interact=L$A_time_interact,
                          risk_period=L$risk_period, censor=L$censor,
                          max_t=L$max_t, beta_Y_count=L$beta_Y_count)
    })
    out <- apply_method(data=data, type=L$estimator, conf_int=L$conf_int,
                        risk_period=L$risk_period,
                        allow_overlap=L$allow_overlap, pairs=L$pairs,
                        n_pairs=L$n_pairs)

    return(out)
  })

  # set configurations
  sim %<>% set_config(
    num_sim = n_repeats,
    packages = c("data.table", "SPMD", "survival", "simDAG", "MatchTime",
                 "splines"),
    batch_levels = c("n", "scenario", "theta", "multiple_Y", "multiple_A",
                     "beta_L_A", "beta_L_Y", "U_time_interact",
                     "A_time_interact", "risk_period", "censor", "max_t",
                     "beta_Y_count"),
    parallel = n_cores > 1,
    n_cores = n_cores,
    seed = seed
  )

  # run simulation
  sim %<>% run()

  return(sim)
}
