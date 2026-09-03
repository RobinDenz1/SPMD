
library(SPMD)
library(simDAG)
library(SimEngine)
library(data.table)
library(ggplot2)
library(survival)
library(splines)
library(scales)

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
                        multiple_Y=TRUE) {

  # no time effects
  if (scenario==1) {
    fa <- fbasehaz_A1
    fy <- fbasehaz_Y1
  # with sinus-curve like time effects
  } else if (scenario==2) {
    fa <- fbasehaz_A2
    fy <- fbasehaz_Y2
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
  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1) +
    node_td("A", type="next_time", model="cox", event_duration=30,
            formula= ~ X*log(2), surv_dist=fa,
            basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
            as_integer=TRUE, immunity_duration=immunity_duration_A) +
    node_td("Y", type="next_time", model="cox", event_duration=1,
            formula= ~ X*log(2) + A*eval(theta), surv_dist=fy,
            basehaz_grid=seq(0.5, 1100, 0.5), extrapolate=TRUE,
            as_integer=TRUE, immunity_duration=immunity_duration_Y)

  # generate data
  data <- sim_discrete_event(dag, n_sim=n, max_t=1000, censor_at_max_t=TRUE,
                             target_event="Y")
  return(data)
}

## get RR estimate using different methods
estimate_rr <- function(data, type) {

  if (type=="cox") {
    mod <- coxph(Surv(start, stop, Y) ~ A + X, data=data)
    rr <- as.vector(exp(coef(mod)["ATRUE"]))
  } else if (type=="spmd") {
    out <- sym_pair_matching(Surv(start, stop, Y) ~ A, data=data,
                             id=".id", pairs="all", risk_period=30,
                             estimator="moments", bounds="(]",
                             convergence=FALSE, allow_overlap=TRUE)
    rr <- out$est
  } else if (type=="sccs") {
    rr <- estimate_sccs(data)
  } else if (type=="sccs_spline_5") {
    rr <- estimate_sccs(data, spline=TRUE, cuts=seq(0, 1000, 50), df=5)
  } else if (type=="sccs_spline_15") {
    rr <- estimate_sccs(data, spline=TRUE, cuts=seq(0, 1000, 50), df=15)
  } else if (type=="cco") {
    rr <- estimate_cco(data, risk_period=30)
  } else if (type=="ctc") {
    rr <- estimate_ctc(data, risk_period=30)
  }
  return(rr)
}

# main simulation
sim <- new_sim()

sim %<>% set_levels(
  estimator = c("cox", "spmd", "cco", "ctc", "sccs", "sccs_spline_5",
                "sccs_spline_15"),
  scenario = c(1, 2),
  #theta = log(c(0.7, 1, 1.5, 2.5, 5)),
  #n = c(5000, 10000, 20000),
  theta = log(2.5),
  n = 20000,
  multiple_A = c(TRUE, FALSE),
  multiple_Y = TRUE
)

sim %<>% set_script(function() {
  batch({
    data <- create_data(n=L$n, scenario=L$scenario, theta=L$theta,
                        multiple_Y=L$multiple_Y, multiple_A=L$multiple_A)
  })
  rr_hat <- estimate_rr(data=data, type=L$estimator)
  return(list("rr_hat"=rr_hat))
})

sim %<>% set_config(
  num_sim = 1000,
  packages = c("data.table", "SPMD", "survival", "simDAG",
               "MatchTime", "splines"),
  batch_levels = c("n", "scenario", "theta", "multiple_Y", "multiple_A"),
  parallel = TRUE,
  n_cores = 8,
  seed = 42,
  return_batch_id = TRUE
)

sim %<>% run()

saveRDS(sim$results, "sim_results.Rds")

# some simulation data pre-processing
plotdata <- sim$results
plotdata$scenario <- paste0("Scenario ", plotdata$scenario)
plotdata$estimator <- factor(
  plotdata$estimator,
  levels = c("cox", "cco", "ctc", "sccs", "sccs_spline_5",
             "sccs_spline_15", "spmd"),
  labels = c("Cox", "CCO", "CTC", "SCCS", "SCCS\nSpline (5)",
             "SCCS\nSpline (15)", "SPMD")
)
plotdata$exp_theta <- exp(plotdata$theta)
plotdata$exp_theta_f <- factor(plotdata$exp_theta,
                               levels=c("0.7", "1", "1.5", "2.5", "5"),
                               labels=c(expression(exp(theta)~`=`~0.7),
                                        expression(exp(theta)~`=`~1),
                                        expression(exp(theta)~`=`~1.5),
                                        expression(exp(theta)~`=`~2.5),
                                        expression(exp(theta)~`=`~5)))

# these are actually -Inf before applying exp(), so they should not count
plotdata <- subset(plotdata, !(rr_hat==0 & estimator=="SPMD"))

## results plot for main paper
ggplot(subset(plotdata, estimator!="Cox" & theta==log(2.5) & n==20000 &
                multiple_A==FALSE),
       aes(y=log(rr_hat) - log(2.5), x=estimator, fill=estimator)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_boxplot(outliers=FALSE, linewidth=0.3) +
  facet_wrap(~ scenario, ncol=1) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(x=NULL, y=expression(hat(theta) - theta))
ggsave("../sim_results.pdf", width=6, height=5)

## boxplots by n, theta
plot_boxplots_n <- function(data) {
  ggplot(data=data,
         aes(y=log(rr_hat) - theta, x=estimator, fill=estimator)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_boxplot(outliers=FALSE, linewidth=0.3) +
    facet_grid(cols=vars(scenario), rows=vars(exp_theta_f),
               labeller = labeller(
                 exp_theta_f = label_parsed,
                 scenario = label_value
               )) +
    theme_minimal() +
    theme(legend.position="none",
          axis.text.x=element_text(size=8)) +
    labs(x=NULL, y=expression(hat(theta) - theta))
}

plot_boxplots_n(subset(plotdata, estimator!="Cox" & n==5000))

plot_boxplots_n(subset(plotdata, estimator!="Cox" & n==10000))
ggsave("../Plots/sim_boxplots_n10000.pdf", width=8, height=8)

plot_boxplots_n(subset(plotdata, estimator!="Cox" & n==20000))
ggsave("../Plots/sim_boxplots_n20000.pdf", width=8, height=8)

ggplot(subset(plotdata, estimator=="SPMD"),
       aes(x=factor(n), y=runtime, fill=factor(exp_theta))) +
  geom_boxplot(size=0.2, linewidth=0.3) +
  facet_wrap(~ scenario, ncol=1) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="n", y="Runtime in seconds", fill=expression(exp(theta)))
ggsave("../Plots/sim_runtime.pdf", width=6, height=5)

# bias / mse table
setDT(plotdata)

d_bias <- subset(plotdata, is.finite(rr_hat) & !is.na(rr_hat))

d_bias <- d_bias[,
  .(bias = mean(log(rr_hat) - theta, na.rm=TRUE),
    bias_mcse = sd(log(rr_hat) - theta, na.rm=TRUE) / sqrt(1000),
    mse = mean((log(rr_hat) - theta)^2, na.rm=TRUE),
    mse_mcse = sd((log(rr_hat) - theta)^2, na.rm=TRUE) / sqrt(1000),
    n_NA = 1000 - .N),
  by=c("n", "exp_theta", "scenario", "estimator")
]

setkey(d_bias, n, exp_theta, scenario, estimator)
d_bias

knitr::kable(subset(d_bias, n==5000)[, -"n"], format="latex", booktabs=TRUE,
             digits=3, linesep="", longtable=TRUE)
knitr::kable(subset(d_bias, n==10000)[, -"n"], format="latex", booktabs=TRUE,
             digits=3, linesep="", longtable=TRUE)
knitr::kable(subset(d_bias, n==20000)[, -"n"], format="latex", booktabs=TRUE,
             digits=3, linesep="", longtable=TRUE)

## time trend plot
plotdata <- data.frame(time=rep(1:1000, 4),
                       p=c(rep(0.000008508646, 1000), p_Y=fbasehaz_Y2(1:1000),
                           rep(0.0002503218, 1000), p_A=fbasehaz_A2(1:1000)),
                       kind=rep(c("h_Y(t)", "h_A(t)"), each=2000),
                       scenario=c(rep("Scenario 1", 1000), rep("Scenario 2", 1000),
                                  rep("Scenario 1", 1000), rep("Scenario 2", 1000)))

ggplot(plotdata, aes(x=time, y=p, color=kind, linetype=scenario)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="t", y="Baseline Hazard", color=NULL, linetype=NULL) +
  scale_y_continuous(labels=label_number()) +
  scale_color_discrete(labels=parse(text=c("h[Y0](t)", "h[A0](t)"))) +
  scale_linetype_manual(values=c("dashed", "solid"),
                        labels=c("Scenario 1", "Scenario 2"))
ggsave("../time_trends.pdf", width=6, height=4)
