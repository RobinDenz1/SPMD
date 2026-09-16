
library(SPMD)
library(simDAG)
library(SimEngine)
library(data.table)
library(ggplot2)
library(survival)
library(splines)
library(scales)
library(beepr)

# code for all other methods used in the simulation
source("./simulation/sim_methods.r")

# code for simulation related functions
source("./simulation/sim_functions.r")

## execute Monte-Carlo study
n_repeats <- 1000
all_methods <- c("spmd", "cco", "ctc", "sccs", "sccs_spline_5",
                 "sccs_spline_15")

## varying n, theta
sim1 <- run_simulation(
  n_sim = c(10000, 20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 1.5, 2.5, 5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  censor = 1,
  n_cores = 8,
  seed = 42
)
saveRDS(sim1$results, "./simulation/data/sim1_results.Rds")

## varying n, theta while allowing multiple exposure periods per person
sim2 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = TRUE,
  multiple_Y= TRUE,
  n_cores = 8,
  seed = 43
)
saveRDS(sim2$results, "./simulation/data/sim2_results.Rds")

## varying n, theta with a terminal event
sim3 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= FALSE,
  n_cores = 8,
  seed = 44
)
saveRDS(sim3$results, "./simulation/data/sim3_results.Rds")

## with time-varying outcome predictor / exposure predictor / confounder
sim4 <- run_simulation(
  n_sim = 20000,
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(2.5),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  beta_L_Y = log(c(1, 3, 5)),
  beta_L_A = log(c(1, 3, 5)),
  n_cores = 8,
  seed = 45
)
saveRDS(sim4$results, "./simulation/data/sim4_results.Rds")

## bootstrap CI coverage
sim5 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = "spmd",
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  conf_int = TRUE,
  n_cores = 8,
  seed = 46
)
saveRDS(sim5$results, "./simulation/data/sim5_results.Rds")

# interaction between time and U
sim6 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  U_time_interact = log(0.5),
  n_cores = 8,
  seed = 47
)
saveRDS(sim6$results, "./simulation/data/sim6_results.Rds")

# interaction between time and A
sim7 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  A_time_interact = log(0.5),
  n_cores = 8,
  seed = 48
)
saveRDS(sim7$results, "./simulation/data/sim7_results.Rds")

# different baseline hazards per person
sim8 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = 3,
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  n_cores = 8,
  seed = 49
)
saveRDS(sim8$results, "./simulation/data/sim8_results.Rds")

# varying risk_period
sim9 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(2.5),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  risk_period = c(10, 50, 100, 150, 300),
  n_cores = 8,
  seed = 50
)
saveRDS(sim9$results, "./simulation/data/sim9_results.Rds")

## varying n, theta and using partial pairs
sim10 <- run_simulation(
  n_sim = c(20000, 30000),
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  allow_overlap = TRUE,
  n_cores = 8,
  seed = 51
)
saveRDS(sim10$results, "./simulation/data/sim10_results.Rds")

## varying theta with completely random right-censoring
sim11 <- run_simulation(
  n_sim = 20000,
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  censor = 1,
  n_cores = 8,
  seed = 52
)
saveRDS(sim11$results, "./simulation/data/sim11_results.Rds")

## varying theta with right-censoring dependent on U
sim12 <- run_simulation(
  n_sim = 20000,
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(c(0.7, 1, 2.5)),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  censor = 2,
  n_cores = 8,
  seed = 53
)
saveRDS(sim12$results, "./simulation/data/sim12_results.Rds")

## varying theta with right-censoring dependent on L
## where L may be a sole outcome / exposure predictor or true confounder
sim13 <- run_simulation(
  n_sim = 20000,
  n_repeats = n_repeats,
  method = all_methods,
  scenario = c(1, 2),
  theta = log(2.5),
  multiple_A = FALSE,
  multiple_Y= TRUE,
  censor = 3,
  beta_L_Y = log(c(1, 3, 5)),
  beta_L_A = log(c(1, 3, 5)),
  n_cores = 8,
  seed = 54
)
saveRDS(sim13$results, "./simulation/data/sim13_results.Rds")

