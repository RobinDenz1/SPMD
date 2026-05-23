library(data.table)
library(survival)
library(lme4)
library(testthat)
library(simDAG)

data.table::setDTthreads(1)

test_check("SPMD")
