# pairs='one', estimator='moments'

    Code
      print(out)
    Output
      A SPMD object
       - using each individual in a single symmetric pair
       - using a risk-period of 40 time units
       - using the estimating equations based estimator

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching using an estimating equation based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 535 
        No. unique exposure times = 535 
        No. exposed individuals with event(s) = 535 
        267 symmetric pairs were created
        8% of the included observation time was used
      
      Final estimate: Inf 
      
      Estimated using: exp(0.5 * log(2/0))

# pairs='one', estimator='none'

    Code
      print(out)
    Output
      A SPMD object
       - using each individual in a single symmetric pair
       - using a risk-period of 40 time units
       - without performing estimation

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching  Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 774 
        No. unique exposure times = 774 
        No. exposed individuals with event(s) = 535 
        385 symmetric pairs were created
        8% of the included observation time was used
      
      No estimation was done.
      

# pairs='random', estimator='moments'

    Code
      print(out)
    Output
      A SPMD object
       - using 500 random unique symmetric pairs
       - using a risk-period of 40 time units
       - using the estimating equations based estimator

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching using an estimating equation based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 535 
        No. unique exposure times = 535 
        No. exposed individuals with event(s) = 535 
        120682 symmetric pairs were created
        12.25% of the included observation time was used
      
      Final estimate: 1.732 
      
      Estimated using: exp(0.5 * log(6/2))

# pairs='all', estimator='moments'

    Code
      print(out)
    Output
      A SPMD object
       - using all possible unique symmetric pairs
       - using a risk-period of 40 time units
       - using the estimating equations based estimator

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching using an estimating equation based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 535 
        No. unique exposure times = 535 
        No. exposed individuals with event(s) = 535 
        120862 symmetric pairs were created
        97.71% of the included observation time was used
      
      Final estimate: 3.3 
      
      Estimated using: exp(0.5 * log(1884/173))

# pairs='one', estimator='glmm'

    Code
      print(out)
    Output
      A SPMD object
       - using each individual in a single symmetric pair
       - using a risk-period of 40 time units
       - using the generalized linear model based estimator

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching using a generalized linear mixed model based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 774 
        No. unique exposure times = 774 
        No. exposed individuals with event(s) = 535 
        385 symmetric pairs were created
        8% of the included observation time was used
      
      Final estimate: 3.908 
      

# pairs='random', estimator='glmm'

    Code
      print(out)
    Output
      A SPMD object
       - using 300 random unique symmetric pairs
       - using a risk-period of 40 time units
       - using the estimating equations based estimator

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching using an estimating equation based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 535 
        No. unique exposure times = 535 
        No. exposed individuals with event(s) = 535 
        120610 symmetric pairs were created
        10.44% of the included observation time was used
      
      Final estimate: 1.633 
      
      Estimated using: exp(0.5 * log(8/3))

# pairs='all', estimator='moments', with bootstrap

    Code
      print(out)
    Output
      A SPMD object
       - using all possible unique symmetric pairs
       - using a risk-period of 40 time units
       - using the estimating equations based estimator

---

    Code
      summary(out)
    Output
      Symmetric Pair Matching using an estimating equation based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 535 
        No. unique exposure times = 535 
        No. exposed individuals with event(s) = 535 
        120862 symmetric pairs were created
        97.71% of the included observation time was used
      
      Final estimate: 3.3 
      
      Estimated using: exp(0.5 * log(1884/173))

