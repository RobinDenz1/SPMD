# pairs='one', estimator='moments'

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
      summary(out)
    Output
      Symmetric Pair Matching using an estimating equation based estimator
        Formula: Surv(start, stop, Y) ~ A 
        Risk period: 40 
      
        No. individuals in data = 800 
        No. exposed individuals = 535 
        No. unique exposure times = 535 
        No. exposed individuals with event(s) = 535 
        500 symmetric pairs were created
        12.33% of the included observation time was used
      
      Final estimate: 3.464 
      
      Estimated using: exp(0.5 * log(12/1))

# pairs='random', estimator='moments', bootstrap=TRUE

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
        500 symmetric pairs were created
        12.33% of the included observation time was used
      
      Final estimate: 3.464 
      95% CI: [1.732; Inf]
      P-Value: 0.1 
      
      Estimated using: exp(0.5 * log(12/1))
      Bootstrap CI based on 10 bootstrap replications

# pairs='all', estimator='moments'

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
        121412 symmetric pairs were created
        97.91% of the included observation time was used
      
      Final estimate: 3.3 
      
      Estimated using: exp(0.5 * log(1895/174))

# pairs='one', estimator='glmm'

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
        10.46% of the included observation time was used
      
      Final estimate: Inf 
      
      Estimated using: exp(0.5 * log(5/0))

# pairs='all', estimator='moments', with bootstrap

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
        121412 symmetric pairs were created
        97.91% of the included observation time was used
      
      Final estimate: 3.3 
      
      Estimated using: exp(0.5 * log(1895/174))

