# pairs='one', estimator='moments'

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 One pair per individual
        Estimator                        Estimating equations
      
      Sample
        Individuals                      800
        Exposed individuals              535
        Exposure episodes                535
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  267
        Observation time used            8.00%
      
      Effect estimate
        No finite estimate available.
      
      Estimation
        Estimating equation: exp{1/2 log(2 / 0)}
        |A_n| / |E_n|^2: 0
      ──────────────────────────────────────────────────────────────

# pairs='one', estimator='none'

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 One pair per individual
        Estimator                        None
      
      Sample
        Individuals                      800
        Exposed individuals              774
        Exposure episodes                774
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  385
        Observation time used            8.00%
      
      Effect estimate
        No finite estimate available.
      ──────────────────────────────────────────────────────────────

# pairs='random', estimator='moments'

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 Random pairs (500)
        Estimator                        Estimating equations
      
      Sample
        Individuals                      800
        Exposed individuals              535
        Exposure episodes                535
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  500
        Observation time used            12.33%
      
      Effect estimate
        log(RR)    RR
        1.242      3.464
      
      Estimation
        Estimating equation: exp{1/2 log(12 / 1)}
        |A_n| / |E_n|^2: 0.007456
      ──────────────────────────────────────────────────────────────

# pairs='random', estimator='moments', bootstrap=TRUE

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 Random pairs (500)
        Estimator                        Estimating equations
      
      Sample
        Individuals                      800
        Exposed individuals              535
        Exposure episodes                535
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  500
        Observation time used            12.33%
      
      Effect estimate
        log(RR)    RR         SE         95% CI          P-value
        1.242      3.464      NaN        1.414 – Inf     0.1
      
      Bootstrap: 10 replicates (5 NA or Inf)
      
      Estimation
        Estimating equation: exp{1/2 log(12 / 1)}
        |A_n| / |E_n|^2: 0.007456
      ──────────────────────────────────────────────────────────────

# pairs='all', estimator='moments'

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 All possible pairs
        Estimator                        Estimating equations
      
      Sample
        Individuals                      800
        Exposed individuals              535
        Exposure episodes                535
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  121,412
        Observation time used            97.91%
      
      Effect estimate
        log(RR)    RR
        1.194      3.300
      
      Estimation
        Estimating equation: exp{1/2 log(1,895 / 174)}
        |A_n| / |E_n|^2: 0.007507649
      ──────────────────────────────────────────────────────────────

# pairs='one', estimator='glmm'

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 One pair per individual
        Estimator                        Generalized linear mixed model
      
      Sample
        Individuals                      800
        Exposed individuals              774
        Exposure episodes                774
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  385
        Observation time used            8.00%
      
      Effect estimate
        log(RR)    RR
        1.363      3.908
      ──────────────────────────────────────────────────────────────

# pairs='random', estimator='glmm'

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 Random pairs (300)
        Estimator                        Estimating equations
      
      Sample
        Individuals                      800
        Exposed individuals              535
        Exposure episodes                535
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  300
        Observation time used            10.46%
      
      Effect estimate
        No finite estimate available.
      
      Estimation
        Estimating equation: exp{1/2 log(5 / 0)}
        |A_n| / |E_n|^2: 0.008044444
      ──────────────────────────────────────────────────────────────

# pairs='all', estimator='moments', with bootstrap

    Code
      summary(out)
    Output
      ──────────────────────────────────────────────────────────────
      Symmetric Pair Matching Design
      ──────────────────────────────────────────────────────────────
      Design
        Risk period                      40
        Pairing strategy                 All possible pairs
        Estimator                        Estimating equations
      
      Sample
        Individuals                      800
        Exposed individuals              535
        Exposure episodes                535
        Individuals with >=1 event       535
        Exposed + event                  535
        Symmetric pairs                  121,412
        Observation time used            97.91%
      
      Effect estimate
        log(RR)    RR
        1.194      3.300
      
      Estimation
        Estimating equation: exp{1/2 log(1,895 / 174)}
        |A_n| / |E_n|^2: 0.007507649
      ──────────────────────────────────────────────────────────────

