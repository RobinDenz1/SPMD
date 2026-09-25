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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     535
        Symmetric pairs                  267
        Observation time used            8.00%
      
      Effect estimate
        No finite estimate available.
      
      Estimation
        Estimating equation: exp{1/2 log(2 / 0)}
        CI estimation method: 'none'
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     774
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     535
        Symmetric pairs                  500
        Observation time used            12.33%
      
      Effect estimate
        log(IRR)   IRR
        1.242      3.464
      
      Estimation
        Estimating equation: exp{1/2 log(12 / 1)}
        CI estimation method: 'none'
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     535
        Symmetric pairs                  500
        Observation time used            12.33%
      
      Effect estimate
        log(IRR)   IRR        SE         95% CI          P-value
        1.242      3.464      0.608      1.414 – 2.769   0.1
      
      Bootstrap: 10 replicates (5 NA or Inf)
      
      Estimation
        Estimating equation: exp{1/2 log(12 / 1)}
        CI estimation method: 'boot'
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     535
        Symmetric pairs                  121,412
        Observation time used            97.91%
      
      Effect estimate
        log(IRR)   IRR        SE         95% CI          P-value
        1.194      3.300      0.483      2.477 – 4.397   <0.001
      
      Estimation
        Estimating equation: exp{1/2 log(1,895 / 174)}
        CI estimation method: 'jackknife'
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     774
        Symmetric pairs                  385
        Observation time used            8.00%
      
      Effect estimate
        log(IRR)   IRR
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     535
        Symmetric pairs                  300
        Observation time used            10.46%
      
      Effect estimate
        No finite estimate available.
      
      Estimation
        Estimating equation: exp{1/2 log(5 / 0)}
        CI estimation method: 'none'
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
        Exposed individuals (total)      774
        Exposure episodes (total)        774
        Individuals with >=1 event       540
        Exposed + event                  535
        Exposure episodes (included)     535
        Symmetric pairs                  121,412
        Observation time used            97.91%
      
      Effect estimate
        log(IRR)   IRR        SE         99% CI          P-value
        1.194      3.300      0.396      2.456 – 3.350   0.25
      
      Bootstrap: 4 replicates
      
      Estimation
        Estimating equation: exp{1/2 log(1,895 / 174)}
        CI estimation method: 'boot'
        |A_n| / |E_n|^2: 0.007507649
      ──────────────────────────────────────────────────────────────

