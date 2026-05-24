
## implements the main estimator build using estimating equations
estimate_moments <- function(data) {

  d_counts <- matches2counts(data)
  l_sums <- list(X2_X4=sum(d_counts$X2 * d_counts$X4),
                 X1_X3=sum(d_counts$X1 * d_counts$X3))
  est <- exp(0.5 * log(l_sums$X2_X4 / l_sums$X1_X3))

  out <- list(d_counts=d_counts, l_sums=l_sums, est=est)

  return(out)
}

## perform estimation using a generalized linear model with
## appropriate random effects
estimate_glmm <- function(data, ...) {

  requireNamespace("lme4", quietly=TRUE)

  # fit mixed model
  model <- lme4::glmer(
    .n_events ~ factor(.group) + (1 | .id_pair:.group),
    data = data,
    family = "poisson",
    ...
  )

  # calculate estimate from coefficients
  beta <- lme4::fixef(model)

  est <- exp(0.5 * (
    beta["factor(.group)2"] +
      beta["factor(.group)4"] -
      beta["factor(.group)3"]
  ))

  out <- list(model=model, est=as.vector(est))

  return(out)
}
