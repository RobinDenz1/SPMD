
## implements the main estimator build using estimating equations
estimate_moments <- function(data, bootstrap=FALSE, n_boot=1000,
                             conf_level=0.95, n_cores=1, progressbar=TRUE) {

  d_counts <- matches2counts(data, bootstrap=bootstrap)

  # faster bootstrap for pairs="all" only
  if (bootstrap) {

    boot <- get_boot_moments(d_counts=d_counts, data=data, n_boot=n_boot,
                             n_cores=n_cores, progressbar=progressbar)
    se <- stats::sd(boot, na.rm=TRUE)
    ci <- stats::quantile(
      x=boot, probs=c((1-conf_level)/2, conf_level+((1-conf_level)/2)),
      na.rm=TRUE, names=FALSE
    )

    l_sums <- list(X2_X4=sum(d_counts$X2_X4),
                   X1_X3=sum(d_counts$X1_X3))
  } else {
    l_sums <- list(X2_X4=sum(d_counts$X2 * d_counts$X4),
                   X1_X3=sum(d_counts$X1 * d_counts$X3))
    se <- NULL
    ci <- NULL
    boot <- NULL
  }

  # point estimate
  est <- exp(0.5 * log(l_sums$X2_X4 / l_sums$X1_X3))

  out <- list(d_counts=d_counts, l_sums=l_sums, est=est, se=se,
              ci=ci, boot_est=boot)

  return(out)
}

## get bootstrap estimates for the estimating equations based estimator
## with pairs="all" without actually performing the matching multiple times
## through the use of weighting
#' @importFrom data.table setDTthreads
get_boot_moments <- function(d_counts, data, n_boot, n_cores, progressbar) {

  X2_X4 <- X2 <- X4 <- X1_X3 <- X1 <- X3 <- NULL

  d_counts[, X2_X4 := X2 * X4]
  d_counts[, X1_X3 := X1 * X3]

  # data.table storing person weights
  d_W <- data.table(.id=unique(data$.id))
  n <- nrow(d_W)

  if (n_cores==1) {
    boot_out <- numeric(n_boot)
    for (i in seq_len(n_boot)) {
      boot_out[i] <- one_boot_iter_moments(d_W=d_W, d_counts=d_counts, n=n)
    }
  } else {

    requireNamespace("parallel", quietly=TRUE)
    requireNamespace("doRNG", quietly=TRUE)
    requireNamespace("doSNOW", quietly=TRUE)
    requireNamespace("foreach", quietly=TRUE)

    `%dorng%` <- doRNG::`%dorng%`

    # setup clusters
    cl <- parallel::makeCluster(n_cores, outfile="")
    doSNOW::registerDoSNOW(cl)
    pkgs <- c("data.table", "SPMD")

    glob_funs <- ls(envir=.GlobalEnv)[vapply(ls(envir=.GlobalEnv), function(obj)
      "function"==class(eval(parse(text=obj)))[1], FUN.VALUE=logical(1))]

    # progressbar
    if (progressbar) {
      pb <- utils::txtProgressBar(max=n_boot, style=3)
      progress <- function(n) {utils::setTxtProgressBar(pb, n)}
      opts <- list(progress=progress)
    } else {
      opts <- NULL
    }

    # run loop in parallel
    boot_out <- foreach::foreach(i=seq_len(n_boot), .packages=pkgs,
                                .export=glob_funs, .options.snow=opts) %dorng% {
      setDTthreads(1)

      one_boot_iter_moments <- utils::getFromNamespace("one_boot_iter_moments",
                                                       "SPMD")
      one_boot_iter_moments(d_W=d_W, d_counts=d_counts, n=n)
    }
    on.exit(close(pb))
    on.exit(parallel::stopCluster(cl))

    boot_out <- unlist(boot_out)
  }

  return(boot_out)
}

## a single iteration of the bootstrap when using pairs="all" and
## estimator="moments"
#' @importFrom data.table copy
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom data.table setnames
one_boot_iter_moments <- function(d_W, d_counts, n) {

  d_W <- copy(d_W)
  d_counts <- copy(d_counts)

  # draw bootstrap weights
  d_W[, W := stats::rpois(n=n, lambda=1)]

  # merge weights for .id1
  d_counts <- merge(d_counts, d_W, by.x=".id1", by.y=".id", all.x=TRUE,
                    all.y=FALSE)
  setnames(d_counts, old="W", new="W1")

  # merge weights for .id2
  d_counts <- merge(d_counts, d_W, by.x=".id2", by.y=".id", all.x=TRUE,
                    all.y=FALSE)
  setnames(d_counts, old="W", new="W2")

  # total pair weight
  W <- d_counts$W1 * d_counts$W2

  # bootstrap estimate
  out <- exp(0.5 * log(sum(W * d_counts$X2_X4) /
                       sum(W * d_counts$X1_X3)))
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
