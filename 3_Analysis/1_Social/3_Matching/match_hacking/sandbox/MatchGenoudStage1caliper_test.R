MatchGenoudStage1caliper <- function (Tr = Tr, X = X, All = All, M = M, weights = weights, 
          exact = exact, caliper = caliper, distance.tolerance, tolerance) 
{
  N <- nrow(X)
  xvars <- ncol(X)
  weights.orig <- as.matrix(weights)
  
#seems like same if-else as in Match.R
  if (!is.null(exact)) {
    exact = as.vector(exact)
    nexacts = length(exact)
    if ((nexacts > 1) & (nexacts != xvars)) {
      warning("length of exact != ncol(X). Ignoring exact option")
      exact <- NULL
    }
    else if (nexacts == 1 & (xvars > 1)) {
      exact <- rep(exact, xvars)
    }
  }
  if (!is.null(caliper)) {
    caliper = as.vector(caliper)
    ncalipers = length(caliper)
    if ((ncalipers > 1) & (ncalipers != xvars)) {
      warning("length of caliper != ncol(X). Ignoring caliper option")
      caliper <- NULL
    }
    else if (ncalipers == 1 & (xvars > 1)) {
      caliper <- rep(caliper, xvars)
    }
  }
  if (!is.null(caliper)) {
    ecaliper <- vector(mode = "numeric", length = xvars)
    sweights <- sum(weights.orig)
    for (i in 1:xvars) {
      meanX <- sum(X[, i] * weights.orig)/sweights
      sdX <- sqrt(sum((X[, i] - meanX)^2)/sweights)
      ecaliper[i] <- caliper[i] * sdX
    }
  }
  else {
    ecaliper <- NULL
  }
  if (!is.null(exact)) {
    if (is.null(caliper)) {
      max.diff <- abs(max(X) - min(X) + distance.tolerance * 
                        100)
      ecaliper <- matrix(max.diff, nrow = xvars, ncol = 1)
    }
    for (i in 1:xvars) {
      if (exact[i]) 
        ecaliper[i] <- distance.tolerance
    }
  }
  if (All == 2) # for ATC
    Tr <- 1 - Tr
  if (All == 1) {
    M <- min(M, min(sum(Tr), sum(1 - Tr))) #ensure M is not grater than # matches
  }
  else {
    M <- min(M, sum(1 - Tr))
  }
  
  # standardizing the X values (X-mean/sd); see eq 1
  
  Mu.X <- matrix(0, xvars, 1) # for mean of all X vars
  Sig.X <- matrix(0, xvars, 1)# for sd of all X vars
  weights.sum <- sum(weights)
  for (k in 1:xvars) {
    Mu.X[k, 1] <- sum(X[, k] * weights)/weights.sum             # mean X
    eps <- X[, k] - Mu.X[k, 1]                                  
    Sig.X[k, 1] <- sqrt(sum(X[, k] * X[, k] * weights)/weights.sum - 
                          Mu.X[k, 1]^2)
    Sig.X[k, 1] <- Sig.X[k, 1] * sqrt(N/(N - 1))
    if (Sig.X[k, 1] < tolerance) 
      Sig.X[k, 1] <- tolerance
    X[, k] = eps/Sig.X[k, 1]
  }
  ret <- list(Tr = Tr, X = X, All = All, M = M, N = N, ecaliper = ecaliper)
  return(ret)
}



