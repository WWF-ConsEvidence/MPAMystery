All = estimand
weight = weights
MatchLoopC.indx = NULL
# function (Y, Tr, X, Z, V, All, M, BiasAdj, Weight, Weight.matrix, 
#           Var.calc, weight, SAMPLE, ccc, cdd, ecaliper = NULL, exact = NULL, 
#           caliper = NULL, restrict = NULL, MatchLoopC.indx = NULL, 
#           weights.flag, replace = TRUE, ties = TRUE, version = "standard", 
#           MatchbyAI = FALSE) 
# {

# MatchGenoudStage1caliper Returns:
# 1) Tr: modified if using ATC
# 2) X: standardized X values (X-mean/sd); see eq 1
# 3) All: estimand
# 4) M: n.matches, corrected if M > n.ctrl or n.treated observations
# 5) N: n.obs
# 6) ecaliper: caliper that factors in exact matches, in sd units

  s1 <- MatchGenoudStage1caliper(Tr = Tr, X = X, All = All, 
                                 M = M, weights = weight, exact = exact, caliper = caliper, 
                                 distance.tolerance = cdd, tolerance = ccc)
  sum.caliper.drops <- 0
  X.orig <- X  # retain original X before standardizing
  
  
  
  if (is.matrix(restrict)) {
    restrict.trigger <- TRUE
  }
  else {
    restrict.trigger <- FALSE
  }
  
  # for ATC
  if (All == 2) 
    Tr <- 1 - Tr
  
  #ensure M is not grater than # matches
  if (All == 1) {
    M <- min(M, min(sum(Tr), sum(1 - Tr))) 
  }
  else {
    M <- min(M, sum(1 - Tr))
  }
  
  # not sure....
  if (All == 1) {
    iot.t <- weight 
    iot.c <- as.matrix(rep(1, length(Tr)))
  }
  else {
    iot.t <- Tr * weight
    iot.c <- 1 - Tr
  }
  
  # standardizing the X values (X-mean/sd); see eq 1
  N <- nrow(X)
  Kx <- ncol(X)
  Kz <- ncol(Z)
  Mu.X <- matrix(0, Kx, 1)
  Sig.X <- matrix(0, Kx, 1)
  for (k in 1:Kx) {
    Mu.X[k, 1] <- sum(X[, k] * weight)/sum(weight)
    eps <- X[, k] - Mu.X[k, 1]
    Sig.X[k, 1] <- sqrt(sum(X[, k] * X[, k] * weight)/sum(weight) - 
                          Mu.X[k, 1]^2)
    Sig.X[k, 1] <- Sig.X[k, 1] * sqrt(N/(N - 1))
    if (Sig.X[k, 1] < ccc) 
      Sig.X[k, 1] <- ccc
    X[, k] = eps/Sig.X[k, 1]
  }
  
  # standardizing V values (X-mean/sd) + more
  Mv <- ncol(V)
  Mu.V <- matrix(0, Mv, 1)
  Sig.V <- matrix(0, Mv, 1)
  for (j in 1:Mv) {
    Mu.V[j, 1] = (t(V[, j]) %*% weight)/sum(weight)
    sv <- sum(V[, j] * V[, j] * weight)/sum(weight) - Mu.V[j, 
                                                           1]^2
    if (sv > 0) {
      sv <- sqrt(sv)
    }
    else {
      sv <- 0
    }
    sv <- sv * sqrt(N/(N - 1))
    Sig.V[j, 1] <- sv
  }
  
  # weighting X covariates in matching algorithm. 1) inverse variances (default); 2) Mahalanobis dist; 3) user defined
  if (Weight == 1) {
    Weight.matrix = diag(Kx)
  }
  else if (Weight == 2) {
    if (min(eigen(t(X) %*% X/N, only.values = TRUE)$values) > 
        1e-07) {
      Weight.matrix = solve(t(X) %*% X/N)
    }
    else {
      Weight.matrix <- diag(Kx)
    }
  }
    if (min(eigen(Weight.matrix, only.values = TRUE)$values) <  
      ccc)                                                       # accounting for tolerance?
    Weight.matrix <- Weight.matrix + diag(Kx) * ccc
  ww <- chol(Weight.matrix)                                      #  product of a lower triangular matrix and its conjugate transpose
  if (is.null(s1$ecaliper)) {
    caliperflag <- 0
    use.ecaliper <- 0
  }
  else {
    caliperflag <- 1
    use.ecaliper <- s1$ecaliper
  }
  if (Kx > 1) {
    DiagWeightMatrixFlag <- as.double(sum((Weight.matrix != 
                                             diag(diag(Weight.matrix)))) == 0) # 0  if diag weight matrix not used?
  }
  else {
    DiagWeightMatrixFlag <- 1
  }
  
  # matching "magic" happens here- my guess is that it passes the parameters to a compiled C code
  
  if (is.null(MatchLoopC.indx)) {  
    if (weights.flag == TRUE) {
      MatchLoopC.indx <- MatchLoopC(N = s1$N, xvars = Kx, 
                                    All = s1$All, M = s1$M, cdd = cdd, caliperflag = caliperflag, 
                                    replace = replace, ties = ties, ww = ww, Tr = s1$Tr, 
                                    Xmod = s1$X, weights = weight, CaliperVec = use.ecaliper, 
                                    Xorig = X.orig, restrict.trigger = restrict.trigger, 
                                    restrict = restrict, DiagWeightMatrixFlag = DiagWeightMatrixFlag)
    }
    else {
      MatchLoopC.indx <- MatchLoopCfast(N = s1$N, xvars = Kx, 
                                        All = s1$All, M = s1$M, cdd = cdd, caliperflag = caliperflag, 
                                        replace = replace, ties = ties, ww = ww, Tr = s1$Tr, 
                                        Xmod = s1$X, CaliperVec = use.ecaliper, Xorig = X.orig, 
                                        restrict.trigger = restrict.trigger, restrict = restrict, 
                                        DiagWeightMatrixFlag = DiagWeightMatrixFlag)
    }
  }
  
  indx <- MatchLoopC.indx # contains index for matched treated (cols 1 & 4), ctrl (cols 2 & 5), and weights (col 3)
  #### - Post matching
  # indx contains the returned values in each column:
  # 1) matched treated index
  # 2) matched ctrl(s) index
  # 3) weights
  # 4) matched treated index
  # 5) matched ctrl(s) index
  # 6) caliper drops
  # not sure what is the difference between cols 1 and 4, 2 and 5.
  
  # if no matches returned?
   if (indx[1, 1] == 0) {  
    ret <- list()
    ret$valid <- 0
    if (caliperflag) {
      ret$sum.caliper.drops <- indx[1, 6]
    }
    else {
      ret$sum.caliper.drops <- 0
    }
    return(ret)
   }
  
 # only for ATC, switch around Tr and Ctrls  
  if (All == 2) {    
    foo <- indx[, 5]
    indx[, 5] <- indx[, 4]
    indx[, 4] <- foo
  }
  
  #  col 6 contains caliper drops (missing if none?)
  if (caliperflag) {
    sum.caliper.drops <- indx[1, 6]   
  }
  else {
    sum.caliper.drops <- 0
  }
  
  I <- indx[, 1] # matched treated index
  IT <- Tr[indx[, 1]] # matched treatment indicator values (0/1s; generally all 1s, length=indx). perhaps neessary for ATC
  IM <- indx[, 2] # matched ctrl(s) index
  Yt <- Y[indx[, 4]] # matched treated outcome
  Yc <- Y[indx[, 5]] # matched ctrl outcome
  W <- indx[, 3] # weights
  if (BiasAdj == 1 & sum(W) < ncol(Z)) {
    warning("Fewer (weighted) matches than variables in 'Z': BiasAdjust set to FALSE")
    BiasAdj = 0
  }
  if (BiasAdj == 1) {
    if (sum(W) < ncol(Z)) {
      warning("Fewer matches than variables for Bias Adjustment")
    }
    IZ <- Z[indx[, 1], ] # matched treated covariates (for bias adj)
    Zt <- Z[indx[, 4], ] # matched treated covariates (for bias adj)
    Zc <- Z[indx[, 5], ] # matched ctrl covariates (for bias adj)
  }
  est.func <- function(N, All, Tr, indx, weight, BiasAdj, 
                       Kz) {
    Kcount <- as.matrix(rep(0, N))   # empty matrix for weights
    KKcount <- as.matrix(rep(0, N))  # empty matrix (vector) for weights^2
    YCAUS <- matrix(0, nrow = N, ncol = 1)  # empty matrix for 
    if (BiasAdj == 1) {
      ZCAUS <- matrix(0, nrow = N, ncol = Kz)   # empty matrix for bias adj covariates
    }
    else {
      ZCAUS <- NULL
    }
    for (i in 1:N) {
      if ((Tr[i] == 1 & All != 1) | All == 1) {  # for each treated value
        foo.indx <- indx[, 1] == i # T/F vector to locate i in matched treated list
        foo.indx2 <- foo.indx # back up foo.indx
        sum.foo <- sum(foo.indx) # number of times i is matched
        if (sum.foo < 1) # if 1 is not matched
          next   # stop loop and skip to the next one (i.e. ignores obs not incl in match)
        # foo = long way to index matched ctrl (T/F)
        foo <- rep(FALSE, N)
        foo.indx <- indx[foo.indx, 2] # index for ctrls matched to i 
        foo[foo.indx] <- rep(TRUE, sum.foo) # T/F vector for location of matched ctrl obs 
        
        Kcount <- Kcount + weight[i] * weight * foo/sum(foo * 
                                                          weight) # weights for ctrl matches for i times weight - eq 2
        KKcount <- KKcount + weight[i] * weight * weight * 
          foo/(sum(foo * weight) * sum(foo * weight)) # weights^2 for ctrl matches for i times weight[i]
        foo.indx2.2 <- indx[foo.indx2, 2] # index for ctrl matched to obs i (same as foo.indx?? line 222)
        foo.indx2.3 <- indx[foo.indx2, 3] # weights for ctrl matched to obs i 
        sum.foo.indx2.3 <- sum(foo.indx2.3) # sum of weights for ctrl matches
        if (Tr[i] == 1) {   # if treated
          YCAUS[i] <- Y[i] - sum((Y[foo.indx2.2] * foo.indx2.3))/sum.foo.indx2.3  # Y(tr) less avg Y of matches
        }
        else { # if ctrl site
          YCAUS[i] <- sum((Y[foo.indx2.2] * foo.indx2.3))/sum.foo.indx2.3 -       # avg Y of matches less Y(tr): eq3
            Y[i]
        }
        if (BiasAdj == 1) {
          if (Tr[i] == 1) {
            if (sum.foo > 1) {  # if >1 matches
              ZCAUS[i, ] <- Z[i, ] - t(Z[foo.indx2.2, 
                                         ]) %*% foo.indx2.3/sum.foo.indx2.3      # Z(tr) less avg Z values for matches): eq4
            }
            else { # if 1 match
              ZCAUS[i, ] <- Z[i, ] - Z[foo.indx2.2, 
                                       ] * foo.indx2.3/sum.foo.indx2.3
            }
          }
          else {
            if (sum.foo > 1) {
              ZCAUS[i, ] <- t(Z[foo.indx2.2, ]) %*% 
                foo.indx2.3/sum.foo.indx2.3 - Z[i, ]
            }
            else {
              ZCAUS[i, ] <- Z[foo.indx2.2, ] * foo.indx2.3/sum.foo.indx2.3 - 
                Z[i, ]
            }
          }
        }
      }
    }
    return(list(YCAUS = YCAUS, ZCAUS = ZCAUS, Kcount = Kcount, 
                KKcount = KKcount))
  }
  
  if (version == "standard" & BiasAdj == 0) {
    ret <- .Call("EstFuncC", as.integer(N), as.integer(All), 
                 as.integer(nrow(indx)), as.double(Y), as.double(Tr), 
                 as.double(weight), as.double(indx), PACKAGE = "Matching")
    YCAUS <- ret[, 1]
    Kcount <- ret[, 2]
    KKcount <- ret[, 3]
  }
  else if (version == "standard") {
    ret.est <- est.func(N = N, All = All, Tr = Tr, indx = indx, 
                        weight = weight, BiasAdj = BiasAdj, Kz = Kz)
    YCAUS <- ret.est$YCAUS 
    ZCAUS <- ret.est$ZCAUS
    Kcount <- ret.est$Kcount
    KKcount <- ret.est$KKcount
  }
# returns list of 4 values:
# 1) YCAUS: treatment effect 
# 2) ZCAUS: the adjusted difference in covariates (if BiasAdj==T)
# 3) Kcount: tr weights X ctrl weights (accounting for supplied tr weights and # times matched)
# 4) KKcount: tr weights X ctrl weights^2 (accounting for supplied tr weights and # times matched)
  
  
  if (All != 1) {
    # If all Tr==1 then these are the same as  above
    I <- as.matrix(I[IT == 1])  # matched treated index values ( IT <- Tr[indx[, 1]] ) . 
    IT <- as.matrix(IT[IT == 1])  # matched treated index where Tr=1 (does nothing if all IT=1; maybe necessary for ATC?)
    Yc <- as.matrix(Yc[IT == 1]) #  matched ctrl outcome where Tr=1
    Yt <- as.matrix(Yt[IT == 1]) #  matched treat outcome where Tr=1
    W <- as.matrix(W[IT == 1]) # weights where Tr=1
    if (BiasAdj == 1) {
      if (Kz > 1) { #if multiple covariates used for bias adjustment
        Zc <- as.matrix(Zc[IT == 1, ]) # matched ctrl covariates (for bias adj)
        Zt <- as.matrix(Zt[IT == 1, ]) # matched treated covariates (for bias adj)
        IZ <- as.matrix(IZ[IT == 1, ]) # matched index  (for bias adj)
      }
      else {
        Zc <- as.matrix(Zc[IT == 1])
        Zt <- as.matrix(Zt[IT == 1])
        IZ <- as.matrix(IZ[IT == 1])
      }
    }
  }
  if (length(I) < 1) {
    return(list(sum.caliper.drops = N))
  }
  if (BiasAdj == 1) {
    # used for ATE, skip to below 
    if (All == 1) { 
      NNt <- nrow(Z)  # length of orig covariates
      ZZt <- cbind(rep(1, NNt), Z) # add a column of all 1s
      Kx <- ncol(ZZt) # covariates + 1
      xw <- ZZt * (sqrt(Tr * Kcount) %*% t(as.matrix(rep(1,     # covariates times Kcount (tr weights and ctrl weights)
                                                         Kx)))) # changes the Kcount from vector to a (Kcount x Kx) matrix (columns identical) for multiplication
      foo <- min(eigen(t(xw) %*% xw, only.values = TRUE)$values)
      foo <- as.double(foo <= ccc)
      foo2 <- apply(xw, 2, sd)
      options(show.error.messages = FALSE)
      wout <- NULL
      try(wout <- solve(t(xw) %*% xw + diag(Kx) * ccc * 
                          (foo) * max(foo2)) %*% (t(xw) %*% (Y * sqrt(Tr * 
                                                                        Kcount))))
      if (is.null(wout)) {
        wout2 <- NULL
        try(wout2 <- ginv(t(xw) %*% xw + diag(Kx) * 
                            ccc * (foo) * max(foo2)) %*% (t(xw) %*% (Y * 
                                                                       sqrt(Tr * Kcount))))
        if (!is.null(wout2)) {
          wout <- wout2
          warning("using generalized inverse to calculate Bias Adjustment probably because of singular 'Z'")
        }
      }
      options(show.error.messages = TRUE)
      if (is.null(wout)) {
        warning("unable to calculate Bias Adjustment probably because of singular 'Z'")
        BiasAdj <- 0
      }
      else {
        NW <- nrow(wout) # covariates + 1
        Alphat <- wout[2:NW, 1] # remove the column of 1s that was added in line 263. Length should =Kz now
      }
    }
    else {
      Alphat <- matrix(0, nrow = Kz, ncol = 1) # this applies to ATT
    }
  }
  if (BiasAdj == 1) {
    # same as above, but for controls (used for ATT)
    NNc <- nrow(Z) # length of orig covariates 
    ZZc <- cbind(matrix(1, nrow = NNc, ncol = 1), Z) # add a column of all 1s
    Kx <- ncol(ZZc) # covariates + 1
    xw <- ZZc * (sqrt((1 - Tr) * Kcount) %*% matrix(1, nrow = 1, # covariates times Kcount (tr weights and ctrl weights)
                                                    ncol = Kx)) # changes the Kcount from vector to a nrow(Kcount x Kx) matrix (columns identical) for multiplication
    # no idea what happens here
    foo <- min(eigen(t(xw) %*% xw, only.values = TRUE)$values) # min. eigenvalue of the covariates times Kcount (squared??)
    foo <- as.double(foo <= ccc) # is the min. eigen value less than or equal the tolerance?
    foo2 <- apply(xw, 2, sd) # get the std deviation of  covariates times Kcount 
    options(show.error.messages = FALSE)
    wout <- NULL # not sure but perhaps this is the correction factor, or maybe something like a regression output?
    try(wout <- solve(t(xw) %*% xw + diag(Kx) * ccc * (foo) * 
                        max(foo2)) %*% (t(xw) %*% (Y * sqrt((1 - Tr) * Kcount))))
    if (is.null(wout)) {
      wout2 <- NULL
      try(wout2 <- ginv(t(xw) %*% xw + diag(Kx) * ccc * 
                          (foo) * max(foo2)) %*% (t(xw) %*% (Y * sqrt((1 - 
                                                                         Tr) * Kcount))))
      if (!is.null(wout2)) {
        wout <- wout2
        warning("using generalized inverse to calculate Bias Adjustment probably because of singular 'Z'")
      }
    }
    
    options(show.error.messages = TRUE)
    if (is.null(wout)) {
      warning("unable to calculate Bias Adjustment probably because of singular 'Z'")
      BiasAdj <- 0
    }
    else {
      NW <- nrow(wout)  #  #covariates + 1
      Alphac <- as.matrix(wout[2:NW, 1])  # remove the column of 1s that was added in line 263. Length should =Kz now
    }
  }
  if (BiasAdj == 1) {
    IZ <- as.matrix(IZ)
    Zc <- as.matrix(Zc)
    Zt <- as.matrix(Zt)
    Alphat <- as.matrix(Alphat)
    SCAUS <- YCAUS - Tr * (ZCAUS %*% Alphac) - (1 - Tr) * # eq 6
      (ZCAUS %*% Alphat)
    Yc.adj <- Yc + BiasAdj * (IZ - Zc) %*% Alphac  # eq 7
    Yt.adj <- Yt + BiasAdj * (IZ - Zt) %*% Alphat
    Tau.i <- Yt.adj - Yc.adj  # bias-adjusted treatment effect
  }
  else {
    Yc.adj <- Yc
    Yt.adj <- Yt
    Tau.i <- Yt.adj - Yc.adj
  }
  art.data <- cbind(I, IM) # index of treated and ctrls
  if (Var.calc > 0) { # if Var.calc supplied
    Sigs <- VarCalcMatchC(N = N, xvars = ncol(X), Var.calc = Var.calc, 
                          cdd = cdd, caliperflag = caliperflag, ww = ww, Tr = Tr, 
                          Xmod = s1$X, CaliperVec = use.ecaliper, Xorig = X.orig, 
                          restrict.trigger = restrict.trigger, restrict = restrict, 
                          DiagWeightMatrixFlag = DiagWeightMatrixFlag, Y = Y, 
                          weightFlag = weights.flag, weight = weight)
  }
  est <- t(W) %*% Tau.i/sum(W) # avg estimate 
  if (version == "standard") {
    if (Var.calc == 0) {
      eps <- Tau.i - as.double(est)   # est - mean(est) eq 8
      eps.sq <- eps * eps
      Sigs <- 0.5 * matrix(1, N, 1) %*% (t(eps.sq) %*%  # eq 9
                                           W)/sum(W)
    }
    SN <- sum(iot.t)  # iot.t= Tr * weight
    var.sample <- sum((Sigs * (iot.t + iot.c * Kcount) *    # iot.t + iot.c basically gives you weights, SN is the sum of the weights
                         (iot.t + iot.c * Kcount))/(SN * SN)) # eq 10
    if (All == 1) {
      var.pop <- sum((Sigs * (iot.c * Kcount * Kcount + 
                                2 * iot.c * Kcount - iot.c * KKcount))/(SN * 
                                                                          SN))
    }
    else {
      var.pop = sum((Sigs * (iot.c * Kcount * Kcount - 
                               iot.c * KKcount))/(SN * SN)) # eq 11
    }
    if (BiasAdj == 1) {
      dvar.pop <- sum(iot.t * (SCAUS - as.double(est)) * 
                        (SCAUS - as.double(est)))/(SN * SN) # eq 12
    }
    else {
      dvar.pop <- sum(iot.t * (YCAUS - as.double(est)) * 
                        (YCAUS - as.double(est)))/(SN * SN)
    }
    var.pop <- var.pop + dvar.pop
    if (SAMPLE == 1) {
      var <- var.sample
    }
    else {
      var <- var.pop
    }
    var.cond <- max(var.sample, var.pop) - var.sample # eq 13
    se <- sqrt(var)
    se.cond <- sqrt(var.cond)
  }
  else {
    se = NULL
    se.cond = NULL
  }
  if (All == 2) 
    est <- -1 * est
  if (!MatchbyAI) {
    return(list(est = est, se = se, se.cond = se.cond, W = W, 
                sum.caliper.drops = sum.caliper.drops, art.data = art.data, 
                MatchLoopC = MatchLoopC.indx))
  }
  else {
    if (Var.calc == 0) 
      Sigs <- NULL
    return(list(est = est, se = se, se.cond = se.cond, W = W, 
                sum.caliper.drops = sum.caliper.drops, art.data = art.data, 
                MatchLoopC = MatchLoopC.indx, YCAUS = YCAUS, Kcount = Kcount, 
                KKcount = KKcount, Sigs = Sigs))
  }
}
