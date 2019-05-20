#### - Post matching
# MatchLoopC.indx contains the returned values in each column:
# 1) matched treated index
# 2) matched ctrl(s) index
# 3) weights
# 4) matched treated index
# 5) matched ctrl(s) index
# 6) caliper drops
# not sure what is the difference between cols 1 and 4, 2 and 5.


RMatchLoop_test <- function (id.var,MatchLoopC.indx,Y = Y, Tr = Tr, X = X, Z = Z, V, All = estimand, M=M,
                             BiasAdj = BiasAdj, Var.calc = Var.calc, weight = weight, SAMPLE = SAMPLE, 
                             ccc = ccc, version = version, ecaliper = NULL, exact = NULL, 
                             caliper = NULL, restrict = NULL, 
                             replace = TRUE, ties = TRUE,
                             MatchbyAI = FALSE) 
{
  N <- nrow(X)
  Kz <- ncol(Z)
  iot.t <- Tr * weight
  iot.c <- 1 - Tr
  
  indx <- MatchLoopC.indx
# if no matches returned?
  # if (indx[1, 1] == 0) {
  #   ret <- list()
  #   ret$valid <- 0
  #   if (caliperflag) {
  #     ret$sum.caliper.drops <- indx[1, 6]
  #   }
  #   else {
  #     ret$sum.caliper.drops <- 0
  #   }
  #   return(ret)
  # }
# only for ATC, switch around Tr and Ctrls  
  # if (All == 2) {
  #   foo <- indx[, 5]
  #   indx[, 5] <- indx[, 4]
  #   indx[, 4] <- foo
  # }
# for calipers (only necessary if VarCalc !=0)
  # if (caliperflag) {
  #   sum.caliper.drops <- indx[1, 6]
  # }
  # else {
  #   sum.caliper.drops <- 0
  # }
  
I <- indx[, 1] # matched treated index
IT <- Tr[indx[, 1]] # matched treatment indicator values (0/1s; generally all 1s, length=indx). perhaps neessary for ATC
IM <- indx[, 2] # matched ctrl(s) index
Yt <- Y[indx[, 4]] # matched treated outcome (don't know why this duplicates col 1 [using lalonde dataset])
Yc <- Y[indx[, 5]] # matched ctrl outcome (don't know why this duplicates col 2 [using lalonde dataset])
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
  Kcount <- as.matrix(rep(0, N))   # empty matrix for combined weights
  KKcount <- as.matrix(rep(0, N))  # empty matrix (vector) for combined  weights^2
  YCAUS <- matrix(0, nrow = N, ncol = 1)  # empty matrix for adjusted Y??
  if (BiasAdj == 1) {
    ZCAUS <- matrix(0, nrow = N, ncol = Kz)   # empty matrix for bias adj covariates
  }
  else {
    ZCAUS <- NULL
  }
  for (i in 1:N) {
    if ((Tr[i] == 1 & All != 1) | All == 1) {  # for each treated value
      foo.indx <- indx[, 1] == i # T/F vector to locate i in matched treated indices
      foo.indx2 <- foo.indx # back up foo.indx
      sum.foo <- sum(foo.indx) # number of times i is matched
      if (sum.foo < 1) # if 1 is not matched
        next   # stop loop and skip to the next one (i.e. ignores obs not incl in match)
      # foo = long way to index matched ctrl as T/F
      foo <- rep(FALSE, N)
      foo.indx <- indx[foo.indx, 2] # index for ctrls matched to i 
      foo[foo.indx] <- rep(TRUE, sum.foo) # T/F vector for location of matched ctrl obs 
      
      Kcount <- Kcount + weight[i] * weight * foo/sum(foo * 
                                                        weight) # (weights for ctrl matches for i ) X (weight for [i])/sum(weight [i]) - eq 2
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
            ZCAUS[i, ] <- as.matrix(Z[i, ] - t(Z[foo.indx2.2,]) %*% foo.indx2.3/sum.foo.indx2.3, ncol = Kz)     # Z(tr) less avg Z values for matches): eq4
          }
          else { # if 1 match
            ZCAUS[i, ] <- as.matrix(Z[i, ] - Z[foo.indx2.2, 
                                     ] * foo.indx2.3/sum.foo.indx2.3,ncol=Kz)
          }
        }
        else {
          if (sum.foo > 1) {
            ZCAUS[i, ] <- as.matrix(t(Z[foo.indx2.2, ]) %*% 
              foo.indx2.3/sum.foo.indx2.3 - Z[i, ],ncol=Kz)
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
# 1) YCAUS: treatment effect(adjusted in some way) 
# 2) ZCAUS: the adjusted difference in covariates (if BiasAdj==T)
# 3) Kcount: tr weights X ctrl weights (accounting for supplied tr weights and # times matched)
# 4) KKcount: tr weights X ctrl weights^2 (accounting for supplied tr weights and # times matched)


if (All != 1) {
  # If all Tr==1 then these are the same as  above
  I <- as.matrix(I[IT == 1])   # matched treated index values ( IT <- Tr[indx[, 1]] ) . 
  IT <- as.matrix(IT[IT == 1]) # matched treated index where Tr=1 (does nothing if all IT=1; maybe necessary for ATC?)
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
  # for controls (used for ATT)
  NNc <- nrow(Z) # length of orig covariates 
  ZZc <- cbind(matrix(1, nrow = NNc, ncol = 1), Z) # add a column of all 1s
  Kx <- ncol(ZZc) # covariates + 1
  xw <- ZZc * (sqrt((1 - Tr) * Kcount) %*% matrix(1, nrow = 1, # covariates times Kcount (tr weights and ctrl weights)
                                                  ncol = Kx)) # changes the Kcount from vector to a nrow(Kcount x Kx) matrix (columns identical) for multiplication
  # no idea what happens here
  foo <- min(eigen(t(xw) %*% as.matrix(xw), only.values = TRUE)$values) # min. eigenvalue of the covariates times Kcount (squared??)
  foo <- as.double(foo <= ccc) # is the min. eigen value less than or equal the tolerance?
  foo2 <- apply(xw, 2, sd) # get the std deviation of  covariates times Kcount 
  options(show.error.messages = FALSE)
  wout <- NULL # not sure but perhaps this is the correction factor, or maybe something like a regression output?
  try(wout <- solve(t(xw) %*% as.matrix(xw) + diag(Kx) * ccc * (foo) * 
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
  } else {
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
est <- t(W) %*% Tau.i/sum(W) # avg AT(T,E,C) estimate 

# variance 
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
} else {
  se = NULL
  se.cond = NULL
}
# if (!MatchbyAI) {
#   return(list(est = est, se = se, se.cond = se.cond, W = W, art.data = art.data, 
#               MatchLoopC = MatchLoopC.indx))
# }
# else {
#   if (Var.calc == 0) 
#     Sigs <- NULL
# }  

# output variables
index.treated <- indx[, 1]
index.control <- indx[, 2]
weights <- indx[, 3]
mest <- sum((Y[index.treated] - Y[index.control]) * weights)/sum(weights)
v1 <- Y[index.treated] - Y[index.control]
varest <- sum(((v1 - mest)^2) * weights)/(sum(weights) * 
                                            sum(weights))
se.standard <- sqrt(varest)
wnobs <- sum(weights)
t.stat = est/se
p.val = (1 - pnorm(abs(est/se))) * 2

ind.est <- data.frame(id.treat=id.var[index.treated],id.ctrl=id.var[index.control],est=Tau.i,weight=W)

return(list(est = est, est.noadj = mest, se = se, se.standard = se.standard,se.cond = se.cond, W = W,  art.data = art.data, 
            MatchLoopC = MatchLoopC.indx, YCAUS = YCAUS, Kcount = Kcount, index.treated=index.treated,index.control=index.control,
            KKcount = KKcount, Sigs = Sigs, Tau=Tau.i,ind.est=ind.est, t.stat=t.stat,p.val=p.val))
}