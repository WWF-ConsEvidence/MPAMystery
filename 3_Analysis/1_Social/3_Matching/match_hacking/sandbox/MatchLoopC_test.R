MatchLoopC <- function (N, xvars, All, M, cdd, caliperflag, replace, ties, 
                        ww, Tr, Xmod, weights, CaliperVec, Xorig, restrict.trigger, 
                        restrict, DiagWeightMatrixFlag) 
{
  if (restrict.trigger) {
    restrict.nrow <- nrow(restrict)
  }
  else {
    restrict.nrow <- 0
  }
  ret <- .Call("MatchLoopC", as.integer(N), as.integer(xvars), 
               as.integer(All), as.integer(M), as.double(cdd), as.integer(caliperflag), 
               as.integer(replace), as.integer(ties), as.double(ww), 
               as.double(Tr), as.double(Xmod), as.double(weights), 
               as.double(CaliperVec), as.double(Xorig), as.integer(restrict.trigger), 
               as.integer(restrict.nrow), as.double(restrict), as.double(DiagWeightMatrixFlag), 
               PACKAGE = "Matching")
  return(ret)
}



MatchLoopCfast <-function (N, xvars, All, M, cdd, caliperflag, replace, ties, 
                           ww, Tr, Xmod, CaliperVec, Xorig, restrict.trigger, restrict, 
                           DiagWeightMatrixFlag) 
{
  if (restrict.trigger) {
    restrict.nrow <- nrow(restrict)
  }
  else {
    restrict.nrow <- 0
  }
  ret <- .Call("MatchLoopCfast", as.integer(N), as.integer(xvars), 
               as.integer(All), as.integer(M), as.double(cdd), as.integer(caliperflag), 
               as.integer(replace), as.integer(ties), as.double(ww), 
               as.double(Tr), as.double(Xmod), as.double(CaliperVec), 
               as.double(Xorig), as.integer(restrict.trigger), as.integer(restrict.nrow), 
               as.double(restrict), as.double(DiagWeightMatrixFlag), 
               PACKAGE = "Matching")
  return(ret)
}
