library(Matching)
library(rio)
library(dplyr)

# Test with default dataset
data(lalonde)
names(lalonde)
X <- lalonde %>% 
  dplyr::select(age,educ,black,hisp,married,re74,u74,re75,u75) %>% 
  mutate_at(vars(re74:u75),funs(./1000))
Y <- lalonde$re78/1000
Tr <- lalonde$treat
M <- 2
Z <- X
estimand <- 0 # ( 0 = ATT)
version <-  "standard"
BiasAdj <- T
weight <- as.double(rep(1,length(Y)))
ccc <- 1e-05 # default tolerance 
Var.calc <- 0
SAMPLE <- 0
id.var <- seq(1:nrow(lalonde))

#run match
match.test<-Matching::Match(Y=Y, Tr,X, M=M, replace=T, ties=T, BiasAdjust = BiasAdj, weights = weight,tolerance = ccc)
source("Rloop_edit.R")

m.out <- RMatchLoop_test(id.var=id.var,MatchLoopC.indx = match.test$MatchLoopC,
                         Y = Y, Tr = Tr, X = X, Z = Z,All = estimand, M = M, BiasAdj = BiasAdj, 
                         Var.calc = Var.calc, weight = weight, SAMPLE = SAMPLE, 
                         ccc = ccc, version = version)

# compare output
data.frame(func=c("Match output","Custom output"),
           est.noadj=c(match.test$est.noadj,m.out$est.noadj),
           est=c(match.test$est,m.out$est),
           AI.se=c(match.test$se,m.out$se),
           se.standard=c(match.test$se.standard,m.out$se.standard),
           se.cond=c(match.test$se.cond,m.out$se.cond))
summary(match.test)
paste0("Estimate: ",round(m.out$est,6));paste0("AI SE: ",round(m.out$se,6));paste0("t.stat: ",round(m.out$t.stat,6));paste0("p.val: ",round(m.out$p.val,6))

# Get individual bias adjusted ATTs (if unweighted)
est <- m.out$ind.est 
# sum((est$est*est$weight)/sum(est$weight))  # should be the same as match.test$est

##-------- Test with some real data
fish.dat <- import("fish_data_test.csv")

X <- fish.dat %>% 
  dplyr::select(SiteLat,SiteLong,Depth,EXPO,SHORE,MRKT,HPOP,SurveyYear,sstmin,CHLORO,DepthErr2,Exp_Error,
                Habitat,ISO3,Ecoregion,Source) %>% 
  mutate_at(vars(Habitat,ISO3,Ecoregion,Source),funs(as.integer(as.factor(.))))
Y <- log(fish.dat$sum_biomass)
Tr <- fish.dat$INSIDE_AFT_GIS
M <- 5
Z <- X
estimand <- 0 # ( 0 = ATT)
version <-  "standard"
BiasAdj <- T
weight <- as.double(rep(1,length(Y)))
ccc <- 1e-05 # default tolerance 
Var.calc <- 0
SAMPLE <- 0
id.var <- fish.dat$ECOID
#X <- X[,1:7]

#run match
match.test<-Matching::Match(Y=Y, Tr,X, M=M, replace=T, ties=T, BiasAdjust = BiasAdj, weights = weight,tolerance = ccc)
source("Rloop_edit.R")

m.out <- RMatchLoop_test(id.var=id.var,MatchLoopC.indx = match.test$MatchLoopC,
                         Y = Y, Tr = Tr, X = X, Z = Z,All = estimand, M = M, BiasAdj = BiasAdj, 
                         Var.calc = Var.calc, weight = weight, SAMPLE = SAMPLE, 
                         ccc = ccc, version = version)

# compare output
data.frame(func=c("Match output","Custom output"),
           est.noadj=c(match.test$est.noadj,m.out$est.noadj),
           est=c(match.test$est,m.out$est),
           AI.se=c(match.test$se,m.out$se),
           se.standard=c(match.test$se.standard,m.out$se.standard),
           se.cond=c(match.test$se.cond,m.out$se.cond))
summary(match.test)
paste0("Estimate: ",round(m.out$est,6));paste0("AI SE: ",round(m.out$se,6));paste0("t.stat: ",round(m.out$t.stat,6));paste0("p.val: ",round(m.out$p.val,6))

# Get individual bias adjusted ATTs (if unweighted)
est <- m.out$ind.est 
# sum((est$est*est$weight)/sum(est$weight))  # should be the same as match.test$est


# source("function_att_significance.R")
# att.significance(match.test)
