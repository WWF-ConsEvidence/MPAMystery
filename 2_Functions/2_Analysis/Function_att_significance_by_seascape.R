# ---
# code:  Compute ATT, Abadie Imbens SE, and p values (longitudinal data and fake the panel)
# author: Louise Glew, louise.glew@gmail.com; 
# created: February 2018
# modified: 

#---
#--- inputs:  
# outcomes:  matrix produced by outcome_ATT_method1 OR outcome_ATT_method2 functions 
# weights: optional weight matrix (if weighted treatment effects being computed)
# conf:  confidence intervals for standardized mean difference (default =0.95)
# sig.test: logical test for whether to consider significance of treatment effect when assigning impact typology.
# p.value:  p value to use if sig.test = TRUE

att.significance.by.seascape <- function(outcomes, HHData, weights = NULL, conf = NULL, sig.test = NULL, p.value = NULL) {
  
  
  # Dealing with optional weight matrix and conf level (defining a default if they were not defined in function call)
  pacman::p_load(Matching,MBESS,plyr,dplyr)
  
  weight.matrix <- 
    data.frame(w=ifelse(is.null(weights) == T,
                        rep(1, length(outcomes$MPA.outcome)),
                        weights))
  
  conf.level <-
    ifelse(is.null(conf) == T,
           0.95,
           conf)
  
  p.value <- 
    ifelse(is.null(p.value) == T,
           0.05,
           p.value)
  
  # Developing initial data frames 
  
  est.data <- 
    outcomes %>%
    dplyr::rename(HouseholdID = tr1tx) %>%
    left_join(HHData, by = "HouseholdID") %>%
    transmute(X=seq(1:length(HouseholdID)),
              MPA.outcome=MPA.outcome,
              Control.outcome=Control.outcome) %>%
    reshape2::melt(.,id.vars=c("X"),variable.name="Tr",value.name="Y") %>%
    mutate(Tr=ifelse(Tr=="MPA.outcome",1,0))
  
  match.out <- data.frame(est=NA,
                          se=NA,
                          nobs=NA)
  
  match.store <- Match(X=est.data$X,
                       Tr=est.data$Tr,
                       Y=est.data$Y,
                       exact=TRUE)
  
  match.out[1,"est"] <- match.store$est
  match.out[1,"se"] <- match.store$se
  match.out[1,"nobs"] <- match.store$nobs
  
  
  x <- 
    outcomes %>%
    dplyr::rename(HouseholdID = tr1tx) %>%
    left_join(HHData, by = "HouseholdID") %>%
    mutate(est=match.out$est,
           se=match.out$se,
           nobs=match.out$nobs) %>%
    cbind.data.frame(weight.matrix)
  
  
  # Calculating smd, ci, impact type, p.val & consolidating with est & se
  
  initial.results <- 
    summarise(x,
              mpa.mean = mean(MPA.outcome),
              mpa.sd = sd(MPA.outcome),
              control.mean = mean(Control.outcome),
              control.sd = sd(Control.outcome),
              est = mean(est),   #ATT calculated through Match function
              se = mean(se),   #Abadie Imbens standard error calculated through Match function
              p.val = (1 - pnorm(abs(est/se))) * 2,
              n.1 = length(MPA.outcome),
              n.2 = length(Control.outcome),
              lower.ci = ci.smd(n.1 = n.1,
                                n.2 = n.2,
                                conf.level = conf.level,
                                smd = smd(Group.1 = MPA.outcome,
                                          Group.2 = Control.outcome))$Lower.Conf.Limit.smd,
              smd = ci.smd(n.1 = n.1,
                           n.2 = n.2,
                           conf.level = conf.level,
                           smd = smd(Group.1 = MPA.outcome,
                                     Group.2 = Control.outcome))$smd,
              upper.ci = ci.smd(n.1 = n.1,
                                n.2 = n.2,
                                conf.level = conf.level,
                                smd = smd(Group.1 = MPA.outcome,
                                          Group.2 = Control.outcome))$Upper.Conf.Limit.smd,
              u3 = round((1 - pnorm(1, ((smd * 1) + 1), 1)), 3),
              impact.type = ifelse(p.val >= p.value, 
                                   "mirror",
                                   ifelse(is.null(sig.test) == F, 
                                          ifelse((est > 0 & 
                                                    mean(MPA.outcome) > 0 & 
                                                    mean(Control.outcome) < 0), 
                                                 "catalyze", 
                                                 ifelse((est > 0 & 
                                                           mean(MPA.outcome) > 0 & 
                                                           mean(Control.outcome) > 0), 
                                                        "magnify",
                                                        ifelse((est > 0 & 
                                                                  mean(MPA.outcome) < 0 & 
                                                                  mean(Control.outcome) < 0), 
                                                               "buffer", 
                                                               ifelse((est < 0 & 
                                                                         mean(MPA.outcome) < 0 & 
                                                                         mean(Control.outcome) > 0), 
                                                                      "reverse", 
                                                                      ifelse((est < 0 & 
                                                                                mean(MPA.outcome) > 0 & 
                                                                                mean(Control.outcome) > 0), 
                                                                             "constrain",
                                                                             "exacerbate"))))),
                                          ifelse((est > 0 & 
                                                    mean(MPA.outcome) > 0 & 
                                                    mean(Control.outcome) < 0), "catalyze", 
                                                 ifelse((est > 0 & 
                                                           mean(MPA.outcome) > 0 & 
                                                           mean(Control.outcome) > 0), "magnify",
                                                        ifelse((est > 0 & 
                                                                  mean(MPA.outcome) < 0 & 
                                                                  mean(Control.outcome) < 0), "buffer", 
                                                               ifelse(est == 0, "mirror", 
                                                                      ifelse((est < 0 & 
                                                                                mean(MPA.outcome) < 0 & 
                                                                                mean(Control.outcome) > 0), "reverse", 
                                                                             ifelse((est < 0 & 
                                                                                       mean(MPA.outcome) > 0 & 
                                                                                       mean(Control.outcome) > 0), "constrain", 
                                                                                    "exacerbate"))))))))
    )
  
  
  # Subsetting to final output data frame
  
  final.result <- 
    initial.results[,-which(names(initial.results) %in% c("varest","n.1","n.2"))]
  
  
  return(final.result)
}


# 
# att.significance <- function(outcomes, weights, conf, p.value, sig.test){
#   
# # compute estimates, AI SE, and p vals.
# mpa.mean <- mean(outcomes$MPA.outcome)
# mpa.sd <- sd(outcomes$MPA.outcome)
# control.mean <- mean(outcomes$Control.outcome)
# control.sd <- sd(outcomes$Control.outcome)
# est <- sum((outcomes$MPA.outcome - outcomes$Control.outcome)*weights)/sum(weights)
# v1 <- outcomes$MPA.outcome - outcomes$Control.outcome
# varest <- sum(((v1 - est)^2) * weights)/(sum(weights) * sum(weights))
# se.standard <- sqrt(varest)
# t.stat <- est/se.standard
# p.val <- (1-pnorm(abs(est/se.standard)))*2
# n.1 <- length(outcomes$MPA.outcome)
# n.2 <-length(outcomes$Control.outcome)
# conf <- 0.95
# x.smd.ci <-(ci.smd
#             (n.1=n.1, n.2=n.2, 
#               conf.level=conf, 
#               smd=(smd(Group.1=outcomes$MPA.outcome,Group.2=outcomes$Control.outcome))))
# u3 <- round((1 - pnorm(1,(x.smd.ci$smd*1 + 1),1)),3)
# 
# 
# # assign impact typology
# 
# type <- ifelse(sig.test ==TRUE, (ifelse (p.val>=p.value, "mirror",
#                                       (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))<0), "catalyze", 
#                                               (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "magnify",
#                                                       (ifelse((est>0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))<0), "buffer", 
#                                                               (ifelse((est<0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))>0), "reverse", 
#                                                                       (ifelse((est<0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "constrain","exacerbate")))))))))))),
#             (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))<0), "catalyze", 
#                     (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "magnify",
#                             (ifelse((est>0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))<0), "buffer", 
#                                     (ifelse(est==0, "mirror", 
#                                             (ifelse((est<0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))>0), "reverse", 
#                                                     (ifelse((est<0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "constrain", 
#                                                             "exacerbate")))))))))))))
# 
# 
# #create output matrix
#   out <- matrix(c(mpa.mean, mpa.sd, control.mean, control.sd, est, se.standard, t.stat,p.val, x.smd.ci$Lower.Conf.Limit.smd,x.smd.ci$smd,x.smd.ci$Upper.Conf.Limit.smd, u3, type), nrow = 1, ncol = 13, byrow = TRUE,
#                 dimnames = list(c("1"),
#                                 c("mpa.mean", "mpa.sd", "control.mean","control.sd","est","se.standard","t.stat","p.val","lower.ci","smd", "upper.ci","u3", "impact.type")))
# return(out)
# }
# 
# 
# 
# 
# 
#  
