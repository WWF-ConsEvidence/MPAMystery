# ---
# code:  Compute ATT, Abadie Imbens SE, and p values 
# author: Louise Glew, louise.glew@gmail.com; Kelly Claborn, modified by David Gill
# created: February 2018. modified Oct 2018
# modified: 

#---
#--- inputs:  
# data:  matched data 
# weights: optional weight matrix (if weighted treatment effects being computed)
# grouping_var: optional group_by variable(s) -- if more than one grouping variable, identify them in concatenated strings 
#               e.g., grouping_var = c("MPAID","GenderHHH")
# conf:  confidence intervals for standardized mean difference (default = 0.95)
# sig.test: logical test for whether to consider significance of treatment effect when assigning impact typology (default = FALSE)
# p.value:  p value to use if sig.test = TRUE (default = 0.05)
# 
# grouping_var="MPAID"
# outcomes <- matcheddata %>% 
#   select(MPAID,Y.tr,Y.Ct)
# weights <- NULL
# conf <- 0.95
# sig.test <- 0.05
# p.value <- 0.05
# sig.test <- NULL
# att.significance(matcheddata,grouping_var = "MPAID")
att.significance <- function(data, match.obj=match.obj, grouping_var = NULL, conf = NULL, sig.test = NULL, p.value = NULL) {
  require(MBESS)
  # Dealing with  conf level, pval, etc (defining a default if they were not defined in function call)
  w <- match.obj$weights
  i.treat<-match.obj$index.treated
  i.control<-match.obj$index.control 
  
  conf.level <-
    ifelse(is.null(conf) == T,
           0.95,
           conf)
  
  p.value <- 
    ifelse(is.null(p.value) == T,
           0.05,
           p.value)
  
  reg.data <- data.frame(cbind(X,Y))
  # linear function predicting Y using the covariates from the matched ctrls 
  reg.fit <- lm(Y ~ .,data=reg.data[i.control,],weights=w)
  reg.data$MU.X <- predict(reg.fit,reg.data)  # predict Y based on ctrl model
  
  # wrangle data to join Tr and Ctrl outcomes
  dat<-as.data.frame(cbind(
    reg.data[i.treat,c("Y","MU.X")],
    reg.data[i.control,c("Y","MU.X")])) 
  names(dat) <- c('Y.tr','mu1.x',"Y.Ct",'mu0.x')
  
  dat$v1 <- dat$Y.tr - dat$Y.Ct
  
  
  # Calculating est, AI se, impact type, etc. 
  initial.results <-  dat %>% 
    summarise(est.noadj=sum((Y.tr - Y.Ct) * w)/sum(w),
              est=sum(((Y.tr-Y.Ct)-(mu1.x-mu0.x))* w)/sum(w),
              varest=sum(((v1 - est)^2) * w)/(sum(w) * sum(w)),
              se.standard = sqrt(varest),
              t.stat = est/se.standard,
              p.val = (1 - pnorm(abs(est/se.standard))) * 2,
              n.1 = length(Y.tr),
              n.2 = length(Y.Ct),
              lower.ci = ci.smd(n.1 = n.1,
                                n.2 = n.2,
                                conf.level = conf.level,
                                smd = smd(Group.1 = Y.tr,
                                          Group.2 = Y.Ct))$Lower.Conf.Limit.smd,
              smd = ci.smd(n.1 = n.1,
                           n.2 = n.2,
                           conf.level = conf.level,
                           smd = smd(Group.1 = Y.tr,
                                     Group.2 = Y.Ct))$smd,
              upper.ci = ci.smd(n.1 = n.1,
                                n.2 = n.2,
                                conf.level = conf.level,
                                smd = smd(Group.1 = Y.tr,
                                          Group.2 = Y.Ct))$Upper.Conf.Limit.smd,
              u3 = round((1 - pnorm(1, ((smd * 1) + 1), 1)), 3),              impact.type = ifelse(p.val >= p.value, 
                                                                                                   "mirror",
                                                                                                   ifelse(is.null(sig.test) == F, 
                                                                                                          ifelse((est > 0 & 
                                                                                                                    mean(Y.tr) > 0 & 
                                                                                                                    mean(Y.Ct) < 0), 
                                                                                                                 "catalyze", 
                                                                                                                 ifelse((est > 0 & 
                                                                                                                           mean(Y.tr) > 0 & 
                                                                                                                           mean(Y.Ct) > 0), 
                                                                                                                        "magnify",
                                                                                                                        ifelse((est > 0 & 
                                                                                                                                  mean(Y.tr) < 0 & 
                                                                                                                                  mean(Y.Ct) < 0), 
                                                                                                                               "buffer", 
                                                                                                                               ifelse((est < 0 & 
                                                                                                                                         mean(Y.tr) < 0 & 
                                                                                                                                         mean(Y.Ct) > 0), 
                                                                                                                                      "reverse", 
                                                                                                                                      ifelse((est < 0 & 
                                                                                                                                                mean(Y.tr) > 0 & 
                                                                                                                                                mean(Y.Ct) > 0), 
                                                                                                                                             "constrain",
                                                                                                                                             "exacerbate"))))),
                                                                                                          ifelse((est > 0 & 
                                                                                                                    mean(Y.tr) > 0 & 
                                                                                                                    mean(Y.Ct) < 0), "catalyze", 
                                                                                                                 ifelse((est > 0 & 
                                                                                                                           mean(Y.tr) > 0 & 
                                                                                                                           mean(Y.Ct) > 0), "magnify",
                                                                                                                        ifelse((est > 0 & 
                                                                                                                                  mean(Y.tr) < 0 & 
                                                                                                                                  mean(Y.Ct) < 0), "buffer", 
                                                                                                                               ifelse(est == 0, "mirror", 
                                                                                                                                      ifelse((est < 0 & 
                                                                                                                                                mean(Y.tr) < 0 & 
                                                                                                                                                mean(Y.Ct) > 0), "reverse", 
                                                                                                                                             ifelse((est < 0 & 
                                                                                                                                                       mean(Y.tr) > 0 & 
                                                                                                                                                       mean(Y.Ct) > 0), "constrain", 
                                                                                                                                                    "exacerbate"))))))))
    )
  
  
  # Subsetting to final output data frame
  
  final.result <- initial.results %>% 
    dplyr::select(-varest,-n.2)
  
  return(final.result)
}






