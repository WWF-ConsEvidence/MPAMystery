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


att.significance <- function(outcomes, weights, conf, p.value, sig.test){
  
# compute estimates, AI SE, and p vals.
mpa.mean <- mean(outcomes$MPA.outcome)
mpa.sd <- sd(outcomes$MPA.outcome)
control.mean <- mean(outcomes$Control.outcome)
control.sd <- sd(outcomes$Control.outcome)
est <- sum((outcomes$MPA.outcome - outcomes$Control.outcome)*weights)/sum(weights)
v1 <- outcomes$MPA.outcome - outcomes$Control.outcome
varest <- sum(((v1 - est)^2) * weights)/(sum(weights) * sum(weights))
se.standard <- sqrt(varest)
t.stat <- est/se.standard
p.val <- (1-pnorm(abs(est/se.standard)))*2
n.1 <- length(outcomes$MPA.outcome)
n.2 <-length(outcomes$Control.outcome)
conf <- 0.95
x.smd.ci <-(ci.smd
            (n.1=n.1, n.2=n.2, 
              conf.level=conf, 
              smd=(smd(Group.1=outcomes$MPA.outcome,Group.2=outcomes$Control.outcome))))
u3 <- round((1 - pnorm(1,(x.smd.ci$smd*1 + 1),1)),3)


# assign impact typology

type <- ifelse(sig.test ==TRUE, (ifelse (p.val>=p.value, "mirror",
                                      (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))<0), "catalyze", 
                                              (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "magnify",
                                                      (ifelse((est>0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))<0), "buffer", 
                                                              (ifelse((est<0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))>0), "reverse", 
                                                                      (ifelse((est<0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "constrain","exacerbate")))))))))))),
            (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))<0), "catalyze", 
                    (ifelse((est>0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "magnify",
                            (ifelse((est>0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))<0), "buffer", 
                                    (ifelse(est==0, "mirror", 
                                            (ifelse((est<0 & (mean(outcomes$MPA.outcome))<0 & (mean(outcomes$Control.outcome))>0), "reverse", 
                                                    (ifelse((est<0 & (mean(outcomes$MPA.outcome))>0 & (mean(outcomes$Control.outcome))>0), "constrain", 
                                                            "exacerbate")))))))))))))


#create output matrix
  out <- matrix(c(mpa.mean, mpa.sd, control.mean, control.sd, est, se.standard, t.stat,p.val, x.smd.ci$Lower.Conf.Limit.smd,x.smd.ci$smd,x.smd.ci$Upper.Conf.Limit.smd, u3, type), nrow = 1, ncol = 13, byrow = TRUE,
                dimnames = list(c("1"),
                                c("mpa.mean", "mpa.sd", "control.mean","control.sd","est","se.standard","t.stat","p.val","lower.ci","smd", "upper.ci","u3", "impact.type")))
return(out)
}





 
