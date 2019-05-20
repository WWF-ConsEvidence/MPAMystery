library(Matching)
require(MBESS)
library(tidyverse)
data(lalonde)
head(lalonde)
summary(lalonde[lalonde$treat==1,])
summary(lalonde[lalonde$treat==0,])



Tr <- lalonde$treat
Y <- lalonde %>% mutate(re78=re78/1000) %>% pull(re78)
mean(Y)
X <- lalonde %>% select(age:married,re74,u74,re75,u75) %>% 
  mutate(re74=re74/1000,
         re75=re75/1000)
apply(X,2,mean)


M=1

#---- Mahalanobis
m2full<-Matching::Match(Y=Y, Tr,X, M=M, replace=T, ties=T, BiasAdjust = T)

w <- match.obj$weights
conf.level <- 0.95
i.treat<-match.obj$index.treated
i.control<-match.obj$index.control 

# Method 1
reg.data <- data.frame(cbind(X,Y))

# summary((lalonde[i.treat,"re78"]/1000)-(lalonde[i.control,"re78"]/1000))
# mean(lalonde[i.treat,"re78"]/1000)-mean(lalonde[i.control,"re78"]/1000)
sum((Y[i.treat] - Y[i.control]) * w)/sum(w) # no-adjusted ATT

# linear function predicting Y using the covariates from the matched ctrls 
reg.fit <- lm(Y ~ .,data=reg.data[i.control,],weights=w)
# summary(reg.fit)

# predict Y based on ctrl model
reg.data$MU.X <- predict(reg.fit,reg.data)
# summary(reg.data$MU.X)
# summary(reg.data[i.treat,"MU.X"]);summary(reg.data[i.control,"Y"])
# qqplot(reg.data[i.control,"MU.X"],reg.data[i.control,"Y"])
# qqplot(reg.data[i.treat,"MU.X"],reg.data[i.treat,"Y"])

# wrangle data to join Tr and Ctrl outcomes
dat<-as.data.frame(cbind(
  reg.data[i.treat,c("Y","MU.X")],
  reg.data[i.control,c("Y","MU.X")])) 
names(dat) <- c('Y.tr','mu1.x',"Y.Ct",'mu0.x')

# calc. response ratios
dat$v1 <- dat$Y.tr - dat$Y.Ct
test <- dat %>% 
     summarise(est=sum((Y.tr - Y.Ct) * w)/sum(w),
               est.adj=sum(((Y.tr-Y.Ct)-(mu1.x-mu0.x))* w)/sum(w),
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
                                                                                                                  mean(t.biomass) > 0 & 
                                                                                                                  mean(c.biomass) < 0), 
                                                                                                               "catalyze", 
                                                                                                               ifelse((est > 0 & 
                                                                                                                         mean(t.biomass) > 0 & 
                                                                                                                         mean(c.biomass) > 0), 
                                                                                                                      "magnify",
                                                                                                                      ifelse((est > 0 & 
                                                                                                                                mean(t.biomass) < 0 & 
                                                                                                                                mean(c.biomass) < 0), 
                                                                                                                             "buffer", 
                                                                                                                             ifelse((est < 0 & 
                                                                                                                                       mean(t.biomass) < 0 & 
                                                                                                                                       mean(c.biomass) > 0), 
                                                                                                                                    "reverse", 
                                                                                                                                    ifelse((est < 0 & 
                                                                                                                                              mean(t.biomass) > 0 & 
                                                                                                                                              mean(c.biomass) > 0), 
                                                                                                                                           "constrain",
                                                                                                                                           "exacerbate"))))),
                                                                                                        ifelse((est > 0 & 
                                                                                                                  mean(t.biomass) > 0 & 
                                                                                                                  mean(c.biomass) < 0), "catalyze", 
                                                                                                               ifelse((est > 0 & 
                                                                                                                         mean(t.biomass) > 0 & 
                                                                                                                         mean(c.biomass) > 0), "magnify",
                                                                                                                      ifelse((est > 0 & 
                                                                                                                                mean(t.biomass) < 0 & 
                                                                                                                                mean(c.biomass) < 0), "buffer", 
                                                                                                                             ifelse(est == 0, "mirror", 
                                                                                                                                    ifelse((est < 0 & 
                                                                                                                                              mean(t.biomass) < 0 & 
                                                                                                                                              mean(c.biomass) > 0), "reverse", 
                                                                                                                                           ifelse((est < 0 & 
                                                                                                                                                     mean(t.biomass) > 0 & 
                                                                                                                                                     mean(c.biomass) > 0), "constrain", 
                                                                                                                                                  "exacerbate"))))))))
     )


# Subsetting to final output data frame

final.result <- 
  initial.results[,-which(names(initial.results) %in% c("varest","n.2"))]




#---- Propensity Score
glm1<-glm(Tr~.,family=binomial, data=cbind(X,Tr))
glm1 <- glm(Tr ~ age + educ + black + hisp + married + re74 + u74 + re75 + u75, family = binomial, data = lalonde)
m2full<-Match(Y=Y, Tr,X=glm1$fitted,M=M,replace=T, ties=T, BiasAdjust = T)


summary(m2full)
m2full$est.noadj
w <- m2full$weights
i.treat<-m2full$index.treated
i.control<-m2full$index.control 
#m2full$mdata

# Method 1
reg.data <- cbind(X,Y)

# summary((lalonde[i.treat,"re78"]/1000)-(lalonde[i.control,"re78"]/1000))
# mean(lalonde[i.treat,"re78"]/1000)-mean(lalonde[i.control,"re78"]/1000)
sum((Y[i.treat] - Y[i.control]) * w)/sum(w) # no-adjusted ATT

# linear function predicting Y using the covariates from the matched ctrls 
reg.fit <- lm(Y ~ .,data=reg.data[i.control,])
# summary(reg.fit)

# predict Y based on ctrl model
reg.data$MU.X <- predict(reg.fit,reg.data)
# summary(reg.data$MU.X)
# summary(reg.data[i.treat,"MU.X"]);summary(reg.data[i.control,"Y"])
# qqplot(reg.data[i.control,"MU.X"],reg.data[i.control,"Y"])
# qqplot(reg.data[i.treat,"MU.X"],reg.data[i.treat,"Y"])

# wrangle data to join Tr and Ctrl outcomes
dat<-as.data.frame(cbind(
  reg.data[i.treat,c("Y","MU.X")],
  reg.data[i.control,c("Y","MU.X")])) 
names(dat) <- c('Y.tr','mu1.x',"Y.Ct",'mu0.x')

# calc. response ratios

test <- dat %>% 
  mutate(lnRR=sum((Y.tr - Y.Ct) * w)/sum(w),
         lnRR.adj=((Y.tr-Y.Ct)-(mu1.x-mu0.x))* w)
#summary(test)
#m2full$est.noadj-mean(test$mu0.x-test$mu1.x)
sum(test$lnRR.adj)/sum(w)
mean(test$lnRR)
m2full$est
m2full$est.noadj

test <- dat %>% 
  mutate(lnRR=((Y.tr - Y.Ct) * w/sum(w))) %>% 
  summarise_all(mean)
test$Y.tr-test$Y.Ct
sum(test$lnRR.adj1)


# calc. response ratios
dat <- dat %>% 
  group_by_at(vars(ECOID:source)) %>% 
  summarise(lnRR=mean(log(t.biomass)-log(c.biomass)),
            lnRR.adj=mean(log(t.biomass))-(sum(log(c.biomass)+(mu1.x-mu0.x))/length(c.biomass)),
            t.biomass=mean(t.biomass),
            c.biomass=mean(c.biomass))
