#.libPaths()
#.libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")
library(lfe)
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)
library(tidyverse)
library(qvalue)
#install.packages(qvalue)

# #------------Installing qvalue package
# library(devtools)
# install_github("jdstorey/qvalue")
# #------------Installing qvalue package
# 

##Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
## Seascape level Impact
## Seascape level Impact
## Seascape level Impact
## Seascape level Impact
## Seascape level Impact


source('R:/Gill/DLe/MPAMystery/2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")


# --- DiD specification 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  filter(MPAID<=6) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear)) %>% 
  filter(!is.na(IndividualGender))


# calculate Z scores (standardized values for each of the Big Five)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  

###############
###############using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out <- data.frame()

regValue.list <-list()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                   | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                   data=DiD.data,exactDOF = TRUE)
  
  regValue.list[[i]] <- regValue
}

summary(regValue.list)

#########produce publishable tables
##save texts
stargazer(regValue.list[1:5],
          out = "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/result_tables/DiD-seascape.txt", type = "text",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))



##save htmls
stargazer(regValue.list[1:5],
          out = "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/result_tables/DiD-seascape.html", type = "html",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))








# ##-------getting into q-value for multiple inference tests
# 
# # Filtering to get just the treatXpost terms for t2; outcomes are BigFive_z
# model.out.z.t2 <- model.out %>% 
#   filter(term%in%c("Treatment:yearsPostF2")) %>% 
#   filter(Response%in%c("PAIndex_z","FSIndex_z","MAIndex_z","MTIndex_z","SERate_z")) %>% 
#   mutate(term=gsub("Treatment:yearsPostF","Treat_Post",term)) 
# 
# # Filtering to get just the treatXpost terms for t4; outcomes are BigFive_z
# model.out.z.t4 <- model.out %>% 
#   filter(term%in%c("Treatment:yearsPostF4")) %>% 
#   filter(Response%in%c("PAIndex_z","FSIndex_z","MAIndex_z","MTIndex_z","SERate_z")) %>% 
#   mutate(term=gsub("Treatment:yearsPostF","Treat_Post",term)) 
# 
# 
# # ----Check p value histogram ----
# #hist(model.out1$p.value, nclass = 10)
# 
# 
# # ----Calculate q values ----
# all.qvalues.BigFive_z.t2 <- qvalue(model.out.BigFive_z.t2$p.value, pi0 = 1)
# all.qvalues.BigFive_z.t4 <- qvalue(model.out.BigFive_z.t4$p.value, pi0 = 1)
# 
# 
# ##Among all the significant tests (i.e. the ones with p-val<=0.05), what is the q-value associated with the least signf. one (i.e. largest p-val)?
# max(all.qvalues.BigFive_z.t2$qvalues[all.qvalues.BigFive_z.t2$pvalues<=0.05])
# max(all.qvalues.BigFive_z.t4$qvalues[all.qvalues.BigFive_z.t4$pvalues<=0.05])
# 
# ##merge the qvalues back in model.out1
# model.out.BigFive_z.t2 <- model.out.BigFive_z.t2 %>%
#   mutate(qvalues=all.qvalues.BigFive_z.t2$qvalues) %>%
#   arrange(p.value)
# 
# model.out.BigFive_z.t4 <- model.out.BigFive_z.t4 %>%
#   mutate(qvalues=all.qvalues.BigFive_z.t4$qvalues) %>%
#   arrange(p.value)
# 
# 
# head(model.out.BigFive_z.t2)
# head(model.out.BigFive_z.t4)
# 
# hist(model.out.BigFive_z.t4$p.value)
