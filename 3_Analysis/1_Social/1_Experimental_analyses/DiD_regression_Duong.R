#.libPaths()
#.libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")

####---DID/DIDID regression analysis for BHS---########
####---Outcomes: Big Fives---------------########
####---1. Seascape-level impacts
####---2. Individual MPA-level impacts
####---3. SubGroup Heterogeneity (Triple Difference; Seascape-level)
####------3.1 Fisher vs. non-Fisher
####------3.2 Male vs. Female
####------3.3 Dominant vs. non-dominant ethnic groups
####------3.4 By wealth quintiles


source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
outPath <- "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/"

library(lfe)
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)
library(tidyverse)
library(qvalue)
#install.packages(qvalue)

# #------------Installing qvalue package 
# install.packages(devtools)
# library(devtools)
# install_github("jdstorey/qvalue")
# #------------Installing qvalue package


##--------1. Seascape level Impact
##--------1. Seascape level Impact
##--------1. Seascape level Impact
##--------1. Seascape level Impact
###Define output directory path

# --- DiD specification 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  filter(MPAID<=6) %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"Telma",
                                ifelse(MPAID==2,"TNTC",
                                       ifelse(MPAID==3,"Kaimana",
                                              ifelse(MPAID==4,"Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",""))))))) %>% 
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

###############Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

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

###------------Output result tables for BigFive
##save texts
stargazer(regValue.list[1:5],
          out = paste0(outPath,"BHS/result_tables/DiD-seascape.txt"), type = "text",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))


##save htmls
stargazer(regValue.list[1:5],
          out = paste0(outPath,"BHS/result_tables/DiD-seascape.html"), type = "html",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))



###------------Output result tables for BigFive_z (standardized)
##save texts
stargazer(regValue.list[6:10],
          out = paste0(outPath,"BHS/result_tables/DiD-seascape-z.txt"), type = "text",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[6:10], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))



##save htmls
stargazer(regValue.list[6:10],
          out = paste0(outPath,"BHS/result_tables/DiD-seascape-z.html"), type = "html",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[6:10], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))

##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##

##--------2. MPA-level Impact
##--------2. MPA-level Impact
##--------2. MPA-level Impact
##--------2. MPA-level Impact

###############Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
MPA_name <- c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool")
##---loop through each MPAID to generate independent table outputs
for (mpa in MPA_name) {
  regValue.list <-list()
  print(mpa)
  DiD.data.mpalevel <- DiD.data %>% filter(MPAName_short==mpa)

  for (i in varNames) {
    print(i)
    Y <- DiD.data.mpalevel[,i]
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                     | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                     data=DiD.data.mpalevel,exactDOF = TRUE)
    
    regValue.list[[i]] <- regValue
  }
  ###------------Output result tables for BigFive for each MPA
  ##save texts
  stargazer(regValue.list[1:5],
            out = paste0(outPath,"BHS/result_tables/DiD-MPA-",mpa,".txt"), type = "text",
            keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
            title=paste0("DiD Regression Results",mpa),
            align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
            add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))
  
  ##save htmls
  stargazer(regValue.list[1:5],
            out = paste0(outPath,"BHS/result_tables/DiD-MPA-",mpa,".html"), type = "html",
            keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
            title=paste0("DiD Regression Results",mpa),
            align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
            add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))
  
  
  
  ###------------Output result tables for BigFive_z (standardized) for each MPA
  ##save texts
  stargazer(regValue.list[6:10],
            out = paste0(outPath,"BHS/result_tables/DiD-MPA-",mpa,"-z.txt"), type = "text",
            keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
            title=paste0("DiD Regression Results",mpa),
            align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
            add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))
  
  ##save htmls
  stargazer(regValue.list[6:10],
            out = paste0(outPath,"BHS/result_tables/DiD-MPA-",mpa,"-z.html"), type = "html",
            keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
            title=paste0("DiD Regression Results",mpa),
            align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
            add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))
  

}

