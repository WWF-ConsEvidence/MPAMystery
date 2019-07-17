
#.libPaths()
#.libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")
library(lfe)
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)


##Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 

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

#summary(DiD.data)

pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  


##############
##############using lfe (felm) for high dimensional FE DiD (similar to reghdfe)

###############
###############using lfe (felm) for high dimensional FE DiD (similar to reghdfe)

###1. FS
DiD.felm.FS.1 <- felm(FSIndex ~   n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear  | 0 | SettlementID,
                      data=DiD.data,exactDOF = TRUE)
summary(DiD.felm.FS.1)

test = update(DiD.felm.FS.1, MTIndex ~ .)


DiD.felm.FS.2 <- felm(FSIndex ~   n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)


###2. MT
DiD.felm.MT.1 <- felm(MTIndex ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear  | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)

DiD.felm.MT.2 <- felm(MTIndex ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)

###3. PA
DiD.felm.PA.1 <- felm(PAIndex ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear  | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)

DiD.felm.PA.2 <- felm(PAIndex ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)

###4. MA
DiD.felm.MA.1 <- felm(MAIndex ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear  | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)

DiD.felm.MA.2 <- felm(MAIndex ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)


###5. SE
DiD.felm.SE.1 <- felm(SERate ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear  | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)

DiD.felm.SE.2 <- felm(SERate ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF 
                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, 
                      data=DiD.data,exactDOF = TRUE)


stargazer(DiD.felm.FS.1, DiD.felm.FS.2, out = "R:/Gill/DLe/R-result/DiD-felm-FS", add.lines = list(c("Settlement FE","Yes","Yes"), c("Year FE","Yes","Yes"),c("MPAxYear FE", "No","Yes")))

stargazer(DiD.felm.FS.1, DiD.felm.FS.2, DiD.felm.MA.1,DiD.felm.MA.2,
          out = "R:/Gill/DLe/R-result/DiD-felm-seasCape.txt", type = "text", 
          add.lines = list(c("Settlement FE","Yes","Yes"), c("Year FE","Yes","Yes"),c("MPAxYear FE", "No","Yes")))

