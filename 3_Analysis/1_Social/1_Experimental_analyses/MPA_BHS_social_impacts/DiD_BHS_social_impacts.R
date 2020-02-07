##----------------------------------------------------------------------------------------------------------------------------------##
##---MPA Social Impacts - BHS---##
##---Jan 2020---##
##---Duong Le---##
##---Research: MPA Social Impacts in BHS; Big Five impact indicators; aggregate and heterogeneous by time, MPA, and subgroup--------##


##--------------------------------------Analysis Steps------------------------------------------------------------------------------##
##---0. Sourcing and creating data frame [line 20]
##---1. DiD Model to generate aggregate impacts (Seasape-level) [line 210]

##---2. DiD Model for heterogeneous impact over time (t2 vs t4) [line 270] ---> coarse-matching DiD result (outdated)
##---3. DiD Model for heterogeneous impact across MPA (6 MPAs) [line 410] ---> coarse-matching DiD result (outdated)
##---4. DiD Model for heterogeneous impact across subgroups (4 subgroups) [line 600] ---> coarse-matching DiD result (outdated)

##***Note: if want to skip Steps 1-4; Run Step 0 (up to line 210) then jump to Step 5
##---5. [DiD + settlement baseline matching]---Perform matching & Construct new dataframe [line 700]
##***Important: DiD.data now becomes DiD.data.SettleMatch for all Section 6's analyses [line 775]
##---6 Repeat steps 1 to 4 with the new dataframe [line 780]
##----------------------------------------------------------------------------------------------------------------------------------##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---0. Sourcing and creating data frame 

setwd("D:/Dropbox/MPA_research/MPAMystery/")
source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
pacman::p_load(lfe,cowplot,stargazer,broom,qvalue,psych,factoextra,ineq,tidyverse)

resultPath <- "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/"

# --- DiD dataframe 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, MAIndex:FSIndex ,SERate ,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         Post = as.factor(ifelse(yearsPost==0,0,1)), 
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear), 
         Fisher=ifelse(PrimaryLivelihood==3,1,0), 
         Male = IndividualGender, 
         PrimaryLivelihood.bin = as.factor(ifelse(PrimaryLivelihood==1,1,
                                        ifelse(PrimaryLivelihood==2,2,
                                               ifelse(PrimaryLivelihood==3|PrimaryLivelihood==4,3,
                                                      ifelse(PrimaryLivelihood==6|PrimaryLivelihood==7,4,5)))))) %>%
  filter(!is.na(IndividualGender))
# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other

# --- Filter to look at 6 BHS and 4 SBS MPAs (with t2 and/or t4 data) --- Jan 2020
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6))


# Prepare ethnic polarization index 
HH.eth <- HH.eth %>% 
  left_join(select(DiD.data,HouseholdID, SettlementID, MPAID, yearsPost),by="HouseholdID")
  
eth.polarize <- HH.eth %>% 
  filter(yearsPost==0) %>% 
  group_by(SettlementID) %>% 
  mutate(num.HH.Settl=n()) %>% 
  group_by(SettlementID, eth.iso) %>% 
    summarize(num.HH.eth=n(), 
              num.HH.Settl=first(num.HH.Settl)) %>% 
    mutate(eth.pct = num.HH.eth/num.HH.Settl,  na.rm =T) %>% 
  group_by(SettlementID) %>% 
    mutate(eth.polarize=4*mean(eth.pct*eth.pct*(1-eth.pct)),  na.rm =T) %>% 
    summarise(eth.polarize=first(eth.polarize))
  
DiD.data <- DiD.data %>% 
  left_join(eth.polarize, by="SettlementID")
  
# Customary Governance Index (%HHs excercise rights to exclude/transfer/manage)  
customary.gov.data <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  select(SettlementID, RightsManage, RightsTransfer, RightsExclude) %>% 
  mutate(customary.gov = ifelse(RightsManage%in%c(1) | RightsExclude%in%c(1) | RightsTransfer%in%c(1), 1,0)) %>% 
  group_by(SettlementID) %>% 
    summarise(num.HH.Settl=n(),
              num.HH.customary = sum(customary.gov)) %>% 
    mutate(customary.gov.pct = num.HH.customary/num.HH.Settl)
        

DiD.data <- DiD.data %>% 
  left_join(select(customary.gov.data,customary.gov.pct,SettlementID), by="SettlementID")



# Indicator wealth.above in each seascape-year (factors)
MA.wealth.breaks <- DiD.data %>%
  group_by(yearsPost, MPAID) %>%
  summarise(wealth.median = median(MAIndex, na.rm = T))

DiD.data <- DiD.data %>% 
  left_join(MA.wealth.breaks, by = c("yearsPost","MPAID")) %>% 
  mutate(wealth.above=ifelse(MAIndex<=wealth.median,0,1))


## --- retaining the raw HHH.age (The IndividualAge is currently already categorized)
HH.age.raw <- IndDemos %>%
  filter(RelationHHH==0) %>%
  mutate(IndividualAge_raw = IndividualAge) %>% 
  select(HouseholdID,IndividualAge_raw) %>%
  left_join(select(DiD.data,HouseholdID,yearsPost),by="HouseholdID") %>%
  select(HouseholdID,IndividualAge_raw)%>% 
  distinct(HouseholdID,.keep_all = T)

DiD.data <- DiD.data %>% 
  left_join(HH.age.raw,by="HouseholdID") 

## --- retaining the raw yrsResidence (The current yrResidence is already categorized)
YrResident.raw <- HHData %>%
  select(HouseholdID, YrResident)

DiD.data <- DiD.data %>% 
  left_join(YrResident.raw, by="HouseholdID") 

## --- Modifying Asset Items to generate sub-Asset Groups (i.e. Household assets (discretionary & appliances), Productive Marine-based Assets (the boats), and land-based (vehicles)
DiD.data <- DiD.data %>% 
  mutate(Entertain = Entertain,
         PhoneCombined = PhoneCombined/2,
         Satellite = Satellite/3,
         TV = TV/4,
         Generator = Generator/5,
         BoatNoMotor = BoatNoMotor/6,
         BoatOutboard = BoatOutboard/7,
         BoatInboard = BoatInboard/8,
         Bicycle = Bicycle/9,
         Motorcycle = Motorcycle/10,
         CarTruck = CarTruck/11) %>% 
  mutate(Entertain_dum = ifelse(Entertain>0,1,0),
         PhoneCombined_dum = ifelse(PhoneCombined>0,1,0),
         Satellite_dum = ifelse(Satellite>0,1,0),
         TV_dum = ifelse(TV>0,1,0),
         Generator_dum = ifelse(Generator>0,1,0),
         BoatNoMotor_dum = ifelse(BoatNoMotor>0,1,0),
         BoatOutboard_dum = ifelse(BoatOutboard>0,1,0),
         BoatInboard_dum = ifelse(BoatInboard>0,1,0),
         Bicycle_dum = ifelse(Bicycle>0,1,0),
         Motorcycle_dum = ifelse(Motorcycle>0,1,0),
         CarTruck_dum = ifelse(CarTruck>0,1,0)) %>%
  mutate(Household_asset = Entertain + 2*PhoneCombined + 3*Satellite + 4*TV + 5*Generator,
         Boats_w1 = BoatNoMotor + 2*BoatOutboard + 3*BoatInboard,
         Boats_w2 = 6*BoatNoMotor + 7*BoatOutboard + 8*BoatInboard,
         Boats_motor_w1 = 1*BoatOutboard + 2*BoatInboard,
         Boats_motor_w2 = 7*BoatOutboard + 8*BoatInboard,
         Vehicles_w1 = Bicycle + 2*Motorcycle + 3*CarTruck,
         Vehicles_w2 = 9*Bicycle + 10*Motorcycle + 11*CarTruck) %>% 
  mutate(Boats_dum = ifelse(BoatNoMotor>0 | BoatOutboard >0 | BoatInboard >0,1,0),
         Boats_motor_dum = ifelse(BoatOutboard >0 | BoatInboard >0,1,0),
         Vehicles_dum = ifelse(Bicycle>0 | Motorcycle>0 | CarTruck>0,1,0))

# --- Adding Indicators for community Participation (marine and non-marine groups)
MarineGroup.Indicators <- Organization %>%
  group_by(HouseholdID) %>%
  summarise(NumMarineGroup=n(),
            Marine.Lead.count = length(HouseholdID[MarinePosition==2]),
            Marine.Meeting.Active = sum(MarineMeeting[MarineMeeting==1]),
            Marine.Days.Participate = sum(MarineDays[MarineDays<=989])) %>%
  left_join(DiD.data[,c("HouseholdID","MPAID")],.,by="HouseholdID") %>%
  mutate(NumMarineGroup=ifelse(is.na(NumMarineGroup),0,NumMarineGroup)) %>% 
  mutate(Marine.Lead.count=ifelse(is.na(Marine.Lead.count),0,Marine.Lead.count)) %>% 
  mutate(Marine.Days.Participate=ifelse(is.na(Marine.Days.Participate),0,Marine.Days.Participate)) %>% 
  mutate(Marine.Meeting.Active=ifelse(is.na(Marine.Days.Participate),0,
                                      ifelse(Marine.Days.Participate>0,1,0))) 

Non_MarineGroup.Indicators <- NMOrganization %>%
  group_by(HouseholdID) %>%
  summarise(NumOtherGroup=length(HouseholdID),
            OtherGroup.Lead.count = length(HouseholdID[OtherGroupPosition==2]),
            OtherGroup.Meeting.Active = sum(OtherGroupMeeting[OtherGroupMeeting==1]),
            OtherGroup.Days.Participate = sum(OtherGroupDays[OtherGroupDays<=989])) %>%
  left_join(DiD.data[,c("HouseholdID","MPAID")],.,by="HouseholdID") %>%
  mutate(NumOtherGroup=ifelse(is.na(NumOtherGroup),0,NumOtherGroup)) %>% 
  mutate(OtherGroup.Lead.count=ifelse(is.na(OtherGroup.Lead.count),0,OtherGroup.Lead.count)) %>% 
  mutate(OtherGroup.Days.Participate=ifelse(is.na(OtherGroup.Days.Participate),0,OtherGroup.Days.Participate)) %>% 
  mutate(OtherGroup.Meeting.Active=ifelse(is.na(OtherGroup.Meeting.Active),0,
                                          ifelse(OtherGroup.Meeting.Active>0,1,0))) 


DiD.data <-  DiD.data %>% 
  left_join(.,MarineGroup.Indicators[,c("HouseholdID", "NumMarineGroup", "Marine.Lead.count", "Marine.Meeting.Active", "Marine.Days.Participate")],by="HouseholdID") %>% 
  left_join(.,Non_MarineGroup.Indicators[,c("HouseholdID", "NumOtherGroup", "OtherGroup.Lead.count", "OtherGroup.Meeting.Active", "OtherGroup.Days.Participate")],by="HouseholdID") 

# --- summary(DiD.data)
DiD.data.summary <- DiD.data %>% 
  select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  


# calculate Z scores (standardized values for each of the Big Five and the sub_asset groups)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, Household_asset:Vehicles_w2), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}



##---END---##
##---END---##
##---END---##

# 
# ##---BEGIN---##
# ##---BEGIN---##
# ##---BEGIN---##
# ##-------------------------------------------------------------------------------##
# ##---1. DiD Model to generate aggregate impacts (Seasape-level) 
# ##-------------------------------------------------------------------------------##
# varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
# 
# ##DiD Regression model
# model.out <- data.frame()
# for (i in varNames) {
#   print(i)
#   Y <- DiD.data[,i]
#   regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
#                    | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
#                    data=DiD.data,exactDOF = TRUE)
#   summary(regValue)
#   reg.broom <- tidy(regValue) %>% 
#     mutate(Response=i)
#   
#   model.out <- rbind(model.out,reg.broom)
# }
# 
# ##keeping only 2 terms yearsPostF2, yearsPostF4 (i.e. time trend) and Treatment:yearsPostF2 &  Treatment:yearsPostF4 (i.e. DiD impacts)
# model.out1 <- model.out %>% 
#   filter(term%in%c("Treatment:Post1")) %>% 
#   mutate(term=gsub("Treatment:Post1","Impact",term),
#          domain=ifelse(Response=="FSIndex_z"," Health (Food Security)",
#                        ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
#                               ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
#                                      ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
#          domain=gsub(" \\(", "\n \\(", domain)) #this line break the labels into 2 lines whenever it finds the symbol "(" in the string
# 
# ##PLOTS
# pd <- position_dodge(width=.3) # move them .05 to the left and right
# 
# 
# Big5.plot <- ggplot(model.out1, aes(x=domain,y=estimate)) + 
#   geom_line( position = pd) + coord_flip() +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="MPA Aggregate Impacts across Social Wellbeing Domains")  
# 
# 
# ggsave(paste0(resultPath,"Coarse_matching/plots/1-big5-seascape-z.jpg"),width = 12, height = 6)
# 
# ##---END---##
# ##---END---##
# ##---END---##
# 
# 
# ##---BEGIN---##
# ##---BEGIN---##
# ##---BEGIN---##
# ##-------------------------------------------------------------------------------##
# ##---2. DiD Model for heterogeneous impact over time (t2 vs t4)
# ##-------------------------------------------------------------------------------##
# varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
# 
# ##DiD Regression model
# model.out <- data.frame()
# for (i in varNames) {
#   print(i)
#   Y <- DiD.data[,i]
#   regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
#                    | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
#                    data=DiD.data,exactDOF = TRUE)
#   summary(regValue)
#   reg.broom <- tidy(regValue) %>% 
#     mutate(Response=i)
#   
#   model.out <- rbind(model.out,reg.broom)
# }
# 
# ##keeping only 2 terms yearsPostF2, yearsPostF4 (i.e. time trend) and Treatment:yearsPostF2 &  Treatment:yearsPostF4 (i.e. DiD impacts)
# model.out1 <- model.out %>% 
#   filter(term%in%c("Treatment:yearsPostF2", "Treatment:yearsPostF4")) %>% 
#   mutate(term=gsub("Treatment:yearsPostF","t",term))
# 
# ##PLOTS
# pd <- position_dodge(width=.3) # move them .05 to the left and right
# 
# 
# FS.plot <- ggplot(filter(model.out1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Food Security")  
# #+ facet_grid(.~Response)
# 
# MT.plot <- ggplot(filter(model.out1,Response=="MTIndex"),aes(x=term,y=estimate)) +  theme(legend.position="none") +
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Marine Tenure")  
# 
# 
# MA.plot <- ggplot(filter(model.out1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Material Assets")  
# 
# 
# PA.plot <- ggplot(filter(model.out1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Place Attachment")  
# 
# 
# SE.plot <- ggplot(filter(model.out1,Response=="SERate"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="School Enrollment")  
# 
# 
# #library(cowplot)
# #Combine "regular BigFive"
# plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
# ggsave(paste0(resultPath,"Coarse_matching/plots/2-big5-seascape-time.jpg"),width = 12, height = 6)
# 
# 
# 
# #####################Repeat the 5 plots, now using standardized scores
# 
# 
# FS.plot <- ggplot(filter(model.out1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Food Security")  
# #+ facet_grid(.~Response)
# 
# MT.plot <- ggplot(filter(model.out1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Marine Tenure")  
# 
# 
# MA.plot <- ggplot(filter(model.out1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Material Assets")  
# 
# 
# PA.plot <- ggplot(filter(model.out1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Place Attachment")  
# 
# 
# SE.plot <- ggplot(filter(model.out1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
#   geom_line( position = pd) + 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="School Enrollment")  
# 
# #Combine "standardize BigFive"
# plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
# ggsave(paste0(resultPath,"Coarse_matching/plots/2-big5-seascape-time-z.jpg"),width = 12, height = 6)
# 
# ##---END---##
# ##---END---##
# ##---END---##
# 
# 
# 
# ##---BEGIN---##
# ##---BEGIN---##
# ##---BEGIN---##
# ##-------------------------------------------------------------------------------##
# ##---3. DiD Model for heterogeneous impact across MPA (6 MPAs)
# ##-------------------------------------------------------------------------------##
# 
# ###generating short MPA names
# mpa.nam <- mpa.nam %>% 
#   mutate(MPAName_short = ifelse(MPACode==1,"  Telma",
#                                 ifelse(MPACode==2,"  TNTC",
#                                        ifelse(MPACode==3," Kaimana",
#                                               ifelse(MPACode==4," Kofiau",
#                                                      ifelse(MPACode==5,"Dampier",
#                                                             ifelse(MPACode==6,"Misool",
#                                                                    ifelse(MPACode==15,"Selat Pantar",
#                                                                           ifelse(MPACode==16,"Flores Timur",
#                                                                                  ifelse(MPACode==17,"Kei Kecil",
#                                                                                         ifelse(MPACode==18,"Koon","")))))))))))
# 
# varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
# 
# ##DiD Regressions: Hetegeneous impact by MPA (and time)
# model.out.mpalevel <- data.frame()
# 
# for (i in varNames) {
#   for (mpaid in 1:6) {
#     print(i)
#     print(mpaid)
#     DiD.data.mpalevel <- DiD.data %>% 
#       filter(MPAID==mpaid)
#     Y <- DiD.data.mpalevel[,i]
#     
#     regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
#                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
#                      data=DiD.data.mpalevel,exactDOF = TRUE)
#     
#     reg.broom <- tidy(regValue) %>%
#       mutate(Response=i, MPAID=mpaid)
#     model.out.mpalevel <- rbind(model.out.mpalevel,reg.broom)
#   }
# }
# 
# ##PLOTs
# model.out.mpalevel1 <- model.out.mpalevel %>% 
#   filter(term%in%c("Treatment:Post1")) %>% 
#   mutate(term=gsub("Treatment:Post1","Impact",term)) %>% 
#   left_join(mpa.nam) %>% 
#   mutate(MPAName=gsub(" MPA","",MPAName),
#          MPAName=gsub("Teluk ","",MPAName))
# 
# 
# ###edit model.out1 (with seascape level reg values; ready to merge/include to model.out.mpalevel1)
# model.out1$MPAID <-0
# model.out1$MPAName <-" Seascape"
# 
# model.out.mpalevel1 <- model.out.mpalevel1 %>% 
#   select(term, estimate, std.error, statistic, p.value, Response, MPAID, MPAName, MPAName_short)
# 
# model.out.mpalevel1 <- rbind(model.out.mpalevel1,model.out1)
# 
# ##PLOT: "regular" index
# pd <- position_dodge(width=.3) # move them .05 to the left and right
# 
# FS.plot <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Food Security")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# MT.plot <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Marine Tenure")  +
#   scale_colour_manual(values = c("black", "blue")) 
# 
# 
# PA.plot <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Place Attachment")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# 
# MA.plot <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Material Assets")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# 
# SE.plot <- ggplot(filter(model.out.mpalevel1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="School Enrollment")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# ##Combine PLOTS
# plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
# ggsave(paste0(resultPath,"Coarse_matching/plots/3-big5-mpa.jpg"),width = 12, height = 6)
# 
# 
# 
# ##Producing Big Five plots using "standardized" index
# FS.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Food Security")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# 
# MT.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Marine Tenure")  +
#   scale_colour_manual(values = c("black", "blue")) 
# 
# 
# PA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Place Attachment")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# 
# MA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="Material Assets")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# 
# SE.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
#   geom_line( position = pd) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   #geom_vline(xintercept = 1.5, linetype = "dotdash") +
#   labs(x="",y="", title="School Enrollment")  +
#   scale_colour_manual(values = c("black", "blue"))
# 
# 
# ##combine PLOTS
# plot_grid(MA.plot_z,FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,ncol=3)
# ggsave(paste0(resultPath,"Coarse_matching/plots/3-big5-mpa-z.jpg"),width = 12, height = 6)
# 
# ##---END---##
# ##---END---##
# ##---END---##
# 
# 
# ##---BEGIN---##
# ##---BEGIN---##
# ##---BEGIN---##
# ##-------------------------------------------------------------------------------##
# ##---4. DiD Model for heterogeneous impact across subgroups (5 subgroups)
# ##-------------------------------------------------------------------------------##
# model.out.subgroup <- data.frame()
# varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
# 
# for (i in varNames) {
#   
#   ##DiD Regressions: by Gender
#   for (genderID in 0:1) {
#     DiD.data.gender <- DiD.data %>%  filter(Male==genderID)
#     Y <- DiD.data.gender[,i]
#     
#     regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
#                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,  data=DiD.data.gender,exactDOF = TRUE)
#     
#     reg.broom <- tidy(regValue) %>%
#       mutate(Response=i, subgroup="Gender", subgroup_id=genderID)
#     model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
#   }
# 
#   ##DiD Regressions: by Occupation
#   for (fisherID in 0:1) {
#     DiD.data.fisher <- DiD.data %>% filter(Fisher==fisherID)
#     Y <- DiD.data.fisher[,i]
#     
#     regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
#                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.fisher,exactDOF = TRUE)
#     
#     reg.broom <- tidy(regValue) %>%
#       mutate(Response=i, subgroup="Fishing Livelihood", subgroup_id=fisherID)
#     model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
#   }
#   
#   ##DiD Regressions: by dom.eth
#   for (dom.ethID in 0:1) {
#     DiD.data.dom.eth <- DiD.data %>% filter(dom.eth==dom.ethID)
#     Y <- DiD.data.dom.eth[,i]
#     
#     regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
#                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.dom.eth,exactDOF = TRUE)
#     
#     reg.broom <- tidy(regValue) %>%
#       mutate(Response=i, subgroup="Ethnicity", subgroup_id=dom.ethID)
#     model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
#   } 
#   
#   ##DiD Regressions: by wealth
#   for (wealthID in 0:1) {
#     DiD.data.wealth <- DiD.data %>% filter(wealth.above==wealthID)
#     Y <- DiD.data.wealth[,i]
#     
#     regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
#                      | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.wealth,exactDOF = TRUE)
#     
#     reg.broom <- tidy(regValue) %>%
#       mutate(Response=i, subgroup="Economic Wealth", subgroup_id=wealthID)
#     model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
#   }
# }
# 
# ##PLOTs
# model.out.subgroup1 <- model.out.subgroup %>%
#   filter(term%in%c("Treatment:Post1")) %>%
#   mutate(term=gsub("Treatment:Post1","Impact",term))
# 
# ##PLOT: "regular" index
# pd <- position_dodge(width=.5) # move them .05 to the left and right
# 
# FS.plot_z <- ggplot(filter(model.out.subgroup1,Response=="FSIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Food Security")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# MT.plot_z <- ggplot(filter(model.out.subgroup1,Response=="MTIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Marine Tenure")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# PA.plot_z <- ggplot(filter(model.out.subgroup1,Response=="PAIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Place Attachment")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# 
# MA.plot_z <- ggplot(filter(model.out.subgroup1,Response=="MAIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Material Assets")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# SE.plot_z <- ggplot(filter(model.out.subgroup1,Response=="SERate_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="School Enrollment")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# ##combine PLOTS
# plot_grid(MA.plot_z,FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,ncol=3)
# ggsave(paste0(resultPath,"Coarse_matching/plots/4-big5-subgroup-z.jpg"),width = 12, height = 6)
# 
# ##---END---##
# ##---END---##
# ##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---5. [DiD + settlement baseline matching]---Perform matching & Construct new dataframe
##-------------------------------------------------------------------------------##

# DiD.data <- DiD.data %>% 
#   mutate(ed.no = ifelse(ed.level==0,1,0),
#          ed.primary = ifelse(ed.level<=1,1,0),
#          ed.high = ifelse(ed.level>=3,1,0),
#          ed.college = ifelse(ed.level>=4,1,0))

##---dataframe for matching settlements
DiD.data.matchingCov <- DiD.data %>%
  filter(yearsPost==0) %>% 
  select(MPAID, MonitoringYear, SettlementID, InterviewYear, Treatment, yearsPost, Post, Fisher, TimeMarket, eth.polarize, customary.gov.pct, MTIndex, YrResident, EconStatusTrend) %>% 
  group_by(SettlementID) %>%
  summarise(MPAID=first(MPAID),
            Treatment=first(Treatment),
            eth.polarize=first(eth.polarize),
            customary.gov.pct=first(customary.gov.pct),
            Fisher = mean(Fisher,na.rm=T), 
            #dom.MT = mean(dom.MT,na.rm=T), 
            #EconStatusTrend = mean(EconStatusTrend, na.rm=T),
            #dom.eth = mean(dom.eth,na.rm=T),
            TimeMarket = mean(TimeMarket,na.rm=T)) %>% 
  select(SettlementID,Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, MPAID) %>% 
  mutate(MPAID=as.integer(MPAID))

DiD.data.matchingCov.final <- DiD.data.matchingCov %>% 
  select(SettlementID,Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, MPAID) 
  
## -- Mathcing settlements
Tr<-as.vector(DiD.data.matchingCov.final$Treatment)  

##-------------------------------------------------------------------------------------------##
### Run match algorithm (COVARIATE MATCHING (Malanobis) with calipers
## Always run with replace=TRUE, and ties=TRUE, vary M (# matches)
# m1<-Match(Y=NULL, Tr,X, M=1, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=TRUE)
# summary(m1)

# Exact matching on MPAID (i.e., control and treatment settlements have to come from the same MPA; that's why the last item is "1")
Xexact<-c(0,0,0,0,1)
m_mahanobis<-Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.final, M=1, exact=Xexact, replace=TRUE, ties=T)
summary(m_mahanobis)

# Compute match balance statistics (don't need stats on exact-matched variables)
m_balance_2<-Matching::MatchBalance(Tr~ TimeMarket + Fisher + eth.polarize + customary.gov.pct,
                                    data=DiD.data.matchingCov, match.out=m_mahanobis, ks=TRUE, nboots=1000, digits=3)
# 
# ##-------------------------------------------------------------------------------------------##
# ##---pscore matching
# # calculate p scores
# pscore <- glm(Tr~ TimeMarket + Fisher +  eth.polarize + customary.gov.pct, data=DiD.data.matchingCov)$fitted.values
# psore_frame = cbind(pscore, DiD.data.matchingCov$MPAID)
# Xexact<-c(0,1)
# m_pscore<-Matching::Match(Y=NULL, Tr, X=psore_frame, M=1, exact=Xexact, replace=TRUE, ties=T)
# m_balance_2<-Matching::MatchBalance(Tr~  TimeMarket + Fisher + eth.polarize + customary.gov.pct ,data=DiD.data.matchingCov, match.out=m_pscore, ks=TRUE, nboots=1000, digits=3)
# ##-------------------------------------------------------------------------------------------##


##--Note: After the matching trials, best option is mahanobis, 1-to-1 match. Go with that for now.
##construct DiD.data.SettlMatch dataframe
i.treat<-m_mahanobis$index.treated
i.control<-m_mahanobis$index.control  

pair.treat.settl <- DiD.data.matchingCov %>% 
  filter(Treatment==1) %>% 
  mutate(pair.id=SettlementID) %>% 
  select(SettlementID, pair.id)

pair.control.settl <- cbind(DiD.data.matchingCov[i.control,"SettlementID"], DiD.data.matchingCov[i.treat,"SettlementID"]) 
names(pair.control.settl) <- c("SettlementID", "pair.id")
head(pair.control.settl)  

DiD.data.SettlMatch <- data.frame()
DiD.data.SettlMatch <- rbind(pair.treat.settl, pair.control.settl)
DiD.data.SettlMatch <-DiD.data.SettlMatch[order(DiD.data.SettlMatch$pair.id),] %>% 
  left_join(DiD.data,by="SettlementID") %>% 
  mutate(pair.id = as.factor(pair.id))
DiD.data.SettlMatch <- as.data.frame(DiD.data.SettlMatch)

##---BEGIN---##
# Export data frame to Excel for exploratory Stata analysis

# #Export to txt file
# library(foreign)
# #install.packages("writexl")
# library(writexl)
# write.table(DiD.data.SettlMatch, "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/matched_DID_BHS_data.txt", sep="\t")
# write_xlsx(DiD.data.SettlMatch, path = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/matched_DID_BHS_data", col_names = TRUE, format_headers = TRUE)


# ##--histogram plots of the matching covariate (baseline -- before matching)
DiD.data.density.before <- DiD.data.matchingCov %>% 
  mutate(Treat=as.factor(Treatment), 
         Group=ifelse(Treatment==1, " MPA", "Control"))

density.TimeMarket <- ggplot(DiD.data.density.before, aes(x = TimeMarket)) + labs(x="Time to Market", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() +
  theme(legend.position=c(0.85,0.85)) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black")) +
  theme(legend.title = element_text(colour="black", size=14, face="bold")) + theme(legend.text = element_text(colour="black", size=11))
                                                                          

density.eth.polarize <- ggplot(DiD.data.density.before, aes(x = eth.polarize)) + labs(x="Ethnic Polarization Index", y="Density") +  
  stat_density(aes(group = Treat, linetype = Group),position="identity", geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

density.Fisher <- ggplot(DiD.data.density.before, aes(x = Fisher)) + labs(x="Fishery Livelihood (%)", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

density.customary <- ggplot(DiD.data.density.before, aes(x = customary.gov.pct)) + labs(x="Customary Governance (%)", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() + theme(legend.position="none")

plot_grid(density.TimeMarket,density.Fisher,density.eth.polarize,density.customary,ncol=2)
ggsave(paste0(resultPath,"Settlement_matching/plots/histograms_matchingCovs_before.jpg"),width = 11, height = 9)

# ##--histogram plots of the matching covariate (baseline -- after matching)
DiD.data.density.after <- DiD.data.SettlMatch %>% 
  filter(yearsPost==0) %>% 
  mutate(Treat=as.factor(Treatment), 
         Group=ifelse(Treatment==1, " MPA", "Control"))

density.TimeMarket <- ggplot(DiD.data.density.after, aes(x = TimeMarket)) + labs(x="Time to Market", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() +
  theme(legend.position=c(0.85,0.85)) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black")) +
  theme(legend.title = element_text(colour="black", size=14, face="bold")) + theme(legend.text = element_text(colour="black", size=11))


density.eth.polarize <- ggplot(DiD.data.density.after, aes(x = eth.polarize)) + labs(x="Ethnic Polarization Index", y="Density") +  
  stat_density(aes(group = Treat, linetype = Group),position="identity", geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

density.Fisher <- ggplot(DiD.data.density.after, aes(x = Fisher)) + labs(x="Fishery Livelihood (%)", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

density.customary <- ggplot(DiD.data.density.after, aes(x = customary.gov.pct)) + labs(x="Customary Governance (%)", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() + theme(legend.position="none")

plot_grid(density.TimeMarket,density.Fisher,density.eth.polarize,density.customary,ncol=2)
ggsave(paste0(resultPath,"Settlement_matching/plots/histograms_matchingCovs_after.jpg"),width = 11, height = 9)

##---END---##
##---END---##
##---END---##

##--Important: Now deleting dataframe "DiD.data", replacing it with "DiD.data.SettlMatch" for Sections 6.1 to 6.4
##--Also, the subsequent DiD models now account for settlement-pair FEs and clustering
DiD.data <- data.frame()
DiD.data <- DiD.data.SettlMatch

DiD.data.SettlMatch <- data.frame()
##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---6.1. DiD Model to generate aggregate impacts (Seasape-level) 
##-------------------------------------------------------------------------------##
varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
#varNames <- c("FSIndex_z")

##DiD Regression model (presenting 6 alternative models)
model.out <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  ## 1. with strictest yearFE-by-pairID specification
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID  + pair.id:InterviewYear  | 0 | SettlementID + pair.id, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ## Get covariance value between Post and Treatment:Post (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue) 
  vcov <- vcov.matrix["Treatment:Post1","Post1"] 
  
  reg.broom.1 <- tidy(regValue) %>% mutate(Response=i, vcov_impact_control=vcov,  spec=1, spec_label="strict")
  
  
  ## 2. with year FEs + pairID (split)
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID  + pair.id + InterviewYear  | 0 |  SettlementID + pair.id, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ## Get covariance value between Post and Treatment:Post (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue) 
  vcov <- vcov.matrix["Treatment:Post1","Post1"] 
  
  reg.broom.2 <- tidy(regValue) %>% mutate(Response=i, vcov_impact_control=vcov, spec=2, spec_label="pairid & InterviewYear split")
  
  ## 3. with year FEs + pairID (split) + lose SettlementID cluster
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID  + pair.id + InterviewYear  | 0 |  pair.id, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ## Get covariance value between Post and Treatment:Post (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue) 
  vcov <- vcov.matrix["Treatment:Post1","Post1"] 
  
  reg.broom.3 <- tidy(regValue) %>% mutate(Response=i, vcov_impact_control=vcov, spec=3, spec_label="pairid & InterviewYear split + no SettlementID cluster")
  
  #####################
  ## 4. no yearFE 
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ## Get covariance value between Post and Treatment:Post (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue) 
  vcov <- vcov.matrix["Treatment:Post1","Post1"] 
  
  reg.broom.4 <- tidy(regValue) %>% mutate(Response=i, vcov_impact_control=vcov, spec=4, spec_label="no yearFE")
  
  
  ## no year FEs + no SettlementID cluster
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID  + pair.id   | 0 | pair.id, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ## Get covariance value between Post and Treatment:Post (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue) 
  vcov <- vcov.matrix["Treatment:Post1","Post1"] 
  
  reg.broom.5 <- tidy(regValue) %>% mutate(Response=i,vcov_impact_control=vcov, spec=5, spec_label="no yearFE & no settlID cluster")
  
  
  ## 5. no year FEs + no SettlementID cluster + 
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post 
                   | SettlementID  + pair.id   | 0 | pair.id, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ## Get covariance value between Post and Treatment:Post (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue) 
  vcov <- vcov.matrix["Treatment:Post1","Post1"] 
  
  reg.broom.6 <- tidy(regValue) %>% mutate(Response=i, vcov_impact_control=vcov, spec=6, spec_label="no yearFE & no settlID cluster & no covHH")
  
  model.out <- rbind(model.out,reg.broom.1,reg.broom.2,reg.broom.3,reg.broom.4,reg.broom.5,reg.broom.6)
}


##keeping only 2 terms yearsPostF2, yearsPostF4 (i.e. time trend) and Treatment:yearsPostF2 &  Treatment:yearsPostF4 (i.e. DiD impacts)
model.out1 <- model.out %>% 
  filter(term%in%c("Treatment:Post1", "Post1")) %>% 
  mutate(term=gsub("Treatment:Post1","Impact",term),
         term=gsub("Post1","Control",term),
         domain=ifelse(Response=="FSIndex_z"," Health (Food Security)",
                       ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) #this line break the labels into 2 lines whenever it finds the symbol "(" in the string


## spead dataframe to compute treatment trend's estimates (=control trend + Impact)
model.out1.rearrange.estimate<-model.out1 %>% 
  select(Response, spec, spec_label, domain, term, estimate, vcov_impact_control) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = Control + Impact) %>% 
  select(Response, spec, spec_label, domain, estimate) %>% 
  mutate(term="Treat", statistic=0, p.value=0, vcov_impact_control=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out1.rearrange.stderr<-model.out1 %>% 
  select(Response, spec, spec_label, domain, term, std.error, vcov_impact_control) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(Control^2 + Impact^2 + 2*Control*Impact*vcov_impact_control))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out1.rearrange.estimate<-cbind(model.out1.rearrange.estimate,model.out1.rearrange.stderr) 

model.out1<-rbind(model.out1, model.out1.rearrange.estimate)

model.out1 <-model.out1[order(model.out1$Response, model.out1$spec),]


##Export 
#export(model.out1,  "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/Settlement_matching/plots/no pairXyearFEs/BHS_impact_output.csv")


##PLOTS
pd <- position_dodge(width=.3) # move them .05 to the left and right


Big5.plot <- ggplot(model.out1, aes(x=domain,y=estimate)) + 
  geom_line( position = pd) + coord_flip() +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimates", title="MPA Aggregate Impacts across Social Wellbeing Domains")  


ggsave(paste0(resultPath,"Settlement_matching/plots/1-big5--seascape-z.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---6.2. DiD Model for heterogeneous impact over time (t2 vs t4)
##-------------------------------------------------------------------------------##
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

##DiD Regression model
model.out <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                   | SettlementID + InterviewYear + MPAID:InterviewYear + pair.id | 0 | SettlementID + pair.id,
                   data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i)
  
  model.out <- rbind(model.out,reg.broom)
}

##keeping only 2 terms yearsPostF2, yearsPostF4 (i.e. time trend) and Treatment:yearsPostF2 &  Treatment:yearsPostF4 (i.e. DiD impacts)
model.out1 <- model.out %>% 
  filter(term%in%c("Treatment:yearsPostF2", "Treatment:yearsPostF4")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term))

##PLOTS
pd <- position_dodge(width=.3) # move them .05 to the left and right


FS.plot <- ggplot(filter(model.out1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out1,Response=="MTIndex"),aes(x=term,y=estimate)) +  theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Material Assets")  


PA.plot <- ggplot(filter(model.out1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out1,Response=="SERate"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="School Enrollment")  


#library(cowplot)
#Combine "regular BigFive"
plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
ggsave(paste0(resultPath,"Settlement_matching/plots/2-big5-seascape-time.jpg"),width = 12, height = 6)



#####################Repeat the 5 plots, now using standardized scores


FS.plot <- ggplot(filter(model.out1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Material Assets")  


PA.plot <- ggplot(filter(model.out1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
ggsave(paste0(resultPath,"Settlement_matching/plots/2-big5-seascape-time-z.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##



##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---6.3. DiD Model for heterogeneous impact across MPA (6 MPAs)
##-------------------------------------------------------------------------------##

###generating short MPA names
mpa.nam <- mpa.nam %>% 
  mutate(MPAName_short = ifelse(MPACode==1,"  Telma",
                                ifelse(MPACode==2,"  TNTC",
                                       ifelse(MPACode==3," Kaimana",
                                              ifelse(MPACode==4," Kofiau",
                                                     ifelse(MPACode==5,"Dampier",
                                                            ifelse(MPACode==6,"Misool",
                                                                   ifelse(MPACode==15,"Selat Pantar",
                                                                          ifelse(MPACode==16,"Flores Timur","")))))))))

varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

##DiD Regressions: Hetegeneous impact by MPA (and time)
model.out.mpalevel <- data.frame()

for (i in varNames) {
  for (mpaid in 1:6) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
                     | SettlementID + InterviewYear + MPAID:InterviewYear + pair.id | 0 | SettlementID + pair.id,
                     data=DiD.data.mpalevel,exactDOF = TRUE)
    
    reg.broom <- tidy(regValue) %>%
      mutate(Response=i, MPAID=mpaid)
    model.out.mpalevel <- rbind(model.out.mpalevel,reg.broom)
  }
}

##PLOTs
model.out.mpalevel1 <- model.out.mpalevel %>% 
  filter(term%in%c("Treatment:Post1")) %>% 
  mutate(term=gsub("Treatment:Post1","Impact",term)) %>% 
  left_join(mpa.nam) %>% 
  mutate(MPAName=gsub(" MPA","",MPAName),
         MPAName=gsub("Teluk ","",MPAName))


###edit model.out1 (with seascape level reg values; ready to merge/include to model.out.mpalevel1)
model.out1$MPAID <-0
model.out1$MPAName <-" Seascape"

model.out.mpalevel1 <- model.out.mpalevel1 %>% 
  select(term, estimate, std.error, statistic, p.value, Response, MPAID, MPAName, MPAName_short)

model.out.mpalevel1 <- rbind(model.out.mpalevel1,model.out1)

##PLOT: "regular" index
pd <- position_dodge(width=.3) # move them .05 to the left and right

FS.plot <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))

MT.plot <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 


PA.plot <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))


MA.plot <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))


SE.plot <- ggplot(filter(model.out.mpalevel1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))

##Combine PLOTS
plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
ggsave(paste0(resultPath,"Settlement_matching/plots/3-big5-mpa.jpg"),width = 12, height = 6)



##Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0(resultPath,"Settlement_matching/plots/3-FS-mpa-z.jpg"),width = 12, height = 6)


MT.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))


##combine PLOTS
plot_grid(MA.plot_z,FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,ncol=3)
ggsave(paste0(resultPath,"Settlement_matching/plots/3-big5-mpa-z.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---6.4. DiD Model for heterogeneous impact across subgroups (5 subgroups)
##-------------------------------------------------------------------------------##
model.out.subgroup <- data.frame()
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

for (i in varNames) {
  
  ##DiD Regressions: by Gender
  for (genderID in 0:1) {
    DiD.data.gender <- DiD.data %>%  filter(Male==genderID)
    Y <- DiD.data.gender[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
                     | SettlementID + InterviewYear + MPAID:InterviewYear + pair.id | 0 | SettlementID + pair.id,  data=DiD.data.gender,exactDOF = TRUE)
    
    reg.broom <- tidy(regValue) %>%
      mutate(Response=i, subgroup="Gender", subgroup_id=genderID)
    model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
  }
  
  ##DiD Regressions: by Occupation
  for (fisherID in 0:1) {
    DiD.data.fisher <- DiD.data %>% filter(Fisher==fisherID)
    Y <- DiD.data.fisher[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
                     | SettlementID + InterviewYear + MPAID:InterviewYear + pair.id | 0 | SettlementID + pair.id, data=DiD.data.fisher,exactDOF = TRUE)
    
    reg.broom <- tidy(regValue) %>%
      mutate(Response=i, subgroup="Fishing Livelihood", subgroup_id=fisherID)
    model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
  }
  
  ##DiD Regressions: by dom.eth
  for (dom.ethID in 0:1) {
    DiD.data.dom.eth <- DiD.data %>% filter(dom.eth==genderID)
    Y <- DiD.data.dom.eth[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
                     | SettlementID + InterviewYear + MPAID:InterviewYear + pair.id | 0 | SettlementID + pair.id, data=DiD.data.dom.eth,exactDOF = TRUE)
    
    reg.broom <- tidy(regValue) %>%
      mutate(Response=i, subgroup="Ethnicity", subgroup_id=dom.ethID)
    model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
  } 
  
  ##DiD Regressions: by wealth
  for (wealthID in 0:1) {
    DiD.data.wealth <- DiD.data %>% filter(wealth.above==wealthID)
    Y <- DiD.data.wealth[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + Post + Treatment:Post
                     | SettlementID + InterviewYear + MPAID:InterviewYear + pair.id | 0 | SettlementID + pair.id, data=DiD.data.wealth,exactDOF = TRUE)
    
    reg.broom <- tidy(regValue) %>%
      mutate(Response=i, subgroup="Economic Wealth", subgroup_id=wealthID)
    model.out.subgroup <- rbind(model.out.subgroup,reg.broom)
  }
}

##PLOTs
model.out.subgroup1 <- model.out.subgroup %>%
  filter(term%in%c("Treatment:Post1")) %>%
  mutate(term=gsub("Treatment:Post1","Impact",term))

##PLOT: "regular" index
pd <- position_dodge(width=.5) # move them .05 to the left and right


FS.plot_z <- ggplot(filter(model.out.subgroup1,Response=="FSIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
  geom_line( position = pd) +
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Food Security")  +
  scale_colour_manual(values = c("red", "blue"))

MT.plot_z <- ggplot(filter(model.out.subgroup1,Response=="MTIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
  geom_line( position = pd) +
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Marine Tenure")  +
  scale_colour_manual(values = c("red", "blue"))

PA.plot_z <- ggplot(filter(model.out.subgroup1,Response=="PAIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
  geom_line( position = pd) +
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Place Attachment")  +
  scale_colour_manual(values = c("red", "blue"))


MA.plot_z <- ggplot(filter(model.out.subgroup1,Response=="MAIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
  geom_line( position = pd) +
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Material Assets")  +
  scale_colour_manual(values = c("red", "blue"))

SE.plot_z <- ggplot(filter(model.out.subgroup1,Response=="SERate_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
  geom_line( position = pd) +
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="School Enrollment")  +
  scale_colour_manual(values = c("red", "blue"))

##combine PLOTS
plot_grid(MA.plot_z,FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,ncol=3)
ggsave(paste0(resultPath,"Settlement_matching/plots/4-big5-subgroup-z.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##


