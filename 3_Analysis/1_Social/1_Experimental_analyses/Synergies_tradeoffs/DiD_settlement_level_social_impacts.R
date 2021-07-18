# 
# code: calculate settlement level social impacts across BHS and SBS
# 
# author: Duong Le; Kelly Claborn (clabornkelly@gmail.com)
# created: September 2020 (based on Duong Le's original DiD regression scripts)
# modified: February 2021
# 
#  
# ---- code sections ----
#  1) LOAD LIBRARIES, SOURCE SCRIPTS, & IMPORT DATA
#  2) CREATE MASTER DiD DATA FRAME
#  3) SETTLEMENT BASELINE MATCHING
#  4) CONSTRUCT DATA FRAME FOR IMPACT ANALYSIS
#  5) PRE-PROCESS IMPACT DATA FRAMES
#  6) DiD MODEL FOR HETEROGENEOUS IMPACT ACROSS SETTLEMENTS
#  7) PLOT SETTLEMENT LEVEL IMPACTS
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES, SOURCE SCRIPTS, & IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Call libraries ----

pacman::p_load(lfe, cowplot, stargazer, broom, qvalue, psych, factoextra, ineq, sf, tidyverse)


# ---- 1.2 Sourcing scripts & flat data ----

# This script also pulls in 'Source_social_data_flat_files.R' and 'Calculate_household_indices.R'
# -- it creates HHData (with the BigFive indices calculated), MPA lookup tables, and ultimately a match.covariate data frame used to create the DiD.data frame
# -- FLAT FILES NEEDED FOR FUNCTION_PROCESS_COVARIATES.R:
      #  All files needed for sourcing script (master database exports)
      # "x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx"
      # "x_Flat_data_files/1_Social/Inputs/education_lkp.xlsx"

source('2_Functions/2_Analysis/Function_process_covariates.R')

# Rename the MPA.LKP table to match the rest of the script as was originally written by Duong
mpa.nam <- MPA.LKP %>% rename(MPAID = mpaid, MPAName = name)

# FLAT FILE: Pull in settlement-level coordinates
soc.coord <- import('x_Flat_data_files/1_Social/Inputs/soc.coord.province.csv') %>% 
  mutate(lat = latitude, long = longitude) %>% filter(!is.na(lat)) %>% dplyr::select(-latitude, -longitude, -Treatment)


# ---- 1.3 Create relative result path for outputs (within the broader GitHub repo file structure)

dir.create("x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs")
resultPath <- "x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs"


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CREATE MASTER DiD DATA FRAME ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Initialize DiD data frame ----

DiD.data <- match.covariate %>% 
  filter(!is.na(HouseholdID)) %>% 
  left_join(dplyr::select(HHData,DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, 
                          MAIndex:FSIndex, SERate, HouseholdID, InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam, by="MPAID") %>% 
  dplyr::select(HouseholdID:InterviewYear, MPAName) %>% 
  mutate(TreatFactor = as.factor(ifelse(Treatment==0, 0, MPAID)),
         Post = as.factor(ifelse(yearsPost==0,0 , 1)),
         Control = ifelse(Treatment==1, 0, 1), 
         
         yearsPostF = as.factor(yearsPost),
         MPAID = as.factor(MPAID),
         InterviewYear = as.factor(InterviewYear), 
         Fisher = ifelse(PrimaryLivelihood==3,1,0), 
         Male = IndividualGender, 
         PrimaryLivelihood.bin = as.factor(ifelse(PrimaryLivelihood==1,1,
                                                  ifelse(PrimaryLivelihood==2,2,
                                                         ifelse(PrimaryLivelihood==3 | PrimaryLivelihood==4,3,
                                                                ifelse(PrimaryLivelihood==6 | PrimaryLivelihood==7,4,5))))),
         MTIndex_AccHarv = RightsAccess + RightsHarvest, 
         MTIndex_ManExcTrans= RightsManage + RightsExclude + RightsTransfer, 
         SocialConflict_increase = ifelse(is.na(SocialConflict), NA, 
                                          ifelse(SocialConflict%in%c(1,2), 1, 0))) %>% 
  filter(!is.na(IndividualGender)) %>% 
  left_join(soc.coord, by = "SettlementID")

# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other


# Filter DiD data to look at 6 BHS and 4 SBS MPAs (with t2, t3, and/or t4 data)

DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6, 15:18))


# ---- 2.2 Prepare ethnic polarization index ----

HH.eth <- HH.eth %>% 
  left_join(dplyr::select(DiD.data, HouseholdID, SettlementID, MPAID, yearsPost),by = "HouseholdID")

eth.polarize <- HH.eth %>% 
  filter(yearsPost==0 & !is.na(HouseholdID)) %>% 
  group_by(SettlementID) %>% 
  mutate(num.HH.Settl=n()) %>% 
  group_by(SettlementID, eth.iso) %>% 
  summarize(num.HH.eth=n(), 
            num.HH.Settl=first(num.HH.Settl)) %>% 
  mutate(eth.pct = num.HH.eth/num.HH.Settl) %>% 
  group_by(SettlementID) %>% 
  summarise(eth.polarize=4*mean(eth.pct*eth.pct*(1-eth.pct)))

# Add eth.polarize to DiD.data
DiD.data <- DiD.data %>% 
  left_join(eth.polarize, by="SettlementID")


# ---- 2.3 Prepare Customary Governance Index (%HHs excercise rights to exclude/transfer/manage) ----

customary.gov.data <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  dplyr::select(SettlementID, RightsManage, RightsTransfer, RightsExclude) %>% 
  mutate(customary.gov = ifelse(RightsManage==1 | RightsExclude==1 | RightsTransfer==1, 1, 0)) %>% 
  group_by(SettlementID) %>% 
  summarise(num.HH.Settl = n(),
            num.HH.customary = sum(customary.gov)) %>% 
  mutate(customary.gov.pct = num.HH.customary/num.HH.Settl)

# Add customary governance data to DiD.data
DiD.data <- DiD.data %>% 
  left_join(dplyr::select(customary.gov.data, customary.gov.pct, SettlementID), by="SettlementID")



# ---- 2.4 Prepare Indicator wealth.above in each seascape-year (factors) ----

MA.wealth.breaks <- DiD.data %>%
  group_by(yearsPost, MPAID) %>%
  summarise(wealth.median = median(MAIndex, na.rm = T))

# Add wealth breaks to DiD.data
DiD.data <- DiD.data %>% 
  left_join(MA.wealth.breaks, by = c("yearsPost","MPAID")) %>% 
  mutate(wealth.above=ifelse(MAIndex<=wealth.median,0,1))


# ---- 2.5 Retain the raw HHH.age (IndividualAge is currently already categorized) ----

HH.age.raw <- IndDemos %>%
  filter(RelationHHH==0) %>%
  mutate(IndividualAge_raw = IndividualAge) %>% 
  dplyr::select(HouseholdID,IndividualAge_raw) %>%
  left_join(dplyr::select(DiD.data,HouseholdID,yearsPost),by="HouseholdID") %>%
  dplyr::select(HouseholdID,IndividualAge_raw) %>% 
  distinct(HouseholdID,.keep_all = T)

# Add raw household age to DiD.data
DiD.data <- DiD.data %>% 
  left_join(HH.age.raw, by = "HouseholdID") 


# ---- 2.6 Retain the raw yrsResidence (the current yrResidence is already categorized) ----

YrResident.raw <- HHData %>%
  dplyr::select(HouseholdID, YrResident)

# Add raw years resident to DiD.data
DiD.data <- DiD.data %>% 
  left_join(YrResident.raw, by = "HouseholdID") 


# ---- 2.7 Modify asset items to generate sub-asset groups ----
#         (i.e. Household assets (discretionary & appliances), Productive Marine-based Assets (the boats), and land-based (vehicles)

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


# ---- 2.8 Add indicators for community participation (marine and non-marine groups) ----

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

# Add to DiD.data
DiD.data <-  DiD.data %>% 
  left_join(.,MarineGroup.Indicators[,c("HouseholdID", "NumMarineGroup", "Marine.Lead.count", "Marine.Meeting.Active", "Marine.Days.Participate")],by="HouseholdID") %>% 
  left_join(.,Non_MarineGroup.Indicators[,c("HouseholdID", "NumOtherGroup", "OtherGroup.Lead.count", "OtherGroup.Meeting.Active", "OtherGroup.Days.Participate")],by="HouseholdID") 


# ---- 2.9 Summarize DiD.data ----

DiD.data.summary <- DiD.data %>% 
  dplyr::select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(n=sum(!is.na(yearsPost)),
            yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  
DiD.data.summary

# NOTE: we do not calculate z scores for the Big Five and sub asset groups for our settlement level impact analysis (like it is done for MPA level impact analysis), 
#       because we scale the impacts later (by dividing by two standard deviations) when looking at relative magnitude across eco and social impacts.

# calculate Z scores (standardized values for each of the Big Five and the sub_asset groups)
# DiD.data <- DiD.data %>% 
#   group_by(MPAID, MonitoringYear) %>% 
#   mutate_at(vars(MAIndex:SERate, Household_asset:Vehicles_w2, MTIndex_AccHarv, MTIndex_ManExcTrans, SocialConflict, SocialConflict_increase), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
#   ungroup()
# summary(DiD.data)
# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: SETTLEMENT BASELINE MATCHING  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Create data frame for matching settlements ----

DiD.data.matchingCov <- DiD.data %>%
  filter(yearsPost==0) %>% 
  dplyr::select(MPAID, MonitoringYear, SettlementID, InterviewYear, Treatment, yearsPost, Post, Fisher, TimeMarket, eth.polarize, customary.gov.pct, MTIndex, YrResident, EconStatusTrend, lat, long) %>% 
  group_by(SettlementID) %>%
  summarise(MPAID=first(MPAID),
            Treatment=first(Treatment),
            eth.polarize=first(eth.polarize),
            customary.gov.pct=first(customary.gov.pct),
            lat = first(lat),
            long = first(long),
            Fisher = mean(Fisher,na.rm=T), 
            #dom.MT = mean(dom.MT,na.rm=T), 
            #EconStatusTrend = mean(EconStatusTrend, na.rm=T),
            #dom.eth = mean(dom.eth,na.rm=T),
            TimeMarket = mean(TimeMarket,na.rm=T)) %>% 
  dplyr::select(SettlementID, Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, lat, long, MPAID) %>% 
  mutate(MPAID=as.character(MPAID),
         MPAID=as.integer(MPAID)) %>%
  filter(!is.na(lat)) # !!! QUICK FIX UNTIL LAT/LONG COORDS FOR ALL SOCIAL SETTLEMENTS

DiD.data.matchingCov.final <- DiD.data.matchingCov %>% 
  dplyr::select(TimeMarket, Fisher, eth.polarize, lat, long, MPAID)

# ---- 3.2 Matching settlements ----

Tr <- as.vector(DiD.data.matchingCov$Treatment)  


# ---- 3.3 Run match algorithm (COVARIATE MATCHING (Malanobis) with calipers ----

# Exact matching on MPAID (i.e., control and treatment settlements have to come from the same MPA; that's why the last item is "1")
# -- Always run with replace=TRUE, and ties=TRUE, vary M (# matches)
# -- Note: After matching trials, best option is mahanobis, 1-to-3 match (3 control setts per treatment sett)
Xexact <- c(0,0,0,0,0,1)

m_mahanobis <- Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.final, M = 3, exact = Xexact, replace = TRUE, ties = T)

summary(m_mahanobis)


# ---- 3.4 Compute match balance statistics (don't need stats on exact-matched variables) ----

m_balance_2 <- Matching::MatchBalance(Tr ~ TimeMarket + Fisher + eth.polarize  + lat + long,
                                      data = DiD.data.matchingCov, match.out = m_mahanobis, ks = TRUE, nboots = 1000, digits = 3)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: CONSTRUCT DATA FRAME FOR IMPACT ANALYSIS  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 4.1 Mahalanobis distance weighting ----

i.treat <- m_mahanobis$index.treated
i.control <- m_mahanobis$index.control  

DiD.data.matchingCov.mahDis <- DiD.data.matchingCov %>% 
  dplyr::select(TimeMarket, Fisher, eth.polarize, lat, long) 

# Calculate distances
mal.dist <- mahalanobis(DiD.data.matchingCov.mahDis,cov(DiD.data.matchingCov.mahDis), center = F)
head(mal.dist)


# ---- 4.2 Wrangle initial paired data frames based on distance weighting ----

# Create treatment settlement pair data frame
pair.treat.settl <- as.data.frame(cbind(DiD.data.matchingCov[i.treat,"SettlementID"], mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist = `mal.dist[i.treat]`,c.mal.dist = `mal.dist[i.control]`) %>%
  mutate(pair.dist = t.mal.dist-c.mal.dist,
         pair.id = 1:nrow(.),
         inv.dist = 1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt = inv.dist/sum(inv.dist)) %>% 
  dplyr::select(SettlementID, pair.id, t.mal.dist:dist.wt) %>% 
  ungroup()

head(pair.treat.settl)
summary(pair.treat.settl)

# Create control settlement pair data frame
pair.control.settl <- cbind(DiD.data.matchingCov[i.control,"SettlementID"], pair.treat.settl) 
names(pair.control.settl)[1:2] <- c("ctrl.id", "treat.id")
head(pair.control.settl)  

# Check data frame
settl.check <- pair.control.settl %>% 
  
  left_join(dplyr::select(Settlements, SettlementName, SettlementID), by = c("ctrl.id"="SettlementID")) %>%
  rename(ctrl.settlement = SettlementName) %>%
  left_join(dplyr::select(DiD.data.matchingCov, TimeMarket, Fisher, eth.polarize, lat, long, SettlementID), by = c("ctrl.id" = "SettlementID")) %>% 
  rename(ctrl.TimeMarket = TimeMarket, ctrl.Fisher = Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(dplyr::select(Settlements, SettlementName, SettlementID), by = c("treat.id" = "SettlementID")) %>%
  rename(treat.settlement = SettlementName) %>% 
  left_join(dplyr::select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long, SettlementID), by = c("treat.id" = "SettlementID")) %>% 
  rename(treat.TimeMarket = TimeMarket, treat.Fisher = Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  dplyr::select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket, ctrl.TimeMarket, treat.Fisher, ctrl.Fisher, 
                treat.eth.polarize, ctrl.eth.polarize, pair.dist, TimeMarket.dis:eth.polarize.dis)


head(settl.check)

# ---- 4.3 Create master data frame for impact analysis, DiD.data.SettlMatch ----

DiD.data.SettlMatch <- data.frame()
DiD.data.SettlMatch <- rbind(pair.treat.settl, 
                             pair.control.settl %>% 
                               dplyr::select(-treat.id) %>% 
                               rename(SettlementID = ctrl.id)) %>% 
  arrange(pair.id, SettlementID) 

head(DiD.data.SettlMatch)

DiD.data.SettlMatch <- DiD.data.SettlMatch %>% left_join(DiD.data, by="SettlementID")
DiD.data.SettlMatch <- as.data.frame(DiD.data.SettlMatch)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: PRE-PROCESS IMPACT DATA FRAMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Generate short MPA names, for reference ----

mpa.nam <- mpa.nam %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"Telma",
                                ifelse(MPAID==2,"TNTC",
                                       ifelse(MPAID==3,"Kaimana",
                                              ifelse(MPAID==4,"Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==15,"Alor",
                                                                          ifelse(MPAID==16,"Flotim",
                                                                                 ifelse(MPAID==17,"Kei",
                                                                                        ifelse(MPAID==18,"Koon","")))))))))),
         MPAName_short = factor(MPAName_short,
                                levels = c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool","Alor","Flotim","Kei","Koon"),
                                ordered = T))


# ---- 5.2 Identify variable names for social impact DiD ----

# (noZ indicates that these are not standardized indices when going into the DiD regression function)
varNames_noZ <- c("FSIndex", "MAIndex", "MTIndex", "PAIndex", "SERate")


# ---- 5.3 Identify groups of matched settlements (1:3 matches of treatment to control settlements) ----

groups <- 
  pair.control.settl %>%
  group_by(treat.id) %>%
  summarise(group.id = unique(treat.id),
            pair1 = ctrl.id[1],
            pair2 = ctrl.id[2],
            pair3 = ctrl.id[3],
            pair.id1 = pair.id[1],
            pair.id2 = pair.id[2],
            pair.id3 = pair.id[3]) %>%
  melt(id.vars=c("group.id", "pair.id1", "pair.id2", "pair.id3"), value.name = "SettlementID") %>%
  dplyr::select(-variable) %>% filter(!is.na(SettlementID))

# Prepare data to match back to master data frame
match.to.DiD.data <-
  DiD.data.SettlMatch[,c("SettlementID","pair.id")] %>%
  group_by(SettlementID, pair.id) %>%
  mutate(group.id = groups$group.id[which((groups$pair.id1==pair.id | 
                                             groups$pair.id2==pair.id | 
                                             groups$pair.id3==pair.id) & 
                                            groups$SettlementID==SettlementID)]) %>%
  ungroup()


# ---- 5.4 Add group.id back into the Did.data frame for impact analysis ----

DiD.data.SettlMatch <-
  cbind.data.frame(DiD.data.SettlMatch, match.to.DiD.data[,"group.id"]) %>%
  mutate(SettlementID = as.factor(SettlementID),
         TreatmentF = ifelse(Treatment==0, 0, group.id),
         pair.id = as.factor(pair.id),
         group.id = as.factor(group.id),
         TreatmentF = as.factor(TreatmentF))


# check group list for quality control
group.list <- unique(groups$group.id)

summary(DiD.data.SettlMatch)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: DiD MODEL FOR HETEROGENEOUS IMPACT ACROSS SETTLEMENTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 6.1 Make empty output data frame for t2, t3, and t4 impacts ----

model.out.settlevel <- data.frame()


# ---- 6.2 DiD Regressions: Heterogeneous impact by settlement, t2, t3, and t4 ----
# NOTE: t2 impacts exist for all BHS MPAs, as well as Koon MPA in SBS
# NOTE: t3 impacts exist for SBS MPAs only -- Alor, Flotim, Kei
# NOTE: t4 impacts exist only for BHS MPAs

for (i in varNames_noZ) {
  print(i)
  
  Y <- DiD.data.SettlMatch.filtered[,i]
  w <- DiD.data.SettlMatch.filtered[,"dist.wt"]
  
  regValue <- felm(Y  ~   TreatmentF + yearsPostF +  TreatmentF:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + pair.id | 0 | SettlementID + pair.id + group.id, data = DiD.data.SettlMatch.filtered, exactDOF = TRUE, weights = w)
  

  reg.broom <- tidy(regValue) %>% 
    mutate(keep = ifelse(grepl("TreatmentF", term) & grepl(":yearsPost", term), 1, 0),
           SettlementID = sub("TreatmentF", "", term),
           SettlementID = sub(":yearsPostF*.", "", SettlementID),
           time = paste("t", gsub(".*PostF", "\\1", term), sep = "")) %>%
    filter(keep==1) %>% 
    dplyr::select(-keep) %>%
    mutate(Response = i) %>%
    na.omit()
  

  model.out.settlevel <- rbind(model.out.settlevel, reg.broom)
}


# ---- 6.3 Keep only 2 relevant terms "Treatment:Post1" and "Post1" ----

model.out.settlevel1 <- model.out.settlevel %>% 
  mutate(SettlementID = as.numeric(SettlementID),
         domain=ifelse(Response=="FSIndex"," Health (Food Security)",
                       ifelse(Response=="MAIndex","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) %>%
  left_join(Settlements[,c("SettlementID", "SettlementName", "MPAID")], by = "SettlementID") %>%
  left_join(soc.coord[,c("SettlementID", "lat", "long")], by = "SettlementID") %>%
  mutate(MPAID=ifelse(SettlementID %in% c(113,82,81,83,84), 7,
                      ifelse(SettlementID %in% c(114,115,93,94,92), 8,
                             ifelse(SettlementID %in% c(85:90,95,91), 9, MPAID))))

export(model.out.settlevel1, paste(resultPath, 'settlevel_impacts_1-3match_20210707.csv', sep="/"))


# ---- 6.4 Export settlement-level impacts to resultPath ----

export(model.out.settlevel1, paste(resultPath, paste('settlevel_impacts_1-3match_', format(Sys.Date(), format = "%Y%m%d"), '.csv', sep = ""), sep = "/"))


# check number of settlements from output per MPA, etc.

Num.setts.years <- HHData %>% group_by(MPAID,InterviewYear,MPAName) %>% summarise(num.setts = length(SettlementID))

Num.setts.impacts <- model.out.settlevel1 %>% filter(term=="Impact", Response=="FSIndex") %>% group_by(MPAID) %>% summarise(num.setts = length(SettlementID))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 7: PLOT SETTLEMENT LEVEL IMPACTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 7.1 Define plot theme(s) ----

plot.theme <- theme(plot.title = element_text(size=16,
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
                    plot.subtitle = element_text(size=13,
                                                 face="italic",
                                                 colour="#303030",
                                                 lineheight = 1.2),
                    axis.ticks = element_blank(),
                    panel.background = element_rect(fill="white",
                                                    colour="#909090"),
                    panel.border = element_rect(fill=NA,
                                                size=0.25,
                                                colour="#303030"),
                    panel.grid = element_blank(),
                    plot.margin = margin(t=15,r=20,b=5,l=5,unit="pt"),
                    axis.title = element_text(size=12,
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
                    axis.text = element_text(size=12,
                                             angle=0,
                                             colour="#303030",
                                             lineheight=0.7),
                    legend.position = "right",
                    legend.justification = "right",
                    legend.box.spacing = unit(0.1,"cm"),
                    legend.margin = margin(l=10,unit="pt"))

legends <- guides(colour = guide_legend(title.theme = element_text(face="bold",
                                                                               size=12,
                                                                               angle=0,
                                                                               colour="#505050",
                                                                               lineheight=0.75),
                                                    label.theme = element_text(size=12,
                                                                               angle=0,
                                                                               colour="#505050",
                                                                               lineheight=1.25),
                                                    barheight = 10,
                                                    order = 1))
  
# ---- 7.2 Prep data ----

model.out.settlevel.plotting <- 
  left_join(model.out.settlevel1, mpa.nam[,c("MPAID","MPAName_short")], by = "MPAID") %>%
  mutate(significant = ifelse(p.value<0.01,"<0.01",
                              ifelse(p.value>=0.01 & p.value<0.05, "<0.05", 
                                     ifelse(p.value>=0.05 & p.value<0.1, "<0.1", 
                                            ifelse(p.value>=0.1 & p.value<0.2, "<0.2", "None")))))

significant.colors <- c("<0.01" = "#16A085", "<0.05" = "#DFB306", "<0.1" = "#8E44AD", "<0.2" = "#B03A2E", "None" = "black")


# ---- 7.3 Food security settlement level impacts ----

# GEOM_POINTS
FS.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="FSIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Food Security", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
FS.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="FSIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.1,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Food Security", subtitle = "Settlement level impacts (t3/t4)")


# ---- 7.4 Material assets settlement level impacts ----

# GEOM_POINTS
MA.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Material Assets", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
MA.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Material Assets", subtitle = "Settlement level impacts (t3/t4)")


# ---- 7.5 Marine tenure settlement level impacts ----

# GEOM_POINTS
MT.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MTIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.1,2.2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Marine Tenure", subtitle = "Settlement level impacts (t3/t4)")


# SETT NAME LABELS
MT.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MTIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.1,2.2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Marine Tenure", subtitle = "Settlement level impacts (t3/t4)")


# ---- 7.6 Place attachment settlement level impacts ----

# GEOM_POINTS
PA.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="PAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.3,2.3)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Place Attachment", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
PA.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="PAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.3,2.3)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Place Attachment", subtitle = "Settlement level impacts (t3/t4)")


# ---- 7.7 School enrollment settlement level impacts ----

# GEOM_POINTS
SE.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="SERate_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5),
                     limits = c(-2.6,2.6)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "School Enrollment", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
SE.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="SERate_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),
                     limits = c(-3,3.1)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "School Enrollment", subtitle = "Settlement level impacts (t3/t4)")


# ---- 7.8 Export plots ----

# Create plot output directory
dir.create(paste(resultPath, paste(format(Sys.Date(), format = "%Y%m%d"), "settimpacts_plots", sep = "_"), sep = "/"))
plot.resultPath <- paste(resultPath, paste(format(Sys.Date(), format = "%Y%m%d"), "settimpacts_plots", sep = "_"), sep = "/")

# GEOM_POINT plots
png(paste(resultPath,"FS.sett.1-3match.impacts.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(FS.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"MA.sett.1-3match.impacts.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(MA.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"MT.sett.1-3match.impacts.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(MT.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"PA.sett.1-3match.impacts.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(PA.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"SE.sett.1-3match.impacts.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(SE.sett.impacts.byMPA)
dev.off()


# SETT NAMES PLOTS
png(paste(resultPath,"FS.sett.1-3match.impacts.namelabels.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(FS.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"MA.sett.1-3match.impacts.namelabels.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(MA.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"MT.sett.1-3match.impacts.namelabels.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(MT.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"PA.sett.1-3match.impacts.namelabels.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(PA.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"SE.sett.1-3match.impacts.namelabels.png", sep = "/"),
    units = "in", height = 6, width = 8, res = 400)
plot(SE.sett.impacts.byMPA.settnames)
dev.off()
