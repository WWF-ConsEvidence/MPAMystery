##---MPA Social Impacts and Equity---##
##---September 2019---##
##---Duong Le---##
##---Data: BHS, with provision to incorporate SBS---##

##---0. Sourcing and creating data frame for status/trend and impact
##---1. Generating subGroup indicators
##---2. Generating household demographic indicators
##---3. Conducting Principle Component Analysis (PCA) for Poverty Index construction
##---4. Generate new ethnic dominance indicator (by marine tenureship)
##---5. Calculate GINI for baseline poverty index 
##---6. Export data frame to Excel for exploratory Stata analysis

##---7. Evidence for baseline inequality in asset wealth (MAIndex_pca)

#--- 8. Reduced-form impact plots on poverty index & perceived changes in econ wellbeing (seperate for each subgroup)
##------8a. set up settlement-matching data frame (coarsematching and 1-to-3 settleMatch)
##------8b. aggreagte subgroup impacts on inequality in poverty exposure (discussed in inequality typology format)
##------8c. changes in poverty impact overtime (t2 and t4; using BHS data only)

# -- 9. aggregate regression on GINI index

#--- 10. Step 3a. (conflict/participation/community cohesiveness splits)
#--- 10. Step 3b - causal explanation using econ trend reason 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 0. SOURCING ---- 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##---0. Sourcing and creating data frame for status/trend and impact 


#setwd("D:/Dropbox/MPA_research/MPAMystery/")
source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_tbl_MPA.xlsx")
pacman::p_load(lfe, cowplot, stargazer, broom, qvalue, psych, writexl, factoextra, ineq, sf, plotly, tidyverse)


##--changing resultPath folder to where you want to store all subsequent analysis results (plots, tables, etc.)
resultPath <- "D:/Dropbox/MPA_research/"
#resultPath <- "C:/Users/dtl20/Dropbox/MPA_research/"

soc.coord<- import("x_Flat_data_files/1_Social/Inputs/soc.coord.province.xlsx")

iso.province <- import("x_Flat_data_files/1_Social/Inputs/Ethnic_byProvince_Indo.xlsx") %>%
  mutate(ProvinceName = Province, eth.iso = iso, indigenous = 1) %>%
  select(eth.iso, ProvinceName, Ethno, indigenous)

# --- DiD dataframe 
DiD.data <- match.covariate %>% 
  filter(!is.na(HouseholdID)) %>% 
  left_join(select(HHData,DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, MAIndex:FSIndex ,SERate ,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         Post = as.factor(ifelse(yearsPost==0,0,1)),
         Control = ifelse(Treatment==1,0,1), 
         
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear), 
         Fisher=ifelse(PrimaryLivelihood==3 | SecondaryLivelihood==3 | TertiaryLivelihood==3,1,0), 
         Male = IndividualGender, 
         PrimaryLivelihood.bin = as.factor(ifelse(PrimaryLivelihood==1,1,
                                                  ifelse(PrimaryLivelihood==2,2,
                                                         ifelse(PrimaryLivelihood==3|PrimaryLivelihood==4,3,
                                                                ifelse(PrimaryLivelihood==6|PrimaryLivelihood==7,4,5)))))) %>% 
  filter(!is.na(IndividualGender)) %>% 
  left_join(soc.coord,by="SettlementID") %>% 
  left_join(select(HH.eth, HouseholdID, eth.iso), by=c("HouseholdID")) %>%
  left_join(iso.province, by=c("eth.iso","ProvinceName")) %>%
  mutate(indigenous = ifelse(is.na(indigenous),0,1),
         indigenous = ifelse(is.na(eth.iso),NA,indigenous))


# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other


# --- Filter to look at 6 BHS and 4 SBS MPAs (with t2 and/or t4 data) --- Jan 2020
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6, 15:18))

DiD.data.test <- DiD.data %>% 
  filter(MPAID%in%c(1:6))

#plyr::count(DiD.data, "indigenous")

# --- two additional outcome variables --- Feb 2020
DiD.data <- DiD.data %>% 
  mutate(MTIndex_AccHarv = RightsAccess + RightsHarvest, 
         MTIndex_ManExcTrans= RightsManage + RightsExclude + RightsTransfer, 
         SocialConflict_increase = ifelse(is.na(SocialConflict), NA, 
                                          ifelse(SocialConflict==1|SocialConflict==2, 1, 0)), 
         EconTrend_decrease = ifelse(is.na(EconStatusTrend), NA, 
                                     ifelse(EconStatusTrend==1|EconStatusTrend==2, 1, 0)))


# Prepare ethnic polarization index 
HH.eth <- HH.eth %>% 
  left_join(select(DiD.data,HouseholdID, SettlementID, MPAID, yearsPost),by="HouseholdID")

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


## --- retaining the raw HHH.age (The IndividualAge is currently already categorized)
HH.age.raw <- IndDemos %>%
  filter(RelationHHH==0) %>%
  mutate(IndividualAge_raw = IndividualAge) %>% 
  select(HouseholdID,IndividualAge_raw) %>%
  left_join(select(DiD.data,HouseholdID,yearsPost),by="HouseholdID") %>%
  select(HouseholdID,IndividualAge_raw) %>% 
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
         CarTruck = CarTruck/11, 
         Boat = BoatNoMotor + BoatOutboard + BoatInboard) %>% 
  
  mutate(Entertain_yes = ifelse(Entertain>0,1,0),
         PhoneCombined_yes = ifelse(PhoneCombined>0,1,0),
         Satellite_yes = ifelse(Satellite>0,1,0),
         TV_yes = ifelse(TV>0,1,0),
         Generator_yes = ifelse(Generator>0,1,0),
         BoatNoMotor_yes = ifelse(BoatNoMotor>0,1,0),
         BoatOutboard_yes = ifelse(BoatOutboard>0,1,0),
         BoatInboard_yes = ifelse(BoatInboard>0,1,0),
         Boat_yes = ifelse(Boat>0,1,0),
         Bicycle_yes = ifelse(Bicycle>0,1,0),
         Motorcycle_yes = ifelse(Motorcycle>0,1,0),
         CarTruck_yes = ifelse(CarTruck>0,1,0)) %>%
  
  mutate(Entertain_no = ifelse(Entertain==0,1,0),
         PhoneCombined_no = ifelse(PhoneCombined==0,1,0),
         Satellite_no = ifelse(Satellite==0,1,0),
         TV_no = ifelse(TV==0,1,0),
         Generator_no = ifelse(Generator==0,1,0),
         BoatNoMotor_no = ifelse(BoatNoMotor==0,1,0),
         BoatOutboard_no = ifelse(BoatOutboard==0,1,0),
         BoatInboard_no = ifelse(BoatInboard==0,1,0),
         Boat_no = ifelse(Boat==0,1,0),
         Bicycle_no = ifelse(Bicycle==0,1,0),
         Motorcycle_no = ifelse(Motorcycle==0,1,0),
         CarTruck_no = ifelse(CarTruck==0,1,0))  


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



# ---- Categorizing cooking fuel indicator 
plyr::count(HHData, "CookingFuel")
cooking.fuel.df <- HHData %>% 
  select(HouseholdID, CookingFuel) %>% 
  mutate(CookingFuel=ifelse(CookingFuel==0| CookingFuel>=998, NA, CookingFuel),
         fuel.elec=ifelse(CookingFuel==1,1,0),
         fuel.oil=ifelse(CookingFuel==2,1,0),
         fuel.wood=ifelse(CookingFuel==3,1,0),
         fuel.other=ifelse(CookingFuel==4|CookingFuel==5|CookingFuel==6,1,0))

DiD.data <- DiD.data %>% 
  select(-CookingFuel.Biomass) %>% 
  left_join(cooking.fuel.df, by=c("HouseholdID")) %>% 
  mutate(fuel.biomass=ifelse(fuel.elec==0 & fuel.oil==0, 1, 0),
         fuel.nonBiomass=ifelse(fuel.elec==1 | fuel.oil==1, 1, 0))

# --- summary(DiD.data)
DiD.data.summary <- DiD.data %>% 
  select(MPAID,MonitoringYear,InterviewYear, SettlementID, yearsPost, MPAName, Treatment) %>% 
  group_by(MPAID,MonitoringYear, Treatment) %>%
  summarise(yearsPost = mean(yearsPost),
            InterviewYear = first(InterviewYear), 
            MPAName = first(MPAName),
            HH.count = n()) 
#write_xlsx(DiD.data.summary, path = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/summary1.xlsx"), col_names = TRUE, format_headers = TRUE)


DiD.data.summary.Settl <- DiD.data %>% 
  select(MPAID,MonitoringYear,InterviewYear, SettlementID, yearsPost, MPAName, Treatment) %>% 
  filter(yearsPost==0) %>% 
  group_by(MPAID, SettlementID, Treatment) %>% 
  summarise(Setl.count = n()) %>% 
  group_by(MPAID, Treatment) %>% 
  summarise(Setl.count = n()) 
#write_xlsx(DiD.data.summary.Settl, path = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/summary2.xlsx"), col_names = TRUE, format_headers = TRUE)



# calculate Z scores (standardized values for each of the Big Five and the sub_asset groups)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, CarTruck:Generator, Boat, fuel.elec:fuel.biomass, MTIndex_AccHarv, MTIndex_ManExcTrans, SocialConflict, SocialConflict_increase), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()
# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 1. GENERATING KEY INDICATORS ---- 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- 1a. Generating subGroup indicators ---- 
#---Import ethnic and education look up tables 
ethnic.lkp<- import("x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx")
education.lkp <- import("x_Flat_data_files/1_Social/Inputs/education_lkp_BHS.xlsx")

#ethnic.indigenous.lkp<- import('D:/Dropbox/MPA_research/Paper 1-MPA and Equity/eth_iso_indigenous_expertCoded.xls')
#ethnic.indigenous.lkp<- import('C:/Users/dtl20/Dropbox/MPA_research/Paper 1-MPA and Equity/eth_iso_indigenous_expertCoded.xls')


# ---Create functions
# Function to remove all white space in string variables
trim <- function(x) gsub("^\\s+|\\s+$","",x)

# Function to clean string variables (lower case, remove punctuation)
str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
    trim()
}


# Education level of HH members
# some duplicates in education table (NAs, perhaps white spaces), filtering out these here
education.lkp1 <- education.lkp %>% 
  distinct(IndividualEducation,ed.level,.keep_all = T) %>%
  filter(ed.level!="NA")


#---subGroup creation
###Indicator for Fisher and Male (dummies)
DiD.data <- DiD.data %>% 
  mutate(Male=ifelse(IndividualGender==1,1,0),
         Fisher=ifelse(PrimaryLivelihood==3,1,0))

# Indicator YrResident.above in each seascape-year 
YrResident.median <- DiD.data %>%
  group_by(yearsPost, MPAID) %>%
  summarise(YrResident.median = median(YrResident, na.rm = T))

DiD.data <- DiD.data %>% 
  left_join(YrResident.median, by = c("yearsPost","MPAID")) %>% 
  mutate(yrResident.above=ifelse(YrResident>=YrResident.median,1,0))

# Indicator age.retired in each seascape-year (factors)
DiD.data <- DiD.data %>% 
  mutate(age.retired = ifelse(IndividualAge_raw>=55,1,0))

# Indicator adat group if have RightExclude and/or RightTransfer
DiD.data <- DiD.data %>% 
  mutate(adat = ifelse(RightsExclude==1|RightsTransfer==1,1,0))

# Indicator adat2 group if have RightExclude and/or RightTransfer AND the eth.iso is deemed indigineous by experts (using ethnic.indigenous.df)
# DiD.data <- DiD.data %>% 
#   left_join(ethnic.indigenous.lkp, by=c("eth.iso"="iso")) %>% 
#   mutate(adat_1 = ifelse(  (adat==1 & (MPAID%in%1:6) & indigenous_BHS==1) | (adat==1 & (MPAID%in%15:18) & indigenous_SBS==1) ,1, 0 ))
#   

DiD.data <- DiD.data %>% 
  mutate(adat_1 = ifelse( (adat==1 & indigenous==1) , 1, 0 ))


# ---- 1b. Generating social indicators ---- 

IndDemos <- IndDemos %>% 
  left_join(education.lkp1, by=c("IndividualEducation")) %>% 
  ## Household-demographic factors: Household composition
  mutate(All.dum=1, 
         Male.dum = ifelse(IndividualGender==1,1,0),
         Female.dum = ifelse(IndividualGender==0,1,0),
         
         All.U15.dum = ifelse(IndividualAge<15,1,0),
         Male.U15.dum = ifelse(IndividualAge<15 & IndividualGender==1,1,0),
         Female.U15.dum = ifelse(IndividualAge<15 & IndividualGender==0,1,0),
         
         All.U18.dum = ifelse(IndividualAge<18,1,0),
         Male.U18.dum = ifelse(IndividualAge<18 & IndividualGender==1,1,0),
         Female.U18.dum = ifelse(IndividualAge<18 & IndividualGender==0,1,0),
         
         All.baby.dum = ifelse(IndividualAge<6,1,0),
         Male.baby.dum = ifelse(IndividualAge<6 & IndividualGender==1,1,0),
         Female.baby.dum = ifelse(IndividualAge<6 & IndividualGender==0,1,0),
         
         All.schoolAge.dum = ifelse(IndividualAge>=6 & IndividualAge<18,1,0),
         Male.schoolAge.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualGender==1,1,0),
         Female.schoolAge.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualGender==0,1,0),
         
         All.schoolAge.enrolled.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualEnrolled==1,1,0),
         Male.schoolAge.enrolled.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualEnrolled==1 & IndividualGender==1,1,0),
         Female.schoolAge.enrolled.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualEnrolled==1 & IndividualGender==0,1,0),
         
         All.workingAge.dum = ifelse(IndividualAge>=18 & IndividualAge<=65,1,0),
         Male.workingAge.dum = ifelse(IndividualAge>=18 & IndividualAge<=65 & IndividualGender==1,1,0),
         Female.workingAge.dum = ifelse(IndividualAge>=18 & IndividualAge<=65 & IndividualGender==0,1,0),
         
         All.elder.dum = ifelse(IndividualAge>65,1,0),
         Male.elder.dum = ifelse(IndividualAge>65,1,0),
         Female.elder.dum = ifelse(IndividualAge>65,1,0)) %>% 
  
  
  ## Household-demographic factors: family relationship 
  mutate(is.HHH = ifelse(RelationHHH==0,1,0),
         is.HHH.spouse = ifelse(RelationHHH==1,1,0),
         
         is.HHH.child = ifelse(RelationHHH==2 | RelationHHH==6 | RelationHHH==11,1,0),
         is.HHH.baby = ifelse((RelationHHH==2 | RelationHHH==6 | RelationHHH==11) & IndividualAge<6,1,0),
         is.HHH.fosterChild = ifelse(RelationHHH==11,1,0),
         
         is.HHH.parent = ifelse(RelationHHH==3 | RelationHHH==5,1,0),
         is.HHH.grandChild = ifelse(RelationHHH==4,1,0),
         is.HHH.relative = ifelse(RelationHHH==7 | RelationHHH==8 | RelationHHH==9 | RelationHHH==10 | RelationHHH==12,1,0),
         is.HHH.notRelated = ifelse(RelationHHH==13,1,0)) %>% 
  
  
  ## Household-demographic factors: marriage structure (husband-wife relationship)
  mutate(HHH.gender = ifelse(RelationHHH == 0, IndividualGender, NA),
         HHH.ed.level = ifelse(RelationHHH == 0, ed.level, NA),
         HHH.age = ifelse(RelationHHH == 0, IndividualAge, NA),
         
         spouse.gender = ifelse(RelationHHH == 1, IndividualGender, NA),
         spouse.ed.level = ifelse(RelationHHH == 1, ed.level, NA),
         spouse.age = ifelse(RelationHHH == 1, IndividualAge, NA)) 


## Get HH.Demos and HH.Mariage, before combining them for the household-demographic indicator dataframe 
HH.Demos <- data.frame()

HH.Demos <- IndDemos %>% 
  select(HouseholdID, All.dum:Female.elder.dum, is.HHH:is.HHH.notRelated, HHH.gender:spouse.age) %>% 
  group_by(HouseholdID) %>% 
  summarise(HH.Total.count = sum(All.dum),
            HH.Male.count = sum(Male.dum),
            HH.Female.count = sum(Female.dum),
            
            HH.U15.count = sum(All.U15.dum),
            HH.Male.U15.count = sum(Male.U15.dum),
            HH.Female.U15.count = sum(Female.U15.dum),
            
            HH.U18.count = sum(All.U18.dum),
            HH.Male.U18.count = sum(Male.U18.dum),
            HH.Female.U18.count = sum(Female.U18.dum),
            
            HH.baby.count = sum(All.baby.dum),
            HH.Male.baby.count = sum(Male.baby.dum),
            HH.Female.baby.count = sum(Female.baby.dum),
            
            HH.schoolAge.count = sum(All.schoolAge.dum),
            HH.Male.schoolAge.count = sum(Male.schoolAge.dum),
            HH.Female.schoolAge.count = sum(Female.schoolAge.dum),
            
            HH.schoolAge.enrol.count = sum(All.schoolAge.enrolled.dum),
            HH.Male.schoolAge.enrol.count = sum(Male.schoolAge.enrolled.dum),
            HH.Female.schoolAge.enrol.count = sum(Female.schoolAge.enrolled.dum),
            
            HH.workingAge.count = sum(All.workingAge.dum),
            HH.Male.workingAge.count = sum(Male.workingAge.dum),
            HH.Female.workingAge.count = sum(Female.workingAge.dum),
            
            HH.elder.count = sum(All.elder.dum),
            HH.Male.elder.count = sum(Male.elder.dum),
            HH.Female.elder.count = sum(Female.elder.dum),
            
            HHH.count = sum(is.HHH),
            HHH.spouse.count = sum(is.HHH.spouse),
            HHH.child.count = sum(is.HHH.child),
            HHH.baby.count = sum(is.HHH.baby),
            HHH.childFoster.count = sum(is.HHH.fosterChild),
            HHH.parent.count = sum(is.HHH.parent),
            HHH.grandChild.count = sum(is.HHH.grandChild),
            HHH.relative.count = sum(is.HHH.relative),
            HHH.notRelated.count = sum(is.HHH.notRelated),
            
            HHH.gender = sum(HHH.gender,na.rm=TRUE), 
            HHH.age = sum(HHH.age,na.rm=TRUE),
            HHH.ed.level = sum(HHH.ed.level,na.rm=TRUE),
            
            spouse.gender = sum(spouse.gender,na.rm=TRUE), 
            spouse.age = sum(spouse.age,na.rm=TRUE),
            spouse.ed.level = sum(spouse.ed.level,na.rm=TRUE),
            
            
            ## Generate important indicators re: household's demographic and compostion
            HHdemo.single.liveAlone = ifelse(HH.Total.count==1,1,0),
            HHdemo.single.withChildren = ifelse(HHH.age<=65 & HHH.spouse.count==0 & HHH.child.count>0,1,0),
            HHdemo.single.withElder = ifelse(HHH.age<=65 & HHH.spouse.count==0 & HH.elder.count>0,1,0),
            
            HHdemo.married.withChildren = ifelse(HHH.age<=65 & HHH.spouse.count==1 & HHH.child.count>0,1,0),
            HHdemo.married.withBaby = ifelse(HHH.age<=65 & HHH.spouse.count==1 & HH.baby.count>0,1,0),
            HHdemo.married.withChildMale = ifelse(HHH.age<=65 & HHH.spouse.count==1 & HHH.child.count>0 & HH.baby.count==0 & HH.Male.schoolAge.count>HH.Female.schoolAge.count,1,0),
            HHdemo.married.multiGen = ifelse((HHH.child.count>0 & HHH.parent.count>0) | HHH.grandChild.count>0,1,0),
            
            #Generate indicators re: marriage characteristics
            HHdemo.Male.headed = ifelse(HHH.gender==1,1,0),
            HHdemo.Husband.Older = ifelse((HHH.age>spouse.age) & HHH.gender==1,1,0),
            HHdemo.Husband.HigherEd = ifelse((HHH.ed.level>spouse.ed.level) & HHH.gender==1,1,0),
            
            #Generate indicators re: having dependent members (children<15 and elderly>65)
            HHdemo.have.dependent = ifelse(HH.elder.count >0 | HH.U15.count>0,1,0))




#merge HH.Demos & Settlement.demo into the main DiD.data
DiD.data <- DiD.data %>% 
  left_join(HH.Demos, by = "HouseholdID") 

count.MPA.IDs <- DiD.data %>% 
  group_by(MPAID, Treatment) %>%
  summarise(MPA=length(MPAID))
count.MPA.IDs


# ---- 1C. Generate ethnic dominance indicator ----

# some duplicates in ethnicity table (NAs), filtering out these here
# ethnic.lkp1 <- ethnic.lkp %>% 
#   distinct(std.eth.str,eth.iso,.keep_all = T) %>%
#   filter(eth.iso!="NA")
# filter(!ethnic.id%in%c(2734,2813,5422,5425,5643)) # select out the specific five duplicates

# HH.eth.new <- HHData %>% 
#   select(HouseholdID,PaternalEthnicity, MTIndex, YrResident, MonitoringYear, SettlementID) %>% 
#   mutate(PaternalEthnicity=str_clean(PaternalEthnicity)) %>% 
#   left_join(ethnic.lkp1, by=c("PaternalEthnicity"="std.eth.str")) %>% 
#   mutate(SettlYear=paste0(MonitoringYear,"_",SettlementID))
# 
# 
# # this code gives you the ethnicity associated with (1) highest MT and (2) MT>=4, for each settlement at each sampling period
# max.eth.highest.Tenure <- HH.eth.new %>%
#   group_by(SettlYear,eth.iso) %>%
#   summarise(MT.mean=mean(MTIndex), MT.median=median(MTIndex)) %>% 
#   top_n(1, MT.mean) 
# 
# 
# HH.eth.new$dom.eth <- NA
# # assign dominant ethnicity in a loop will assign a 0 if parentalEthinicity==NA
# for (i in unique(HH.eth.new$SettlYear)){
#   max.eth.dom <-  max.eth.highest.Tenure$eth.iso[max.eth.highest.Tenure$SettlYear==i]
#   HH.eth.new$dom.eth[HH.eth.new$SettlYear==i] <- ifelse(HH.eth.new$eth.iso[HH.eth.new$SettlYear==i]%in%max.eth.dom,1,0)
# }
# 
# HH.eth.new <- HH.eth.new %>% 
#   mutate(ethDom.highest.MT=dom.eth) %>% 
#   select(HouseholdID, ethDom.highest.MT) 
# 
# 
# #merge HH.eth.new into the main DiD.data
# DiD.data <- DiD.data %>% 
#   left_join(HH.eth.new, by = "HouseholdID") 
# summary(DiD.data)
# 
# 
# # calculate p scores
# pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + ethDom.highest.MT + YearsResident + IndividualGender + IndividualAge,
#               data=DiD.data)$fitted.values
# 
# DiD.data <- cbind(DiD.data,pscore)  




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 2. PCA CONSTRUCTION---- 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##---2. Conducting Principle Component Analysis (PCA) for Asset weights

##---raw 11 assets (3 separate boats) and 4 fuel types
# poverty.df.1 <- DiD.data %>%
#   subset(RemoveMA=="No") %>% 
#   select(Bicycle_z, Motorcycle_z, BoatNoMotor_z, BoatOutboard_z, BoatInboard_z, PhoneCombined_z, TV_z, Entertain_z, Satellite_z, Generator_z, fuel.elec_z, fuel.oil_z, fuel.wood_z, fuel.other_z) %>% 
#   na.omit()
# 
# ##---raw 9 assets (3 separate boats) and 4 fuel types
# poverty.df.2 <- DiD.data %>%
#   subset(RemoveMA=="No") %>% 
#   select(Bicycle_z, Motorcycle_z, Boat_z, PhoneCombined_z, TV_z, Entertain_z, Satellite_z, Generator_z, fuel.elec_z, fuel.oil_z, fuel.wood_z, fuel.other_z) %>% 
#   na.omit()


##---dummies (negative) only (no boat)
MA.df.2 <- DiD.data %>%
  subset(RemoveMA=="No") %>%
  select(Entertain_z, PhoneCombined_z, Satellite_z, TV_z, Generator_z, Bicycle_z, Motorcycle_z, CarTruck_z, Boat_z) %>%                  
  na.omit()


##---dummies (negative) only (with boat)
poverty.df.3 <- DiD.data %>%
  subset(RemoveMA=="No") %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, CarTruck_yes, Boat_yes, fuel.nonBiomass) %>%                  
  na.omit()


##--function to get weights: 
# Function to clean string variables (lower case, remove punctuation)
get_pca_weight <- function(df) {
  require(dplyr)
  
  ##---Using z values: Perform PCA to get eigenvalues (i.e. loading factors) for each asset's weight-----##
  poverty.pca.z <- prcomp(df, scale = FALSE)
  
  #Get eigenvalues, or variance percentage, for each principle components (i.e. how well each of PCs contribute in explaning the total variance in our asset data set)
  asset.eig.val <- get_eigenvalue(poverty.pca.z)
  asset.eig.val
  fviz_eig(poverty.pca.z) #a scree plot to illustrate the above
  asset.eig.val.keep <- asset.eig.val %>% 
    #subset(eigenvalue>=1) %>% 
    mutate(pc.contrib = variance.percent/sum(variance.percent)) #keep only PCs with eigenvalue >=1 and find % of explained variance that each PC contributes
  asset.eig.val.keep <- as.data.frame(asset.eig.val.keep)
  
  #Get the contribution of each asset items in each PCs 
  #Importantly, literature use (1) the contribution factors associated with the first component (i.e. Dimension 1) 
  #to construct the asset weights in computing composite asset-based index, or (2) all Dimensions with eigenvalue >=1
  asset.weight.frame <- get_pca_var(poverty.pca.z)$contrib
  asset.weight.frame <- asset.weight.frame[ ,1:nrow(asset.eig.val.keep)] 
  asset.weight.frame <- as.data.frame(t(asset.weight.frame))
  asset.weight.frame <- cbind(asset.weight.frame,asset.eig.val.keep) 
  
  #Extract weights (scoring factors) using the variance contribution of each assets to the First PC
  asset.weight.pc1 <- asset.weight.frame[1,] %>% 
    select(-c("eigenvalue", "variance.percent", "cumulative.variance.percent", "pc.contrib")) 
  return(asset.weight.pc1)
}

#Produce 2-dimensional PCA plots (i.e. looking at the first 2 PCs) to visualize the correlation of each assets in contributing to the first 2 PCs
poverty.pca.z <- prcomp(poverty.df.3, scale = FALSE)
fviz_pca_var(poverty.pca.z, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseSCale/povertyAllev_PCA_dimension.png"),width = 12, height = 8)

##--- excecute fn to get_pca_weights to get weights
asset.weight.pc1 <- get_pca_weight(poverty.df.3)

CarTruck_w <- (1/100) * asset.weight.pc1$CarTruck_yes
Bicycle_w <- (1/100) * asset.weight.pc1$Bicycle_yes
Motorcycle_w <- (1/100) * asset.weight.pc1$Motorcycle_yes
PhoneCombined_w <- (1/100) * asset.weight.pc1$PhoneCombined_yes
TV_w <- (1/100) * asset.weight.pc1$TV_yes
Entertain_w <- (1/100) * asset.weight.pc1$Entertain_yes
Satellite_w <- (1/100) * asset.weight.pc1$Satellite_yes
Generator_w <- (1/100) * asset.weight.pc1$Generator_yes
fuel.nonBiomass_w <- (1/100) * asset.weight.pc1$fuel.nonBiomass
Boat_w <- (1/100) * asset.weight.pc1$Boat_yes


#----------##
DiD.data <- DiD.data %>% 
  mutate(PovertyIndex_pca = Entertain_w*Entertain_yes + PhoneCombined_w*PhoneCombined_yes + Satellite_w*Satellite_yes + TV_w*TV_yes +
           Generator_w*Generator_yes + Bicycle_w*Bicycle_yes + Motorcycle_w*Motorcycle_yes + CarTruck_w*CarTruck_yes + Boat_w*Boat_yes + fuel.nonBiomass_w*fuel.nonBiomass)     


# histogram of PovertyIndex_pca
hist(DiD.data$PovertyIndex_pca, main="Distribution of Asset-based Poverty", xlab="Poverty Index (PCA)")
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseSCale/povertyIndex_PCA_histo.png"),width = 12, height = 8)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ---- 3. GINI INDEX CALCULATION ----

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##---3. Calculate GINI for baseline MA for each settlement at each sampling period

#install.packages("ineq")
library(ineq)

PovertyIndex_GINI_Settl <- DiD.data %>% group_by(SettlementID, yearsPost) %>% 
  summarise(PovertyIndex_GINI_Settl = ineq(PovertyIndex_pca,type="Gini")) 

DiD.data <- DiD.data %>% 
  left_join(PovertyIndex_GINI_Settl, by=c("SettlementID", "yearsPost"))


#Plot the Lorenz curve 
DiD.data.pre <- DiD.data %>% filter(yearsPost==0) 
DiD.data.post <- DiD.data %>% filter(yearsPost%in% c(3:4)) 

Lc.pre <- Lc(DiD.data.pre$PovertyIndex_pca)
Lc.post <- Lc(DiD.data.post$PovertyIndex_pca)


plot(Lc.pre, col="red",lwd=2, main="Lorenz Curve of Poverty Alleviation Index Score",  
     xlab="Household Percentile by Poverty Alleviation Score",  ylab="Cumulative Poverty Alleviation Score")
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseSCale/povertyAllev_GINI_Lorenz.png"),width = 12, height = 8)


#Plot settlement-level GINI histogram
DiD.data.GINI <- DiD.data %>% 
  mutate(seascape = ifelse(MPAID %in% c(1:6), "BHS", "SBS")) %>% 
  group_by(SettlementID, yearsPost) %>% 
  summarise(PovertyIndex_GINI_Settl=first(PovertyIndex_GINI_Settl),
            seascape = first(seascape))


Mean <- DiD.data.GINI %>% group_by(seascape, yearsPost) %>% summarise(mean = mean(PovertyIndex_GINI_Settl, na.rm=TRUE))

#Additional test for seascapes
DiD.data.BHS.pre <- DiD.data %>% filter(yearsPost==0, MPAID %in% c(1:6)) 
DiD.data.BHS.post <- DiD.data %>% filter(yearsPost==4, MPAID %in% c(1:6)) 
DiD.data.SBS.pre <- DiD.data %>% filter(yearsPost==0, MPAID %in% c(15:18)) 
DiD.data.SBS.post <- DiD.data %>% filter(yearsPost>0, MPAID %in% c(15:18)) 

Lc.BHS.pre <- Lc(DiD.data.BHS.pre$PovertyIndex_pca)
Lc.BHS.post <- Lc(DiD.data.BHS.post$PovertyIndex_pca)
Lc.SBS.pre <- Lc(DiD.data.SBS.pre$PovertyIndex_pca)
Lc.SBS.post <- Lc(DiD.data.SBS.post$PovertyIndex_pca)

#jpeg(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/PovertyIndex_GINI_LorenzCurve_SBS.jpg"))
plot(Lc.SBS.pre, col="red",lwd=2, main="Lorenz Curve of Poverty Exposure Score: SBS",  
     xlab="Household Percentile by Poverty Exposure Score",  ylab="Cumulative Poverty Exposure")
lines(Lc.SBS.post, col="blue")
#dev.off()

#jpeg(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/PovertyIndex_GINI_LorenzCurve_BHS.jpg"))
plot(Lc.BHS.pre, col="red",lwd=2, main="Lorenz Curve of Poverty Exposure Score: BHS",  
     xlab="Household Percentile by Poverty Exposure Score",  ylab="Cumulative Poverty Exposure")
lines(Lc.BHS.post, col="blue")
#dev.off()


hist(DiD.data.GINI$PovertyIndex_GINI_Settl, main="Distribution of Poverty-alleviation GINI Index", xlab="GINI Index (Settlement-level)")
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseSCale/histogram_povertyGINI_settlementLvl.png"),width = 12, height = 8)




##---Export data frame to Excel for exploratory Stata analysis
# 
# #Export to txt file
# library(foreign)
# #install.packages("writexl")
# library(writexl)
# write.table(DiD.data, "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/MPA_HouseholdDemo_data.txt", sep="\t")
# write_xlsx(DiD.data, path = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/MPA_HouseholdDemo_data", col_names = TRUE, format_headers = TRUE)
# #source("2_Functions/my_summary_plot_functions.R")

DiD.data <- DiD.data %>% 
  mutate(yearsPost.recode=ifelse(yearsPost==0,"Baseline",
                                 ifelse(yearsPost%in%2:3,"First Postline",
                                        "Second Postline")))





# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ---- 4. SUMMARY STATISTICS & DESCRIPTIVE EVIDENCE ----

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# stargazer(subset(DiD.data[c("MAIndex_pca","MAIndex_pca.1","Fisher", "Female", "Ethnic", "Indigenous",
#                             "TimeMarket","n.child","ed.level", 
#                             "ed.no", "ed.primary", "ed.high", "ed.college",
#                             "FSIndex","MTIndex","PAIndex")], DiD.data$yearsPost==0),
#           title="Descriptive statistics: Baseline", type = "text", digits=2, out="D:/Dropbox/MPA_research/Paper 1-MPA and Equity/results/tables/summary Stat/baseline-all.txt")
# 
# 


## ---- 4a. Evidence for baseline inequality in Poverty (PovertyIndex_pca) ----
#Define 4 subgroups (and construct baseline daaset) Male, Fisher, age.retired, yrResident.above
DiD.data.baseline <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  mutate(Gender = ifelse(Male==1,"Male","Female"),
         Livelihood = ifelse(Fisher==1,"Fisher","non-Fisher"), 
         Livelihood_nonFisher = ifelse(Fisher==0,"Non-Fisher","Fisher"), 
         #Ethnicity = ifelse(ethDom.highest.MT==1,"Dominant", " Non-dominant"),
         Indigenousness = ifelse(yrResident.above==1,"Indigenous", "non-Indigenous"),
         Age_Cohort= ifelse(age.retired==1,"Retirement-age", "Working-age"),
         Adat_label= ifelse(adat==1,"Adat Right-holder", "Non-Adat"),
         Ada_1_label= ifelse(adat_1==1,"Adat Right-holder", "Non-Adat"))


DiD.data.baseline <- DiD.data.baseline %>% 
  mutate(ed.level.round = round(ed.level, 0),
         ed.level.round = ifelse(ed.level.round<2,2,ed.level.round),
         ed.level.label = ifelse(ed.level.round==2, "1. Primary or Below",
                                 ifelse(ed.level.round==3, "2. Junior Secondary",
                                        ifelse(ed.level.round==4, "3. Senior Secondary", "4. College or above"))))

plyr::count(DiD.data.baseline, "ed.level.round")
##-------Correlation Matrix for the 4 subGroups ---##
#install.packages("Hmisc")
#install.packages("corrplot")

corr.subGroups.baseline.data <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  mutate(Female=ifelse(Male==0,1,0)) %>% 
  select(Female, Fisher, yrResident.above, age.retired)

library("Hmisc")
correlation.subGroups <- rcorr(as.matrix(corr.subGroups.baseline.data))
correlation.subGroups


library(corrplot)
corrplot(correlation.subGroups$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 0)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable to be summarized
# groupnames : vector of column names to be used as grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

##---------------------------------------------------------------------------##
## ---- 4b. Summary statistics tables ----


DiD.data.sumstat <- DiD.data.baseline %>%
  mutate(age.working = 1- age.retired) %>% 
  select(Treatment, PovertyIndex_pca, EconStatusTrend, EconTrend_decrease, TimeMarket, n.child, ed.level, dom.eth, YrResident, IndividualAge, IndividualAge_raw, ed.level.round, FSIndex, Male, Fisher, yrResident.above, age.retired, age.working, adat, adat_1, NumMarineGroup, NumOtherGroup) %>% 
  mutate(NumTotalGroup = NumMarineGroup + NumOtherGroup,
         NumTotalGroup.Female = ifelse(Male==0, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.Fisher = ifelse(Fisher==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.Age = ifelse(age.retired==1, NumMarineGroup + NumOtherGroup, NA), 
         NumTotalGroup.Indigenous = ifelse(yrResident.above==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.Adat = ifelse(adat==1, NumMarineGroup + NumOtherGroup, NA),
         
         Parti.dum = ifelse(NumTotalGroup>0, 1, ifelse(NumTotalGroup==0, 0, NA)),
         Parti.Female.dum = ifelse(Male==0 & NumTotalGroup>0, 1, ifelse(Male==0 & NumTotalGroup==0, 0, NA)),
         Parti.Male.dum = ifelse(Male==1 & NumTotalGroup>0, 1, ifelse(Male==1 & NumTotalGroup==0, 0, NA)),
         
         Parti.Fisher.dum = ifelse(Fisher==1 & NumTotalGroup>0, 1, ifelse(Fisher==1 & NumTotalGroup==0, 0, NA)),
         Parti.nonFisher.dum = ifelse(Fisher==0 & NumTotalGroup>0, 1, ifelse(Fisher==0 & NumTotalGroup==0, 0, NA)),
         
         Parti.AgeRetired.dum = ifelse(age.retired==1 & NumTotalGroup>0, 1, ifelse(age.retired==1 & NumTotalGroup==0, 0, NA)),
         Parti.AgeWorking.dum = ifelse(age.retired==0 & NumTotalGroup>0, 1, ifelse(age.retired==0 & NumTotalGroup==0, 0, NA)),
         
         Parti.Indigenous.dum = ifelse(yrResident.above==1 & NumTotalGroup>0, 1, ifelse(yrResident.above==1 & NumTotalGroup==0, 0, NA)),
         Parti.nonIndigenous.dum = ifelse(yrResident.above==0 & NumTotalGroup>0, 1, ifelse(yrResident.above==0 & NumTotalGroup==0, 0, NA)),
         
         Parti.Adat.dum = ifelse(adat==1 & NumTotalGroup>0, 1, ifelse(adat==1 & NumTotalGroup==0, 0, NA)),
         Parti.nonAdat.dum = ifelse(adat==0 & NumTotalGroup>0, 1, ifelse(adat==0 & NumTotalGroup==0, 0, NA))) %>% 
  mutate(ed.PrimaryOrNone = ifelse(ed.level.round==2,1,0), 
         ed.JuniorSecondary= ifelse(ed.level.round==3,1,0), 
         ed.SeniorSecondary = ifelse(ed.level.round==4,1,0), 
         ed.Higher = ifelse(ed.level.round==5,1,0)) %>% 
  mutate(EconTrend_DecreaseMore = ifelse(EconStatusTrend==1,1,0),
         EconTrend_DecreaseLittle = ifelse(EconStatusTrend==2,1,0),
         EconTrend_Stable = ifelse(EconStatusTrend==3,1,0),
         EconTrend_IncreaseLittle= ifelse(EconStatusTrend==4,1,0),
         EconTrend_IncreaseMore = ifelse(EconStatusTrend==5,1,0), 
         EconTrend_Increase = ifelse(EconStatusTrend%in%c(3:5),1,0)) %>% 
  mutate(FS.secure = ifelse(FSIndex>=4.02, 1,0), 
         FS.insecure.noHunger = ifelse(FSIndex>=1.56 & FSIndex<4.02, 1, 0), 
         FS.insecure.Hunger = ifelse(FSIndex<1.56, 1, 0))


DiD.data.sumstat.treat <- DiD.data.sumstat %>%  filter(Treatment==1)
DiD.data.sumstat.treat <- describeBy(DiD.data.sumstat.treat)
write_xlsx(DiD.data.sumstat.treat, path = paste0(resultPath, "Paper 1-MPA and Equity/results/2020/tables/Summary_Stat_Treat_Nov2020.xlsx"), col_names = TRUE, format_headers = TRUE)

DiD.data.sumstat.control <- DiD.data.sumstat %>%  filter(Treatment==0)
DiD.data.sumstat.control <- describeBy(DiD.data.sumstat.control)
write_xlsx(DiD.data.sumstat.control, path = paste0(resultPath, "Paper 1-MPA and Equity/results/2020/tables/Summary_Stat_Control_Nov2020.xlsx"), col_names = TRUE, format_headers = TRUE)

plyr::count(DiD.data.baseline, "eth.iso")


##----------------------------------------------------------------#
##--- correlation between Poverty Index and other indicators of poverty

## ---- 4c. correlation bw Poverty Index and social indicators ----

# corr.poverty.df <- DiD.data %>% 
#   select(PovertyIndex_pca, EconStatusTrend, EconTrend_decrease, MAIndex, FSIndex, SERate, PAIndex, MTIndex, DidNotLast:Hungry) 
# 
# library("Hmisc", corrplot)
# corr.poverty <- rcorr(as.matrix(corr.poverty.df))
# corr.poverty
# corrplot(corr.poverty$r, method="number", type = "upper", order = "hclust", tl.col = "black", diag=FALSE)

## -- scatterplots to see the relationship between poverty_index_pca and other indicators of poverty
##-- With FSIndex
Poverty_FS.plot <- ggplot(DiD.data.baseline, aes(x=FSIndex, y=PovertyIndex_pca)) +  
  stat_summary(geom="errorbar", fun.data=mean_cl_normal, size= 1, width=0) +
  stat_summary(geom="point", fun.y = mean, shape=21, color="black", fill="#69b3a2", size=5) +
  scale_x_continuous(breaks=c(0,1.56, 4.02, 6.06)) +
  theme_bw() + theme(axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
                     axis.title.x=element_text(size=13,face="bold"),
                     axis.title.y=element_text(size=13,face="bold")) +
  labs(x="Food Security Index",y="Poverty Alleviation Index", title="Correlation: Poverty Alleviation Index & Food Security Index")
Poverty_FS.plot
Poverty_FS.fig <- plotly::ggplotly(Poverty_FS.plot)
Poverty_FS.fig

# create legend: 0-1.56: Food Insecure with Hunger; 1.56-4.02: Food Insecure without Hunger; 4.02-6.06: Food Secure 

##-- With ed.level
Poverty_Educ.plot <- ggplot(DiD.data.baseline, aes(x=ed.level.round, y=PovertyIndex_pca)) +  
  stat_summary(geom="errorbar", fun.data=mean_cl_normal, size= 1, width=0) +
  stat_summary(geom="point", fun.y = mean, shape=21, color="black", fill="#69b3a2", size=5) +
  theme_bw() + theme(axis.text.x=element_text(size=11, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
                     axis.title.x=element_text(size=13,face="bold"),
                     axis.title.y=element_text(size=13,face="bold")) +
  labs(x="Educational level of household's head",y="Poverty Alleviation Index", title="Correlation: Poverty Alleviation Index & Educational Attainment")
Poverty_Educ.fig <- plotly::ggplotly(Poverty_Educ.plot)
Poverty_Educ.fig

plot_grid(Poverty_FS.plot, Poverty_Educ.plot, ncol=1)
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseScale/Poverty_Heath_Educ_corr.jpg"), width = 10, height = 10)


#------------------------------------------------------------------------------------------------------#

# ---- 4d. Baseline social inequality (Point plots) ----

pd <- position_dodge(.05) # move them .05 to the left and right
##----Gender
Gender.baseline <- DiD.data.baseline %>% 
  mutate(Male = factor(Male)) %>% 
  group_by(Male) %>% 
  summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
            sd = sd(PovertyIndex_pca, na.rm=TRUE), 
            median = median(PovertyIndex_pca, na.rm=TRUE),
            n=length(PovertyIndex_pca), 
            se = sd/sqrt(n)) %>% 
  mutate(Gender=ifelse(Male==0,"Female","Male"))

test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$Gender)$statistic, digits = 3)

Gender.point.plot <- ggplot(Gender.baseline,aes(x=Gender,y=mean, label=paste0("n= ",n))) + 
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
  geom_point(position = pd, aes(color=factor(Gender)), size=5, shape="square") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  #geom_point(aes(x=Gender,y=median), fill='red', shape=24, size=4) + 
  geom_text(aes(y = mean+1.96*se + 0.02),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
        axis.title.x=element_text(size=13,face="bold"),
        axis.title.y=element_text(size=13,face="bold")) +
  labs(x="",y="Poverty Alleviation Index", title="Gender (Female vs. Male) ", subtitle=paste0("Baseline means difference t-test = ",test.val))  
Gender.point.plot
#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Gender_baseIneq_nov2020",".jpg"))



##----Fisher
Fisher.baseline <- DiD.data.baseline %>% 
  filter(!is.na(Fisher)) %>% 
  mutate(Fisher = factor(Fisher)) %>% 
  group_by(Fisher) %>% 
  summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
            sd = sd(PovertyIndex_pca, na.rm=TRUE), 
            median = median(PovertyIndex_pca, na.rm=TRUE),
            n=length(PovertyIndex_pca), 
            se = sd/sqrt(n)) %>% 
  mutate(nonFisher_label=ifelse(Fisher==0," Non-Fisher","Fisher"))

test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$Fisher)$statistic, digits = 3)

Fisher.point.plot <- ggplot(Fisher.baseline,aes(x=nonFisher_label,y=mean, label=paste0("n= ",n))) + 
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
  geom_point(position = pd, aes(color=factor(nonFisher_label)), size=5, shape="square") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  #geom_point(aes(x=Adat_label,y=median), fill='red', shape=24, size=4) + 
  geom_text(aes(y = mean+1.96*se + 0.005),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + 
  theme(legend.position="none",
        axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
        axis.title.x=element_text(size=13,face="bold"),
        axis.title.y=element_text(size=13,face="bold")) +
  labs(x="",y="Poverty Alleviation Index", title="Livelihood (non-Fisher vs. Fisher)", subtitle=paste0("Baseline means difference t-test = ",test.val))
Fisher.point.plot


# 
# ##----Indigenuousness
# Indigenous.baseline <- DiD.data.baseline %>% 
#   filter(!is.na(Indigenousness)) %>% 
#   group_by(Indigenousness) %>% 
#   summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
#             sd = sd(PovertyIndex_pca, na.rm=TRUE), 
#             median = median(PovertyIndex_pca, na.rm=TRUE),
#             n=length(PovertyIndex_pca), 
#             se = sd/sqrt(n)) 
# 
# test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$Indigenousness)$statistic, digits = 3)
# 
# Indigenous.point.plot <- ggplot(Indigenous.baseline,aes(x=Indigenousness,y=mean, label=paste0("n= ",n))) + 
#   geom_point(stat="identity", position = pd, fill='black', size=4, shape="square") + 
#   #geom_point(aes(x=Indigenousness,y=median), fill='red', shape=24, size=4) + 
#   geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
#   geom_text(aes(y = mean+1.96*se + 0.005),position = position_dodge(0.9),vjust = 0, size=4) +
#   geom_line( position = pd) + theme_bw() + 
# theme(legend.position="none",
#       axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
#       axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
#       axis.title.x=element_text(size=13,face="bold"),
#       axis.title.y=element_text(size=13,face="bold")) +
#   labs(x="",y="Poverty Index", title="Residence (Indigenousness)", subtitle=paste0("Baseline means difference t-test = ",test.val))
# Indigenous.point.plot
# ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Indigeneous_baseIneq",".jpg"))


##----Age_Group
Age_Cohort.baseline <- DiD.data.baseline %>% 
  group_by(Age_Cohort) %>% 
  filter(!(Age_Cohort=="NA")) %>% 
  summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
            sd = sd(PovertyIndex_pca, na.rm=TRUE), 
            median = median(PovertyIndex_pca, na.rm=TRUE),
            n=length(PovertyIndex_pca), 
            se = sd/sqrt(n)) 

test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$Age_Cohort)$statistic, digits = 3)

Age_Cohort.point.plot <- ggplot(Age_Cohort.baseline,aes(x=Age_Cohort,y=mean, label=paste0("n= ",n))) + 
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
  geom_point(position = pd, aes(color=factor(Age_Cohort)), size=5, shape="square") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  #geom_point(aes(x=Age_Cohort,y=median), fill='red', shape=24, size=4) + 
  geom_text(aes(y = mean+1.96*se + 0.005),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + 
  theme(legend.position="none",
        axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
        axis.title.x=element_text(size=13,face="bold"),
        axis.title.y=element_text(size=13,face="bold")) +
  labs(x="",y="Poverty Alleviation Index", title="Age Cohort (Retired vs. Working)",subtitle=paste0("Baseline means difference t-test = ",test.val)) 
Age_Cohort.point.plot
#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Age_Cohort_baseIneq",".jpg"))


##----Adat
Adat.baseline <- DiD.data.baseline %>% 
  filter(!is.na(adat)) %>% 
  mutate(adat = factor(adat)) %>% 
  group_by(adat) %>% 
  summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
            sd = sd(PovertyIndex_pca, na.rm=TRUE), 
            median = median(PovertyIndex_pca, na.rm=TRUE),
            n=length(PovertyIndex_pca), 
            se = sd/sqrt(n)) %>% 
  mutate(nonAdat_label=ifelse(adat==0," Non-Adat","Adat"))

test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$adat)$statistic, digits = 3)

Adat.point.plot <- ggplot(Adat.baseline,aes(x=nonAdat_label,y=mean, label=paste0("n= ",n))) + 
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
  geom_point(position = pd, aes(color=factor(nonAdat_label)), size=5, shape="square") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  #geom_point(aes(x=Adat_label,y=median), fill='red', shape=24, size=4) + 
  geom_text(aes(y = mean+1.96*se + 0.005),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + 
  theme(legend.position="none",
        axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
        axis.title.x=element_text(size=13,face="bold"),
        axis.title.y=element_text(size=13,face="bold")) +
  labs(x="",y="Poverty Alleviation Index", title="Tenure Right-holder (Non-Adat vs. Adat) ", subtitle=paste0("Baseline means difference t-test = ",test.val))  
Adat.point.plot

#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Adat_baseIneq",".jpg"))



# ##----Adat_1 (i.e., Adat + being indigenous)
# Adat.baseline <- DiD.data.baseline %>% 
#   filter(!is.na(adat_1)) %>% 
#   mutate(adat = factor(adat_1)) %>% 
#   group_by(adat_1) %>% 
#   summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
#             sd = sd(PovertyIndex_pca, na.rm=TRUE), 
#             median = median(PovertyIndex_pca, na.rm=TRUE),
#             n=length(PovertyIndex_pca), 
#             se = sd/sqrt(n)) %>% 
#   mutate(Adat_label=ifelse(adat_1==1,"Adat","Non-Adat"))
# 
# test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$adat_1)$statistic, digits = 3)
# 
# Adat.point.plot <- ggplot(Adat.baseline,aes(x=Adat_label,y=mean, label=paste0("n= ",n))) + 
#   geom_point(stat="identity", position = pd, fill='black', size=4) + 
#   #geom_point(aes(x=Adat_label,y=median), fill='red', shape=24, size=4) + 
#   geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
#   geom_text(aes(y = mean+1.96*se + 0.01),position = position_dodge(0.9),vjust = 0, size=4) +
#   geom_line( position = pd) + theme_bw() + 
# theme(legend.position="none",
#       axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
#       axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
#       axis.title.x=element_text(size=13,face="bold"),
#       axis.title.y=element_text(size=13,face="bold")) +
#   labs(x="",y="Poverty Index", title="Tenure Right-holder (Adat vs. Non-Adat) ", subtitle=paste0("Baseline means difference t-test = ",test.val))  
# Adat.point.plot
# 
# ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Adat_1_baseIneq",".jpg"))

library(cowplot)
# -- Combine 
plot_grid(Gender.point.plot, Age_Cohort.point.plot, Fisher.point.plot, Adat.point.plot, ncol=2)
#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_1",".pdf"), width = 12, height = 9)
#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_1",".jpg"), width = 12, height = 9)
ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseScale/Combine_baseIneq_1",".png"), width = 12, height = 9)


plot_grid(Gender.point.plot, Age_Cohort.point.plot, Fisher.point.plot, Adat.point.plot, ncol=2)
#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_2",".pdf"), width = 12, height = 12)
#ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_2",".jpg"), width = 12, height = 12)




# ---- 4d. Baseline social inequality (Violin plots) ----


# # --Gender  
# Gender.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Gender)),aes(x=Gender,y=PovertyIndex_pca))+
#   geom_violin(trim=T) +
#   scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
#   #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
#   labs(x="",y="Poverty Index", title="Gender (Male vs. Female)", subtitle=paste0("Baseline distributions")) 
#   #+ ylim(0,100)
# Gender.violin
# 
# 
# # --Livelihood  
# Livelihood.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Livelihood)),aes(x=Livelihood,y=PovertyIndex_pca))+
#   geom_violin(trim=T) +
#   scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
#   #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
#   labs(x="",y="Poverty Index", title="Livelihood (Fishers vs. other)", subtitle=paste0("Baseline distributions"))  
#   #+ ylim(0,100)
# Livelihood.violin
# 
# # 
# # # --Ethnicity  
# # Ethnicity.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Ethnicity)),aes(x=Ethnicity,y=PovertyIndex_pca))+
# #   geom_violin(trim=T) +
# #   scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
# #   #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
# #   labs(x="",y="Poverty Index", title="Ethnicity (non-Dominant vs. Dominant)", subtitle=paste0("Baseline distributions"))  
# #   + ylim(0,100)
# # Ethnicity.violin
# 
# # --Residence  
# Residence.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Indigenousness)),aes(x=Indigenousness,y=PovertyIndex_pca))+
#   geom_violin(trim=T) +
#   scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
#   #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
#   labs(x="",y="Poverty Index", title="Residence (Indigenousness)", subtitle=paste0("Baseline distributions"))  
#   #+ ylim(0,100)
# Residence.violin
# 
# # --Gender  
# Age_Group.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Age_Cohort)),aes(x=Age_Cohort,y=PovertyIndex_pca))+
#   geom_violin(trim=T) +
#   scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
#   #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
#   labs(x="",y="Poverty Index", title="Age Cohort (Working vs. Retirement)", subtitle=paste0("Baseline distributions"))  
#   #+ ylim(0,100)
# Age_Group.violin
# 
# 
# library(cowplot)
# # -- Combine 
# plot_grid(Gender.violin, Livelihood.violin, Residence.violin, Age_Group.violin, ncol=2)
# ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_1_violin",".pdf"), width = 12, height = 9)
# ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_1_violin",".jpg"), width = 12, height = 9)
# 
# plot_grid(Gender.violin, Livelihood.violin, Residence.violin, Age_Group.violin, ncol=2)
# ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_2_violin",".pdf"), width = 12, height = 12)
# ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Combine_baseIneq_2_violin",".jpg"), width = 12, height = 12)
# 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ---- 5. REGRESSION ANALYSIS 1----

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- 5a. Covariates matching (Mahalanobis) ----
##-- create dataframe for matching settlements; note; we are using coarsematching for this paper

DiD.data.matchingCov <- DiD.data %>%
  filter(yearsPost==0) %>% 
  select(MPAID, MonitoringYear, SettlementID, InterviewYear, Treatment, yearsPost, Post, Fisher, TimeMarket, eth.polarize, customary.gov.pct, MTIndex, YrResident, EconStatusTrend, lat, long) %>% 
  group_by(SettlementID) %>%
  summarise(MPAID=first(MPAID),
            Treatment=first(Treatment),
            eth.polarize=first(eth.polarize),
            customary.gov.pct=first(customary.gov.pct),
            lat = first(lat),
            long = first(long),
            Fisher = mean(Fisher,na.rm=T), 
            TimeMarket = mean(TimeMarket,na.rm=T)) %>% 
  select(SettlementID,Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, lat, long, MPAID) %>% 
  mutate(MPAID=as.integer(MPAID))

DiD.data.matchingCov.final <- DiD.data.matchingCov %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long, MPAID) 

## -- Mathcing settlements
Tr<-as.vector(DiD.data.matchingCov$Treatment)  


##-------------------------------------------------------------------------------------------##
### Run match algorithm (COVARIATE MATCHING (Malanobis) with calipers
## Always run with replace=TRUE, and ties=TRUE, vary M (# matches)

# Exact matching on MPAID (i.e., control and treatment settlements have to come from the same MPA; that's why the last item is "1")
Xexact<-c(0,0,0,0,0,1)
m_mahanobis<-Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.final, M=3, exact=Xexact, replace=TRUE, ties=T)
summary(m_mahanobis)

# Compute match balance statistics (don't need stats on exact-matched variables)
m_balance_2<-Matching::MatchBalance(Tr~ TimeMarket + Fisher + eth.polarize  + lat + long,
                                    data=DiD.data.matchingCov, match.out=m_mahanobis, ks=TRUE, nboots=1000, digits=3)


##--Note: After the matching trials, best option is mahanobis, 1-to-3 match.
##construct DiD.data.SettlMatch dataframe
i.treat<-m_mahanobis$index.treated
i.control<-m_mahanobis$index.control  

##-- mahalanobis distance weighting
DiD.data.matchingCov.mahDis <- DiD.data.matchingCov %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long) 

mal.dist <- mahalanobis(DiD.data.matchingCov.mahDis,cov(DiD.data.matchingCov.mahDis) ,center=F)
head(mal.dist)
pair.treat.settl <-as.data.frame(cbind(DiD.data.matchingCov[i.treat,"SettlementID"],mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist=`mal.dist[i.treat]`,c.mal.dist=`mal.dist[i.control]`) %>%
  mutate(pair.dist=t.mal.dist-c.mal.dist,
         pair.id=1:nrow(.),
         inv.dist=1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt=inv.dist/sum(inv.dist)) %>% 
  select(SettlementID,pair.id,t.mal.dist:dist.wt) %>% 
  ungroup()
head(pair.treat.settl)
summary(pair.treat.settl)

pair.control.settl <- cbind(DiD.data.matchingCov[i.control,"SettlementID"], pair.treat.settl) 
names(pair.control.settl)[1:2] <- c("ctrl.id", "treat.id")
head(pair.control.settl)  

settl.check <- pair.control.settl %>% 
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("ctrl.id"="SettlementID")  ) %>%
  rename(ctrl.settlement=SettlementName) %>%
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("ctrl.id"="SettlementID") ) %>% 
  rename(ctrl.TimeMarket=TimeMarket, ctrl.Fisher=Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("treat.id"="SettlementID") ) %>%
  rename(treat.settlement=SettlementName) %>% 
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("treat.id"="SettlementID") ) %>% 
  rename(treat.TimeMarket=TimeMarket, treat.Fisher=Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket,ctrl.TimeMarket,treat.Fisher,ctrl.Fisher, 
         treat.eth.polarize, ctrl.eth.polarize, pair.dist,TimeMarket.dis:eth.polarize.dis)
head(settl.check)
#export(settl.check,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name_1to3_w_wLatLon_comp.xlsx")

DiD.data.SettlMatch <- data.frame()
DiD.data.SettlMatch <- rbind(pair.treat.settl, 
                             pair.control.settl %>% 
                               select(-treat.id) %>% 
                               rename(SettlementID=ctrl.id)) %>% 
  arrange(pair.id,SettlementID) 
head(DiD.data.SettlMatch)

DiD.data.SettlMatch <- DiD.data.SettlMatch %>% left_join(DiD.data, by="SettlementID")
DiD.data.SettlMatch <- as.data.frame(DiD.data.SettlMatch)


##--Important: Now changing "DiD.data" into "DiD.data" dataframe "DiD.data.coarseMatch", and renaming "DiD.data.SettlMatch" "DiD.data" for Sections 6.1 to 6.4
DiD.data.coarseMatch <- DiD.data
DiD.data.coarseMatch <- as.data.frame(DiD.data.coarseMatch)

DiD.data.coarseMatch <-DiD.data.coarseMatch %>% 
  mutate(dist.wt=1, 
         pair.id=1)

DiD.data.coarseMatch <-DiD.data.coarseMatch %>% 
  mutate(EconTrend_increase = ifelse(EconTrend_decrease==1,0,1))

##-----------------------------------------------------------------------------------------##

# ---- 5b. Produce aggregate subgroup impacts (stargazer) ----

model.out.subgroup <- data.frame()
varNames <- c("PovertyIndex_pca", "EconTrend_increase")
regValue.list <-list()

##--------------------Stargazer Section------------------##
for (i in varNames) {
  
  ##----------------Gender------------------------------##
  ##-----Female
  reg.df <- DiD.data.coarseMatch %>%  filter(Male==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear + pair.id | 0 | SettlementID,  data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.gender.female <- regValue
  
  ##-----Male
  reg.df <- DiD.data.coarseMatch %>%  filter(Male==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear + pair.id | 0 | SettlementID,  data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.gender.male <- regValue
  
  
  ##----------------Occupation (fisher/non-fisher)------------------------------##
  ##---DiD Regressions: by Occupation (non-fisher only)
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.livelihood.nonFisher <- regValue
  
  ##---DiD Regressions: by Occupation (fisher only)
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.livelihood.fisher <- regValue
  
  
  
  ##----------------Tenure (Adat/non-Adat)------------------------------##
  ##---Adat
  reg.df <- DiD.data.coarseMatch %>% filter(adat==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.tenure.adat <- regValue
  
  ##---non-Adat
  reg.df <- DiD.data.coarseMatch %>% filter(adat==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.tenure.nonAdat <- regValue
  
  
  
  ##----------------Age COhort (working/retired)------------------------------##
  ##---retired
  reg.df <- DiD.data.coarseMatch %>% filter(age.retired==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.age.retired <- regValue
  
  ##---working
  reg.df <- DiD.data.coarseMatch %>% filter(age.retired==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  regValue.age.working <- regValue
  
  regValue.list[[i]] <- list(regValue.gender.female, regValue.gender.male, regValue.age.retired, regValue.age.working, regValue.tenure.adat, regValue.tenure.nonAdat, regValue.livelihood.fisher, regValue.livelihood.nonFisher)
  
  
}
##-------------------------------------------------------------------------------##
##-- Output Sensitivity result tables for GINI outcome
##save html (can open in MS Word)
stargazer(regValue.list["PovertyIndex_pca"],
          out = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/impact_subGroup_povertyIndex_Nov2020"), type = "html", 
          df = FALSE, notes.append = FALSE, omit.table.layout = "n",
          column.labels=c("Female","Male","Retired","Working", "Adat", "non-Adat", "Fisher", "non-Fisher"),
          title="Subgroup Impacts: DiD Regression Results",
          align=TRUE, dep.var.labels="Poverty Exposure Index",
          add.lines = list(c("MPA-cluster FEs","Yes","Yes","Yes","Yes", "Yes","Yes","Yes","Yes"),
                           c("Interview Year FEs","Yes","Yes","Yes","Yes", "Yes","Yes","Yes","Yes")))


stargazer(regValue.list["EconTrend_increase"],
          out = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/impact_subGroup_EconTrend_Nov2020"), type = "html", 
          df = FALSE, notes.append = FALSE, omit.table.layout = "n",
          column.labels=c("Female","Male","Retired","Working", "Adat", "non-Adat", "Fisher", "non-Fisher"),
          title="Subgroup Impacts: DiD Regression Results",
          align=TRUE, dep.var.labels="Positive Perception on Economic Trend",
          add.lines = list(c("MPA-cluster FEs","Yes","Yes","Yes","Yes", "Yes","Yes","Yes","Yes"),
                           c("Interview Year FEs","Yes","Yes","Yes","Yes", "Yes","Yes","Yes","Yes")))


##--------------------------------------------------------------------##
##------------------Plot section--------------------------------------##
# ---- 5c. Produce aggregate subgroup impacts (plots) ----

for (i in varNames) {
  
  ##----------------Gender------------------------------##
  for (genderID in 0:1) {
    reg.df <- DiD.data.coarseMatch %>%  filter(Male==genderID)
    Y <- reg.df[,i]
    w <- reg.df[,"dist.wt"]
    
    
    regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID,  data=reg.df, exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:Post1", "Post1")) %>% 
      mutate(term=gsub("Treatment:Post1","Impact",term),
             term=gsub("Post1","Control_trend",term),
             Response=i, subgroup="Gender", subgroup_id=genderID)
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                  n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                                | SettlementID + InterviewYear + pair.id  | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights = w)
    summary(regValue.treatTrend)
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("Post1")) %>% 
      mutate(term=gsub("Post1","Treatment_trend",term),
             Response=i, subgroup="Gender", subgroup_id=genderID)
    
    model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
  }
  
  # ##----------------Indigenous (by Yrresident)------------------------------##
  # for (yrResident.aboveID in 0:1) {
  #   reg.df <- DiD.data.coarseMatch %>% filter(yrResident.above==yrResident.aboveID)
  #   Y <- reg.df[,i]
  #   w <- reg.df[,"dist.wt"]
  #   
  #   regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
  #                      n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
  #                    | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  #   
  #   reg.broom <- tidy(regValue) %>% 
  #     filter(term%in%c("Treatment:Post1", "Post1")) %>% 
  #     mutate(term=gsub("Treatment:Post1","Impact",term),
  #            term=gsub("Post1","Control_trend",term),
  #            Response=i, subgroup="Indigenous", subgroup_id=yrResident.aboveID)
  #   
  #   ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  #   regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
  #                                 n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
  #                               | SettlementID + InterviewYear + pair.id  | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights = w)
  #   summary(regValue.treatTrend)
  #   
  #   reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
  #     filter(term%in%c("Post1")) %>% 
  #     mutate(term=gsub("Post1","Treatment_trend",term),
  #            Response=i, subgroup="Indigenous", subgroup_id=yrResident.aboveID)
  #   
  #   model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
  # } 
  # 
  
  ##----------------Adat (by Tenure right)------------------------------##
  for (adat_ID in 0:1) {
    reg.df <- DiD.data.coarseMatch %>% filter(adat==adat_ID)
    Y <- reg.df[,i]
    w <- reg.df[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:Post1", "Post1")) %>% 
      mutate(term=gsub("Treatment:Post1","Impact",term),
             term=gsub("Post1","Control_trend",term),
             Response=i, subgroup="Tenureship", subgroup_id=adat_ID)
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                  n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                                | SettlementID + InterviewYear + pair.id  | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights = w)
    summary(regValue.treatTrend)
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("Post1")) %>% 
      mutate(term=gsub("Post1","Treatment_trend",term),
             Response=i, subgroup="Tenureship", subgroup_id=adat_ID)
    
    model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
  } 
  
  
  
  
  ##----------------Age (retirement vs working)------------------------------##
  for (age.retiredID in 0:1) {
    reg.df <- DiD.data.coarseMatch %>% filter(age.retired==age.retiredID)
    Y <- reg.df[,i]
    w <- reg.df[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID+ InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:Post1", "Post1")) %>% 
      mutate(term=gsub("Treatment:Post1","Impact",term),
             term=gsub("Post1","Control_trend",term),
             Response=i, subgroup="Age", subgroup_id=age.retiredID)
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                  n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                                | SettlementID+ InterviewYear  + pair.id  | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights = w)
    summary(regValue.treatTrend)
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("Post1")) %>% 
      mutate(term=gsub("Post1","Treatment_trend",term),
             Response=i, subgroup="Age", subgroup_id=age.retiredID)
    
    model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
  }
  
  
  
  ##----------------Occupation (fisher/non-fisher)------------------------------##
  
  ##---DiD Regressions: by Occupation (non-fisher only)
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=0)
  
  ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                              | SettlementID + InterviewYear + pair.id  | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights = w)
  summary(regValue.treatTrend)
  
  reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
    filter(term%in%c("Post1")) %>% 
    mutate(term=gsub("Post1","Treatment_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=0)
  
  model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
  
  
  ##---DiD Regressions: by Occupation (fisher only)
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=1)
  
  ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                              | SettlementID + InterviewYear + pair.id  | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights = w)
  summary(regValue.treatTrend)
  
  reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
    filter(term%in%c("Post1")) %>% 
    mutate(term=gsub("Post1","Treatment_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=1)
  
  model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
  
}


##-------------------------------------------------------------------------------##
##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
model.out.subgroup1 <- model.out.subgroup %>%
  mutate(domain=ifelse(Response=="PovertyIndex_PCA"," Poverty Alleviation Index (Asset-based PCA)",
                       ifelse(Response=="EconTrend_increase","Positive Economic Trend (Subjective outcome)",
                              ifelse(Response=="FSIndex_z"," Health (Food Security)",
                                     ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
                                            ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
                                                   ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))))),
         domain=gsub(" \\(", "\n \\(", domain))

model.out.subgroup1 <- model.out.subgroup %>%
  mutate(Indicator= ifelse(Response=="EconTrend_increase", "2. Economic Stability or Improvement (Household perception [subjective])", 
                           ifelse(Response=="PovertyIndex_pca", "1. Poverty Alleviation Index (Asset-based [objective])", "")),
         Indicator=gsub(" \\(", "\n \\(", Indicator)) %>% 
  mutate(subgroup=ifelse(subgroup=="Indigenous", "Residence", subgroup), 
         subgroup=ifelse(subgroup=="Fishing Livelihood", "Livelihood", subgroup)) %>% 
  mutate(subgroup_label = ifelse(subgroup=="Gender" & subgroup_id==0, "Female",    
                                 ifelse(subgroup=="Gender" & subgroup_id==1, "Male", 
                                        ifelse(subgroup=="Tenureship" & subgroup_id==0, " non-Adat", 
                                               ifelse(subgroup=="Tenureship" & subgroup_id==1, "Adat", 
                                                      ifelse(subgroup=="Livelihood" & subgroup_id==0, " non-Fisher", 
                                                             ifelse(subgroup=="Livelihood" & subgroup_id==1, "Fisher",
                                                                    ifelse(subgroup=="Age" & subgroup_id==0, "Working-age", "Retirement-age"))))))))

##Export 
#export(model.out.subgroup1,  paste0(resultPath, "Paper 1-MPA and Equity/results/2020/outputs/impact_ouput_coarseMatch_Nov2020.xlsx"))




##----------------------------------------------------------------##

pd <- position_dodge(width=.5) # move them .05 to the left and right

# Gender
Gender.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",subgroup=="Gender"),aes(x=Indicator, y=estimate, fill=subgroup_label), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.45,0.13), legend.title = element_blank(), legend.text = element_text(size=9),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Gender")   
Gender.plot_z


# Livelihood
Livelihood.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",subgroup=="Livelihood"),aes(x=Indicator, y=estimate, fill=subgroup_label), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.45,0.13), legend.title = element_blank(), legend.text = element_text(size=9),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Livelihood")  
Livelihood.plot_z


# # Residence
# Residence.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",subgroup=="Residence"),aes(x=Indicator, y=estimate, fill=subgroup_label), group=2) +
#   geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
#   theme_bw() + theme(legend.position=c(0.15,0.85), legend.title = element_blank(), legend.text = element_text(size=9),
#                      legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
#                      axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
#                      axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
#   geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
#   labs(x="",y="Impact Estimates", title="MPA Impacts by Residence")  
# Residence.plot_z


# Adat - Tenureship
Tenureship.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",subgroup=="Tenureship"),aes(x=Indicator, y=estimate, fill=subgroup_label), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.45,0.13), legend.title = element_blank(), legend.text = element_text(size=9),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Tenureship")  
Tenureship.plot_z

# Age
Age.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",subgroup=="Age"),aes(x=Indicator, y=estimate, fill=subgroup_label), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.45,0.13), legend.title = element_blank(), legend.text = element_text(size=9),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Age Group")  
Age.plot_z


##combine PLOTS
plot_grid(Gender.plot_z, Age.plot_z, Livelihood.plot_z, Tenureship.plot_z, ncol=2)
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseScale/Poverty_EconTrend_4groups.png"),width = 12, height = 8)



# ---- 5d. Produce over-time impact plot ----
##------5d. changes in poverty impact overtime (t2 and t4; using BHS data only)
model.out.time <- data.frame()
varNames <- c("PovertyIndex_pca", "EconTrend_increase")
DiD.data.coarseMatch.BHS <- DiD.data.coarseMatch %>% filter(MPAID%in%c(1:6))

for (i in varNames) {
  
  ##----------------Gender------------------------------##
  for (genderID in 0:1) {
    reg.df <- DiD.data.coarseMatch.BHS %>%  filter(Male==genderID)
    Y <- reg.df[,i]
    w <- reg.df[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
      mutate(Response=i, subgroup="Gender", subgroup_id=genderID,
             term=gsub("Treatment:yearsPostF2","Impact_2",term),
             term=gsub("yearsPostF2","Control_2",term),
             term=gsub("Treatment:yearsPostF4","Impact_4",term),
             term=gsub("yearsPostF4","Control_4",term),
             term=gsub("Treatment:yearsPostF7","Impact_7",term),
             term=gsub("yearsPostF7","Control_7",term),)
    
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    summary(regValue.treatTrend)
    
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
      mutate(Response=i, subgroup="Gender", subgroup_id=genderID,
             term=gsub("yearsPostF2","Treatment_2",term),
             term=gsub("yearsPostF4","Treatment_4",term),
             term=gsub("yearsPostF7","Treatment_7",term),)
    
    model.out.time <- rbind(model.out.time, reg.broom, reg.broom.treatTrend)
  }
  
  # ##----------------Indigenous (by Yrresident)------------------------------##
  # for (yrResident.aboveID in 0:1) {
  #   reg.df <- DiD.data.coarseMatch.BHS %>% filter(yrResident.above==yrResident.aboveID)
  #   Y <- reg.df[,i]
  #   w <- reg.df[,"dist.wt"]
  #   
  #   regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
  #                      n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
  #                    | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  #   
  #   reg.broom <- tidy(regValue) %>% 
  #     filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
  #     mutate(Response=i, subgroup="Indigenous", subgroup_id=yrResident.aboveID,
  #            term=gsub("Treatment:yearsPostF2","Impact_2",term),
  #            term=gsub("yearsPostF2","Control_2",term),
  #            term=gsub("Treatment:yearsPostF4","Impact_4",term),
  #            term=gsub("yearsPostF4","Control_4",term),
  #            term=gsub("Treatment:yearsPostF7","Impact_7",term),
  #            term=gsub("yearsPostF7","Control_7",term),)
  #   
  #   
  #   ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  #   regValue <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
  #                      n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
  #                    | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  #   summary(regValue.treatTrend)
  #   
  #   
  #   reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
  #     filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
  #     mutate(Response=i, subgroup="Indigenous", subgroup_id=yrResident.aboveID,
  #            term=gsub("yearsPostF2","Treatment_2",term),
  #            term=gsub("yearsPostF4","Treatment_4",term),
  #            term=gsub("yearsPostF7","Treatment_7",term),)
  #   
  #   model.out.time <- rbind(model.out.time, reg.broom, reg.broom.treatTrend)
  #} 
  
  
  ##----------------Tenureship (by Adat)------------------------------##
  for (Adat_ID in 0:1) {
    reg.df <- DiD.data.coarseMatch.BHS %>% filter(adat==Adat_ID)
    Y <- reg.df[,i]
    w <- reg.df[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
      mutate(Response=i, subgroup="Tenureship", subgroup_id=Adat_ID,
             term=gsub("Treatment:yearsPostF2","Impact_2",term),
             term=gsub("yearsPostF2","Control_2",term),
             term=gsub("Treatment:yearsPostF4","Impact_4",term),
             term=gsub("yearsPostF4","Control_4",term),
             term=gsub("Treatment:yearsPostF7","Impact_7",term),
             term=gsub("yearsPostF7","Control_7",term),)
    
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    summary(regValue.treatTrend)
    
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
      mutate(Response=i, subgroup="Tenureship", subgroup_id=Adat_ID,
             term=gsub("yearsPostF2","Treatment_2",term),
             term=gsub("yearsPostF4","Treatment_4",term),
             term=gsub("yearsPostF7","Treatment_7",term),)
    
    model.out.time <- rbind(model.out.time, reg.broom, reg.broom.treatTrend)
  } 
  
  ##----------------Age (retirement vs working)------------------------------##
  for (age.retiredID in 0:1) {
    reg.df <- DiD.data.coarseMatch.BHS %>% filter(age.retired==age.retiredID)
    Y <- reg.df[,i]
    w <- reg.df[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
      mutate(Response=i, subgroup="Age", subgroup_id=age.retiredID,
             term=gsub("Treatment:yearsPostF2","Impact_2",term),
             term=gsub("yearsPostF2","Control_2",term),
             term=gsub("Treatment:yearsPostF4","Impact_4",term),
             term=gsub("yearsPostF4","Control_4",term),
             term=gsub("Treatment:yearsPostF7","Impact_7",term),
             term=gsub("yearsPostF7","Control_7",term),)
    
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
    summary(regValue.treatTrend)
    
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
      mutate(Response=i, subgroup="Age", subgroup_id=age.retiredID,
             term=gsub("yearsPostF2","Treatment_2",term),
             term=gsub("yearsPostF4","Treatment_4",term),
             term=gsub("yearsPostF7","Treatment_7",term),)
    
    
    model.out.time <- rbind(model.out.time, reg.broom, reg.broom.treatTrend)
  }
  
  
  
  ##----------------Occupation (fisher/non-fisher)------------------------------##
  
  ##---DiD Regressions: by Occupation (non-fisher only)
  reg.df <- DiD.data.coarseMatch.BHS %>% filter(Fisher==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
    mutate(Response=i, subgroup="Fishing Livelihood", subgroup_id=0,
           term=gsub("Treatment:yearsPostF2","Impact_2",term),
           term=gsub("yearsPostF2","Control_2",term),
           term=gsub("Treatment:yearsPostF4","Impact_4",term),
           term=gsub("yearsPostF4","Control_4",term),
           term=gsub("Treatment:yearsPostF7","Impact_7",term),
           term=gsub("yearsPostF7","Control_7",term),)
  
  
  ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  regValue <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  summary(regValue.treatTrend)
  
  
  reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
    filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
    mutate(Response=i, subgroup="Fishing Livelihood", subgroup_id=0,
           term=gsub("yearsPostF2","Treatment_2",term),
           term=gsub("yearsPostF4","Treatment_4",term),
           term=gsub("yearsPostF7","Treatment_7",term),)
  
  
  model.out.time <- rbind(model.out.time, reg.broom, reg.broom.treatTrend)
  
  
  ##---DiD Regressions: by Occupation (fisher only; note: has to drop "occupation FE")
  reg.df <- DiD.data.coarseMatch.BHS %>% filter(Fisher==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                   | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
    mutate(Response=i, subgroup="Fishing Livelihood", subgroup_id=1,
           term=gsub("Treatment:yearsPostF2","Impact_2",term),
           term=gsub("yearsPostF2","Control_2",term),
           term=gsub("Treatment:yearsPostF4","Impact_4",term),
           term=gsub("yearsPostF4","Control_4",term),
           term=gsub("Treatment:yearsPostF7","Impact_7",term),
           term=gsub("yearsPostF7","Control_7",term),)
  
  
  ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  regValue <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                   | SettlementID + InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  summary(regValue.treatTrend)
  
  
  reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
    filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
    mutate(Response=i, subgroup="Fishing Livelihood", subgroup_id=1,
           term=gsub("yearsPostF2","Treatment_2",term),
           term=gsub("yearsPostF4","Treatment_4",term),
           term=gsub("yearsPostF7","Treatment_7",term),)
  
  
  model.out.time <- rbind(model.out.time, reg.broom, reg.broom.treatTrend)
}


model.out.time1 <- model.out.time %>%
  mutate(Indicator= ifelse(Response=="EconTrend_decrease", "2. Decreasing Economic Trend (Household perception [subjective])", 
                           ifelse(Response=="PovertyIndex_pca", "1. Poverty Index (Asset-based [objective])", "")),
         Indicator=gsub(" \\(", "\n \\(", Indicator)) %>% 
  mutate(impact_time= ifelse(term=="Impact_2", "2 years", 
                             ifelse(term=="Impact_4", "4 years", NA))) %>% 
  mutate(subgroup=ifelse(subgroup=="Indigenous", "Residence", subgroup), 
         subgroup=ifelse(subgroup=="Fishing Livelihood", "Livelihood", subgroup)) %>% 
  mutate(subgroup_label = ifelse(subgroup=="Gender" & subgroup_id==0, "Female",    
                                 ifelse(subgroup=="Gender" & subgroup_id==1, "Male", 
                                        ifelse(subgroup=="Tenureship" & subgroup_id==0, "non-Adat", 
                                               ifelse(subgroup=="Tenureship" & subgroup_id==1, " Adat", 
                                                      ifelse(subgroup=="Livelihood" & subgroup_id==0, "non-Fisher", 
                                                             ifelse(subgroup=="Livelihood" & subgroup_id==1, "Fisher",
                                                                    ifelse(subgroup=="Age" & subgroup_id==0, "Working-age", "Retirement-age"))))))))

##Export 
#export(model.out.subgroup1,  paste0(resultPath, "Paper 1-MPA and Equity/results/2020/outputs/impact_ouput_coarseMatch.csv"))



##----------------------------------------------------------------##
pd <- position_dodge(width=.5) # move them .05 to the left and right

# Gender
Gender.plot_z <- ggplot(filter(model.out.time1, !is.na(impact_time), subgroup=="Gender"),aes(x=Indicator, y=estimate, color=subgroup_label, fill=impact_time), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1.5)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=subgroup_label), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=subgroup_label), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.18,0.85), legend.box = "horizontal", legend.title = element_blank(), legend.text = element_text(size=8),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_color_grey() + scale_fill_hue(name="Time") +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Gender")  
Gender.plot_z


# Livelihood
Livelihood.plot_z <- ggplot(filter(model.out.time1, !is.na(impact_time),subgroup=="Livelihood"),aes(x=Indicator, y=estimate, color=subgroup_label, fill=impact_time), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1.5)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=subgroup_label), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=subgroup_label), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.19,0.85), legend.box = "horizontal", legend.title = element_blank(), legend.text = element_text(size=8),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_color_grey() +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Livelihood")  
Livelihood.plot_z


# # Residence
# Residence.plot_z <- ggplot(filter(model.out.time1,!is.na(impact_time),subgroup=="Residence"),aes(x=Indicator, y=estimate, color=subgroup_label, fill=impact_time), group=2) +
#   geom_bar(stat="identity", position =pd, width = 0.5, size=1.5)+ 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=subgroup_label), width=0.0, size=1.5, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=subgroup_label), width=0.1, size=0.3, position = pd ) +
#   theme_bw() + theme(legend.position=c(0.2,0.85), legend.box = "horizontal", legend.title = element_blank(), legend.text = element_text(size=8),
#                      legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
#                      axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
#                      axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
#   geom_hline(yintercept = 0, linetype = "dashed") + scale_color_grey() +
#   labs(x="",y="Impact Estimates", title="MPA Impacts by Residence")  
# Residence.plot_z

# Tenureship
Tenureship.plot_z <- ggplot(filter(model.out.time1,!is.na(impact_time),subgroup=="Tenureship"),aes(x=Indicator, y=estimate, color=subgroup_label, fill=impact_time), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1.5)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=subgroup_label), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=subgroup_label), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.2,0.85), legend.box = "horizontal", legend.title = element_blank(), legend.text = element_text(size=8),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_color_grey() +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Tenureship")  
Tenureship.plot_z


# Age
Age.plot_z <- ggplot(filter(model.out.time1,!is.na(impact_time),subgroup=="Age"),aes(x=Indicator, y=estimate, color=subgroup_label, fill=impact_time), group=2) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1.5)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=subgroup_label), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=subgroup_label), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.2,0.85), legend.box = "horizontal", legend.title = element_blank(), legend.text = element_text(size=8),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_color_grey() +
  labs(x="",y="Impact Estimates", title="MPA Impacts by Age Group")  
Age.plot_z


##combine PLOTS
plot_grid(Gender.plot_z, Age.plot_z, Livelihood.plot_z, Tenureship.plot_z, ncol=2)
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/Impact_plots/Poverty_EconTrend_t2-4.png"),width = 12, height = 8)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ---- 6. REGRESSION ANALYSIS 2----

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ---- 6a. Settlement-level Aggregate impacts ----

#--- Generating settlement-level of participation index

##------##
## Generating Participation indicators for subgroups 
DiD.data.coarseMatch <- DiD.data.coarseMatch %>% 
  mutate(NumTotalGroup = NumMarineGroup + NumOtherGroup,
         NumTotalGroup.Female = ifelse(Male==0, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.Male = ifelse(Male==1, NumMarineGroup + NumOtherGroup, NA),
         
         NumTotalGroup.Fisher = ifelse(Fisher==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.nonFisher = ifelse(Fisher==0, NumMarineGroup + NumOtherGroup, NA),
         
         NumTotalGroup.AgeRetired = ifelse(age.retired==1, NumMarineGroup + NumOtherGroup, NA), 
         NumTotalGroup.AgeWorking = ifelse(age.retired==0, NumMarineGroup + NumOtherGroup, NA), 
         
         NumTotalGroup.Indigenous = ifelse(yrResident.above==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.nonIndigenous = ifelse(yrResident.above==0, NumMarineGroup + NumOtherGroup, NA),
         
         NumTotalGroup.Adat = ifelse(adat==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.nonAdat = ifelse(adat==0, NumMarineGroup + NumOtherGroup, NA),
         
         Parti.dum = ifelse(NumTotalGroup>0, 1, ifelse(NumTotalGroup==0, 0, NA)),
         Parti.Female.dum = ifelse(Male==0 & NumTotalGroup>0, 1, ifelse(Male==0 & NumTotalGroup==0, 0, NA)),
         Parti.Male.dum = ifelse(Male==1 & NumTotalGroup>0, 1, ifelse(Male==1 & NumTotalGroup==0, 0, NA)),
         
         Parti.Fisher.dum = ifelse(Fisher==1 & NumTotalGroup>0, 1, ifelse(Fisher==1 & NumTotalGroup==0, 0, NA)),
         Parti.nonFisher.dum = ifelse(Fisher==0 & NumTotalGroup>0, 1, ifelse(Fisher==0 & NumTotalGroup==0, 0, NA)),
         
         Parti.AgeRetired.dum = ifelse(age.retired==1 & NumTotalGroup>0, 1, ifelse(age.retired==1 & NumTotalGroup==0, 0, NA)),
         Parti.AgeWorking.dum = ifelse(age.retired==0 & NumTotalGroup>0, 1, ifelse(age.retired==0 & NumTotalGroup==0, 0, NA)),
         
         Parti.Indigenous.dum = ifelse(yrResident.above==1 & NumTotalGroup>0, 1, ifelse(yrResident.above==1 & NumTotalGroup==0, 0, NA)),
         Parti.nonIndigenous.dum = ifelse(yrResident.above==0 & NumTotalGroup>0, 1, ifelse(yrResident.above==0 & NumTotalGroup==0, 0, NA)),
         
         Parti.Adat.dum = ifelse(adat==1 & NumTotalGroup>0, 1, ifelse(adat==1 & NumTotalGroup==0, 0, NA)),
         Parti.nonAdat.dum = ifelse(adat==0 & NumTotalGroup>0, 1, ifelse(adat==0 & NumTotalGroup==0, 0, NA)))
##-----##

## important: get the intensive (# of groups) and extensive (whether 0/1) measure of subgroup participations 
DiD.data.coarseMatch.settl <- data.frame()
DiD.data.coarseMatch.settl <- DiD.data.coarseMatch %>% 
  select(MPAID, SettlementID, Treatment, Post, yearsPost, InterviewYear, TimeMarket, PovertyIndex_GINI_Settl, PovertyIndex_pca, EconTrend_decrease,EconTrend_increase, customary.gov.pct, Fisher, lat, long, 
         NumTotalGroup, NumTotalGroup.AgeRetired, NumTotalGroup.AgeWorking, NumTotalGroup.Fisher, NumTotalGroup.nonFisher, NumTotalGroup.Female, NumTotalGroup.Male, 
         NumTotalGroup.Indigenous, NumTotalGroup.nonIndigenous, NumTotalGroup.Adat, NumTotalGroup.nonAdat,
         Parti.dum, Parti.Female.dum, Parti.Male.dum, Parti.AgeRetired.dum, Parti.AgeWorking.dum, Parti.Fisher.dum, Parti.nonFisher.dum,
         Parti.Indigenous.dum, Parti.nonIndigenous.dum, Parti.Adat.dum, Parti.nonAdat.dum) %>% 
  group_by(SettlementID, yearsPost) %>% 
  summarise(MPAID = first(MPAID),
            Treatment = first(Treatment), 
            Post= first(Post), 
            InterviewYear = first(InterviewYear), 
            PovertyIndex_GINI_Settl=first(PovertyIndex_GINI_Settl), 
            PovertyIndex_pca = mean(PovertyIndex_pca, na.rm=TRUE),
            EconTrend_decrease = mean(EconTrend_decrease, na.rm=TRUE), 
            EconTrend_increase = mean(EconTrend_increase, na.rm=TRUE), 
            TimeMarket = mean(TimeMarket, na.rm=TRUE),
            Fisher = mean(Fisher, na.rm=TRUE),
            lat = first(lat), 
            long=first(long),
            #eth.polarize = first(eth.polarize),
            customary.gov.pct = first(customary.gov.pct),
            
            TimeMarket_settl.m = mean(TimeMarket),
            Parti_settl.m = mean(NumTotalGroup),
            Parti_settl.Female.m = mean(NumTotalGroup.Female, na.rm = TRUE),
            Parti_settl.Male.m = mean(NumTotalGroup.Male, na.rm = TRUE),
            
            Parti_settl.AgeRetired.m = mean(NumTotalGroup.AgeRetired, na.rm = TRUE),
            Parti_settl.AgeWorking.m = mean(NumTotalGroup.AgeWorking, na.rm = TRUE),
            
            Parti_settl.Fisher.m = mean(NumTotalGroup.Fisher, na.rm = TRUE),
            Parti_settl.nonFisher.m = mean(NumTotalGroup.nonFisher, na.rm = TRUE),
            
            Parti_settl.Indigenous.m = mean(NumTotalGroup.Indigenous, na.rm = TRUE),
            Parti_settl.nonIndigenous.m = mean(NumTotalGroup.nonIndigenous, na.rm = TRUE),
            
            Parti_settl.Adat.m = mean(NumTotalGroup.Adat, na.rm = TRUE),
            Parti_settl.nonAdat.m = mean(NumTotalGroup.nonAdat, na.rm = TRUE),
            
            Parti_settl.pct = mean(Parti.dum, na.rm = TRUE),
            Parti_settl.Female.pct = mean(Parti.Female.dum, na.rm = TRUE),
            Parti_settl.Male.pct = mean(Parti.Male.dum, na.rm = TRUE),
            
            Parti_settl.AgeRetired.pct = mean(Parti.AgeRetired.dum, na.rm = TRUE),
            Parti_settl.AgeWorking.pct = mean(Parti.AgeWorking.dum, na.rm = TRUE),
            
            Parti_settl.Fisher.pct = mean(Parti.Fisher.dum, na.rm = TRUE),
            Parti_settl.nonFisher.pct = mean(Parti.nonFisher.dum, na.rm = TRUE),
            
            Parti_settl.Indigenous.pct = mean(Parti.Indigenous.dum, na.rm = TRUE),
            Parti_settl.nonIndigenous.pct = mean(Parti.nonIndigenous.dum, na.rm = TRUE),
            
            Parti_settl.Adat.pct = mean(Parti.Adat.dum, na.rm = TRUE),
            Parti_settl.nonAdat.pct = mean(Parti.nonAdat.dum, na.rm = TRUE))


## -- Replace all participation measures with NaN as 0 (NaN is because that settlement doesnt have any Female --> level of female participation can be thought of as 0)
DiD.data.coarseMatch.settl <- DiD.data.coarseMatch.settl %>% 
  mutate(Parti_settl.Female.m = ifelse(is.nan(Parti_settl.Female.m), 0, Parti_settl.Female.m), 
         Parti_settl.Female.pct = ifelse(is.nan(Parti_settl.Female.pct), 0, Parti_settl.Female.pct), 
         Parti_settl.Male.m = ifelse(is.nan(Parti_settl.Male.m), 0, Parti_settl.Male.m), 
         Parti_settl.Male.pct = ifelse(is.nan(Parti_settl.Male.pct), 0, Parti_settl.Male.pct), 
         
         Parti_settl.Fisher.m = ifelse(is.nan(Parti_settl.Fisher.m), 0, Parti_settl.Fisher.m), 
         Parti_settl.Fisher.pct = ifelse(is.nan(Parti_settl.Fisher.pct), 0, Parti_settl.Fisher.pct), 
         Parti_settl.nonFisher.m = ifelse(is.nan(Parti_settl.nonFisher.m), 0, Parti_settl.nonFisher.m), 
         Parti_settl.nonFisher.pct = ifelse(is.nan(Parti_settl.nonFisher.pct), 0, Parti_settl.nonFisher.pct), 
         
         Parti_settl.AgeRetired.m = ifelse(is.nan(Parti_settl.AgeRetired.m), 0, Parti_settl.AgeRetired.m), 
         Parti_settl.AgeRetired.pct = ifelse(is.nan(Parti_settl.AgeRetired.pct), 0, Parti_settl.AgeRetired.pct), 
         Parti_settl.AgeWorking.m = ifelse(is.nan(Parti_settl.AgeWorking.m), 0, Parti_settl.AgeWorking.m), 
         Parti_settl.AgeWorking.pct = ifelse(is.nan(Parti_settl.AgeWorking.pct), 0, Parti_settl.AgeWorking.pct), 
         
         
         Parti_settl.Indigenous.m = ifelse(is.nan(Parti_settl.Indigenous.m), 0, Parti_settl.Indigenous.m), 
         Parti_settl.Indigenous.pct = ifelse(is.nan(Parti_settl.Indigenous.pct), 0, Parti_settl.Indigenous.pct),
         Parti_settl.nonIndigenous.m = ifelse(is.nan(Parti_settl.nonIndigenous.m), 0, Parti_settl.nonIndigenous.m), 
         Parti_settl.nonIndigenous.pct = ifelse(is.nan(Parti_settl.nonIndigenous.pct), 0, Parti_settl.nonIndigenous.pct),
         
         Parti_settl.Adat.m = ifelse(is.nan(Parti_settl.Adat.m), 0, Parti_settl.Adat.m), 
         Parti_settl.Adat.pct = ifelse(is.nan(Parti_settl.Adat.pct), 0, Parti_settl.Adat.pct),
         Parti_settl.nonAdat.m = ifelse(is.nan(Parti_settl.nonAdat.m), 0, Parti_settl.nonAdat.m), 
         Parti_settl.nonAdat.pct = ifelse(is.nan(Parti_settl.nonAdat.pct), 0, Parti_settl.nonAdat.pct))

## --  get mean/median value for each MPA (i.e. to split settlements by participation values later on)
DiD.data.coarseMatch.MPA <- DiD.data.coarseMatch.settl %>% 
  group_by(MPAID, yearsPost) %>% 
  summarise(PovertyIndex_GINI_MPA.m = mean(PovertyIndex_GINI_Settl, na.rm=TRUE), 
            TimeMarket_MPA.m = mean(TimeMarket),
            Parti_MPA.m = mean(Parti_settl.m, na.rm=TRUE),
            Parti_MPA.pct = mean(Parti_settl.pct, na.rm=TRUE),
            
            Parti_MPA.Female.m = mean(Parti_settl.Female.m, na.rm=TRUE),
            Parti_MPA.Male.m = mean(Parti_settl.Male.m, na.rm=TRUE),
            
            Parti_MPA.AgeRetired.m = mean(Parti_settl.AgeRetired.m, na.rm=TRUE),
            Parti_MPA.AgeWorking.m = mean(Parti_settl.AgeWorking.m, na.rm=TRUE),
            
            Parti_MPA.Fisher.m = mean(Parti_settl.Fisher.m, na.rm=TRUE),
            Parti_MPA.nonFisher.m = mean(Parti_settl.nonFisher.m, na.rm=TRUE),
            
            Parti_MPA.Indigenous.m = mean(Parti_settl.Indigenous.m, na.rm=TRUE),
            Parti_MPA.nonIndigenous.m = mean(Parti_settl.nonIndigenous.m, na.rm=TRUE),
            
            Parti_MPA.Adat.m = mean(Parti_settl.Adat.m, na.rm=TRUE),
            Parti_MPA.nonAdat.m = mean(Parti_settl.nonAdat.m, na.rm=TRUE),
            
            Parti_MPA.Female.pct = mean(Parti_settl.Female.pct, na.rm=TRUE),
            Parti_MPA.Male.pct = mean(Parti_settl.Male.pct, na.rm=TRUE),
            
            Parti_MPA.AgeRetired.pct = mean(Parti_settl.AgeRetired.pct, na.rm=TRUE),
            Parti_MPA.AgeWorking.pct = mean(Parti_settl.AgeWorking.pct, na.rm=TRUE),
            
            Parti_MPA.Fisher.pct = mean(Parti_settl.Fisher.pct, na.rm=TRUE),
            Parti_MPA.nonFisher.pct = mean(Parti_settl.nonFisher.pct, na.rm=TRUE),
            
            Parti_MPA.Indigenous.pct = mean(Parti_settl.Indigenous.pct, na.rm=TRUE),
            Parti_MPA.nonIndigenous.pct = mean(Parti_settl.nonIndigenous.pct, na.rm=TRUE),
            
            Parti_MPA.Adat.pct = mean(Parti_settl.Adat.pct, na.rm=TRUE), 
            Parti_MPA.nonAdat.pct = mean(Parti_settl.nonAdat.pct, na.rm=TRUE))

DiD.data.coarseMatch.settl <- DiD.data.coarseMatch.settl %>% 
  mutate(dist.wt=1, pair.id=1, 
         Control = ifelse(Treatment==1,0,1)) 

DiD.data.coarseMatch.settl.base <- DiD.data.coarseMatch.settl %>% 
  filter(yearsPost == 0) %>% 
  select(Fisher, SettlementID, TimeMarket, customary.gov.pct) %>% 
  rename(Fisher.base = Fisher,
         TimeMarket.base = TimeMarket, 
         customary.gov.pct.base = customary.gov.pct)

DiD.data.coarseMatch.settl <- DiD.data.coarseMatch.settl %>% 
  left_join(DiD.data.coarseMatch.settl.base, by=c("SettlementID"))

##-- Define settlement's above-below MPA icipation level 
DiD.data.Parti.sGroup.base <- DiD.data.coarseMatch.settl %>% 
  left_join(DiD.data.coarseMatch.MPA, by=c("MPAID", "yearsPost")) %>% 
  filter(yearsPost==0) %>% 
  mutate(Parti_settl.m.above = ifelse(Parti_settl.m>=Parti_MPA.m, 1, 0),
         Parti_settl.pct.above = ifelse(Parti_settl.pct>=Parti_MPA.pct, 1, 0),
         
         Parti_settl.Female.m.above = ifelse(Parti_settl.Female.m>=Parti_MPA.Female.m, 1, 0),
         Parti_settl.Female.pct.above = ifelse(Parti_settl.Female.pct>=Parti_MPA.Female.pct, 1, 0),
         Parti_settl.Male.m.above = ifelse(Parti_settl.Male.m>=Parti_MPA.Male.m, 1, 0),
         Parti_settl.Male.pct.above = ifelse(Parti_settl.Male.pct>=Parti_MPA.Male.pct, 1, 0),
         
         Parti_settl.Fisher.m.above = ifelse(Parti_settl.Fisher.m>=Parti_MPA.Fisher.m, 1, 0),
         Parti_settl.Fisher.pct.above = ifelse(Parti_settl.Fisher.pct>=Parti_MPA.Fisher.pct, 1, 0),
         Parti_settl.nonFisher.m.above = ifelse(Parti_settl.nonFisher.m>=Parti_MPA.nonFisher.m, 1, 0),
         Parti_settl.nonFisher.pct.above = ifelse(Parti_settl.nonFisher.pct>=Parti_MPA.nonFisher.pct, 1, 0),
         
         Parti_settl.Indigenous.m.above = ifelse(Parti_settl.Indigenous.m>=Parti_MPA.Indigenous.m, 1, 0),
         Parti_settl.Indigenous.pct.above = ifelse(Parti_settl.Indigenous.pct>=Parti_MPA.Indigenous.pct, 1, 0),
         Parti_settl.nonIndigenous.m.above = ifelse(Parti_settl.nonIndigenous.m>=Parti_MPA.nonIndigenous.m, 1, 0),
         Parti_settl.nonIndigenous.pct.above = ifelse(Parti_settl.nonIndigenous.pct>=Parti_MPA.nonIndigenous.pct, 1, 0),
         
         Parti_settl.AgeRetired.m.above = ifelse(Parti_settl.AgeRetired.m>=Parti_MPA.AgeRetired.m, 1, 0),
         Parti_settl.AgeRetired.pct.above = ifelse(Parti_settl.AgeRetired.pct>=Parti_MPA.AgeRetired.pct, 1, 0),
         Parti_settl.AgeWorking.m.above = ifelse(Parti_settl.AgeWorking.m>=Parti_MPA.AgeWorking.m, 1, 0),
         Parti_settl.AgeWorking.pct.above = ifelse(Parti_settl.AgeWorking.pct>=Parti_MPA.AgeWorking.pct, 1, 0),
         
         Parti_settl.Adat.m.above = ifelse(Parti_settl.Adat.m>=Parti_MPA.Adat.m, 1, 0),
         Parti_settl.Adat.pct.above = ifelse(Parti_settl.Adat.pct>=Parti_MPA.Adat.pct, 1, 0),
         Parti_settl.nonAdat.m.above = ifelse(Parti_settl.nonAdat.m>=Parti_MPA.nonAdat.m, 1, 0),
         Parti_settl.nonAdat.pct.above = ifelse(Parti_settl.nonAdat.pct>=Parti_MPA.nonAdat.pct, 1, 0)) 


##--- merge the baseline participation moderating factors in DiD.data
DiD.data.coarseMatch <- DiD.data.coarseMatch %>% 
  left_join(select(DiD.data.Parti.sGroup.base, SettlementID, Parti_settl.m.above:Parti_settl.nonAdat.pct.above), by=c("SettlementID"))


##--- merge the baseline participation moderating factors in DiD.data.settl (for GINI regression)  
DiD.data.coarseMatch.settl <- DiD.data.coarseMatch.settl %>% 
  left_join(select(DiD.data.Parti.sGroup.base, SettlementID, Parti_settl.m.above:Parti_settl.nonAdat.pct.above), by=c("SettlementID"))

#---------------------------------------------------------------------------#
# --- Settlement-level regressions: Poverty Index, EconTrend, and Poverty's Gini

model.out.GINI <- data.frame()
varNames <- c("PovertyIndex_GINI_Settl", "PovertyIndex_pca", "EconTrend_increase")


regValue.list <-list()
master.reg.list <- list()

for (i in varNames) {
  reg.df <- DiD.data.coarseMatch.settl
  Y <- as_vector(reg.df[,i])
  w <- as_vector(reg.df[,"dist.wt"])
  
  ## --- 1. base (MPAID only)
  regValue <- felm(Y  ~ Treatment + Post + Treatment:Post 
                   # + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
                   | MPAID  + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
  regValue.1 <- regValue
  
  reg.broom1 <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, Spec="base", sample="All Settlements")
  
  ## --- 2. base + InterviewYear
  regValue <- felm(Y  ~ Treatment + Post + Treatment:Post 
                   # + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
                   | MPAID + InterviewYear + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
  regValue.2 <- regValue
  
  reg.broom2 <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, Spec="base + InterviewYear", sample="All Settlements")
  
  ## --- 3. base + settlement's baseline controls
  regValue <- felm(Y  ~ Treatment + Post + Treatment:Post 
                   + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
                   | MPAID  + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
  regValue.3 <- regValue
  
  reg.broom3 <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, Spec="base + controls", sample="All Settlements")
  
  ## --- 4. base + controls + interview year
  regValue <- felm(Y  ~ Treatment + Post + Treatment:Post 
                   + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
                   | MPAID  + InterviewYear + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
  regValue.4 <- regValue
  
  reg.broom4 <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, Spec="base+InterviewYear+control", sample="All Settlements")
  
  
  
  ##-------Now use model 4, splitting to sub-samples of below and above participation
  ##-- Parti.above == 1
  reg.df <- DiD.data.coarseMatch.settl %>%  filter(Parti_settl.pct.above==1)
  Y <- as_vector(reg.df[,i])
  w <- as_vector(reg.df[,"dist.wt"])
  
  regValue <- felm(Y  ~ Treatment + Post + Treatment:Post 
                   + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
                   | MPAID + InterviewYear + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
  regValue.5 <- regValue
  
  reg.broom5 <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, Spec="base+InterviewYear+control", sample="High Participation")
  
  ##-- Parti.above == 0
  reg.df <- DiD.data.coarseMatch.settl %>%  filter(Parti_settl.pct.above==0)
  Y <- as_vector(reg.df[,i])
  w <- as_vector(reg.df[,"dist.wt"])
  
  regValue <- felm(Y  ~ Treatment + Post + Treatment:Post 
                   + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
                   | MPAID   + InterviewYear + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
  regValue.6 <- regValue
  
  reg.broom6 <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, Spec="base+InterviewYear+control", sample="Low Participation")
  
  master.reg.list[[i]] <- list(regValue.1,regValue.2,regValue.3,regValue.4, regValue.5 ,regValue.6)
}

names(master.reg.list$PovertyIndex_GINI_Settl) <- paste0("Spec ",seq(1:6))
names(master.reg.list$PovertyIndex_pca) <- paste0("Spec ",seq(1:6))
names(master.reg.list$EconTrend_increase) <- paste0("Spec ",seq(1:6))


model.out.GINI <- rbind(reg.broom1,reg.broom2, reg.broom3, reg.broom4, reg.broom5, reg.broom6)
model.out.GINI[order(model.out.GINI$term),]


# ##-------------------------------------------##
# ##-- Now try triple interaction term (not adding into the masterRegList for now; just for reference) --> negataive but insignificant triple-intraction term
# varNames <- c("PovertyIndex_GINI_Settl")
# 
# for (i in varNames) {
#   reg.df <- DiD.data.coarseMatch.settl 
#   Y <- as_vector(reg.df[,i])
#   w <- as_vector(reg.df[,"dist.wt"])
#   
#   regValue <- felm(Y  ~ Treatment + Post + Parti_settl.m.above + Treatment:Post + Treatment:Parti_settl.m.above + Post:Parti_settl.m.above + Treatment:Post:Parti_settl.m.above +
#                    + Fisher.base + TimeMarket.base + lat + long + customary.gov.pct.base 
#                    | MPAID   + InterviewYear + pair.id | 0 | 0, data=reg.df, exactDOF = TRUE, weights=w)
# }  
# reg.broom <- tidy(regValue) 
# reg.broom
# ##-------------------------------------------##


# ##-------------------------------------------------------------------------------##
# ##-- Output Sensitivity result tables for GINI outcome
# ##save html (can open in MS Word)
# stargazer(master.reg.list$PovertyIndex_GINI_Settl[c("Spec 1","Spec 2","Spec 3","Spec 4", "Spec 5", "Spec 6")],
#           out = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/GINI_impact"), type = "html", 
#           df = FALSE, notes.append = FALSE, star.cutoffs = NA, omit.table.layout = "n", 
#           #keep = c("Treatment:Post1"), 
#           #covariate.labels=c("Treatment X Post"),
#           column.labels=names(master.reg.list$PovertyIndex_GINI_Settl),
#           title="MPA impact on Settlement's poverty GINI: DiD Regression Results",
#           align=TRUE, dep.var.labels="Poverty Index's GINI",
#           add.lines = list(c("MPA-cluster FEs","Yes","Yes","Yes","Yes", "Yes","Yes"),
#                            c("Interview Year FEs","No","Yes","No","Yes","Yes","Yes"),
#                            c("Sample","All","All","All","All","High Participation","Low Participation")))


##-------------------------------------------------------------------------------##
##-- Settlement-level results (with model sensitivity tests) for 
##-- 1. Poverty_pca; 2.Poverty_GINI outcome; 3. EconTrend_decrease

##save html (can open in MS Word or Excel)
stargazer(master.reg.list$PovertyIndex_GINI_Settl[c("Spec 1","Spec 2","Spec 3","Spec 4")],
          out = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/Settle_GINI_impact_Nov2020"), type = "html", 
          df = FALSE, notes.append = FALSE,  omit.table.layout = "n", 
          #keep = c("Treatment:Post1"), 
          #covariate.labels=c("Treatment X Post"),
          column.labels=names(master.reg.list$PovertyIndex_GINI_Settl),
          title="MPA impact: Settlement level regression",
          align=TRUE, dep.var.labels="Poverty Index's GINI",
          add.lines = list(c("MPA-cluster FEs","Yes","Yes","Yes","Yes"),
                           c("Interview Year FEs","No","Yes","No","Yes"),
                           c("Settlement Covariates","No","No","Yes","Yes")))

##save html (can open in MS Word or Excel)
stargazer(master.reg.list$PovertyIndex_pca[c("Spec 1","Spec 2","Spec 3","Spec 4")],
          out = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/Settle_poverty_impact_Nov2020"), type = "html", 
          df = FALSE, notes.append = FALSE, omit.table.layout = "n", 
          #keep = c("Treatment:Post1"), 
          #covariate.labels=c("Treatment X Post"),
          column.labels=names(master.reg.list$PovertyIndex_GINI_Settl),
          title="MPA impact: Settlement level regression",
          align=TRUE, dep.var.labels="Poverty Index PCA",
          add.lines = list(c("MPA-cluster FEs","Yes","Yes","Yes","Yes"),
                           c("Interview Year FEs","No","Yes","No","Yes"),
                           c("Settlement Covariates","No","No","Yes","Yes")))


##save html (can open in MS Word or Excel)
stargazer(master.reg.list$EconTrend_increase[c("Spec 1","Spec 2","Spec 3","Spec 4")],
          out = paste0(resultPath,"Paper 1-MPA and Equity/results/2020/tables/Settle_EconTrend_impact_Nov2020"), type = "html", 
          df = FALSE, notes.append = FALSE, omit.table.layout = "n", 
          #keep = c("Treatment:Post1"), 
          #covariate.labels=c("Treatment X Post"),
          column.labels=names(master.reg.list$PovertyIndex_GINI_Settl),
          title="MPA impact: Settlement level regression",
          align=TRUE, dep.var.labels="Econ Trend",
          add.lines = list(c("MPA-cluster FEs","Yes","Yes","Yes","Yes"),
                           c("Interview Year FEs","No","Yes","No","Yes"),
                           c("Settlement Covariates","No","No","Yes","Yes")))



##-- Drawing impacts for All, "High Participation",and "Low Participation"

# GINI.plot <- ggplot(filter(model.out.GINI, term=="Impact", Spec=="base+InterviewYear+control"),aes(x=sample, y=estimate)) +
#   geom_bar(stat="identity", fill="white", color="black", position =pd, width = 0.5, size=1)+ 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
#   theme_bw() + theme(legend.position="none", 
#                      axis.text.x=element_text(size=10, angle = 0, hjust = 0.5, vjust = 1),
#                      axis.text.y=element_text(size=10, angle = 90, hjust = 0.5, vjust = 1)) +
#   geom_hline(yintercept = 0, linetype = "dashed") + 
#   geom_vline(xintercept = 1.5, linetype = "dashed") + 
#   labs(x="",y="Impact Estimates", title="GINI Index: Heterogenous Impacts on Poverty Reduction by Community Cohesiveness")  
# GINI.plot

#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/Impact_plots/GINI_impacts.jpg"), width = 8, height = 8)



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 7. REGRESSION ANALYSIS 3----

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 7a. Moderating Factor: Communiy Participation  ----

model.out.subgroup.Parti <- data.frame()
reg.broom.cumulative <- data.frame() 

varNames <- c("PovertyIndex_pca", "EconTrend_increase")
#varNames <- c("PovertyIndex_pca")

for (i in varNames) {
  ##----------------Gender------------------------------##
  
  ##----DiD with Female group 
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(Male==0, Parti_settl.Female.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Gender", subgroup_id=0, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(Male==0, Parti_settl.Female.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Gender", subgroup_id=0, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  ##----DiD with Male group 
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(Male==1, Parti_settl.Female.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Gender", subgroup_id=1, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(Male==1, Parti_settl.Female.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Gender", subgroup_id=1, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  
  ##----------------Tenureship (by Adat)------------------------------##
  
  ##----DiD with nonAdat group 
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(adat==0, Parti_settl.nonAdat.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Tenureship", subgroup_id=0, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(adat==0, Parti_settl.nonAdat.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Tenureship", subgroup_id=0, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  ##----DiD with Adat group 
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(adat==1, Parti_settl.nonAdat.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Tenureship", subgroup_id=1, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(adat==1, Parti_settl.nonAdat.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Tenureship", subgroup_id=1, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  
  ##----------------Age (retirement vs working)------------------------------##
  ##---Age = Retired
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(age.retired==1, Parti_settl.AgeRetired.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID+ InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Age", subgroup_id=0, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(age.retired==1, Parti_settl.AgeRetired.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID+ InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Age", subgroup_id=0, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  ##---Age = Working
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(age.retired==0, Parti_settl.AgeRetired.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID+ InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Age", subgroup_id=1, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)
  
  
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(age.retired==0, Parti_settl.AgeRetired.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID+ InterviewYear + pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Age", subgroup_id=1, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)  
  
  
  ##----------------Occupation (fisher/non-fisher)------------------------------##
  
  ##---DiD Regressions: by Occupation (non-fisher only)
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==0, Parti_settl.nonFisher.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=0, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)  
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==0, Parti_settl.nonFisher.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=0, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)  
  
  
  ##---DiD Regressions: by Occupation (fisher only)
  #-- Parti_settl.pct.above==1
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==1, Parti_settl.nonFisher.pct.above==1)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=1, sample="High Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)  
  
  
  #-- Parti_settl.pct.above==0
  reg.df <- DiD.data.coarseMatch %>% filter(Fisher==1, Parti_settl.nonFisher.pct.above==0)
  Y <- reg.df[,i]
  w <- reg.df[,"dist.wt"]
  
  regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
                   | SettlementID + InterviewYear+ pair.id | 0 | SettlementID, data=reg.df, exactDOF = TRUE, weights=w)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i, subgroup="Fishing Livelihood", subgroup_id=1, sample="Low Participation")
  
  reg.broom.cumulative <- rbind(reg.broom.cumulative, reg.broom)  
  
  ## -- binding all reg.brooms for first outcome variable and into model.out df
  model.out.subgroup.Parti <- reg.broom.cumulative
}

## -- binding all model.out dfs for all outcome vars together
model.out.subgroup.Parti <- rbind(model.out.subgroup.Parti, reg.broom.cumulative)


##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
model.out.subgroup.Parti <- model.out.subgroup.Parti %>%
  mutate(Indicator= ifelse(Response=="EconTrend_increase", "2. Economic Stability or Improvement (Household perception [subjective])", 
                           ifelse(Response=="PovertyIndex_pca", "1. Poverty Alleviation Index (Asset-based [objective])", "")),
         Indicator=gsub(" \\(", "\n \\(", Indicator)) %>% 
  mutate(subgroup=ifelse(subgroup=="Indigenous", "Residence", subgroup), 
         subgroup=ifelse(subgroup=="Fishing Livelihood", "Livelihood", subgroup)) %>% 
  mutate(subgroup_label = ifelse(subgroup=="Gender" & subgroup_id==0, "Female",    
                                 ifelse(subgroup=="Gender" & subgroup_id==1, "Male", 
                                        ifelse(subgroup=="Tenureship" & subgroup_id==0, " non-Adat", 
                                               ifelse(subgroup=="Tenureship" & subgroup_id==1, "Adat", 
                                                      ifelse(subgroup=="Livelihood" & subgroup_id==0, " non-Fisher", 
                                                             ifelse(subgroup=="Livelihood" & subgroup_id==1, "Fisher",
                                                                    ifelse(subgroup=="Age" & subgroup_id==0, "Retirement-age", "Working-age"))))))))

##----------------------------------------------------------------##
pd <- position_dodge(width=.5) # move them .05 to the left and right

# Gender
Gender.plot.Parti <- ggplot(filter(model.out.subgroup.Parti,term=="Impact",subgroup=="Gender"),aes(x=Indicator, y=estimate, fill=subgroup_label, group=subgroup_label)) +
  facet_grid(. ~ sample) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.5,0.8), legend.title = element_blank(), legend.text = element_text(size=10),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
                     axis.title.y=element_text(size=12,face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="Gender")  
Gender.plot.Parti


# Livelihood
Livelihood.plot.Parti <- ggplot(filter(model.out.subgroup.Parti,term=="Impact",subgroup=="Livelihood"),aes(x=Indicator, y=estimate, fill=subgroup_label, group=subgroup_label)) +
  facet_grid(. ~ sample) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.5,0.8), legend.title = element_blank(), legend.text = element_text(size=10),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
                     axis.title.y=element_text(size=12,face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="Livelihood")  
Livelihood.plot.Parti


# # Residence
# Residence.plot.Parti <- ggplot(filter(model.out.subgroup.Parti,term=="Impact",subgroup=="Residence"),aes(x=Indicator, y=estimate, fill=subgroup_label, group=subgroup_label)) +
#   facet_grid(. ~ sample) +
#   geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
#   theme_bw() + theme(legend.position=c(0.5,0.8), legend.title = element_blank(), legend.text = element_text(size=10),
#                      legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
# axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
# axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
# axis.title.y=element_text(size=12,face="bold")) +
#   geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
#   labs(x="",y="Impact Estimates", title="Indigenousness")  
# Residence.plot.Parti

# Tenureship
Tenureship.plot.Parti <- ggplot(filter(model.out.subgroup.Parti,term=="Impact",subgroup=="Tenureship"),aes(x=Indicator, y=estimate, fill=subgroup_label, group=subgroup_label)) +
  facet_grid(. ~ sample) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.5,0.8), legend.title = element_blank(), legend.text = element_text(size=10),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
                     axis.title.y=element_text(size=12,face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="Tenureship")  
Tenureship.plot.Parti

# Age
Age.plot.Parti <- ggplot(filter(model.out.subgroup.Parti,term=="Impact",subgroup=="Age"),aes(x=Indicator, y=estimate, fill=subgroup_label, group=subgroup_label)) +
  facet_grid(. ~ sample) +
  geom_bar(stat="identity", position =pd, width = 0.5, size=1)+ 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=1.5, position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.1, size=0.3, position = pd ) +
  theme_bw() + theme(legend.position=c(0.5,0.8), legend.title = element_blank(), legend.text = element_text(size=10),
                     legend.background = element_rect(size=0.5, linetype="solid", color="black"), 
                     axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
                     axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
                     axis.title.y=element_text(size=12,face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed") + scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="",y="Impact Estimates", title="Age Group")  
Age.plot.Parti


##combine PLOTS
plot_grid(Gender.plot.Parti, Age.plot.Parti, Livelihood.plot.Parti, Tenureship.plot.Parti, ncol=1)
ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseScale/Poverty_EconTrend_4groups_Parti_OneSampleSplit.jpg"), width=12, height=14)



# ---- 7b. Moderating Factor: Communiy Participation (add. evidence)  ----

#summary stats

DiD.sumstat.Parti.settl.base <- DiD.data.coarseMatch.settl %>%
  filter(yearsPost==0) %>% 
  select(Treatment, Parti_settl.pct:Parti_settl.nonAdat.pct) 
DiD.sumstat.Parti.settl.base.out <- describeBy(DiD.sumstat.Parti.settl.base)
write_xlsx(DiD.sumstat.Parti.settl.base.out, path = paste0(resultPath, "Paper 1-MPA and Equity/results/2020/tables/Summary_Stat_Parti.xlsx"), col_names = TRUE, format_headers = TRUE)



DiD.sumstat.Parti.settl.base.treat <- DiD.sumstat.Parti.settl.base %>%  filter(Treatment==1)
DiD.sumstat.Parti.settl.base.treat <- describeBy(DiD.sumstat.Parti.settl.base.treat)
write_xlsx(DiD.sumstat.Parti.settl.base.treat, path = paste0(resultPath, "Paper 1-MPA and Equity/results/2020/tables/Summary_Stat_Parti_Treat.xlsx"), col_names = TRUE, format_headers = TRUE)

DiD.sumstat.Parti.settl.base.control <- DiD.sumstat.Parti.settl.base %>%  filter(Treatment==0)
DiD.sumstat.Parti.settl.base.control <- describeBy(DiD.sumstat.Parti.settl.base.control)
write_xlsx(DiD.sumstat.Parti.settl.base.control, path = paste0(resultPath, "Paper 1-MPA and Equity/results/2020/tables/Summary_Stat_Parti_control.xlsx"), col_names = TRUE, format_headers = TRUE)

describe(DiD.sumstat.Parti.settl.base)


# Correlation plots
library(psych)
DiD.sumstat.Parti.settl.base.1 <- DiD.sumstat.Parti.settl.base %>%
  select(Parti_settl.Female.pct:Parti_settl.nonFisher.pct, Parti_settl.Adat.pct:Parti_settl.nonAdat.pct) %>% 
  transmute(Female = Parti_settl.Female.pct, 
            Male = Parti_settl.Male.pct, 
            Retired = Parti_settl.AgeRetired.pct, 
            Working = Parti_settl.AgeWorking.pct, 
            Fisher = Parti_settl.Fisher.pct, 
            non_Fisher = Parti_settl.nonFisher.pct, 
            Adat = Parti_settl.Adat.pct, 
            non_Adat = Parti_settl.nonAdat.pct)

pairs.panels(DiD.sumstat.Parti.settl.base.1 %>% select(-SettlementID), scale=TRUE, )



