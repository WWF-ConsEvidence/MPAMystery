##----------04/13/2020-----------------##
##----------Word Frequency by Subgroups-----------------##

#setwd("D:/Dropbox/MPA_research/MPAMystery/")
source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
pacman::p_load(lfe,cowplot,stargazer,broom,qvalue,psych,factoextra,ineq, sf, tidytext, plotly, tidyverse)

#resultPath <- "D:/Dropbox/MPA_research/"
resultPath <- "C:/Users/dtl20/Dropbox/MPA_research/"

soc.coord<- import("x_Flat_data_files/1_Social/Inputs/soc.coord.xlsx")

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
  left_join(soc.coord,by="SettlementID")


summary(DiD.data)
# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other


# --- Filter to look at 6 BHS and 4 SBS MPAs (with t2 and/or t4 data) --- Jan 2020
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6, 15:18))

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
         Boat_no = ifelse(Boat>0,1,0),
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
# mutate(Household_asset = Entertain + 2*PhoneCombined + 3*Satellite + 4*TV + 5*Generator,
#        Boats_w1 = BoatNoMotor + 2*BoatOutboard + 3*BoatInboard,
#        Boats_w2 = 6*BoatNoMotor + 7*BoatOutboard + 8*BoatInboard,
#        Boats_motor_w1 = 1*BoatOutboard + 2*BoatInboard,
#        Boats_motor_w2 = 7*BoatOutboard + 8*BoatInboard,
#        Vehicles_w1 = Bicycle + 2*Motorcycle + 3*CarTruck,
#        Vehicles_w2 = 9*Bicycle + 10*Motorcycle + 11*CarTruck) %>% 
# mutate(Boats_dum = ifelse(BoatNoMotor>0 | BoatOutboard >0 | BoatInboard >0,1,0),
#        Boats_motor_dum = ifelse(BoatOutboard >0 | BoatInboard >0,1,0),
#        Vehicles_dum = ifelse(Bicycle>0 | Motorcycle>0 | CarTruck>0,1,0))

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

# --- Categorizing cooking fuel indicator
plyr::count(WELLBEING, "CookingFuel")
cooking.fuel.df <- WELLBEING %>% 
  select(HouseholdID, CookingFuel) %>% 
  mutate(CookingFuel=ifelse(CookingFuel==0| CookingFuel>=998, NA, CookingFuel),
         fuel.elec=ifelse(CookingFuel==1,1,0),
         fuel.oil=ifelse(CookingFuel==2,1,0),
         fuel.wood=ifelse(CookingFuel==3,1,0),
         fuel.other=ifelse(CookingFuel==4|CookingFuel==5|CookingFuel==6,1,0))

DiD.data <- DiD.data %>% 
  select(-CookingFuel.Biomass) %>% 
  left_join(cooking.fuel.df, by=c("HouseholdID")) %>% 
  mutate(fuel.biomass=ifelse(fuel.elec==0 & fuel.oil==0, 1, 0))

# --- summary(DiD.data)
DiD.data.summary <- DiD.data %>% 
  select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  

WELLBEING.summary <- WELLBEING %>% 
  select(MPAID,InterviewYear, AssetCar:AssetGenerator) %>% 
  group_by(MPAID,InterviewYear) %>%
  summarise(AssetCar.count.993=sum(AssetCar==993),
            AssetTruck.count.993=sum(AssetTruck==993),
            AssetCarTruck.count.993=sum(AssetCarTruck==993),
            AssetBicycle.count.993=sum(AssetBicycle==993),
            AssetMotorcycle.count.993=sum(AssetMotorcycle==993),
            AssetLandlinePhone.count.993=sum(AssetLandlinePhone==993),
            AssetCellPhone.count.993=sum(AssetCellPhone==993),
            AssetPhoneCombined.count.993=sum(AssetPhoneCombined==993),
            AssetRadio.count.993=sum(AssetRadio==993),
            AssetStereo.count.993=sum(AssetStereo==993),
            AssetCD.count.993=sum(AssetCD==993),
            AssetDVD.count.993=sum(AssetDVD==993),
            AssetEntertain.count.993=sum(AssetEntertain==993))


# calculate Z scores (standardized values for each of the Big Five and the sub_asset groups)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, CarTruck:Generator, Boat, fuel.elec:fuel.biomass, MTIndex_AccHarv, MTIndex_ManExcTrans, SocialConflict, SocialConflict_increase), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()
summary(DiD.data)
# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---2. Generating subGroup indicators (Male, Fisher, age.retired, yrResident.above)
##---2. Generating subGroup indicators


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
##---END---##
##---END---##

##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---2. merging Econ Response Indicator

#---- Import ethnic and education look up tables ----
ethnic.lkp<- import("x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx")
education.lkp <- import("x_Flat_data_files/1_Social/Inputs/education_lkp_BHS.xlsx")

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


#-------------------------------------#
Econ.reason <- import('R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Economic_StatusReason_Classification_OUtput_20191126.csv')

# Import the "unclear" list 
unclear <- unique(Econ.reason$Unclear)

Econ.reason <- Econ.reason %>% 
  select(HouseholdID, EconomicStatusTrend, EconomicStatusReason, EconomicStatusReasonEnglish) 


##---dataframe with EconResponse and subgroup indicators 
subgroup.EconResponse.df <- DiD.data %>% 
  select(HouseholdID, MPAID, SettlementID, InterviewYear, yearsPost, Treatment, Male, Fisher, yrResident.above, age.retired) %>% 
  left_join(select(Econ.reason, HouseholdID, EconomicStatusTrend, EconomicStatusReason, EconomicStatusReasonEnglish), by="HouseholdID")

# Haven't translated for SBS yet, so doing for with BHS for now
subgroup.EconResponse.df <- subgroup.EconResponse.df %>%  filter(MPAID%in%1:6) 



#---------------------------------------------------------------------------------------------------------#
#--- 1. Plots by Gender (what are the top words/top distinct words for each)
Gender.out <- subgroup.EconResponse.df %>% 
  select(Male, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish), 
         Gender=ifelse(Male==1, "Male", "Female"))

words.by.Gender.n <- Gender.out %>% 
  group_by(Gender) %>% 
  summarise(Gender.count=n()) 

# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.Gender <- Gender.out %>% 
  # filter(MPAID <= 6) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsBySubGroup, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsBySubGroup %in% stop_words$word) %>% 
  filter(!wordsBySubGroup %in% unclear) %>% 
  #filter(!wordsBySubGroup %in% c("due","stable","cost", "2")) %>% 
  count(Gender, wordsBySubGroup, sort = TRUE) %>% 
  left_join(words.by.Gender.n,by="Gender")  
 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.Gender <- words.by.Gender %>%
  bind_tf_idf(wordsBySubGroup, Gender, n)

##---------------------------------------------------------------------#
#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.Gender %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsBySubGroup = factor(wordsBySubGroup, levels = rev(unique(wordsBySubGroup)))) %>% 
  group_by(Gender) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsBySubGroup, tf_idf, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0(resultPath,'Paper 1-MPA and Equity/results/plots/subGroup_word_frequency/ByGender_distinct.pdf'),width = 12, height = 8)



# 2. top all words (not distinct) by econ status change
words.by.Gender %>%
  arrange(desc(n)) %>%
  mutate(wordsBySubGroup = factor(wordsBySubGroup, levels = rev(unique(wordsBySubGroup)))) %>% 
  group_by(Gender) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsBySubGroup, n, fill = Gender)) +
  #ggplot(aes(x = reorder(f.name, -age), y = age))
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0(resultPath,'Paper 1-MPA and Equity/results/plots/subGroup_word_frequency/ByGender_full.pdf'),width = 12, height = 8)

export(words.by.Gender,paste0(resultPath,'Paper 1-MPA and Equity/results/plots/subGroup_word_frequency/ByGender_full.xlsx'))


