# 
# code:  Source social data from flat files for all social MPA Mystery analysis
# 
# github: WWF-ConsEvidence/MPAMystery/1_Data_wrangling/1_Social/2_Source_data
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: May 2019
# modified: 
# 
# 
# ---- inputs ----
#  1) Exported HH_tbl_WELLBEING.csv in x_Flat_data_files/Inputs
#  2) Exported HH_tbl_DEMOGRAPHIC.csv in x_Flat_data_files/Inputs
#  3) Exported HH_tbl_SETTLEMENT.csv in x_Flat_data_files/Inputs
# 
# ---- outputs ----
#  1) HHData data frame for all analyses on the BigFive & Middle15 variables 
#      (used in technical reports and impact summaries)
#  2) IndDemos data frame for all analyses on the BigFive & Middle15 variables
#      (used in technical reports and impact summaries)
#  3) Organization data frame for analyses on marine organization participation 
#      (used in technical reports for Sunda Banda Seascape)
# 
# ---- code sections ----
#  1) LOAD LIBRARIES AND DATA
#  2) CLEAN & POST-CODE DATA
#  3) OPTIONAL FILTERING BY SEASCAPE OR MPA
# 
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Load libraries & data ----

pacman::p_load(rio,dplyr)

# Sourcing most recent files

# Date in format YYYYMMDD (could be changed but we believe it makes most sense 
# to avoid hyphens in file names and to have the date first so files get sorted chronologically)
today.date <- gsub("-","",Sys.Date())

# Files (with package rio)
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))}

# # Shapefiles (with package sf)
# st_last.file <- function(dir.nam,nam){
#   st_read(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))}
# 
# # RData files
# last.Rdata <- function(dir.nam,nam){
#   load(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
# }


# ---- 1.2 Import data ----

WELLBEING <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/',nam='HH_tbl_WELLBEING')
DEMOGRAPHIC <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/',nam='HH_tbl_DEMOGRAPHIC')
SETTLEMENT <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/',nam='HH_tbl_SETTLEMENT')
ORGANIZATION <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/',nam='HH_tbl_ORGANIZATION')


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CLEAN & POST-CODE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Clean & post-code WELLBEING to create HHData for analysis ----

HHData <- 
  WELLBEING %>%
  transmute(HouseholdID = HouseholdID, 
                   MPAID = MPAID, 
                   SettlementID = SettlementID, 
                   InterviewYear = InterviewYear,
                   DidNotLastCoded = as.integer(ifelse((FSDidNotLast==1 | FSDidNotLast==2),1,ifelse(FSDidNotLast==3,0,990))),
                   BalancedDietCoded = as.integer(ifelse((FSBalancedDiet==1 | FSBalancedDiet==2),1,ifelse(FSBalancedDiet==3,0,990))),
                   FreqAdultSkipCoded = as.integer(ifelse((FSFreqAdultSkip==1 | FSFreqAdultSkip==2),1,ifelse(FSFreqAdultSkip==3 | FSAdultSkip==0,0,990))),
                   AdultSkipCoded = as.integer(ifelse(FSAdultSkip==1,1,ifelse(FSAdultSkip==0,0,990))),
                   EatLessCoded = as.integer(ifelse(FSEatLess==1,1, ifelse(FSEatLess==0,0,990))),
                   HungryCoded = as.integer(ifelse(FSHungry==1,1,ifelse(FSHungry==0,0,990))),
                   DidNotLast = as.integer(ifelse(DidNotLastCoded==990,ifelse((BalancedDietCoded==1 | AdultSkipCoded==1 | EatLessCoded==1 | 
                                                                                 FreqAdultSkipCoded==1 | HungryCoded==1),1,NA),DidNotLastCoded)),
                   BalancedDiet = as.integer(ifelse(BalancedDietCoded==990,ifelse((DidNotLast==1 & (AdultSkipCoded==1 | EatLessCoded==1 | 
                                                                                                      FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),BalancedDietCoded)),
                   AdultSkip = as.integer(ifelse(AdultSkipCoded==990,ifelse(FreqAdultSkipCoded==1 | (DidNotLast==1 & BalancedDiet==1 & 
                                                                                                       (EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),AdultSkipCoded)),
                   EatLess = as.integer(ifelse(EatLessCoded==990,ifelse((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & (FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),EatLessCoded)),
                   FreqAdultSkip = as.integer(ifelse(FreqAdultSkipCoded==990,ifelse((DidNotLast==1 & BalancedDiet==1 & 
                                                                                       AdultSkip==1 & EatLess==1 & HungryCoded==1),1,NA),FreqAdultSkipCoded)),
                   Hungry = as.integer(ifelse(HungryCoded==990,ifelse((DidNotLast==1 & BalancedDiet==1 & FreqAdultSkip==1 & 
                                                                         AdultSkip==1 & EatLess==1),1,NA),HungryCoded)),
                   CarTruck = as.integer(ifelse(AssetCarTruck>989,NA,AssetCarTruck*11)),
                   Bicycle = as.integer(ifelse(AssetBicycle>989,NA,AssetBicycle*9)),
                   Motorcycle = as.integer(ifelse(AssetMotorcycle>989,NA,AssetMotorcycle*10)),
                   BoatNoMotor = as.integer(ifelse(AssetBoatNoMotor>989,NA,AssetBoatNoMotor*6)),
                   BoatOutboard = as.integer(ifelse(AssetBoatOutboard>989,NA,AssetBoatOutboard*7)),
                   BoatInboard =  as.integer(ifelse(AssetBoatInboard>989,NA,AssetBoatInboard*8)),
                   PhoneCombined = as.integer(ifelse(AssetPhoneCombined>989,NA,AssetPhoneCombined*4)),
                   TV = as.integer(ifelse(AssetTV>989,NA,AssetTV*2)),
                   Entertain = as.integer(ifelse(AssetEntertain>989,NA,AssetEntertain*1)),
                   Satellite = as.integer(ifelse(AssetSatellite>989,NA,AssetSatellite*3)),
                   Generator = as.integer(ifelse(AssetGenerator>989,NA,AssetGenerator*5)),
                   PlaceHappy = as.integer(ifelse(PlaceHappy<6,PlaceHappy,NA)),
                   PlaceFavourite = as.integer(ifelse(PlaceFavourite<6,PlaceFavourite,NA)),
                   PlaceMiss = as.integer(ifelse(PlaceMiss<6,PlaceMiss,NA)),
                   PlaceBest = as.integer(ifelse(PlaceBest<6,PlaceBest,NA)),
                   PlaceFishHere = as.integer(ifelse(PlaceFishHere<6,PlaceFishHere,NA)),
                   PlaceBeMyself = as.integer(ifelse(PlaceBeMyself<6,PlaceBeMyself,NA)),
                   RightsAccess = as.integer(ifelse(RightsAccess>989,NA,RightsAccess)),
                   RightsHarvest = as.integer(ifelse(RightsHarvest>989,NA,RightsHarvest)),
                   RightsManage = as.integer(ifelse(RightsManage>989,NA,RightsManage)),
                   RightsExclude = as.integer(ifelse(RightsExclude>989,NA,RightsExclude)),
                   RightsTransfer = as.integer(ifelse(RightsTransfer>989,NA,RightsTransfer)),
                   ChildPortionCoded = as.integer(ifelse(FSChildPortion==1,1,ifelse(FSChildPortion==0,0,990))),
                   LowCostFoodCoded = as.integer(ifelse((FSLowCostFood==1 | FSLowCostFood==2),1,ifelse(FSLowCostFood==3,0,990))),
                   ChildSkipCoded =  as.integer(ifelse(FSChildSkip==1,1,ifelse(FSChildSkip==0,0,990))),
                   FreqChildSkipCoded = as.integer(ifelse((FSFreqChildSkip==1 | FSFreqChildSkip==2),1,ifelse(FSFreqChildSkip==3,0,990))),
                   NoMealChildCoded = as.integer(ifelse((FSNoMealChild==1 | FSNoMealChild==2),1,ifelse(FSNoMealChild==3,0,990))),
                   LowCostFood = as.integer(ifelse(LowCostFoodCoded==990,ifelse((ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | 
                                                                                   NoMealChildCoded==1),1,NA),LowCostFoodCoded)),
                   ChildBalancedMeal = as.integer(ifelse((LowCostFood==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | 
                                                                              NoMealChildCoded==1)),1,0)),
                   ChildNotEnough =  as.integer(ifelse((LowCostFood==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | 
                                                                            FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                   ChildPortion = as.integer(ifelse(ChildPortionCoded==990,ifelse((LowCostFood==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | 
                                                                                                       NoMealChildCoded==1)),1,NA),ChildPortionCoded)),
                   ChildHungry = as.integer(ifelse((LowCostFood==1 & ChildPortion==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                   ChildSkip = as.integer(ifelse(ChildSkipCoded==990,ifelse((LowCostFood==1 & ChildPortion==1 & 
                                                                               (FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,NA),ChildSkipCoded)),
                   FreqChildSkip = as.integer(ifelse(FreqChildSkipCoded==990,ifelse((LowCostFood==1 & ChildPortion==1 & 
                                                                                       ChildSkip==1 & NoMealChildCoded==1),1,NA),FreqChildSkipCoded)),
                   NoMealChild =  as.integer(ifelse(NoMealChildCoded==990,ifelse((LowCostFood==1 & ChildPortion==1 & 
                                                                                    ChildSkip==1 & FreqChildSkip==1),1,NA),NoMealChildCoded)),
                   PrimaryLivelihood = as.integer(ifelse((PrimaryLivelihood>989 & PrimaryLivelihood!=996),NA,PrimaryLivelihood)),
                   SecondaryLivelihood = as.integer(ifelse((SecondaryLivelihood>989 & SecondaryLivelihood!=996),NA,SecondaryLivelihood)),
                   TertiaryLivelihood = as.integer(ifelse((TertiaryLivelihood>989 & TertiaryLivelihood!=996),NA,TertiaryLivelihood)),
                   FreqFish = as.integer(ifelse(FreqFish>989,NA,FreqFish)),
                   FreqSaleFish = as.integer(ifelse(FreqSaleFish >989,NA, FreqSaleFish)),
                   PercentIncFish = as.integer(ifelse(PercentIncomeFish>989, NA, PercentIncomeFish)),
                   MajFishTechnique = as.integer(ifelse(MajorFishTechnique>989,NA,MajorFishTechnique)),
                   FreqEatFish = as.integer(ifelse(FreqEatFish>989,NA,FreqEatFish)),
                   PercentProteinFish = as.integer(ifelse(PercentProteinFish>989,NA,PercentProteinFish)),
                   EconStatusTrend = as.integer(ifelse(EconomicStatusTrend>989,NA,EconomicStatusTrend)),
                   EconStatusReason = ifelse(EconomicStatusReason %in% c("994", "995", "996", "997", "998", "999"), NA,
                                             as.character(EconomicStatusReason)), 
                   Religion= as.integer(ifelse(Religion>989,NA,Religion)),
                   YrResident = as.integer(ifelse(YearsResident>989,NA,YearsResident)),
                   TimeMarket = as.numeric(ifelse(TimeMarket>989,NA,TimeMarket)),
                   SocialConflict = as.integer(ifelse(SocialConflict>989,NA,SocialConflict)), 
                   LessProductiveDaysFishing = LessProductiveDaysFishing, 
                   PoorCatch = PoorCatch, 
                   PoorCatchUnits = PoorCatchUnits, 
                   MoreProductiveDaysFishing = MoreProductiveDaysFishing, 
                   GoodCatch = GoodCatch, 
                   GoodCatchUnits = GoodCatchUnits,
                   PaternalEthnicity = PaternalEthnicity) %>%
  
  mutate(RemoveFS = as.factor(ifelse(rowSums(is.na(.[c("DidNotLast", "BalancedDiet", "FreqAdultSkip", 
                                                              "AdultSkip", "EatLess", "Hungry")]))>3,"Yes","No")),
                RemoveMA = as.factor(ifelse(rowSums(is.na(.[c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", 
                                                              "BoatOutboard", "BoatInboard", "PhoneCombined", 
                                                              "TV", "Entertain", "Satellite", "Generator")]))>10,"Yes","No")),
                RemovePA = as.factor(ifelse(rowSums(is.na(.[c("PlaceHappy", "PlaceFavourite", "PlaceMiss", 
                                                              "PlaceBest", "PlaceFishHere", "PlaceBeMyself")]))>5,"Yes","No")),
                RemoveMT = as.factor(ifelse(rowSums(is.na(.[c("RightsAccess", "RightsHarvest", "RightsManage", 
                                                              "RightsExclude", "RightsTransfer")]))>4,"Yes","No")),
                RemovecFS = as.factor(ifelse(rowSums(is.na(.[c("ChildPortion", "LowCostFood", "ChildSkip", 
                                                               "FreqChildSkip", "NoMealChild")]))>2,"Yes","No"))) %>%
  
  select(HouseholdID, MPAID, SettlementID, InterviewYear, DidNotLast, BalancedDiet, AdultSkip, EatLess, FreqAdultSkip, Hungry, RemoveFS,
                CarTruck, Bicycle, Motorcycle,  BoatNoMotor, BoatOutboard, BoatInboard, PhoneCombined, TV, Entertain, Satellite, Generator, RemoveMA,
                PlaceHappy,  PlaceFavourite, PlaceMiss, PlaceBest, PlaceFishHere, PlaceBeMyself, RemovePA,
                RightsAccess, RightsHarvest, RightsManage, RightsExclude, RightsTransfer, RemoveMT,
                LowCostFood, ChildBalancedMeal, ChildNotEnough, ChildPortion, ChildHungry, ChildSkip, FreqChildSkip, NoMealChild, RemovecFS,
                PrimaryLivelihood, SecondaryLivelihood, TertiaryLivelihood, FreqFish, FreqSaleFish, PercentIncFish, MajFishTechnique, FreqEatFish, PercentProteinFish, 
                EconStatusTrend, EconStatusReason, Religion, YrResident, TimeMarket, SocialConflict,
                LessProductiveDaysFishing, PoorCatch, PoorCatchUnits, MoreProductiveDaysFishing, GoodCatch, GoodCatchUnits, PaternalEthnicity)


# ---- 2.2 Clean & post-code DEMOGRAPHIC to create IndDemos for analysis ----

IndDemos <- 
  DEMOGRAPHIC %>%
  transmute(DemographicID = DemographicID,
                HouseholdID = HouseholdID,
                RelationHHH = RelationHHH,
                IndividualGender = ifelse(IndividualGender==2,0,ifelse(IndividualGender>989,NA,IndividualGender)),
                IndividualAge = ifelse(IndividualAge>989,NA,IndividualAge),
                IndividualEducation = ifelse(IndividualEducation %in% c("995", "997", "998", "999"),
                                             NA,
                                             as.character(IndividualEducation)),
                SchoolAge = ifelse((IndividualAge>4 & IndividualAge<19),1,ifelse(IndividualAge>989 | is.na(IndividualAge),NA,0)),
                ChildEnrolled = ifelse((IndividualEnrolled==1 & SchoolAge==1),1,ifelse((is.na(IndividualAge) | SchoolAge==0),NA,0)),
                DaysUnwell = ifelse(IndividualDaysUnwell>989,NA,IndividualDaysUnwell),
                IndividualUnwell = ifelse(IndividualUnwell>989, NA, IndividualUnwell),
                IndividualLostDays = ifelse(IndividualLostDays>989, NA, IndividualLostDays)) %>%
  left_join(., HHData[,c("HouseholdID","MPAID","SettlementID")], by="HouseholdID")


# ---- 2.3 Call, clean, & post-code ORGANIZATION to create Organization for analysis ----

Organization <- 
  ORGANIZATION %>%
  transmute(HouseholdID = HouseholdID,
                   MarineMeeting = ifelse(!(MarineMeeting==0 | MarineMeeting==1),NA, MarineMeeting),
                   MarineContribution = ifelse(MarineContribution%in%c(994, 995, 996, 997, 998, 999, 0), NA, MarineContribution)) %>%
  left_join(HHData[,c("HouseholdID","MPAID")], ., by="HouseholdID")


# ---- 2.4 Add seascape column to SETTLEMENTS for analysis ----

Settlements <- 
  SETTLEMENT %>%
  mutate(Seascape = ifelse(MPAID %in% c(1,2,3,4,5,6), 1, 
                                  ifelse(MPAID %in% c(15,16,17,18,19,20), 2,
                                         NA))) %>%
  select(.,c("SettlementID","SettlementName","MPAID","Treatment","Seascape"))


# ---- 2.5 Add monitoring year column to HHData for analysis ----

HHData$MonitoringYear <- factor(mapply(a=HHData$MPAID,
                                       b=HHData$InterviewYear,
                                       function(a,b){
                                         define <- 
                                           HHData %>% 
                                           group_by(MPAID) %>% 
                                           summarise(Baseline=min(InterviewYear),
                                                     TwoYear=Baseline+2,
                                                     ThreeYear=Baseline+3,
                                                     FourYear=Baseline+4,
                                                     FiveYear=Baseline+5,
                                                     SixYear=Baseline+6) %>%
                                           na.omit(.)
                                         
                                         mon.year <- ifelse(b==define$Baseline[define$MPAID==a],"Baseline",
                                                            ifelse(b==define$TwoYear[define$MPAID==a],"2 Year Post",
                                                                   ifelse(b==define$ThreeYear[define$MPAID==a],"3 Year Post",
                                                                          ifelse(b==define$FourYear[define$MPAID==a],"4 Year Post",
                                                                                 ifelse(b==define$FiveYear[define$MPAID==a],"5 Year Post",
                                                                                        ifelse(b==define$SixYear[define$MPAID==a],"6 Year Post",NA))))))
                                         mon.year
                                       }),
                                levels=c("Baseline", "2 Year Post", "3 Year Post", "4 Year Post", "5 Year Post", "6 Year Post"),
                                ordered=T)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: ADDRESS PECULIARITIES IN DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Remove observations from BHS that do not have post-baseline data ----

HHData <- HHData[HHData$SettlementID!=84 &
                   HHData$SettlementID!=96 &
                   HHData$SettlementID!=97 &
                   HHData$SettlementID!=98 &
                   HHData$SettlementID!=99 &
                   HHData$SettlementID!=100 &
                   HHData$SettlementID!=101,]

IndDemos <- IndDemos[!is.na(IndDemos$SettlementID) &
                       IndDemos$SettlementID!=84 &
                       IndDemos$SettlementID!=96 &
                       IndDemos$SettlementID!=97 &
                       IndDemos$SettlementID!=98 &
                       IndDemos$SettlementID!=99 &
                       IndDemos$SettlementID!=100 &
                       IndDemos$SettlementID!=101,]

Settlements <- Settlements[!is.na(Settlements$SettlementID) &
                             Settlements$SettlementID!=84 &
                             Settlements$SettlementID!=96 &
                             Settlements$SettlementID!=97 &
                             Settlements$SettlementID!=98 &
                             Settlements$SettlementID!=99 &
                             Settlements$SettlementID!=100 &
                             Settlements$SettlementID!=101,]
Settlements$SettlementName <- as.character(Settlements$SettlementName)


# remove household from baseline that refused every question but material assets (no demographic info, etc.)

HHData <- HHData[HHData$HouseholdID!=1347,]
IndDemos <-IndDemos[IndDemos$HouseholdID!=1347,]

# ---- 3.2 Re-code settlements in Kaimana MPA that changed designation after baseline year ----

Settlements$Treatment <- ifelse(Settlements$SettlementID==83 | Settlements$SettlementID==91 | Settlements$SettlementID==92,
                                0,Settlements$Treatment)


# ---- 3.3. Add dummy row of data for all settlements (in Bird's Head) that do not have baseline data ----


# ---- 3.4 Join Settlements and HHData tables ----

HHData <- 
  left_join(HHData,Settlements,by=c("SettlementID","MPAID"))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: OPTIONAL FILTERING BY SEASCAPE OR MPA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Filter by seascape ----

# adjust filter() function with either "Seascape %in% 1" for Bird's Head, or "Seascape %in% 2" for Sunda Banda

# HHData <-
#   HHData %>%
#   filter(Seascape %in% 1)
# 
# IndDemos <-
#   left_join(IndDemos,HHData[,c("HouseholdID","Seascape")],by=c("HouseholdID"))
#   filter(Seascape %in% 1)
# 
# Organization <-
#   left_join(Organization,HHData[,c("HouseholdID","Seascape")],by="HouseholdID") %>%
#   filter(Seascape %in% 1)


# ---- 4.2 Filter by MPA ----

# adjust filter() function with "MPAID %in%" whatever list of MPAs you'd like to filter by

# HHData <-
#   HHData %>%
#   filter(MPAID %in% c(1,2,3))
# 
# IndDemos <-
#   left_join(IndDemos,HHData[,c("HouseholdID","Seascape")],by=c("HouseholdID")) %>%
#   filter(MPAID %in% c(1,2,3))
# 
# Organization <-
#   left_join(Organization,HHData[,c("HouseholdID","Seascape")],by="HouseholdID") %>%
#   filter(MPAID %in% c(1,2,3))



