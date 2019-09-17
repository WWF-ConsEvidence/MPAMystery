# 
# code:  Source social data from flat files for all social MPA Mystery analysis
# 
# github: WWF-ConsEvidence/MPAMystery/1_Data_wrangling/1_Social/2_Source_data
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: May 2019
# QAQC_modified by: Duong Le & David Gill  
# Date: June 2019
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
LTHREAT <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/',nam='HH_tbl_LTHREAT')
LSTEPS <- last.file(dir.nam='x_Flat_data_files/1_Social/Inputs/Master_database_exports/',nam='HH_tbl_LSTEPS')

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CLEAN & POST-CODE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# -- 2.1 Clean & post-code WELLBEING to create HHData for analysis ----

HHData <-   WELLBEING %>%
  dplyr::transmute(HouseholdID = HouseholdID, 
                   MPAID = MPAID, 
                   SettlementID = SettlementID, 
                   InterviewYear = InterviewYear,
                   
                   # Food Security
                   DidNotLastCoded = as.integer(ifelse((FSDidNotLast==1 | FSDidNotLast==2),1,ifelse(FSDidNotLast==3,0,990))),
                   BalancedDietCoded = as.integer(ifelse((FSBalancedDiet==1 | FSBalancedDiet==2),1,ifelse(FSBalancedDiet==3,0,990))),
                   FreqAdultSkipCoded = as.integer(ifelse((FSFreqAdultSkip==1 | FSFreqAdultSkip==2),1,ifelse(FSFreqAdultSkip==3 | FSAdultSkip==0,0,990))),
                   AdultSkipCoded = as.integer(ifelse(FSAdultSkip==1,1,ifelse(FSAdultSkip==0,0,990))),
                   EatLessCoded = as.integer(ifelse(FSEatLess==1,1, ifelse(FSEatLess==0,0,990))),
                   HungryCoded = as.integer(ifelse(FSHungry==1,1,ifelse(FSHungry==0,0,990))),
                   
                   DidNotLast = as.integer(ifelse(DidNotLastCoded==990,
                                                  ifelse((BalancedDietCoded==1 | AdultSkipCoded==1 | EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1),1,NA),DidNotLastCoded)),
                   BalancedDiet = as.integer(ifelse(BalancedDietCoded==990,
                                                    ifelse((DidNotLast==1 & (AdultSkipCoded==1 | EatLessCoded==1 |FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),BalancedDietCoded)),
                   AdultSkip = as.integer(ifelse(AdultSkipCoded==990,
                                                 ifelse(FreqAdultSkipCoded==1 | (DidNotLast==1 & BalancedDiet==1 & (EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),AdultSkipCoded)),
                   EatLess = as.integer(ifelse(EatLessCoded==990,
                                               ifelse((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & (FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),EatLessCoded)),
                   FreqAdultSkip = as.integer(ifelse(FreqAdultSkipCoded==990,
                                                     ifelse((DidNotLast==1 & BalancedDiet==1 & AdultSkip==1 & EatLess==1 & HungryCoded==1),1,NA),FreqAdultSkipCoded)),
                   Hungry = as.integer(ifelse(HungryCoded==990,
                                              ifelse((DidNotLast==1 & BalancedDiet==1 & FreqAdultSkip==1 & AdultSkip==1 & EatLess==1),1,NA),HungryCoded)),
                   
                   # Assets
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
                   
                   # Place Attachment
                   PlaceHappy = as.integer(ifelse(PlaceHappy%in%c(1:5),PlaceHappy,NA)),
                   PlaceFavourite = as.integer(ifelse(PlaceFavourite%in%c(1:5),PlaceFavourite,NA)),
                   PlaceMiss = as.integer(ifelse(PlaceMiss%in%c(1:5),PlaceMiss,NA)),
                   PlaceBest = as.integer(ifelse(PlaceBest%in%c(1:5),PlaceBest,NA)),
                   PlaceFishHere = as.integer(ifelse(PlaceFishHere%in%c(1:5),PlaceFishHere,NA)),
                   PlaceBeMyself = as.integer(ifelse(PlaceBeMyself%in%c(1:5),PlaceBeMyself,NA)),
                   
                   # Tenure
                   RightsAccess = as.integer(ifelse(RightsAccess%in%c(0:1),RightsAccess,NA)),
                   RightsHarvest = as.integer(ifelse(RightsHarvest%in%c(0:1),RightsHarvest,NA)),
                   RightsManage = as.integer(ifelse(RightsManage%in%c(0:1),RightsManage,NA)),
                   RightsExclude = as.integer(ifelse(RightsExclude%in%c(0:1),RightsExclude,NA)),
                   RightsTransfer = as.integer(ifelse(RightsTransfer%in%c(0:1),RightsTransfer,NA)),
                   
                   
                   # Child's Food Security
                   ChildPortionCoded = as.integer(ifelse(FSChildPortion==1,1,ifelse(FSChildPortion==0,0,990))),
                   LowCostFoodCoded = as.integer(ifelse((FSLowCostFood==1 | FSLowCostFood==2),1,ifelse(FSLowCostFood==3,0,990))),
                   ChildSkipCoded =  as.integer(ifelse(FSChildSkip==1,1,ifelse(FSChildSkip==0,0,990))),
                   FreqChildSkipCoded = as.integer(ifelse((FSFreqChildSkip==1 | FSFreqChildSkip==2),1,ifelse(FSFreqChildSkip==3,0,990))),
                   NoMealChildCoded = as.integer(ifelse((FSNoMealChild==1 | FSNoMealChild==2),1,ifelse(FSNoMealChild==3,0,990))),
                   
                   LowCostFood = as.integer(ifelse(LowCostFoodCoded==990,
                                                  ifelse((ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 |NoMealChildCoded==1),1,NA),LowCostFoodCoded)),
                   ChildBalancedMeal = as.integer(ifelse((LowCostFood==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                   ChildNotEnough =  as.integer(ifelse((LowCostFood==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 |FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                   ChildPortion = as.integer(ifelse(ChildPortionCoded==990,
                                                   ifelse((LowCostFood==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 |NoMealChildCoded==1)),1,NA),ChildPortionCoded)),
                   ChildHungry = as.integer(ifelse((LowCostFood==1 & ChildPortion==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                   ChildSkip = as.integer(ifelse(ChildSkipCoded==990,
                                                ifelse((LowCostFood==1 & ChildPortion==1 & (FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,NA),ChildSkipCoded)),
                   FreqChildSkip = as.integer(ifelse(FreqChildSkipCoded==990,
                                                    ifelse((LowCostFood==1 & ChildPortion==1 & ChildSkip==1 & NoMealChildCoded==1),1,NA),FreqChildSkipCoded)),
                   NoMealChild =  as.integer(ifelse(NoMealChildCoded==990,
                                                   ifelse((LowCostFood==1 & ChildPortion==1 & ChildSkip==1 & FreqChildSkip==1),1,NA),NoMealChildCoded)),
                   
                   
                   # Livelihoods & Occupations
                   PrimaryLivelihood = as.integer(ifelse(PrimaryLivelihood%in%c(1:7,996),PrimaryLivelihood,NA)),
                   SecondaryLivelihood = as.integer(ifelse(SecondaryLivelihood%in%c(1:7,996),SecondaryLivelihood,NA)),
                   TertiaryLivelihood = as.integer(ifelse(TertiaryLivelihood%in%c(1:7,996),TertiaryLivelihood,NA)),
                   
                   
                   # Fishing Characteristics
                   FreqFish = as.integer(ifelse(FreqFish%in%c(1:5),FreqFish,NA)),
                   FreqSaleFish = as.integer(ifelse(FreqSaleFish%in%c(1:5),FreqSaleFish,NA)),
                   PercentIncFish = as.integer(ifelse(PercentIncomeFish%in%c(1:5),PercentIncomeFish,NA)),
                   MajFishTechnique = as.integer(ifelse(MajorFishTechnique%in%c(1:6),MajorFishTechnique,NA)),
                   FreqEatFish = as.integer(ifelse(FreqEatFish%in%c(1:5),FreqEatFish,NA)),
                   PercentProteinFish = as.integer(ifelse(PercentProteinFish%in%c(1:5),PercentProteinFish,NA)),
                   
                   
                   # Economic Well-being
                   EconStatusTrend = as.integer(ifelse(EconomicStatusTrend%in%c(1:5),EconomicStatusTrend,NA)),
                   EconStatusReason = ifelse(EconomicStatusReason %in% c("994", "995", "996", "997", "998", "999"), NA, as.character(EconomicStatusReason)), 
                   
                   
                   # Other Characteristics
                   Religion = as.integer(ifelse(Religion%in%c(1:7),Religion,NA)),
                   YrResident = as.integer(ifelse(YearsResident>=150,NA,YearsResident)),
                   TimeMarket = as.numeric(ifelse(TimeMarket>989,NA,TimeMarket)),
                   SocialConflict = as.integer(ifelse(SocialConflict%in%c(1:5),SocialConflict,NA)),
                   PaternalEthnicity = PaternalEthnicity,
                   
                   #Fishing 
                   LessProductiveDaysFishing = as.integer(ifelse(LessProductiveDaysFishing%in%c(0:366),LessProductiveDaysFishing,NA)),
                   PoorCatch = PoorCatch,
                   PoorCatchUnits = PoorCatchUnits, 
                   
                   MoreProductiveDaysFishing = as.integer(ifelse(MoreProductiveDaysFishing%in%c(0:366),MoreProductiveDaysFishing,NA)),
                   GoodCatch = GoodCatch,
                   GoodCatchUnits = GoodCatchUnits) %>%
  
  
  dplyr::mutate(RemoveFS = as.factor(ifelse(rowSums(.[c("DidNotLastCoded", "BalancedDietCoded", "FreqAdultSkipCoded", 
                                                              "AdultSkipCoded", "EatLessCoded", "HungryCoded")])>2969,"Yes","No")), #2970 would be 3 or more blind codes
                RemoveMA = as.factor(ifelse(rowSums(is.na(.[c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", 
                                                              "BoatOutboard", "BoatInboard", "PhoneCombined", 
                                                              "TV", "Entertain", "Satellite", "Generator")]))>10,"Yes","No")),
                RemovePA = as.factor(ifelse(rowSums(is.na(.[c("PlaceHappy", "PlaceFavourite", "PlaceMiss", 
                                                              "PlaceBest", "PlaceFishHere", "PlaceBeMyself")]))>5,"Yes","No")),
                RemoveMT = as.factor(ifelse(rowSums(is.na(.[c("RightsAccess", "RightsHarvest", "RightsManage", 
                                                              "RightsExclude", "RightsTransfer")]))>4,"Yes","No")),
                RemovecFS = as.factor(ifelse(rowSums(.[c("ChildPortionCoded", "LowCostFoodCoded", "ChildSkipCoded", 
                                                               "FreqChildSkipCoded", "NoMealChildCoded")])>1979,"Yes","No"))) %>% #1980 would vbe 2 or more blind codes
  
  dplyr::select(HouseholdID, MPAID, SettlementID, InterviewYear, DidNotLast, BalancedDiet, AdultSkip, EatLess, FreqAdultSkip, Hungry, RemoveFS,
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
  dplyr::transmute(DemographicID = DemographicID,
                HouseholdID = HouseholdID,
                RelationHHH = as.integer(ifelse(RelationHHH%in%c(0:13),RelationHHH,NA)),
                IndividualGender = ifelse(IndividualGender==2,0,ifelse(IndividualGender>989,NA,IndividualGender)),
                IndividualAge = ifelse(IndividualAge>150,NA,IndividualAge),
                IndividualEducation = ifelse(IndividualEducation %in% c("995", "997", "998", "999"),NA,as.character(IndividualEducation)),
                SchoolAge = ifelse((IndividualAge>4 & IndividualAge<19),1,ifelse(IndividualAge>150 | is.na(IndividualAge),NA,0)),
                IndividualEnrolled = as.integer(ifelse(IndividualEnrolled%in%c(0:1),IndividualEnrolled,NA)),
                ChildEnrolled = ifelse((IndividualEnrolled==1 & SchoolAge==1),1,ifelse((is.na(IndividualAge) | SchoolAge==0),NA,0)),
                DaysUnwell = ifelse(IndividualDaysUnwell>32,NA,
                                    ifelse(IndividualDaysUnwell%in%c(29:32),28,IndividualDaysUnwell)),
                IndividualUnwell = as.integer(ifelse(IndividualUnwell%in%c(0:1),IndividualUnwell,NA)),
                IndividualLostDays = ifelse(IndividualLostDays>32, NA, 
                                            ifelse(IndividualLostDays %in% c(29:32),28,IndividualLostDays))) %>%
  left_join(., HHData[,c("HouseholdID","MPAID","SettlementID")], by="HouseholdID")


# ---- 2.3 Call, clean, & post-code ORGANIZATION to create Organization for analysis ----

Organization <- 
  ORGANIZATION %>%
  dplyr::transmute(HouseholdID = HouseholdID,
                   MarineMeeting = ifelse(MarineMeeting%in%c(0:1),MarineMeeting, NA),
                   MarineContribution = ifelse(MarineContribution%in%c(994, 995, 996, 997, 998, 999, 0), NA, MarineContribution)) %>%
  left_join(HHData[,c("HouseholdID","MPAID")], ., by="HouseholdID")


# ---- 2.4 Add seascape column to SETTLEMENTS for analysis ----

Settlements <- 
  SETTLEMENT %>%
  dplyr::mutate(Seascape = ifelse(MPAID %in% c(1,2,3,4,5,6), 1,  #Seascape 1 is Bird's Head, and Seascape 2 is Sunda Banda
                                  ifelse(MPAID %in% c(15,16,17,18,19,20), 2,
                                         NA))) %>%
  dplyr::select(.,c("SettlementID","SettlementName","MPAID","Treatment","Seascape"))


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

baseline.dummy.rows <- 
  data.frame(HouseholdID=rep(NA,13),
             MPAID=c(5,rep(2,9),rep(3,3)),
             SettlementID=c(72,104:112,113:115),
             InterviewYear=c(2012,rep(2010,9),rep(2012,3)),
             as.data.frame(matrix(rep(NA,length(colnames(HHData[5:length(colnames(HHData))]))),
                                  ncol=length(colnames(HHData[5:length(colnames(HHData))])),
                                  nrow=13,
                                  dimnames=list(NULL,colnames(HHData[5:length(colnames(HHData))]))))) %>%
  mutate(RemoveFS="No",
         RemoveMA="No",
         RemoveMT="No",
         RemovePA="No",
         MonitoringYear="Baseline")


HHData <- 
  rbind.data.frame(HHData,
                   baseline.dummy.rows)


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
#   dplyr::filter(Seascape %in% 2)
# 
# IndDemos <-
#   left_join(IndDemos,HHData[,c("HouseholdID","Seascape")],by=c("HouseholdID"))
#   dplyr::filter(Seascape %in% 2)
# 
# Organization <-
#   left_join(Organization,HHData[,c("HouseholdID","Seascape")],by="HouseholdID") %>%
#   dplyr::filter(Seascape %in% 2)


# ---- 4.2 Filter by MPA ----

# adjust filter() function with "MPAID %in%" whatever list of MPAs you'd like to filter by

# HHData <-
#   HHData %>%
#   dplyr::filter(MPAID %in% c(1,2,3))
# 
# IndDemos <-
#   left_join(IndDemos,HHData[,c("HouseholdID","Seascape")],by=c("HouseholdID")) %>%
#   dplyr::filter(MPAID %in% c(1,2,3))
# 
# Organization <-
#   left_join(Organization,HHData[,c("HouseholdID","Seascape")],by="HouseholdID") %>%
#   dplyr::filter(MPAID %in% c(1,2,3))


rm(baseline.dummy.rows)
