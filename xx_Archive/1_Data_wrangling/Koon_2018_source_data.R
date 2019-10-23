

# ---- 1 Load libraries & data ----
pacman::p_load(plyr,dplyr,ggplot2,xlsx)

Koon_2018_WELLBEING <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/SEA_REPORT_ANALYSIS_DATA/KOON_2018_WELLBEING_mid-QAQC_forSEAanalysis_2019_0417.csv',
                                na.strings="#N/A")
Koon_2018_DEMOGRAPHIC <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/SEA_REPORT_ANALYSIS_DATA/KOON_2018_DEMOGRAPHIC_mid-QAQC_forSEAanalysis_2019_0417.csv',
                                  na.strings="#N/A")
SETTLEMENT <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/4_PUSH_MASTER/1_HWB/Sultra_2017_HH_tbl_SETTLEMENT.csv')


# ---- 2.1 Call, clean, and collect data to create HHData for SBS analysis ----

HHData_Koon_2018 <- 
  Koon_2018_WELLBEING %>%
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



HHData_Koon_2018$MonitoringYear <- "3 Year Post"

HHData_Koon_2018 <- 
  left_join(HHData_Koon_2018,Settlements,by=c("SettlementID","MPAID"))




# ---- 2.2 Clean & post-code DEMOGRAPHIC to create IndDemos for analysis ----

IndDemos_Koon_2018 <- 
  Koon_2018_DEMOGRAPHIC %>%
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




