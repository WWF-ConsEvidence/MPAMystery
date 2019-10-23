

# ---- 1 Load libraries & data ----
pacman::p_load(plyr,dplyr,ggplot2,xlsx)

Sultra_2017_WELLBEING <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/4_PUSH_MASTER/1_HWB/Sultra_2017_HH_tbl_WELLBEING_CleanedforTesting_2019_0129.csv')
Sultra_2017_DEMOGRAPHIC <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/4_PUSH_MASTER/1_HWB/Sultra_2017_HH_tbl_DEMOGRAPHIC_CleanedforTesting_2019_0129.csv')
Sultra_2017_SETTLEMENT <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/4_PUSH_MASTER/1_HWB/Sultra_2017_HH_tbl_SETTLEMENT.csv')


# ---- 2.1 Call, clean, and collect data to create HHData for SBS analysis ----


HHData<- 
  Sultra_2017_WELLBEING %>%
  dplyr::mutate(DidNotLastCoded = as.integer(ifelse((FSDidNotLast==1 | FSDidNotLast==2),1,ifelse(FSDidNotLast==3,0,990))),
                BalancedDietCoded = as.integer(ifelse((FSBalancedDiet==1 | FSBalancedDiet==2),1,ifelse(FSBalancedDiet==3,0,990))),
                FreqAdultSkipCoded = as.integer(ifelse((FSFreqAdultSkip==1 | FSFreqAdultSkip==2),1,ifelse(FSFreqAdultSkip==3 | FSAdultSkip==0,0,990))),
                AdultSkipCoded = as.integer(ifelse(FSAdultSkip==1,1,ifelse(FSAdultSkip==0,0,990))),
                EatLessCoded = as.integer(ifelse(FSEatLess==1,1, ifelse(FSEatLess==0,0,990))),
                HungryCoded = as.integer(ifelse(FSHungry==1,1,ifelse(FSHungry==0,0,990))),
                DidNotLastClean = as.integer(ifelse(DidNotLastCoded==990,ifelse((BalancedDietCoded==1 | AdultSkipCoded==1 | EatLessCoded==1 | 
                                                                                   FreqAdultSkipCoded==1 | HungryCoded==1),1,NA),DidNotLastCoded)),
                BalancedDietClean = as.integer(ifelse(BalancedDietCoded==990,ifelse((DidNotLastClean==1 & (AdultSkipCoded==1 | EatLessCoded==1 | 
                                                                                                             FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),BalancedDietCoded)),
                AdultSkipClean = as.integer(ifelse(AdultSkipCoded==990,ifelse(FreqAdultSkipCoded==1 | (DidNotLastClean==1 & BalancedDietClean==1 & 
                                                                                 (EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),AdultSkipCoded)),
                EatLessClean = as.integer(ifelse(EatLessCoded==990,ifelse((DidNotLastClean==1 & BalancedDietClean==1 & AdultSkipClean==1 & (FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),EatLessCoded)),
                FreqAdultSkipClean = as.integer(ifelse(FreqAdultSkipCoded==990,ifelse((DidNotLastClean==1 & BalancedDietClean==1 & 
                                                                                         AdultSkipClean==1 & EatLessClean==1 & HungryCoded==1),1,NA),FreqAdultSkipCoded)),
                HungryClean = as.integer(ifelse(HungryCoded==990,ifelse((DidNotLastClean==1 & BalancedDietClean==1 & FreqAdultSkipClean==1 & 
                                                                           AdultSkipClean==1 & EatLessClean==1),1,NA),HungryCoded)),
                CarTruckClean = as.integer(ifelse(AssetCarTruck>989,990,AssetCarTruck*11)),
                BicycleClean = as.integer(ifelse(AssetBicycle>989,990,AssetBicycle*9)),
                MotorcycleClean = as.integer(ifelse(AssetMotorcycle>989,990,AssetMotorcycle*10)),
                BoatNoMotorClean = as.integer(ifelse(AssetBoatNoMotor>989,990,AssetBoatNoMotor*6)),
                BoatOutboardClean = as.integer(ifelse(AssetBoatOutboard>989,990,AssetBoatOutboard*7)),
                BoatInboardClean =  as.integer(ifelse(AssetBoatInboard>989,990,AssetBoatInboard*8)),
                PhoneCombinedClean = as.integer(ifelse(AssetPhoneCombined>989,990,AssetPhoneCombined*4)),
                TVClean = as.integer(ifelse(AssetTV>989,990,AssetTV*2)),
                EntertainClean = as.integer(ifelse(AssetEntertain>989,990,AssetEntertain*1)),
                SatelliteClean = as.integer(ifelse(AssetSatellite>989,990,AssetSatellite*3)),
                GeneratorClean = as.integer(ifelse(AssetGenerator>989,990,AssetGenerator*5)),
                CarTruck = as.integer(ifelse(CarTruckClean==990,NA,CarTruckClean)),
                Bicycle = as.integer(ifelse(BicycleClean==990,NA,BicycleClean)),
                Motorcycle = as.integer(ifelse(MotorcycleClean==990,NA,MotorcycleClean)),
                BoatNoMotor = as.integer(ifelse(BoatNoMotorClean==990,NA,BoatNoMotorClean)),
                BoatOutboard = as.integer(ifelse(BoatOutboardClean==990,NA,BoatOutboardClean)),
                BoatInboard = as.integer(ifelse(BoatInboardClean==990,NA,BoatInboardClean)),
                PhoneCombined = as.integer(ifelse(PhoneCombinedClean==990,NA,PhoneCombinedClean)),
                TV = as.integer(ifelse(TVClean==990,NA,TVClean)),
                Entertain = as.integer(ifelse(EntertainClean==990,NA,EntertainClean)),
                Satellite = as.integer(ifelse(SatelliteClean==990,NA,SatelliteClean)),
                Generator = as.integer(ifelse(GeneratorClean==990,NA,GeneratorClean)),
                PlaceHappyCoded = as.integer(ifelse(PlaceHappy<6,PlaceHappy,990)),
                PlaceFavouriteCoded = as.integer(ifelse(PlaceFavourite<6,PlaceFavourite,990)),
                PlaceMissCoded = as.integer(ifelse(PlaceMiss<6,PlaceMiss,990)),
                PlaceBestCoded = as.integer(ifelse(PlaceBest<6,PlaceBest,990)),
                PlaceFishHereCoded = as.integer(ifelse(PlaceFishHere<6,PlaceFishHere,990)),
                PlaceBeMyselfCoded = as.integer(ifelse(PlaceBeMyself<6,PlaceBeMyself,990)),
                PlaceHappyClean =  as.integer(ifelse(PlaceHappyCoded==990,NA,PlaceHappyCoded)),
                PlaceFavClean = as.integer(ifelse(PlaceFavouriteCoded==990,NA,PlaceFavouriteCoded)),
                PlaceMissClean = as.integer(ifelse(PlaceMissCoded==990,NA,PlaceMissCoded)),
                PlaceBestClean = as.integer(ifelse(PlaceBestCoded==990,NA,PlaceBestCoded)),
                PlaceFishClean = as.integer(ifelse(PlaceFishHereCoded==990,NA,PlaceFishHereCoded)),
                PlaceMyselfClean = as.integer(ifelse(PlaceBeMyselfCoded==990,NA,PlaceBeMyselfCoded)),
                RightsAccessCoded = as.integer(ifelse(RightsAccess>989,990,RightsAccess)),
                RightsHarvestCoded = as.integer(ifelse(RightsHarvest>989,990,RightsHarvest)),
                RightsManageCoded = as.integer(ifelse(RightsManage>989,990,RightsManage)),
                RightsExcludeCoded = as.integer(ifelse(RightsExclude>989,990,RightsExclude)),
                RightsTransferCoded = as.integer(ifelse(RightsTransfer>989,990,RightsTransfer)),
                RightsAccessClean = as.integer(ifelse(RightsAccessCoded==990,NA,RightsAccessCoded)),
                RightsHarvestClean = as.integer(ifelse(RightsHarvestCoded==990,NA,RightsHarvestCoded)),
                RightsManageClean = as.integer(ifelse(RightsManageCoded==990,NA,RightsManageCoded)),
                RightsExcludeClean = as.integer(ifelse(RightsExcludeCoded==990,NA,RightsExcludeCoded)),
                RightsTransferClean = as.integer(ifelse(RightsTransferCoded==990,NA,RightsTransferCoded)),
                ChildPortionCoded = as.integer(ifelse(FSChildPortion==1,1,ifelse(FSChildPortion==0,0,990))),
                LowCostFoodCoded = as.integer(ifelse((FSLowCostFood==1 | FSLowCostFood==2),1,ifelse(FSLowCostFood==3,0,990))),
                ChildSkipCoded =  as.integer(ifelse(FSChildSkip==1,1,ifelse(FSChildSkip==0,0,990))),
                FreqChildSkipCoded = as.integer(ifelse((FSFreqChildSkip==1 | FSFreqChildSkip==2),1,ifelse(FSFreqChildSkip==3,0,990))),
                NoMealChildCoded = as.integer(ifelse((FSNoMealChild==1 | FSNoMealChild==2),1,ifelse(FSNoMealChild==3,0,990))),
                LowCostFoodClean = as.integer(ifelse(LowCostFoodCoded==990,ifelse((ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | 
                                                                                     NoMealChildCoded==1),1,NA),LowCostFoodCoded)),
                ChildBalancedMealClean = as.integer(ifelse((LowCostFoodClean==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | 
                                                                                     NoMealChildCoded==1)),1,0)),
                ChildNotEnoughClean =  as.integer(ifelse((LowCostFoodClean==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | 
                                                                                   FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                ChildPortionClean = as.integer(ifelse(ChildPortionCoded==990,ifelse((LowCostFoodClean==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | 
                                                                                                              NoMealChildCoded==1)),1,NA),ChildPortionCoded)),
                ChildHungryClean = as.integer(ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0)),
                ChildSkipClean = as.integer(ifelse(ChildSkipCoded==990,ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & 
                                                                                 (FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,NA),ChildSkipCoded)),
                FreqChildSkipClean = as.integer(ifelse(FreqChildSkipCoded==990,ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & 
                                                                                         ChildSkipClean==1 & NoMealChildCoded==1),1,NA),FreqChildSkipCoded)),
                NoMealChildClean =  as.integer(ifelse(NoMealChildCoded==990,ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & 
                                                                                      ChildSkipClean==1 & FreqChildSkipClean==1),1,NA),NoMealChildCoded)),
                PrimaryLivelihoodClean = as.integer(ifelse((PrimaryLivelihood>989 & PrimaryLivelihood!=996),NA,PrimaryLivelihood)),
                SecondaryLivelihoodClean = as.integer(ifelse((SecondaryLivelihood>989 & SecondaryLivelihood!=996),NA,SecondaryLivelihood)),
                TertiaryLivelihoodClean = as.integer(ifelse((TertiaryLivelihood>989 & TertiaryLivelihood!=996),NA,TertiaryLivelihood)),
                FreqFishClean = as.integer(ifelse(FreqFish>989,NA,FreqFish)),
                FreqSaleFishClean = as.integer(ifelse(FreqSaleFish >989,NA, FreqSaleFish)),
                PercentIncFishClean = as.integer(ifelse(PercentIncomeFish>989, NA, PercentIncomeFish)),
                MajFishTechniqueClean = as.integer(ifelse(MajorFishTechnique>989,NA,MajorFishTechnique)),
                EconStatusTrendClean = as.integer(ifelse(EconomicStatusTrend>989,NA,EconomicStatusTrend)),
                FreqEatFishClean = as.integer(ifelse(FreqEatFish>989,NA,FreqEatFish)),
                PercentProteinFishClean = as.integer(ifelse(PercentProteinFish>989,NA,PercentProteinFish)),
                TimeMarketClean = as.numeric(ifelse(TimeMarket>989,NA,TimeMarket)),
                ReligionClean= as.integer(ifelse(Religion>989,NA,Religion)),
                YrResidentClean = as.integer(ifelse(YearsResident>989,NA,YearsResident))) %>%
  dplyr::select(HouseholdID, MPAID, SettlementID, InterviewYear, DidNotLastCoded, 
                BalancedDietCoded, FreqAdultSkipCoded, AdultSkipCoded, EatLessCoded, HungryCoded, DidNotLastClean, 
                BalancedDietClean, AdultSkipClean, EatLessClean, FreqAdultSkipClean, HungryClean, CarTruckClean, BicycleClean, MotorcycleClean, 
                BoatNoMotorClean, BoatOutboardClean, BoatInboardClean,  PhoneCombinedClean, TVClean, EntertainClean, SatelliteClean, 
                GeneratorClean, CarTruck, Bicycle, Motorcycle,  BoatNoMotor, BoatOutboard, BoatInboard, PhoneCombined, 
                TV, Entertain, Satellite, Generator, PlaceHappyCoded,  PlaceFavouriteCoded, PlaceMissCoded, PlaceBestCoded, PlaceFishHereCoded, 
                PlaceBeMyselfCoded, PlaceHappyClean, PlaceFavClean,  PlaceMissClean, PlaceBestClean, PlaceFishClean, PlaceMyselfClean, 
                RightsAccessCoded, RightsHarvestCoded, RightsManageCoded, RightsExcludeCoded, RightsTransferCoded, RightsAccessClean, 
                RightsHarvestClean, RightsManageClean, RightsExcludeClean, RightsTransferClean, ChildPortionCoded, LowCostFoodCoded, 
                ChildSkipCoded, FreqChildSkipCoded, NoMealChildCoded, LowCostFoodClean, ChildBalancedMealClean, ChildNotEnoughClean, 
                ChildPortionClean, ChildHungryClean, ChildSkipClean, FreqChildSkipClean, NoMealChildClean, PrimaryLivelihoodClean, SecondaryLivelihoodClean, 
                TertiaryLivelihoodClean, FreqFishClean, FreqSaleFishClean, PercentIncFishClean, MajFishTechniqueClean, EconStatusTrendClean, 
                FreqEatFishClean, PercentProteinFishClean, EconomicStatusReason, TimeMarketClean, ReligionClean, YrResidentClean, 
                EconomicStatusTrend, SocialConflict, LessProductiveDaysFishing, PoorCatch, PoorCatchUnits, MoreProductiveDaysFishing, GoodCatch, GoodCatchUnits)



HHData <- 
  HHData %>%
  dplyr::mutate(EconomicStatusReasonClean = ifelse(HHData$EconomicStatusReason %in% c("994", "995", "996", "997", "998", "999"), NA,
                                                   as.character(HHData$EconomicStatusReason))) %>%
  
  dplyr::mutate(RemoveFS = as.factor(ifelse(rowSums(HHData[c("DidNotLastCoded", "BalancedDietCoded", "FreqAdultSkipCoded", 
                                                             "AdultSkipCoded", "EatLessCoded", "HungryCoded")],na.rm=T)>3959,"Yes","No"))) %>%
  
  dplyr::mutate(RemoveMA = as.factor(ifelse(rowSums(HHData[c("CarTruckClean", "BicycleClean", "MotorcycleClean", "BoatNoMotorClean", 
                                                             "BoatOutboardClean", "BoatInboardClean", "PhoneCombinedClean", 
                                                             "TVClean", "EntertainClean", "SatelliteClean", "GeneratorClean")],na.rm=T)>=10890,"Yes","No"))) %>%
  
  dplyr::mutate(RemovePA = as.factor(ifelse(rowSums(HHData[c("PlaceHappyCoded", "PlaceFavouriteCoded", "PlaceMissCoded", 
                                                             "PlaceBestCoded", "PlaceFishHereCoded", "PlaceBeMyselfCoded")],na.rm=T)>=5940,"Yes","No"))) %>%
  
  dplyr::mutate(RemoveMT = as.factor(ifelse(rowSums(HHData[c("RightsAccessCoded", "RightsHarvestCoded", "RightsManageCoded", 
                                                             "RightsExcludeCoded", "RightsTransferCoded")],na.rm=T)>=4950,"Yes","No"))) %>%
  
  dplyr::mutate(RemovecFS = as.factor(ifelse(rowSums(HHData[c("ChildPortionCoded", "LowCostFoodCoded", "ChildSkipCoded", 
                                                              "FreqChildSkipCoded", "NoMealChildCoded")],na.rm=T)>2969,"Yes","No"))) %>%
  
  dplyr::select(HouseholdID, MPAID, SettlementID, InterviewYear, DidNotLastCoded, 
                BalancedDietCoded, FreqAdultSkipCoded, AdultSkipCoded, 
                EatLessCoded, HungryCoded, RemoveFS, DidNotLastClean, 
                BalancedDietClean, AdultSkipClean, EatLessClean, FreqAdultSkipClean, 
                HungryClean, CarTruckClean, BicycleClean, MotorcycleClean, 
                BoatNoMotorClean, BoatOutboardClean, BoatInboardClean, 
                PhoneCombinedClean, TVClean, EntertainClean, SatelliteClean, 
                GeneratorClean, RemoveMA, CarTruck, Bicycle, Motorcycle, 
                BoatNoMotor, BoatOutboard, BoatInboard, PhoneCombined, 
                TV, Entertain, Satellite, Generator, PlaceHappyCoded, 
                PlaceFavouriteCoded, PlaceMissCoded, PlaceBestCoded, PlaceFishHereCoded, 
                PlaceBeMyselfCoded, RemovePA, PlaceHappyClean, PlaceFavClean, 
                PlaceMissClean, PlaceBestClean, PlaceFishClean, PlaceMyselfClean, 
                RightsAccessCoded, RightsHarvestCoded, RightsManageCoded, 
                RightsExcludeCoded, RightsTransferCoded, RemoveMT, RightsAccessClean, 
                RightsHarvestClean, RightsManageClean, RightsExcludeClean, 
                RightsTransferClean, ChildPortionCoded, LowCostFoodCoded, 
                ChildSkipCoded, FreqChildSkipCoded, NoMealChildCoded, RemovecFS, 
                LowCostFoodClean, ChildBalancedMealClean, ChildNotEnoughClean, 
                ChildPortionClean, ChildHungryClean, ChildSkipClean, FreqChildSkipClean, 
                NoMealChildClean, PrimaryLivelihoodClean, SecondaryLivelihoodClean, 
                TertiaryLivelihoodClean, FreqFishClean, FreqSaleFishClean, 
                PercentIncFishClean, MajFishTechniqueClean, EconStatusTrendClean, 
                FreqEatFishClean, PercentProteinFishClean, EconomicStatusReasonClean, 
                TimeMarketClean, ReligionClean, YrResidentClean, EconomicStatusTrend, 
                SocialConflict, LessProductiveDaysFishing, PoorCatch, PoorCatchUnits, 
                MoreProductiveDaysFishing, GoodCatch, GoodCatchUnits)



# ---- 2.2 Call, clean, and collect data to create IndDemos for SBS analysis ----
IndDemos <- Sultra_2017_DEMOGRAPHIC %>%
  dplyr::mutate(IndividualGenderClean = ifelse(IndividualGender==2,0,ifelse(IndividualGender>989,NA,IndividualGender))) %>%
  dplyr::mutate(IndividualAgeClean = ifelse(IndividualAge>989,NA,IndividualAge)) %>%
  dplyr::mutate(ChildOrAdult = ifelse((IndividualAge>4 & IndividualAge<19),1,ifelse(IndividualAge>989,NA,0))) %>%
  dplyr::mutate(ChildEnrolled = ifelse((IndividualEnrolled==1 & ChildOrAdult==1),1,ifelse((IndividualAge>989 | ChildOrAdult==0),NA,0))) %>%
  dplyr::mutate(DaysUnwellClean = ifelse(IndividualDaysUnwell>989,NA,IndividualDaysUnwell)) %>%
  dplyr::mutate(IndividualUnwellClean = ifelse(IndividualUnwell>989, NA, IndividualUnwell)) %>%
  dplyr::mutate(IndividualLostDaysClean = ifelse(IndividualLostDays>989, NA, IndividualLostDays)) %>%
  dplyr::mutate(IndividualEducationClean = ifelse(IndividualEducation %in% c("995", "997", "998", "999"),
                                                  NA,
                                                  as.character(IndividualEducation))) %>%
  dplyr::select(HouseholdID, IndividualGenderClean, IndividualEducationClean, IndividualAgeClean, ChildOrAdult, ChildEnrolled, DaysUnwellClean, RelationHHH, IndividualUnwellClean, IndividualLostDaysClean) %>%
  collect()


IndDemos <- left_join(HHData[,c("HouseholdID","MPAID")], IndDemos, by="HouseholdID")

# ---- 3.4 Call, clean, and collect data to create Settlement for SBS analysis and filter for SBS MPAs Only ----
Settlements <- Sultra_2017_SETTLEMENT %>%
  dplyr::filter(MPAID %in% 20) %>%
  dplyr::select("SettlementID", "MPAID", "SettlementName", "Treatment")

# ---- 4 Filter datasets for SBS MPAs ONLY ---- 

Organization <- Organization %>%
  dplyr::filter(MPAID%in%c(10:20))

IndDemos <- IndDemos %>%
dplyr::filter(MPAID%in%c(10:20))

HHData <- HHData %>%
  dplyr::filter(MPAID%in%c(10:20))


