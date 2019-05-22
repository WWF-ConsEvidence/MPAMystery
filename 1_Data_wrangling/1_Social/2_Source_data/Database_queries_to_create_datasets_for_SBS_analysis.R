# ---- 1 Load libraries ----
pacman::p_load(dplyr, dbplyr, RPostgreSQL)


# ---- 2 Create connection with database ----

# if connected wwinternal.org etheret use:
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="", password="",
                host="WWFUS-ArcgisDev", port=5432, dbname="mpasocial")

# if NOT connected wwinternal.org etheret use:
# con = dbConnect(pg, user="", password="",
#                 host="pandamaps.wwfus.org", port=5432, dbname="mpasocial")



# ---- 3.1 Call, clean, and collect data to create HHData for SBS analysis ----


HHData<- tbl(con, "HH_tbl_WELLBEING") %>%
  dplyr::mutate(DidNotLastCoded = as.integer(ifelse((FSDidNotLast==1 | FSDidNotLast==2),1,ifelse(FSDidNotLast==3,0,990)))) %>%
  dplyr::mutate(BalancedDietCoded = as.integer(ifelse((FSBalancedDiet==1 | FSBalancedDiet==2),1,ifelse(FSBalancedDiet==3,0,990)))) %>%
  dplyr::mutate(FreqAdultSkipCoded = as.integer(ifelse((FSFreqAdultSkip==1 | FSFreqAdultSkip==2),1,ifelse(FSFreqAdultSkip==3 | FSAdultSkip==0,0,990)))) %>%
  dplyr::mutate(AdultSkipCoded = as.integer(ifelse(FSAdultSkip==1,1,ifelse(FSAdultSkip==0,0,990)))) %>%
  dplyr::mutate(EatLessCoded = as.integer(ifelse(FSEatLess==1,1, ifelse(FSEatLess==0,0,990)))) %>%
  dplyr::mutate(HungryCoded = as.integer(ifelse(FSHungry==1,1,ifelse(FSHungry==0,0,990)))) %>%
  dplyr::mutate(DidNotLastClean = as.integer(ifelse(DidNotLastCoded==990,ifelse((BalancedDietCoded==1 | AdultSkipCoded==1 | EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1),1,NA),DidNotLastCoded))) %>%
  dplyr::mutate(BalancedDietClean = as.integer(ifelse(BalancedDietCoded==990,ifelse((DidNotLastClean==1 & (AdultSkipCoded==1 | EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),BalancedDietCoded))) %>%
  dplyr::mutate(AdultSkipClean = as.integer(ifelse(AdultSkipCoded==990,ifelse(FreqAdultSkipCoded==1 | (DidNotLastClean==1 & BalancedDietClean==1 & (EatLessCoded==1 | FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),AdultSkipCoded))) %>%
  dplyr::mutate(EatLessClean = as.integer(ifelse(EatLessCoded==990,ifelse((DidNotLastClean==1 & BalancedDietClean==1 & AdultSkipClean==1 & (FreqAdultSkipCoded==1 | HungryCoded==1)),1,NA),EatLessCoded))) %>%
  dplyr::mutate(FreqAdultSkipClean = as.integer(ifelse(FreqAdultSkipCoded==990,ifelse((DidNotLastClean==1 & BalancedDietClean==1 & AdultSkipClean==1 & EatLessClean==1 & HungryCoded==1),1,NA),FreqAdultSkipCoded))) %>%
  dplyr::mutate(HungryClean = as.integer(ifelse(HungryCoded==990,ifelse((DidNotLastClean==1 & BalancedDietClean==1 & FreqAdultSkipClean==1 & AdultSkipClean==1 & EatLessClean==1),1,NA),HungryCoded))) %>%
  dplyr::mutate(CarTruckClean = as.integer(ifelse(AssetCarTruck>989,990,AssetCarTruck*11))) %>%
  dplyr::mutate(BicycleClean = as.integer(ifelse(AssetBicycle>989,990,AssetBicycle*9))) %>%
  dplyr::mutate(MotorcycleClean = as.integer(ifelse(AssetMotorcycle>989,990,AssetMotorcycle*10))) %>%
  dplyr::mutate(BoatNoMotorClean = as.integer(ifelse(AssetBoatNoMotor>989,990,AssetBoatNoMotor*6))) %>%
  dplyr::mutate(BoatOutboardClean = as.integer(ifelse(AssetBoatOutboard>989,990,AssetBoatOutboard*7))) %>%
  dplyr::mutate(BoatInboardClean =  as.integer(ifelse(AssetBoatInboard>989,990,AssetBoatInboard*8))) %>%
  dplyr::mutate(PhoneCombinedClean = as.integer(ifelse(AssetPhoneCombined>989,990,AssetPhoneCombined*4))) %>%
  dplyr::mutate(TVClean = as.integer(ifelse(AssetTV>989,990,AssetTV*2))) %>%
  dplyr::mutate(EntertainClean = as.integer(ifelse(AssetEntertain>989,990,AssetEntertain*1))) %>%
  dplyr::mutate(SatelliteClean = as.integer(ifelse(AssetSatellite>989,990,AssetSatellite*3))) %>%
  dplyr::mutate(GeneratorClean = as.integer(ifelse(AssetGenerator>989,990,AssetGenerator*5))) %>%
  dplyr::mutate(CarTruck = as.integer(ifelse(CarTruckClean==990,NA,CarTruckClean))) %>%
  dplyr::mutate(Bicycle = as.integer(ifelse(BicycleClean==990,NA,BicycleClean))) %>%
  dplyr::mutate(Motorcycle = as.integer(ifelse(MotorcycleClean==990,NA,MotorcycleClean))) %>%
  dplyr::mutate(BoatNoMotor = as.integer(ifelse(BoatNoMotorClean==990,NA,BoatNoMotorClean))) %>%
  dplyr::mutate(BoatOutboard = as.integer(ifelse(BoatOutboardClean==990,NA,BoatOutboardClean))) %>%
  dplyr::mutate(BoatInboard = as.integer(ifelse(BoatInboardClean==990,NA,BoatInboardClean))) %>%
  dplyr::mutate(PhoneCombined = as.integer(ifelse(PhoneCombinedClean==990,NA,PhoneCombinedClean))) %>%
  dplyr::mutate(TV = as.integer(ifelse(TVClean==990,NA,TVClean))) %>%
  dplyr::mutate(Entertain = as.integer(ifelse(EntertainClean==990,NA,EntertainClean))) %>%
  dplyr::mutate(Satellite = as.integer(ifelse(SatelliteClean==990,NA,SatelliteClean))) %>%
  dplyr::mutate(Generator = as.integer(ifelse(GeneratorClean==990,NA,GeneratorClean))) %>%
  dplyr::mutate(PlaceHappyCoded = as.integer(ifelse(PlaceHappy<6,PlaceHappy,990))) %>%
  dplyr::mutate(PlaceFavouriteCoded = as.integer(ifelse(PlaceFavourite<6,PlaceFavourite,990))) %>%
  dplyr::mutate(PlaceMissCoded = as.integer(ifelse(PlaceMiss<6,PlaceMiss,990))) %>%
  dplyr::mutate(PlaceBestCoded = as.integer(ifelse(PlaceBest<6,PlaceBest,990))) %>%
  dplyr::mutate(PlaceFishHereCoded = as.integer(ifelse(PlaceFishHere<6,PlaceFishHere,990))) %>%
  dplyr::mutate(PlaceBeMyselfCoded = as.integer(ifelse(PlaceBeMyself<6,PlaceBeMyself,990))) %>%
  dplyr::mutate(PlaceHappyClean =  as.integer(ifelse(PlaceHappyCoded==990,NA,PlaceHappyCoded))) %>%
  dplyr::mutate(PlaceFavClean = as.integer(ifelse(PlaceFavouriteCoded==990,NA,PlaceFavouriteCoded))) %>%
  dplyr::mutate(PlaceMissClean = as.integer(ifelse(PlaceMissCoded==990,NA,PlaceMissCoded))) %>%
  dplyr::mutate(PlaceBestClean = as.integer(ifelse(PlaceBestCoded==990,NA,PlaceBestCoded))) %>%
  dplyr::mutate(PlaceFishClean = as.integer(ifelse(PlaceFishHereCoded==990,NA,PlaceFishHereCoded))) %>%
  dplyr::mutate(PlaceMyselfClean = as.integer(ifelse(PlaceBeMyselfCoded==990,NA,PlaceBeMyselfCoded))) %>%
  dplyr::mutate(RightsAccessCoded = as.integer(ifelse(RightsAccess>989,990,RightsAccess))) %>%
  dplyr::mutate(RightsHarvestCoded = as.integer(ifelse(RightsHarvest>989,990,RightsHarvest))) %>%
  dplyr::mutate(RightsManageCoded = as.integer(ifelse(RightsManage>989,990,RightsManage))) %>%
  dplyr::mutate(RightsExcludeCoded = as.integer(ifelse(RightsExclude>989,990,RightsExclude))) %>%
  dplyr::mutate(RightsTransferCoded = as.integer(ifelse(RightsTransfer>989,990,RightsTransfer))) %>%
  dplyr::mutate(RightsAccessClean = as.integer(ifelse(RightsAccessCoded==990,NA,RightsAccessCoded))) %>%
  dplyr::mutate(RightsHarvestClean = as.integer(ifelse(RightsHarvestCoded==990,NA,RightsHarvestCoded))) %>%
  dplyr::mutate(RightsManageClean = as.integer(ifelse(RightsManageCoded==990,NA,RightsManageCoded))) %>%
  dplyr::mutate(RightsExcludeClean = as.integer(ifelse(RightsExcludeCoded==990,NA,RightsExcludeCoded))) %>%
  dplyr::mutate(RightsTransferClean = as.integer(ifelse(RightsTransferCoded==990,NA,RightsTransferCoded))) %>%
  dplyr::mutate(ChildPortionCoded = as.integer(ifelse(FSChildPortion==1,1,ifelse(FSChildPortion==0,0,990)))) %>%
  dplyr::mutate(LowCostFoodCoded = as.integer(ifelse((FSLowCostFood==1 | FSLowCostFood==2),1,ifelse(FSLowCostFood==3,0,990)))) %>%
  dplyr::mutate(ChildSkipCoded =  as.integer(ifelse(FSChildSkip==1,1,ifelse(FSChildSkip==0,0,990)))) %>%
  dplyr::mutate(FreqChildSkipCoded = as.integer(ifelse((FSFreqChildSkip==1 | FSFreqChildSkip==2),1,ifelse(FSFreqChildSkip==3,0,990)))) %>%
  dplyr::mutate(NoMealChildCoded = as.integer(ifelse((FSNoMealChild==1 | FSNoMealChild==2),1,ifelse(FSNoMealChild==3,0,990)))) %>%
  dplyr::mutate(LowCostFoodClean = as.integer(ifelse(LowCostFoodCoded==990,ifelse((ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1),1,NA),LowCostFoodCoded))) %>%
  dplyr::mutate(ChildBalancedMealClean = as.integer(ifelse((LowCostFoodClean==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0))) %>%
  dplyr::mutate(ChildNotEnoughClean =  as.integer(ifelse((LowCostFoodClean==1 & (ChildPortionCoded==1 | ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0))) %>%
  dplyr::mutate(ChildPortionClean = as.integer(ifelse(ChildPortionCoded==990,ifelse((LowCostFoodClean==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,NA),ChildPortionCoded))) %>%
  dplyr::mutate(ChildHungryClean = as.integer(ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & (ChildSkipCoded==1 | FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,0))) %>%
  dplyr::mutate(ChildSkipClean = as.integer(ifelse(ChildSkipCoded==990,ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & (FreqChildSkipCoded==1 | NoMealChildCoded==1)),1,NA),ChildSkipCoded))) %>%
  dplyr::mutate(FreqChildSkipClean = as.integer(ifelse(FreqChildSkipCoded==990,ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & ChildSkipClean==1 & NoMealChildCoded==1),1,NA),FreqChildSkipCoded))) %>%
  dplyr::mutate(NoMealChildClean =  as.integer(ifelse(NoMealChildCoded==990,ifelse((LowCostFoodClean==1 & ChildPortionClean==1 & ChildSkipClean==1 & FreqChildSkipClean==1),1,NA),NoMealChildCoded))) %>%
  dplyr::mutate(PrimaryLivelihoodClean = as.integer(ifelse((PrimaryLivelihood>989 & PrimaryLivelihood!=996),NA,PrimaryLivelihood))) %>%
  dplyr::mutate(SecondaryLivelihoodClean = as.integer(ifelse((SecondaryLivelihood>989 & SecondaryLivelihood!=996),NA,SecondaryLivelihood))) %>%
  dplyr::mutate(TertiaryLivelihoodClean = as.integer(ifelse((TertiaryLivelihood>989 & TertiaryLivelihood!=996),NA,TertiaryLivelihood))) %>%
  dplyr::mutate(FreqFishClean = as.integer(ifelse(FreqFish>989,NA,FreqFish))) %>%
  dplyr::mutate(FreqSaleFishClean = as.integer(ifelse(FreqSaleFish >989,NA, FreqSaleFish))) %>%
  dplyr::mutate(PercentIncFishClean = as.integer(ifelse(PercentIncomeFish>989, NA, PercentIncomeFish))) %>%
  dplyr::mutate(MajFishTechniqueClean = as.integer(ifelse(MajorFishTechnique>989,NA,MajorFishTechnique))) %>%
  dplyr::mutate(EconStatusTrendClean = as.integer(ifelse(EconomicStatusTrend>989,NA,EconomicStatusTrend))) %>%
  dplyr::mutate(FreqEatFishClean = as.integer(ifelse(FreqEatFish>989,NA,FreqEatFish))) %>%
  dplyr::mutate(PercentProteinFishClean = as.integer(ifelse(PercentProteinFish>989,NA,PercentProteinFish))) %>%
  dplyr::mutate(TimeMarketClean = as.numeric(ifelse(TimeMarket>989,NA,TimeMarket))) %>%
  dplyr::mutate(ReligionClean= as.integer(ifelse(Religion>989,NA,Religion))) %>%
  dplyr::mutate(YrResidentClean = as.integer(ifelse(YearsResident>989,NA,YearsResident))) %>%
  dplyr::mutate(HHDeathClean = as.integer(ifelse(HouseholdDeath>989,NA,HouseholdDeath))) %>%
  dplyr::mutate(HHBirthClean = as.integer(ifelse(HouseholdBirth>989,NA,HouseholdBirth))) %>%
  dplyr::mutate(PaternalEthnicityClean = ifelse(PaternalEthnicity%in%c("994", "995", "996", "997", "998", "999"), NA, 
                                                as.character(PaternalEthnicity))) %>%
  dplyr::mutate(NumLocalThreatClean = ifelse(NumLocalThreat>989, NA, NumLocalThreat)) %>%
  dplyr::mutate(MarineGroupClean = ifelse(!(MarineGroup==0 | MarineGroup==1),NA, MarineGroup)) %>%
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
                FreqEatFishClean, PercentProteinFishClean, EconomicStatusReason, TimeMarketClean, ReligionClean, YrResidentClean, HHDeathClean, 
                HHBirthClean, MarineGroupClean, OtherGroup, EconomicStatusTrend, SocialConflict, PaternalEthnicityClean, NumLocalThreatClean) %>%
  collect()



HHData <- 
  HHData %>%
  dplyr::mutate(EconomicStatusReasonClean = ifelse(HHData$EconomicStatusReason%in%c("994", "995", "996", "997", "998", "999") ,NA,
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
                TimeMarketClean, ReligionClean, YrResidentClean, HHDeathClean, 
                HHBirthClean, MarineGroupClean, OtherGroup, EconomicStatusTrend, 
                SocialConflict, PaternalEthnicityClean, NumLocalThreatClean)




# ---- 3.2 Call, clean, and collect data to create IndDemos for SBS analysis ----
IndDemos <- tbl(con, "HH_tbl_DEMOGRAPHIC") %>%
  dplyr::mutate(IndividualGenderClean = ifelse(IndividualGender==2,0,ifelse(IndividualGender>989,NA,IndividualGender))) %>%
  dplyr::mutate(IndividualAgeClean = ifelse(IndividualAge>989,NA,IndividualAge)) %>%
  dplyr::mutate(ChildOrAdult = ifelse((IndividualAge>4 & IndividualAge<19),1,ifelse(IndividualAge>989,NA,0))) %>%
  dplyr::mutate(ChildEnrolled = ifelse((IndividualEnrolled==1 & ChildOrAdult==1),1,ifelse((IndividualAge>989 | ChildOrAdult==0),NA,0))) %>%
  dplyr::mutate(DaysUnwellClean = ifelse(IndividualDaysUnwell>989,NA,IndividualDaysUnwell)) %>%
  dplyr::mutate(IndividualUnwellClean = ifelse(IndividualUnwell>989, NA, IndividualUnwell)) %>%
  dplyr::mutate(IndividualLostDaysClean = ifelse(IndividualLostDays>989, NA, IndividualLostDays)) %>%
  dplyr::mutate(IndividualEducationClean = ifelse(IndividualEducation%in%c("995", "997", "998", "999"),
                                                  NA,
                                                  as.character(IndividualEducation))) %>%
  dplyr::select(HouseholdID, IndividualGenderClean, IndividualEducationClean, IndividualAgeClean, ChildOrAdult, ChildEnrolled, DaysUnwellClean, RelationHHH, IndividualUnwellClean, IndividualLostDaysClean) %>%
  collect()


IndDemos <- left_join(HHData[,c("HouseholdID","MPAID")], IndDemos, by="HouseholdID")

Organization<- tbl(con, "HH_tbl_ORGANIZATION") %>%
  dplyr::mutate(MarineMeetingClean = ifelse(!(MarineMeeting==0 | MarineMeeting==1),NA, MarineMeeting)) %>%
  dplyr::mutate(MarineContributionClean = ifelse(MarineContribution%in%c(994, 995, 996, 997, 998, 999, 0), NA, MarineContribution)) %>%
  dplyr::select(HouseholdID, MarineMeetingClean, MarineContributionClean) %>%
  collect()

# ---- 3.3 Call, clean, and collect data to create Organization for SBS analysis ----

Organization <- left_join(HHData[,c("HouseholdID","MPAID")], Organization, by="HouseholdID")

# ---- 3.4 Call, clean, and collect data to create Settlement for SBS analysis and filter for SBS MPAs Only ----
Settlements <- tbl(con,"HH_tbl_SETTLEMENT") %>%
  dplyr::filter(MPAID%in%c(10:20)) %>%
  dplyr::select("SettlementID", "MPAID", "SettlementName", "Treatment") %>%
  collect()

# ---- 4 Filter datasets for SBS MPAs ONLY ---- 

Organization <- Organization %>%
  dplyr::filter(MPAID%in%c(10:20))

IndDemos <- IndDemos %>%
dplyr::filter(MPAID%in%c(10:20))

HHData <- HHData %>%
  dplyr::filter(MPAID%in%c(10:20))


