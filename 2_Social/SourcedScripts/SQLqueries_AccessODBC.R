# SQL queries for established ODBC with social MPA Mystery database

# Call HHData
HHData <- sqlQuery(MPAMysteryDB,
'select 
HouseholdID, MPAID, SettlementID, InterviewYear, 
iif((FSDidNotLast=1 or FSDidNotLast=2),1,iif(FSDidNotLast=3,0,990)) as DidNotLastCoded, 
iif((FSBalancedDiet=1 or FSBalancedDiet=2),1,iif(FSBalancedDiet=3,0,990)) as BalancedDietCoded, 
iif((FSFreqAdultSkip=1 or FSFreqAdultSkip=2),1,iif(FSFreqAdultSkip=3,0,990)) as FreqAdultSkipCoded, 
iif(FSAdultSkip=1,1,iif(FSAdultSkip=0,0,990)) as AdultSkipCoded, 
iif(FSEatLess=1,1,iif(FSEatLess=0,0,990)) as EatLessCoded, 
iif(FSHungry=1,1,iif(FSHungry=0,0,990)) as HungryCoded, 
iif(DidNotLastCoded=990,iif((BalancedDietCoded=1 or AdultSkipCoded=1 or EatLessCoded=1 or FreqAdultSkipCoded=1 or HungryCoded=1),1,Null),DidNotLastCoded) as DidNotLastClean, 
iif(BalancedDietCoded=990,iif((DidNotLastClean=1 and (AdultSkipCoded=1 or EatLessCoded=1 or FreqAdultSkipCoded=1 or HungryCoded=1)),1,Null),BalancedDietCoded) as BalancedDietClean, 
iif(AdultSkipCoded=990,iif((DidNotLastClean=1 and BalancedDietClean=1 and (EatLessCoded=1 or FreqAdultSkipCoded=1 or HungryCoded=1)),1,Null),AdultSkipCoded) as AdultSkipClean, 
iif(EatLessCoded=990,iif((DidNotLastClean=1 and BalancedDietClean=1 and AdultSkipClean=1 and (FreqAdultSkipCoded=1 or HungryCoded=1)),1,Null),EatLessCoded) as EatLessClean, 
iif(FreqAdultSkipCoded=990,iif((DidNotLastClean=1 and BalancedDietClean=1 and AdultSkipClean=1 and EatLessClean=1 and HungryCoded=1),1,Null),FreqAdultSkipCoded) as FreqAdultSkipClean, 
iif(HungryCoded=990,iif((DidNotLastClean=1 and BalancedDietClean=1 and FreqAdultSkipClean=1 and AdultSkipClean=1 and EatLessClean=1),1,Null),HungryCoded) as HungryClean, 
iif(AssetCarTruck>989 or AssetCarTruck is Null,
  iif((AssetCar>989 or AssetCar is Null) and (AssetTruck>989 or AssetTruck is Null), 
      990, 
      (AssetCar+AssetTruck)*11), 
  AssetCarTruck*11) as CarTruckClean 
iif(AssetBicycle>989,990,AssetBicycle*9) as BicycleClean, 
iif(AssetMotorcycle>989,990,AssetMotorcycle*10) as MotorcycleClean, 
iif(AssetBoatNoMotor>989,990,AssetBoatNoMotor*6) as BoatNoMotorClean, 
iif(AssetBoatOutboard>989,990,AssetBoatOutboard*7) as BoatOutboardClean, 
iif(AssetBoatInboard>989,990,AssetBoatInboard*8) as BoatInboardClean, 
iif(AssetPhoneCombined>989 Or AssetPhoneCombined is Null,
  iif((AssetLandlinePhone>989 or AssetLandlinePhone is Null) and (AssetCellPhone>989 or AssetCellPhone is Null), 
      990, 
      (AssetLandlinePhone+AssetCellPhone)*4), 
  AssetPhoneCombined*4) as PhoneCombinedClean, 
iif(AssetTV>989,990,AssetTV*2) as TVClean, 
iif(AssetEntertain>989 or AssetEntertain is Null,
  iif((AssetRadio>989 or AssetRadio is Null) and (AssetStereo>989 or AssetStereo is Null) and (AssetCD>989 or AssetCD is Null) and (AssetDVD>989 or AssetDVD is Null), 
      990, 
      (AssetRadio+AssetStereo+AssetCD+AssetDVD)*1), 
  AssetEntertain*1) as EntertainClean, 
iif(AssetSatellite>989,990,AssetSatellite*3) as SatelliteClean, 
iif(AssetGenerator>989,990,AssetGenerator*5) as GeneratorClean, 
iif(CarTruckClean=990,Null,CarTruckClean) as CarTruck, 
iif(BicycleClean=990,Null,BicycleClean) as Bicycle, 
iif(MotorcycleClean=990,Null,MotorcycleClean) as Motorcycle, 
iif(BoatNoMotorClean=990,Null,BoatNoMotorClean) as BoatNoMotor, 
iif(BoatOutboardClean=990,Null,BoatOutboardClean) as BoatOutboard, 
iif(BoatInboardClean=990,Null,BoatInboardClean) as BoatInboard, 
iif(PhoneCombinedClean=990,Null,PhoneCombinedClean) as PhoneCombined, 
iif(TVClean=990,Null,TVClean) as TV, 
iif(EntertainClean=990,Null,EntertainClean) as Entertain, 
iif(SatelliteClean=990,Null,SatelliteClean) as Satellite, 
iif(GeneratorClean=990,Null,GeneratorClean) as Generator, 
iif(PlaceHappy<6,PlaceHappy,990) as PlaceHappyCoded, 
iif(PlaceFavourite<6,PlaceFavourite,990) as PlaceFavouriteCoded, 
iif(PlaceMiss<6,PlaceMiss,990) as PlaceMissCoded, 
iif(PlaceBest<6,PlaceBest,990) as PlaceBestCoded, 
iif(PlaceFishHere<6,PlaceFishHere,990) as PlaceFishHereCoded, 
iif(PlaceBeMyself<6,PlaceBeMyself,990) as PlaceBeMyselfCoded, 
iif(PlaceHappyCoded=990,Null,PlaceHappyCoded) as PlaceHappyClean, 
iif(PlaceFavouriteCoded=990,Null,PlaceFavouriteCoded) as PlaceFavClean, 
iif(PlaceMissCoded=990,Null,PlaceMissCoded) as PlaceMissClean, 
iif(PlaceBestCoded=990,Null,PlaceBestCoded) as PlaceBestClean, 
iif(PlaceFishHereCoded=990,Null,PlaceFishHereCoded) as PlaceFishClean, 
iif(PlaceBeMyselfCoded=990,Null,PlaceBeMyselfCoded) as PlaceMyselfClean, 
iif(RightsAccess>989,990,RightsAccess) as RightsAccessCoded, 
iif(RightsHarvest>989,990,RightsHarvest) as RightsHarvestCoded, 
iif(RightsManage>989,990,RightsManage) as RightsManageCoded, 
iif(RightsExclude>989,990,RightsExclude) as RightsExcludeCoded, 
iif(RightsTransfer>989,990,RightsTransfer) as RightsTransferCoded, 
iif(RightsAccessCoded=990,Null,RightsAccessCoded) as RightsAccessClean, 
iif(RightsHarvestCoded=990,Null,RightsHarvestCoded) as RightsHarvestClean, 
iif(RightsManageCoded=990,Null,RightsManageCoded) as RightsManageClean, 
iif(RightsExcludeCoded=990,Null,RightsExcludeCoded) as RightsExcludeClean, 
iif(RightsTransferCoded=990,Null,RightsTransferCoded) as RightsTransferClean, 
iif(FSChildPortion=1,1,iif(FSChildPortion=0,0,990)) as ChildPortionCoded, 
iif((FSLowCostFood=1 or FSLowCostFood=2),1,iif(FSLowCostFood=3,0,990)) as LowCostFoodCoded, 
iif(FSChildSkip=1,1,iif(FSChildSkip=0,0,990)) as ChildSkipCoded, 
iif((FSFreqChildSkip=1 or FSFreqChildSkip=2),1,iif(FSFreqChildSkip=3,0,990)) as FreqChildSkipCoded, 
iif((FSNoMealChild=1 or FSNoMealChild=2),1,iif(FSNoMealChild=3,0,990)) as NoMealChildCoded, 
iif(LowCostFoodCoded=990,iif((ChildPortionCoded=1 or ChildSkipCoded=1 or FreqChildSkipCoded=1 or NoMealChildCoded=1),1,Null),LowCostFoodCoded) as LowCostFoodClean, 
iif((LowCostFoodClean=1 and (ChildPortionCoded=1 or ChildSkipCoded=1 or FreqChildSkipCoded=1 or NoMealChildCoded=1)),1,0) as ChildBalancedMealClean, 
iif((LowCostFoodClean=1 and (ChildPortionCoded=1 or ChildSkipCoded=1 or FreqChildSkipCoded=1 or NoMealChildCoded=1)),1,0) as ChildNotEnoughClean, 
iif(ChildPortionCoded=990,iif((LowCostFoodClean=1 and (ChildSkipCoded=1 or FreqChildSkipCoded=1 or NoMealChildCoded=1)),1,Null),ChildPortionCoded) as ChildPortionClean, 
iif((LowCostFoodClean=1 and ChildPortionClean=1 and (ChildSkipCoded=1 or FreqChildSkipCoded=1 or NoMealChildCoded=1)),1,0) as ChildHungryClean, 
iif(ChildSkipCoded=990,iif((LowCostFoodClean=1 and ChildPortionClean=1 and (FreqChildSkipCoded=1 or NoMealChildCoded=1)),1,Null),ChildSkipCoded) as ChildSkipClean, 
iif(FreqChildSkipCoded=990,iif((LowCostFoodClean=1 and ChildPortionClean=1 and ChildSkipClean=1 and NoMealChildCoded=1),1,Null),FreqChildSkipCoded) as FreqChildSkipClean, 
iif(NoMealChildCoded=990,iif((LowCostFoodClean=1 and ChildPortionClean=1 and ChildSkipClean=1 and FreqChildSkipClean=1),1,Null),NoMealChildCoded) as NoMealChildClean, 
iif((PrimaryLivelihood>989 and PrimaryLivelihood<>996),Null,PrimaryLivelihood) as PrimaryLivelihoodClean, 
iif((SecondaryLivelihood>989 and SecondaryLivelihood<>996),Null,SecondaryLivelihood) as SecondaryLivelihoodClean, 
iif((TertiaryLivelihood>989 and TertiaryLivelihood<>996),Null,TertiaryLivelihood) as TertiaryLivelihoodClean, 
iif(FreqFish>989,Null,FreqFish) as FreqFishClean, 
iif((((MPAID=3 or MPAID=4 or MPAID=5 or MPAID=6) and (FreqSaleFish>989 and FreqSaleFish<>994)) or 
     ((MPAID=1 or MPAID=2) and (FreqSaleFish>989 and (FreqSaleFish<>998 or FreqSaleFish<>994)))),Null,
        iif(FreqSaleFish=994,1,iif((FreqSaleFish=998 and (MPAID=1 or MPAID=2)),1,FreqSaleFish))) as FreqSaleFishClean, 
iif((((MPAID=3 or MPAID=4 or MPAID=5 or MPAID=6) and (PercentIncomeFish>989 and PercentIncomeFish<>994)) or 
     ((MPAID=1 or MPAID=2) and (PercentIncomeFish>989 and (PercentIncomeFish<>998 or PercentIncomeFish<>994)))),Null,
        iif(PercentIncomeFish=994,1,iif((PercentIncomeFish=998 and (MPAID=1 or MPAID=2)),1,PercentIncomeFish))) as PercentIncFishClean, 
iif(MajorFishTechnique>989,Null,MajorFishTechnique) as MajFishTechniqueClean, 
iif(EconomicStatusTrend>989,Null,EconomicStatusTrend) as EconStatusTrendClean, 
iif(FreqEatFish>989,Null,FreqEatFish) as FreqEatFishClean, 
iif((((MPAID=3 or MPAID=4 or MPAID=5 or MPAID=6) and (PercentProteinFish>989 and PercentProteinFish<>994)) or 
     ((MPAID=1 or MPAID=2) and (PercentProteinFish>989 and (PercentProteinFish<>998 or PercentProteinFish<>994)))),Null,
        iif(PercentProteinFish=994,1,iif((PercentProteinFish=998 and (MPAID=1 or MPAID=2)),1,PercentProteinFish))) as PercentProteinFishClean, 
EconomicStatusReason,
iif(TimeMarket>989,Null,TimeMarket) as TimeMarketClean, 
iif(Religion>989,Null,Religion) as ReligionClean, 
iif(YearsResident>989,Null,YearsResident) as YrResidentClean, 
iif(HouseholdDeath>989,Null,HouseholdDeath) as HHDeathClean, 
iif(HouseholdBirth>989,Null,HouseholdBirth) as HHBirthClean,
MarineGroup, OtherGroup, EconomicStatusTrend, SocialConflict
from HH_tbl_WELLBEING')

HHData$EconomicStatusReason <- ifelse(HHData$EconomicStatusReason=="994" |
                                              HHData$EconomicStatusReason=="995" |
                                              HHData$EconomicStatusReason=="996" |
                                              HHData$EconomicStatusReason=="997" |
                                              HHData$EconomicStatusReason=="998" |
                                              HHData$EconomicStatusReason=="999",NA,
                                            as.character(HHData$EconomicStatusReason))

HHData$RemoveFS <- ifelse(rowSums(HHData[,5:10],na.rm=T)>3959,"Yes","No")
HHData$RemoveMA <- ifelse(rowSums(HHData[,17:27],na.rm=T)>=10890,"Yes","No")
HHData$RemovePA <- ifelse(rowSums(HHData[,39:44],na.rm=T)>=5940,"Yes","No")
HHData$RemoveMT <- ifelse(rowSums(HHData[,51:55],na.rm=T)>=4950,"Yes","No")
HHData$RemovecFS <- ifelse(rowSums(HHData[,61:65],na.rm=T)>2969,"Yes","No")

HHData <- HHData[,c(1:10,94,11:27,95,28:44,96,45:55,97,56:65,98,66:93)]


# Call IndDemos
IndDemos <- sqlQuery(MPAMysteryDB,
'select HouseholdID, 
iif(IndividualGender=2,0,iif(IndividualGender>989,Null,IndividualGender)) as IndividualGenderClean, 
IndividualEducation,
iif(IndividualAge>989,Null,[IndividualAge]) as IndividualAgeClean, 
iif((IndividualAge>4 and IndividualAge<19),1,iif(IndividualAge>989,Null,0)) as ChildOrAdult, 
iif((IndividualEnrolled=1 and ChildOrAdult=1),1,iif((IndividualAge>989 or ChildOrAdult=0),Null,0)) as ChildEnrolled, 
iif(IndividualDaysUnwell>989,Null,IndividualDaysUnwell) as DaysUnwellClean, 
RelationHHH
from HH_tbl_DEMOGRAPHIC')
                     
IndDemos$IndividualEducationClean <- ifelse(IndDemos$IndividualEducation=="994" |
                                              IndDemos$IndividualEducation=="995" |
                                              IndDemos$IndividualEducation=="996" |
                                              IndDemos$IndividualEducation=="997" |
                                              IndDemos$IndividualEducation=="998" |
                                              IndDemos$IndividualEducation=="999",NA,
                                            as.character(IndDemos$IndividualEducation))

IndDemos <- left_join(IndDemos,HHData[,c("HouseholdID","MPAID")],by="HouseholdID")


IndDemos <- IndDemos[IndDemos$MPAID==1 | IndDemos$MPAID==2 | IndDemos$MPAID==3 | 
                       IndDemos$MPAID==4 | IndDemos$MPAID==5 | IndDemos$MPAID==6,
                     c(1:2,9,4:8)]
HHData <- HHData[HHData$MPAID==1 | HHData$MPAID==2 | HHData$MPAID==3 | 
                   HHData$MPAID==4 | HHData$MPAID==5 | HHData$MPAID==6,]


IndDemos <- left_join(IndDemos,HHData[,c("HouseholdID","SettlementID")],by="HouseholdID")

# Call Settlements
Settlements <- sqlFetch(MPAMysteryDB,"HH_tbl_SETTLEMENT")
Settlements <- Settlements[Settlements$MPAID==1 | Settlements$MPAID==2 | Settlements$MPAID==3 | 
                             Settlements$MPAID==4 | Settlements$MPAID==5 | Settlements$MPAID==6,
                           c(1,3:5)]
