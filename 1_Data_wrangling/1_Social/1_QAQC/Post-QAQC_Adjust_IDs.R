
# ---- 1.1 Load libraries & data ----

pacman::p_load(plyr, dplyr, openxlsx, chron)


Alor_WELLBEING <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_WELLBEING.csv')
Alor_DEMOGRAPHIC <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_DEMOGRAPHIC.csv')
Alor_BIRTHS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_BIRTHS.csv')
Alor_DEATHS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_DEATHS.csv')
Alor_ORGANIZATION <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_ORGANIZATION.csv')
Alor_NMORGANIZATION <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_NMORGANIZATION.csv')
Alor_LTHREAT <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_LTHREAT.csv')
Alor_LSTEPS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_LSTEPS.csv')
Alor_GTHREAT <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_GTHREAT.csv')
Alor_GSTEPS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Selat_Pantar/1_HWB/Post-QAQC_FlatFiles_2019_0519/ALOR_2017_post-QAQC_GSTEPS.csv')

Flotim_WELLBEING <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_WELLBEING.csv')
Flotim_DEMOGRAPHIC <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_DEMOGRAPHIC.csv')
Flotim_BIRTHS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_BIRTHS.csv')
Flotim_DEATHS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_DEATHS.csv')
Flotim_ORGANIZATION <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_ORGANIZATION.csv')
Flotim_NMORGANIZATION <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_NMORGANIZATION.csv')
Flotim_LTHREAT <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_LTHREAT.csv')
Flotim_LSTEPS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_LSTEPS.csv')
Flotim_GTHREAT <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_GTHREAT.csv')
Flotim_GSTEPS <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/Flores_Timur/1_HWB/Post-QAQC_FlatFiles_2019_0519/FLOTIM_2017_post-QAQC_GSTEPS.csv')

# find the maximum primary ID for each table in current version of master database
Max_ID_per_table_master <- data.frame(HouseholdID=9190,
                                      DemographicID=51976,
                                      BirthID=5603,
                                      DeathID=4344,
                                      OrganizationID=5240,
                                      NMOrganizationID=8545,
                                      LThreatID=17274,
                                      LocalStepsID=11092,
                                      GlobalThreatID=13042,
                                      GlobalStepsID=7945)

# ---- 2.1 ALOR: Rename HouseholdID to EntryHouseholdID & add new IDs----

Alor_WELLBEING <- 
  Alor_WELLBEING %>% plyr::rename(c("HouseholdID"="EntryHouseholdID", "KK.Code"="KK Code")) %>%
  .[order(.$SettlementID,.$`KK Code`),] %>%
  dplyr::mutate(HouseholdID=seq(Max_ID_per_table_master$HouseholdID+1,Max_ID_per_table_master$HouseholdID+length(EntryHouseholdID), by=1),
                EntryComputerIdentifier="ALOR_FLOTIM_2017_post-QAQC_2019_0519",
                Baseline_t2_pairs=NA) %>%
  dplyr::select(HouseholdID,CountryID,MPAID,SettlementID,`KK Code`,Respondent,SecondaryRespondent,PrimaryInterviewer,SecondaryInterviewer,FieldCoordinator,
                LatDeg,LatMin,LatSec,LatFrac,LatSphere,LonDeg,LonMin,LonSec,LonFrac,LonSphere,InterviewDay,InterviewMonth,InterviewYear,InterviewStart,InterviewEnd,
                InterviewLength,SurveyVersionNumber,UsualFish,HouseholdSize,YearsResident,TimeMarket,PaternalEthnicity,MaternalEthnicity,Religion,PrimaryLivelihood,
                SecondaryLivelihood,TertiaryLivelihood,FreqFish,FreqSaleFish,PercentIncomeFish,FreqEatFish,PercentProteinFish,MajorFishTechnique,LessProductiveDaysFishing,
                PoorCatch,PoorCatchUnits,PoorFishIncomeL,PoorFishUnits,MoreProductiveDaysFishing,GoodCatch,GoodCatchUnits,GoodFishIncomeL,GoodFishUnits,EconomicStatusTrend,
                EconomicStatusReason,EconomicStatusReasonEnglish,AssetCar,AssetTruck,AssetCarTruck,AssetBicycle,AssetMotorcycle,AssetBoatNoMotor,AssetBoatOutboard,
                AssetBoatInboard,AssetLandlinePhone,AssetCellPhone,AssetPhoneCombined,AssetTV,AssetRadio,AssetStereo,AssetCD,AssetDVD,AssetEntertain,AssetSatellite,AssetGenerator,
                CookingFuel,HouseholdDeath,HouseholdBirth,FSNotEnough,FSDidNotLast,FSBalancedDiet,FSAdultSkip,FSFreqAdultSkip,FSEatLess,FSHungry,FSChildPortion,FSLowCostFood,
                FSChildSkip,FSFreqChildSkip,FSNoMealChild,RightsAccess,RightsHarvest,RightsManage,RightsExclude,RightsTransfer,SocialConflict,MarineGroup,NumberMarineGroup,
                OtherGroup,NumberOtherGroup,VoteDistrict,VoteNational,NumLocalThreat,NumGlobalThreat,NumLocalAction,NumGlobalAction,PlaceHappy,PlaceFavourite,PlaceMiss,
                PlaceBest,PlaceFishHere,PlaceBeMyself,AnyOtherInfo,WillingParticipant,Notes,DataEntryComplete,DataCheckComplete,DataEntryID,DataCheckID,WorstDayCatch,
                WorstDayCatchUnits,BestDayCatch,BestDayCatchUnits,AverageIncomeL,AverageIncomeUnits,WorstIncomeL,WorstIncomeUnits,BestIncomeL,BestIncomeUnits,EntryComputerIdentifier,
                EntryHouseholdID,PilotReferenceCode,InterviewDate,Baseline_t2_pairs)

Alor_DEMOGRAPHIC <- 
  Alor_DEMOGRAPHIC %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID,.$DemographicCode),] %>%
  dplyr::mutate(DemographicID=seq(Max_ID_per_table_master$DemographicID+1,Max_ID_per_table_master$DemographicID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DemographicID,HouseholdID,IndividualName,RelationHHH,IndividualAge,IndividualGender,IndividualEducation,IndividualEdLevel,IndividualEnrolled,HouseholdHead,
                IndividualUnwell,IndividualDaysUnwell,IndividualLostDays)

Alor_BIRTHS <- 
  Alor_BIRTHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID","DateofDeath"="DateOfDeath"))  %>%
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(BirthID=seq(Max_ID_per_table_master$BirthID+1,Max_ID_per_table_master$BirthID+length(EntryHouseholdID), by=1),
                InfantSurvived=ChildSurvived) %>%
  dplyr::select(BirthID,HouseholdID,EntryHouseholdID,NameInfant,InfantSurvived,DateOfDeath)

Alor_DEATHS <- 
  Alor_DEATHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DeathID=seq(Max_ID_per_table_master$DeathID+1,Max_ID_per_table_master$DeathID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DeathID,HouseholdID,EntryHouseholdID,NameDeceased,Gender,AgeAtDeath,DateDeath)

Alor_ORGANIZATION <- 
  Alor_ORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(OrganizationID=seq(Max_ID_per_table_master$OrganizationID+1,Max_ID_per_table_master$OrganizationID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(OrganizationID,HouseholdID,EntryHouseholdID,MarineGroupName,MarinePosition,MarineMeeting,MarineDays,MarineContribution)

Alor_NMORGANIZATION <- 
  Alor_NMORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(NMOrganizationID=seq(Max_ID_per_table_master$NMOrganizationID+1,Max_ID_per_table_master$NMOrganizationID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(NMOrganizationID,HouseholdID,EntryHouseholdID,OtherGroupName,OtherGroupPosition,OtherGroupMeeting,OtherGroupDays,OtherGroupContribution)

Alor_LTHREAT <- 
  Alor_LTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LThreatID=seq(Max_ID_per_table_master$LThreatID+1,Max_ID_per_table_master$LThreatID+length(EntryHouseholdID), by=1),
                LocalThreatID=NA) %>%
  dplyr::select(LThreatID,HouseholdID,EntryHouseholdID,LocalMarineThreat,LocalThreatID)

Alor_LSTEPS <- 
  Alor_LSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LocalStepsID=seq(Max_ID_per_table_master$LocalStepsID+1,Max_ID_per_table_master$LocalStepsID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(LocalStepsID,HouseholdID,EntryHouseholdID,LocalSteps)

Alor_GTHREAT <- 
  Alor_GTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalThreatID=seq(Max_ID_per_table_master$GlobalThreatID+1,Max_ID_per_table_master$GlobalThreatID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalThreatID,HouseholdID,EntryHouseholdID,GLobalMarineThreat)

Alor_GSTEPS <- 
  Alor_GSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%   
  left_join(.,Alor_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalStepsID=seq(Max_ID_per_table_master$GlobalStepsID+1,Max_ID_per_table_master$GlobalStepsID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalStepsID,HouseholdID,EntryHouseholdID,GLobalMarineSteps)


# ---- 2.2 FLOTIM Rename HouseholdID to EntryHouseholdID & add new IDs ----

Flotim_WELLBEING <- 
  Flotim_WELLBEING %>% plyr::rename(c("HouseholdID"="EntryHouseholdID", "KK.Code"="KK Code")) %>%
  .[order(.$SettlementID,.$`KK Code`),] %>%
  dplyr::mutate(HouseholdID=seq(max(Alor_WELLBEING$HouseholdID)+1,max(Alor_WELLBEING$HouseholdID)+length(EntryHouseholdID), by=1),
                EntryComputerIdentifier="ALOR_FLOTIM_2017_post-QAQC_2019_0519",
                Baseline_t2_pairs=NA) %>%
  dplyr::select(HouseholdID,CountryID,MPAID,SettlementID,`KK Code`,Respondent,SecondaryRespondent,PrimaryInterviewer,SecondaryInterviewer,FieldCoordinator,
                LatDeg,LatMin,LatSec,LatFrac,LatSphere,LonDeg,LonMin,LonSec,LonFrac,LonSphere,InterviewDay,InterviewMonth,InterviewYear,InterviewStart,InterviewEnd,
                InterviewLength,SurveyVersionNumber,UsualFish,HouseholdSize,YearsResident,TimeMarket,PaternalEthnicity,MaternalEthnicity,Religion,PrimaryLivelihood,
                SecondaryLivelihood,TertiaryLivelihood,FreqFish,FreqSaleFish,PercentIncomeFish,FreqEatFish,PercentProteinFish,MajorFishTechnique,LessProductiveDaysFishing,
                PoorCatch,PoorCatchUnits,PoorFishIncomeL,PoorFishUnits,MoreProductiveDaysFishing,GoodCatch,GoodCatchUnits,GoodFishIncomeL,GoodFishUnits,EconomicStatusTrend,
                EconomicStatusReason,EconomicStatusReasonEnglish,AssetCar,AssetTruck,AssetCarTruck,AssetBicycle,AssetMotorcycle,AssetBoatNoMotor,AssetBoatOutboard,
                AssetBoatInboard,AssetLandlinePhone,AssetCellPhone,AssetPhoneCombined,AssetTV,AssetRadio,AssetStereo,AssetCD,AssetDVD,AssetEntertain,AssetSatellite,AssetGenerator,
                CookingFuel,HouseholdDeath,HouseholdBirth,FSNotEnough,FSDidNotLast,FSBalancedDiet,FSAdultSkip,FSFreqAdultSkip,FSEatLess,FSHungry,FSChildPortion,FSLowCostFood,
                FSChildSkip,FSFreqChildSkip,FSNoMealChild,RightsAccess,RightsHarvest,RightsManage,RightsExclude,RightsTransfer,SocialConflict,MarineGroup,NumberMarineGroup,
                OtherGroup,NumberOtherGroup,VoteDistrict,VoteNational,NumLocalThreat,NumGlobalThreat,NumLocalAction,NumGlobalAction,PlaceHappy,PlaceFavourite,PlaceMiss,
                PlaceBest,PlaceFishHere,PlaceBeMyself,AnyOtherInfo,WillingParticipant,Notes,DataEntryComplete,DataCheckComplete,DataEntryID,DataCheckID,WorstDayCatch,
                WorstDayCatchUnits,BestDayCatch,BestDayCatchUnits,AverageIncomeL,AverageIncomeUnits,WorstIncomeL,WorstIncomeUnits,BestIncomeL,BestIncomeUnits,EntryComputerIdentifier,
                EntryHouseholdID,PilotReferenceCode,InterviewDate,Baseline_t2_pairs)

Flotim_DEMOGRAPHIC <- 
  Flotim_DEMOGRAPHIC %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID,.$DemographicCode),] %>%
  dplyr::mutate(DemographicID=seq(max(Alor_DEMOGRAPHIC$DemographicID)+1,max(Alor_DEMOGRAPHIC$DemographicID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DemographicID,HouseholdID,IndividualName,RelationHHH,IndividualAge,IndividualGender,IndividualEducation,IndividualEdLevel,IndividualEnrolled,HouseholdHead,
                IndividualUnwell,IndividualDaysUnwell,IndividualLostDays)

Flotim_BIRTHS <- 
  Flotim_BIRTHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID","DateofDeath"="DateOfDeath"))  %>%
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(BirthID=seq(max(Alor_BIRTHS$BirthID)+1,max(Alor_BIRTHS$BirthID)+length(EntryHouseholdID), by=1),
                InfantSurvived=ChildSurvived) %>%
  dplyr::select(BirthID,HouseholdID,EntryHouseholdID,NameInfant,InfantSurvived,DateOfDeath)

Flotim_DEATHS <- 
  Flotim_DEATHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DeathID=seq(max(Alor_DEATHS$DeathID)+1,max(Alor_DEATHS$DeathID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DeathID,HouseholdID,EntryHouseholdID,NameDeceased,Gender,AgeAtDeath,DateDeath)

Flotim_ORGANIZATION <- 
  Flotim_ORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(OrganizationID=seq(max(Alor_ORGANIZATION$OrganizationID)+1,max(Alor_ORGANIZATION$OrganizationID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(OrganizationID,HouseholdID,EntryHouseholdID,MarineGroupName,MarinePosition,MarineMeeting,MarineDays,MarineContribution)

Flotim_NMORGANIZATION <- 
  Flotim_NMORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(NMOrganizationID=seq(max(Alor_NMORGANIZATION$NMOrganizationID)+1,max(Alor_NMORGANIZATION$NMOrganizationID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(NMOrganizationID,HouseholdID,EntryHouseholdID,OtherGroupName,OtherGroupPosition,OtherGroupMeeting,OtherGroupDays,OtherGroupContribution)

Flotim_LTHREAT <- 
  Flotim_LTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LThreatID=seq(max(Alor_LTHREAT$LThreatID)+1,max(Alor_LTHREAT$LThreatID)+length(EntryHouseholdID), by=1),
                LocalThreatID=NA) %>%
  dplyr::select(LThreatID,HouseholdID,EntryHouseholdID,LocalMarineThreat,LocalThreatID)

Flotim_LSTEPS <- 
  Flotim_LSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LocalStepsID=seq(max(Alor_LSTEPS$LocalStepsID)+1,max(Alor_LSTEPS$LocalStepsID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(LocalStepsID,HouseholdID,EntryHouseholdID,LocalSteps)

Flotim_GTHREAT <- 
  Flotim_GTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalThreatID=seq(max(Alor_GTHREAT$GlobalThreatID)+1,max(Alor_GTHREAT$GlobalThreatID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalThreatID,HouseholdID,EntryHouseholdID,GLobalMarineThreat)

Flotim_GSTEPS <- 
  Flotim_GSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Flotim_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalStepsID=seq(max(Alor_GSTEPS$GlobalStepsID)+1,max(Alor_GSTEPS$GlobalStepsID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalStepsID,HouseholdID,EntryHouseholdID,GLobalMarineSteps)



# ---- 3.1 Write to xlsx for final data type check before pasting to master database ----

wb <- createWorkbook("Alor_Flotim_2017") 
addWorksheet(wb,"HH_tbl_WELLBEING")
addWorksheet(wb,"HH_tbl_DEMOGRAPHIC")
addWorksheet(wb,"HH_tbl_BIRTHS")
addWorksheet(wb,"HH_tbl_DEATHS")
addWorksheet(wb,"HH_tbl_ORGANIZATION")
addWorksheet(wb,"HH_tbl_NMORGANIZATION")
addWorksheet(wb,"HH_tbl_LTHREAT")
addWorksheet(wb,"HH_tbl_LSTEPS")
addWorksheet(wb,"HH_tbl_GTHREAT")
addWorksheet(wb,"HH_tbl_GSTEPS")


writeData(wb,"HH_tbl_WELLBEING",rbind.data.frame(Alor_WELLBEING,Flotim_WELLBEING))
writeData(wb,"HH_tbl_DEMOGRAPHIC",rbind.data.frame(Alor_DEMOGRAPHIC,Flotim_DEMOGRAPHIC))
writeData(wb,"HH_tbl_BIRTHS",rbind.data.frame(Alor_BIRTHS,Flotim_BIRTHS))
writeData(wb,"HH_tbl_DEATHS",rbind.data.frame(Alor_DEATHS,Flotim_DEATHS))
writeData(wb,"HH_tbl_ORGANIZATION",rbind.data.frame(Alor_ORGANIZATION,Flotim_ORGANIZATION))
writeData(wb,"HH_tbl_NMORGANIZATION",rbind.data.frame(Alor_NMORGANIZATION,Flotim_NMORGANIZATION))
writeData(wb,"HH_tbl_LTHREAT",rbind.data.frame(Alor_LTHREAT,Flotim_LTHREAT))
writeData(wb,"HH_tbl_LSTEPS",rbind.data.frame(Alor_LSTEPS,Flotim_LSTEPS))
writeData(wb,"HH_tbl_GTHREAT",rbind.data.frame(Alor_GTHREAT,Flotim_GTHREAT))
writeData(wb,"HH_tbl_GSTEPS",rbind.data.frame(Alor_GSTEPS,Flotim_GSTEPS))


saveWorkbook(wb,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/4_PUSH_MASTER/1_HWB/ALOR_FLOTIM_2017_post-QAQC_2019_0527.xlsx')
