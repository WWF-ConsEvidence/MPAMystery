
# ---- 1.1 Load libraries & data ----

pacman::p_load(rio, plyr, foreach, dplyr, openxlsx, chron)

# - SULTRA
Sultra_WELLBEING <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_WELLBEING")  %>%
  filter(!is.na(HouseholdID))
Sultra_DEMOGRAPHIC <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_DEMOGRAPHIC")  %>%
  filter(!is.na(HouseholdID))
Sultra_BIRTHS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_BIRTHS")  %>%
  filter(!is.na(HouseholdID))
Sultra_DEATHS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_DEATHS")  %>%
  filter(!is.na(HouseholdID))
Sultra_ORGANIZATION <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_ORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Sultra_NMORGANIZATION <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_NMORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Sultra_LTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_LTHREAT")  %>%
  filter(!is.na(HouseholdID))
Sultra_LSTEPS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_LSTEPS")  %>%
  filter(!is.na(HouseholdID))
Sultra_GTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_GTHREAT")  %>%
  filter(!is.na(HouseholdID))
Sultra_GSTEPS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/SULTRA_2017_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_GSTEPS")  %>%
  filter(!is.na(HouseholdID))

# - KOON
Koon_WELLBEING <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_WELLBEING")  %>%
  filter(!is.na(HouseholdID))
Koon_DEMOGRAPHIC <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_DEMOGRAPHIC")  %>%
  filter(!is.na(HouseholdID))
Koon_BIRTHS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_BIRTHS")  %>%
  filter(!is.na(HouseholdID))
Koon_DEATHS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_DEATHS")  %>%
  filter(!is.na(HouseholdID))
Koon_ORGANIZATION <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_ORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Koon_NMORGANIZATION <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_NMORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Koon_LTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_LTHREAT") %>% 
  filter(!is.na(HouseholdID))
Koon_LSTEPS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_LSTEPS") %>% 
  filter(!is.na(HouseholdID))
Koon_GTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_GTHREAT") %>% 
  filter(!is.na(HouseholdID))
Koon_GSTEPS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_GSTEPS") %>% 
  filter(!is.na(HouseholdID))

# - KEI
Kei_WELLBEING <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_WELLBEING")  %>%
  filter(!is.na(HouseholdID))
Kei_DEMOGRAPHIC <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_DEMOGRAPHIC")  %>%
  filter(!is.na(HouseholdID))
Kei_BIRTHS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_BIRTHS")  %>%
  filter(!is.na(HouseholdID))
Kei_DEATHS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_DEATHS")  %>%
  filter(!is.na(HouseholdID))
Kei_ORGANIZATION <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_ORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Kei_NMORGANIZATION <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_NMORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Kei_LTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_LTHREAT") %>% 
  filter(!is.na(HouseholdID))
Kei_LSTEPS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_LSTEPS") %>% 
  filter(!is.na(HouseholdID))
Kei_GTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_GTHREAT") %>% 
  filter(!is.na(HouseholdID))
Kei_GSTEPS <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
         sheet="HH_tbl_GSTEPS") %>% 
  filter(!is.na(HouseholdID))

# find the maximum primary ID for each table in current version of master database
Max_ID_per_table_master <- data.frame(HouseholdID=9941,
                                      DemographicID=55451,
                                      BirthID=5636,
                                      DeathID=4352,
                                      OrganizationID=5313,
                                      NMOrganizationID=8771,
                                      LThreatID=17821,
                                      LocalStepsID=11472,
                                      GlobalThreatID=13248,
                                      GlobalStepsID=8072)

# ---- 2.1 SULTRA: Rename HouseholdID to EntryHouseholdID & add new IDs----

Sultra_WELLBEING <- 
  Sultra_WELLBEING %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  .[order(.$SettlementID,.$`KK Code`),] %>%
  dplyr::mutate(HouseholdID=seq(Max_ID_per_table_master$HouseholdID+1,Max_ID_per_table_master$HouseholdID+length(EntryHouseholdID), by=1),
                EntryComputerIdentifier="SULTRA_2017_post-QAQC_2019_1111",
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

Sultra_DEMOGRAPHIC <- 
  Sultra_DEMOGRAPHIC %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID,.$DemographicCode),] %>%
  dplyr::mutate(DemographicID=seq(Max_ID_per_table_master$DemographicID+1,Max_ID_per_table_master$DemographicID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DemographicID,HouseholdID,IndividualName,RelationHHH,IndividualAge,IndividualGender,IndividualEducation,IndividualEdLevel,IndividualEnrolled,HouseholdHead,
                IndividualUnwell,IndividualDaysUnwell,IndividualLostDays,EntryHouseholdID,DemographicCode)

Sultra_BIRTHS <- 
  Sultra_BIRTHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID","DateofDeath"="DateOfDeath"))  %>%
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(BirthID=seq(Max_ID_per_table_master$BirthID+1,Max_ID_per_table_master$BirthID+length(EntryHouseholdID), by=1),
                InfantSurvived=ChildSurvived) %>%
  dplyr::select(BirthID,HouseholdID,EntryHouseholdID,NameInfant,InfantSurvived,DateOfDeath)

Sultra_DEATHS <- 
  Sultra_DEATHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DeathID=seq(Max_ID_per_table_master$DeathID+1,Max_ID_per_table_master$DeathID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DeathID,HouseholdID,EntryHouseholdID,NameDeceased,Gender,AgeAtDeath,DateDeath)

Sultra_ORGANIZATION <- 
  Sultra_ORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(OrganizationID=seq(Max_ID_per_table_master$OrganizationID+1,Max_ID_per_table_master$OrganizationID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(OrganizationID,HouseholdID,EntryHouseholdID,MarineGroupName,MarinePosition,MarineMeeting,MarineDays,MarineContribution)

Sultra_NMORGANIZATION <- 
  Sultra_NMORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(NMOrganizationID=seq(Max_ID_per_table_master$NMOrganizationID+1,Max_ID_per_table_master$NMOrganizationID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(NMOrganizationID,HouseholdID,EntryHouseholdID,OtherGroupName,OtherGroupPosition,OtherGroupMeeting,OtherGroupDays,OtherGroupContribution)

Sultra_LTHREAT <- 
  Sultra_LTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LThreatID=seq(Max_ID_per_table_master$LThreatID+1,Max_ID_per_table_master$LThreatID+length(EntryHouseholdID), by=1),
                LocalThreatID=NA) %>%
  dplyr::select(LThreatID,HouseholdID,EntryHouseholdID,LocalMarineThreat,LocalThreatID)

Sultra_LSTEPS <- 
  Sultra_LSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LocalStepsID=seq(Max_ID_per_table_master$LocalStepsID+1,Max_ID_per_table_master$LocalStepsID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(LocalStepsID,HouseholdID,EntryHouseholdID,LocalSteps)

Sultra_GTHREAT <- 
  Sultra_GTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalThreatID=seq(Max_ID_per_table_master$GlobalThreatID+1,Max_ID_per_table_master$GlobalThreatID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalThreatID,HouseholdID,EntryHouseholdID,GLobalMarineThreat)

Sultra_GSTEPS <- 
  Sultra_GSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%   
  left_join(.,Sultra_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalStepsID=seq(Max_ID_per_table_master$GlobalStepsID+1,Max_ID_per_table_master$GlobalStepsID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalStepsID,HouseholdID,EntryHouseholdID,GLobalMarineSteps)


# ---- 2.2 KOON Rename HouseholdID to EntryHouseholdID & add new IDs ----

Koon_WELLBEING <- 
  Koon_WELLBEING %>% plyr::rename(c("HouseholdID"="EntryHouseholdID","KKCode"="KK Code")) %>%
  .[order(.$SettlementID,.$`KK Code`),] %>%
  dplyr::mutate(HouseholdID=seq(max(Sultra_WELLBEING$HouseholdID)+1,max(Sultra_WELLBEING$HouseholdID)+length(EntryHouseholdID), by=1),
                EntryComputerIdentifier="KOON_2018_post-QAQC_2019_1111",
                Baseline_t2_pairs=NA,
                DataEntryComplete=T,
                DataCheckComplete=T,
                DataEntryID=SecondaryInterviewer,
                DataCheckID=NA,
                WorstDayCatch=NA,
                WorstDayCatchUnits=NA,
                BestDayCatch=NA,
                BestDayCatchUnits=NA,
                AverageIncomeL=NA,
                AverageIncomeUnits=NA,
                WorstIncomeL=NA,
                WorstIncomeUnits=NA,
                BestIncomeL=NA,
                BestIncomeUnits=NA,
                PilotReferenceCode=NA) %>%
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

Koon_DEMOGRAPHIC <- 
  Koon_DEMOGRAPHIC %>% 
  plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID,.$DemographicID),] %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(DemographicCode=1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(DemographicID=seq(max(Sultra_DEMOGRAPHIC$DemographicID)+1,max(Sultra_DEMOGRAPHIC$DemographicID)+length(EntryHouseholdID), by=1),
                IndividualEdLevel=NA,
                HouseholdHead=NA) %>%
  dplyr::select(DemographicID,HouseholdID,IndividualName,RelationHHH,IndividualAge,IndividualGender,IndividualEducation,IndividualEdLevel,IndividualEnrolled,HouseholdHead,
                IndividualUnwell,IndividualDaysUnwell,IndividualLostDays,EntryHouseholdID,DemographicCode)

Koon_BIRTHS <- 
  Koon_BIRTHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID"))  %>%
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(BirthID=seq(max(Sultra_BIRTHS$BirthID)+1,max(Sultra_BIRTHS$BirthID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(BirthID,HouseholdID,EntryHouseholdID,NameInfant,InfantSurvived,DateOfDeath)

Koon_DEATHS <- 
  Koon_DEATHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DeathID=seq(max(Sultra_DEATHS$DeathID)+1,max(Sultra_DEATHS$DeathID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DeathID,HouseholdID,EntryHouseholdID,NameDeceased,Gender,AgeAtDeath,DateDeath)

Koon_ORGANIZATION <- 
  Koon_ORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(OrganizationID=seq(max(Sultra_ORGANIZATION$OrganizationID)+1,max(Sultra_ORGANIZATION$OrganizationID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(OrganizationID,HouseholdID,EntryHouseholdID,MarineGroupName,MarinePosition,MarineMeeting,MarineDays,MarineContribution)

Koon_NMORGANIZATION <- 
  Koon_NMORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(NMOrganizationID=seq(max(Sultra_NMORGANIZATION$NMOrganizationID)+1,max(Sultra_NMORGANIZATION$NMOrganizationID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(NMOrganizationID,HouseholdID,EntryHouseholdID,OtherGroupName,OtherGroupPosition,OtherGroupMeeting,OtherGroupDays,OtherGroupContribution)

Koon_LTHREAT <- 
  Koon_LTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LThreatID=seq(max(Sultra_LTHREAT$LThreatID)+1,max(Sultra_LTHREAT$LThreatID)+length(EntryHouseholdID), by=1),
                LocalThreatID=NA) %>%
  dplyr::select(LThreatID,HouseholdID,EntryHouseholdID,LocalMarineThreat,LocalThreatID)

Koon_LSTEPS <- 
  Koon_LSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LocalStepsID=seq(max(Sultra_LSTEPS$LocalStepsID)+1,max(Sultra_LSTEPS$LocalStepsID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(LocalStepsID,HouseholdID,EntryHouseholdID,LocalSteps)

Koon_GTHREAT <- 
  Koon_GTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalThreatID=seq(max(Sultra_GTHREAT$GlobalThreatID)+1,max(Sultra_GTHREAT$GlobalThreatID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalThreatID,HouseholdID,EntryHouseholdID,GLobalMarineThreat)

Koon_GSTEPS <- 
  Koon_GSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalStepsID=seq(max(Sultra_GSTEPS$GlobalStepsID)+1,max(Sultra_GSTEPS$GlobalStepsID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalStepsID,HouseholdID,EntryHouseholdID,GLobalMarineSteps)


# ---- 2.3 KEI Rename HouseholdID to EntryHouseholdID & add new IDs ----

Kei_WELLBEING <- 
  Kei_WELLBEING %>% plyr::rename(c("HouseholdID"="EntryHouseholdID","KKCode"="KK Code")) %>%
  .[order(.$SettlementID,.$`KK Code`),] %>%
  dplyr::mutate(HouseholdID=seq(max(Koon_WELLBEING$HouseholdID)+1,max(Koon_WELLBEING$HouseholdID)+length(EntryHouseholdID), by=1),
                EntryComputerIdentifier="KEI_2019_post-QAQC_2019_1111",
                Baseline_t2_pairs=NA,
                DataEntryComplete=T,
                DataCheckComplete=T,
                DataEntryID=SecondaryInterviewer,
                DataCheckID=NA,
                WorstDayCatch=NA,
                WorstDayCatchUnits=NA,
                BestDayCatch=NA,
                BestDayCatchUnits=NA,
                AverageIncomeL=NA,
                AverageIncomeUnits=NA,
                WorstIncomeL=NA,
                WorstIncomeUnits=NA,
                BestIncomeL=NA,
                BestIncomeUnits=NA,
                PilotReferenceCode=NA) %>%
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

Kei_DEMOGRAPHIC <- 
  Kei_DEMOGRAPHIC %>% 
  plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Koon_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID,.$DemographicID),] %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::mutate(DemographicCode=1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(DemographicID=seq(max(Koon_DEMOGRAPHIC$DemographicID)+1,max(Koon_DEMOGRAPHIC$DemographicID)+length(EntryHouseholdID), by=1),
                IndividualEdLevel=NA,
                HouseholdHead=NA) %>%
  dplyr::select(DemographicID,HouseholdID,IndividualName,RelationHHH,IndividualAge,IndividualGender,IndividualEducation,IndividualEdLevel,IndividualEnrolled,HouseholdHead,
                IndividualUnwell,IndividualDaysUnwell,IndividualLostDays,EntryHouseholdID,DemographicCode)

Kei_BIRTHS <- 
  Kei_BIRTHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID"))  %>%
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(BirthID=seq(max(Koon_BIRTHS$BirthID)+1,max(Koon_BIRTHS$BirthID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(BirthID,HouseholdID,EntryHouseholdID,NameInfant,InfantSurvived,DateOfDeath)

Kei_DEATHS <- 
  Kei_DEATHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DeathID=seq(max(Koon_DEATHS$DeathID)+1,max(Koon_DEATHS$DeathID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(DeathID,HouseholdID,EntryHouseholdID,NameDeceased,Gender,AgeAtDeath,DateDeath)

Kei_ORGANIZATION <- 
  Kei_ORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(OrganizationID=seq(max(Koon_ORGANIZATION$OrganizationID)+1,max(Koon_ORGANIZATION$OrganizationID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(OrganizationID,HouseholdID,EntryHouseholdID,MarineGroupName,MarinePosition,MarineMeeting,MarineDays,MarineContribution)

Kei_NMORGANIZATION <- 
  Kei_NMORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(NMOrganizationID=seq(max(Koon_NMORGANIZATION$NMOrganizationID)+1,max(Koon_NMORGANIZATION$NMOrganizationID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(NMOrganizationID,HouseholdID,EntryHouseholdID,OtherGroupName,OtherGroupPosition,OtherGroupMeeting,OtherGroupDays,OtherGroupContribution)

Kei_LTHREAT <- 
  Kei_LTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LThreatID=seq(max(Koon_LTHREAT$LThreatID)+1,max(Koon_LTHREAT$LThreatID)+length(EntryHouseholdID), by=1),
                LocalThreatID=NA) %>%
  dplyr::select(LThreatID,HouseholdID,EntryHouseholdID,LocalMarineThreat,LocalThreatID)

Kei_LSTEPS <- 
  Kei_LSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LocalStepsID=seq(max(Koon_LSTEPS$LocalStepsID)+1,max(Koon_LSTEPS$LocalStepsID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(LocalStepsID,HouseholdID,EntryHouseholdID,LocalSteps)

Kei_GTHREAT <- 
  Kei_GTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalThreatID=seq(max(Koon_GTHREAT$GlobalThreatID)+1,max(Koon_GTHREAT$GlobalThreatID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalThreatID,HouseholdID,EntryHouseholdID,GLobalMarineThreat)

Kei_GSTEPS <- 
  Kei_GSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Kei_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalStepsID=seq(max(Koon_GSTEPS$GlobalStepsID)+1,max(Koon_GSTEPS$GlobalStepsID)+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalStepsID,HouseholdID,EntryHouseholdID,GLobalMarineSteps)


# ---- 3.1 Write to xlsx for final data type check before pasting to master database ----

wb <- createWorkbook("Sultra_Koon_Kei_2019") 
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


writeData(wb,"HH_tbl_WELLBEING",rbind.data.frame(Sultra_WELLBEING,Koon_WELLBEING,Kei_WELLBEING))
writeData(wb,"HH_tbl_DEMOGRAPHIC",rbind.data.frame(Sultra_DEMOGRAPHIC,Koon_DEMOGRAPHIC,Kei_DEMOGRAPHIC))
writeData(wb,"HH_tbl_BIRTHS",rbind.data.frame(Sultra_BIRTHS,Koon_BIRTHS,Kei_BIRTHS))
writeData(wb,"HH_tbl_DEATHS",rbind.data.frame(Sultra_DEATHS,Koon_DEATHS,Kei_DEATHS))
writeData(wb,"HH_tbl_ORGANIZATION",rbind.data.frame(Sultra_ORGANIZATION,Koon_ORGANIZATION,Kei_ORGANIZATION))
writeData(wb,"HH_tbl_NMORGANIZATION",rbind.data.frame(Sultra_NMORGANIZATION,Koon_NMORGANIZATION,Kei_NMORGANIZATION))
writeData(wb,"HH_tbl_LTHREAT",rbind.data.frame(Sultra_LTHREAT,Koon_LTHREAT,Kei_LTHREAT))
writeData(wb,"HH_tbl_LSTEPS",rbind.data.frame(Sultra_LSTEPS,Koon_LSTEPS,Kei_LSTEPS))
writeData(wb,"HH_tbl_GTHREAT",rbind.data.frame(Sultra_GTHREAT,Koon_GTHREAT,Kei_GTHREAT))
writeData(wb,"HH_tbl_GSTEPS",rbind.data.frame(Sultra_GSTEPS,Koon_GSTEPS,Kei_GSTEPS))


saveWorkbook(wb,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_SULAWESI_TENGGARA/4_PUSH_MASTER/1_HWB/SULTRA_KOON_KEI_post-QAQC_2019_1111.xlsx')
saveWorkbook(wb,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/4_PUSH_MASTER/1_HWB/SULTRA_KOON_KEI_post-QAQC_2019_1111.xlsx')
saveWorkbook(wb,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/4_PUSH_MASTER/1_HWB/SULTRA_KOON_KEI_post-QAQC_2019_1111.xlsx')
