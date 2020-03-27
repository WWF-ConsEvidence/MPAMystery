
# ---- 1.1 Load libraries & data ----

pacman::p_load(rio, plyr, foreach, dplyr, openxlsx, chron)

Import_filepath <- 'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_WAKATOBI/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2020_0313/WAKATOBI_2019_post-QAQC_2020_0313.xlsx'

# - WAKATOBI
Wakatobi_WELLBEING <- 
  import(Import_filepath,
         sheet="HH_tbl_WELLBEING")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_DEMOGRAPHIC <- 
  import(Import_filepath,
         sheet="HH_tbl_DEMOGRAPHIC")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_BIRTHS <- 
  import(Import_filepath,
         sheet="HH_tbl_BIRTHS")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_DEATHS <- 
  import(Import_filepath,
         sheet="HH_tbl_DEATHS")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_ORGANIZATION <- 
  import(Import_filepath,
         sheet="HH_tbl_ORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_NMORGANIZATION <- 
  import(Import_filepath,
         sheet="HH_tbl_NMORGANIZATION")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_LTHREAT <- 
  import(Import_filepath,
         sheet="HH_tbl_LTHREAT")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_LSTEPS <- 
  import(Import_filepath,
         sheet="HH_tbl_LSTEPS")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_GTHREAT <- 
  import(Import_filepath,
         sheet="HH_tbl_GTHREAT")  %>%
  filter(!is.na(HouseholdID))
Wakatobi_GSTEPS <- 
  import(Import_filepath,
         sheet="HH_tbl_GSTEPS")  %>%
  filter(!is.na(HouseholdID))


# find the maximum primary ID for each table in current version of master database
Max_ID_per_table_master <- data.frame(HouseholdID=11165,
                                      DemographicID=60735,
                                      BirthID=5689,
                                      DeathID=4379,
                                      OrganizationID=5379,
                                      NMOrganizationID=8867,
                                      LThreatID=18878,
                                      LocalStepsID=12308,
                                      GlobalThreatID=13593,
                                      GlobalStepsID=8343)

# ---- 1.2 Calculated columns ----

# -- Calculate:
#     combined assets columns
#     time market (from hours+minutes)
#     fishing frequency (from value+units)
#     sell fish frequency (from value+units)
#     eat fish frequency (from value+units)
#     major fishing technique (post-code from more built-out response options)

Wakatobi_WELLBEING <-
  Wakatobi_WELLBEING %>%
  mutate(AssetCarTruck=ifelse(AssetCar<989 & AssetTruck<989,
                              rowSums(select(.,AssetCar,AssetTruck),na.rm=T),
                              ifelse(AssetCar<989 & AssetTruck>989,
                                     AssetCar,
                                     ifelse(AssetCar>989 & AssetTruck<989,
                                            AssetTruck,
                                            NA))),
         AssetPhoneCombined=ifelse(AssetLandlinePhone<989 & AssetCellPhone<989,
                                   rowSums(select(.,AssetLandlinePhone,AssetCellPhone),na.rm=T),
                                   ifelse(AssetLandlinePhone<989 & AssetCellPhone>989,
                                          AssetLandlinePhone,
                                          ifelse(AssetLandlinePhone>989 & AssetCellPhone<989,
                                                 AssetCellPhone,
                                                 NA))),
         AssetRadioCoded=ifelse(AssetRadio<989,AssetRadio,NA),
         AssetStereoCoded=ifelse(AssetStereo<989,AssetStereo,NA),
         AssetCDCoded=ifelse(AssetCD<989,AssetCD,NA),
         AssetDVDCoded=ifelse(AssetDVD<989,AssetDVD,NA)) %>%
  mutate(AssetEntertain=rowSums(select(.,AssetRadioCoded,AssetStereoCoded,AssetCDCoded,AssetDVDCoded),na.rm=T),
         TimeMarket=ifelse(TimeMarket1_Hour<989 & TimeMarket1_Minute<989, round(((TimeMarket1_Hour*60)+TimeMarket1_Minute)/60,3),TimeMarket1_Hour),
         TimeSecondaryMarket=ifelse(TimeMarket2_Hour<989 & TimeMarket2_Minute<989, round(((TimeMarket2_Hour*60)+TimeMarket2_Minute)/60,3),TimeMarket2_Hour),
         # -- to calculate frequency questions, we have to calculate things into the categories outlined in Glew et al (2012) protocol:
              # 1 = Once in 6 months or never (0-1 time per 6 months)
              # 2 = A few times per 6 months (2-10 times per 6 months; or 1 time per month)
              # 3 = A few times per month (11-30 times per 6 months; or 2-5 times per month; or 1 time per week)
              # 4 = A few times per week (31-90 times per 6 months; or 6-15 times per month; or 2-3 times per week)
              # 5 = More than a few times per week (>90 times per 6 months; or >15 times per month; or >3 times per week; or >=1 time per day)
         # -- the unit codes provided in the ODK form are:
              # 1 = per day
              # 2 = per week
              # 3 = per month
              # 4 = per six months
              # 997/998/999 = blind codes
         FreqFish=ifelse(FreqFishNumber==0 | 
                           (FreqFishNumber==1 & FreqFishUnit==4),1,
                         ifelse((FreqFishNumber%in%c(2:10) & FreqFishUnit==4) | 
                                  (FreqFishNumber==1 & FreqFishUnit==3),2,
                                ifelse((FreqFishNumber%in%c(11:30) & FreqFishUnit==4) | 
                                         (FreqFishNumber%in%c(2:5) & FreqFishUnit==3) | 
                                         (FreqFishNumber==1 & FreqFishUnit==2),3,
                                       ifelse((FreqFishNumber%in%c(31:90) & FreqFishUnit==4) | 
                                                (FreqFishNumber%in%c(6:15) & FreqFishUnit==3) | 
                                                (FreqFishNumber%in%c(2,3) & FreqFishUnit==2),4,
                                              ifelse((FreqFishNumber>90 & FreqFishUnit==4) | 
                                                       (FreqFishNumber>15 & FreqFishUnit==3) | 
                                                       (FreqFishNumber>3 & FreqFishUnit==2) | 
                                                       (FreqFishNumber>0 & FreqFishUnit==1),5,998))))),
         FreqSaleFish=ifelse(FreqSaleFishNumber==0 | 
                               (FreqSaleFishNumber==1 & FreqSaleFishUnit==4),1,
                             ifelse((FreqSaleFishNumber%in%c(2:10) & FreqSaleFishUnit==4) | 
                                      (FreqSaleFishNumber==1 & FreqSaleFishUnit==3),2,
                                    ifelse((FreqSaleFishNumber%in%c(11:30) & FreqSaleFishUnit==4) | 
                                             (FreqSaleFishNumber%in%c(2:5) & FreqSaleFishUnit==3) | 
                                             (FreqSaleFishNumber==1 & FreqSaleFishUnit==2),3,
                                           ifelse((FreqSaleFishNumber%in%c(31:90) & FreqSaleFishUnit==4) | 
                                                    (FreqSaleFishNumber%in%c(6:15) & FreqSaleFishUnit==3) | 
                                                    (FreqSaleFishNumber%in%c(2,3) & FreqSaleFishUnit==2),4,
                                                  ifelse((FreqSaleFishNumber>90 & FreqSaleFishUnit==4) | 
                                                           (FreqSaleFishNumber>15 & FreqSaleFishUnit==3) | 
                                                           (FreqSaleFishNumber>3 & FreqSaleFishUnit==2) | 
                                                           (FreqSaleFishNumber>0 & FreqSaleFishUnit==1),5,998))))),
         FreqEatFish=ifelse(FreqEatFishNumber==0 | 
                              (FreqEatFishNumber==1 & FreqEatFishUnit==4),1,
                            ifelse((FreqEatFishNumber%in%c(2:10) & FreqEatFishUnit==4) | 
                                     (FreqEatFishNumber==1 & FreqEatFishUnit==3),2,
                                   ifelse((FreqEatFishNumber%in%c(11:30) & FreqEatFishUnit==4) | 
                                            (FreqEatFishNumber%in%c(2:5) & FreqEatFishUnit==3) | 
                                            (FreqEatFishNumber==1 & FreqEatFishUnit==2),3,
                                          ifelse((FreqEatFishNumber%in%c(31:90) & FreqEatFishUnit==4) | 
                                                   (FreqEatFishNumber%in%c(6:15) & FreqEatFishUnit==3) | 
                                                   (FreqEatFishNumber%in%c(2,3) & FreqEatFishUnit==2),4,
                                                 ifelse((FreqEatFishNumber>90 & FreqEatFishUnit==4) | 
                                                          (FreqEatFishNumber>15 & FreqEatFishUnit==3) | 
                                                          (FreqEatFishNumber>3 & FreqEatFishUnit==2) | 
                                                          (FreqEatFishNumber>0 & FreqEatFishUnit==1),5,998))))),
         # -- to post-code fishing technique into a smaller set of response options (to be comparable with earlier survey vesions), 
         #    we have to put things into the categories outlined in the Glew et al (2012) protocol:
              # 1 = Fishing by hand or handheld gear -- from ODK: this includes codes 1 (hand line), 4 (gleaning), 5 (spear gun), 6 (falling gear, cast nets)
              # 2 = Fishing with stationary net -- from ODK: this includes codes 7 (stationary gillnet), 8 (trammel net), 10 (traps, stow net, portable trap), 11 (fences), 12 (stationary lift net)
              # 3 = Fishing with mobile net -- from ODK: this includes codes 9 (mobile gillnet), 13 (mobile dredges), 14 (trawl), 15 (purse seine)
              # 4 = Fishing with stationary line -- from ODK: this includes code 2 (stationary line)
              # 5 = Fishing with mobile line -- from ODK: this includes code 3 (mobile line)
              # 6 = Fishing with explosives -- from ODK: this includes code 16 (explosives, poison)
              # 996 = other
              # 995/997/998/999 = blind responses
         MajorFishTechnique=ifelse(MajorFishTechnique_Primary%in%c(1,4,5,6),1,
                                   ifelse(MajorFishTechnique_Primary%in%c(7,8,10,11,12),2,
                                          ifelse(MajorFishTechnique_Primary%in%c(9,13,14,15),3,
                                                 ifelse(MajorFishTechnique_Primary==2,4,
                                                        ifelse(MajorFishTechnique_Primary==3,5,
                                                               ifelse(MajorFishTechnique_Primary==16,6,
                                                                      ifelse(grepl("996",MajorFishTechnique_Primary),996,MajorFishTechnique_Primary))))))),
         LatDeg=ifelse(nchar(as.character(LatDeg))==1,paste("0",LatDeg,sep=""),as.character(LatDeg)),
         LatMin=ifelse(nchar(as.character(LatMin))==1,paste("0",LatMin,sep=""),as.character(LatMin)),
         LatSec=ifelse(nchar(as.character(LatSec))==1,paste("0",LatSec,sep=""),as.character(LatSec)),
         LatFrac=as.character(LatFrac),
         LonDeg=ifelse(nchar(as.character(LonDeg))==1,paste("0",LonDeg,sep=""),as.character(LonDeg)),
         LonMin=ifelse(nchar(as.character(LonMin))==1,paste("0",LonMin,sep=""),as.character(LonMin)),
         LonSec=ifelse(nchar(as.character(LonSec))==1,paste("0",LonSec,sep=""),as.character(LonSec)))


# ---- 2.1 WAKATOBI: Rename HouseholdID to EntryHouseholdID, rename a few columns, & add new IDs----

Wakatobi_WELLBEING <- 
  Wakatobi_WELLBEING %>% plyr::rename(c("HouseholdID"="EntryHouseholdID",
                                        "SurveyVersion"="SurveyVersionNumber",
                                        "MajorFishTechnique_Primary"="PrimaryFishTechnique",
                                        "MajorFishTechnique_Secondary"="SecondaryFishTechnique",
                                        "MajorFishTechnique_Tertiary"="TertiaryFishTechnique",
                                        "Market1_Name"="PrimaryMarketName",
                                        "Market2_Name"="SecondaryMarketName")) %>%
  .[order(.$SettlementID,.$KKCode),] %>%
  dplyr::mutate(HouseholdID=seq(Max_ID_per_table_master$HouseholdID+1,Max_ID_per_table_master$HouseholdID+length(EntryHouseholdID), by=1),
                EntryComputerIdentifier="WAKATOBI_2019_post-QAQC_2020_0313",
                CountryID=360,
                `KK Code`=KKCode,
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
                EntryHouseholdID,PilotReferenceCode,InterviewDate,Baseline_t2_pairs,PrimaryFishTechnique,SecondaryFishTechnique,TertiaryFishTechnique,PrimaryMarketName,SecondaryMarketName,TimeSecondaryMarket)

Wakatobi_DEMOGRAPHIC <- 
  Wakatobi_DEMOGRAPHIC %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DemographicID=seq(Max_ID_per_table_master$DemographicID+1,Max_ID_per_table_master$DemographicID+length(EntryHouseholdID), by=1),
                DemographicCode=ave(DemographicID, HouseholdID, FUN = seq_along),
                IndividualEdLevel=NA,
                HouseholdHead=NA) %>%
  dplyr::select(DemographicID,HouseholdID,IndividualName,RelationHHH,IndividualAge,IndividualGender,IndividualEducation,IndividualEdLevel,IndividualEnrolled,HouseholdHead,
                IndividualUnwell,IndividualDaysUnwell,IndividualLostDays,EntryHouseholdID,DemographicCode)

Wakatobi_BIRTHS <- 
  Wakatobi_BIRTHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID"))  %>%
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(BirthID=seq(Max_ID_per_table_master$BirthID+1,Max_ID_per_table_master$BirthID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(BirthID,HouseholdID,EntryHouseholdID,NameInfant,InfantSurvived,DateOfDeath)

Wakatobi_DEATHS <- 
  Wakatobi_DEATHS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(DeathID=seq(Max_ID_per_table_master$DeathID+1,Max_ID_per_table_master$DeathID+length(EntryHouseholdID), by=1),
                AgeAtDeath=round(ifelse(AgeAtDeath_Year==0 & AgeAtDeath_Month<989,AgeAtDeath_Month/12,AgeAtDeath_Year),2)) %>%
  dplyr::select(DeathID,HouseholdID,EntryHouseholdID,NameDeceased,Gender,AgeAtDeath,DateDeath)

Wakatobi_ORGANIZATION <- 
  Wakatobi_ORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(OrganizationID=seq(Max_ID_per_table_master$OrganizationID+1,Max_ID_per_table_master$OrganizationID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(OrganizationID,HouseholdID,EntryHouseholdID,MarineGroupName,MarinePosition,MarineMeeting,MarineDays,MarineContribution)

Wakatobi_NMORGANIZATION <- 
  Wakatobi_NMORGANIZATION %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(NMOrganizationID=seq(Max_ID_per_table_master$NMOrganizationID+1,Max_ID_per_table_master$NMOrganizationID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(NMOrganizationID,HouseholdID,EntryHouseholdID,OtherGroupName,OtherGroupPosition,OtherGroupMeeting,OtherGroupDays,OtherGroupContribution)

Wakatobi_LTHREAT <- 
  Wakatobi_LTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LThreatID=seq(Max_ID_per_table_master$LThreatID+1,Max_ID_per_table_master$LThreatID+length(EntryHouseholdID), by=1),
                LocalThreatID=NA) %>%
  dplyr::select(LThreatID,HouseholdID,EntryHouseholdID,LocalMarineThreat,LocalThreatID)

Wakatobi_LSTEPS <- 
  Wakatobi_LSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(LocalStepsID=seq(Max_ID_per_table_master$LocalStepsID+1,Max_ID_per_table_master$LocalStepsID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(LocalStepsID,HouseholdID,EntryHouseholdID,LocalSteps)

Wakatobi_GTHREAT <- 
  Wakatobi_GTHREAT %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>% 
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalThreatID=seq(Max_ID_per_table_master$GlobalThreatID+1,Max_ID_per_table_master$GlobalThreatID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalThreatID,HouseholdID,EntryHouseholdID,GLobalMarineThreat)

Wakatobi_GSTEPS <- 
  Wakatobi_GSTEPS %>% plyr::rename(c("HouseholdID"="EntryHouseholdID")) %>%   
  left_join(.,Wakatobi_WELLBEING[,c("EntryHouseholdID","HouseholdID")],by="EntryHouseholdID") %>%
  .[order(.$HouseholdID),] %>%
  dplyr::mutate(GlobalStepsID=seq(Max_ID_per_table_master$GlobalStepsID+1,Max_ID_per_table_master$GlobalStepsID+length(EntryHouseholdID), by=1)) %>%
  dplyr::select(GlobalStepsID,HouseholdID,EntryHouseholdID,GLobalMarineSteps)



# ---- 3.1 Write to xlsx for final data type check before pasting to master database ----

wb <- createWorkbook("Wakatobi_2019") 
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


writeData(wb,"HH_tbl_WELLBEING",Wakatobi_WELLBEING)
writeData(wb,"HH_tbl_DEMOGRAPHIC",Wakatobi_DEMOGRAPHIC)
writeData(wb,"HH_tbl_BIRTHS",Wakatobi_BIRTHS)
writeData(wb,"HH_tbl_DEATHS",Wakatobi_DEATHS)
writeData(wb,"HH_tbl_ORGANIZATION",Wakatobi_ORGANIZATION)
writeData(wb,"HH_tbl_NMORGANIZATION",Wakatobi_NMORGANIZATION)
writeData(wb,"HH_tbl_LTHREAT",Wakatobi_LTHREAT)
writeData(wb,"HH_tbl_LSTEPS",Wakatobi_LSTEPS)
writeData(wb,"HH_tbl_GTHREAT",Wakatobi_GTHREAT)
writeData(wb,"HH_tbl_GSTEPS",Wakatobi_GSTEPS)


saveWorkbook(wb,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_WAKATOBI/4_PUSH_MASTER/1_HWB/WAKATOBI_post-QAQC_2020_0316.xlsx')
