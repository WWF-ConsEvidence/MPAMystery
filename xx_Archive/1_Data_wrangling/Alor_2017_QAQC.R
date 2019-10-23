# load libraries, load data
pacman::p_load(dplyr,xlsx,foreach,reshape2,stringdist)


WELLBEING <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/2_PRE_QAQC_RECOMBINE/SELAT_PANTAR/FlatFiles/ALOR_2017_Pre-QAQC_WELLBEING.csv')

DEMOGRAPHIC <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/2_PRE_QAQC_RECOMBINE/SELAT_PANTAR/FlatFiles/ALOR_2017_Pre-QAQC_DEMOGRAPHIC.csv')


WELLBEING_all <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/GitHub/MPAMystery/2_Social/FlatDataFiles/SBS/HH_tbl_WELLBEING.csv')


WELLBEING_all <- filter(WELLBEING_all,MPAID==18)


empty_rows <-
  WELLBEING[,-which(names(WELLBEING) %in% 
                      c("PoorFishUnits","GoodFishUnits",
                        "DataEntryComplete","DataCheckComplete"))] %>%
  dplyr::transmute(HouseholdID=HouseholdID,
                   OnlyHHID=foreach(i=1:length(.$HouseholdID)) %do% ifelse(nrow(data.frame(A=as.character(is.na(.[i,-which(names(.) %in% "HouseholdID")]) |
                                                                                                            .[i,-which(names(.) %in% "HouseholdID")]=="")) %>%
                                                                                  filter(A==FALSE))>0,"NO","YES"),
                   OnlyHHID.SettID=foreach(i=1:length(.$HouseholdID)) %do% ifelse(nrow(data.frame(A=as.character(is.na(.[i,-which(names(.) %in% c("HouseholdID","SettlementID"))]) |
                                                                                                                   .[i,-which(names(.) %in% c("HouseholdID","SettlementID"))]=="")) %>%
                                                                                         filter(A==FALSE))>0,"NO","YES")) %>%
  dplyr::filter(.,OnlyHHID=="YES" |
                  OnlyHHID.SettID=="YES") %>%
  dplyr::mutate(RemoveEmpty="YES")


# remove all empty rows, outputting number of removed rows and household pairs that need to be checked from duplicated analysis
WELLBEING_appended <-
  left_join(WELLBEING,empty_rows[,c("HouseholdID","RemoveEmpty")],by="HouseholdID") %>%
  dplyr::filter(.,is.na(RemoveEmpty)) %>%
  .[,-which(names(.) %in% "RemoveEmpty")]


# -- duplicated rows 
duplicated_rows <- 
  WELLBEING_appended %>% 
  left_join(.,DEMOGRAPHIC %>%
              dplyr::group_by(HouseholdID) %>%
              dplyr::summarise(NumInd=length(DemographicID),
                               IndividualName.1=IndividualName[1],
                               RelationHHH.1=RelationHHH[1],
                               IndividualAge.1=IndividualAge[1],
                               IndividualEducation.1=IndividualEducation[1],
                               IndividualName.2=ifelse(NumInd>1,IndividualName[2],NA),
                               RelationHHH.2=ifelse(NumInd>1,RelationHHH[2],NA),
                               IndividualAge.2=ifelse(NumInd>1,IndividualAge[2],NA),
                               IndividualEducation.2=ifelse(NumInd>1,IndividualEducation[2],NA),
                               IndividualName.3=ifelse(NumInd>2,IndividualName[3],NA),
                               RelationHHH.3=ifelse(NumInd>2,RelationHHH[3],NA),
                               IndividualAge.3=ifelse(NumInd>2,IndividualAge[3],NA),
                               IndividualEducation.3=ifelse(NumInd>2,IndividualEducation[3],NA),
                               IndividualName.4=ifelse(NumInd>3,IndividualName[4],NA),
                               RelationHHH.4=ifelse(NumInd>3,RelationHHH[4],NA),
                               IndividualAge.4=ifelse(NumInd>3,IndividualAge[4],NA),
                               IndividualEducation.4=ifelse(NumInd>3,IndividualEducation[4],NA)), by="HouseholdID") %>%
  dplyr::group_by(Respondent,SecondaryRespondent) %>% 
  dplyr::summarise(NumHH=length(HouseholdID)) %>% 
  dplyr::filter(.,NumHH>1) %>%
  dplyr::mutate(HouseholdID.1=WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent)][1],
                HouseholdID.2=WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent)][2],
                HouseholdID.3=ifelse(NumHH>2,
                                     WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent)][3],
                                     NA),
                Diff.bt.Household.1.2=ifelse(HouseholdID.1==HouseholdID.2,
                                             ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                     WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")][2,])) %>%
                                                           filter(A==FALSE))>0,
                                                    paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                      WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")][2,])==FALSE)]),
                                                          collapse=", "),
                                                    "NONE"),
                                             ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                                     WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
                                                           filter(A==FALSE))>0,
                                                    paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                                      WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
                                                          collapse=", "),
                                                    "NONE")),
                Diff.bt.Household.1.3=ifelse(is.na(HouseholdID.3), 
                                             NA,
                                             ifelse(HouseholdID.1==HouseholdID.2 & HouseholdID.1==HouseholdID.3,
                                                    ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                            WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])) %>%
                                                                  filter(A==FALSE))>0,
                                                           paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                             WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])==FALSE)]),
                                                                 collapse=", "),
                                                           "NONE"),
                                                    ifelse(HouseholdID.1==HouseholdID.3 & HouseholdID.1!=HouseholdID.2,
                                                           ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                                   WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])) %>%
                                                                         filter(A==FALSE))>0,
                                                                  paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])==FALSE)]),
                                                                        collapse=", "),
                                                                  "NONE"),
                                                           ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                                                   WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
                                                                         filter(A==FALSE))>0,
                                                                  paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
                                                                        collapse=", "),
                                                                  "NONE")))),
                Diff.bt.Household.2.3=ifelse(is.na(HouseholdID.3), 
                                             NA,
                                             ifelse(HouseholdID.2==HouseholdID.3 & HouseholdID.2==HouseholdID.1,
                                                    ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][2,]==
                                                                                            WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])) %>%
                                                                  filter(A==FALSE))>0,
                                                           paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][2,]==
                                                                                             WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])==FALSE)]),
                                                                 collapse=", "),
                                                           "NONE"),
                                                    ifelse(HouseholdID.2==HouseholdID.3 & HouseholdID.2!=HouseholdID.1,
                                                           ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                                   WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])) %>%
                                                                         filter(A==FALSE))>0,
                                                                  paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])==FALSE)]),
                                                                        collapse=", "),
                                                                  "NONE"),
                                                           ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                                                   WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
                                                                         filter(A==FALSE))>0,
                                                                  paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
                                                                        collapse=", "),
                                                                  "NONE"))))) %>%
  data.frame(HouseholdID=c(.$HouseholdID.2,.$HouseholdID.3),
             RemoveDuplicate=c(ifelse(.$Diff.bt.Household.1.2=="NONE","YES","MANUAL CHECK"),
                               ifelse(is.na(.$HouseholdID.3)==T,
                                      NA,
                                      ifelse(.$Diff.bt.Household.1.3=="NONE" | .$Diff.bt.Household.2.3=="NONE",
                                             "YES",
                                             "MANUAL CHECK"))),
             CheckAgainst=c(ifelse(.$Diff.bt.Household.1.2!="NONE",.$HouseholdID.1,NA),
                            ifelse(.$Diff.bt.Household.1.3!="NONE" & .$Diff.bt.Household.2.3!="NONE" & .$Diff.bt.Household.1.2!="NONE",paste(.$HouseholdID.1,.$HouseholdID.2,sep=","),
                                   ifelse(.$Diff.bt.Household.1.3!="NONE" & .$Diff.bt.Household.2.3!="NONE" &.$Diff.bt.Household.1.2=="NONE",
                                          .$HouseholdID.1,
                                          ifelse(.$Diff.bt.Household.1.3!="NONE" & .$Diff.bt.Household.2.3=="NONE",
                                                 .$HouseholdID.1,
                                                 ifelse(.$Diff.bt.Household.1.3=="NONE" & .$Diff.bt.Household.2.3!="NONE",
                                                        .$HouseholdID.2,
                                                        NA))))))

write.csv(duplicated_rows,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/SELAT_PANTAR/1_HWB/duplicated_rows.csv')

WELLBEING_appended <- 
  left_join(WELLBEING_appended,duplicated_rows[,c("HouseholdID","RemoveDuplicate")],by="HouseholdID") %>%
  dplyr::filter(.,RemoveDuplicate=="MANUAL CHECK" | is.na(RemoveDuplicate)) %>%
  .[,-which(names(.) %in% "RemoveDuplicate")]

DEMOGRAPHIC_appended <-
  left_join(DEMOGRAPHIC,duplicated_rows[,c("HouseholdID","RemoveDuplicate")],by="HouseholdID") %>%
  dplyr::filter(.,RemoveDuplicate=="MANUAL CHECK" | is.na(RemoveDuplicate)) %>%
  .[,-which(names(.) %in% "RemoveDuplicate")]


# -- ambiguous rows

ambiguous_rows <-
  WELLBEING_appended %>% 
  dplyr::group_by(CountryID,MPAID,SettlementID,KK.Code) %>% 
  dplyr::summarise(NumHH=length(HouseholdID)) %>% 
  dplyr::filter(.,NumHH>1) %>%
  dplyr::mutate(HouseholdID.1=unlist(foreach(i=1:length(CountryID)) %do% WELLBEING_appended$HouseholdID[which(WELLBEING_appended$SettlementID==SettlementID[i] &
                                                                                                                WELLBEING_appended$KK.Code==KK.Code[i])][1]),
                HouseholdID.2=unlist(foreach(i=1:length(CountryID)) %do% WELLBEING_appended$HouseholdID[which(WELLBEING_appended$SettlementID==SettlementID[i] &
                                                                                                                WELLBEING_appended$KK.Code==KK.Code[i])][2]),
                Respondent.1=unlist(foreach(i=1:length(CountryID)) %do% as.character(WELLBEING_appended$Respondent[which(WELLBEING_appended$SettlementID==SettlementID[i] &
                                                                                                                           WELLBEING_appended$KK.Code==KK.Code[i])][1])),
                Respondent.2=unlist(foreach(i=1:length(CountryID)) %do% as.character(WELLBEING_appended$Respondent[which(WELLBEING_appended$SettlementID==SettlementID[i] &
                                                                                                                           WELLBEING_appended$KK.Code==KK.Code[i])][2])),
                StrDist=unlist(foreach(i=1:length(CountryID)) %do% stringdist(Respondent.1[i],Respondent.2[i])))

write.csv(ambiguous_rows,'C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2017_FLORES_TIMUR_ALOR/3_QAQC/SELAT_PANTAR/1_HWB/ambiguous_rows.csv')


# -- incomplete rows (dataentrycomplete == FALSE)

incomplete_rows



# 
# duplicated_rows_DEMOS <- 
#   DEMOGRAPHIC %>% 
#   dplyr::group_by(IndividualName, HouseholdID) %>% 
#   dplyr::summarise(NumInd=length(DemographicID)) %>% 
#   dplyr::filter(.,NumInd>1)


# NOTES:
# - check for outliers, coding errors, etc. (age, assets, etc.)
blind_codes <-
  

coding_errors

# NOTE: should I add TimeMarket here?  needs to be checked, but has different potential bounds (i.e., an outlier for TimeMarket might be >500)
WELLBEING_coding_error_colnames <- c("YearsResident","Religion","PrimaryLivelihood","SecondaryLivelihood","TertiaryLivelihood","FreqFish",
                                     "FreqSaleFish","PercentIncomeFish","FreqEatFish","PercentProteinFish","MajorFishTechnique","LessProductiveDaysFishing",
                                     "PoorCatch","PoorFishIncomeL","MoreProductiveDaysFishing","GoodCatch","GoodFishIncomeL","EconomicStatusTrend","AssetCar",
                                     "AssetTruck","AssetBicycle","AssetMotorcycle","AssetBoatNoMotor","AssetBoatOutboard","AssetBoatInboard","AssetLandlinePhone",
                                     "AssetCellPhone","AssetTV","AssetRadio","AssetStereo","AssetCD","AssetDVD","AssetSatellite","CookingFuel","AssetGenerator",
                                     "HouseholdDeath","HouseholdBirth","FSNotEnough","FSDidNotLast","FSBalancedDiet","FSAdultSkip","FSFreqAdultSkip","FSEatLess",
                                     "FSHungry","FSChildPortion","FSLowCostFood","FSChildSkip","FSFreqChildSkip","FSNoMealChild","RightsAccess","RightsHarvest",
                                     "RightsManage","RightsExclude","RightsTransfer","SocialConflict","MarineGroup","NumberMarineGroup","OtherGroup","NumberOtherGroup",
                                     "VoteDistrict","VoteNational","NumLocalThreat","NumGlobalThreat","NumLocalAction","NumGlobalAction","PlaceHappy","PlaceFavourite",
                                     "PlaceMiss","PlaceBest","PlaceFishHere","PlaceBeMyself")

for(i in WELLBEING_coding_error_colnames) {
  a[,i] <- as.data.frame(mapply(a=WELLBEING[,i],
       b=WELLBEING$HouseholdID,
       function(a,b){
         coding_error <- data.frame(c=ifelse(a>8 & a<993,"yes","no"),
                                    i=a,
                                    HouseholdID=b) %>% filter(.,c=="yes") %>% select(.,c(HouseholdID,i))
         coding_error
       }))} %>% unlist(.) %>% t(.) %>% data.frame(.) %>% filter(.,!is.null(HouseholdID))
}


# ADD: cFS if no children
logic_errors <-
  DEMOGRAPHIC %>%
  group_by(HouseholdID) %>%
  summarise(OldestHHMember=max(IndividualAge, na.rm=T),
            NumberOldHHMember=length(DemographicID[IndividualAge==OldestHHMember]),
            DemographicID.OldestHHMember=DemographicID[IndividualAge==OldestHHMember][1],
            IndividualAge=IndividualAge[DemographicID==DemographicID.OldestHHMember],
            NumHeadHH=length(DemographicID[RelationHHH==0])) %>%
  left_join(WELLBEING, ., by="HouseholdID") %>%
  transmute(HouseholdID=HouseholdID,
            OldestHHMember=IndividualAge,
            YearsResident=YearsResident,
            YearResident.v.Age=ifelse(YearsResident>IndividualAge,"Logic error","NONE"),
            NumHeadHH=NumHeadHH,
            NumHeadHH.over1=ifelse(NumHeadHH>1,"Logic error","NONE"),
            NumHeadHH.zero=ifelse(NumHeadHH==0,"Logic error","NONE")) %>%
  filter(YearResident.v.Age=="Logic error" | NumHeadHH.over1=="Logic error" | NumHeadHH.zero=="Logic error")
  


# - record total number of blind codes per variable
# - should I turn the auto-skipped questions to 998 (or blind code for "skipped" -- 994??)?