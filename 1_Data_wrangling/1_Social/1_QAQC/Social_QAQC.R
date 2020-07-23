# 
# code:  MPA Mystery social data QAQC code snippets
# 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: February 2019
# modified: July 2020
# 
# 
# ---- inputs ----
# 
# ---- outputs ----
# 
# ---- code sections ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIBRARIES & PRE-QAQC DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 load libraries ----

pacman::p_load(foreach, reshape2, stringdist, rio, dplyr)

# ---- 1.2 import data ----

DEMOGRAPHIC.QAQC <- import('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_WAKATOBI/3_QAQC/1_HWB/1_Pre-QAQC_imports_for_R/WAKATOBI_2019_pre-QAQC_DEMOGRAPHIC.csv')
WELLBEING.QAQC <-  import('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_WAKATOBI/3_QAQC/1_HWB/1_Pre-QAQC_imports_for_R/WAKATOBI_2019_pre-QAQC_WELLBEING.csv')

# GTHREAT <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_GTHREAT_SelatPantar_2017_Pre-QAQC.csv')
# GSTEPS <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_GSTEPS_SelatPantar_2017_Pre-QAQC.csv')
# LTHREAT <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_LTHREAT_SelatPantar_2017_Pre-QAQC.csv')
# LSTEPS <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_LSTEPS_SelatPantar_2017_Pre-QAQC.csv')
# ORGANIZATION <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_ORGANIZATION_SelatPantar_2017_Pre-QAQC.csv')
# NMORGANIZATION <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_NMORGANIZATION_SelatPantar_2017_Pre-QAQC.csv')
# BIRTHS <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_BIRTHS_SelatPantar_2017_Pre-QAQC.csv')
# DEATHS <- read.csv('2_Social/FlatDataFiles/SBS/SelatPantar_2017_QAQC/HH_tbl_DEATHS_SelatPantar_2017_Pre-QAQC.csv')


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: IDENTIFICATION ERRORS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# !!!NOTE!!! Much of this script provides snippets of code that were useful prior to tablet data collection, such as identifying empty rows or duplicated rows.
#            With mobile tablet data collection, these tend to not be issues anymore.  However, I've left the code here (commented out) in case it is useful to 
#            use parts of it in future QAQC processes.  


# -- check for empty rows in both WB and demos tables
# -- check for duplicated rows in both WB and demos tables
# -- check for refused households (this should be easier to check with field sampling sheets)
#     - has an interview start and end time, all identifying features but no data.  
#     - this needs to be recorded as refused in an output table then deleted from database (in both WB and demos tables)
# -- check for incomplete records in both WB and demos tables
#     - this could be two records that were split by mistake


# # -- check for coding errors
# 
# # HH LEVEL
# # ---- empty rows ----
# empty_rows <-
#   WELLBEING.QAQC[,-which(names(WELLBEING.QAQC) %in% 
#                       c("PoorFishUnits","GoodFishUnits",
#                         "DataEntryComplete","DataCheckComplete"))] %>%
#   dplyr::transmute(HouseholdID=HouseholdID,
#                    OnlyHHID=foreach(i=1:length(.$HouseholdID)) %do% ifelse(nrow(data.frame(A=as.character(is.na(.[i,-which(names(.) %in% "HouseholdID")]) |
#                                                                                                             .[i,-which(names(.) %in% "HouseholdID")]=="")) %>%
#                                                                                   filter(A==FALSE))>0,"NO","YES"),
#                    OnlyHHID.SettID=foreach(i=1:length(.$HouseholdID)) %do% ifelse(nrow(data.frame(A=as.character(is.na(.[i,-which(names(.) %in% c("HouseholdID","SettlementID"))]) |
#                                                                                                                    .[i,-which(names(.) %in% c("HouseholdID","SettlementID"))]=="")) %>%
#                                                                                          filter(A==FALSE))>0,"NO","YES")) %>%
#   dplyr::filter(.,OnlyHHID=="YES" |
#                   OnlyHHID.SettID=="YES") %>%
#   dplyr::mutate(RemoveEmpty="YES")
# 
# 
# # remove all empty rows, outputting number of removed rows and household pairs that need to be checked from duplicated analysis
# WELLBEING_appended <-
#   left_join(WELLBEING.QAQC,empty_rows[,c("HouseholdID","RemoveEmpty")],by="HouseholdID") %>%
#   dplyr::filter(.,is.na(RemoveEmpty)) %>%
#   .[,-which(names(.) %in% "RemoveEmpty")]
# 
# 
# # ---- duplicate rows ----
# duplicated_rows <- 
#   WELLBEING.QAQC %>% 
#   left_join(.,DEMOGRAPHIC.QAQC %>%
#               dplyr::group_by(HouseholdID) %>%
#               dplyr::summarise(NumInd=length(DemographicID),
#                                IndividualName.1=IndividualName[1],
#                                RelationHHH.1=RelationHHH[1],
#                                IndividualAge.1=IndividualAge[1],
#                                IndividualEducation.1=IndividualEducation[1]), by="HouseholdID") %>%
#   dplyr::group_by(Respondent,SecondaryRespondent) %>% 
#   dplyr::summarise(NumHH=length(HouseholdID)) %>% 
#   dplyr::filter(.,NumHH>1) %>%
#   dplyr::mutate(HouseholdID.1=WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent)][1],
#                 HouseholdID.2=WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent)][2],
#                 HouseholdID.3=ifelse(NumHH>2,
#                                      WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent)][3],
#                                      NA),
#                 Diff.bt.Household.1.2=ifelse(HouseholdID.1==HouseholdID.2,
#                                              ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                      WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")][2,])) %>%
#                                                            filter(A==FALSE))>0,
#                                                     paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                       WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")][2,])==FALSE)]),
#                                                           collapse=", "),
#                                                     "NONE"),
#                                              ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
#                                                                                      WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
#                                                            filter(A==FALSE))>0,
#                                                     paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
#                                                                                       WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
#                                                           collapse=", "),
#                                                     "NONE")),
#                 Diff.bt.Household.1.3=ifelse(is.na(HouseholdID.3), 
#                                              NA,
#                                              ifelse(HouseholdID.1==HouseholdID.2 & HouseholdID.1==HouseholdID.3,
#                                                     ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                             WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])) %>%
#                                                                   filter(A==FALSE))>0,
#                                                            paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                              WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])==FALSE)]),
#                                                                  collapse=", "),
#                                                            "NONE"),
#                                                     ifelse(HouseholdID.1==HouseholdID.3 & HouseholdID.1!=HouseholdID.2,
#                                                            ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])) %>%
#                                                                          filter(A==FALSE))>0,
#                                                                   paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                                     WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])==FALSE)]),
#                                                                         collapse=", "),
#                                                                   "NONE"),
#                                                            ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
#                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
#                                                                          filter(A==FALSE))>0,
#                                                                   paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
#                                                                                                     WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
#                                                                         collapse=", "),
#                                                                   "NONE")))),
#                 Diff.bt.Household.2.3=ifelse(is.na(HouseholdID.3), 
#                                              NA,
#                                              ifelse(HouseholdID.2==HouseholdID.3 & HouseholdID.2==HouseholdID.1,
#                                                     ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][2,]==
#                                                                                             WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])) %>%
#                                                                   filter(A==FALSE))>0,
#                                                            paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][2,]==
#                                                                                              WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][3,])==FALSE)]),
#                                                                  collapse=", "),
#                                                            "NONE"),
#                                                     ifelse(HouseholdID.2==HouseholdID.3 & HouseholdID.2!=HouseholdID.1,
#                                                            ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])) %>%
#                                                                          filter(A==FALSE))>0,
#                                                                   paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")][1,]==
#                                                                                                     WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")][2,])==FALSE)]),
#                                                                         collapse=", "),
#                                                                   "NONE"),
#                                                            ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
#                                                                                                    WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
#                                                                          filter(A==FALSE))>0,
#                                                                   paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
#                                                                                                     WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
#                                                                         collapse=", "),
#                                                                   "NONE"))))) %>%
#   data.frame(HouseholdID=c(.$HouseholdID.2,.$HouseholdID.3),
#              RemoveDuplicate=c(ifelse(.$Diff.bt.Household.1.2=="NONE","YES","MANUAL CHECK"),
#                                ifelse(is.na(.$HouseholdID.3)==T,
#                                       NA,
#                                       ifelse(.$Diff.bt.Household.1.3=="NONE" | .$Diff.bt.Household.2.3=="NONE",
#                                              "YES",
#                                              "MANUAL CHECK"))),
#              CheckAgainst=c(ifelse(.$Diff.bt.Household.1.2!="NONE",.$HouseholdID.1,NA),
#                             ifelse(.$Diff.bt.Household.1.3!="NONE" & .$Diff.bt.Household.2.3!="NONE" & .$Diff.bt.Household.1.2!="NONE",paste(.$HouseholdID.1,.$HouseholdID.2,sep=","),
#                                    ifelse(.$Diff.bt.Household.1.3!="NONE" & .$Diff.bt.Household.2.3!="NONE" &.$Diff.bt.Household.1.2=="NONE",
#                                           .$HouseholdID.1,
#                                           ifelse(.$Diff.bt.Household.1.3!="NONE" & .$Diff.bt.Household.2.3=="NONE",
#                                                  .$HouseholdID.1,
#                                                  ifelse(.$Diff.bt.Household.1.3=="NONE" & .$Diff.bt.Household.2.3!="NONE",
#                                                         .$HouseholdID.2,
#                                                         NA))))))
# 
# check_duplicates <- 
#   duplicated_rows[duplicated_rows$RemoveDuplicate=="CHECK" &
#                     !is.na(duplicated_rows$RemoveDuplicate),] %>%
#   transmute(HouseholdsToCompare=paste(CheckAgainst,HouseholdID,sep=", "),
#             CheckColNames=ifelse(Household.1.2!="YES" & !is.na(Household.1.2),
#                                  Household.1.2,
#                                  ifelse(Household.1.3!="YES" & !is.na(Household.1.3),
#                                         Household.1.3,
#                                         ifelse(Household.2.3!="YES" & !is.na(Household.2.3),
#                                                Household.2.3,NA))))
# 
# # Now need to output pairs that need to be checked, and remove all duplicated households 
# # (from all tables -- should I verify that the same households are duplicated in the demos table first???)
# 
# # remove all duplicated rows, outputting number of removed rows and household pairs that need to be checked from duplicated analysis
# WELLBEING_appended <-
#   left_join(WELLBEING_appended,duplicated_rows[,c("HouseholdID","RemoveDuplicate")],by="HouseholdID") %>%
#   dplyr::filter(.,RemoveDuplicate=="CHECK" | is.na(RemoveDuplicate)) %>%
#   .[,-which(names(.) %in% "RemoveDuplicate")]
# 
# 
# # check dataentrycomplete column, then check if any identifiers are blank
# # ---will need to output all incomplete rows to a "check" file, along with duplicate items to be checked and all ambiguous rows
# incomplete_rows <-
#   left_join(WELLBEING_appended,duplicated_rows[,c("HouseholdID","RemoveDuplicate")],by="HouseholdID") %>%
#   transmute(HouseholdID=HouseholdID,
#             IncompleteCheck=ifelse(DataEntryComplete==FALSE,"CHECK",NA),
#             RemoveDuplicate=RemoveDuplicate)
# 
# # ---should do this after already removing duplicated rows & checking incomplete rows. Is there a way to 
# # create running WELLBEING, DEMOGRAPHIC, etc. data frames with updates as they are made?
# ambiguous_rows <-
#   WELLBEING_appended %>% 
#   dplyr::group_by(CountryID,MPAID,SettlementID,KK.Code) %>% 
#   dplyr::summarise(NumHH=length(HouseholdID)) %>% 
#   dplyr::filter(.,NumHH>1) %>%
#   dplyr::mutate(HouseholdID.1=unlist(foreach(i=1:length(CountryID)) %do% WELLBEING_appended$HouseholdID[which(WELLBEING_appended$SettlementID==SettlementID[i] &
#                                                                                                          WELLBEING_appended$KK.Code==KK.Code[i])][1]),
#                 HouseholdID.2=unlist(foreach(i=1:length(CountryID)) %do% WELLBEING_appended$HouseholdID[which(WELLBEING_appended$SettlementID==SettlementID[i] &
#                                                                                                          WELLBEING_appended$KK.Code==KK.Code[i])][2]),
#                 Respondent.1=unlist(foreach(i=1:length(CountryID)) %do% as.character(WELLBEING_appended$Respondent[which(WELLBEING_appended$SettlementID==SettlementID[i] &
#                                                                                                        WELLBEING_appended$KK.Code==KK.Code[i])][1])),
#                 Respondent.2=unlist(foreach(i=1:length(CountryID)) %do% as.character(WELLBEING_appended$Respondent[which(WELLBEING_appended$SettlementID==SettlementID[i] &
#                                                                                                        WELLBEING_appended$KK.Code==KK.Code[i])][2])),
#                 StrDist=unlist(foreach(i=1:length(CountryID)) %do% stringdist(Respondent.1[i],Respondent.2[i]))),
#                 RemoveAmbiguous=ifelse(StrDist>4,NA,
#                                        ifelse(nrow(data.frame(A=as.character(WELLBEING_appended[which(WELLBEING_appended$HouseholdID==HouseholdID.1),
#                                                                                                    -which(names(WELLBEING_appended) %in% c("HouseholdID","Respondent","SecondaryRespondent"))]==
#                                                                                   WELLBEING_appended[which(WELLBEING_appended$HouseholdID==HouseholdID.2),
#                                                                                                      -which(names(WELLBEING_appended) %in% c("HouseholdID","Respondent","SecondaryRespondent"))])) %>%
#                                                         filter(A==FALSE))>0,
#                                                         "CHECK",
#                                                         "YES")))
# 
# 
# # INDIVIDUAL LEVEL
# 
# # remove duplicated households before checking for more duplicated rows in demos data
# 
# DEMOGRAPHIC_appended <-
#   left_join(DEMOGRAPHIC.QAQC,WELLBEING.QAQC[,c("HouseholdID","SettlementID","KK.Code",
#                                       "Respondent","SecondaryRespondent",
#                                       "PrimaryInterviewer","SecondaryInterviewer")], by="HouseholdID") %>%
#   left_join(.,empty_rows[,c("HouseholdID","RemoveEmpty")], by="HouseholdID") %>%
#   left_join(.,duplicated_rows[,c("HouseholdID","RemoveDuplicate")], by="HouseholdID") %>%
#   dplyr::filter(.,is.na(RemoveEmpty) & (RemoveDuplicate=="CHECK" | is.na(RemoveDuplicate)))
# 
# DEMOGRAPHIC_joined <-
#   left_join(DEMOGRAPHIC.QAQC, WELLBEING.QAQC[,c("HouseholdID","SettlementID","KK.Code","Respondent")],by="HouseholdID")
# 
# duplicated_demos_rows <-
#   DEMOGRAPHIC_joined %>%
#   dplyr::group_by(IndividualName,Respondent,SettlementID,KK.Code) %>%
#   dplyr::summarise(NumInd=length(DemographicID)) %>%
#   dplyr::filter(.,NumInd>1) %>%
#   dplyr::mutate(DemographicID.1=DEMOGRAPHIC_joined$DemographicID[which(DEMOGRAPHIC_joined$IndividualName==IndividualName & 
#                                                                          DEMOGRAPHIC_joined$Respondent==Respondent &
#                                                                          DEMOGRAPHIC_joined$SettlementID==SettlementID &
#                                                                          DEMOGRAPHIC_joined$KK.Code==KK.Code)][1],
#                 DemographicID.2=DEMOGRAPHIC_joined$DemographicID[which(DEMOGRAPHIC_joined$IndividualName==IndividualName & 
#                                                                   DEMOGRAPHIC_joined$Respondent==Respondent &
#                                                                   DEMOGRAPHIC_joined$SettlementID==SettlementID &
#                                                                   DEMOGRAPHIC_joined$KK.Code==KK.Code)][2],
#                 DemographicID.3=ifelse(NumInd>2,
#                                        DEMOGRAPHIC_joined$DemographicID[which(DEMOGRAPHIC_joined$IndividualName==IndividualName & 
#                                                                                 DEMOGRAPHIC_joined$Respondent==Respondent &
#                                                                                 DEMOGRAPHIC_joined$SettlementID==SettlementID &
#                                                                                 DEMOGRAPHIC_joined$KK.Code==KK.Code)][3],
#                                        NA),
#                 HouseholdID.1=DEMOGRAPHIC_joined$HouseholdID[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.1)],
#                 HouseholdID.2=DEMOGRAPHIC_joined$HouseholdID[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.2)],
#                 HouseholdID.3=ifelse(NumInd>2,
#                                      DEMOGRAPHIC_joined$HouseholdID[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.3)],
#                                      NA),
#          Demographic.1.2=ifelse(nrow(data.frame(A=as.character(DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.1),
#                                                                                     -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))]==
#                                                                  DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.2),
#                                                                                       -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))])) %>%
#                                        filter(A==FALSE))>0,
#                                 "CHECK",
#                                 "YES"),
#          Demographic.1.3=ifelse(is.na(DemographicID.3), 
#                                 NA,
#                                 ifelse(nrow(data.frame(A=as.character(DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.1),
#                                                                                   -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))]==
#                                                                         DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.3),
#                                                                                     -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))])) %>%
#                                               filter(A==FALSE))>0,
#                                        "CHECK",
#                                        "YES")),
#          Demographic.2.3=ifelse(is.na(DemographicID.3), 
#                                 NA,
#                                 ifelse(nrow(data.frame(A=as.character(DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.2),
#                                                                                   -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))]==
#                                                                         DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.3),
#                                                                                     -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))])) %>%
#                                               filter(A==FALSE))>0,
#                                        "CHECK",
#                                        "YES"))) %>%
#   data.frame(HouseholdID=c(.$HouseholdID.2,.$HouseholdID.3),
#              RemoveDuplicateDemos=c(ifelse(.$Demographic.1.2=="YES","YES","CHECK"),
#                                ifelse(is.na(.$HouseholdID.3)==T,
#                                       NA,
#                                       ifelse(.$Demographic.1.3=="YES" | .$Demographic.2.3=="YES",
#                                              "YES",
#                                              "CHECK"))),
#              CheckAgainst=c(ifelse(.$Demographic.1.2!="YES",.$HouseholdID.1,NA),
#                             ifelse(.$Demographic.1.3!="YES" & .$Demographic.2.3!="YES" & .$Demographic.1.2!="YES",paste(.$HouseholdID.1,.$HouseholdID.2,sep=","),
#                                    ifelse(.$Demographic.1.3!="YES" & .$Demographic.2.3!="YES" &.$Demographic.1.2=="YES",
#                                           .$HouseholdID.1,
#                                           ifelse(.$Demographic.1.3!="YES" & .$Demographic.2.3=="YES",
#                                                  .$HouseholdID.1,
#                                                  ifelse(.$Demographic.1.3=="YES" & .$Demographic.2.3!="YES",
#                                                         .$HouseholdID.2,
#                                                         NA)))))) %>%
#   group_by(HouseholdID,RemoveDuplicateDemos,CheckAgainst) %>%
#   summarise(NumInd=length(HouseholdID)) %>%
#   filter(.,!is.na(HouseholdID))
# 
# duplicates <- 
#   full_join(duplicated_rows[!is.na(duplicated_rows$HouseholdID),
#                             c("HouseholdID","RemoveDuplicate","CheckAgainst")],
#             duplicated_demos_rows,by=c("HouseholdID","CheckAgainst"))
# 
# DEMOGRAPHIC_appended <-
#   left_join(DEMOGRAPHIC.QAQC,duplicates,by="HouseholdID") %>%
#   filter(.,RemoveDuplicate=="CHECK" | RemoveDuplicateDemos=="CHECK" | (is.na(RemoveDuplicate) & is.na(RemoveDuplicateDemos)))



# this provides info on intra-HH non-unique combos of HHID and DemographicCode (coding errors)
DEMOGRAPHIC.QAQC %>%
  group_by(HouseholdID,DemographicCode) %>%
  summarise(NumInd=length(DemographicID)) %>%
  filter(.,NumInd>1)

  
# manual checks for repeated names, in case the enumerator began a survey form and then had to restart.  
# copy/paste these output dataframes into the "LogicErrors_ROutput" tab of the QAQC report
repeat.names.wellbeing <-
  WELLBEING.QAQC %>%
  group_by(Respondent) %>%
  summarise(NumHH=length(HouseholdID)) %>%
  filter(NumHH>1)

repeat.names.demographic <-
  DEMOGRAPHIC.QAQC %>%
  group_by(IndividualName) %>%
  summarise(NumInd=length(DemographicID)) %>%
  filter(NumInd>1)




# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: COMPLETENESS ASSESSMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   

# Check for blank cells & missing data blind codes 
# --- need to output number of blank cells from appended data frames for every column



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: CORRECTNESS ASSESSMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   

# Check for coding errors
# - Identification variables
    # YEAR
# - Household characteristics
    # NAME (some individual names or birth/death names end up repeating within the same cell)
# - Fishing characteristics
# - Health
# - Political empowerment
# Check for outliers
# Inlier analysis


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: IMPLEMENTATION ASSESSMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   

# Check for logic errors
# - Skipped questions
#   -- 
# - Logical coherence
#   -- One HHH per HH
  
#   -- If no children, cFS questions should be skipped
#   -- Years resident should be no greater than individual age
# - Calculate combined assets columns
# - Calculate NumLocalThreat


# copy/paste these two dataframe outputs to the "LogicErrors_ROutput" tab in the QAQC report.

# outputs the number of households surveyed per settlement, for a manual cross-walk between the sampling design and the actual numbers on the ground.
num.HH.per.sett <-
  WELLBEING.QAQC %>%
  group_by(SettlementID) %>%
  summarise(NumHH=length(HouseholdID))

# currently, the logic errors dataframe identifies errors in logic relating to number of head of household in a single house (if there are 0, or more than 1), and also if there are
# discrepancies between the age of the oldest resident in the household and the number of years resident.  Typically, we will manually adjust the number of years resident to the 
# oldest household member if the number of years resident listed was greater than the oldest resident.  This still signifies a lifelong resident.  

# could eventually add more logic error columns here, to check for things like births listed (numbirth in the wellbeing table vs. if there is an observation recorded in the births table, for instance), 
# or if the child food security questions were only answered by those who have children listed in the demographic table.  Though, some of these errors may now be impossible due to the logic of the ODK survey form.
logic_errors <-
  DEMOGRAPHIC.QAQC %>%
  group_by(HouseholdID) %>%
  summarise(OldestHHMember=max(IndividualAge, na.rm=T),
            NumberOldHHMember=length(DemographicID[IndividualAge==OldestHHMember]),
            DemographicID.OldestHHMember=DemographicID[IndividualAge==OldestHHMember][1],
            IndividualAge=IndividualAge[DemographicID==DemographicID.OldestHHMember],
            NumHeadHH=length(DemographicID[RelationHHH==0])) %>%
  left_join(WELLBEING.QAQC, ., by="HouseholdID") %>%
  transmute(HouseholdID=HouseholdID,
            OldestHHMember=IndividualAge,
            YearsResident=YearsResident,
            YearResident.v.Age=ifelse(YearsResident>IndividualAge,"Logic error","NONE"),
            NumHeadHH=NumHeadHH,
            NumHeadHH.over1=ifelse(NumHeadHH>1,"Logic error","NONE"),
            NumHeadHH.zero=ifelse(NumHeadHH==0,"Logic error","NONE")) %>%
  filter(YearResident.v.Age=="Logic error" | NumHeadHH.over1=="Logic error" | NumHeadHH.zero=="Logic error")  
  
  
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: INSTRUMENT UTILITY ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   

# # average length of survey -- minimum, maximum, median? Compare to results in some way?  
# 
# interview_time <- WELLBEING_appended[,c("HouseholdID","InterviewStart","InterviewEnd")] %>%
#   mutate(InterviewStart=as.character(.$InterviewStart),
#          InterviewEnd=as.character(.$InterviewEnd),
#          InterviewStart.dec=sapply(strsplit(InterviewStart,":"),
#                                function(x) {
#                                  x <- as.numeric(x)
#                                  x[1]+(x[2]/60)
#                                }),
#          InterviewEnd.dec=sapply(strsplit(InterviewEnd,":"),
#                                  function(x) {
#                                    x <- as.numeric(x)
#                                    x[1]+(x[2]/60)
#                                  }),
#          LengthInterview.dec=InterviewEnd.dec-InterviewStart.dec,
#          LengthInterview.min=LengthInterview.dec*60)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 7: SAMPLING ASSESSMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 8: OUTPUT QAQC REPORT & APPEND DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   
