# 
# code:  MPA Mystery social data QAQC automation
# 
# github: WWF-ConsEvidence/MPAMystery/
# --- Duplicate all code from 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: February 2019
# modified: 
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

pacman::p_load(dplyr, foreach, reshape2, stringdist)

# ---- 1.2 import data ----

DEMOGRAPHIC <- read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/2_PRE_QAQC_RECOMBINE/1_HWB/KOON_2018_Pre-QAQC_DEMOGRAPHIC.csv',
                        na.strings="#N/A")
WELLBEING <-  read.csv('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/2_PRE_QAQC_RECOMBINE/1_HWB/KOON_2018_Pre-QAQC_WELLBEING.csv',
                       na.strings="#N/A")

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

# -- check for empty rows in both WB and demos tables
# -- check for duplicated rows in both WB and demos tables
# -- check for refused households (this should be easier to check with field sampling sheets)
#     - has an interview start and end time, all identifying features but no data.  
#     - this needs to be recorded as refused in an output table then deleted from database (in both WB and demos tables)
# -- check for incomplete records in both WB and demos tables
#     - this could be two records that were split by mistake

# -- check for coding errors

# HH LEVEL
# ---- empty rows ----
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


# ---- duplicate rows ----
duplicated_rows <- 
  WELLBEING_appended %>% 
  left_join(.,DEMOGRAPHIC %>%
              dplyr::group_by(HouseholdID) %>%
              dplyr::summarise(NumInd=length(DemographicID),
                               IndividualAge.1=IndividualAge[1],
                               IndividualEducation.1=IndividualEducation[1]), by="HouseholdID") %>%
  dplyr::group_by(Respondent,SecondaryRespondent) %>% 
  dplyr::summarise(NumHH=length(HouseholdID)) %>% 
  dplyr::filter(.,NumHH>1) %>%
  dplyr::mutate(HouseholdID.1=WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent & 
                                                     WELLBEING$SecondaryRespondent==SecondaryRespondent)][1],
         HouseholdID.2=WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent & 
                                                     WELLBEING$SecondaryRespondent==SecondaryRespondent)][2],
         HouseholdID.3=ifelse(NumHH>2,WELLBEING$HouseholdID[which(WELLBEING$Respondent==Respondent &
                                                                    WELLBEING$SecondaryRespondent==SecondaryRespondent)][3],
                              NA),
         Household.1.2=ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                               WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
                                     filter(A==FALSE))>0,
                              paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
                                    collapse=", "),
                              "YES"),
         Household.1.3=ifelse(is.na(HouseholdID.3), 
                              NA,
                              ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                      WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
                                            filter(A==FALSE))>0,
                                     paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.1),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                       WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
                                           collapse=", "),
                                     "YES")),
         Household.2.3=ifelse(is.na(HouseholdID.3), 
                              NA,
                              ifelse(nrow(data.frame(A=as.character(WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                      WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])) %>%
                                            filter(A==FALSE))>0,
                                     paste(colnames(WELLBEING[which((WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.2),-which(names(WELLBEING) %in% "HouseholdID")]==
                                                                       WELLBEING[which(WELLBEING$HouseholdID==HouseholdID.3),-which(names(WELLBEING) %in% "HouseholdID")])==FALSE)]),
                                           collapse=", "),
                                     "YES"))) %>%
  data.frame(HouseholdID=c(.$HouseholdID.2,.$HouseholdID.3),
             RemoveDuplicate=c(ifelse(.$Household.1.2=="YES","YES","CHECK"),
                               ifelse(is.na(.$HouseholdID.3)==T,
                                      NA,
                                      ifelse(.$Household.1.3=="YES" | .$Household.2.3=="YES",
                                                      "YES",
                                                      "CHECK"))),
             CheckAgainst=c(ifelse(.$Household.1.2!="YES",.$HouseholdID.1,NA),
                            ifelse(.$Household.1.3!="YES" & .$Household.2.3!="YES" & .$Household.1.2!="YES",paste(.$HouseholdID.1,.$HouseholdID.2,sep=","),
                                   ifelse(.$Household.1.3!="YES" & .$Household.2.3!="YES" &.$Household.1.2=="YES",
                                          .$HouseholdID.1,
                                          ifelse(.$Household.1.3!="YES" & .$Household.2.3=="YES",
                                                 .$HouseholdID.1,
                                                 ifelse(.$Household.1.3=="YES" & .$Household.2.3!="YES",
                                                        .$HouseholdID.2,
                                                        NA))))))

check_duplicates <- 
  duplicated_rows[duplicated_rows$RemoveDuplicate=="CHECK" &
                    !is.na(duplicated_rows$RemoveDuplicate),] %>%
  transmute(HouseholdsToCompare=paste(CheckAgainst,HouseholdID,sep=", "),
            CheckColNames=ifelse(Household.1.2!="YES" & !is.na(Household.1.2),
                                 Household.1.2,
                                 ifelse(Household.1.3!="YES" & !is.na(Household.1.3),
                                        Household.1.3,
                                        ifelse(Household.2.3!="YES" & !is.na(Household.2.3),
                                               Household.2.3,NA))))

# Now need to output pairs that need to be checked, and remove all duplicated households 
# (from all tables -- should I verify that the same households are duplicated in the demos table first???)

# remove all duplicated rows, outputting number of removed rows and household pairs that need to be checked from duplicated analysis
WELLBEING_appended <-
  left_join(WELLBEING_appended,duplicated_rows[,c("HouseholdID","RemoveDuplicate")],by="HouseholdID") %>%
  dplyr::filter(.,RemoveDuplicate=="CHECK" | is.na(RemoveDuplicate)) %>%
  .[,-which(names(.) %in% "RemoveDuplicate")]


# check dataentrycomplete column, then check if any identifiers are blank
# ---will need to output all incomplete rows to a "check" file, along with duplicate items to be checked and all ambiguous rows
incomplete_rows <-
  left_join(WELLBEING_appended,duplicated_rows[,c("HouseholdID","RemoveDuplicate")],by="HouseholdID") %>%
  transmute(HouseholdID=HouseholdID,
            IncompleteCheck=ifelse(DataEntryComplete==FALSE,"CHECK",NA),
            RemoveDuplicate=RemoveDuplicate)

# ---should do this after already removing duplicated rows & checking incomplete rows. Is there a way to 
# create running WELLBEING, DEMOGRAPHIC, etc. data frames with updates as they are made?
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
                StrDist=unlist(foreach(i=1:length(CountryID)) %do% stringdist(Respondent.1[i],Respondent.2[i]))),
                RemoveAmbiguous=ifelse(StrDist>4,NA,
                                       ifelse(nrow(data.frame(A=as.character(WELLBEING_appended[which(WELLBEING_appended$HouseholdID==HouseholdID.1),
                                                                                                   -which(names(WELLBEING_appended) %in% c("HouseholdID","Respondent","SecondaryRespondent"))]==
                                                                                  WELLBEING_appended[which(WELLBEING_appended$HouseholdID==HouseholdID.2),
                                                                                                     -which(names(WELLBEING_appended) %in% c("HouseholdID","Respondent","SecondaryRespondent"))])) %>%
                                                        filter(A==FALSE))>0,
                                                        "CHECK",
                                                        "YES")))


# INDIVIDUAL LEVEL

# remove duplicated households before checking for more duplicated rows in demos data

DEMOGRAPHIC_appended <-
  left_join(DEMOGRAPHIC,WELLBEING[,c("HouseholdID","SettlementID","KK.Code",
                                      "Respondent","SecondaryRespondent",
                                      "PrimaryInterviewer","SecondaryInterviewer")], by="HouseholdID") %>%
  left_join(.,empty_rows[,c("HouseholdID","RemoveEmpty")], by="HouseholdID") %>%
  left_join(.,duplicated_rows[,c("HouseholdID","RemoveDuplicate")], by="HouseholdID") %>%
  dplyr::filter(.,is.na(RemoveEmpty) & (RemoveDuplicate=="CHECK" | is.na(RemoveDuplicate)))

DEMOGRAPHIC_joined <-
  left_join(DEMOGRAPHIC, WELLBEING[,c("HouseholdID","SettlementID","KK.Code","Respondent")],by="HouseholdID")

duplicated_demos_rows <-
  DEMOGRAPHIC_joined %>%
  dplyr::group_by(IndividualName,Respondent,SettlementID,KK.Code) %>%
  dplyr::summarise(NumInd=length(DemographicID)) %>%
  dplyr::filter(.,NumInd>1) %>%
  dplyr::mutate(DemographicID.1=DEMOGRAPHIC_joined$DemographicID[which(DEMOGRAPHIC_joined$IndividualName==IndividualName & 
                                                                         DEMOGRAPHIC_joined$Respondent==Respondent &
                                                                         DEMOGRAPHIC_joined$SettlementID==SettlementID &
                                                                         DEMOGRAPHIC_joined$KK.Code==KK.Code)][1],
                DemographicID.2=DEMOGRAPHIC_joined$DemographicID[which(DEMOGRAPHIC_joined$IndividualName==IndividualName & 
                                                                  DEMOGRAPHIC_joined$Respondent==Respondent &
                                                                  DEMOGRAPHIC_joined$SettlementID==SettlementID &
                                                                  DEMOGRAPHIC_joined$KK.Code==KK.Code)][2],
                DemographicID.3=ifelse(NumInd>2,
                                       DEMOGRAPHIC_joined$DemographicID[which(DEMOGRAPHIC_joined$IndividualName==IndividualName & 
                                                                                DEMOGRAPHIC_joined$Respondent==Respondent &
                                                                                DEMOGRAPHIC_joined$SettlementID==SettlementID &
                                                                                DEMOGRAPHIC_joined$KK.Code==KK.Code)][3],
                                       NA),
                HouseholdID.1=DEMOGRAPHIC_joined$HouseholdID[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.1)],
                HouseholdID.2=DEMOGRAPHIC_joined$HouseholdID[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.2)],
                HouseholdID.3=ifelse(NumInd>2,
                                     DEMOGRAPHIC_joined$HouseholdID[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.3)],
                                     NA),
         Demographic.1.2=ifelse(nrow(data.frame(A=as.character(DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.1),
                                                                                    -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))]==
                                                                 DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.2),
                                                                                      -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))])) %>%
                                       filter(A==FALSE))>0,
                                "CHECK",
                                "YES"),
         Demographic.1.3=ifelse(is.na(DemographicID.3), 
                                NA,
                                ifelse(nrow(data.frame(A=as.character(DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.1),
                                                                                  -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))]==
                                                                        DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.3),
                                                                                    -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))])) %>%
                                              filter(A==FALSE))>0,
                                       "CHECK",
                                       "YES")),
         Demographic.2.3=ifelse(is.na(DemographicID.3), 
                                NA,
                                ifelse(nrow(data.frame(A=as.character(DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.2),
                                                                                  -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))]==
                                                                        DEMOGRAPHIC_joined[which(DEMOGRAPHIC_joined$DemographicID==DemographicID.3),
                                                                                    -which(names(DEMOGRAPHIC_joined) %in% c("HouseholdID","DemographicID"))])) %>%
                                              filter(A==FALSE))>0,
                                       "CHECK",
                                       "YES"))) %>%
  data.frame(HouseholdID=c(.$HouseholdID.2,.$HouseholdID.3),
             RemoveDuplicateDemos=c(ifelse(.$Demographic.1.2=="YES","YES","CHECK"),
                               ifelse(is.na(.$HouseholdID.3)==T,
                                      NA,
                                      ifelse(.$Demographic.1.3=="YES" | .$Demographic.2.3=="YES",
                                             "YES",
                                             "CHECK"))),
             CheckAgainst=c(ifelse(.$Demographic.1.2!="YES",.$HouseholdID.1,NA),
                            ifelse(.$Demographic.1.3!="YES" & .$Demographic.2.3!="YES" & .$Demographic.1.2!="YES",paste(.$HouseholdID.1,.$HouseholdID.2,sep=","),
                                   ifelse(.$Demographic.1.3!="YES" & .$Demographic.2.3!="YES" &.$Demographic.1.2=="YES",
                                          .$HouseholdID.1,
                                          ifelse(.$Demographic.1.3!="YES" & .$Demographic.2.3=="YES",
                                                 .$HouseholdID.1,
                                                 ifelse(.$Demographic.1.3=="YES" & .$Demographic.2.3!="YES",
                                                        .$HouseholdID.2,
                                                        NA)))))) %>%
  group_by(HouseholdID,RemoveDuplicateDemos,CheckAgainst) %>%
  summarise(NumInd=length(HouseholdID)) %>%
  filter(.,!is.na(HouseholdID))

duplicates <- 
  full_join(duplicated_rows[!is.na(duplicated_rows$HouseholdID),
                            c("HouseholdID","RemoveDuplicate","CheckAgainst")],
            duplicated_demos_rows,by=c("HouseholdID","CheckAgainst"))

DEMOGRAPHIC_appended <-
  left_join(DEMOGRAPHIC,duplicates,by="HouseholdID") %>%
  filter(.,RemoveDuplicate=="CHECK" | RemoveDuplicateDemos=="CHECK" | (is.na(RemoveDuplicate) & is.na(RemoveDuplicateDemos)))

  # this provides info on intra-HH non-unique combos of HHID and DemographicCode (coding errors)
  DEMOGRAPHIC %>%
  group_by(HouseholdID,DemographicCode) %>%
  summarise(NumInd=length(DemographicID)) %>%
  filter(.,NumInd>1)


# will need to remove all duplicated households, but note which householdIDs were removed in an output Excel file
# for those households that need to be checked, will output to the same Excel file that they must be manually checked.  
# then, will manually enter what action was taken for the manually checked ones



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

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: INSTRUMENT UTILITY ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   

# average length of survey -- minimum, maximum, median? Compare to results in some way?  

interview_time <- WELLBEING_appended[,c("HouseholdID","InterviewStart","InterviewEnd")] %>%
  mutate(InterviewStart=as.character(.$InterviewStart),
         InterviewEnd=as.character(.$InterviewEnd),
         InterviewStart.dec=sapply(strsplit(InterviewStart,":"),
                               function(x) {
                                 x <- as.numeric(x)
                                 x[1]+(x[2]/60)
                               }),
         InterviewEnd.dec=sapply(strsplit(InterviewEnd,":"),
                                 function(x) {
                                   x <- as.numeric(x)
                                   x[1]+(x[2]/60)
                                 }),
         LengthInterview.dec=InterviewEnd.dec-InterviewStart.dec,
         LengthInterview.min=LengthInterview.dec*60)


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
