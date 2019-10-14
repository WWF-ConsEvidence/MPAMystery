# 
# code: Social MPA Mystery Analysis, Bird's Head Seascape
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: October 2017
# 
# ---- inputs ----
# KC_HHData_ForMPAMystery -- in BHS Master Database in Access
#   -this table can be created by running the query saved in the 
#    'KC_CodingSubsettingAnalysis' Group, named 'Q_HHData_ForMPAMystery'.
#   -this table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculated into the food security, material assets, place attachment, marine
#    tenure, and school enrollment indexes -- along with other livelihood and household 
#    demographic variables.
# KC_IndDemos_ForMPAMystery -- in BHS Master Database in Access
#   -the IndoDemos table can be created by running the query saved in the 
#    'KC_CodingSubsettingAnalysis' Group, named 'Q_IndividualDemos_ForMPAMystery'.
#   -the IndDemos table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculate the school enrollment rate, days unwell, and head of household gender. 
# HH_tbl_SETTLEMENT -- in BHS Master Database in Access
#   -the Settlement table provides the names and treatment vs. non-treatment status for 
#    each settlementID.
# !!!!!! TO FIX !!!!!!!!!!!!!! ADD: Ethnicity
# 
# 
# ---- code sections ----
#  1) Import Data, and Subset
#  2) "Big Five" Indexes
#  3) Technical Report Datasets, Proportional Data
#  4) MPA Impact Summary/Technical Report Introduction Stats
#  5) Define Global Plot Themes and Guides 
# 
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Import Data, and Subset ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Import data ----

# FROM FLAT FILE
source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files.R')



pacman::p_load(tidyverse)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: "Big Five" Indexes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Compute indexes ----

HHData$MAIndex <- ifelse(HHData$RemoveMA=="No",
                     rowSums(HHData[,c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                                   "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", 
                                   "Generator")],
                             na.rm=TRUE),
                     NA)

HHData$PAIndex <- ifelse(HHData$RemovePA=="No",
                         round(rowMeans(HHData[,c("PlaceHappy", "PlaceFavourite", "PlaceMiss", "PlaceBest", 
                                                  "PlaceFishHere", "PlaceBeMyself")],
                                        na.rm=TRUE),2),
                         NA)

HHData$MTIndex <- ifelse(HHData$RemoveMT=="No",
                     rowSums(HHData[,c("RightsAccess", "RightsHarvest", "RightsManage", 
                                   "RightsExclude", "RightsTransfer")],
                             na.rm=TRUE),
                     NA)

HHData$FSIndex <- 
  as.character(ifelse(HHData$RemoveFS=="No",
                      rowSums(HHData[,c("DidNotLast", "BalancedDiet", "AdultSkip", "EatLess", 
                                        "FreqAdultSkip", "Hungry")],
                              na.rm=TRUE),
                      NA)) %>%
  recode(., "0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06")

# This line will reverse the scale so that larger values indicate more Food Secure

HHData$FSIndex <- 6.06-as.numeric(HHData$FSIndex) 

HHData <- 
  IndDemos %>%
  group_by(HouseholdID) %>%
           summarise(Household.Size=length(HouseholdID),
                     NumberChild=sum(SchoolAge,na.rm=T),
                     NumberEnrolled=sum(ChildEnrolled,na.rm=T),
                     PercentEnrolled=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                                            as.character(round((NumberEnrolled/NumberChild)*100,2)),
                                            ifelse(NumberChild==0,
                                                   "No School-Aged Children","No Data")),
                     SERate=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                                   round((NumberEnrolled/NumberChild),2),
                                   NA)) %>%
  left_join(HHData,.,by="HouseholdID")


# ---- 2.2 Define "Big Five" data frame - averaged by settlement, for each monitoring year ----

BigFive.SettleGroup <- 
  HHData %>%
  group_by(SettlementID,SettlementName,MPAID,Treatment,MonitoringYear) %>%
  summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
            FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
            MAMean=round(mean(MAIndex,na.rm=T),2),
            MAErr=round(sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),2),
            PAMean=round(mean(PAIndex,na.rm=T),2),
            PAErr=round(sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),2),
            MTMean=round(mean(MTIndex,na.rm=T),2),
            MTErr=round(sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),2),
            SEMean=round(mean(SERate,na.rm=T),2),
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2)) %>%
  na.omit()

#Adding blank-data row to complete the BigFive.SettleGroup dataframe (for settlements that did not have Baseline)
BigFive.SettleGroup <- rbind.data.frame(BigFive.SettleGroup,
                                        data.frame(SettlementID=c(72,104:112,113:115),
                                                   SettlementName=c(as.character(unique(BigFive.SettleGroup$SettlementName[BigFive.SettleGroup$SettlementID==72])),
                                                                    as.character(unique(BigFive.SettleGroup$SettlementName[BigFive.SettleGroup$MPAID==2 &
                                                                                                                             BigFive.SettleGroup$Treatment==1 &
                                                                                                                             BigFive.SettleGroup$SettlementID>100])),
                                                                    as.character(unique(BigFive.SettleGroup$SettlementName[BigFive.SettleGroup$MPAID==3 &
                                                                                                                             BigFive.SettleGroup$SettlementID>110]))),
                                                   MPAID=c(5,rep(2,9),rep(3,3)),
                                                   Treatment=rep(1,13),
                                                   MonitoringYear=rep("Baseline",13),
                                                   as.data.frame(matrix(rep(NA,10),ncol=10,nrow=13,
                                                                        dimnames=list(NULL,colnames(BigFive.SettleGroup[6:15]))))))


# ---- 2.4 Define "Big Five" data frame - averaged by control group (per MPA), for each monitoring year ----

BigFive.ControlGroup <- 
  HHData %>%
  filter(Treatment==0) %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
            FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
            MAMean=round(mean(MAIndex,na.rm=T),2),
            MAErr=round(sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),2),
            PAMean=round(mean(PAIndex,na.rm=T),2),
            PAErr=round(sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),2),
            MTMean=round(mean(MTIndex,na.rm=T),2),
            MTErr=round(sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),2),
            SEMean=round(mean(SERate,na.rm=T),2),
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2)) %>%
  na.omit()

BigFive.ControlGroup <- 
  cbind.data.frame("SettlementID"=rep.int(NA,nrow(unique(BigFive.SettleGroup[c("MPAID","MonitoringYear")]))),
                   "SettlementName"=rep.int("Control",nrow(unique(BigFive.SettleGroup[c("MPAID","MonitoringYear")]))),
                   BigFive.ControlGroup[,"MPAID"],
                   "Treatment"=rep.int(0,nrow(unique(BigFive.SettleGroup[c("MPAID","MonitoringYear")]))),
                   BigFive.ControlGroup[,2:12])

# ---- 2.5 Define "Big Five" data frame - averaged by MPA, for each monitoring year ----

BigFive.MPAGroup <- 
  HHData %>%
  filter(Treatment==1) %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
            FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
            MAMean=round(mean(MAIndex,na.rm=T),2),
            MAErr=round(sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),2),
            PAMean=round(mean(PAIndex,na.rm=T),2),
            PAErr=round(sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),2),
            MTMean=round(mean(MTIndex,na.rm=T),2),
            MTErr=round(sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),2),
            SEMean=round(mean(SERate,na.rm=T),2),
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2)) %>%
  na.omit()

BigFive.MPAGroup <-
  cbind.data.frame("SettlementID"=rep.int(NA,nrow(unique(BigFive.SettleGroup[c("MPAID","MonitoringYear")]))),
                   "SettlementName"=rep.int("MPA",nrow(unique(BigFive.SettleGroup[c("MPAID","MonitoringYear")]))),
                   BigFive.MPAGroup[,"MPAID"],
                   "Treatment"=rep.int(1,nrow(unique(BigFive.SettleGroup[c("MPAID","MonitoringYear")]))),
                   BigFive.MPAGroup[,2:12])

