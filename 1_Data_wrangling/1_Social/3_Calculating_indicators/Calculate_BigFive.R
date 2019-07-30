# 
# code: Social MPA Mystery Analysis, Bird's Head Seascape
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: October 2017
# 
# ---- inputs ----
# source_social_data R script
# 
# 
# ---- code sections ----
#  1) "Big Five" Indexes
#  2) Derive Additional Variables
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: "Big Five" Indexes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Compute indexes ----

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
  summarise(NumberChild=sum(SchoolAge,na.rm=T),
            NumberEnrolled=sum(ChildEnrolled,na.rm=T),
            PercentEnrolled=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                                   as.character(round((NumberEnrolled/NumberChild)*100,2)),
                                   ifelse(NumberChild==0,
                                          "No School-Aged Children","No Data")),
            SERate=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                          round((NumberEnrolled/NumberChild),2),
                          NA)) %>%
  left_join(HHData,.,by="HouseholdID")
  

# ---- 1.2 Define "Big Five" data frame - averaged by settlement, for each monitoring year ----

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
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2))


# ---- 1.3 Define "Big Five" data frame - averaged by control group (per MPA), for each monitoring year ----

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
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2))


# ---- 1.4 Define "Big Five" data frame - averaged by MPA, for each monitoring year ----

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
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Derive Additional Variables ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
HHData$cFS

# ---- 2.1 Child food security ----

#Summing child food variables to construct child food security index on the household level
HHData$cFS <-
  ifelse(HHData$RemoveFS=="No",
                     rowSums(HHData[,c("LowCostFood", "ChildBalancedMeal", "ChildNotEnough", 
                                       "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip", 
                                       "NoMealChild")],
                             na.rm=TRUE),
                     NA)

#Adding a column that classifies whether they meet the standard for child food insecurity 
HHData$cat.cFS <- ifelse(HHData$cFS>=6.9,"Evidence",
                         ifelse(HHData$cFS<6.9,"No or insufficient evidence",NA))


# ---- 2.2 Additional variables & data fixes ----

# Subsetting table to just include the household heads, removing NA's in the relation to household head column, 
#  adding a fisher dummy variable, calculating HH size, days unwell per individual
HHData <- 
  right_join(HHData,IndDemos[IndDemos$RelationHHH==0 &
                                       !is.na(IndDemos$RelationHHH),c("HouseholdID","IndividualGender",
                                                                      "IndividualEducation","IndividualAge")],by="HouseholdID") %>%
  mutate(InterviewYear=factor(InterviewYear,
                              levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018",
                                       "2019","2020","2021","2022","2023","2024","2025","2026","2027",
                                       "2028","2029","2030"),
                              ordered=T),
         HHsize=sapply(HouseholdID,
                       function(i){
                         c(length(IndDemos$HouseholdID[which(IndDemos$HouseholdID==i)]))
                       }),
         fisher=ifelse(PrimaryLivelihood==3 | SecondaryLivelihood==3 | TertiaryLivelihood==3,1,0)) %>%
  left_join(.,(IndDemos %>% group_by(HouseholdID) %>% summarise(DaysUnwell=sum(DaysUnwell,na.rm=T)/length(HouseholdID))), by="HouseholdID")
