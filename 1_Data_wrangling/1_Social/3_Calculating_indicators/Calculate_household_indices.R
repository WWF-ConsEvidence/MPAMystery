# 
# code: Calculate household-level indices for MPA social analysis
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: October 2019
# 
# ---- inputs ----
#  1) Source_social_data_for_function.R
# 
# ---- code sections ----
#  1) Compute Indices 
# 
#
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Compute Indices ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Compute indices ----

HHData <-
  HHData %>%
  dplyr::mutate(MAIndex = ifelse(RemoveMA=="No" & (MPAID==17 | # Kei Kecil (MPAID==17) cannot use TV in MAIndex score because question was missing from t3 repeat, 
                                                     MPAID==21), # and Wakatobi (MPAID==21) cannot use TV in MAIndex score because question was missing from t0 monitoring
                                 rowSums(dplyr::select(., "CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard",
                                                       "BoatInboard", "PhoneCombined", "Entertain", "Satellite", "Generator"),
                                         na.rm = TRUE),
                                 ifelse(RemoveMA=="No" & MPAID!=17 & MPAID!=21,
                                        rowSums(dplyr::select(.,"CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard",
                                                              "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", "Generator"),
                                                na.rm = TRUE),
                                        NA)),
                
                PAIndex = ifelse(RemovePA=="No",
                                 round(rowMeans(dplyr::select(.,"PlaceHappy", "PlaceFavourite", "PlaceMiss", "PlaceBest", 
                                                              "PlaceFishHere", "PlaceBeMyself"),
                                                na.rm = TRUE), 2),
                                 NA),
                
                MTIndex = ifelse(RemoveMT=="No",
                                 rowSums(dplyr::select(.,"RightsAccess", "RightsHarvest", "RightsManage", 
                                                       "RightsExclude", "RightsTransfer"),
                                         na.rm = TRUE),
                                 NA),
                
                FSIndex = as.character(ifelse(RemoveFS=="No",
                                              rowSums(dplyr::select(., "DidNotLast", "BalancedDiet", "AdultSkip", "EatLess", 
                                                                    "FreqAdultSkip", "Hungry"),
                                                      na.rm = TRUE),
                                              NA)) %>%
                  dplyr::recode(., "0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06") %>%
                  as.numeric(.),
                
                FSIndex = 6.06 - FSIndex,
                
                cFS = ifelse(RemovecFS=="No",
                             rowSums(dplyr::select(.,"LowCostFood", "ChildBalancedMeal", "ChildNotEnough", 
                                                   "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip", 
                                                   "NoMealChild"),
                                     na.rm = TRUE),
                             NA),
                
                cat.cFS = ifelse(cFS>=6.9,"Evidence",
                                 ifelse(cFS<6.9,"No or insufficient evidence",NA)),
                
                InterviewYear = factor(InterviewYear,
                                       levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018",
                                                "2019","2020","2021","2022","2023","2024","2025","2026","2027",
                                                "2028","2029","2030"),
                                       ordered=T))


HHData <- 
  IndDemos %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(Household.Size=length(HouseholdID),
            NumberChild=sum(SchoolAge,na.rm=T),
            NumberEnrolled=sum(ChildEnrolled,na.rm=T),
            PercentEnrolled=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                                   as.character(round((NumberEnrolled/NumberChild)*100,2)),
                                   ifelse(NumberChild==0,
                                          "No School-Aged Children","No Data")),
            SERate=ifelse(NumberChild!=0 & !is.na(NumberEnrolled),
                          round((NumberEnrolled/NumberChild),2),
                          NA),
            HHHAge=ifelse(length(DemographicID[RelationHHH==0])==1, 
                                 IndividualAge[RelationHHH==0 & !is.na(RelationHHH)],
                                 NA),
            HHHGender=ifelse(length(DemographicID[RelationHHH==0])==1,
                                    IndividualGender[RelationHHH==0 & !is.na(RelationHHH)],
                                    NA),
            HHHEducation=as.character(ifelse(length(DemographicID[RelationHHH==0])==1,
                                IndividualEducation[RelationHHH==0 & !is.na(RelationHHH)],
                                NA)),
            DaysUnwell=sum(DaysUnwell,na.rm=T)/length(HouseholdID)) %>%
  left_join(HHData,.,by="HouseholdID")


HHData <- 
  Organization %>% 
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(NumMarineGroup=length(HouseholdID),
            MarineMeetingSum=ifelse(length(MarineMeeting[is.na(MarineMeeting)==T])==NumMarineGroup,NA,sum(MarineMeeting,na.rm=T)),
            MarineContribution=ifelse(length(MarineContribution[is.na(MarineContribution)==T])==NumMarineGroup,NA,sum(MarineContribution,na.rm=T))) %>%
  left_join(HHData,.,by="HouseholdID")

HHData <- arrange(HHData, HouseholdID)
