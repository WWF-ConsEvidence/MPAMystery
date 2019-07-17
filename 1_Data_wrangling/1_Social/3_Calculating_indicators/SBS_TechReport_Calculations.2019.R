# 
# code: Social MPA Mystery Analysis, Sunda Banda Seascape
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: January 2018
# 
# modified by Amari: June 2019
# Alor and Flotim
#
# ---- inputs ----
# -- Dependencies: Source_social_data_flat_files.R
#
# SBS_Repeated_HHdata.csv 
# SBS_Repeated_IndDemos.csv 
# SBS_Repeated_Organization.csv
# 
# ---- outputs ----
# Techreport.ByMPA.contol
# Techreport.Status.BySett
# Techreport.Trend.ByMPA
# AgeGender.AvgAge.byMPA
# AgeGender.AvgAge.bySett
# AgeGender.AvgAge.control
#
#
# ---- code sections ----
#  1) Import Data, and Subset
#  2) "Big Five" Indices
#  3) Technical Report Datasets, Proportional Data
#  4) Define Global plot themes and guides

# ---- 1.0 Load libraries and data ----
pacman::p_load(plyr,tidyverse,reldist,Kendall,reshape2,gridExtra)

library(tidyverse)
library(Kendall)
library(reldist)
library(reshape2)
library(gridExtra)

HHData<-read.csv("SBS_Repeated_HHdata.csv")
IndDemos<-read.csv("SBS_Repeated_IndDemos.csv")
Organization<-read.csv("SBS_Repeated_Organization.csv")


# ---- 1.1 Subset variables to Alor and Flotim MPA's, then subset from HHData for Big Five calculations & descriptive statistics ----
HHData <- HHData %>%
  filter(MPAID < 17 & MPAID >14)

IndDemos <- IndDemos %>%
  filter(MPAID < 17 & MPAID >14)

Organization <- Organization %>%
  filter(MPAID < 17 & MPAID >14)

# Fix improper capitalization
HHData$SettlementName<-gsub("Alila TImur","Alila Timur",HHData$SettlementName)

FS <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemoveFS", 
                "DidNotLast", "BalancedDiet", "AdultSkip", "EatLess", 
                "FreqAdultSkip", "Hungry","MonitoringYear")]


MA <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemoveMA", 
                "CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", "Generator","MonitoringYear")]


PA <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemovePA", "PlaceHappy",
                "PlaceFavourite", "PlaceMiss", "PlaceBest", "PlaceFishHere", "PlaceBeMyself","MonitoringYear")]


MT <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear",  "RemoveMT", "RightsAccess", "RightsHarvest", 
                "RightsManage", "RightsExclude", "RightsTransfer","MonitoringYear")]

HHDemos <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "Religion", "YrResident", 
                      "MonitoringYear","Treatment","PaternalEthnicity")]


HeadOfHH <- IndDemos[IndDemos$RelationHHH==0 &
                       !is.na(IndDemos$RelationHHH), c("HouseholdID", "IndividualGender", "IndividualEducation", 
                                                       "IndividualAge")]

# ---- 2.0 Calculating BigFive ----

MA$MAIndex <- ifelse(MA$RemoveMA=="No",
                     rowSums(MA[,c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                                   "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", 
                                   "Generator")],
                             na.rm=TRUE),
                     NA)

PA$PAIndex <- ifelse(PA$RemovePA=="No",
                     round(rowMeans(PA[,c("PlaceHappy", "PlaceFavourite", "PlaceMiss", "PlaceBest", 
                                          "PlaceFishHere", "PlaceBeMyself")],
                                    na.rm=TRUE),2),
                     NA)
MT$MTIndex <- ifelse(MT$RemoveMT=="No",
                     rowSums(MT[,c("RightsAccess", "RightsHarvest", "RightsManage", 
                                   "RightsExclude", "RightsTransfer")],
                             na.rm=TRUE),
                     NA)

FS$FSIndex <- as.character(ifelse(FS$RemoveFS=="No",
                                  rowSums(FS[,c("DidNotLast", "BalancedDiet", "AdultSkip", "EatLess", 
                                                "FreqAdultSkip", "Hungry")],
                                          na.rm=TRUE),
                                  NA))
FS$FSIndex <- revalue(FS$FSIndex, c("0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06"))
FS$FSIndex <- 6.06-as.numeric(FS$FSIndex)

MA.PA.MT.FS <- left_join(MA,PA)
MA.PA.MT.FS <- left_join(MA.PA.MT.FS,MT)
MA.PA.MT.FS <- left_join(MA.PA.MT.FS,FS)

IndDemos.1 <- IndDemos %>%
  group_by(HouseholdID) %>%
  summarise(NumberChild = sum(SchoolAge,na.rm=T),
            EnrolledHH=sum(ChildEnrolled,na.rm=T))

IndDemos.1 <- IndDemos.1 %>%
  mutate(PercentEnrolled = ifelse(NumberChild !=0 & !is.na(EnrolledHH),
                                   as.character(round((EnrolledHH/NumberChild)*100,2)),
                                   ifelse(NumberChild==0,
                                          "No School-Aged Children","No Data")))

BigFive.allvar <- left_join(MA.PA.MT.FS,IndDemos.1)
BigFive.allvar <- left_join(BigFive.allvar,HHData[,c("HouseholdID","MonitoringYear","Treatment","SettlementName")])


# ---- 2.1 Define "Big Five" data frame - at household level, all MPAs, all years ----

BigFive <- data.frame(BigFive.allvar[,c("HouseholdID","MPAID","SettlementID",
                                        "InterviewYear","MonitoringYear","MAIndex",
                                        "FSIndex","PAIndex","MTIndex","NumberChild","EnrolledHH", "Treatment",
                                        "SettlementName","PercentEnrolled")])
colnames(BigFive) <- c(colnames(BigFive[c("HouseholdID", "MPAID", "SettlementID", 
                                          "InterviewYear", "MonitoringYear", "MAIndex", "FSIndex", "PAIndex", 
                                          "MTIndex", "NumberChild", "EnrolledHH","Treatment","SettlementName")]),"SERate")
BigFive$SERate <- ifelse(BigFive$SERate=="No Data" |
                           BigFive$SERate=="No School-Aged Children",NA,
                         as.numeric(BigFive$SERate)/100)

BigFive.bySett <- 
  BigFive %>% 
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



# ---- 2.2 Define "Big Five" data frame - averaged by control group (per MPA), for each monitoring year ----


BigFive.ControlGroup <- 
  BigFive[BigFive$Treatment==0,] %>%
  filter(MonitoringYear=="3 Year Post") %>%
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

BigFive.ControlGroup <- na.omit(BigFive.ControlGroup)

BigFive.ControlGroup.annex <- 
  BigFive[BigFive$Treatment==0,] %>%
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

BigFive.ControlGroup.annex <- na.omit(BigFive.ControlGroup.annex)



# ---- 2.3 Define "Big Five" data frame - averaged by MPA, for each monitoring year ----

BigFive.MPAGroup <- 
  BigFive[BigFive$Treatment==1,] %>%
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

BigFive.MPAGroup <- na.omit(BigFive.MPAGroup)


# ----  3.0 Technical Report Datasets, Proportional Data ----


cFS <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "MonitoringYear", "RemoveFS", 
                 "LowCostFood", "ChildBalancedMeal", "ChildNotEnough", 
                 "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip", 
                 "NoMealChild")]
cFS$cFS <- ifelse(cFS$RemoveFS=="No",
                  rowSums(cFS[,c("LowCostFood", "ChildBalancedMeal", "ChildNotEnough", 
                                 "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip", 
                                 "NoMealChild")],
                          na.rm=TRUE),
                  NA)
cFS$cat.cFS <- ifelse(cFS$cFS>=6.9,"Evidence",
                      ifelse(cFS$cFS<6.9,"No or insufficient evidence",NA))

MPANames <- data.frame(MPAID=c(15:19),
                       MPAName=c("Alor","Flotim","Kei","Koon","Yamdena"))

HHDemos.context.1 <- left_join(HHData[,c("HouseholdID", "MPAID", "SettlementID", "Religion", "YrResident", 
                                          "MonitoringYear","Treatment","SettlementName","PaternalEthnicity","PrimaryLivelihood", "SecondaryLivelihood", 
                                         "TertiaryLivelihood", "FreqFish", "FreqSaleFish", 
                                         "PercentIncFish", "MajFishTechnique", "FreqEatFish", 
                                         "PercentProteinFish", "TimeMarket")],
                               MPANames,
                               by="MPAID")
HHDemos.context.1 <- left_join(HHDemos.context.1,
                               HeadOfHH,
                               by="HouseholdID")

HHDemos.context.1 <- left_join(HHDemos.context.1,
                               BigFive[,c("HouseholdID", "InterviewYear", 
                                          "SERate")],
                               by="HouseholdID")

HHDemos.context.1 <- left_join(HHDemos.context.1,
                               cFS[,c("HouseholdID", "cat.cFS")],
                               by="HouseholdID")

HHDemos.context.1$InterviewYear <- factor(HHDemos.context.1$InterviewYear,
                                          levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
                                          ordered=T)
HHDemos.context.1$HHsize <- sapply(HHDemos.context.1$HouseholdID,
                                   function(i){
                                     c(length(IndDemos$HouseholdID[which(IndDemos$HouseholdID==i)]))
                                   })
HHDemos.context <- HHDemos.context.1[HHDemos.context.1$Treatment==1,]

MPA.currentyear <- group_by(HHDemos.context.1,MPAID) 
MPA.currentyear <- summarise(MPA.currentyear,
                             CurrentYear=max(InterviewYear))
MPA.currentyear$CurrentYear <- factor(MPA.currentyear$CurrentYear,
                                      levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
                                      ordered=T)

CurrentDemos.context <- left_join(HHDemos.context.1,MPA.currentyear)
CurrentDemos.context <- CurrentDemos.context[CurrentDemos.context$InterviewYear==CurrentDemos.context$CurrentYear,]

MPA.baseline<-group_by(HHDemos.context.1,MPAID)
MPA.baseline<-summarise(MPA.baseline,
                         baseline = min(InterviewYear))
MPA.baseline$baseline <- factor(MPA.baseline$baseline,
                                      levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
                                      ordered=T)
BaselineDemos.context <- left_join(HHDemos.context.1,MPA.baseline)
BaselineDemos.context <- BaselineDemos.context[BaselineDemos.context$InterviewYear==BaselineDemos.context$baseline,]

CurrentDemos.control <- left_join(HHDemos.context.1,MPA.currentyear)
CurrentDemos.control <- CurrentDemos.control[CurrentDemos.control$InterviewYear==CurrentDemos.control$CurrentYear &
                                               CurrentDemos.control$Treatment==0,]

AnnexDemos.control <- HHDemos.context.1[HHDemos.context.1$Treatment==0,]


# ---- 3.1 Settlement-level analysis, for status and annex plots ----


Techreport.Status.BySett <- 
  HHDemos.context %>%
  group_by(SettlementID,MonitoringYear,Treatment) %>%
  summarise(MPAID=unique(MPAID),
            SettlementName=unique(SettlementName),
            HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                  !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
            HHH.male=(length(IndividualGender[IndividualGender==1 &
                                                !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
            Percent.Rel.Christian=(length(Religion[Religion==1 &
                                                     !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
            Percent.Rel.Muslim=(length(Religion[Religion==2 &
                                                  !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
            Percent.Rel.Other=(length(Religion[Religion!=1 & Religion!=2 &
                                                 !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
            Percent.PrimaryOcc.Fish=(length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                                !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Farm=(length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                                !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                                     !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                         !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                                   !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihood[PrimaryLivelihood==4 &
                                                                            !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihood[PrimaryLivelihood==5 &
                                                                      !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Other=(length(PrimaryLivelihood[PrimaryLivelihood==996 & !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.IncFish.None=(length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Some=(length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Half=(length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Most=(length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.All=(length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Child.FS.no=(length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
            Child.FS.yes=(length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
            Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                              !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
            Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                 !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
            Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
            Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
            Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                 !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
            ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                          !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
            ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                          !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
            ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                          !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
            ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                          !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
            ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                         !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
            SERate=mean(SERate,na.rm=T),
            TimeMarketMean=mean(TimeMarket,na.rm=T),
            TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)))

Techreport.Status.BySett <- Techreport.Status.BySett[!is.na(Techreport.Status.BySett$SettlementID),]

#  ---- 3.2 MPA-level analysis, for trend plots ----

Techreport.Trend.ByMPA <- 
  HHDemos.context %>%
  group_by(MPAID,MonitoringYear,Treatment) %>%
  summarise(HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                 !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
            HHH.male=(length(IndividualGender[IndividualGender==1 &
                                               !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
            Percent.Rel.Christian=(length(Religion[Religion==1 &
                                                          !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
            Percent.Rel.Muslim=(length(Religion[Religion==2 &
                                                       !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
            Percent.Rel.Other=(length(Religion[Religion!=1 & Religion!=2 &
                                                      !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
            Percent.PrimaryOcc.Fish=(length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                                     !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Farm=(length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                                     !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                                          !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                              !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                                        !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihood[PrimaryLivelihood==4 &
                                                                            !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihood[PrimaryLivelihood==5 &
                                                                           !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Other=(length(PrimaryLivelihood[PrimaryLivelihood==996 & !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
            Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
            Prop.IncFish.None=(length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Some=(length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Half=(length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Most=(length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.All=(length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
            Child.FS.no=(length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
            Child.FS.yes=(length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100, 
            Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                                          !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                        Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                             !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                        Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                            !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                        Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                            !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                        Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                             !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                        ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                                      !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                        ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                                      !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                        ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                                      !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                        ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                                      !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                        ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                                     !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100, 
            SERate=mean(SERate,na.rm=T),
            TimeMarketMean=mean(TimeMarket,na.rm=T),
            TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)))

Techreport.Trend.ByMPA <- Techreport.Trend.ByMPA[!is.na(Techreport.Trend.ByMPA$MPAID),]

# --- 3.3 COMPARE MPAS TO CONTROLS, for most recent monitoring year 
#     (not used in any significance tests or data frames, just for comparing proportional variables by sight)
Techreport.ByMPA.control <- group_by(CurrentDemos.control,MPAID,Treatment)
Techreport.ByMPA.control <- summarise(Techreport.ByMPA.control,
                                      HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                                           !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
                                      HHH.male=(length(IndividualGender[IndividualGender==1 &
                                                                         !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
                                      Percent.Rel.Christian=(length(Religion[Religion==1 &
                                                                                    !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                                      Percent.Rel.Muslim=(length(Religion[Religion==2 &
                                                                                 !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                                      Percent.Rel.Other=(length(Religion[Religion!=1 & Religion!=2 &
                                                                                !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                                      Percent.PrimaryOcc.Fish=(length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                                                               !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Farm=(length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                                                               !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                                                                    !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                                                        !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                                                                  !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihood[PrimaryLivelihood==4 &
                                                                                                      !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihood[PrimaryLivelihood==5 &
                                                                                                     !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Other=(length(PrimaryLivelihood[PrimaryLivelihood==996 & !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.IncFish.None=(length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.Some=(length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.Half=(length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.Most=(length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.All=(length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Child.FS.no=(length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                                      Child.FS.yes=(length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                                      Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                                                                  !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                                Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                                                     !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                                Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                                                    !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                                Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                                                    !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                                Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                                                     !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                                ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                                                              !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                                ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                                                              !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                                ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                                                              !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                                ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                                                              !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                                ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                                                             !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                      SERate=mean(SERate,na.rm=T),
                                      TimeMarketMean=mean(TimeMarket,na.rm=T),
                                      TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)))

# Comparing control settlements over time for annex plots
Techreport.ByMPA.control.annex <- group_by(AnnexDemos.control,MPAID,Treatment,MonitoringYear)
Techreport.ByMPA.control.annex <- summarise(Techreport.ByMPA.control.annex,
                                      HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                                            !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
                                      HHH.male=(length(IndividualGender[IndividualGender==1 &
                                                                          !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
                                      Percent.Rel.Christian=(length(Religion[Religion==1 &
                                                                               !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                                      Percent.Rel.Muslim=(length(Religion[Religion==2 &
                                                                            !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                                      Percent.Rel.Other=(length(Religion[Religion!=1 & Religion!=2 &
                                                                           !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                                      Percent.PrimaryOcc.Fish=(length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                                                          !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Farm=(length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                                                          !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                                                               !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                                                   !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                                                             !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihood[PrimaryLivelihood==4 &
                                                                                                 !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihood[PrimaryLivelihood==5 &
                                                                                                !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Percent.PrimaryOcc.Other=(length(PrimaryLivelihood[PrimaryLivelihood==996 & !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                                      Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                                      Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                                      Prop.IncFish.None=(length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.Some=(length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.Half=(length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.Most=(length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.IncFish.All=(length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
                                      Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                                      Child.FS.no=(length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                                      Child.FS.yes=(length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                                      Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                                                        !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                      Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                                           !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                      Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                                          !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                      Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                                          !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                      Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                                           !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                                      ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                                                    !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                      ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                                                    !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                      ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                                                    !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                      ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                                                    !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                      ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                                                   !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                                      SERate=mean(SERate,na.rm=T),
                                      TimeMarketMean=mean(TimeMarket,na.rm=T),
                                      TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)))



# - Number years resident by settlement, to look for signs of rapid immigration 
# - Changes in social conflict by settlement
# - Marine tenure components by settlement

HHDemos.context$InterviewYear <- as.integer(HHDemos.context$InterviewYear)


Synth.techreport.bySett <-
  left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict","PaternalEthnicity","Treatment")]) %>%
  left_join(MT[,c("HouseholdID","RightsAccess","RightsHarvest","RightsManage","RightsExclude","RightsTransfer","MTIndex")]) %>%
  left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex","PAIndex")]) %>%
  group_by(SettlementID,MPAID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            YrResident=mean(YrResident,na.rm=T),
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
                                                                  !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            MTManage=mean(RightsManage,na.rm=T),
            MTHarvest=mean(RightsHarvest,na.rm=T),
            MTAccess=mean(RightsAccess,na.rm=T),
            MTTransfer=mean(RightsTransfer,na.rm=T),
            MTExclude=mean(RightsExclude,na.rm=T),
            MatAssets.gini=gini(MAIndex),
            MAIndex=mean(MAIndex,na.rm=T),
            Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100
            )
Synth.techreport.bySett <-left_join(Synth.techreport.bySett,BigFive.bySett)

# - same data as above, by MPA
Synth.techreport.byMPA <-
  left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict")]) %>%
  left_join(MT[,c("HouseholdID","RightsAccess","RightsHarvest","RightsManage","RightsExclude","RightsTransfer")]) %>%
  left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex","MTIndex","PAIndex")]) %>%
  group_by(MPAID,MonitoringYear,Treatment) %>%
  summarise(YrResident=mean(YrResident,na.rm=T),
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
                                                                  !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            MTManage=mean(RightsManage,na.rm=T),
            MTHarvest=mean(RightsHarvest,na.rm=T),
            MTAccess=mean(RightsAccess,na.rm=T),
            MTTransfer=mean(RightsTransfer,na.rm=T),
            MTExclude=mean(RightsExclude,na.rm=T),
            MatAssets.gini=gini(MAIndex),
            MAIndex=mean(MAIndex,na.rm=T),
            Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100
           )
Synth.techreport.byMPA <-left_join(Synth.techreport.byMPA,BigFive.MPAGroup)

# - same data as above but for control, by MPA


CurrentDemos.control$InterviewYear <- as.integer(CurrentDemos.control$InterviewYear)

Synth.techreport.byMPA.control <-
  left_join(CurrentDemos.control,HHData[,c("HouseholdID","SocialConflict","Treatment")]) %>%
  left_join(MT[,c("HouseholdID","RightsAccess","RightsHarvest","RightsManage","RightsExclude","RightsTransfer")]) %>%
  left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex","MTIndex","PAIndex")]) %>%
  group_by(MPAID,MonitoringYear,Treatment) %>%
  summarise(YrResident=mean(YrResident,na.rm=T),
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
                                                                  !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            MTManage=mean(RightsManage,na.rm=T),
            MTHarvest=mean(RightsHarvest,na.rm=T),
            MTAccess=mean(RightsAccess,na.rm=T),
            MTTransfer=mean(RightsTransfer,na.rm=T),
            MTExclude=mean(RightsExclude,na.rm=T),
            MatAssets.gini=gini(MAIndex),
            MAIndex=mean(MAIndex,na.rm=T),
            Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
Synth.techreport.byMPA.control <-left_join(Synth.techreport.byMPA.control,BigFive.ControlGroup)

# ---- 3.4 Calculating Days unwell by MPA and settlement, joining with technical report data frame ----

Days.unwell <- group_by(IndDemos,HouseholdID)
Days.unwell <- summarise(Days.unwell,
                         DaysUnwell=sum(DaysUnwell,na.rm=T)/length(HouseholdID))
Days.unwell <- left_join(Days.unwell,
                         BigFive[,c("HouseholdID", "MPAID", "SettlementID", "SettlementName", "Treatment", 
                                    "InterviewYear", "MonitoringYear")])

Days.unwell.Alor.control.annex <- Days.unwell[Days.unwell$Treatment==0,]

Days.unwell <- left_join(Days.unwell,
                         MPA.currentyear)

Days.unwell.treatment <- Days.unwell[Days.unwell$Treatment==1,]



# ---Days unwell, by MPA ("Days.unwell.ByMPA" is most current cross-section of data)
Days.unwell.ByMPA <- 
  Days.unwell.treatment %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))

Synth.techreport.byMPA <-left_join(Synth.techreport.byMPA,Days.unwell.ByMPA)


# ---Days unwell, by settlement ("Days.unwell.BySett" is most current cross-section of data)
Days.unwell.BySett <- 
  Days.unwell.treatment %>%
  group_by(SettlementID,MPAID,MonitoringYear) %>%
  summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))

Synth.techreport.bySett <-left_join(Synth.techreport.bySett,Days.unwell.BySett)

Days.unwell.control <- 
  Days.unwell[Days.unwell$InterviewYear==Days.unwell$CurrentYear &
                Days.unwell$Treatment==0,] %>%
  group_by(MPAID) %>%
  summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))
Synth.techreport.byMPA.control <-left_join(Synth.techreport.byMPA.control,Days.unwell.control)

# ---- 3.4 Calculating Age and Gender breakdown by MPA and settlement ----

AgeGenderDemos <- left_join(HHDemos.context[,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName","Treatment")],
                            IndDemos[,c("HouseholdID", "IndividualGender", "IndividualAge")])

AgeGenderDemos.control <- left_join(HHDemos.context.1[HHDemos.context.1$Treatment==0,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName","Treatment")],
                                    IndDemos[,c("HouseholdID", "IndividualGender", "IndividualAge")])
AgeGender.AvgAge.byMPA <-
  AgeGenderDemos %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))

AgeGender.AvgAge.bySett <-
  AgeGenderDemos %>%
  group_by(SettlementName,MPAID,MonitoringYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))

AgeGender.AvgAge.control <-
  AgeGenderDemos.control %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))


AgeGenderDemos.ByMPA <- 
  AgeGenderDemos %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(Male.0.4=(length(IndividualAge[IndividualAge<=4 &
                                                  !is.na(IndividualAge) &
                                                  IndividualGender==1&
                                                  !is.na(IndividualGender)])/
                        length(IndividualAge[!is.na(IndividualAge)&
                                                    !is.na(IndividualGender)]))*-100,
            Female.0.4=(length(IndividualAge[IndividualAge<=4 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==0&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*100,
            Male.5.9=(length(IndividualAge[IndividualAge>4 & 
                                                  IndividualAge<=9 &
                                                  !is.na(IndividualAge) &
                                                  IndividualGender==1&
                                                  !is.na(IndividualGender)])/
                        length(IndividualAge[!is.na(IndividualAge)&
                                                    !is.na(IndividualGender)]))*-100,
            Female.5.9=(length(IndividualAge[IndividualAge>4 & 
                                                    IndividualAge<=9 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==0&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*100,
            Male.10.14=(length(IndividualAge[IndividualAge>9 &
                                                    IndividualAge<=14 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.10.14=(length(IndividualAge[IndividualAge>9 &
                                                      IndividualAge<=14 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.15.19=(length(IndividualAge[IndividualAge>14 &
                                                    IndividualAge<=19 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.15.19=(length(IndividualAge[IndividualAge>14 &
                                                      IndividualAge<=19 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.20.24=(length(IndividualAge[IndividualAge>19 &
                                                    IndividualAge<=24 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.20.24=(length(IndividualAge[IndividualAge>19 &
                                                      IndividualAge<=24 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.25.29=(length(IndividualAge[IndividualAge>24 &
                                                    IndividualAge<=29 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.25.29=(length(IndividualAge[IndividualAge>24 &
                                                      IndividualAge<=29 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.30.34=(length(IndividualAge[IndividualAge>29 &
                                                    IndividualAge<=34 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.30.34=(length(IndividualAge[IndividualAge>29 &
                                                      IndividualAge<=34 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.35.39=(length(IndividualAge[IndividualAge>34 &
                                                    IndividualAge<=39 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.35.39=(length(IndividualAge[IndividualAge>34 &
                                                      IndividualAge<=39 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.40.44=(length(IndividualAge[IndividualAge>39 &
                                                    IndividualAge<=44 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)]))*-100,
            Female.40.44=(length(IndividualAge[IndividualAge>39 &
                                                      IndividualAge<=44 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.45.49=(length(IndividualAge[IndividualAge>44 &
                                                    IndividualAge<=49 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.45.49=(length(IndividualAge[IndividualAge>44 &
                                                      IndividualAge<=49 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.50.54=(length(IndividualAge[IndividualAge>49 &
                                                    IndividualAge<=54 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.50.54=(length(IndividualAge[IndividualAge>49 &
                                                      IndividualAge<=54 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.55.59=(length(IndividualAge[IndividualAge>54 &
                                                    IndividualAge<=59 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.55.59=(length(IndividualAge[IndividualAge>54 &
                                                      IndividualAge<=59 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.60.64=(length(IndividualAge[IndividualAge>59 &
                                                    IndividualAge<=64 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.60.64=(length(IndividualAge[IndividualAge>59 &
                                                      IndividualAge<=64 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.65.69=(length(IndividualAge[IndividualAge>64 &
                                                    IndividualAge<=69 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.65.69=(length(IndividualAge[IndividualAge>64 &
                                                      IndividualAge<=69 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.70.74=(length(IndividualAge[IndividualAge>69 &
                                                    IndividualAge<=74 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.70.74=(length(IndividualAge[IndividualAge>69 &
                                                      IndividualAge<=74 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.75.79=(length(IndividualAge[IndividualAge>74 &
                                                    IndividualAge<=79 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.75.79=(length(IndividualAge[IndividualAge>74 &
                                                      IndividualAge<=79 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.80.84=(length(IndividualAge[IndividualAge>79 &
                                                    IndividualAge<=84 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.80.84=(length(IndividualAge[IndividualAge>79 &
                                                      IndividualAge<=84 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.85.89=(length(IndividualAge[IndividualAge>84 &
                                                    IndividualAge<=89 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.85.89=(length(IndividualAge[IndividualAge>84 &
                                                      IndividualAge<=89 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.90.94=(length(IndividualAge[IndividualAge>89 &
                                                    IndividualAge<=94 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.90.94=(length(IndividualAge[IndividualAge>89 &
                                                      IndividualAge<=94 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0&
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100,
            Male.95.99=(length(IndividualAge[IndividualAge>94 &
                                                    IndividualAge<=99 &
                                                    !is.na(IndividualAge) &
                                                    IndividualGender==1&
                                                    !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                      !is.na(IndividualGender)]))*-100,
            Female.95.99=(length(IndividualAge[IndividualAge>94 &
                                                      IndividualAge<=99 &
                                                      !is.na(IndividualAge) &
                                                      IndividualGender==0 &
                                                      !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                        !is.na(IndividualGender)]))*100)

AgeGenderDemos.ByMPA <- AgeGenderDemos.ByMPA[!is.na(AgeGenderDemos.ByMPA$MPAID),]

rm(MA,PA,MA.PA.MT.FS,MPANames,cFS)

Techreport.Status.BySett <- left_join(Techreport.Status.BySett,Synth.techreport.bySett)
Techreport.Trend.ByMPA <- left_join(Techreport.Trend.ByMPA,Synth.techreport.byMPA)
Techreport.ByMPA.control <-left_join(Techreport.ByMPA.control,Synth.techreport.byMPA.control)

# ---- 4.1 MPA Technical Report plot themes ----

plot.theme <- theme(axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.x=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.y=element_blank(),
                    plot.margin=margin(t=0,r=20,b=5,l=5,unit="pt"),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                           angle=0,
                                           colour="#303030",
                                           lineheight=0.7),
                    legend.position="top",
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"))

age.gender.plot.theme <- theme(axis.ticks=element_blank(),
                               panel.background=element_rect(fill="white",
                                                             colour="#909090"),
                               panel.border=element_rect(fill=NA,
                                                         size=0.25,
                                                         colour="#C0C0C0"),
                               panel.grid.major.x=element_line(colour="#C0C0C0",
                                                               size=0.25,
                                                               linetype=3),
                               panel.grid.major.y=element_blank(),
                               axis.title=element_text(size=rel(0.8),
                                                       angle=0,
                                                       face="bold",
                                                       colour="#303030"),
                               axis.text=element_text(size=rel(0.6),
                                                      angle=0,
                                                      colour="#303030"),
                               legend.position="top",
                               legend.justification="right",
                               legend.box.spacing=unit(0.1,"cm"))



fillcols.status <- c("NotDummy"=alpha("#2C7FB8",0.95),"Dummy"=alpha("#FFFFFF",0))
fillcols.trend <- c(alpha("#2C7FB8",0.95))

errcols.status <- c("NotDummy"=alpha("#21577C",0.95),"Dummy"=alpha("#FFFFFF",0))
errcols.trend <- c(alpha("#21577C",0.95))

multianswer.fillcols.status <- list(Gender=c("HHH.male"=alpha("#253494",0.95),
                                             "HHH.female"=alpha("#7FCDBB",0.95)),
                                    Religion=c("Percent.Rel.Christian"=alpha("#253494",0.95),
                                               "Percent.Rel.Muslim"=alpha("#7FCDBB",0.95),
                                               "Percent.Rel.Other"=alpha("#FDC086",0.95)),
                                    PrimaryOcc=c("Percent.PrimaryOcc.Farm"=alpha("#A6CEE3",0.95), # light blue 
                                                 "Percent.PrimaryOcc.HarvestForest"=alpha("#7FC97F",0.95), # light green
                                                 "Percent.PrimaryOcc.Fish"=alpha("#1F78B4",0.95), # blue                                                  "Percent.PrimaryOcc.Tourism"=alpha("#C23737",0.95), # red
                                                 "Percent.PrimaryOcc.WageLabor"=alpha("#33A02C",0.95), # green 
                                                 "Percent.PrimaryOcc.Tourism"=alpha("#E31A1C",0.95), # red 
                                                 "Percent.PrimaryOcc.Aquaculture"=alpha("#FDBF6F",0.95), # blue (X)
                                                 "Percent.PrimaryOcc.Extraction"=alpha("#FB9A99",0.95), # light red (X)
                                                 "Percent.PrimaryOcc.Other"=alpha("#FF7F00",0.95)), # orange 
                                    FreqFish=c("Prop.Fish.AlmostNever"=alpha("#E1E198",0.95),
                                               "Prop.Fish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                               "Prop.Fish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                               "Prop.Fish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                               "Prop.Fish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    FreqSellFish=c("Prop.SellFish.AlmostNever"=alpha("#E1E198",0.95),
                                                   "Prop.SellFish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                                   "Prop.SellFish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                                   "Prop.SellFish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                                   "Prop.SellFish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    IncFish=c("Prop.IncFish.None"=alpha("#E1E198",0.95),
                                              "Prop.IncFish.Some"=alpha("#7FCDBB",0.95),
                                              "Prop.IncFish.Half"=alpha("#2CA9B8",0.95),
                                              "Prop.IncFish.Most"=alpha("#2C7FB8",0.95),
                                              "Prop.IncFish.All"=alpha("#253494",0.95)),
                                    FishTech=c("Prop.FishTech.ByHand"=alpha("#7FC97F",0.95),
                                               "Prop.FishTech.StatNet"=alpha("#BEAED4",0.95),
                                               "Prop.FishTech.MobileNet"=alpha("#FDC086",0.95),
                                               "Prop.FishTech.StatLine"=alpha("#E1E198",0.95),
                                               "Prop.FishTech.MobileLine"=alpha("#386CB0",0.95)),
                                    ChildFS=c("Child.FS.no"=alpha("#253494",0.95),
                                              "Child.FS.yes"=alpha("#7FCDBB",0.95)),
                                    Protein=c("ProteinFish.None"=alpha("#E1E198",0.95),
                                              "ProteinFish.Some"=alpha("#7FCDBB",0.95),
                                              "ProteinFish.Half"=alpha("#2CA9B8",0.95),
                                              "ProteinFish.Most"=alpha("#2C7FB8",0.95),
                                              "ProteinFish.All"=alpha("#253494",0.95)),
                                    Member=c("Member.Yes"=alpha("#253494",0.95),
                                             "Member.No"=alpha("#7FCDBB",0.95)),
                                    PropRules=c("PropRuleHab"=alpha("#253494",0.95),
                                                "PropRuleSpp"=alpha("#7FCDBB",0.95)),
                                    Attendance=c("Prop.Member.Yes.Meeting.Yes"=alpha("#253494",0.95),
                                                 "Prop.Member.Yes.Meeting.No"=alpha("#7FCDBB",0.95)),
                                    FSCategorical=c("Percent.FoodInsecure.YesHunger"=alpha("#E1E198",0.95),
                                                    "Percent.FoodInsecure.NoHunger"=alpha("#7FCDBB",0.95),
                                                    "Percent.FoodSecure"=alpha("#253494",0.95)),
                                    Illness=c("Percent.Ill"=alpha("#253494",0.95),
                                              "Percent.Not.Ill"=alpha("#7FCDBB",0.95)),
                                    EconStatus=c("Econ.Status.Much.Worse"=alpha("#E1E198",0.95),
                                                 "Econ.Status.Slighly.Worse"=alpha("#7FCDBB",0.95),
                                                 "Econ.Status.Neutral"=alpha("#2CA9B8",0.95),
                                                 "Econ.Status.Slightly.Better"=alpha("#2C7FB8",0.95),
                                                 "Econ.Status.Much.Better"=alpha("#253494",0.95)),
                                    SocialConflict=c("Percent.GreatlyDecreased.SocConflict"=alpha("#E1E198",0.95),
                                                     "Percent.Decreased.SocConflict"=alpha("#7FCDBB",0.95),
                                                     "Percent.Same.SocConflict"=alpha("#2CA9B8",0.95),
                                                     "Percent.Increased.SocConflict"=alpha("#2C7FB8",0.95),
                                                     "Percent.GreatlyIncreased.SocConflict"=alpha("#253494",0.95)),
                                    NumThreats=c("Threat.None"=alpha("#7FC97F",0.95),
                                                 "Threat.One"=alpha("#BEAED4",0.95),
                                                 "Threat.Two"=alpha("#FDC086",0.95),
                                                 "Threat.Three"=alpha("#FB9A99",0.95),
                                                 "Threat.Four"=alpha("#E1E198",0.95),
                                                 "Threat.Minimum.Five"=alpha("#E31A1C", 0.95)),
                                    ThreatType=c("Pollution"=alpha("#A6CEE3",0.95), # light blue 
                                                 "DestructiveFishing"=alpha("#1F78B4",0.95), # blue
                                                 "IllegalFishing"=alpha("#7FC97F",0.95), # light green
                                                 "ClimateChange"=alpha("#33A02C",0.95), # green
                                                 "HabitatLoss"=alpha("#FB9A99",0.95), # light red
                                                 "NaturalProcesses"=alpha("#E31A1C",0.95), # red 
                                                 "OtherMarineUses"=alpha("#FDBF6F",0.95), # light orange
                                                 "Other"=alpha("#FF7F00",0.95)), # orange 
                                    Participate=c("ParticipateOrg"=alpha("#7FC97F",0.95),
                                                  "ParticipateEstablish"=alpha("#FFFFB3",0.95),
                                                  "ParticipateBnd"=alpha("#2C7FB8",0.95),
                                                  "ParticipateRules"=alpha("#253494",0.95)),
                                    HHHEducation=c("HHHEducNone"=alpha("#7FC97F",0.95),
                                                   "HHHEducPre"=alpha("#FFFFB3",0.95),
                                                   "HHHEducPrim"=alpha("#BEBADA",0.95),
                                                   "HHHEducMid"=alpha("#2C7FB8",0.95),
                                                   "HHHEducSec"=alpha("#253494",0.95),
                                                   "HHHEducHigher"=alpha("#FDB462", 0.95)),
                                    AdultEducation=c("AdultEducNone"=alpha("#7FC97F",0.95),
                                                     "AdultEducPre"=alpha("#FFFFB3",0.95),
                                                     "AdultEducPrim"=alpha("#BEBADA",0.95),
                                                     "AdultEducMid"=alpha("#2C7FB8",0.95),
                                                     "AdultEducSec"=alpha("#253494",0.95),
                                                     "AdultEducHigher"=alpha("#FDB462", 0.95)))





# ---- 4.2 MPA Impact Summary plot themes ----

fill.cols.MPAimpact.summ <- c("MPA"=alpha("#1B448B",0.85),"Control"=alpha("#6B6B6B",0.4))
err.cols.MPAimpact.summ <- c(alpha("#0A1D4E",0.5),alpha("#242424",0.25))

snapshot.plot.theme.MPAimpact.summ <- theme(panel.background=element_blank(),
                                            panel.grid.major.x=element_line(size=0.25,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.major.y=element_line(size=0.5,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.minor.x=element_blank(),
                                            axis.ticks=element_blank(),
                                            axis.text=element_text(size=11,
                                                                   angle=0,
                                                                   colour="#505050",
                                                                   hjust=0.5),
                                            axis.title=element_text(size=12,
                                                                    angle=0,
                                                                    face="bold",
                                                                    colour="#505050"),
                                            plot.title=element_text(colour="#505050",
                                                                    face="bold",
                                                                    size=14),
                                            panel.border=element_rect(size=0.5,
                                                                      colour="#D0D0D0",
                                                                      linetype=3,
                                                                      fill=NA))

plot.theme.MPAimpact.summ <- theme(axis.ticks=element_blank(),
                                   axis.text=element_text(vjust=0.5,
                                                          size=rel(1.1),
                                                          colour="#505050"),
                                   axis.title.y=element_text(face="bold",
                                                             size=rel(1.15),
                                                             angle=90,
                                                             colour="#505050"),
                                   axis.title.x=element_blank(),
                                   plot.title=element_blank(),
                                   panel.background=element_rect(fill="white",
                                                                 colour="#D0D0D0"),
                                   panel.grid.major.x=element_blank(),
                                   panel.grid.major.y=element_line(size=0.5,
                                                                   colour="#808080",
                                                                   linetype=3),
                                   panel.grid.minor.y=element_blank(),
                                   panel.border=element_rect(fill=NA,
                                                             colour="#D0D0D0"))

fs.st.plot.theme.MPAimpact.summ <- theme(axis.ticks.x=element_blank(),
                                         axis.ticks.y=element_line(colour="#505050"),
                                         axis.text=element_text(vjust=0.5,
                                                                size=rel(1.1),
                                                                colour="#505050"),
                                         axis.title.y=element_text(face="bold",
                                                                   size=rel(1.15),
                                                                   angle=90,
                                                                   colour="#505050"),
                                         axis.title.x=element_blank(),
                                         plot.title=element_blank(),
                                         panel.background=element_rect(fill="white",
                                                                       colour="#D0D0D0"),
                                         panel.grid.major.x=element_blank(),
                                         panel.grid.major.y=element_blank(),
                                         panel.grid.minor.y=element_blank(),
                                         panel.border=element_rect(fill=NA,
                                                                   colour="#D0D0D0"))


# ---- 4.3 MPA Technical Report plot legend guide ----


plot.guides.techreport <- guides(alpha=guide_legend(title.hjust=1,
                                                    title.theme=element_text(face="bold",
                                                                             size=rel(9),
                                                                             angle=0,
                                                                             colour="#505050",
                                                                             lineheight=0.75),
                                                    label.vjust=0.5,
                                                    label.theme=element_text(size=rel(8),
                                                                             angle=0,
                                                                             colour="#505050",
                                                                             lineheight=0.75),
                                                    direction="horizontal",
                                                    ncol=3,
                                                    title.position="left",
                                                    label.position="right",
                                                    keywidth=unit(0.75,"cm"),
                                                    keyheight=unit(0.5,"cm")),
                                 fill=guide_legend(title.hjust=1,
                                                   title.theme=element_text(face="bold",
                                                                            size=rel(9),
                                                                            angle=0,
                                                                            colour="#505050",
                                                                            lineheight=0.75),
                                                   label.vjust=0.5,
                                                   label.theme=element_text(size=rel(9),
                                                                            angle=0,
                                                                            colour="#505050",
                                                                            lineheight=0.75),
                                                   direction="horizontal",
                                                   ncol=2,
                                                   title.position="left",
                                                   label.position="right",
                                                   keywidth=unit(0.75,"cm"),
                                                   keyheight=unit(0.5,"cm"),
                                                   reverse=T),
                                 colour=guide_legend(title=element_blank(),
                                                     label.position="bottom",
                                                     label.theme=element_text(size=9,
                                                                              angle=0,
                                                                              colour="#505050"),
                                                     label.hjust=0.5,
                                                     keywidth=unit(2.5,"cm")),
                                 shape=guide_legend(title=element_blank(),
                                                    label.position="right",
                                                    label.theme=element_text(size=9,
                                                                             angle=0,
                                                                             colour="#505050"),
                                                    label.hjust=0.6))

# - Function to create common legend between multiple ggplots
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---- 4.4 MPA Impact Summary plot legend guide ----

plot.guides.MPAimpact.summ <- guides(size=guide_legend(title=element_blank(),
                                                       label.theme=element_text(size=9,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       label.position="bottom",
                                                       keywidth=unit(2.5,"cm"),
                                                       label.hjust=0.5,
                                                       order=2),
                                     fill=guide_legend(title.hjust=0.5,
                                                       title.theme=element_text(face="bold",
                                                                                size=10,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       label.theme=element_text(size=9,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       order=1,
                                                       ncol=2,
                                                       nrow=1,
                                                       title.position="top",
                                                       label.position="bottom",
                                                       keywidth=unit(1.2,"cm"),
                                                       label.hjust=0.5),
                                     colour=guide_legend(title=element_blank(),
                                                         label.position="bottom",
                                                         label.theme=element_text(size=9,
                                                                                  angle=0,
                                                                                  colour="#505050"),
                                                         label.hjust=0.5,
                                                         keywidth=unit(2.5,"cm"),
                                                         order=3),
                                     shape=guide_legend(title=element_blank(),
                                                        label.position="right",
                                                        label.theme=element_text(size=9,
                                                                                 angle=0,
                                                                                 colour="#505050"),
                                                        label.hjust=0.6,
                                                        order=4))

snapshot.plot.guide.MPAimpact.summ <- guides(fill=guide_legend(order=1,
                                                               keywidth=unit(1,"cm"),
                                                               label.theme=element_text(size=9,
                                                                                        angle=0,
                                                                                        colour="#505050"),
                                                               label.position="right",
                                                               label.hjust=0.5,
                                                               label.vjust=0.5,
                                                               title.theme=element_text(size=10,
                                                                                        angle=0,
                                                                                        colour="#505050",
                                                                                        face="bold",
                                                                                        lineheight=0.8),
                                                               title.hjust=0.5),
                                             linetype=guide_legend(order=3,
                                                                   keywidth=unit(2.5,"cm"),
                                                                   title.theme=element_text(size=10,
                                                                                            angle=0,
                                                                                            colour="#505050",
                                                                                            lineheight=0.8,
                                                                                            face="bold"),
                                                                   title.hjust=0.5,
                                                                   label=F))


# ---- 4.5 MPA continuous variables distribution plot theme ----

dist.plot.theme <- theme(axis.ticks=element_blank(),
                         plot.title=element_text(face="bold",
                                                 colour="#303030",
                                                 hjust=0.5),
                         panel.background=element_rect(fill="white",
                                                       colour="#909090"),
                         panel.border=element_rect(fill=NA,
                                                   size=0.25,
                                                   colour="#C0C0C0"),
                         panel.grid.major.x=element_line(colour="#C0C0C0",
                                                         size=0.25),
                         panel.grid.major.y=element_line(colour="#C0C0C0",
                                                         size=0.25,
                                                         linetype=3),
                         axis.title=element_text(size=11,
                                                 angle=0,
                                                 face="bold",
                                                 colour="#303030"),
                         axis.text=element_text(size=10,
                                                angle=0,
                                                colour="#303030"))


# ---- 4.6 MPA Technical Report plot labels ----

Statusplot.labs <- list(FS=labs(y="Mean household food security",x="Settlement"),
                        MA=labs(y="Mean household assets",x="Settlement"),
                        PA=labs(y="Mean place attachment",x="Settlement"),
                        MT=labs(y="Mean household marine tenure",x="Settlement"),
                        SE=labs(y="School enrollment (% children ages 5-18 years old)",x="Settlement"),
                        Time=labs(y="Mean travel time to closest market (hours)",x="Settlement"),
                        Unwell=labs(y="Mean time suffering from illness or injury in past 4 weeks (days)",
                                    x="Settlement"),
                        Gender=labs(y="Gender (% head of household)",x="Settlement"),
                        Religion=labs(y="Religion (% head of household)",x="Settlement"),
                        PrimaryOcc=labs(y="Primary occupation (% households)",x="Settlement"),
                        FreqFish=labs(y="Frequency of fishing (% households)",x="Settlement"),
                        FreqSellFish=labs(y="Frequency of selling at least some catch (% households)",
                                          x="Settlement"),
                        FishProtein=labs(y="Dietary protein from fish in past 6 months (% households)",
                                         x="Settlement"),
                        IncFish=labs(y="Income from fishing in past 6 months (% households)",
                                     x="Settlement"),
                        FishTech=labs(y="Fishing technique most often used in past 6 months (% households)",
                                      x="Settlement"),
                        ChildFS=labs(y="Child hunger (% households)",x="Settlement"),
                        Ethnicity=labs(y="Mean number of ethnicities (Settlement ethnicity frequency)",x="Settlement"),
                        AdultEduc=labs(y="Education completed (% adults 18 years and older)",x="Settlement"),
                        HHHEduc=labs(y="Education completed (% household heads)",x="Settlement"),
                        EconStatus=labs(y="Change in economic status of fishing households (% households)",x="Settlement"),
                        Conflict=labs(y="Perceive change in level of social conflict over marine resources in past 12 months (% households)",x="Settlement"),
                        NumLocalThreats=labs(y="Percent of respondents identifying threats to marine environement",x="Settlement"),
                        ThreatTypes=labs(y="Percent of local threats to marine environment identified",x="Settlement"),
                        Member=labs(y="Households who are a member of an organization",x="Settlement"),
                        Attendance=labs(y="Households who are a member of an organization and have attended a meeting",x="Settlement"),
                        Contribution=labs(y="Mean household contribution (Indonesian Rupiah)",x="Settlement"),
                        FSCategorical= labs(y="Food security by category (% households)",x="Settlement"),
                        Rules= labs(y="Percent of important species or habitats subject to specific harvest rule",x="Settlement"),
                        Ill = labs(y="Illness or Injury in the past 4 weeks (% individuals)", x="Settlement"),
                        Participation= labs(y="Percent of important user groups participating in marine resource decisions",x="Settlement"))


continuous.variables.plotlabs <- c("Mean household food security","Mean household assets",
                                   "Mean place attachment",
                                   "School enrollment (% children ages 5-18 years old)",
                                   "Mean time suffering from illness or injury in past 4 weeks (days)")


# ---- 4.7 MPA Impact Summary "Big Five" plot labels ----

plot.fs.labs.st <- labs(y="Household Food Security\n ",title="STATUS AND TREND")
plot.ma.labs.st <- labs(y="Household Material Assets\n ",title="STATUS AND TREND")
plot.pa.labs.st <- labs(y="Household Place Attachment\n ",title="STATUS AND TREND")
plot.mt.labs.st <- labs(y="Household Marine Tenure\n ",title="STATUS AND TREND")
plot.se.labs.st <- labs(y="Enrollment Rate\n ",title="STATUS AND TREND")

plot.fs.labs.i <- labs(y="Change in Household Food Security\nsince Baseline",title="IMPACT")
plot.ma.labs.i <- labs(y="Change in Household Material Assets\nsince Baseline",title="IMPACT")
plot.pa.labs.i <- labs(y="Change in Household Place Attachment\nsince Baseline",title="IMPACT")
plot.mt.labs.i <- labs(y="Change in Household Marine Tenure\nsince Baseline",title="IMPACT")
plot.se.labs.i <- labs(y="Change in Enrollment Rate\nsince Baseline",title="IMPACT")

impact.x.labs <- c("MPA\nHouseholds","Control\nHouseholds")


