# 
# code: Social MPA Mystery Analysis, Sunda Banda Seascape
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: January 2018
# 
# modified by Amari: June 2019
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

# ---- 1.0 Load libraries and data ----
pacman::p_load(plyr,tidyverse,reldist)

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


# ---- 3.1 Settlement-level analysis, for status and annex plots ----


Techreport.Status.BySett <- 
  HHDemos.context %>%
  group_by(SettlementID,MonitoringYear) %>%
  filter(MonitoringYear=="3 Year Post") %>%
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
  group_by(MPAID,MonitoringYear) %>%
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
Techreport.ByMPA.control <- group_by(CurrentDemos.control,MPAID)
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


# - Number years resident by settlement, to look for signs of rapid immigration 
# - Changes in social conflict by settlement
# - Marine tenure components by settlement


HHDemos.context$InterviewYear <- as.integer(HHDemos.context$InterviewYear)


Synth.techreport.bySett <-
  left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict","PaternalEthnicity")]) %>%
  left_join(MT[,c("HouseholdID","RightsAccess","RightsHarvest","RightsManage","RightsExclude","RightsTransfer","MTIndex")]) %>%
  left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex","PAIndex")]) %>%
  group_by(SettlementID,MPAID,MonitoringYear) %>%
  filter(MonitoringYear=="3 Year Post") %>%
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
  group_by(MPAID,MonitoringYear) %>%
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

MT %>%
  filter(RightsAccess==2) #REMOVE THIS ONE, INVALID RESPONSE? ?????????????????????????????????

CurrentDemos.control$InterviewYear <- as.integer(CurrentDemos.control$InterviewYear)

Synth.techreport.byMPA.control <-
  left_join(CurrentDemos.control,HHData[,c("HouseholdID","SocialConflict")]) %>%
  left_join(MT[,c("HouseholdID","RightsAccess","RightsHarvest","RightsManage","RightsExclude","RightsTransfer")]) %>%
  left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex","MTIndex","PAIndex")]) %>%
  group_by(MPAID,MonitoringYear) %>%
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

AgeGenderDemos <- left_join(HHDemos.context[,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName")],
                            IndDemos[,c("HouseholdID", "IndividualGender", "IndividualAge")])

AgeGenderDemos.control <- left_join(HHDemos.context.1[HHDemos.context.1$Treatment==0,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName")],
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

rm(MA,PA,MA.PA.MT.FS,MPANames,cFS,Days.unwell,Days.unwell.ByMPA,Days.unwell.BySett,Days.unwell.control,Days.unwell.treatment)

Techreport.Status.BySett <- left_join(Techreport.Status.BySett,Synth.techreport.bySett)
Techreport.Trend.ByMPA <- left_join(Techreport.Trend.ByMPA,Synth.techreport.byMPA)
Techreport.ByMPA.control <-left_join(Techreport.ByMPA.control,Synth.tecehreport.byMPA.control)



