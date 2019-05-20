# 
# code: Social MPA Mystery Analysis, Koon 2018
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: April 2019
# 
# ---- inputs ----
# SBS_HHData -- in SBS Master Database in Access
#   -this table can be created by running the query saved named 'Q_HHData_ForExport_R'.
#   -this table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculated into the food security, material assets, place attachment, marine
#    tenure, and school enrollment indexes -- along with other livelihood and household 
#    demographic variables.
# SBS_HHDemos -- in SBS Master Database in Access
#   -the HHDemos table can be created by running the query saved named 'Q_HHDemos_ForExport_R'.
#   -the HHDemos table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculate the school enrollment rate, days unwell, and head of household gender. 
# SBS_HH_tbl_SETTLEMENT -- in SBS Master Database in Access
#   -the Settlement table provides the names and treatment vs. non-treatment status for 
#    each settlementID.
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
# ---- SECTION 1: Load libraries, Import Data, and Subset ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Load libraries ----

pacman::p_load(plyr,dplyr,ggplot2,reshape2,reldist,grid,gridExtra,varhandle,xlsx,
               RODBC,Matching,optmatch,tidyr,RItools,Hmisc,MBESS,rbounds,Kendall)


# ---- 1.2 Import data ----

source('Koon_2018_source_data.R')

# ---- 1.3 Define monitoring year for each MPA ----

# MonitoringYear <- group_by(HHData,MPAID)
# MonitoringYear <- summarise(MonitoringYear,
#                             Baseline=min(InterviewYear),
#                             TwoYear=as.integer(min(InterviewYear)+2),
#                             ThreeYear=as.integer(min(InterviewYear)+3),
#                             SixYear=as.integer(min(InterviewYear)+6))
# MonitoringYear <- left_join(HHData[,c("HouseholdID","MPAID")],
#                             MonitoringYear)
# 
# HHData$MonitoringYear <- factor(mapply(a=HHData$HouseholdID,
#                                        b=HHData$InterviewYear,
#                                        function(a,b){
#                                          ifelse(b==MonitoringYear$Baseline[MonitoringYear$HouseholdID==a],"Baseline",
#                                                 ifelse(b==MonitoringYear$TwoYear.Koon[MonitoringYear$HouseholdID==a],"2 Year Post",
#                                                        ifelse(b==MonitoringYear$ThreeYear[MonitoringYear$HouseholdID==a],"3 Year Post",
#                                                               ifelse(b==MonitoringYear$SixYear[MonitoringYear$HouseholdID==a],"6 Year Post",NA))))
#                                        }),
#                                 levels=c("Baseline","2 Year Post", "3 Year Post", "4 Year Post"),
#                                 ordered=T)

# ---- 1.4 Subset variables from HHData for Big Five calculations & descriptive statistics ----

FS <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemoveFS", 
                "DidNotLastClean", "BalancedDietClean", "AdultSkipClean", "EatLessClean", 
                "FreqAdultSkipClean", "HungryClean")]


MA <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemoveMA", 
                "CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", "Generator")]


PA <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemovePA", "PlaceHappyClean",
                "PlaceFavClean", "PlaceMissClean", "PlaceBestClean", "PlaceFishClean", "PlaceMyselfClean")]


MT <- HHData[,c("HouseholdID","HouseholdID", "MPAID", "SettlementID", "InterviewYear",  "RemoveMT", "RightsAccessClean", "RightsHarvestClean", 
                "RightsManageClean", "RightsExcludeClean", "RightsTransferClean")]


    
HHLivelihood <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "PrimaryLivelihoodClean", 
                          "SecondaryLivelihoodClean", "TertiaryLivelihoodClean", "FreqFishClean", 
                          "FreqSaleFishClean", "PercentIncFishClean", "MajFishTechniqueClean", 
                          "FreqEatFishClean", "PercentProteinFishClean", "TimeMarketClean")]

HHDemos <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "ReligionClean", "YrResidentClean")]

HeadOfHH <- IndDemos[IndDemos$RelationHHH==0 &
                       !is.na(IndDemos$RelationHHH), c("HouseholdID", "IndividualGenderClean", "IndividualEducationClean", 
                                                       "IndividualAgeClean")]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: "Big Five" Indexes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Compute indexes ----

MA$MAIndex <- ifelse(MA$RemoveMA=="No",
                     rowSums(MA[,c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                                   "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", 
                                   "Generator")],
                             na.rm=TRUE),
                     NA)

PA$PAIndex <- ifelse(PA$RemovePA=="No",
                     round(rowMeans(PA[,c("PlaceHappyClean", "PlaceFavClean", "PlaceMissClean", "PlaceBestClean", 
                                          "PlaceFishClean", "PlaceMyselfClean")],
                                    na.rm=TRUE),2),
                     NA)

MT$MTIndex <- ifelse(MT$RemoveMT=="No",
                     rowSums(MT[,c("RightsAccessClean", "RightsHarvestClean", "RightsManageClean", 
                                   "RightsExcludeClean", "RightsTransferClean")],
                             na.rm=TRUE),
                     NA)

FS$FSIndex <- as.character(ifelse(FS$RemoveFS=="No",
                                  rowSums(FS[,c("DidNotLastClean", "BalancedDietClean", "AdultSkipClean", "EatLessClean", 
                                                "FreqAdultSkipClean", "HungryClean")],
                                          na.rm=TRUE),
                                  NA))
FS$FSIndex <- revalue(FS$FSIndex, c("0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06"))
FS$FSIndex <- 6.06-as.numeric(FS$FSIndex)

MA.PA.MT.FS <- left_join(MA,PA)
MA.PA.MT.FS <- left_join(MA.PA.MT.FS,MT)
MA.PA.MT.FS <- left_join(MA.PA.MT.FS,FS)

IndDemos.1 <- 
  IndDemos %>%
  group_by(HouseholdID) %>%
  summarise(NumberChild=sum(ChildOrAdult,na.rm=T),
            EnrolledHH=sum(ChildEnrolled,na.rm=T),
            PercentEnrolled=ifelse(NumberChild!=0 & !is.na(EnrolledHH),
                                   as.character(round((EnrolledHH/NumberChild)*100,2)),
                                   ifelse(NumberChild==0,
                                          "No School-Aged Children","No Data")))

BigFive.allvar <- left_join(MA.PA.MT.FS,IndDemos.1)
BigFive.allvar <- left_join(BigFive.allvar,Settlements)

# ---- 2.2 Define "Big Five" data frame - at household level, all MPAs, all years ----

BigFive <- data.frame(BigFive.allvar[,c("HouseholdID","MPAID","SettlementID","SettlementName",
                                        "Treatment","InterviewYear","MAIndex","FSIndex","PAIndex","MTIndex",
                                        "NumberChild","EnrolledHH","PercentEnrolled")])
colnames(BigFive) <- c(colnames(BigFive[c("HouseholdID", "MPAID", "SettlementID", "SettlementName", "Treatment", 
                                          "InterviewYear", "MAIndex", "FSIndex", "PAIndex", 
                                          "MTIndex", "NumberChild", "EnrolledHH")]),"SERate")
BigFive$SERate <- ifelse(BigFive$SERate=="No Data" |
                           BigFive$SERate=="No School-Aged Children",NA,
                         as.numeric(BigFive$SERate)/100)

# ---- 2.3 Define "Big Five" data frame - averaged by settlement, for each monitoring year ----

BigFive.SettleGroup <- 
  BigFive %>%
  group_by(SettlementID,SettlementName,MPAID,Treatment) %>%
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



# ---- 2.4 Define "Big Five" data frame - averaged by control group (per MPA), for each monitoring year ----

BigFive.ControlGroup <- 
  BigFive[BigFive$Treatment==0,] %>%
  group_by(MPAID) %>%
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
BigFive.ControlGroup <- cbind.data.frame("SettlementID"=NA,
                                         "SettlementName"="Control",
                                         BigFive.ControlGroup[,"MPAID"],
                                         "Treatment"=0,
                                         BigFive.ControlGroup[,c("FSMean", "FSErr", "MAMean", "MAErr", "PAMean", 
                                                                 "PAErr", "MTMean", "MTErr", "SEMean", "SEErr")])

# ---- 2.5 Define "Big Five" data frame - averaged by MPA, for each monitoring year ----

BigFive.MPAGroup <- 
  BigFive[BigFive$Treatment==1,] %>%
  group_by(MPAID) %>%
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
BigFive.MPAGroup <- cbind.data.frame("SettlementID"=NA,
                                     "SettlementName"="MPA",
                                     BigFive.MPAGroup[,"MPAID"],
                                     "Treatment"=1,
                                     BigFive.MPAGroup[,c("FSMean", "FSErr", "MAMean", "MAErr", "PAMean", 
                                                    "PAErr", "MTMean", "MTErr", "SEMean", "SEErr")])

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Technical Report Datasets, Proportional Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Wrangle data frame ----

# cFS <- HHData[,c("HouseholdID", "MPAID", "SettlementID", "InterviewYear", "RemovecFS", 
#                  "LowCostFoodClean", "ChildBalancedMealClean", "ChildNotEnoughClean", 
#                  "ChildPortionClean", "ChildHungryClean", "ChildSkipClean", "FreqChildSkipClean", 
#                  "NoMealChildClean")]
# cFS$cFS <- ifelse(cFS$RemovecFS=="No",
#                   rowSums(cFS[,c("LowCostFoodClean", "ChildBalancedMealClean", "ChildNotEnoughClean", 
#                               "ChildPortionClean", "ChildHungryClean", "ChildSkipClean", "FreqChildSkipClean", 
#                               "NoMealChildClean")],
#                           na.rm=TRUE),
#                   NA)
# cFS$cat.cFS <- ifelse(cFS$cFS>=6.9,"Evidence",
#                       ifelse(cFS$cFS<6.9,"No or insufficient evidence",NA))

# MPANames <- data.frame(MPAID=c(15:19),
#                        MPAName=c("Alor","Flotim","Kei","Koon","Yamdena"))

HHDemos.context.1 <- 
  left_join(HHDemos[,c("HouseholdID", "MPAID", "SettlementID", "ReligionClean")],
                               HeadOfHH,
                               by="HouseholdID") %>%
  left_join(.,BigFive[,c("HouseholdID", "SettlementName", "Treatment", "InterviewYear", "SERate")],
            by="HouseholdID") %>%
  left_join(.,HHLivelihood[,c("HouseholdID", "PrimaryLivelihoodClean", "SecondaryLivelihoodClean")],
            by="HouseholdID")

colnames(HHDemos.context.1) <- c("HouseholdID","MPAID","SettlementID","ReligionClean","HeadofHH.gender","HeadofHH.education","HeadofHH.age",
                                 "SettlementName","Treatment","InterviewYear","SERate","PrimaryLivelihoodClean","SecondaryLivelihoodClean")

# HHDemos.context.1 <- left_join(HHDemos.context.1,
#                                cFS[,c("HouseholdID", "cat.cFS")],
#                                by="HouseholdID")
# colnames(HHDemos.context.1) <- c("HouseholdID", "MPAID", "SettlementID", "ReligionClean", "YrResidentClean", 
#                                  "MonitoringYear", "MPAName", "HeadofHH.gender","HeadofHH.educ","HeadofHH.age","SettlementName", "Treatment", "InterviewYear", "SERate", 
#                                  "PrimaryLivelihoodClean", "SecondaryLivelihoodClean", "TertiaryLivelihoodClean", "FreqFishClean", "FreqSaleFishClean", "PercentIncFishClean", 
#                                  "MajFishTechniqueClean", "FreqEatFishClean", "PercentProteinFishClean", "TimeMarketClean", "Child.FS.category")
# HHDemos.context.1$InterviewYear <- factor(HHDemos.context.1$InterviewYear,
#                                           levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
#                                           ordered=T)
# HHDemos.context.1$HHsize <- sapply(HHDemos.context.1$HouseholdID,
#                                    function(i){
#                                      c(length(IndDemos$HouseholdID[which(IndDemos$HouseholdID==i)]))
#                                    })

HHDemos.context <- HHDemos.context.1[HHDemos.context.1$Treatment==1,]


# ---- 3.2 Settlement-level analysis, for status and annex plots ----

Techreport.BySett <- 
  HHDemos.context %>%
  group_by(SettlementID) %>%
  summarise(MPAID=unique(MPAID),
            SettlementName=unique(SettlementName),
            HHH.female=(length(HeadofHH.gender[HeadofHH.gender==0 &
                                                 !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
            HHH.male=(length(HeadofHH.gender[HeadofHH.gender==1 &
                                               !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
            Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
                                                          !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
                                                       !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
                                                      !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
                                                                          !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
                                                                              !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
                                                                        !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==4 &
                                                                        !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==5 &
                                                                        !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==996 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Fish=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==3 &
                                                                     !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Farm=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==1 &
                                                                     !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.WageLabor=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==7 &
                                                                          !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.HarvestForest=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==2 &
                                                                              !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Tourism=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==6 &
                                                                        !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Aquaculture=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==4 &
                                                                            !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Extraction=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==5 &
                                                                           !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Other=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==996 & !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100)

Techreport.BySett <- Techreport.BySett[!is.na(Techreport.BySett$SettlementID),]


#  ---- 3.3 MPA-level analysis, for trend plots ----

Techreport.ByMPA <- 
  HHDemos.context %>%
  group_by(MPAID) %>%
  summarise(HHH.female=(length(HeadofHH.gender[HeadofHH.gender==0 &
                                                 !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
            HHH.male=(length(HeadofHH.gender[HeadofHH.gender==1 &
                                               !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
            Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
                                                          !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
                                                       !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
                                                      !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
                                                                          !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
                                                                              !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
                                                                        !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==4 &
                                                                            !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==5 &
                                                                           !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==996 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Fish=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==3 &
                                                                         !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Farm=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==1 &
                                                                         !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.WageLabor=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==7 &
                                                                              !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.HarvestForest=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==2 &
                                                                                  !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Tourism=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==6 &
                                                                            !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Aquaculture=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==4 &
                                                                                !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Extraction=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==5 &
                                                                               !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Other=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==996 & !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100)

Techreport.ByMPA <- Techreport.ByMPA[!is.na(Techreport.ByMPA$MPAID),]


# --- COMPARE MPAS TO CONTROLS, for most recent monitoring year 
#     (not used in any significance tests or data frames, just for comparing proportional variables by sight)
Techreport.ByMPA.control <- 
  HHDemos.context.1[HHDemos.context.1$Treatment==0,] %>%
  group_by(MPAID) %>%
  summarise(HHH.female=(length(HeadofHH.gender[HeadofHH.gender==0 &
                                                 !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
            HHH.male=(length(HeadofHH.gender[HeadofHH.gender==1 &
                                               !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
            Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
                                                          !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
                                                       !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
                                                      !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
            Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
                                                                          !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
                                                                              !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
                                                                        !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==4 &
                                                                            !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==5 &
                                                                           !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==996 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Fish=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==3 &
                                                                         !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Farm=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==1 &
                                                                         !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.WageLabor=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==7 &
                                                                              !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.HarvestForest=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==2 &
                                                                                  !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Tourism=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==6 &
                                                                            !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Aquaculture=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==4 &
                                                                                !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Extraction=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==5 &
                                                                               !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100,
            Percent.SecondaryOcc.Other=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==996 & !is.na(SecondaryLivelihoodClean)])/length(SecondaryLivelihoodClean[!is.na(SecondaryLivelihoodClean)]))*100)

# 
# 
# FishProtein.BySett <- 
#   HHDemos.context %>%
#   group_by(SettlementID,MonitoringYear) %>%
#   summarise(MPAID=unique(MPAID),
#             SettlementName=unique(SettlementName),
#             Percent.EatFish.RareOrNever=(length(FreqEatFishClean[FreqEatFishClean==1 &
#                                                                    !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFishClean[FreqEatFishClean==2 &
#                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPerMo=(length(FreqEatFishClean[FreqEatFishClean==3 &
#                                                                      !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPerWk=(length(FreqEatFishClean[FreqEatFishClean==4 &
#                                                                      !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.MoreFewTimesWk=(length(FreqEatFishClean[FreqEatFishClean==5 &
#                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             ProteinFish.None=(length(PercentProteinFishClean[PercentProteinFishClean==1 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Some=(length(PercentProteinFishClean[PercentProteinFishClean==2 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Half=(length(PercentProteinFishClean[PercentProteinFishClean==3 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Most=(length(PercentProteinFishClean[PercentProteinFishClean==4 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.All=(length(PercentProteinFishClean[PercentProteinFishClean==5 &
#                                                               !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100)
# 
# 
# FishProtein.ByMPA <- 
#   HHDemos.context %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(Percent.EatFish.RareOrNever=(length(FreqEatFishClean[FreqEatFishClean==1 &
#                                                                    !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFishClean[FreqEatFishClean==2 &
#                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPerMo=(length(FreqEatFishClean[FreqEatFishClean==3 &
#                                                                      !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPerWk=(length(FreqEatFishClean[FreqEatFishClean==4 &
#                                                                      !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.MoreFewTimesWk=(length(FreqEatFishClean[FreqEatFishClean==5 &
#                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             ProteinFish.None=(length(PercentProteinFishClean[PercentProteinFishClean==1 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Some=(length(PercentProteinFishClean[PercentProteinFishClean==2 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Half=(length(PercentProteinFishClean[PercentProteinFishClean==3 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Most=(length(PercentProteinFishClean[PercentProteinFishClean==4 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.All=(length(PercentProteinFishClean[PercentProteinFishClean==5 &
#                                                               !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100)  
# 
# FishProtein.ByMPA.control <- 
#   CurrentDemos.control %>%
#   group_by(MPAID) %>%
#   summarise(Percent.EatFish.RareOrNever=(length(FreqEatFishClean[FreqEatFishClean==1 &
#                                                                    !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFishClean[FreqEatFishClean==2 &
#                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPerMo=(length(FreqEatFishClean[FreqEatFishClean==3 &
#                                                                      !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.FewTimesPerWk=(length(FreqEatFishClean[FreqEatFishClean==4 &
#                                                                      !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             Percent.EatFish.MoreFewTimesWk=(length(FreqEatFishClean[FreqEatFishClean==5 &
#                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#             ProteinFish.None=(length(PercentProteinFishClean[PercentProteinFishClean==1 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Some=(length(PercentProteinFishClean[PercentProteinFishClean==2 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Half=(length(PercentProteinFishClean[PercentProteinFishClean==3 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.Most=(length(PercentProteinFishClean[PercentProteinFishClean==4 &
#                                                                !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#             ProteinFish.All=(length(PercentProteinFishClean[PercentProteinFishClean==5 &
#                                                               !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100)  
# 
# 
# # - Number years resident by settlement, to look for signs of rapid immigration 
# # - Changes in social conflict by settlement
# # - Marine tenure components by settlement
# 
# 
# HHDemos.context$InterviewYear <- as.integer(HHDemos.context$InterviewYear)
# 
# Synth.techreport.bySett <-
#   left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict")]) %>%
#   left_join(MT) %>%
#   left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex")]) %>%
#   group_by(SettlementID,MPAID,MonitoringYear) %>%
#   summarise(SettlementName=unique(SettlementName),
#             YrResident=mean(YrResidentClean,na.rm=T),
#             Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
#                                                                    !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
#                                                                    !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
#                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             MTManage=mean(RightsManageClean,na.rm=T),
#             MTHarvest=mean(RightsHarvestClean,na.rm=T),
#             MTAccess=mean(RightsAccessClean,na.rm=T),
#             MTTransfer=mean(RightsTransferClean,na.rm=T),
#             MTExclude=mean(RightsExcludeClean,na.rm=T),
#             MatAssets.gini=gini(MAIndex),
#             MAIndex=mean(MAIndex,na.rm=T),
#             Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
# 
# 
# # - same data as above, by MPA
# Synth.techreport.byMPA <-
#   left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict")]) %>%
#   left_join(MT) %>%
#   left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex")]) %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(YrResident=mean(YrResidentClean,na.rm=T),
#             Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
#                                                                    !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
#                                                                    !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
#                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             MTManage=mean(RightsManageClean,na.rm=T),
#             MTHarvest=mean(RightsHarvestClean,na.rm=T),
#             MTAccess=mean(RightsAccessClean,na.rm=T),
#             MTTransfer=mean(RightsTransferClean,na.rm=T),
#             MTExclude=mean(RightsExcludeClean,na.rm=T),
#             MatAssets.gini=gini(MAIndex),
#             MAIndex=mean(MAIndex,na.rm=T),
#             Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
# 
# # - same data as above but for control, by MPA
# 
# CurrentDemos.control$InterviewYear <- as.integer(CurrentDemos.control$InterviewYear)
# 
# Synth.techreport.byMPA.control <-
#   left_join(CurrentDemos.control,HHData[,c("HouseholdID","SocialConflict")]) %>%
#   left_join(MT) %>%
#   left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex")]) %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(YrResident=mean(YrResidentClean,na.rm=T),
#             Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
#                                                                    !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
#                                                                    !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
#                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             MTManage=mean(RightsManageClean,na.rm=T),
#             MTHarvest=mean(RightsHarvestClean,na.rm=T),
#             MTAccess=mean(RightsAccessClean,na.rm=T),
#             MTTransfer=mean(RightsTransferClean,na.rm=T),
#             MTExclude=mean(RightsExcludeClean,na.rm=T),
#             MatAssets.gini=gini(MAIndex),
#             MAIndex=mean(MAIndex,na.rm=T),
#             Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
# 
# 
# 
# # Days unwell by MPA and settlement
# Days.unwell <- group_by(IndDemos,HouseholdID)
# Days.unwell <- summarise(Days.unwell,
#                          DaysUnwell=sum(DaysUnwellClean,na.rm=T)/length(HouseholdID))
# Days.unwell <- left_join(Days.unwell,
#                          BigFive[,c("HouseholdID", "MPAID", "SettlementID", "SettlementName", "Treatment", 
#                                     "InterviewYear", "MonitoringYear")])
# Days.unwell <- left_join(Days.unwell,
#                          MPA.currentyear)
# 
# Days.unwell.treatment <- Days.unwell[Days.unwell$Treatment==1,]
# 
# # ---Days unwell, by MPA ("Days.unwell.ByMPA" is most current cross-section of data)
# Days.unwell.ByMPA <- 
#   Days.unwell.treatment %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
#             UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))
# 
# # ---Days unwell, by settlement ("Days.unwell.BySett" is most current cross-section of data)
# Days.unwell.BySett <- 
#   Days.unwell.treatment %>%
#   group_by(SettlementID,MPAID,MonitoringYear) %>%
#   summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
#             UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))
# 
# Days.unwell.control <- 
#   Days.unwell[Days.unwell$InterviewYear==Days.unwell$CurrentYear &
#                 Days.unwell$Treatment==0,] %>%
#   group_by(MPAID) %>%
#   summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
#             UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))
# 
# # Individual Age and Gender breakdowns, by MPA
# AgeGenderDemos <- left_join(HHDemos.context[,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName")],
#                             IndDemos[,c("HouseholdID", "IndividualGenderClean", "IndividualAgeClean")])
# 
# AgeGenderDemos.control <- left_join(HHDemos.context.1[HHDemos.context.1$Treatment==0,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName")],
#                                     IndDemos[,c("HouseholdID", "IndividualGenderClean", "IndividualAgeClean")])
# 
# AgeGender.AvgAge.byMPA <-
#   AgeGenderDemos %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(AvgAge=mean(IndividualAgeClean,na.rm=T))
# 
# AgeGender.AvgAge.bySett <-
#   AgeGenderDemos %>%
#   group_by(SettlementName,MPAID,MonitoringYear) %>%
#   summarise(AvgAge=mean(IndividualAgeClean,na.rm=T))
# 
# AgeGender.AvgAge.control <-
#   AgeGenderDemos.control %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(AvgAge=mean(IndividualAgeClean,na.rm=T))
# 
# 
# AgeGenderDemos.ByMPA <- 
#   AgeGenderDemos %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(Male.0.4=(length(IndividualAgeClean[IndividualAgeClean<=4 &
#                                                   !is.na(IndividualAgeClean) &
#                                                   IndividualGenderClean==1&
#                                                   !is.na(IndividualGenderClean)])/
#                         length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                     !is.na(IndividualGenderClean)]))*-100,
#             Female.0.4=(length(IndividualAgeClean[IndividualAgeClean<=4 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==0&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*100,
#             Male.5.9=(length(IndividualAgeClean[IndividualAgeClean>4 & 
#                                                   IndividualAgeClean<=9 &
#                                                   !is.na(IndividualAgeClean) &
#                                                   IndividualGenderClean==1&
#                                                   !is.na(IndividualGenderClean)])/
#                         length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                     !is.na(IndividualGenderClean)]))*-100,
#             Female.5.9=(length(IndividualAgeClean[IndividualAgeClean>4 & 
#                                                     IndividualAgeClean<=9 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==0&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*100,
#             Male.10.14=(length(IndividualAgeClean[IndividualAgeClean>9 &
#                                                     IndividualAgeClean<=14 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.10.14=(length(IndividualAgeClean[IndividualAgeClean>9 &
#                                                       IndividualAgeClean<=14 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.15.19=(length(IndividualAgeClean[IndividualAgeClean>14 &
#                                                     IndividualAgeClean<=19 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.15.19=(length(IndividualAgeClean[IndividualAgeClean>14 &
#                                                       IndividualAgeClean<=19 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.20.24=(length(IndividualAgeClean[IndividualAgeClean>19 &
#                                                     IndividualAgeClean<=24 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.20.24=(length(IndividualAgeClean[IndividualAgeClean>19 &
#                                                       IndividualAgeClean<=24 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.25.29=(length(IndividualAgeClean[IndividualAgeClean>24 &
#                                                     IndividualAgeClean<=29 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.25.29=(length(IndividualAgeClean[IndividualAgeClean>24 &
#                                                       IndividualAgeClean<=29 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.30.34=(length(IndividualAgeClean[IndividualAgeClean>29 &
#                                                     IndividualAgeClean<=34 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.30.34=(length(IndividualAgeClean[IndividualAgeClean>29 &
#                                                       IndividualAgeClean<=34 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.35.39=(length(IndividualAgeClean[IndividualAgeClean>34 &
#                                                     IndividualAgeClean<=39 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.35.39=(length(IndividualAgeClean[IndividualAgeClean>34 &
#                                                       IndividualAgeClean<=39 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.40.44=(length(IndividualAgeClean[IndividualAgeClean>39 &
#                                                     IndividualAgeClean<=44 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)]))*-100,
#             Female.40.44=(length(IndividualAgeClean[IndividualAgeClean>39 &
#                                                       IndividualAgeClean<=44 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.45.49=(length(IndividualAgeClean[IndividualAgeClean>44 &
#                                                     IndividualAgeClean<=49 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.45.49=(length(IndividualAgeClean[IndividualAgeClean>44 &
#                                                       IndividualAgeClean<=49 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.50.54=(length(IndividualAgeClean[IndividualAgeClean>49 &
#                                                     IndividualAgeClean<=54 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.50.54=(length(IndividualAgeClean[IndividualAgeClean>49 &
#                                                       IndividualAgeClean<=54 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.55.59=(length(IndividualAgeClean[IndividualAgeClean>54 &
#                                                     IndividualAgeClean<=59 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.55.59=(length(IndividualAgeClean[IndividualAgeClean>54 &
#                                                       IndividualAgeClean<=59 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.60.64=(length(IndividualAgeClean[IndividualAgeClean>59 &
#                                                     IndividualAgeClean<=64 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.60.64=(length(IndividualAgeClean[IndividualAgeClean>59 &
#                                                       IndividualAgeClean<=64 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.65.69=(length(IndividualAgeClean[IndividualAgeClean>64 &
#                                                     IndividualAgeClean<=69 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.65.69=(length(IndividualAgeClean[IndividualAgeClean>64 &
#                                                       IndividualAgeClean<=69 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.70.74=(length(IndividualAgeClean[IndividualAgeClean>69 &
#                                                     IndividualAgeClean<=74 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.70.74=(length(IndividualAgeClean[IndividualAgeClean>69 &
#                                                       IndividualAgeClean<=74 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.75.79=(length(IndividualAgeClean[IndividualAgeClean>74 &
#                                                     IndividualAgeClean<=79 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.75.79=(length(IndividualAgeClean[IndividualAgeClean>74 &
#                                                       IndividualAgeClean<=79 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.80.84=(length(IndividualAgeClean[IndividualAgeClean>79 &
#                                                     IndividualAgeClean<=84 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.80.84=(length(IndividualAgeClean[IndividualAgeClean>79 &
#                                                       IndividualAgeClean<=84 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.85.89=(length(IndividualAgeClean[IndividualAgeClean>84 &
#                                                     IndividualAgeClean<=89 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.85.89=(length(IndividualAgeClean[IndividualAgeClean>84 &
#                                                       IndividualAgeClean<=89 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.90.94=(length(IndividualAgeClean[IndividualAgeClean>89 &
#                                                     IndividualAgeClean<=94 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.90.94=(length(IndividualAgeClean[IndividualAgeClean>89 &
#                                                       IndividualAgeClean<=94 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0&
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100,
#             Male.95.99=(length(IndividualAgeClean[IndividualAgeClean>94 &
#                                                     IndividualAgeClean<=99 &
#                                                     !is.na(IndividualAgeClean) &
#                                                     IndividualGenderClean==1&
#                                                     !is.na(IndividualGenderClean)])/
#                           length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                       !is.na(IndividualGenderClean)]))*-100,
#             Female.95.99=(length(IndividualAgeClean[IndividualAgeClean>94 &
#                                                       IndividualAgeClean<=99 &
#                                                       !is.na(IndividualAgeClean) &
#                                                       IndividualGenderClean==0 &
#                                                       !is.na(IndividualGenderClean)])/
#                             length(IndividualAgeClean[!is.na(IndividualAgeClean)&
#                                                         !is.na(IndividualGenderClean)]))*100)
# 
# AgeGenderDemos.ByMPA <- AgeGenderDemos.ByMPA[!is.na(AgeGenderDemos.ByMPA$MPAID),]

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Define Global Plot Themes and Guides ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


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
                                    SecondaryOcc=c("Percent.SecondaryOcc.Farm"=alpha("#A6CEE3",0.95), # light blue 
                                                 "Percent.SecondaryOcc.HarvestForest"=alpha("#7FC97F",0.95), # light green
                                                 "Percent.SecondaryOcc.Fish"=alpha("#1F78B4",0.95), # blue                                                  "Percent.PrimaryOcc.Tourism"=alpha("#C23737",0.95), # red
                                                 "Percent.SecondaryOcc.WageLabor"=alpha("#33A02C",0.95), # green 
                                                 "Percent.SecondaryOcc.Tourism"=alpha("#E31A1C",0.95), # red 
                                                 "Percent.SecondaryOcc.Aquaculture"=alpha("#FDBF6F",0.95), # blue (X)
                                                 "Percent.SecondaryOcc.Extraction"=alpha("#FB9A99",0.95), # light red (X)
                                                 "Percent.SecondaryOcc.Other"=alpha("#FF7F00",0.95)), # orange 
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
                                   "Mean place attachment","Mean household marine tenure",
                                   "School enrollment (% children ages 5-18 years old)",
                                   "Mean travel time to closest market (hours)",
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



### HOW TO CREATE HIGH RES IMAGES
# png("Social impacts, BHS -- Kelly/MPA impact summaries/Kofiau/Kof.enrol.impact.legend2.png",
# units="in",height=10,width=10,res=500)
# plot(kof.MPAimpact.summ.se.i)
# dev.off()

