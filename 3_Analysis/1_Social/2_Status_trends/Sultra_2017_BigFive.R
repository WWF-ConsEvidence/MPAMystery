# 
# code: Social MPA Mystery Analysis, Sunda Banda Seascape
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: September 2016
# modified: January 2018
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
# ---- SECTION 1: Import Data, and Subset ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Import data ----


# Set working directory and import flat data files (.csv files)

# HHData <- read.csv('2_Social/FlatDataFiles/SBS/SBS_HHData.csv',header=T,sep=',')
# 
# IndDemos <- read.csv('2_Social/FlatDataFiles/SBS/SBS_HHDemos.csv',header=T,sep=',')
# IndDemos <- left_join(IndDemos,HHData[,c("HouseholdID","SettlementID")],by="HouseholdID")
# 
# Settlements <- read.csv('2_Social/FlatDataFiles/SBS/SBS_HH_tbl_SETTLEMENT.csv',header=T,sep=',')
# Settlements <- Settlements[,c(1,3:5)]
# Settlements$SettlementName <- as.character(Settlements$SettlementName)


# ---- 1.2 Define monitoring year for each MPA ----

# MonitoringYear <- group_by(HHData,MPAID)
# MonitoringYear <- summarise(MonitoringYear,
#                             Baseline=min(InterviewYear),
#                             ThreeYear=as.integer(min(InterviewYear)+3),
#                             SixYear=as.integer(min(InterviewYear)+6))
# MonitoringYear <- left_join(HHData[,c("HouseholdID","MPAID")],
#                             MonitoringYear,
#                             by="MPAID")
# 
# HHData$MonitoringYear <- factor(mapply(a=HHData$HouseholdID,
#                                 b=HHData$InterviewYear,
#                                 function(a,b){
#                                   ifelse(b==MonitoringYear$Baseline[MonitoringYear$HouseholdID==a],"Baseline",
#                                          ifelse(b==MonitoringYear$ThreeYear[MonitoringYear$HouseholdID==a],"3 Year Post",
#                                                 ifelse(b==MonitoringYear$SixYear[MonitoringYear$HouseholdID==a],"6 Year Post",NA)))
#                                 }),
#                                 levels=c("Baseline","3 Year Post","6 Year Post"),
#                                 ordered=T)

# ---- 1.3 Subset variables from HHData for Big Five calculations & descriptive statistics ----

FS <- HHData[,c(1:4,11:17)]
MA <- HHData[,c(1,29:40)]
PA <- HHData[,c(1,47:53)]
MT <- HHData[,c(1,59:64)]

# HHLivelihood <- HHData[,c(1:3,79:90)]
# HHDemos <- HHData[,c(1:3,91:94,99)]
# HeadOfHH <- IndDemos[IndDemos$RelationHHH==0 &
#                        !is.na(IndDemos$RelationHHH),1:4]
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: "Big Five" Indexes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Compute indexes ----

MA$MAIndex <- ifelse(MA$RemoveMA=="No",
                     rowSums(MA[,3:13],
                             na.rm=TRUE),
                     NA)

PA$PAIndex <- ifelse(PA$RemovePA=="No",
                     round(rowMeans(PA[,3:8],
                              na.rm=TRUE),2),
                     NA)

MT$MTIndex <- ifelse(MT$RemoveMT=="No",
                     rowSums(MT[,3:7],
                             na.rm=TRUE),
                     NA)

FS$FSIndex <- as.character(ifelse(FS$RemoveFS=="No",
                     rowSums(FS[,6:11],
                             na.rm=TRUE),
                     NA))
FS$FSIndex <- revalue(FS$FSIndex, c("0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06"))
FS$FSIndex <- 6.06-as.numeric(FS$FSIndex)

MA.PA.MT.FS <- left_join(MA,PA,by="HouseholdID")
MA.PA.MT.FS <- left_join(MA.PA.MT.FS,MT,by="HouseholdID")
MA.PA.MT.FS <- left_join(MA.PA.MT.FS,FS,by="HouseholdID")

IndDemos.1 <- 
  IndDemos %>%
  dplyr::group_by(HouseholdID) %>%
  dplyr::summarise(NumberChild=sum(ChildOrAdult,na.rm=T),
            EnrolledHH=sum(ChildEnrolled,na.rm=T),
            PercentEnrolled=ifelse(NumberChild!=0 & !is.na(EnrolledHH),
                                   as.character(round((EnrolledHH/NumberChild)*100,2)),
                                   ifelse(NumberChild==0,
                                          "No School-Aged Children","No Data")))

BigFive.allvar <- left_join(MA.PA.MT.FS,IndDemos.1,by="HouseholdID")
BigFive.allvar <- left_join(BigFive.allvar,Settlements,by=c("SettlementID","MPAID"))

# ---- 2.2 Define "Big Five" data frame - at household level, all MPAs, all years ----

BigFive <- data.frame(BigFive.allvar[,c("HouseholdID","MPAID","SettlementID","SettlementName",
                                        "Treatment","InterviewYear","MAIndex",
                                        "FSIndex","PAIndex","MTIndex","NumberChild","EnrolledHH",
                                        "PercentEnrolled")])
colnames(BigFive) <- c(colnames(BigFive[1:12]),"SERate")
BigFive$SERate <- ifelse(BigFive$SERate=="No Data" |
                            BigFive$SERate=="No School-Aged Children",NA,
                         as.numeric(BigFive$SERate)/100)

BigFive <- 
  BigFive %>%
  filter(.,!is.na(HouseholdID))

# ---- 2.3 Define "Big Five" data frame - averaged by settlement, for each monitoring year ----

BigFive.SettleGroup <- 
  BigFive %>%
  dplyr::group_by(SettlementID,SettlementName,MPAID,Treatment) %>%
  dplyr::summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
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

BigFive.ControlGroup <- cbind.data.frame(SettlementID=NA,
                                         SettlementName="Control",
                                         MPAID=20,
                                         Treatment=0,
                                         BigFive.ControlGroup[,1:10])

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

BigFive.MPAGroup <- cbind.data.frame(SettlementID=NA,
                                     SettlementName="MPA",
                                     MPAID=20,
                                     Treatment=0,
                                     BigFive.MPAGroup[,1:10])


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Technical Report Datasets, Proportional Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Wrangle data frame ----

# cFS <- HHData[,c(1:4,70:78)]
# cFS$cFS <- ifelse(cFS$RemovecFS=="No",
#                   rowSums(cFS[,6:13],
#                           na.rm=TRUE),
#                   NA)
# cFS$cat.cFS <- ifelse(cFS$cFS>=6.9,"Evidence",
#                       ifelse(cFS$cFS<6.9,"No or insufficient evidence",NA))
# 
# MPANames <- data.frame(MPAID=c(15:19),
#                        MPAName=c("Alor","Flotim","Kei","Koon","Yamdena"))
# 
# HHDemos.context.1 <- left_join(HHDemos[,c(1:5,8)],
#                                MPANames,
#                              by="MPAID")
# HHDemos.context.1 <- left_join(HHDemos.context.1,
#                              HeadOfHH,
#                              by="HouseholdID")
# HHDemos.context.1 <- left_join(HHDemos.context.1,
#                              BigFive[,c(1,4:6,14)],
#                              by="HouseholdID")
# HHDemos.context.1 <- left_join(HHDemos.context.1,
#                              HHLivelihood[,c(1,4:10,12:13,15)],
#                              by="HouseholdID")
# HHDemos.context.1 <- left_join(HHDemos.context.1,
#                              cFS[,c(1,15)],
#                              by="HouseholdID")
# colnames(HHDemos.context.1) <- c(colnames(HHDemos.context.1)[c(1:7)],
#                                "HeadofHH.gender","HeadofHH.educ","HeadofHH.age",
#                                colnames(BigFive)[c(4:6,14)],
#                                colnames(HHLivelihood)[c(4:10,12:13,15)],
#                                "Child.FS.category")
# HHDemos.context.1$InterviewYear <- factor(HHDemos.context.1$InterviewYear,
#                                         levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
#                                         ordered=T)
# HHDemos.context.1$HHsize <- sapply(HHDemos.context.1$HouseholdID,
#                                  function(i){
#                                    c(length(IndDemos$HouseholdID[which(IndDemos$HouseholdID==i)]))
#                                  })
# HHDemos.context <- HHDemos.context.1[HHDemos.context.1$Treatment==1,]
# 
# MPA.currentyear <- group_by(HHDemos.context.1,MPAID)
# MPA.currentyear <- summarise(MPA.currentyear,
#                              CurrentYear=max(InterviewYear))
# MPA.currentyear$CurrentYear <- factor(MPA.currentyear$CurrentYear,
#                                       levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
#                                       ordered=T)
# 
# 
# CurrentDemos.context <- left_join(HHDemos.context.1,MPA.currentyear,by="MPAID")
# CurrentDemos.context <- CurrentDemos.context[CurrentDemos.context$InterviewYear==CurrentDemos.context$CurrentYear &
#                                                CurrentDemos.context$Treatment==1,]
# CurrentDemos.control <- left_join(HHDemos.context.1,MPA.currentyear,by="MPAID")
# CurrentDemos.control <- CurrentDemos.control[CurrentDemos.control$InterviewYear==CurrentDemos.control$CurrentYear &
#                                                CurrentDemos.control$Treatment==0,]
# 
# # ---- 3.2 Settlement-level analysis, for status and annex plots ----
# 
# Techreport.BySett <- 
#   HHDemos.context %>%
#   group_by(SettlementID,MonitoringYear) %>%
#   summarise(MPAID=unique(MPAID),
#             SettlementName=unique(SettlementName),
#             HHH.female=(length(HeadofHH.gender[HeadofHH.gender==0 &
#                                                  !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
#             HHH.male=(length(HeadofHH.gender[HeadofHH.gender==1 &
#                                                !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
#             Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
#                                                           !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#             Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
#                                                        !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#             Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
#                                                       !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#             Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                      !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
#                                                                      !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
#                                                                           !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
#                                                                               !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
#                                                                         !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[(PrimaryLivelihoodClean==996 | PrimaryLivelihoodClean==4 | 
#                                                                        PrimaryLivelihoodClean==5) & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Prop.Fish.AlmostNever=(length(FreqFishClean[FreqFishClean==1 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.FewTimesPer6Mo=(length(FreqFishClean[FreqFishClean==2 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.FewTimesPerMo=(length(FreqFishClean[FreqFishClean==3 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.FewTimesPerWk=(length(FreqFishClean[FreqFishClean==4 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.MoreFewTimesWk=(length(FreqFishClean[FreqFishClean==5 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.IncFish.None=(length(PercentIncFishClean[PercentIncFishClean==1 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.Some=(length(PercentIncFishClean[PercentIncFishClean==2 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.Half=(length(PercentIncFishClean[PercentIncFishClean==3 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.Most=(length(PercentIncFishClean[PercentIncFishClean==4 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.All=(length(PercentIncFishClean[PercentIncFishClean==5 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.FishTech.ByHand=(length(MajFishTechniqueClean[MajFishTechniqueClean==1 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.StatNet=(length(MajFishTechniqueClean[MajFishTechniqueClean==2 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.MobileNet=(length(MajFishTechniqueClean[MajFishTechniqueClean==3 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.StatLine=(length(MajFishTechniqueClean[MajFishTechniqueClean==4 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.MobileLine=(length(MajFishTechniqueClean[MajFishTechniqueClean==5 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Child.FS.no=(length(Child.FS.category[Child.FS.category=="No or insufficient evidence" & !is.na(Child.FS.category)])/length(Child.FS.category[!is.na(Child.FS.category)]))*100,
#             Child.FS.yes=(length(Child.FS.category[Child.FS.category=="Evidence" & !is.na(Child.FS.category)])/length(Child.FS.category[!is.na(Child.FS.category)]))*100,
#             TimeMarketMean=mean(TimeMarketClean,na.rm=T),
#             TimeMarketErr=sd(TimeMarketClean,na.rm=T)/sqrt(length(TimeMarketClean)))
# 
# Techreport.BySett <- Techreport.BySett[!is.na(Techreport.BySett$SettlementID),]
# 
# 
# #  ---- 3.3 MPA-level analysis, for trend plots ----
# 
# Techreport.ByMPA <- 
#   HHDemos.context %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(HHH.female=(length(HeadofHH.gender[HeadofHH.gender==0 &
#                                                  !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
#             HHH.male=(length(HeadofHH.gender[HeadofHH.gender==1 &
#                                                !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
#             Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
#                                                           !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#             Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
#                                                        !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#             Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
#                                                       !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#             Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                      !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
#                                                                      !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
#                                                                           !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
#                                                                               !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
#                                                                         !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[(PrimaryLivelihoodClean==996 | PrimaryLivelihoodClean==4 | 
#                                                                        PrimaryLivelihoodClean==5) & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             Prop.Fish.AlmostNever=(length(FreqFishClean[FreqFishClean==1 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.FewTimesPer6Mo=(length(FreqFishClean[FreqFishClean==2 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.FewTimesPerMo=(length(FreqFishClean[FreqFishClean==3 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.FewTimesPerWk=(length(FreqFishClean[FreqFishClean==4 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.Fish.MoreFewTimesWk=(length(FreqFishClean[FreqFishClean==5 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#             Prop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             Prop.IncFish.None=(length(PercentIncFishClean[PercentIncFishClean==1 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.Some=(length(PercentIncFishClean[PercentIncFishClean==2 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.Half=(length(PercentIncFishClean[PercentIncFishClean==3 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.Most=(length(PercentIncFishClean[PercentIncFishClean==4 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.IncFish.All=(length(PercentIncFishClean[PercentIncFishClean==5 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#             Prop.FishTech.ByHand=(length(MajFishTechniqueClean[MajFishTechniqueClean==1 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.StatNet=(length(MajFishTechniqueClean[MajFishTechniqueClean==2 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.MobileNet=(length(MajFishTechniqueClean[MajFishTechniqueClean==3 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.StatLine=(length(MajFishTechniqueClean[MajFishTechniqueClean==4 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Prop.FishTech.MobileLine=(length(MajFishTechniqueClean[MajFishTechniqueClean==5 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#             Child.FS.no=(length(Child.FS.category[Child.FS.category=="No or insufficient evidence" & !is.na(Child.FS.category)])/length(Child.FS.category[!is.na(Child.FS.category)]))*100,
#             Child.FS.yes=(length(Child.FS.category[Child.FS.category=="Evidence" & !is.na(Child.FS.category)])/length(Child.FS.category[!is.na(Child.FS.category)]))*100,
#             TimeMarketMean=mean(TimeMarketClean,na.rm=T),
#             TimeMarketErr=sd(TimeMarketClean,na.rm=T)/sqrt(length(TimeMarketClean)))
# 
# Techreport.ByMPA <- Techreport.ByMPA[!is.na(Techreport.ByMPA$MPAID),]
# 
# # --- COMPARE MPAS TO CONTROLS, for most recent monitoring year 
# #     (not used in any significance tests or data frames, just for comparing proportional variables by sight)
# Techreport.ByMPA.control <- group_by(CurrentDemos.control,MPAID)
# Techreport.ByMPA.control <- summarise(Techreport.ByMPA.control,
#                                       HHH.female=(length(HeadofHH.gender[HeadofHH.gender==0 &
#                                                                            !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
#                                       HHH.male=(length(HeadofHH.gender[HeadofHH.gender==1 &
#                                                                          !is.na(HeadofHH.gender)])/length(HeadofHH.gender[!is.na(HeadofHH.gender)]))*100,
#                                       Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
#                                                                                     !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#                                       Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
#                                                                                  !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#                                       Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
#                                                                                 !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#                                       Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                                                !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                       Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
#                                                                                                !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                       Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
#                                                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                       Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
#                                                                                                         !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                       Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
#                                                                                                   !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                       Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[(PrimaryLivelihoodClean==996 | PrimaryLivelihoodClean==4 | 
#                                                                                                  PrimaryLivelihoodClean==5) & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                       Prop.Fish.AlmostNever=(length(FreqFishClean[FreqFishClean==1 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                       Prop.Fish.FewTimesPer6Mo=(length(FreqFishClean[FreqFishClean==2 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                       Prop.Fish.FewTimesPerMo=(length(FreqFishClean[FreqFishClean==3 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                       Prop.Fish.FewTimesPerWk=(length(FreqFishClean[FreqFishClean==4 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                       Prop.Fish.MoreFewTimesWk=(length(FreqFishClean[FreqFishClean==5 & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                       Prop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#                                       Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#                                       Prop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#                                       Prop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#                                       Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#                                       Prop.IncFish.None=(length(PercentIncFishClean[PercentIncFishClean==1 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#                                       Prop.IncFish.Some=(length(PercentIncFishClean[PercentIncFishClean==2 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#                                       Prop.IncFish.Half=(length(PercentIncFishClean[PercentIncFishClean==3 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#                                       Prop.IncFish.Most=(length(PercentIncFishClean[PercentIncFishClean==4 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#                                       Prop.IncFish.All=(length(PercentIncFishClean[PercentIncFishClean==5 & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#                                       Prop.FishTech.ByHand=(length(MajFishTechniqueClean[MajFishTechniqueClean==1 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#                                       Prop.FishTech.StatNet=(length(MajFishTechniqueClean[MajFishTechniqueClean==2 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#                                       Prop.FishTech.MobileNet=(length(MajFishTechniqueClean[MajFishTechniqueClean==3 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#                                       Prop.FishTech.StatLine=(length(MajFishTechniqueClean[MajFishTechniqueClean==4 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#                                       Prop.FishTech.MobileLine=(length(MajFishTechniqueClean[MajFishTechniqueClean==5 & !is.na(MajFishTechniqueClean)])/length(MajFishTechniqueClean[!is.na(MajFishTechniqueClean)]))*100,
#                                       Child.FS.no=(length(Child.FS.category[Child.FS.category=="No or insufficient evidence" & !is.na(Child.FS.category)])/length(Child.FS.category[!is.na(Child.FS.category)]))*100,
#                                       Child.FS.yes=(length(Child.FS.category[Child.FS.category=="Evidence" & !is.na(Child.FS.category)])/length(Child.FS.category[!is.na(Child.FS.category)]))*100,
#                                       TimeMarketMean=mean(TimeMarketClean,na.rm=T),
#                                       TimeMarketErr=sd(TimeMarketClean,na.rm=T)/sqrt(length(TimeMarketClean)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: MPA Impact Summary/Technical Report Introduction Stats ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# 4.1 Define dataframe for MPA Impact Summary/MPA Tech Report Intros
# 
# KeyFishSpecies.FGdata <- read.csv("Social impacts, BHS -- Kelly/R codes & data/FGD Fish Species Data 2016_1130.csv",
#                                   sep=",",header=T)
# 
# KeyFishSpecies <- KeyFishSpecies.FGdata[,c(3,6,7)]
# colnames(KeyFishSpecies) <- c("Year","SettlementID","Species")
# KeyFishSpecies <- left_join(KeyFishSpecies,
#                             BigFive.SettleGroup[,c(31,32,34)],
#                             by="SettlementID")
# KeyFishSpecies <- KeyFishSpecies[KeyFishSpecies$Treatment==1,c(1:4)]
# KeyFishSpecies <- left_join(KeyFishSpecies,
#                             MPA.currentyear,
#                             by="MPAID")
# KeyFishSpecies <- KeyFishSpecies[ifelse(KeyFishSpecies$MPAID==1 | KeyFishSpecies$MPAID==2,
#                                         KeyFishSpecies$Year==KeyFishSpecies$CurrentYear,
#                                         KeyFishSpecies$Year==2013 | KeyFishSpecies$Year==2014),c(1:4)]
# 
# KeyFishSpecies.ExactMatches <- rbind.data.frame(c(1,paste(as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==1]),decreasing=T),n=5))[,1],
#                                                           as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==1]),decreasing=T),n=5))[,2],sep=", ")),
#                                                 c(2,paste(as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==2]),decreasing=T),n=5))[,1],
#                                                           as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==2]),decreasing=T),n=5))[,2],sep=", ")),
#                                                 c(3,paste(as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==3]),decreasing=T),n=5))[,1],
#                                                           as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==3]),decreasing=T),n=5))[,2],sep=", ")),
#                                                 c(4,paste(as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==4]),decreasing=T),n=5))[,1],
#                                                           as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==4]),decreasing=T),n=5))[,2],sep=", ")),
#                                                 c(5,paste(as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==5]),decreasing=T),n=5))[,1],
#                                                           as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==5]),decreasing=T),n=5))[,2],sep=", ")),
#                                                 c(6,paste(as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==6]),decreasing=T),n=5))[,1],
#                                                           as.data.frame(head(sort(table(KeyFishSpecies$Species[KeyFishSpecies$MPAID==6]),decreasing=T),n=5))[,2],sep=", ")))
# colnames(KeyFishSpecies.ExactMatches) <- c("MPAID","MostCited","2ndMostCited","3rdMostCited","4thMostCited","5thMostCited")


# MPAimpact.intro.context <- group_by(CurrentDemos.context,MPAID)
# MPAimpact.intro.context <- summarise(MPAimpact.intro.context,
#                                      MPAName=unique(MPAName),
#                                      HH.size=mean(HHsize,na.rm=T),
#                                      HHH.age.min=min(HeadofHH.age,na.rm=T),
#                                      HHH.age.max=max(HeadofHH.age,na.rm=T),
#                                      HHH.age.mean=mean(HeadofHH.age,na.rm=T),
#                                      HH.YrRes=mean(YrResidentClean,na.rm=T),
#                                      Percent.Rel.Christian=(length(ReligionClean[ReligionClean==1 &
#                                                                                    !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#                                      Percent.Rel.Muslim=(length(ReligionClean[ReligionClean==2 &
#                                                                                 !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#                                      Percent.Rel.Other=(length(ReligionClean[ReligionClean!=1 & ReligionClean!=2 &
#                                                                                !is.na(ReligionClean)])/length(ReligionClean[!is.na(ReligionClean)]))*100,
#                                      Percent.PrimaryOcc.Fish=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                                               !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                      Percent.PrimaryOcc.Farm=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
#                                                                                               !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                      Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
#                                                                                                    !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                      Percent.PrimaryOcc.Other=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean!=3 &
#                                                                                                PrimaryLivelihoodClean!=1 &
#                                                                                                PrimaryLivelihoodClean!=7 &
#                                                                                                !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#                                      Percent.SecondOcc.Fish=(length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==3 &
#                                                                                                !is.na(SecondaryLivelihoodClean)])/length(HouseholdID[!is.na(PrimaryLivelihoodClean)]))*100,
#                                      Percent.Fish.MoreThanFewPerMonth=(length(FreqFishClean[(FreqFishClean==3 | FreqFishClean==4 |
#                                                                                              FreqFishClean==5) & !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                      Percent.Fish.RareOrNever=(length(FreqFishClean[FreqFishClean==1 &
#                                                                                       !is.na(FreqFishClean)])/length(FreqFishClean[!is.na(FreqFishClean)]))*100,
#                                      FishTech.MostUsed=tail(names(sort(table(MajFishTechniqueClean))), 1),
#                                      Percent.HalfIncome.Fish=(length(PercentIncFishClean[(PercentIncFishClean==3 | PercentIncFishClean==4 |
#                                                                                             PercentIncFishClean==5) & !is.na(PercentIncFishClean)])/length(PercentIncFishClean[!is.na(PercentIncFishClean)]))*100,
#                                      Percent.EatFishWeekly=(length(FreqEatFishClean[(FreqEatFishClean==4 | FreqEatFishClean==5) & 
#                                                                                       !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#                                      Percent.EatFishDaily=(length(FreqEatFishClean[FreqEatFishClean==5 & !is.na(FreqEatFishClean)])/length(FreqEatFishClean[!is.na(FreqEatFishClean)]))*100,
#                                      Percent.HalfProtein.Fish=(length(PercentProteinFishClean[(PercentProteinFishClean==3 | PercentProteinFishClean==4 |
#                                                                                                  PercentProteinFishClean==5) & !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
#                                      Percent.RelyFish=((length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                                        !is.na(PrimaryLivelihoodClean)]) + 
#                                                          length(SecondaryLivelihoodClean[SecondaryLivelihoodClean==3 &
#                                                                                            !is.na(SecondaryLivelihoodClean)]) +
#                                                          length(TertiaryLivelihoodClean[TertiaryLivelihoodClean==3 &
#                                                                                           !is.na(TertiaryLivelihoodClean)]))/length(HouseholdID[!is.na(PrimaryLivelihoodClean)]))*100,
#                                      TimeMarketErr=sd(TimeMarketClean,na.rm=T)/sqrt(length(TimeMarketClean)))
# 
# MPAimpact.intro.context$Settlement.LeastFishers <- sapply(MPAimpact.intro.context$MPAID,
#                                                           function(i){
#                                                             min <- tail(sort(Techreport.BySett$Percent.PrimaryOcc.Fish[Techreport.BySett$MPAID==i],decreasing=T),1)
#                                                             paste(as.character(Techreport.BySett$SettlementName[Techreport.BySett$Percent.PrimaryOcc.Fish==min &
#                                                                                                             Techreport.BySett$MPAID==i]),paste(round(min,2),"%",sep=""),sep=", ")
#                                                           })
# MPAimpact.intro.context$Settlement.MostFishers <- sapply(MPAimpact.intro.context$MPAID,
#                                                          function(i){
#                                                            max <- head(sort(Techreport.BySett$Percent.PrimaryOcc.Fish[Techreport.BySett$MPAID==i],decreasing=T),1)
#                                                            paste(as.character(Techreport.BySett$SettlementName[Techreport.BySett$Percent.PrimaryOcc.Fish==max &
#                                                                                                            Techreport.BySett$MPAID==i]),paste(round(max,2),"%",sep=""),sep=", ")
#                                                          })
# 
# # - Fish consumption by settlement or MPA, per monitoring year, 
# #   to glean some insight into primary occupation and fishing behavior changes
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
#                                                         !is.na(PercentProteinFishClean)])/length(PercentProteinFishClean[!is.na(PercentProteinFishClean)]))*100,
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
# Synth.techreport.bySett <-
#   left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict")]) %>%
#   left_join(MT) %>%
#   left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex")]) %>%
#   group_by(SettlementID,MPAID,MonitoringYear) %>%
#   summarise(SettlementName=unique(SettlementName),
#             YrResident=mean(YrResidentClean,na.rm=T),
#             Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
#                                                     !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
#                                                            !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
#                                                           !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             MTManage=mean(RightsManageClean,na.rm=T),
#             MTHarvest=mean(RightsHarvestClean,na.rm=T),
#             MatAssets.gini=gini(MAIndex),
#             MAIndex=mean(MAIndex,na.rm=T),
#             Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
# 
# # - same data as above, by MPA
# Synth.techreport.byMPA <-
#   left_join(HHDemos.context,HHData[,c("HouseholdID","SocialConflict")]) %>%
#   left_join(MT) %>%
#   left_join(BigFive[,c("HouseholdID","MAIndex","FSIndex")]) %>%
#   group_by(MPAID,MonitoringYear) %>%
#   summarise(YrResident=mean(YrResidentClean,na.rm=T),
#             Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
#                                                                 !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
#                                                                 !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
#                                                                !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
#             MTManage=mean(RightsManageClean,na.rm=T),
#             MTHarvest=mean(RightsHarvestClean,na.rm=T),
#             MatAssets.gini=gini(MAIndex),
#             MAIndex=mean(MAIndex,na.rm=T),
#             Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
# 
# # - same data as above but for control, by MPA
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
#             MatAssets.gini=gini(MAIndex),
#             MAIndex=mean(MAIndex,na.rm=T),
#             Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
#             Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)
# 
# 
# # # Sampling Frame info -- develop estimates for settlement populations
# # SamplingFrame <- read.delim("Social impacts, BHS -- Kelly/R codes & data/SamplingFrameMPA.csv",header=T,sep=",")
# # SamplingFrame$TotalPop <- SamplingFrame$TotalHouseholds*MPAimpact.intro.context$HH.size
# 
# 
# # Days unwell by MPA and settlement
# Days.unwell <- group_by(IndDemos,HouseholdID)
# Days.unwell <- summarise(Days.unwell,
#                          DaysUnwell=sum(DaysUnwellClean,na.rm=T)/length(HouseholdID))
# Days.unwell <- left_join(Days.unwell,
#                          BigFive[,1:7],
#                          by="HouseholdID")
# Days.unwell <- left_join(Days.unwell,
#                          MPA.currentyear,
#                          by="MPAID")
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
# 
# # Compare.prop.data <- rbind.data.frame(data.frame(Treatment="MPA",Techreport.BySett.MPA[,-4]),
# #                                       data.frame(Treatment="Control",Techreport.BySett.control[,-4]))
# # Compare.prop.data <- Compare.prop.data[order(Compare.prop.data$MPAID),]
# # 
# # Compare.cont.data <- rbind.data.frame(data.frame(Treatment="MPA",BigFive.MPAGroup[,c(31,5,11,17,23,29)],
# #                                                  Techreport.BySett.MPA[,4],Days.unwell.ByMPA[,2]),
# #                                       data.frame(Treatment="Control",BigFive.ControlGroup[,c(31,5,11,17,23,29)],
# #                                                  Techreport.BySett.control[,4],Days.unwell.control[,2]))
# # Compare.cont.data <- Compare.cont.data[order(Compare.cont.data$MPAID),]
# 
# 
# # Individual Age and Gender breakdowns, by MPA
# AgeGenderDemos <- left_join(HHDemos.context[,c(1:3,6:7,11)],
#                             IndDemos[,c(1,2,4)],
#                             by="HouseholdID")
# 
# AgeGenderDemos.control <- left_join(HHDemos.context.1[HHDemos.context.1$Treatment==0,c(1:3,6:7,11)],
#                             IndDemos[,c(1,2,4)],
#                             by="HouseholdID")
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
# ---- SECTION 5: Define Global Plot Themes and Guides ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 MPA Technical Report plot themes ----

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


# age.gender.plot.theme <- theme(axis.ticks=element_blank(),
#                                panel.background=element_rect(fill="white",
#                                                              colour="#909090"),
#                                panel.border=element_rect(fill=NA,
#                                                          size=0.25,
#                                                          colour="#C0C0C0"),
#                                panel.grid.major.x=element_line(colour="#C0C0C0",
#                                                                size=0.25,
#                                                                linetype=3),
#                                panel.grid.major.y=element_blank(),
#                                axis.title=element_text(size=rel(0.8),
#                                                        angle=0,
#                                                        face="bold",
#                                                        colour="#303030"),
#                                axis.text=element_text(size=rel(0.6),
#                                                       angle=0,
#                                                       colour="#303030"),
#                                legend.position="top",
#                                legend.justification="right",
#                                legend.box.spacing=unit(0.1,"cm"))



fillcols.status <- c("NotDummy"=alpha("#2C7FB8",0.95),"Dummy"=alpha("#FFFFFF",0))
fillcols.trend <- c(alpha("#2C7FB8",0.95))

errcols.status <- c("NotDummy"=alpha("#21577C",0.95),"Dummy"=alpha("#FFFFFF",0))
errcols.trend <- c(alpha("#21577C",0.95))
# 
# multianswer.fillcols.status <- list(Gender=c("HHH.male"=alpha("#253494",0.95),
#                                              "HHH.female"=alpha("#7FCDBB",0.95)),
#                                     Religion=c("Percent.Rel.Christian"=alpha("#253494",0.95),
#                                                "Percent.Rel.Muslim"=alpha("#7FCDBB",0.95),
#                                                "Percent.Rel.Other"=alpha("#FDC086",0.95)),
#                                     PrimaryOcc=c("Percent.PrimaryOcc.Farm"=alpha("#7FC97F",0.95),
#                                                  "Percent.PrimaryOcc.HarvestForest"=alpha("#BEAED4",0.95),
#                                                  "Percent.PrimaryOcc.Fish"=alpha("#FDC086",0.95),
#                                                  "Percent.PrimaryOcc.Tourism"=alpha("#E1E198",0.95),
#                                                  "Percent.PrimaryOcc.WageLabor"=alpha("#386CB0",0.95),
#                                                  "Percent.PrimaryOcc.Other"=alpha("#C23737",0.95)),
#                                     FreqFish=c("Prop.Fish.AlmostNever"=alpha("#E1E198",0.95),
#                                                "Prop.Fish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
#                                                "Prop.Fish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
#                                                "Prop.Fish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
#                                                "Prop.Fish.MoreFewTimesWk"=alpha("#253494",0.95)),
#                                     FreqSellFish=c("Prop.SellFish.AlmostNever"=alpha("#E1E198",0.95),
#                                                    "Prop.SellFish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
#                                                    "Prop.SellFish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
#                                                    "Prop.SellFish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
#                                                    "Prop.SellFish.MoreFewTimesWk"=alpha("#253494",0.95)),
#                                     IncFish=c("Prop.IncFish.None"=alpha("#E1E198",0.95),
#                                               "Prop.IncFish.Some"=alpha("#7FCDBB",0.95),
#                                               "Prop.IncFish.Half"=alpha("#2CA9B8",0.95),
#                                               "Prop.IncFish.Most"=alpha("#2C7FB8",0.95),
#                                               "Prop.IncFish.All"=alpha("#253494",0.95)),
#                                     FishTech=c("Prop.FishTech.ByHand"=alpha("#7FC97F",0.95),
#                                                "Prop.FishTech.StatNet"=alpha("#BEAED4",0.95),
#                                                "Prop.FishTech.MobileNet"=alpha("#FDC086",0.95),
#                                                "Prop.FishTech.StatLine"=alpha("#E1E198",0.95),
#                                                "Prop.FishTech.MobileLine"=alpha("#386CB0",0.95)),
#                                     ChildFS=c("Child.FS.no"=alpha("#253494",0.95),
#                                               "Child.FS.yes"=alpha("#7FCDBB",0.95)),
#                                     Protein=c("ProteinFish.None"=alpha("#E1E198",0.95),
#                                               "ProteinFish.Some"=alpha("#7FCDBB",0.95),
#                                               "ProteinFish.Half"=alpha("#2CA9B8",0.95),
#                                               "ProteinFish.Most"=alpha("#2C7FB8",0.95),
#                                               "ProteinFish.All"=alpha("#253494",0.95)))
# 
# # ---- 5.2 MPA Impact Summary plot themes ----
# 
# fill.cols.MPAimpact.summ <- c("MPA"=alpha("#1B448B",0.85),"Control"=alpha("#6B6B6B",0.4))
# err.cols.MPAimpact.summ <- c(alpha("#0A1D4E",0.5),alpha("#242424",0.25))
# 
# snapshot.plot.theme.MPAimpact.summ <- theme(panel.background=element_blank(),
#                                        panel.grid.major.x=element_line(size=0.25,
#                                                                        colour="#D0D0D0D0"),
#                                        panel.grid.major.y=element_line(size=0.5,
#                                                                        colour="#D0D0D0D0"),
#                                        panel.grid.minor.x=element_blank(),
#                                        axis.ticks=element_blank(),
#                                        axis.text=element_text(size=11,
#                                                               angle=0,
#                                                               colour="#505050",
#                                                               hjust=0.5),
#                                        axis.title=element_text(size=12,
#                                                                angle=0,
#                                                                face="bold",
#                                                                colour="#505050"),
#                                        plot.title=element_text(colour="#505050",
#                                                                face="bold",
#                                                                size=14),
#                                        panel.border=element_rect(size=0.5,
#                                                                  colour="#D0D0D0",
#                                                                  linetype=3,
#                                                                  fill=NA))
# 
# plot.theme.MPAimpact.summ <- theme(axis.ticks=element_blank(),
#                               axis.text=element_text(vjust=0.5,
#                                                      size=rel(1.1),
#                                                      colour="#505050"),
#                               axis.title.y=element_text(face="bold",
#                                                         size=rel(1.15),
#                                                         angle=90,
#                                                         colour="#505050"),
#                               axis.title.x=element_blank(),
#                               plot.title=element_blank(),
#                               panel.background=element_rect(fill="white",
#                                                             colour="#D0D0D0"),
#                               panel.grid.major.x=element_blank(),
#                               panel.grid.major.y=element_line(size=0.5,
#                                                               colour="#808080",
#                                                               linetype=3),
#                               panel.grid.minor.y=element_blank(),
#                               panel.border=element_rect(fill=NA,
#                                                         colour="#D0D0D0"))
# 
# fs.st.plot.theme.MPAimpact.summ <- theme(axis.ticks.x=element_blank(),
#                                          axis.ticks.y=element_line(colour="#505050"),
#                                    axis.text=element_text(vjust=0.5,
#                                                           size=rel(1.1),
#                                                           colour="#505050"),
#                                    axis.title.y=element_text(face="bold",
#                                                              size=rel(1.15),
#                                                              angle=90,
#                                                              colour="#505050"),
#                                    axis.title.x=element_blank(),
#                                    plot.title=element_blank(),
#                                    panel.background=element_rect(fill="white",
#                                                                  colour="#D0D0D0"),
#                                    panel.grid.major.x=element_blank(),
#                                    panel.grid.major.y=element_blank(),
#                                    panel.grid.minor.y=element_blank(),
#                                    panel.border=element_rect(fill=NA,
#                                                              colour="#D0D0D0"))


# ---- 5.3 MPA Technical Report plot legend guide ----

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
# 
# # - Function to create common legend between multiple ggplots
# g_legend<- function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# # ---- 5.4 MPA Impact Summary plot legend guide ----
# 
# plot.guides.MPAimpact.summ <- guides(size=guide_legend(title=element_blank(),
#                                                   label.theme=element_text(size=9,
#                                                                            angle=0,
#                                                                            colour="#505050"),
#                                                   label.position="bottom",
#                                                   keywidth=unit(2.5,"cm"),
#                                                   label.hjust=0.5,
#                                                   order=2),
#                                 fill=guide_legend(title.hjust=0.5,
#                                                   title.theme=element_text(face="bold",
#                                                                            size=10,
#                                                                            angle=0,
#                                                                            colour="#505050"),
#                                                   label.theme=element_text(size=9,
#                                                                            angle=0,
#                                                                            colour="#505050"),
#                                                   order=1,
#                                                   ncol=2,
#                                                   nrow=1,
#                                                   title.position="top",
#                                                   label.position="bottom",
#                                                   keywidth=unit(1.2,"cm"),
#                                                   label.hjust=0.5),
#                                 colour=guide_legend(title=element_blank(),
#                                                     label.position="bottom",
#                                                     label.theme=element_text(size=9,
#                                                                              angle=0,
#                                                                              colour="#505050"),
#                                                     label.hjust=0.5,
#                                                     keywidth=unit(2.5,"cm"),
#                                                     order=3),
#                                 shape=guide_legend(title=element_blank(),
#                                                    label.position="right",
#                                                    label.theme=element_text(size=9,
#                                                                             angle=0,
#                                                                             colour="#505050"),
#                                                    label.hjust=0.6,
#                                                    order=4))
# 
# snapshot.plot.guide.MPAimpact.summ <- guides(fill=guide_legend(order=1,
#                                                           keywidth=unit(1,"cm"),
#                                                           label.theme=element_text(size=9,
#                                                                                    angle=0,
#                                                                                    colour="#505050"),
#                                                           label.position="right",
#                                                           label.hjust=0.5,
#                                                           label.vjust=0.5,
#                                                           title.theme=element_text(size=10,
#                                                                                    angle=0,
#                                                                                    colour="#505050",
#                                                                                    face="bold",
#                                                                                    lineheight=0.8),
#                                                           title.hjust=0.5),
#                                         linetype=guide_legend(order=3,
#                                                               keywidth=unit(2.5,"cm"),
#                                                               title.theme=element_text(size=10,
#                                                                                        angle=0,
#                                                                                        colour="#505050",
#                                                                                        lineheight=0.8,
#                                                                                        face="bold"),
#                                                               title.hjust=0.5,
#                                                               label=F))
# 
# 
# # ---- 5.5 MPA continuous variables distribution plot theme ----
# 
# dist.plot.theme <- theme(axis.ticks=element_blank(),
#                          plot.title=element_text(face="bold",
#                                                  colour="#303030",
#                                                  hjust=0.5),
#                          panel.background=element_rect(fill="white",
#                                                        colour="#909090"),
#                          panel.border=element_rect(fill=NA,
#                                                    size=0.25,
#                                                    colour="#C0C0C0"),
#                          panel.grid.major.x=element_line(colour="#C0C0C0",
#                                                          size=0.25),
#                          panel.grid.major.y=element_line(colour="#C0C0C0",
#                                                          size=0.25,
#                                                          linetype=3),
#                          axis.title=element_text(size=11,
#                                                  angle=0,
#                                                  face="bold",
#                                                  colour="#303030"),
#                          axis.text=element_text(size=10,
#                                                 angle=0,
#                                                 colour="#303030"))


# ---- 5.6 MPA Technical Report plot labels ----

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
                        IncFish=labs(y="Income from fishing in past 6 months (% households)",
                                     x="Settlement"),
                        FishTech=labs(y="Fishing technique most often used in past 6 months (% households)",
                                      x="Settlement"),
                        ChildFS=labs(y="Child hunger (% households)",x="Settlement"))

continuous.variables.plotlabs <- c("Mean household food security","Mean household assets",
                                   "Mean place attachment","Mean household marine tenure",
                                   "School enrollment (% children ages 5-18 years old)",
                                   "Mean travel time to closest market (hours)",
                                   "Mean time suffering from illness or injury in past 4 weeks (days)")


# ---- 5.7 MPA Impact Summary "Big Five" plot labels ----

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
