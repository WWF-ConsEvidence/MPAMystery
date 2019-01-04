# 
# code:  Aggregated BHS-level Technical Report Datasets
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/BHS
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: April 2018
# modified: 
# 
# 
# ---- inputs ----
#  1) Source BHS.Seascape.TechReport.SigTests.R 
#     - Dependencies: BHS_MPA_Mystery.R
# 
# ---- code sections ----
#  1) Data Sourcing, Configuration, and Subsetting
#  2) Define Datasets for Status, Trend, and Annex Plots for Export
#  3) Export Data to Excel
#  4) Synthesize other social data for interpretation/context
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Data Sourcing, Configuration, and Subsetting ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Source statistical test results from "Dampier.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/BHS.Seascape.TechReport.SigTests.R")


# ---- 1.2 Create BigFive.BHSGroup ----

BigFive.BHSGroup <- 
  BigFive[BigFive$Treatment==1,] %>%
  group_by(MonitoringYear) %>%
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


Days.unwell.ByMPA

Days.unwell.BHS 


# ---- 1.3 Proportional Data of Age/Gender for BHS ----

BHS.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.BHS[AgeGenderDemos.BHS$MonitoringYear=="Baseline",
                                                  seq(2,40,by=2)]),
             Female.Baseline=t(AgeGenderDemos.BHS[AgeGenderDemos.BHS$MonitoringYear=="Baseline",
                                                    seq(3,41,by=2)]),
             Male.2yr=t(AgeGenderDemos.BHS[AgeGenderDemos.BHS$MonitoringYear=="2 Year Post",
                                             seq(2,40,by=2)]),
             Female.2yr=t(AgeGenderDemos.BHS[AgeGenderDemos.BHS$MonitoringYear=="2 Year Post",
                                               seq(3,41,by=2)]),
             Male.4yr=t(AgeGenderDemos.BHS[AgeGenderDemos.BHS$MonitoringYear=="4 Year Post",
                                             seq(2,40,by=2)]),
             Female.4yr=t(AgeGenderDemos.BHS[AgeGenderDemos.BHS$MonitoringYear=="4 Year Post",
                                               seq(3,41,by=2)]),
             row.names=NULL)


# ---- 1.4 Seascape-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

BHS.level.PropData.status <- 
  data.frame(MonitoringYear="4 Year Post",
             MPAID=0,
             MPAName="Bird's Head Seascape",
             Techreport.BHSmeans[Techreport.BHSmeans$MonitoringYear=="4 Year Post",7:40],
             FishProtein.BHSmeans[FishProtein.BHSmeans$MonitoringYear=="4 Year Post",7:11])

BHS.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   MPAID=0, 
                   MPAName="Bird's Head Seascape",
                   Techreport.BHSmeans[,7:40],
                   FishProtein.BHSmeans[,7:11])

null.row.PropData <- 
  matrix(rep(NA,42),ncol=42,dimnames=list(NULL,colnames(BHS.level.PropData.status)))


# ---- 1.5 Seascape-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

BHS.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",
                   MPAID=0,
                   MPAName="Bird's Head Seascape",
                   BigFive.BHSGroup[BigFive.BHSGroup$MonitoringYear=="4 Year Post",2:11],
                   Techreport.BHSmeans[Techreport.BHSmeans$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.BHS[Days.unwell.BHS$MonitoringYear=="4 Year Post",
                                             c("UnwellMean","UnwellErr")])

BHS.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   MPAID=0,
                   MPAName="Bird's Head Seascape",
                   BigFive.BHSGroup[,2:11],
                   Techreport.BHSmeans[,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.BHS[,c("UnwellMean","UnwellErr")])

BHS.level.ContData.baseline <-
  cbind.data.frame(MonitoringYear="Baseline",
                   MPAID=0,
                   MPAName="Bird's Head Seascape",
                   BigFive.BHSGroup[BigFive.BHSGroup$MonitoringYear=="Baseline",2:11],
                   Days.unwell.BHS[Days.unwell.BHS$MonitoringYear=="Baseline",
                                   c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(BHS.level.ContData.status))))


null.row.ContData.baseline <- 
  cbind.data.frame(matrix(rep(NA,15),ncol=15,dimnames=list(NULL,colnames(BHS.level.ContData.baseline))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for BHS, proportional data ----

BHS.PropData.Techreport.status <- 
  left_join(Techreport.ByMPA[Techreport.ByMPA$MonitoringYear=="4 Year Post",1],
            data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                              "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")),
            by="MPAID") %>%
  left_join(Techreport.ByMPA[Techreport.ByMPA$MonitoringYear=="4 Year Post",c(1,3:36)],
            by="MPAID") %>%
  left_join(FishProtein.ByMPA[FishProtein.ByMPA$MonitoringYear=="4 Year Post",c(1,8:12)],
            by="MPAID")


BHS.PropData.Techreport.status <- 
  BHS.PropData.Techreport.status[rev(order(BHS.PropData.Techreport.status$MPAName)),]

BHS.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(BHS.level.PropData.status[2:42],
                   null.row.PropData[2:42],
                   BHS.PropData.Techreport.status)

# - make MPAName an ordered factor for plotting
BHS.PropData.Techreport.status.PLOTFORMAT$MPAName <-
  ifelse(is.na(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName),"",
         as.character(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName))

BHS.PropData.Techreport.status.PLOTFORMAT$MPAName <-
  factor(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName,
         levels=unique(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName),
         ordered=T)

# - add row for plot fill colour formatting
BHS.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for BHS, continuous data (with p values) ----

BHS.ContData.Techreport.status <- 
  data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                              "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="4 Year Post",c(3,6:15)],
            by="MPAID") %>%
  left_join(Techreport.ByMPA[Techreport.ByMPA$MonitoringYear=="4 Year Post",c("MPAID","TimeMarketMean","TimeMarketErr")],
            by="MPAID") %>%
  left_join(Days.unwell.ByMPA[Days.unwell.ByMPA$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="MPAID")

BHS.ContData.Techreport.status <- 
  BHS.ContData.Techreport.status[rev(order(BHS.ContData.Techreport.status$MPAName)),]

BHS.ContData.Techreport.status.withBHSlevel <- 
  rbind.data.frame(BHS.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   BHS.ContData.Techreport.status)

# - plot-formatted dataset
BHS.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(BHS.ContData.Techreport.status.withBHSlevel,
            sigvals.BHS,by="MPAName")

# - make MPAName an ordered factor for plotting
BHS.ContData.Techreport.status.PLOTFORMAT$MPAName <-
  ifelse(is.na(BHS.ContData.Techreport.status.PLOTFORMAT$MPAName)," ",
         as.character(BHS.ContData.Techreport.status.PLOTFORMAT$MPAName))

BHS.ContData.Techreport.status.PLOTFORMAT$MPAName <-
  factor(BHS.ContData.Techreport.status.PLOTFORMAT$MPAName,
         levels=unique(BHS.ContData.Techreport.status.PLOTFORMAT$MPAName),
         ordered=T)

# - add row for plot fill colour formatting
BHS.ContData.Techreport.status.PLOTFORMAT$MPALevel <- 
  ifelse(BHS.ContData.Techreport.status.PLOTFORMAT$MPAName=="","Dummy","NotDummy")


# ---- 2.2b FOR BASELINE, status dataset for BHS, continuous data (with p values) ----

BHS.ContData.Techreport.baseline <- 
  data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                    "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",c(3,6:15)],
            by="MPAID") %>%
  left_join(Days.unwell.ByMPA[Days.unwell.ByMPA$MonitoringYear=="Baseline",c(1,3,4)],
            by="MPAID")

BHS.ContData.Techreport.baseline <- 
  BHS.ContData.Techreport.baseline[rev(order(BHS.ContData.Techreport.baseline$MPAName)),]

BHS.ContData.Techreport.baseline.withBHSlevel <- 
  rbind.data.frame(BHS.level.ContData.baseline[2:15],
                   null.row.ContData.baseline[2:15],
                   BHS.ContData.Techreport.baseline)

# - plot-formatted dataset
BHS.ContData.Techreport.baseline.PLOTFORMAT <- 
  left_join(BHS.ContData.Techreport.baseline.withBHSlevel,
            sigvals.BHS.baseline,by="MPAName")

# - make MPAName an ordered factor for plotting
BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName <-
  ifelse(is.na(BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName)," ",
         as.character(BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName))

BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName <-
  factor(BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName,
         levels=unique(BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName),
         ordered=T)

# - add row for plot fill colour formatting
BHS.ContData.Techreport.baseline.PLOTFORMAT$MPALevel <- 
  ifelse(BHS.ContData.Techreport.baseline.PLOTFORMAT$MPAName=="","Dummy","NotDummy")



# ---- 2.3 Trend dataset for BHS, Seascape-level proportional data ----

BHS.TrendPropData.Techreport.PLOTFORMAT <- 
  left_join(Techreport.BHSmeans[,c(1,7:40)],
            FishProtein.BHSmeans[,c(1,7:11)],
            by="MonitoringYear")


# ---- 2.4 Trend dataset for BHS, Seascape-level continuous data (with p values) ----

BHS.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(BHS.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.BHS)

# - make MonitoringYear an ordered factor for plotting
BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)


# ---- 2.5 Annex dataset for BHS, MPA-level proportional data ----

BHS.AnnexPropData.Techreport <- 
  left_join(Techreport.ByMPA[,c(2,1)],
            data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                              "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")),
            by="MPAID") %>%
  left_join(Techreport.ByMPA[,c(1:36)],
            by=c("MonitoringYear","MPAID")) %>%
  left_join(FishProtein.ByMPA[,c(1,2,8:12)],
            by=c("MonitoringYear","MPAID"))

BHS.AnnexPropData.Techreport <- 
  BHS.AnnexPropData.Techreport[rev(order(BHS.AnnexPropData.Techreport$MPAName)),]

BHS.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(BHS.level.PropData.annex[BHS.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   BHS.level.PropData.annex[BHS.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   BHS.level.PropData.annex[BHS.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData,
                   BHS.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for BHS, MPA-level continuous data (with p values) ----

BHS.AnnexContData.Techreport <- 
  left_join(BigFive.MPAGroup[,c(3,5)],
            data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                              "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")),
            by="MPAID") %>%
  left_join(BigFive.MPAGroup[,c(3,5,6:15)],
            by=c("MonitoringYear","MPAID")) %>%
  left_join(Techreport.ByMPA[,c("MPAID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("MonitoringYear","MPAID")) %>%
  left_join(Days.unwell.ByMPA,
            by=c("MonitoringYear","MPAID"))

BHS.AnnexContData.Techreport$MonitoringYear <- 
  factor(BHS.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

BHS.AnnexContData.Techreport <- 
  BHS.AnnexContData.Techreport[rev(order(BHS.AnnexContData.Techreport$MPAName,
                                         BHS.AnnexContData.Techreport$MonitoringYear)),]

BHS.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(BHS.level.ContData.annex[BHS.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   BHS.level.ContData.annex[BHS.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   BHS.level.ContData.annex[BHS.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   BHS.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
BHS.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(BHS.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make MPAName an ordered factor for plotting
BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName <-
  ifelse(is.na(BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName),"",
         BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName)

BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName <-
  factor(BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName,
         levels=unique(BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName),
         ordered=T)

# - add row for plot fill colour formatting
BHS.AnnexContData.Techreport.PLOTFORMAT$MPALevel <- 
  ifelse(BHS.AnnexContData.Techreport.PLOTFORMAT$MPAName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/BHS_level_Analysis/TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")

# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(BHS.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(BHS.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(BHS.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(propdata.trend.test.BHS,FileName,sheetName='PropData_TrendPlot_pvals',row.names=F,append=T)
write.xlsx(BHS.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(BHS.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(BHS.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.BHS,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(BHS.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 4: Synthesize other social data for interpretation/context ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
# # ---- 4.1 Tech report data synthesis aid ---- 
# #   years resident, categorical food security, changes in social conflict, 
# #   material assets gini coefficient, mean material assets, % fishers, 
# #   % wage labor, marine tenure manage and harvest components
# 
# Dampier.level.synth <- data.frame(SettlementID=NA,
#                                   Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==5,c("MPAID","MonitoringYear")],
#                                   SettlementName="MPA",
#                                   Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==5,3:length(Synth.techreport.byMPA)],
#                                   AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==5,3])
# Dampier.level.synth <- left_join(Dampier.level.synth,
#                                  Techreport.ByMPA[c("MPAID","MonitoringYear",
#                                                     "Percent.PrimaryOcc.Fish",
#                                                     "Percent.PrimaryOcc.WageLabor")],
#                                  by=c("MPAID","MonitoringYear"))
# 
# null.row.synth <- matrix(NA,ncol=length(colnames(Dampier.level.synth)),
#                          dimnames=list(NULL,colnames(Dampier.level.synth)))
# 
# Dampier.setts.synth <- 
#   Synth.techreport.bySett[Synth.techreport.bySett$MPAID==5,] %>%
#   left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
#                                  "Percent.PrimaryOcc.Fish",
#                                  "Percent.PrimaryOcc.WageLabor")],
#             by=c("SettlementID","MonitoringYear")) %>%
#   left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])
# 
# 
# # ---- 4.2 Output for data synthesis/interpretation ----
# 
# Dampier.synth.techreport <- rbind.data.frame(Dampier.level.synth,
#                                              null.row.synth,
#                                              Dampier.setts.synth)
# 
# 
# write.xlsx(Dampier.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)
# 
# 
# # ---- 4.3 Playing around ----
# 
# # Compare frequency of fishing sales for entire population vs. only fishing households
# Dampier.FreqSellFish.bySett <- 
#   HHDemos.context[HHDemos.context$MPAID==5,] %>%
#   group_by(SettlementID,MonitoringYear) %>%
#   summarise(SettlementName=unique(SettlementName),
#             Prop.FishingHouseholds=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             WholePop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean) &
#                                                                      PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                              !is.na(PrimaryLivelihoodClean)]))*100,
#             WholePop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean) &
#                                                                         PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                 !is.na(PrimaryLivelihoodClean)]))*100,
#             WholePop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean) &
#                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                !is.na(PrimaryLivelihoodClean)]))*100,
#             WholePop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean) &
#                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                !is.na(PrimaryLivelihoodClean)]))*100,
#             WholePop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean) &
#                                                                         PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                 !is.na(PrimaryLivelihoodClean)]))*100)
# Dampier.FreqSellFish <- 
#   HHDemos.context[HHDemos.context$MPAID==5,] %>%
#   group_by(MonitoringYear) %>%
#   summarise(Prop.FishingHouseholds=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
#                                                                     !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             WholePop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean) &
#                                                                      PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                              !is.na(PrimaryLivelihoodClean) &
#                                                                                                                                                              PrimaryLivelihoodClean==3]))*100,
#             WholePop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean) &
#                                                                         PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                 !is.na(PrimaryLivelihoodClean) &
#                                                                                                                                                                 PrimaryLivelihoodClean==3]))*100,
#             WholePop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean) &
#                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                !is.na(PrimaryLivelihoodClean) &
#                                                                                                                                                                PrimaryLivelihoodClean==3]))*100,
#             WholePop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean) &
#                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                !is.na(PrimaryLivelihoodClean) &
#                                                                                                                                                                PrimaryLivelihoodClean==3]))*100,
#             WholePop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
#             FishPop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean) &
#                                                                         PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
#                                                                                                                                                                 !is.na(PrimaryLivelihoodClean) &
#                                                                                                                                                                 PrimaryLivelihoodClean==3]))*100)
# 
# # Is food security status linked to occupation?
# Dampier.foodsec.byocc <-
#   left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
#             HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
#   subset(MPAID==5) %>%
#   group_by(SettlementID,MonitoringYear) %>%
#   summarise(SettlementName=unique(SettlementName),
#             PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
#             FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
#             FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
#             FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
#             FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))
# 
# Dampier.foodsec.byocc$SettlementName <- factor(Dampier.foodsec.byocc$SettlementName,
#                                                levels=c(as.character(rev(sort(unique(Dampier.foodsec.byocc$SettlementName)))),"  "),
#                                                ordered=T)
# 


# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(BHS.level.PropData.status)
rm(BHS.level.ContData.status)
rm(BHS.level.PropData.annex)
rm(BHS.level.ContData.annex)
rm(null.row.PropData)
rm(null.row.ContData)
rm(BHS.PropData.Techreport.status)
rm(BHS.ContData.Techreport.status)
rm(BHS.AnnexPropData.Techreport)
rm(BHS.AnnexContData.Techreport)
rm(BHS.ContData.Techreport.status.withBHSlevel)
rm(FileName)