# 
# code:  Mayalibit Technical Report Datasets
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/BHS
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: November 2017
# 
# 
# ---- inputs ----
#  1) Source Mayalibit.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Mayalibit.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/Mayalibit.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Mayalibit.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==1 &
                       !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]

Days.unwell.Mayalibit.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==1 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Mayalibit ----

Mayalibit.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==1 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==1 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==1 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                             seq(3,41,by=2)]),
             Female.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==1 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                               seq(4,42,by=2)]),
             Male.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==1 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                             seq(3,41,by=2)]),
             Female.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==1 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                               seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Mayalibit.level.PropData.status <- 
  data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Teluk Mayalibit\nMPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==1 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:36],
               FishProtein.ByMPA[FishProtein.ByMPA$MPAID==1 &
                                   FishProtein.ByMPA$MonitoringYear=="4 Year Post",8:12],
             MPAimpact.intro.context[MPAimpact.intro.context$MPAID==1,c(12:15,33:36)])

Mayalibit.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Teluk Mayalibit\nMPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==1,3:36],
                   FishProtein.ByMPA[FishProtein.ByMPA$MPAID==1,8:12])

null.row.PropData <- 
  matrix(rep(NA,50),ncol=50,dimnames=list(NULL,colnames(Mayalibit.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Mayalibit.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Teluk Mayalibit\nMPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==1 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==1 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Mayalibit.ByMPA[Days.unwell.Mayalibit.ByMPA$MonitoringYear=="4 Year Post",
                                               c("UnwellMean","UnwellErr")])
Mayalibit.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Teluk Mayalibit\nMPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==1,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==1,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Mayalibit.ByMPA[,c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Mayalibit.level.ContData.status)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Mayalibit, proportional data ----

Mayalibit.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==1 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:38,41:48)],
            FishProtein.BySett[FishProtein.BySett$MPAID==1 &
                                 FishProtein.BySett$MonitoringYear=="4 Year Post",c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Mayalibit.PropData.Techreport.status <- 
  Mayalibit.PropData.Techreport.status[rev(order(Mayalibit.PropData.Techreport.status$SettlementName)),]

Mayalibit.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.PropData.status[2:50],
                   null.row.PropData[2:50],
                   Mayalibit.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Mayalibit.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Mayalibit.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Mayalibit, continuous data (with p values) ----

Mayalibit.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==1,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==1 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Mayalibit.ContData.Techreport.status <- 
  left_join(Mayalibit.ContData.Techreport.status,
            Days.unwell.Mayalibit.BySett[Days.unwell.Mayalibit.BySett$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="SettlementID")

Mayalibit.ContData.Techreport.status <- 
  Mayalibit.ContData.Techreport.status[rev(order(Mayalibit.ContData.Techreport.status$SettlementName)),]

Mayalibit.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Mayalibit.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Mayalibit.ContData.Techreport.status)

# - plot-formatted dataset
Mayalibit.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Mayalibit.ContData.Techreport.status.withMPA,
            sigvals.Maya,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Mayalibit.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for Mayalibit, MPA-level proportional data ----

Mayalibit.TrendPropData.Techreport.PLOTFORMAT <- 
  left_join(Techreport.ByMPA[Techreport.ByMPA$MPAID==1,c(2,1,3:36)],
            FishProtein.ByMPA[FishProtein.ByMPA$MPAID==1,c(2,8:12)],
            by="MonitoringYear")


# ---- 2.4 Trend dataset for Mayalibit, MPA-level continuous data (with p values) ----

Mayalibit.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Maya)

# - make MonitoringYear an ordered factor for plotting
Mayalibit.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Mayalibit.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)


# ---- 2.5 Annex dataset for Mayalibit, Settlement-level proportional data ----

Mayalibit.AnnexPropData.Techreport <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==1,c(2,1,4:38)],
            FishProtein.BySett[FishProtein.BySett$MPAID==1,c(2,1,4,10:14)],
            by=c("SettlementID","SettlementName","MonitoringYear"))

Mayalibit.AnnexPropData.Techreport <- 
  Mayalibit.AnnexPropData.Techreport[rev(order(Mayalibit.AnnexPropData.Techreport$SettlementName)),]

Mayalibit.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.PropData.annex[Mayalibit.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Mayalibit.level.PropData.annex[Mayalibit.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Mayalibit.level.PropData.annex[Mayalibit.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData[,-43:-51],
                   Mayalibit.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Mayalibit, Settlement-level continuous data (with p values) ----

Mayalibit.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==1 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==1,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

Mayalibit.AnnexContData.Techreport <- 
  left_join(Mayalibit.AnnexContData.Techreport,
            Days.unwell.Mayalibit.BySett,
            by=c("SettlementID","MonitoringYear"))

Mayalibit.AnnexContData.Techreport$MonitoringYear <- 
  factor(Mayalibit.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

Mayalibit.AnnexContData.Techreport <- 
  Mayalibit.AnnexContData.Techreport[rev(order(Mayalibit.AnnexContData.Techreport$SettlementName,
                                             Mayalibit.AnnexContData.Techreport$MonitoringYear)),]

Mayalibit.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.ContData.annex[Mayalibit.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   Mayalibit.level.ContData.annex[Mayalibit.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   Mayalibit.level.ContData.annex[Mayalibit.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Mayalibit.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Mayalibit.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Mayalibit.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make SettlementName an ordered factor for plotting
Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Mayalibit.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Mayalibit/Mayalibit_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Mayalibit.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Mayalibit.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(Mayalibit.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(propdata.trend.test.Maya,FileName,sheetName='PropData_TrendPlot_pvals',row.names=F,append=T)
write.xlsx(Mayalibit.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Mayalibit.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Mayalibit.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Maya,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Mayalibit.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 4: Synthesize other social data for interpretation/context ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 4.1 Tech report data synthesis aid ---- 
#   years resident, categorical food security, changes in social conflict, 
#   material assets gini coefficient, mean material assets, % fishers, 
#   % wage labor, marine tenure manage and harvest components

Mayalibit.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==1,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==1,3:16],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==1,3])
Mayalibit.level.synth <- left_join(Mayalibit.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Mayalibit.level.synth)),
                         dimnames=list(NULL,colnames(Mayalibit.level.synth)))

Mayalibit.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==1,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Mayalibit.synth.techreport <- rbind.data.frame(Mayalibit.level.synth,
                                             null.row.synth,
                                             Mayalibit.setts.synth)


write.xlsx(Mayalibit.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Mayalibit.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==1,] %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            Prop.FishingHouseholds=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
                                                                    !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            WholePop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean) &
                                                                     PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                             !is.na(PrimaryLivelihoodClean)]))*100,
            WholePop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean) &
                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                                !is.na(PrimaryLivelihoodClean)]))*100,
            WholePop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean) &
                                                                       PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                               !is.na(PrimaryLivelihoodClean)]))*100,
            WholePop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean) &
                                                                       PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                               !is.na(PrimaryLivelihoodClean)]))*100,
            WholePop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean) &
                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                                !is.na(PrimaryLivelihoodClean)]))*100)
Mayalibit.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==1,] %>%
  group_by(MonitoringYear) %>%
  summarise(Prop.FishingHouseholds=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
                                                                    !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            WholePop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.AlmostNever=(length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean) &
                                                                     PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                             !is.na(PrimaryLivelihoodClean) &
                                                                                                                                                             PrimaryLivelihoodClean==3]))*100,
            WholePop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.FewTimesPer6Mo=(length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean) &
                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                                !is.na(PrimaryLivelihoodClean) &
                                                                                                                                                                PrimaryLivelihoodClean==3]))*100,
            WholePop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.FewTimesPerMo=(length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean) &
                                                                       PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                               !is.na(PrimaryLivelihoodClean) &
                                                                                                                                                               PrimaryLivelihoodClean==3]))*100,
            WholePop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.FewTimesPerWk=(length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean) &
                                                                       PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                               !is.na(PrimaryLivelihoodClean) &
                                                                                                                                                               PrimaryLivelihoodClean==3]))*100,
            WholePop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean)]))*100,
            FishPop.SellFish.MoreFewTimesWk=(length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean) &
                                                                        PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(FreqSaleFishClean[!is.na(FreqSaleFishClean) & 
                                                                                                                                                                !is.na(PrimaryLivelihoodClean) &
                                                                                                                                                                PrimaryLivelihoodClean==3]))*100)

# Is food security status linked to occupation?
Mayalibit.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==1) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))

Mayalibit.foodsec.byocc$SettlementName <- factor(Mayalibit.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Mayalibit.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Mayalibit.level.PropData.status)
rm(Mayalibit.level.ContData.status)
rm(Mayalibit.level.PropData.annex)
rm(Mayalibit.level.ContData.annex)
rm(Days.unwell.Mayalibit.ByMPA)
rm(Days.unwell.Mayalibit.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Mayalibit.PropData.Techreport.status)
rm(Mayalibit.ContData.Techreport.status)
rm(Mayalibit.AnnexPropData.Techreport)
rm(Mayalibit.AnnexContData.Techreport)
rm(Mayalibit.ContData.Techreport.status.withMPA)
rm(Mayalibit.level.synth)
rm(null.row.synth)
rm(Mayalibit.setts.synth)
rm(Mayalibit.synth.techreport)
rm(FileName)