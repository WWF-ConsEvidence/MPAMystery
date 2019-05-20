# 
# code:  Dampier Technical Report Datasets
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
#  1) Source Dampier.TechReport.SigTests.R 
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

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/Dampier.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Dampier.BySett <- 
  rbind.data.frame(Days.unwell.BySett[Days.unwell.BySett$MPAID==5 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)],
                   cbind.data.frame(MonitoringYear="Baseline",
                                    SettlementID=72,
                                    UnwellMean=NA,
                                    UnwellErr=NA))

Days.unwell.Dampier.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==5 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Dampier ----

Dampier.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==5 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==5 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==5 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                             seq(3,41,by=2)]),
             Female.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==5 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                               seq(4,42,by=2)]),
             Male.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==5 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                             seq(3,41,by=2)]),
             Female.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==5 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                               seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Dampier.level.PropData.status <- 
  data.frame(MonitoringYear="4 Year Post",
             SettlementID=0,
             SettlementName="Selat Dampier\nMPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==5 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:36],
               FishProtein.ByMPA[FishProtein.ByMPA$MPAID==5 &
                                   FishProtein.ByMPA$MonitoringYear=="4 Year Post",8:12],
             MPAimpact.intro.context[MPAimpact.intro.context$MPAID==5,c(12:15,33:36)])

Dampier.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Selat Dampier\nMPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==5,3:36],
                   FishProtein.ByMPA[FishProtein.ByMPA$MPAID==5,8:12])

null.row.PropData <- 
  matrix(rep(NA,50),ncol=50,dimnames=list(NULL,colnames(Dampier.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Dampier.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Selat Dampier\nMPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==5 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Dampier.ByMPA[Days.unwell.Dampier.ByMPA$MonitoringYear=="4 Year Post",
                                             c("UnwellMean","UnwellErr")])
Dampier.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Selat Dampier\nMPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==5,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Dampier.ByMPA[,c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Dampier.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Dampier, proportional data ----

Dampier.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==5 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:38,41:48)],
            FishProtein.BySett[FishProtein.BySett$MPAID==5 &
                                 FishProtein.BySett$MonitoringYear=="4 Year Post",c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Dampier.PropData.Techreport.status <- 
  Dampier.PropData.Techreport.status[rev(order(Dampier.PropData.Techreport.status$SettlementName)),]

Dampier.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Dampier.level.PropData.status[2:50],
                   null.row.PropData[2:50],
                   Dampier.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Dampier.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Dampier, continuous data (with p values) ----

Dampier.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==5,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==5 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Dampier.ContData.Techreport.status <- 
  left_join(Dampier.ContData.Techreport.status,
            Days.unwell.Dampier.BySett[Days.unwell.Dampier.BySett$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="SettlementID")

Dampier.ContData.Techreport.status <- 
  Dampier.ContData.Techreport.status[rev(order(Dampier.ContData.Techreport.status$SettlementName)),]

Dampier.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Dampier.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Dampier.ContData.Techreport.status)

# - plot-formatted dataset
Dampier.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Dampier.ContData.Techreport.status.withMPA,
            sigvals.Damp,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Dampier.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Dampier.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for Dampier, MPA-level proportional data ----

Dampier.TrendPropData.Techreport.PLOTFORMAT <- 
  left_join(Techreport.ByMPA[Techreport.ByMPA$MPAID==5,c(2,1,3:36)],
            FishProtein.ByMPA[FishProtein.ByMPA$MPAID==5,c(2,8:12)],
            by="MonitoringYear")


# ---- 2.4 Trend dataset for Dampier, MPA-level continuous data (with p values) ----

Dampier.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Dampier.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Damp)

# - make MonitoringYear an ordered factor for plotting
Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)


# ---- 2.5 Annex dataset for Dampier, Settlement-level proportional data ----

Dampier.AnnexPropData.Techreport <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==5,c(2,1,4:38)],
            FishProtein.BySett[FishProtein.BySett$MPAID==5,c(2,1,4,10:14)],
            by=c("SettlementID","SettlementName","MonitoringYear"))

Dampier.AnnexPropData.Techreport <- 
  Dampier.AnnexPropData.Techreport[rev(order(Dampier.AnnexPropData.Techreport$SettlementName)),]

Dampier.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Dampier.level.PropData.annex[Dampier.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Dampier.level.PropData.annex[Dampier.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Dampier.level.PropData.annex[Dampier.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData[,-43:-51],
                   Dampier.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Dampier, Settlement-level continuous data (with p values) ----

Dampier.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==5 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==5,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

Dampier.AnnexContData.Techreport <- 
  left_join(Dampier.AnnexContData.Techreport,
            Days.unwell.Dampier.BySett,
            by=c("SettlementID","MonitoringYear"))

Dampier.AnnexContData.Techreport$MonitoringYear <- 
  factor(Dampier.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

Dampier.AnnexContData.Techreport <- 
  Dampier.AnnexContData.Techreport[rev(order(Dampier.AnnexContData.Techreport$SettlementName,
                                             Dampier.AnnexContData.Techreport$MonitoringYear)),]

Dampier.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Dampier.level.ContData.annex[Dampier.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   Dampier.level.ContData.annex[Dampier.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   Dampier.level.ContData.annex[Dampier.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Dampier.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Dampier.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Dampier.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make SettlementName an ordered factor for plotting
Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Dampier.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Dampier.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Dampier/Dampier_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")

# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Dampier.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Dampier.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(Dampier.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(propdata.trend.test.Damp,FileName,sheetName='PropData_TrendPlot_pvals',row.names=F,append=T)
write.xlsx(Dampier.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Dampier.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Dampier.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Damp,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Dampier.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


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

Dampier.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==5,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==5,3:length(Synth.techreport.byMPA)],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==5,3])
Dampier.level.synth <- left_join(Dampier.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Dampier.level.synth)),
                         dimnames=list(NULL,colnames(Dampier.level.synth)))

Dampier.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==5,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Dampier.synth.techreport <- rbind.data.frame(Dampier.level.synth,
                                             null.row.synth,
                                             Dampier.setts.synth)


write.xlsx(Dampier.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Dampier.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==5,] %>%
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
Dampier.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==5,] %>%
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
Dampier.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==5) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))
  
Dampier.foodsec.byocc$SettlementName <- factor(Dampier.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Dampier.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)



# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Dampier.level.PropData.status)
rm(Dampier.level.ContData.status)
rm(Dampier.level.PropData.annex)
rm(Dampier.level.ContData.annex)
rm(Days.unwell.Dampier.ByMPA)
rm(Days.unwell.Dampier.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Dampier.PropData.Techreport.status)
rm(Dampier.ContData.Techreport.status)
rm(Dampier.AnnexPropData.Techreport)
rm(Dampier.AnnexContData.Techreport)
rm(Dampier.ContData.Techreport.status.withMPA)
rm(Dampier.level.synth)
rm(null.row.synth)
rm(Dampier.setts.synth)
rm(Dampier.synth.techreport)
rm(FileName)