# 
# code:  Kaimana Technical Report Datasets
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
#  1) Source Kaimana.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Kaimana.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/Kaimana.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Kaimana.BySett <- 
  rbind.data.frame(Days.unwell.BySett[Days.unwell.BySett$MPAID==3 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)],
                   cbind.data.frame(MonitoringYear="Baseline",
                                    SettlementID=c(113,114,115),
                                    UnwellMean=NA,
                                    UnwellErr=NA))

Days.unwell.Kaimana.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==3 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Kaimana ----

Kaimana.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==3 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==3 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==3 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                             seq(3,41,by=2)]),
             Female.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==3 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                               seq(4,42,by=2)]),
             Male.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==3 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                             seq(3,41,by=2)]),
             Female.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==3 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                               seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Kaimana.level.PropData.status <- 
  data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Kaimana MPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==3 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:36],
               FishProtein.ByMPA[FishProtein.ByMPA$MPAID==3 &
                                   FishProtein.ByMPA$MonitoringYear=="4 Year Post",8:12],
             MPAimpact.intro.context[MPAimpact.intro.context$MPAID==3,c(12:15,33:36)])

Kaimana.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Kaimana MPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==3,3:36],
                   FishProtein.ByMPA[FishProtein.ByMPA$MPAID==3,8:12])

null.row.PropData <- 
  matrix(rep(NA,50),ncol=50,dimnames=list(NULL,colnames(Kaimana.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Kaimana.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Kaimana MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==3 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Kaimana.ByMPA[Days.unwell.Kaimana.ByMPA$MonitoringYear=="4 Year Post",
                                             c("UnwellMean","UnwellErr")])
Kaimana.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==3,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Kaimana.ByMPA[,c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Kaimana.level.ContData.status)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Kaimana, proportional data ----

Kaimana.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==3 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:38,41:48)],
            FishProtein.BySett[FishProtein.BySett$MPAID==3 &
                                 FishProtein.BySett$MonitoringYear=="4 Year Post",c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Kaimana.PropData.Techreport.status <- 
  Kaimana.PropData.Techreport.status[rev(order(Kaimana.PropData.Techreport.status$SettlementName)),]

Kaimana.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.PropData.status[2:50],
                   null.row.PropData[2:50],
                   Kaimana.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Kaimana.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Kaimana.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Kaimana, continuous data (with p values) ----

Kaimana.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==3 &
                                  !is.na(BigFive.SettleGroup$SettlementID),
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==3 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Kaimana.ContData.Techreport.status <- 
  left_join(Kaimana.ContData.Techreport.status,
            Days.unwell.Kaimana.BySett[Days.unwell.Kaimana.BySett$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="SettlementID")

Kaimana.ContData.Techreport.status <- 
  Kaimana.ContData.Techreport.status[rev(order(Kaimana.ContData.Techreport.status$SettlementName)),]

Kaimana.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Kaimana.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Kaimana.ContData.Techreport.status)

# - plot-formatted dataset
Kaimana.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Kaimana.ContData.Techreport.status.withMPA,
            sigvals.Kai,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Kaimana.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Kaimana.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for Kaimana, MPA-level proportional data ----

Kaimana.TrendPropData.Techreport.PLOTFORMAT <- 
  left_join(Techreport.ByMPA[Techreport.ByMPA$MPAID==3,c(2,1,3:36)],
            FishProtein.ByMPA[FishProtein.ByMPA$MPAID==3,c(2,8:12)],
            by="MonitoringYear")


# ---- 2.4 Trend dataset for Kaimana, MPA-level continuous data (with p values) ----

Kaimana.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Kai)

# - make MonitoringYear an ordered factor for plotting
Kaimana.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Kaimana.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# ---- 2.5 Annex dataset for Kaimana, Settlement-level proportional data ----

Kaimana.AnnexPropData.Techreport <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==3,c(2,1,4:38)],
            FishProtein.BySett[FishProtein.BySett$MPAID==3,c(2,1,4,10:14)],
            by=c("SettlementID","SettlementName","MonitoringYear"))

Kaimana.AnnexPropData.Techreport <- 
  Kaimana.AnnexPropData.Techreport[rev(order(Kaimana.AnnexPropData.Techreport$SettlementName,
                                             Kaimana.AnnexPropData.Techreport$MonitoringYear)),]

Kaimana.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.PropData.annex[Kaimana.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Kaimana.level.PropData.annex[Kaimana.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Kaimana.level.PropData.annex[Kaimana.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData[,-43:-51],
                   Kaimana.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Kaimana, Settlement-level continuous data (with p values) ----

Kaimana.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==3 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==3,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

Kaimana.AnnexContData.Techreport <- 
  left_join(Kaimana.AnnexContData.Techreport,
            Days.unwell.Kaimana.BySett,
            by=c("SettlementID","MonitoringYear"))

Kaimana.AnnexContData.Techreport$MonitoringYear <- 
  factor(Kaimana.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

Kaimana.AnnexContData.Techreport <- 
  Kaimana.AnnexContData.Techreport[rev(order(Kaimana.AnnexContData.Techreport$SettlementName,
                                             Kaimana.AnnexContData.Techreport$MonitoringYear)),]

Kaimana.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.ContData.annex[Kaimana.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   Kaimana.level.ContData.annex[Kaimana.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   Kaimana.level.ContData.annex[Kaimana.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Kaimana.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Kaimana.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Kaimana.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make SettlementName an ordered factor for plotting
Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Kaimana.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Kaimana/Kaimana_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Kaimana.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Kaimana.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(Kaimana.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(propdata.trend.test.Kai,FileName,sheetName='PropData_TrendPlot_pvals',row.names=F,append=T)
write.xlsx(Kaimana.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Kaimana.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Kaimana.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Kai,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Kaimana.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


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

Kaimana.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==3,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==3,3:16],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==3,3])
Kaimana.level.synth <- left_join(Kaimana.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Kaimana.level.synth)),
                         dimnames=list(NULL,colnames(Kaimana.level.synth)))

Kaimana.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==3,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Kaimana.synth.techreport <- rbind.data.frame(Kaimana.level.synth,
                                             null.row.synth,
                                             Kaimana.setts.synth)


write.xlsx(Kaimana.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Kaimana.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==3,] %>%
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
Kaimana.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==3,] %>%
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
Kaimana.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==3) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))

Kaimana.foodsec.byocc$SettlementName <- factor(Kaimana.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Kaimana.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Kaimana.level.PropData.status)
rm(Kaimana.level.ContData.status)
rm(Kaimana.level.PropData.annex)
rm(Kaimana.level.ContData.annex)
rm(Days.unwell.Kaimana.ByMPA)
rm(Days.unwell.Kaimana.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Kaimana.PropData.Techreport.status)
rm(Kaimana.ContData.Techreport.status)
rm(Kaimana.AnnexPropData.Techreport)
rm(Kaimana.AnnexContData.Techreport)
rm(Kaimana.ContData.Techreport.status.withMPA)
rm(Kaimana.level.synth)
rm(null.row.synth)
rm(Kaimana.setts.synth)
rm(Kaimana.synth.techreport)
rm(FileName)