# 
# code:  Misool Technical Report Datasets
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
#  1) Source Misool.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Misool.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/Misool.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Misool.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==6 &
                       !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]

Days.unwell.Misool.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==6 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Misool ----

Misool.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==6 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==6 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==6 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                             seq(3,41,by=2)]),
             Female.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==6 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                               seq(4,42,by=2)]),
             Male.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==6 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                             seq(3,41,by=2)]),
             Female.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==6 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                               seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Misool.level.PropData.status <- 
  data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Misool Selatan\nTimur MPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==6 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:36],
               FishProtein.ByMPA[FishProtein.ByMPA$MPAID==6 &
                                   FishProtein.ByMPA$MonitoringYear=="4 Year Post",8:12],
             MPAimpact.intro.context[MPAimpact.intro.context$MPAID==6,c(12:15,33:36)])
Misool.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Misool Selatan\nTimur MPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==6,3:36],
                   FishProtein.ByMPA[FishProtein.ByMPA$MPAID==6,8:12])

null.row.PropData <- 
  matrix(rep(NA,50),ncol=50,dimnames=list(NULL,colnames(Misool.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Misool.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Misool Selatan\nTimur MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==6 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==6 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Misool.ByMPA[Days.unwell.Misool.ByMPA$MonitoringYear=="4 Year Post",
                                            c("UnwellMean","UnwellErr")])
Misool.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Misool Selatan\nTimur MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==6,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==6,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Misool.ByMPA[,c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Misool.level.ContData.status)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Misool, proportional data ----

Misool.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==6 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:38,41:48)],
            FishProtein.BySett[FishProtein.BySett$MPAID==6 &
                                 FishProtein.BySett$MonitoringYear=="4 Year Post",c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Misool.PropData.Techreport.status <- 
  Misool.PropData.Techreport.status[rev(order(Misool.PropData.Techreport.status$SettlementName)),]

Misool.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Misool.level.PropData.status[2:50],
                   null.row.PropData[2:50],
                   Misool.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Misool.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Misool.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")



# ---- 2.2 Status dataset for Misool, continuous data (with p values) ----

Misool.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==6,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==6 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Misool.ContData.Techreport.status <- 
  left_join(Misool.ContData.Techreport.status,
            Days.unwell.Misool.BySett[Days.unwell.Misool.BySett$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="SettlementID")

Misool.ContData.Techreport.status <- 
  Misool.ContData.Techreport.status[rev(order(Misool.ContData.Techreport.status$SettlementName)),]

Misool.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Misool.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Misool.ContData.Techreport.status)

# - plot-formatted dataset
Misool.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Misool.ContData.Techreport.status.withMPA,
            sigvals.Mis,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Misool.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Misool.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for Misool, MPA-level proportional data ----

Misool.TrendPropData.Techreport.PLOTFORMAT <- 
  left_join(Techreport.ByMPA[Techreport.ByMPA$MPAID==6,c(2,1,3:36)],
            FishProtein.ByMPA[FishProtein.ByMPA$MPAID==6,c(2,8:12)],
            by="MonitoringYear")


# ---- 2.4 Trend dataset for Misool, MPA-level continuous data (with p values) ----

Misool.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Misool.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Mis)

# - make MonitoringYear an ordered factor for plotting
Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)


# ---- 2.5 Annex dataset for Misool, Settlement-level proportional data ----

Misool.AnnexPropData.Techreport <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==6,c(2,1,4:38)],
            FishProtein.BySett[FishProtein.BySett$MPAID==6,c(2,1,4,10:14)],
            by=c("SettlementID","SettlementName","MonitoringYear"))

Misool.AnnexPropData.Techreport <- 
  Misool.AnnexPropData.Techreport[rev(order(Misool.AnnexPropData.Techreport$SettlementName)),]

Misool.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Misool.level.PropData.annex[Misool.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Misool.level.PropData.annex[Misool.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Misool.level.PropData.annex[Misool.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData[,-43:-51],
                   Misool.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Misool, Settlement-level continuous data (with p values) ----

Misool.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==6 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==6,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

Misool.AnnexContData.Techreport <- 
  left_join(Misool.AnnexContData.Techreport,
            Days.unwell.Misool.BySett,
            by=c("SettlementID","MonitoringYear"))

Misool.AnnexContData.Techreport$MonitoringYear <- 
  factor(Misool.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

Misool.AnnexContData.Techreport <- 
  Misool.AnnexContData.Techreport[rev(order(Misool.AnnexContData.Techreport$SettlementName,
                                               Misool.AnnexContData.Techreport$MonitoringYear)),]

Misool.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Misool.level.ContData.annex[Misool.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   Misool.level.ContData.annex[Misool.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   Misool.level.ContData.annex[Misool.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Misool.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Misool.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Misool.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make SettlementName an ordered factor for plotting
Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Misool.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Misool.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Misool/Misool_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Misool.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Misool.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(Misool.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(propdata.trend.test.Mis,FileName,sheetName='PropData_TrendPlot_pvals',row.names=F,append=T)
write.xlsx(Misool.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Misool.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Misool.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Mis,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Misool.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


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

Misool.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==6,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==6,3:16],
                                 AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==6,3])
Misool.level.synth <- left_join(Misool.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Misool.level.synth)),
                         dimnames=list(NULL,colnames(Misool.level.synth)))

Misool.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==6,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Misool.synth.techreport <- rbind.data.frame(Misool.level.synth,
                                             null.row.synth,
                                             Misool.setts.synth)


write.xlsx(Misool.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Misool.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==6,] %>%
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
Misool.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==6,] %>%
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
Misool.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==6) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))

Misool.foodsec.byocc$SettlementName <- factor(Misool.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Misool.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Misool.level.PropData.status)
rm(Misool.level.ContData.status)
rm(Misool.level.PropData.annex)
rm(Misool.level.ContData.annex)
rm(Days.unwell.Misool.ByMPA)
rm(Days.unwell.Misool.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Misool.PropData.Techreport.status)
rm(Misool.ContData.Techreport.status)
rm(Misool.AnnexPropData.Techreport)
rm(Misool.AnnexContData.Techreport)
rm(Misool.ContData.Techreport.status.withMPA)
rm(Misool.level.synth)
rm(null.row.synth)
rm(Misool.setts.synth)
rm(Misool.synth.techreport)
rm(FileName)