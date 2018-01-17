# 
# code:  Cenderawasih Technical Report Datasets
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
#  1) Source TNTC.TechReport.SigTests.R
#     - Dependencies: BHS_MPA_Mystery.R
# 
# ---- code sections ----
#  1) Data Sourcing, Configuration, and Subsetting
#  2) Define Datasets for Status, Trend, and Annex Plots for Export
#  3) Export Data to Excel 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Data Sourcing, Configuration, and Subsetting ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Source statistical test results from "TNTC.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/TNTC.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.TNTC.BySett <- 
  rbind.data.frame(Days.unwell.BySett[Days.unwell.BySett$MPAID==2 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)],
                   cbind.data.frame(MonitoringYear=rep("Baseline",9),
                                    SettlementID=seq(104,112,by=1),
                                    UnwellMean=rep(NA,9),
                                    UnwellErr=rep(NA,9)))

Days.unwell.TNTC.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==2 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for TNTC ----

TNTC.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==2 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==2 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==2 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                             seq(3,41,by=2)]),
             Female.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==2 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                               seq(4,42,by=2)]),
             Male.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==2 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                             seq(3,41,by=2)]),
             Female.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==2 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                               seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

TNTC.level.PropData.status <- 
  data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Teluk Cenderawasih\nNational Park",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==2 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:36],
               FishProtein.ByMPA[FishProtein.ByMPA$MPAID==2 &
                                   FishProtein.ByMPA$MonitoringYear=="4 Year Post",8:12],
             MPAimpact.intro.context[MPAimpact.intro.context$MPAID==2,c(12:15,33:36)])
             
TNTC.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Teluk Cenderawasih\nNational Park",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==2,3:36],
                   FishProtein.ByMPA[FishProtein.ByMPA$MPAID==2,8:12])

null.row.PropData <- 
  matrix(rep(NA,50),ncol=50,dimnames=list(NULL,colnames(TNTC.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

TNTC.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Teluk Cenderawasih\nNational Park",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==2 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.TNTC.ByMPA[Days.unwell.TNTC.ByMPA$MonitoringYear=="4 Year Post",
                                          c("UnwellMean","UnwellErr")])
TNTC.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Teluk Cenderawasih\nNational Park",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==2,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.TNTC.ByMPA[,c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(TNTC.level.ContData.status)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for TNTC, proportional data ----

TNTC.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==2 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:38,41:48)],
            FishProtein.BySett[FishProtein.BySett$MPAID==2 &
                                 FishProtein.BySett$MonitoringYear=="4 Year Post",c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))


TNTC.PropData.Techreport.status <- 
  TNTC.PropData.Techreport.status[rev(order(TNTC.PropData.Techreport.status$SettlementName)),]

TNTC.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.PropData.status[2:50],
                   null.row.PropData[2:50],
                   TNTC.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName))

TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
TNTC.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(TNTC.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")



# ---- 2.2 Status dataset for TNTC, continuous data (with p values) ----

TNTC.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==2,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==2 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

TNTC.ContData.Techreport.status <- 
  left_join(TNTC.ContData.Techreport.status,
            Days.unwell.TNTC.BySett[Days.unwell.TNTC.BySett$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="SettlementID")

TNTC.ContData.Techreport.status <- 
  TNTC.ContData.Techreport.status[rev(order(TNTC.ContData.Techreport.status$SettlementName)),]

TNTC.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(TNTC.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   TNTC.ContData.Techreport.status)

# - plot-formatted dataset
TNTC.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(TNTC.ContData.Techreport.status.withMPA,
            sigvals.TNTC,by="SettlementName")

# - make SettlementName an ordered factor for plotting
TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName)

TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
TNTC.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(TNTC.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for TNTC, MPA-level proportional data ----

TNTC.TrendPropData.Techreport.PLOTFORMAT <- 
  left_join(Techreport.ByMPA[Techreport.ByMPA$MPAID==2,c(2,1,3:36)],
            FishProtein.ByMPA[FishProtein.ByMPA$MPAID==2,c(2,8:12)],
            by="MonitoringYear")

# ---- 2.4 Trend dataset for TNTC, MPA-level continuous data (with p values) ----

TNTC.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.TNTC)

# - make MonitoringYear an ordered factor for plotting
TNTC.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(TNTC.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)


# ---- 2.5 Annex dataset for TNTC, Settlement-level proportional data ----

TNTC.AnnexPropData.Techreport <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==2,c(2,1,4:38)],
            FishProtein.BySett[FishProtein.BySett$MPAID==2,c(2,1,4,10:14)],
            by=c("SettlementID","SettlementName","MonitoringYear"))

TNTC.AnnexPropData.Techreport <- 
  TNTC.AnnexPropData.Techreport[rev(order(TNTC.AnnexPropData.Techreport$SettlementName)),]

TNTC.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.PropData.annex[TNTC.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   TNTC.level.PropData.annex[TNTC.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   TNTC.level.PropData.annex[TNTC.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData[,-43:-51],
                   TNTC.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for TNTC, Settlement-level continuous data (with p values) ----

TNTC.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==2 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==2,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

TNTC.AnnexContData.Techreport <- 
  left_join(TNTC.AnnexContData.Techreport,
            Days.unwell.TNTC.BySett,
            by=c("SettlementID","MonitoringYear"))

TNTC.AnnexContData.Techreport$MonitoringYear <- 
  factor(TNTC.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

TNTC.AnnexContData.Techreport <- 
  TNTC.AnnexContData.Techreport[rev(order(TNTC.AnnexContData.Techreport$SettlementName,
                                          TNTC.AnnexContData.Techreport$MonitoringYear)),]

TNTC.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.ContData.annex[TNTC.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   TNTC.level.ContData.annex[TNTC.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   TNTC.level.ContData.annex[TNTC.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   TNTC.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
TNTC.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(TNTC.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make SettlementName an ordered factor for plotting
TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
TNTC.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(TNTC.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/TNTC/TNTC_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(TNTC.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(TNTC.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(TNTC.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(propdata.trend.test.TNTC,FileName,sheetName='PropData_TrendPlot_pvals',row.names=F,append=T)
write.xlsx(TNTC.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(TNTC.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(TNTC.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.TNTC,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(TNTC.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)



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

TNTC.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==2,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==2,3:length(Synth.techreport.byMPA)],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==2,3])
TNTC.level.synth <- left_join(TNTC.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(TNTC.level.synth)),
                         dimnames=list(NULL,colnames(TNTC.level.synth)))

TNTC.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==2,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

TNTC.synth.techreport <- rbind.data.frame(TNTC.level.synth,
                                             null.row.synth,
                                             TNTC.setts.synth)


write.xlsx(TNTC.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
TNTC.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==2,] %>%
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
TNTC.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==2,] %>%
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
TNTC.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==2) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))

TNTC.foodsec.byocc$SettlementName <- factor(TNTC.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(TNTC.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)



# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(TNTC.level.PropData.status)
rm(TNTC.level.ContData.status)
rm(TNTC.level.PropData.annex)
rm(TNTC.level.ContData.annex)
rm(Days.unwell.TNTC.ByMPA)
rm(Days.unwell.TNTC.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(TNTC.PropData.Techreport.status)
rm(TNTC.ContData.Techreport.status)
rm(TNTC.AnnexPropData.Techreport)
rm(TNTC.AnnexContData.Techreport)
rm(TNTC.ContData.Techreport.status.withMPA)
rm(TNTC.level.synth)
rm(null.row.synth)
rm(TNTC.setts.synth)
rm(TNTC.synth.techreport)
rm(FileName)