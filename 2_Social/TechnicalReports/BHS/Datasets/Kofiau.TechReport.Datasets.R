# 
# code:  Kofiau Technical Report Datasets
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
#  1) Source Kofiau.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Kofiau.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/BHS/SignificanceTestCodes/Kofiau.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Kofiau.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==4 &
                       !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]
Days.unwell.Kofiau.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==4 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Kofiau ----

Kofiau.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==4 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==4 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==4 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                             seq(3,41,by=2)]),
             Female.2yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==4 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="2 Year Post",
                                               seq(4,42,by=2)]),
             Male.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==4 &
                                               AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                             seq(3,41,by=2)]),
             Female.4yr=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==4 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="4 Year Post",
                                               seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Kofiau.level.PropData.status <- 
  data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Kofiau dan\nPulau Boo MPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==4 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:36]))
Kofiau.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Kofiau dan\nPulau Boo MPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==4,3:36])

null.row.PropData <- 
  matrix(rep(NA,37),ncol=37,dimnames=list(NULL,colnames(Kofiau.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Kofiau.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="Kofiau dan\nPulau Boo MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==4 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==4 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Kofiau.ByMPA[Days.unwell.Kofiau.ByMPA$MonitoringYear=="4 Year Post",
                                            c("UnwellMean","UnwellErr")])
Kofiau.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="Kofiau dan\nPulau Boo MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==4,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==4,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Kofiau.ByMPA[,c("UnwellMean","UnwellErr")])

null.row.ContData <- 
  matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Kofiau.level.ContData.status)))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Kofiau, proportional data ----

Kofiau.PropData.Techreport.status <- 
  Techreport.BySett[Techreport.BySett$MPAID==4 &
                      Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:38)]
Kofiau.PropData.Techreport.status <- 
  Kofiau.PropData.Techreport.status[rev(order(Kofiau.PropData.Techreport.status$SettlementName)),]

Kofiau.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Kofiau.level.PropData.status[2:37],
                   null.row.PropData[1:35],
                   Kofiau.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Kofiau.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Kofiau.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Kofiau, continuous data (with p values) ----

Kofiau.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==4 &
                                  !is.na(BigFive.SettleGroup$SettlementID),
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==4 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Kofiau.ContData.Techreport.status <- 
  left_join(Kofiau.ContData.Techreport.status,
            Days.unwell.Kofiau.BySett[Days.unwell.Kofiau.BySett$MonitoringYear=="4 Year Post",c(1,3,4)],
            by="SettlementID")

Kofiau.ContData.Techreport.status <- 
  Kofiau.ContData.Techreport.status[rev(order(Kofiau.ContData.Techreport.status$SettlementName)),]

Kofiau.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Kofiau.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Kofiau.ContData.Techreport.status)

# - plot-formatted dataset
Kofiau.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Kofiau.ContData.Techreport.status.withMPA,
            sigvals.Kof,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Kofiau.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Kofiau.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for Kofiau, MPA-level proportional data ----
Kofiau.TrendPropData.Techreport.PLOTFORMAT <- 
  Techreport.ByMPA[Techreport.ByMPA$MPAID==4,c(2,1,3:36)]


# ---- 2.4 Trend dataset for Kofiau, MPA-level continuous data (with p values) ----

Kofiau.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kofiau.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Kof)

# - make MonitoringYear an ordered factor for plotting
Kofiau.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Kofiau.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)


# ---- 2.5 Annex dataset for Kofiau, Settlement-level proportional data ----

Kofiau.AnnexPropData.Techreport <- 
  Techreport.BySett[Techreport.BySett$MPAID==4 &
                      !is.na(Techreport.BySett$SettlementID),c(2,1,4:38)]

Kofiau.AnnexPropData.Techreport <- 
  Kofiau.AnnexPropData.Techreport[rev(order(Kofiau.AnnexPropData.Techreport$SettlementName,
                                             Kofiau.AnnexPropData.Techreport$MonitoringYear)),]

Kofiau.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kofiau.level.PropData.annex[Kofiau.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Kofiau.level.PropData.annex[Kofiau.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Kofiau.level.PropData.annex[Kofiau.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData,
                   Kofiau.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Kofiau, Settlement-level continuous data (with p values) ----

Kofiau.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==4 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==4,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

Kofiau.AnnexContData.Techreport <- 
  left_join(Kofiau.AnnexContData.Techreport,
            Days.unwell.Kofiau.BySett,
            by=c("SettlementID","MonitoringYear"))

Kofiau.AnnexContData.Techreport$MonitoringYear <- 
  factor(Kofiau.AnnexContData.Techreport$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)

Kofiau.AnnexContData.Techreport <- 
  Kofiau.AnnexContData.Techreport[rev(order(Kofiau.AnnexContData.Techreport$SettlementName,
                                             Kofiau.AnnexContData.Techreport$MonitoringYear)),]

Kofiau.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kofiau.level.ContData.annex[Kofiau.level.ContData.annex$MonitoringYear=="4 Year Post",],
                   Kofiau.level.ContData.annex[Kofiau.level.ContData.annex$MonitoringYear=="2 Year Post",],
                   Kofiau.level.ContData.annex[Kofiau.level.ContData.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Kofiau.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Kofiau.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Kofiau.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","2 Year Post","4 Year Post"),
         ordered=T)

# - make SettlementName an ordered factor for plotting
Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Kofiau.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Kofiau/Kofiau_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Kofiau.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Kofiau.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(as.data.frame(Kofiau.TrendPropData.Techreport.PLOTFORMAT),FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(Kofiau.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Kofiau.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Kofiau.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Kof,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Kofiau.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)





# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Kofiau.level.PropData.status)
rm(Kofiau.level.ContData.status)
rm(Kofiau.level.PropData.annex)
rm(Kofiau.level.ContData.annex)
rm(Days.unwell.Kofiau.ByMPA)
rm(Days.unwell.Kofiau.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Kofiau.PropData.Techreport.status)
rm(Kofiau.ContData.Techreport.status)
rm(Kofiau.AnnexPropData.Techreport)
rm(Kofiau.AnnexContData.Techreport)
rm(Kofiau.ContData.Techreport.status.withMPA)
rm(FileName)
