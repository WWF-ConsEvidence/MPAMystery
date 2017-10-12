# 
# code:  Cenderawasih Technical Report Datasets
# 
# github: kaclaborn/WWF-conservation-evidence/MPAMystery/Social/TechnicalReports/BHS
# --- Duplicate all code from "Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: October 2017
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

source("MPAMystery/Social/TechnicalReports/BHS/SignificanceTestCodes/TNTC.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.TNTC.BySett <- 
  rbind.data.frame(Days.unwell.BySett[Days.unwell.BySett$MPAID==2 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)],
                   cbind.data.frame(MonitoringYear=rep("Baseline",9),
                                    SettlementID=seq(104,112,by=1),
                                    Days.unwell=rep(NA,9),
                                    Days.unwell.err=rep(NA,9)))

Days.unwell.TNTC.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==2 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for TNTC ----

TNTC.AgeGender <- 
  data.frame(AgeCat=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
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
  data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==2 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:38]))
TNTC.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==2,3:38])

null.row.PropData <- 
  matrix(rep(NA,39),ncol=39,dimnames=list(NULL,colnames(TNTC.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

TNTC.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==2 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",39:40],
                   Days.unwell.TNTC.ByMPA[Days.unwell.TNTC.ByMPA$MonitoringYear=="4 Year Post",
                                          c("Days.unwell","Days.unwell.err")])
TNTC.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==2,39:40],
                   Days.unwell.TNTC.ByMPA[,c("Days.unwell","Days.unwell.err")])

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
  Techreport.BySett[Techreport.BySett$MPAID==2 &
                      Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:40)]

TNTC.PropData.Techreport.status <- 
  TNTC.PropData.Techreport.status[rev(order(TNTC.PropData.Techreport.status$SettlementName)),]

TNTC.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.PropData.status[2:39],
                   null.row.PropData[1:37],
                   TNTC.PropData.Techreport.status)


# ---- 2.2 Status dataset for TNTC, continuous data (with p values) ----

TNTC.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==2,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==2 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,41:42)],
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
TNTC.ContData.Techreport.status.withMPA$SettlementName <-
  factor(TNTC.ContData.Techreport.status.withMPA$SettlementName)

TNTC.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(TNTC.ContData.Techreport.status.withMPA,
            sigvals.TNTC,by="SettlementName")


# ---- 2.3 Trend dataset for TNTC, MPA-level proportional data ----

TNTC.TrendPropData.Techreport.PLOTFORMAT <- 
  Techreport.ByMPA[Techreport.ByMPA$MPAID==2,c(2,1,3:38)]


# ---- 2.4 Trend dataset for TNTC, MPA-level continuous data (with p values) ----

TNTC.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.TNTC)


# ---- 2.5 Annex dataset for TNTC, Settlement-level proportional data ----

TNTC.AnnexPropData.Techreport <- 
  Techreport.BySett[Techreport.BySett$MPAID==2,c(2,1,4:40)]

TNTC.AnnexPropData.Techreport <- 
  TNTC.AnnexPropData.Techreport[rev(order(TNTC.AnnexPropData.Techreport$SettlementName)),]

TNTC.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(TNTC.level.PropData.annex[TNTC.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   TNTC.level.PropData.annex[TNTC.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   TNTC.level.PropData.annex[TNTC.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData,
                   TNTC.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for TNTC, Settlement-level continuous data (with p values) ----

TNTC.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==2 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==2,c(1,2,41,42)],
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("MPAMystery/Social/FlatDataFiles/BHS/TechReportOutput/TNTC/TNTC_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(TNTC.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(TNTC.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(TNTC.TrendPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(TNTC.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(TNTC.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(TNTC.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.TNTC,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(TNTC.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)




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
rm(FileName)