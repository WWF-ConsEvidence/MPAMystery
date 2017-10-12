# 
# code:  Mayalibit Technical Report Datasets
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
#  1) Source Mayalibit.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Mayalibit.TechReport.SigTests.R" ----

source("MPAMystery/Social/TechnicalReports/BHS/SignificanceTestCodes/Mayalibit.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Mayalibit.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==1 &
                       !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]

Days.unwell.Mayalibit.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==1 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Mayalibit ----

Mayalibit.AgeGender <- 
  data.frame(AgeCat=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
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
  data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==1 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:38]))
Mayalibit.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==1,3:38])

null.row.PropData <- 
  matrix(rep(NA,39),ncol=39,dimnames=list(NULL,colnames(Mayalibit.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Mayalibit.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==1 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==1 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",39:40],
                   Days.unwell.Mayalibit.ByMPA[Days.unwell.Mayalibit.ByMPA$MonitoringYear=="4 Year Post",
                                             c("Days.unwell","Days.unwell.err")])
Mayalibit.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==1,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==1,39:40],
                   Days.unwell.Mayalibit.ByMPA[,c("Days.unwell","Days.unwell.err")])

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
  Techreport.BySett[Techreport.BySett$MPAID==1 &
                      Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:40)]
Mayalibit.PropData.Techreport.status <- 
  Mayalibit.PropData.Techreport.status[rev(order(Mayalibit.PropData.Techreport.status$SettlementName)),]

Mayalibit.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.PropData.status[2:39],
                   null.row.PropData[1:37],
                   Mayalibit.PropData.Techreport.status)


# ---- 2.2 Status dataset for Mayalibit, continuous data (with p values) ----

Mayalibit.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==1,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==1 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,41:42)],
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

Mayalibit.ContData.Techreport.status.withMPA$SettlementName <- 
  factor(Mayalibit.ContData.Techreport.status.withMPA$SettlementName)

Mayalibit.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Mayalibit.ContData.Techreport.status.withMPA,
            sigvals.Maya,by="SettlementName")


# ---- 2.3 Trend dataset for Mayalibit, MPA-level proportional data ----

Mayalibit.TrendPropData.Techreport.PLOTFORMAT <- 
  Techreport.ByMPA[Techreport.ByMPA$MPAID==1,c(2,1,3:38)]


# ---- 2.4 Trend dataset for Mayalibit, MPA-level continuous data (with p values) ----

Mayalibit.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Maya)


# ---- 2.5 Annex dataset for Mayalibit, Settlement-level proportional data ----

Mayalibit.AnnexPropData.Techreport <- 
  Techreport.BySett[Techreport.BySett$MPAID==1,c(2,1,4:40)]

Mayalibit.AnnexPropData.Techreport <- 
  Mayalibit.AnnexPropData.Techreport[rev(order(Mayalibit.AnnexPropData.Techreport$SettlementName)),]

Mayalibit.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Mayalibit.level.PropData.annex[Mayalibit.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Mayalibit.level.PropData.annex[Mayalibit.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Mayalibit.level.PropData.annex[Mayalibit.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData,
                   Mayalibit.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Mayalibit, Settlement-level continuous data (with p values) ----

Mayalibit.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==1 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==1,c(1,2,41,42)],
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("MPAMystery/Social/FlatDataFiles/BHS/TechReportOutput/Mayalibit/Mayalibit_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Mayalibit.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Mayalibit.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(Mayalibit.TrendPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(Mayalibit.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Mayalibit.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Mayalibit.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Maya,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Mayalibit.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)





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
rm(FileName)