# 
# code:  Kaimana Technical Report Datasets
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
#  1) Source MPA.Mystery.R 
#  2) Source Kaimana.TechReport.SigTests.R 
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


# ---- 1.1 Source MPA.Mystery and statistical test results from "Kaimana.TechReport.SigTests.R" ----

source("MPAMystery/Social/MPA.Mystery.R")  # !!! No need to re-source this if already done from previous script
source("MPAMystery/Social/TechnicalReports/BHS/Kaimana.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Kaimana.BySett <- 
  rbind.data.frame(Days.unwell.BySett[Days.unwell.BySett$MPAID==3 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)],
                   cbind.data.frame(MonitoringYear="Baseline",
                                    SettlementID=c(113,114,115),
                                    Days.unwell=NA,
                                    Days.unwell.err=NA))

Days.unwell.Kaimana.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==3 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]


# ---- 1.3 Subset Proportional Data of Age/Gender for Kaimana ----

Kaimana.AgeGender <- 
  data.frame(AgeCat=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
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
  data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
               Techreport.ByMPA[Techreport.ByMPA$MPAID==3 &
                                  Techreport.ByMPA$MonitoringYear=="4 Year Post",3:38]))
Kaimana.level.PropData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==3,3:38])

null.row.PropData <- 
  matrix(rep(NA,39),ncol=39,dimnames=list(NULL,colnames(Kaimana.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Kaimana.level.ContData.status <- 
  cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3 &
                                      BigFive.MPAGroup$MonitoringYear=="4 Year Post",6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==3 &
                                      Techreport.ByMPA$MonitoringYear=="4 Year Post",39:40],
                   Days.unwell.Kaimana.ByMPA[Days.unwell.Kaimana.ByMPA$MonitoringYear=="4 Year Post",
                                             c("Days.unwell","Days.unwell.err")])
Kaimana.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("Baseline","2 Year Post","4 Year Post"),
                   SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,6:15],
                   Techreport.ByMPA[Techreport.ByMPA$MPAID==3,39:40],
                   Days.unwell.Kaimana.ByMPA[,c("Days.unwell","Days.unwell.err")])

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
  Techreport.BySett[Techreport.BySett$MPAID==3 &
                      Techreport.BySett$MonitoringYear=="4 Year Post",c(1,4:40)]
Kaimana.PropData.Techreport.status <- 
  Kaimana.PropData.Techreport.status[rev(order(Kaimana.PropData.Techreport.status$SettlementName)),]

Kaimana.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.PropData.status[2:39],
                   null.row.PropData[1:37],
                   Kaimana.PropData.Techreport.status)


# ---- 2.2 Status dataset for Kaimana, continuous data (with p values) ----

Kaimana.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==3 &
                                  !is.na(BigFive.SettleGroup$SettlementID),
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==3 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",c(1,41:42)],
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

Kaimana.ContData.Techreport.status.withMPA$SettlementName <- 
  factor(Kaimana.ContData.Techreport.status.withMPA$SettlementName)

Kaimana.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Kaimana.ContData.Techreport.status.withMPA,
            sigvals.Kai,by="SettlementName")


# ---- 2.3 Trend dataset for Kaimana, MPA-level proportional data ----

Kaimana.TrendPropData.Techreport.PLOTFORMAT <- 
  Techreport.ByMPA[Techreport.ByMPA$MPAID==3,c(2,1,3:38)]


# ---- 2.4 Trend dataset for Kaimana, MPA-level continuous data (with p values) ----

Kaimana.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Kai)


# ---- 2.5 Annex dataset for Kaimana, Settlement-level proportional data ----

Kaimana.AnnexPropData.Techreport <- 
  Techreport.BySett[Techreport.BySett$MPAID==3 &
                      !is.na(Techreport.BySett$SettlementID),c(2,1,4:40)]

Kaimana.AnnexPropData.Techreport <- 
  Kaimana.AnnexPropData.Techreport[rev(order(Kaimana.AnnexPropData.Techreport$SettlementName,
                                             Kaimana.AnnexPropData.Techreport$MonitoringYear)),]

Kaimana.AnnexPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Kaimana.level.PropData.annex[Kaimana.level.PropData.annex$MonitoringYear=="4 Year Post",],
                   Kaimana.level.PropData.annex[Kaimana.level.PropData.annex$MonitoringYear=="2 Year Post",],
                   Kaimana.level.PropData.annex[Kaimana.level.PropData.annex$MonitoringYear=="Baseline",],
                   null.row.PropData,
                   Kaimana.AnnexPropData.Techreport)


# ---- 2.6 Annex dataset for Kaimana, Settlement-level continuous data (with p values) ----

Kaimana.AnnexContData.Techreport <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==3 &
                                  BigFive.SettleGroup$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==3,c(1,2,41,42)],
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("MPAMystery/Social/FlatDataFiles/BHS/TechReportOutput/Kaimana/Kaimana_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Kaimana.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Kaimana.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(Kaimana.TrendPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_TrendPlots',row.names=F,append=T)
write.xlsx(Kaimana.TrendContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_TrendPlots_withpvals',row.names=F,append=T)
write.xlsx(Kaimana.AnnexPropData.Techreport.PLOTFORMAT,FileName,sheetName='PropData_AnnexPlots',row.names=F,append=T)
write.xlsx(Kaimana.AnnexContData.Techreport.PLOTFORMAT,FileName,sheetName='ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(annex.sigvals.Kai,FileName,sheetName='Pvals_ContData_AnnexPlots',row.names=F,append=T)
write.xlsx(Kaimana.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)






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
rm(FileName)