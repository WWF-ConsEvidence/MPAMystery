# 
# code: Koon 2018 Technical Report Datasets
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: April 2019
# 
# 
# ---- inputs ----
#  1) Source Koon_2018_TechReport_SigTest.R 
#     - Dependencies: Koon_2018_calculate_indicators.R
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


# ---- 1.1 Source or run statistical test results  ----
# 
# source("C:/Users/denardo/Dropbox (Personal)/MPA_R_Scripts_for_Kelly/SBS/Scripts/Koon_TechReport_SigTests.R")


# ---- 1.2 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Koon.level.PropData.status <- 
  rbind.data.frame(data.frame(SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==18,c("HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
                                                                                            "Percent.Rel.Other", "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", 
                                                                                            "Percent.PrimaryOcc.WageLabor", "Percent.PrimaryOcc.HarvestForest", 
                                                                                            "Percent.PrimaryOcc.Tourism", "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction", 
                                                                                            "Percent.PrimaryOcc.Other", "Percent.SecondaryOcc.Fish", "Percent.SecondaryOcc.Farm", 
                                                                                            "Percent.SecondaryOcc.WageLabor", "Percent.SecondaryOcc.HarvestForest", 
                                                                                            "Percent.SecondaryOcc.Tourism", "Percent.SecondaryOcc.Aquaculture", "Percent.SecondaryOcc.Extraction", 
                                                                                            "Percent.SecondaryOcc.Other")]),
                   data.frame(SettlementID=0,
                              SettlementName="Koon MPA",
                              Techreport.ByMPA[Techreport.ByMPA$MPAID==18,c("HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
                                                                            "Percent.Rel.Other", "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", 
                                                                            "Percent.PrimaryOcc.WageLabor", "Percent.PrimaryOcc.HarvestForest", 
                                                                            "Percent.PrimaryOcc.Tourism", "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction", 
                                                                            "Percent.PrimaryOcc.Other", "Percent.SecondaryOcc.Fish", "Percent.SecondaryOcc.Farm", 
                                                                            "Percent.SecondaryOcc.WageLabor", "Percent.SecondaryOcc.HarvestForest", 
                                                                            "Percent.SecondaryOcc.Tourism", "Percent.SecondaryOcc.Aquaculture", "Percent.SecondaryOcc.Extraction", 
                                                                            "Percent.SecondaryOcc.Other")]))



null.row.PropData <- 
  matrix(rep(NA,23),ncol=23,dimnames=list(NULL,colnames(Koon.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----


Koon.level.ContData.status <- 
  rbind.data.frame(cbind.data.frame(SettlementID=0,
                                    SettlementName="Control\nSettlements",
                                    BigFive.ControlGroup[BigFive.ControlGroup$MPAID==18,c("FSMean", "FSErr", "MAMean", "MAErr", "PAMean", "PAErr", "MTMean", 
                                                                                          "MTErr", "SEMean", "SEErr")]),
                   cbind.data.frame(SettlementID=0,SettlementName="Koon MPA",
                                    BigFive.MPAGroup[BigFive.MPAGroup$MPAID==18,c("FSMean", "FSErr", "MAMean", "MAErr", "PAMean", "PAErr", "MTMean", 
                                                                                  "MTErr", "SEMean", "SEErr")]))


null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,12),ncol=12,dimnames=list(NULL,colnames(Koon.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Koon, proportional data ----

Koon.PropData.Techreport.status <- 
  Techreport.BySett[Techreport.BySett$MPAID==18,c("SettlementID", "SettlementName", "HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
                                                            "Percent.Rel.Other", "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", 
                                                            "Percent.PrimaryOcc.WageLabor", "Percent.PrimaryOcc.HarvestForest", 
                                                            "Percent.PrimaryOcc.Tourism", "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction", 
                                                            "Percent.PrimaryOcc.Other", "Percent.SecondaryOcc.Fish", "Percent.SecondaryOcc.Farm", 
                                                            "Percent.SecondaryOcc.WageLabor", "Percent.SecondaryOcc.HarvestForest", 
                                                            "Percent.SecondaryOcc.Tourism", "Percent.SecondaryOcc.Aquaculture", "Percent.SecondaryOcc.Extraction", 
                                                            "Percent.SecondaryOcc.Other")]



Koon.PropData.Techreport.status <- 
  Koon.PropData.Techreport.status[rev(order(Koon.PropData.Techreport.status$SettlementName)),]

Koon.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Koon.level.PropData.status,
                   null.row.PropData,
                   Koon.PropData.Techreport.status)


# - make SettlementName an ordered factor for plotting
Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Koon.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Koon.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Koon, continuous data (with p values) ----

Koon.ContData.Techreport.status  <- 
  BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MPAID==18,
                                c("SettlementID", "SettlementName", "FSMean", "FSErr", "MAMean", 
                                  "MAErr", "PAMean", "PAErr", "MTMean", "MTErr", "SEMean", "SEErr")]


Koon.ContData.Techreport.status <- 
  Koon.ContData.Techreport.status[rev(order(Koon.ContData.Techreport.status$SettlementName)),]

Koon.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Koon.level.ContData.status,
                   null.row.ContData,
                   Koon.ContData.Techreport.status)


# - plot-formatted dataset
Koon.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Koon.ContData.Techreport.status.withMPA,
            sigvals.Koon.2018,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add column for plot fill colour formatting
Koon.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Koon.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Synthesize other social data for interpretation/context ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Tech report data synthesis aid ---- 
#   years resident, categorical food security, changes in social conflict, 
#   material assets gini coefficient, mean material assets, % fishers, 
#   % wage labor, marine tenure manage and harvest components
# 
# 
# Koon.level.synth <- rbind.data.frame(cbind.data.frame(SettlementID=NA,
#                                                          Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==17,c("MPAID","MonitoringYear")],
#                                                          SettlementName="MPA",
#                                                          Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==17,3:length(Synth.techreport.byMPA)],
#                                                          AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==17,3]),
#                                         cbind.data.frame(SettlementID=NA,
#                                                          Synth.techreport.byMPA.control[Synth.techreport.byMPA.control$MPAID==17,c("MPAID","MonitoringYear")],
#                                                          SettlementName="Control",
#                                                          Synth.techreport.byMPA.control[Synth.techreport.byMPA.control$MPAID==17,3:length(Synth.techreport.byMPA.control)],
#                                                          AgeGender.AvgAge.control[AgeGender.AvgAge.control$MPAID==17,3]))
# 
# 
# null.row.synth <- matrix(NA,ncol=length(colnames(Koon.level.synth)),
#                          dimnames=list(NULL,colnames(Koon.level.synth)))
# 
# Koon.setts.synth <- 
#   Synth.techreport.bySett[Synth.techreport.bySett$MPAID==17,] %>%
#   left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])
# 
# Koon.setts.synth <- 
#   Koon.setts.synth[rev(order(Koon.setts.synth$SettlementName)),]
# 
# # ---- 3.2 Output for data synthesis/interpretation ----
# 
# Koon.synth.techreport.extra.PLOTFORMAT <- rbind.data.frame(Koon.level.synth,
#                                              null.row.synth,
#                                              Koon.setts.synth)
# 
# # - make SettlementName an ordered factor for plotting
# Koon.synth.techreport.extra.PLOTFORMAT$SettlementName <-
#   ifelse(is.na(Koon.synth.techreport.extra.PLOTFORMAT$SettlementName),"",
#          as.character(Koon.synth.techreport.extra.PLOTFORMAT$SettlementName))
# 
# Koon.synth.techreport.extra.PLOTFORMAT$SettlementName <-
#   factor(Koon.synth.techreport.extra.PLOTFORMAT$SettlementName,
#          levels=unique(Koon.synth.techreport.extra.PLOTFORMAT$SettlementName),
#          ordered=T)
# 
# # - add row for plot fill colour formatting
# Koon.synth.techreport.extra.PLOTFORMAT$Dummy <-
#   ifelse(Koon.synth.techreport.extra.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 4: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 4.1 Define filename for Excel spreadsheet ----

# If necessary, change file name to match the desired directory in your machine.

FileName <- paste(paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Koon/Koon_2018_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")

# ---- 4.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Koon.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Koon.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
# write.xlsx(Koon.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)
# write.xlsx(Koon.synth.techreport.extra.PLOTFORMAT,FileName,sheetName='Extra_data',row.names=F,append=T)



# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Koon.level.PropData.status)
rm(Koon.level.ContData.status)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Koon.PropData.Techreport.status)
rm(Koon.ContData.Techreport.status)
rm(Koon.ContData.Techreport.status.withMPA)
rm(FileName)
