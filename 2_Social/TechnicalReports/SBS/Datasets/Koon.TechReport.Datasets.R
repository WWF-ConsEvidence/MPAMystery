# 
# code:  Koon Technical Report Datasets
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: November 2017
# 
# 
# ---- inputs ----
#  1) Source Koon.TechReport.SigTests.R 
#     - Dependencies: SBS_MPA_Mystery.R
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


# ---- 1.1 Source statistical test results from "Koon.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/SBS/SignificanceTestCodes/Koon.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Koon.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==18 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]

Days.unwell.Koon.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==18 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]

Days.unwell.Koon.control <-
  Days.unwell.control[Days.unwell.control$MPAID==18 &
                        !is.na(Days.unwell.control$MPAID),2:3]


# ---- 1.3 Subset Proportional Data of Age/Gender for Koon ----

Koon.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==18 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==18 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Koon.level.PropData.status <- 
  rbind.data.frame(data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==18,2:34],
                              FishProtein.ByMPA.control[FishProtein.ByMPA.control$MPAID==18,7:11]),
                   data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Koon MPA",
                              Techreport.ByMPA[Techreport.ByMPA$MPAID==18,3:35],
                              FishProtein.ByMPA[FishProtein.ByMPA$MPAID==18,8:12]))

null.row.PropData <- 
  matrix(rep(NA,41),ncol=41,dimnames=list(NULL,colnames(Koon.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Koon.level.ContData.status <- 
  rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",SettlementID=0,SettlementName="Control\nSettlements",
                                    BigFive.ControlGroup[BigFive.ControlGroup$MPAID==18,6:15],
                                    Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==18,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Koon.control[,c("UnwellMean","UnwellErr")]),
                   cbind.data.frame(MonitoringYear="Baseline",SettlementID=0,SettlementName="Koon MPA",
                                    BigFive.MPAGroup[BigFive.MPAGroup$MPAID==18,6:15],
                                    Techreport.ByMPA[Techreport.ByMPA$MPAID==18,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Koon.ByMPA[,c("UnwellMean","UnwellErr")]))

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Koon.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Koon, proportional data ----

Koon.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==18,c(1,4:37)],
            FishProtein.BySett[FishProtein.BySett$MPAID==18,c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Koon.PropData.Techreport.status <- 
  Koon.PropData.Techreport.status[rev(order(Koon.PropData.Techreport.status$SettlementName)),]

Koon.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Koon.level.PropData.status[2:41],
                   null.row.PropData[2:41],
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

Koon.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MPAID==18,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==18,c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Koon.ContData.Techreport.status <- 
  left_join(Koon.ContData.Techreport.status,
            Days.unwell.Koon.BySett[,c(1,3,4)],
            by="SettlementID")

Koon.ContData.Techreport.status <- 
  Koon.ContData.Techreport.status[rev(order(Koon.ContData.Techreport.status$SettlementName)),]

Koon.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Koon.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Koon.ContData.Techreport.status)

# - plot-formatted dataset
Koon.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Koon.ContData.Techreport.status.withMPA,
            sigvals.Koon,by="SettlementName")

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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Koon/Koon_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Koon.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Koon.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(Koon.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


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

Koon.level.synth <- rbind.data.frame(cbind.data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==18,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==18,3:length(Synth.techreport.byMPA)],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==18,3]),
                 cbind.data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA.control[Synth.techreport.byMPA.control$MPAID==18,c("MPAID","MonitoringYear")],
                                  SettlementName="Control",
                                  Synth.techreport.byMPA.control[Synth.techreport.byMPA.control$MPAID==18,3:length(Synth.techreport.byMPA.control)],
                                  AgeGender.AvgAge.control[AgeGender.AvgAge.control$MPAID==18,3]))
                 
                 

null.row.synth <- matrix(NA,ncol=length(colnames(Koon.level.synth)),
                         dimnames=list(NULL,colnames(Koon.level.synth)))

Koon.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==18,] %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Koon.synth.techreport <- rbind.data.frame(Koon.level.synth,
                                             null.row.synth,
                                             Koon.setts.synth)


write.xlsx(Koon.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Koon.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==18,] %>%
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
Koon.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==18,] %>%
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
Koon.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==18) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))
  
Koon.foodsec.byocc$SettlementName <- factor(Koon.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Koon.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)



# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Koon.level.PropData.status)
rm(Koon.level.ContData.status)
rm(Koon.level.PropData.annex)
rm(Koon.level.ContData.annex)
rm(Days.unwell.Koon.ByMPA)
rm(Days.unwell.Koon.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Koon.PropData.Techreport.status)
rm(Koon.ContData.Techreport.status)
rm(Koon.AnnexPropData.Techreport)
rm(Koon.AnnexContData.Techreport)
rm(Koon.ContData.Techreport.status.withMPA)
rm(Koon.level.synth)
rm(null.row.synth)
rm(Koon.setts.synth)
rm(Koon.synth.techreport)
rm(FileName)