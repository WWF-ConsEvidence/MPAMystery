# 
# code:  Alor Technical Report Datasets
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
#  1) Source Alor.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Alor.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/SBS/SignificanceTestCodes/Alor.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Alor.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==15 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]

Days.unwell.Alor.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==15 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]

Days.unwell.Alor.control <-
  Days.unwell.control[Days.unwell.control$MPAID==15 &
                        !is.na(Days.unwell.control$MPAID),2:3]


# ---- 1.3 Subset Proportional Data of Age/Gender for Alor ----

Alor.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==15 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==15 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Alor.level.PropData.status <- 
  rbind.data.frame(data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==15,2:34],
                              FishProtein.ByMPA.control[FishProtein.ByMPA.control$MPAID==15,7:11]),
                   data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Selat Pantar MPA",
                              Techreport.ByMPA[Techreport.ByMPA$MPAID==15,3:35],
                              FishProtein.ByMPA[FishProtein.ByMPA$MPAID==15,8:12]))

null.row.PropData <- 
  matrix(rep(NA,41),ncol=41,dimnames=list(NULL,colnames(Alor.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Alor.level.ContData.status <- 
  rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",SettlementID=0,SettlementName="Control\nSettlements",
                                    BigFive.ControlGroup[BigFive.ControlGroup$MPAID==15,6:15],
                                    Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==15,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Alor.control[,c("UnwellMean","UnwellErr")]),
                   cbind.data.frame(MonitoringYear="Baseline",SettlementID=0,SettlementName="Selat Pantar MPA",
                                    BigFive.MPAGroup[BigFive.MPAGroup$MPAID==15,6:15],
                                    Techreport.ByMPA[Techreport.ByMPA$MPAID==15,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Alor.ByMPA[,c("UnwellMean","UnwellErr")]))

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Alor.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Alor, proportional data ----

Alor.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==15,c(1,4:37)],
            FishProtein.BySett[FishProtein.BySett$MPAID==15,c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Alor.PropData.Techreport.status <- 
  Alor.PropData.Techreport.status[rev(order(Alor.PropData.Techreport.status$SettlementName)),]

Alor.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Alor.level.PropData.status[2:41],
                   null.row.PropData[2:41],
                   Alor.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Alor.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Alor.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Alor, continuous data (with p values) ----

Alor.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MPAID==15,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==15,c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Alor.ContData.Techreport.status <- 
  left_join(Alor.ContData.Techreport.status,
            Days.unwell.Alor.BySett[,c(1,3,4)],
            by="SettlementID")

Alor.ContData.Techreport.status <- 
  Alor.ContData.Techreport.status[rev(order(Alor.ContData.Techreport.status$SettlementName)),]

Alor.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Alor.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Alor.ContData.Techreport.status)

# - plot-formatted dataset
Alor.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Alor.ContData.Techreport.status.withMPA,
            sigvals.Alor,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add column for plot fill colour formatting
Alor.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Alor.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Alor/Alor_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Alor.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Alor.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(Alor.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


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

Alor.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==15,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==15,3:length(Synth.techreport.byMPA)],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==15,3])
Alor.level.synth <- left_join(Alor.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Alor.level.synth)),
                         dimnames=list(NULL,colnames(Alor.level.synth)))

Alor.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==15,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Alor.synth.techreport <- rbind.data.frame(Alor.level.synth,
                                             null.row.synth,
                                             Alor.setts.synth)


write.xlsx(Alor.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Alor.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==15,] %>%
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
Alor.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==15,] %>%
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
Alor.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==15) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))
  
Alor.foodsec.byocc$SettlementName <- factor(Alor.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Alor.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)



# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Alor.level.PropData.status)
rm(Alor.level.ContData.status)
rm(Alor.level.PropData.annex)
rm(Alor.level.ContData.annex)
rm(Days.unwell.Alor.ByMPA)
rm(Days.unwell.Alor.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Alor.PropData.Techreport.status)
rm(Alor.ContData.Techreport.status)
rm(Alor.AnnexPropData.Techreport)
rm(Alor.AnnexContData.Techreport)
rm(Alor.ContData.Techreport.status.withMPA)
rm(Alor.level.synth)
rm(null.row.synth)
rm(Alor.setts.synth)
rm(Alor.synth.techreport)
rm(FileName)