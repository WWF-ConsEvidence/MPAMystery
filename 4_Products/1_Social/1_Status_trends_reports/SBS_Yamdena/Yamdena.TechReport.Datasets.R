# 
# code:  Yamdena Technical Report Datasets
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
#  1) Source Yamdena.TechReport.SigTests.R 
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


# ---- 1.1 Source statistical test results from "Yamdena.TechReport.SigTests.R" ----

source("2_Social/TechnicalReports/SBS/SignificanceTestCodes/Yamdena.TechReport.SigTests.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Yamdena.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==19 &
                                        !is.na(Days.unwell.BySett$SettlementID),c(1,3,4,5)]

Days.unwell.Yamdena.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==19 &
                      !is.na(Days.unwell.ByMPA$MPAID),2:4]

Days.unwell.Yamdena.control <-
  Days.unwell.control[Days.unwell.control$MPAID==19 &
                        !is.na(Days.unwell.control$MPAID),2:3]


# ---- 1.3 Subset Proportional Data of Age/Gender for Yamdena ----

Yamdena.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==19 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==19 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Yamdena.level.PropData.status <- 
  rbind.data.frame(data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==19,2:34],
                              FishProtein.ByMPA.control[FishProtein.ByMPA.control$MPAID==19,7:11]),
                   data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Yamdena MPA",
                              Techreport.ByMPA[Techreport.ByMPA$MPAID==19,3:35],
                              FishProtein.ByMPA[FishProtein.ByMPA$MPAID==19,8:12]))

null.row.PropData <- 
  matrix(rep(NA,41),ncol=41,dimnames=list(NULL,colnames(Yamdena.level.PropData.status)))


# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----

Yamdena.level.ContData.status <- 
  rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",SettlementID=0,SettlementName="Control\nSettlements",
                                    BigFive.ControlGroup[BigFive.ControlGroup$MPAID==19,6:15],
                                    Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==19,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Yamdena.control[,c("UnwellMean","UnwellErr")]),
                   cbind.data.frame(MonitoringYear="Baseline",SettlementID=0,SettlementName="Yamdena MPA",
                                    BigFive.MPAGroup[BigFive.MPAGroup$MPAID==19,6:15],
                                    Techreport.ByMPA[Techreport.ByMPA$MPAID==19,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Yamdena.ByMPA[,c("UnwellMean","UnwellErr")]))

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Yamdena.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Yamdena, proportional data ----

Yamdena.PropData.Techreport.status <- 
  left_join(Techreport.BySett[Techreport.BySett$MPAID==19,c(1,4:37)],
            FishProtein.BySett[FishProtein.BySett$MPAID==19,c(1,4,10:14)],
            by=c("SettlementID","SettlementName"))

Yamdena.PropData.Techreport.status <- 
  Yamdena.PropData.Techreport.status[rev(order(Yamdena.PropData.Techreport.status$SettlementName)),]

Yamdena.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Yamdena.level.PropData.status[2:41],
                   null.row.PropData[2:41],
                   Yamdena.PropData.Techreport.status)

# - make SettlementName an ordered factor for plotting
Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Yamdena.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Yamdena.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Yamdena, continuous data (with p values) ----

Yamdena.ContData.Techreport.status <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MPAID==19,
                                c(1,2,6:15)],
            Techreport.BySett[Techreport.BySett$MPAID==19,c("SettlementID","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")

Yamdena.ContData.Techreport.status <- 
  left_join(Yamdena.ContData.Techreport.status,
            Days.unwell.Yamdena.BySett[,c(1,3,4)],
            by="SettlementID")

Yamdena.ContData.Techreport.status <- 
  Yamdena.ContData.Techreport.status[rev(order(Yamdena.ContData.Techreport.status$SettlementName)),]

Yamdena.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Yamdena.level.ContData.status[2:17],
                   null.row.ContData[2:17],
                   Yamdena.ContData.Techreport.status)

# - plot-formatted dataset
Yamdena.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Yamdena.ContData.Techreport.status.withMPA,
            sigvals.Yamdena,by="SettlementName")

# - make SettlementName an ordered factor for plotting
Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add column for plot fill colour formatting
Yamdena.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Yamdena.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Export Data to Excel ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Define filename for Excel spreadsheet ----

FileName <- paste(paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Yamdena/Yamdena_TechReportData--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_"),
                  "xlsx",sep=".")


# ---- 3.2 Write to Excel, each data frame as a new sheet ----

write.xlsx(Yamdena.PropData.Techreport.status.PLOTFORMAT,FileName,sheetName='PropData_StatusPlots',row.names=F)
write.xlsx(Yamdena.ContData.Techreport.status.PLOTFORMAT,FileName,sheetName='ContData_StatusPlots_withpvals',row.names=F,append=T)
write.xlsx(Yamdena.AgeGender,FileName,sheetName='AgeGender',row.names=F,append=T)


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

Yamdena.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==19,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==19,3:length(Synth.techreport.byMPA)],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==19,3])
Yamdena.level.synth <- left_join(Yamdena.level.synth,
                                 Techreport.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Yamdena.level.synth)),
                         dimnames=list(NULL,colnames(Yamdena.level.synth)))

Yamdena.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==19,] %>%
  left_join(Techreport.BySett[,c("SettlementID","MonitoringYear",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])


# ---- 4.2 Output for data synthesis/interpretation ----

Yamdena.synth.techreport <- rbind.data.frame(Yamdena.level.synth,
                                             null.row.synth,
                                             Yamdena.setts.synth)


write.xlsx(Yamdena.synth.techreport,FileName,sheetName='Extra_data',row.names=F,append=T)


# ---- 4.3 Playing around ----

# Compare frequency of fishing sales for entire population vs. only fishing households
Yamdena.FreqSellFish.bySett <- 
  HHDemos.context[HHDemos.context$MPAID==19,] %>%
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
Yamdena.FreqSellFish <- 
  HHDemos.context[HHDemos.context$MPAID==19,] %>%
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
Yamdena.foodsec.byocc <-
  left_join(BigFive[BigFive$Treatment==1,c("HouseholdID","MonitoringYear","SettlementID","MPAID","FSIndex")],
            HHDemos.context[,c("HouseholdID","SettlementName","PrimaryLivelihoodClean")],by="HouseholdID") %>%
  subset(MPAID==19) %>%
  group_by(SettlementID,MonitoringYear) %>%
  summarise(SettlementName=unique(SettlementName),
            PropFishers=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            PropWageLabor=(length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])/length(PrimaryLivelihoodClean[!is.na(PrimaryLivelihoodClean)]))*100,
            FoodSec.Fishers=mean(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.Fishers=sd(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==3 & !is.na(PrimaryLivelihoodClean)])),
            FoodSec.WageLabor=mean(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T),
            FoodSecErr.WageLabor=sd(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)],na.rm=T)/sqrt(length(FSIndex[PrimaryLivelihoodClean==7 & !is.na(PrimaryLivelihoodClean)])))
  
Yamdena.foodsec.byocc$SettlementName <- factor(Yamdena.foodsec.byocc$SettlementName,
                                               levels=c(as.character(rev(sort(unique(Yamdena.foodsec.byocc$SettlementName)))),"  "),
                                               ordered=T)



# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Yamdena.level.PropData.status)
rm(Yamdena.level.ContData.status)
rm(Yamdena.level.PropData.annex)
rm(Yamdena.level.ContData.annex)
rm(Days.unwell.Yamdena.ByMPA)
rm(Days.unwell.Yamdena.BySett)
rm(null.row.PropData)
rm(null.row.ContData)
rm(Yamdena.PropData.Techreport.status)
rm(Yamdena.ContData.Techreport.status)
rm(Yamdena.AnnexPropData.Techreport)
rm(Yamdena.AnnexContData.Techreport)
rm(Yamdena.ContData.Techreport.status.withMPA)
rm(Yamdena.level.synth)
rm(null.row.synth)
rm(Yamdena.setts.synth)
rm(Yamdena.synth.techreport)
rm(FileName)