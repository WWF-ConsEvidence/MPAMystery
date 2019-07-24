# 
# code:   Flotim Technical Report Datasets
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
#  1) Source Flotim.TechReport.SigTest.2019.R 
#     - Dependencies: SBS_TechReport_Calculations.R
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

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/SBS_TechReport_Calculations.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/Flotim.TechReport.SigTest.2019.R")


# ---- 1.2 Subset Days Unwell variable by settlement and MPA ----

Days.unwell.Flotim.BySett <- 
  Days.unwell.BySett[Days.unwell.BySett$MPAID==16 & Days.unwell.BySett$MonitoringYear=="3 Year Post" &
                       !is.na(Days.unwell.BySett$SettlementID), c("SettlementID", "MonitoringYear", "UnwellMean", "UnwellErr")]


Days.unwell.Flotim.ByMPA <- 
  Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==16 &
                      !is.na(Days.unwell.ByMPA$MPAID),c("MonitoringYear", "UnwellMean", "UnwellErr")]


Days.unwell.Flotim.control <-
  Days.unwell.control[Days.unwell.control$MPAID==16  &
                        !is.na(Days.unwell.control$MPAID),c("UnwellMean", "UnwellErr")]


# ---- 1.3 Subset Proportional Data of Age/Gender for Flotim----

Flotim.AgeGender <- 
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                      ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==16 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==16 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.3Year=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==16 &
                                                    AgeGenderDemos.ByMPA$MonitoringYear=="3 Year Post",
                                                  seq(3,41,by=2)]),
             Female.3Year=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==16 &
                                                      AgeGenderDemos.ByMPA$MonitoringYear=="3 Year Post",
                                                    seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Flotim.level.PropData.status <- 
  rbind.data.frame(data.frame(MonitoringYear="3 Year Post",
                              SettlementName="Control Settlements",
                              Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==16 & Techreport.ByMPA.control$MonitoringYear=="3 Year Post",c("HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
                                                                                            "Percent.Rel.Other", "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", 
                                                                                            "Percent.PrimaryOcc.WageLabor", "Percent.PrimaryOcc.HarvestForest", 
                                                                                            "Percent.PrimaryOcc.Tourism", "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction", 
                                                                                            "Percent.PrimaryOcc.Other", "Prop.Fish.AlmostNever", 
                                                                                            "Prop.Fish.FewTimesPer6Mo", "Prop.Fish.FewTimesPerMo", "Prop.Fish.FewTimesPerWk", 
                                                                                            "Prop.Fish.MoreFewTimesWk", "Prop.SellFish.AlmostNever", "Prop.SellFish.FewTimesPer6Mo", 
                                                                                            "Prop.SellFish.FewTimesPerMo", "Prop.SellFish.FewTimesPerWk", 
                                                                                            "Prop.SellFish.MoreFewTimesWk", "Prop.IncFish.None", "Prop.IncFish.Some", 
                                                                                            "Prop.IncFish.Half", "Prop.IncFish.Most", "Prop.IncFish.All", 
                                                                                            "Prop.FishTech.ByHand", "Prop.FishTech.StatNet", "Prop.FishTech.MobileNet", 
                                                                                            "Prop.FishTech.StatLine", "Prop.FishTech.MobileLine", "Child.FS.no", 
                                                                                            "Child.FS.yes","ProteinFish.None", "ProteinFish.Some", "ProteinFish.Half", 
                                                                                            "ProteinFish.Most", "ProteinFish.All","Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")]),
                             
                   data.frame(MonitoringYear="3 Year Post",
                              SettlementName="Flores Timur\nMPA",
                              Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==16 & Techreport.Trend.ByMPA$MonitoringYear=="3 Year Post",c("HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
                                                                            "Percent.Rel.Other", "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", 
                                                                            "Percent.PrimaryOcc.WageLabor", "Percent.PrimaryOcc.HarvestForest", 
                                                                            "Percent.PrimaryOcc.Tourism", "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction", 
                                                                            "Percent.PrimaryOcc.Other", "Prop.Fish.AlmostNever", 
                                                                            "Prop.Fish.FewTimesPer6Mo", "Prop.Fish.FewTimesPerMo", "Prop.Fish.FewTimesPerWk", 
                                                                            "Prop.Fish.MoreFewTimesWk", "Prop.SellFish.AlmostNever", "Prop.SellFish.FewTimesPer6Mo", 
                                                                            "Prop.SellFish.FewTimesPerMo", "Prop.SellFish.FewTimesPerWk", 
                                                                            "Prop.SellFish.MoreFewTimesWk", "Prop.IncFish.None", "Prop.IncFish.Some", 
                                                                            "Prop.IncFish.Half", "Prop.IncFish.Most", "Prop.IncFish.All", 
                                                                            "Prop.FishTech.ByHand", "Prop.FishTech.StatNet", "Prop.FishTech.MobileNet", 
                                                                            "Prop.FishTech.StatLine", "Prop.FishTech.MobileLine", "Child.FS.no", 
                                                                            "Child.FS.yes","ProteinFish.None", "ProteinFish.Some", "ProteinFish.Half", 
                                                                            "ProteinFish.Most", "ProteinFish.All","Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")]))
                           



null.row.PropData <- 
  matrix(rep(NA,length(Flotim.level.PropData.status)),ncol=length(Flotim.level.PropData.status),dimnames=list(NULL,colnames(Flotim.level.PropData.status)))



# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----


Flotim.level.ContData.status <- 
  rbind.data.frame(cbind.data.frame(MonitoringYear="3 Year Post",SettlementID=0,SettlementName="Control Settlements",
                                    BigFive.ControlGroup[BigFive.ControlGroup$MPAID==16,c("FSMean", "FSErr", "MAMean", "MAErr", "MTMean","MTErr", "PAMean", "PAErr", "SEMean", "SEErr")],
                                    Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==16,c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Flotim.control[,c("UnwellMean","UnwellErr")]),
                   cbind.data.frame(MonitoringYear="3 Year Post",SettlementID=0,SettlementName="FlotimMPA",
                                    BigFive.MPAGroup[BigFive.MPAGroup$MPAID==16 & BigFive.MPAGroup$MonitoringYear=="3 Year Post",c("FSMean", "FSErr", "MAMean", "MAErr", "MTMean","MTErr","PAMean", "PAErr", "SEMean", "SEErr")],
                                    Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==16 & Techreport.Trend.ByMPA$MonitoringYear=="3 Year Post",c("TimeMarketMean","TimeMarketErr")],
                                    Days.unwell.Flotim.ByMPA[Days.unwell.Flotim.ByMPA$MonitoringYear=="3 Year Post",c("UnwellMean","UnwellErr")]))

Flotim.level.ContData.annex <- 
  cbind.data.frame(MonitoringYear=c("3 Year Post","Baseline"),
                   SettlementID=0,SettlementName="MPA",
                   BigFive.MPAGroup[BigFive.MPAGroup$MPAID==16,3:12],
                   Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==16,c("TimeMarketMean","TimeMarketErr")],
                   Days.unwell.Flotim.ByMPA[,c("UnwellMean","UnwellErr")])

Techreport.ByMPA.control.annex <- Techreport.ByMPA.control.annex %>% filter(MPAID==16)

Flotim.level.ContData.control.annex <-
  cbind.data.frame(SettlementName="Control\nSettlements", SettlementID=0,
                   Techreport.ByMPA.control.annex[Techreport.ByMPA.control.annex$MPAID==16,2:12],
                   Techreport.ByMPA.control.annex[Techreport.ByMPA.control.annex$MPAID==16,60:63])

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,length(Flotim.level.ContData.status)),ncol=17,dimnames=list(NULL,colnames(Flotim.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Flotim, proportional data ----

Flotim.PropData.Techreport.status <- 
  (Techreport.Status.BySett[Techreport.Status.BySett$MPAID==16 & Techreport.Status.BySett$MonitoringYear=="3 Year Post",c( "SettlementName", "HHH.female", "HHH.male", 
                                                            "Percent.Rel.Christian", "Percent.Rel.Muslim", "Percent.Rel.Other", 
                                                            "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", "Percent.PrimaryOcc.WageLabor", 
                                                            "Percent.PrimaryOcc.HarvestForest", "Percent.PrimaryOcc.Tourism", 
                                                            "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction",
                                                            "Percent.PrimaryOcc.Other", "Prop.Fish.AlmostNever", "Prop.Fish.FewTimesPer6Mo", 
                                                            "Prop.Fish.FewTimesPerMo", "Prop.Fish.FewTimesPerWk", "Prop.Fish.MoreFewTimesWk", 
                                                            "Prop.SellFish.AlmostNever", "Prop.SellFish.FewTimesPer6Mo", 
                                                            "Prop.SellFish.FewTimesPerMo", "Prop.SellFish.FewTimesPerWk", 
                                                            "Prop.SellFish.MoreFewTimesWk", "Prop.IncFish.None", "Prop.IncFish.Some", 
                                                            "Prop.IncFish.Half", "Prop.IncFish.Most", "Prop.IncFish.All", 
                                                            "Prop.FishTech.ByHand", "Prop.FishTech.StatNet", "Prop.FishTech.MobileNet", 
                                                            "Prop.FishTech.StatLine", "Prop.FishTech.MobileLine", "Child.FS.no", 
                                                            "Child.FS.yes", "ProteinFish.None", "ProteinFish.Some", 
                                                            "ProteinFish.Half", "ProteinFish.Most", "ProteinFish.All","Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")])
        


Flotim.PropData.Techreport.status <- 
  Flotim.PropData.Techreport.status[rev(order(Flotim.PropData.Techreport.status$SettlementName)),]

Flotim.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Flotim.level.PropData.status[c("SettlementName", "HHH.female", "HHH.male", 
                                                "Percent.Rel.Christian", "Percent.Rel.Muslim", "Percent.Rel.Other", 
                                                "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", "Percent.PrimaryOcc.WageLabor", 
                                                "Percent.PrimaryOcc.HarvestForest", "Percent.PrimaryOcc.Tourism", 
                                                "Percent.PrimaryOcc.Aquaculture", "Percent.PrimaryOcc.Extraction",
                                                "Percent.PrimaryOcc.Other", "Prop.Fish.AlmostNever", "Prop.Fish.FewTimesPer6Mo", 
                                                "Prop.Fish.FewTimesPerMo", "Prop.Fish.FewTimesPerWk", "Prop.Fish.MoreFewTimesWk", 
                                                "Prop.SellFish.AlmostNever", "Prop.SellFish.FewTimesPer6Mo", 
                                                "Prop.SellFish.FewTimesPerMo", "Prop.SellFish.FewTimesPerWk", 
                                                "Prop.SellFish.MoreFewTimesWk", "Prop.IncFish.None", "Prop.IncFish.Some", 
                                                "Prop.IncFish.Half", "Prop.IncFish.Most", "Prop.IncFish.All", 
                                                "Prop.FishTech.ByHand", "Prop.FishTech.StatNet", "Prop.FishTech.MobileNet", 
                                                "Prop.FishTech.StatLine", "Prop.FishTech.MobileLine", "Child.FS.no", 
                                                "Child.FS.yes", "ProteinFish.None", "ProteinFish.Some", "ProteinFish.Half", 
                                                "ProteinFish.Most", "ProteinFish.All","Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")],
                   null.row.PropData[c( "SettlementName", "HHH.female", "HHH.male", 
                                       "Percent.Rel.Christian", "Percent.Rel.Muslim", "Percent.Rel.Other", 
                                       "Percent.PrimaryOcc.Fish", "Percent.PrimaryOcc.Farm", "Percent.PrimaryOcc.WageLabor", 
                                       "Percent.PrimaryOcc.HarvestForest", "Percent.PrimaryOcc.Tourism", "Percent.PrimaryOcc.Aquaculture", 
                                       "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.Other", "Prop.Fish.AlmostNever", "Prop.Fish.FewTimesPer6Mo", 
                                       "Prop.Fish.FewTimesPerMo", "Prop.Fish.FewTimesPerWk", "Prop.Fish.MoreFewTimesWk", 
                                       "Prop.SellFish.AlmostNever", "Prop.SellFish.FewTimesPer6Mo", 
                                       "Prop.SellFish.FewTimesPerMo", "Prop.SellFish.FewTimesPerWk", 
                                       "Prop.SellFish.MoreFewTimesWk", "Prop.IncFish.None", "Prop.IncFish.Some", 
                                       "Prop.IncFish.Half", "Prop.IncFish.Most", "Prop.IncFish.All", 
                                       "Prop.FishTech.ByHand", "Prop.FishTech.StatNet", "Prop.FishTech.MobileNet", 
                                       "Prop.FishTech.StatLine", "Prop.FishTech.MobileLine", "Child.FS.no", 
                                       "Child.FS.yes", "ProteinFish.None", "ProteinFish.Some", "ProteinFish.Half", 
                                       "ProteinFish.Most", "ProteinFish.All","Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")],
                   Flotim.PropData.Techreport.status)

Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName <- gsub("Flores Timur\nMPA","Flores Timur MPA",Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName)
Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName <- gsub("Control\nSettlements","Control Settlements",Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName)

# - make SettlementName an ordered factor for plotting
Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName))

Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Flotim.PropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Flotim.PropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.2 Status dataset for Flotim, continuous data (with p values) ----

Flotim.ContData.Techreport.status  <- 
  left_join(BigFive.bySett[BigFive.bySett$Treatment==1 &
                                  BigFive.bySett$MPAID==16 &
                                  BigFive.bySett$MonitoringYear=="3 Year Post",
                                c("SettlementID", "SettlementName", "FSMean", "FSErr", "MAMean", 
                                  "MAErr", "MTMean","MTErr", "PAMean", "PAErr", "SEMean", "SEErr"
                                )],
            Techreport.Status.BySett[Techreport.Status.BySett$MPAID==16,c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by="SettlementID")


Flotim.ContData.Techreport.status <- 
  left_join(Flotim.ContData.Techreport.status,
            Days.unwell.Flotim.BySett[,c("SettlementID", "UnwellMean", "UnwellErr")],
            by="SettlementID")


Flotim.ContData.Techreport.status <- 
  Flotim.ContData.Techreport.status[rev(order(Flotim.ContData.Techreport.status$SettlementName)),]

Flotim.ContData.Techreport.status <- Flotim.ContData.Techreport.status %>%
  filter(Flotim.ContData.Techreport.status$MonitoringYear=="3 Year Post")

Flotim.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Flotim.level.ContData.status[c("SettlementID", "SettlementName", "FSMean", "FSErr", "MAMean", 
                                                "MAErr", "MTMean","MTErr","PAMean", "PAErr", "SEMean", "SEErr", 
                                                "TimeMarketMean", "TimeMarketErr", "UnwellMean", "UnwellErr")],
                   null.row.ContData[c("SettlementID", "SettlementName", "FSMean", "FSErr", "MAMean", "MAErr", "MTMean","MTErr",
                                        "PAMean", "PAErr", "SEMean", "SEErr", 
                                       "TimeMarketMean", "TimeMarketErr", "UnwellMean", "UnwellErr")],
                   Flotim.ContData.Techreport.status[c("SettlementID", "SettlementName", "FSMean", "FSErr", "MAMean", 
                                                     "MAErr","MTMean","MTErr","PAMean", "PAErr", "SEMean", "SEErr", 
                                                     "TimeMarketMean", "TimeMarketErr", "UnwellMean", "UnwellErr")])


# - plot-formatted dataset
Flotim.ContData.Techreport.status.PLOTFORMAT <- 
  left_join(Flotim.ContData.Techreport.status.withMPA,
            sigvals.Flotim,by="SettlementName")

Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName <- gsub("FlotimMPA","Flores Timur MPA",Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName)

# - make SettlementName an ordered factor for plotting
Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName),"",
         Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName)

Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add column for plot fill colour formatting
Flotim.ContData.Techreport.status.PLOTFORMAT$SettLevel <- 
  ifelse(Flotim.ContData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


# ---- 2.3 Trend dataset for Flotim, MPA-level proportional data ----

Flotim.TrendPropData.Techreport.PLOTFORMAT <- 
  Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==16,c(2,1,3:48,71:75)]


# ---- 2.4 Trend dataset for Flotim, MPA-level continuous data (with p values) ----

Flotim.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Flotim.level.ContData.annex[,c(1,4:17)],
                   trend.sigvals.Flotim)

# - make MonitoringYear an ordered factor for plotting
Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","3 Year Post"),
         ordered=T)




# ---- 2.6 Annex dataset for Flotim, Settlement-level continuous data (with p values) ----

#Removing settlements that were not resampled and/possibly miscoded (Alila Timur) 

BigFive.bySett <- BigFive.bySett %>%
  filter(SettlementID != 149 & SettlementID != 150 )

Techreport.Status.BySett <- Techreport.Status.BySett %>%
  filter(SettlementID != 149 & SettlementID != 150)



Flotim.AnnexContData.Techreport <- 
  left_join(BigFive.bySett[BigFive.bySett$MPAID==16 &
                                  BigFive.bySett$Treatment==1,
                                c(5,1,2,6:15)],
            Techreport.Status.BySett[Techreport.Status.BySett$MPAID==16, 
                                       c("SettlementID","MonitoringYear","TimeMarketMean","TimeMarketErr")],
            by=c("SettlementID","MonitoringYear"))

Flotim.AnnexContData.Techreport <- 
  left_join(Flotim.AnnexContData.Techreport,
            Days.unwell.BySett[c("SettlementID","MonitoringYear","UnwellMean","UnwellErr")],
            by=c("SettlementID","MonitoringYear"))

Flotim.AnnexContData.Techreport$MonitoringYear <- 
  factor(Flotim.AnnexContData.Techreport$MonitoringYear,
         levels=c("3 Year Post","Baseline"),ordered=T)

Flotim.AnnexContData.Techreport <- 
  Flotim.AnnexContData.Techreport[rev(order(Flotim.AnnexContData.Techreport$SettlementName,
                                             Flotim.AnnexContData.Techreport$MonitoringYear)),]
                                

Flotim.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Flotim.level.ContData.annex[Flotim.level.ContData.annex$MonitoringYear=="3 Year Post",],
                   Flotim.level.ContData.annex[Flotim.level.ContData.annex$MonitoringYear=="Baseline",],
                   Flotim.level.ContData.control.annex[Flotim.level.ContData.control.annex$MonitoringYear=="3 Year Post",],
                   Flotim.level.ContData.control.annex[Flotim.level.ContData.control.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Flotim.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Flotim.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Flotim.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("3 Year Post","Baseline"),
         ordered=T)

# - make SettlementName an ordered factor for plotting

Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
 ifelse(is.na(Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
        Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)


# - add row for plot fill colour formatting
Flotim.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Flotim.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")




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


Flotim.level.synth <- data.frame(SettlementID=NA,
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==16,c("MPAID","MonitoringYear")],
                                  SettlementName="MPA",
                                  Synth.techreport.byMPA[Synth.techreport.byMPA$MPAID==16,3:17],
                                  AgeGender.AvgAge.byMPA[AgeGender.AvgAge.byMPA$MPAID==16,3])

Flotim.level.synth <- left_join(Flotim.level.synth,
                                 Techreport.Trend.ByMPA[c("MPAID","MonitoringYear",
                                                    "Percent.PrimaryOcc.Fish",
                                                    "Percent.PrimaryOcc.WageLabor")],
                                 by=c("MPAID","MonitoringYear"))

null.row.synth <- matrix(NA,ncol=length(colnames(Flotim.level.synth)),
                         dimnames=list(NULL,colnames(Flotim.level.synth)))

Flotim.setts.synth <- 
  Synth.techreport.bySett[Synth.techreport.bySett$MPAID==16,1:18] %>%
  left_join(Techreport.Status.BySett[,c("SettlementID","MonitoringYear", "Treatment",
                                 "Percent.PrimaryOcc.Fish",
                                 "Percent.PrimaryOcc.WageLabor")],
            by=c("SettlementID","MonitoringYear")) %>%
  left_join(AgeGender.AvgAge.bySett[,c("SettlementName","MonitoringYear","AvgAge")])

# ---- 3.2 Output for data synthesis/interpretation ----

Flotim.synth.techreport.extra.PLOTFORMAT <- rbind.data.frame(Flotim.level.synth,
                                             null.row.synth,
                                             Flotim.setts.synth)

# - make SettlementName an ordered factor for plotting
Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName),"",
         as.character(Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName))

Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName <-
  factor(Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName,
         levels=unique(Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Flotim.synth.techreport.extra.PLOTFORMAT$Dummy <-
  ifelse(Flotim.synth.techreport.extra.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")



#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 4: Function to define asterisks and reference settlements ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# Defining Plot Labels
continuous.variables.plotlabs <- c("Mean household food security","Mean household assets",
                                   "Mean Marine Tenure","Mean Place Attachment",
                                   "School enrollment (% children ages 5-18 years old)", "Distance to market (hours)",
                                   "Mean time suffering from illness or injury in past 4 weeks (days)")

# ---- Functions to define number of asterisks & reference settlement -- status plots ----

define.statusplot.asterisks <- function(x) {
  result <- x
  reference <- x
  for(a in colnames(x[2:8])){
    for(i in 1:length(x$SettlementName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
      reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
    }
  }
  colnames(result) <- c("SettlementName","FS","MA","MT","PA","SE","Market","Unwell")
  colnames(reference) <- c("SettlementName","FS.ref","MA.ref","MT.ref","PA.ref","SE.ref","Market.ref","Unwell.ref")
  result <- left_join(result,reference,by="SettlementName")
  result
}


# Define (x,y) position of asterisks & reference settlement "R" -- status plots
define.statusplot.asterisk.pos <- function(x,asterisks) {
  result <- asterisks[,1:8]
  ref <- asterisks[,c(1,9:15)]
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,2:8] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks[,2:8],
                         d=c(1:7),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,2:8] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks[,9:14],
                      d=c(1:7),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="SettlementName")
  result
}


# Define y labels, with asterisks -- continuous variables trend plots
define.conttrendplot.ylabels.withasterisks <- function(x) {
  result <- x
  labs <- continuous.variables.plotlabs
  for(a in 1:7) {
    result[a] <- ifelse(as.numeric(x[a])<0.01,paste(labs[a],"***",sep=" "),
                        ifelse(as.numeric(x[a])<0.05 & as.numeric(x[a])>=0.01,paste(labs[a],"**",sep=" "),
                               ifelse(as.numeric(x[a])<0.1 & as.numeric(x[a])>=0.05,paste(labs[a],"*",sep=" "),
                                      labs[a])))
  }
  result
}

# Define y labels, with asterisks -- proportional variables trend plots
define.proptrendplot.ylabels.withasterisks <- function(x) {
  result <- x
  labs <- proportional.variables.plotlabs
  for(a in 1:7) {
    result[a] <- ifelse(as.numeric(x[a])<0.01,paste(labs[a],"***",sep=" "),
                        ifelse(as.numeric(x[a])<0.05 & as.numeric(x[a])>=0.01,paste(labs[a],"**",sep=" "),
                               ifelse(as.numeric(x[a])<0.1 & as.numeric(x[a])>=0.05,paste(labs[a],"*",sep=" "),
                                      labs[a])))
  }
  result
}

# Define Settlement Name labels, with asterisks -- annex plots
define.annexplot.settname.labels <- function(x) {
  result <- x
  sett.names <- x$SettlementName
  for(a in colnames(x[2:8])) {
    for(i in 1:length(x$SettlementName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,
                            paste("***",as.character(sett.names[i]),sep=" "),
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,
                                   paste("**",as.character(sett.names[i]),sep=" "),
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,
                                          paste("*",as.character(sett.names[i]),sep=" "),
                                          as.character(sett.names[i]))))
    }
  }
  colnames(result) <- c("SettlementName","FS","MA","MT","PA","SE","TimeMarket","Unwell")
  result
}



# Define Year/Monitoring Year column for axis & legend labels
define.year.monitoryear.column <- function(annex.data) {
  result <- annex.data[5:10,c("SettlementID","MonitoringYear")]
  result <- left_join(result,HHData[,c("SettlementID","InterviewYear","MonitoringYear")],
                      by=c("SettlementID","MonitoringYear"))
  result <- result[!is.na(result$MonitoringYear) &
                     !is.na(result$InterviewYear),]
  result$MonitoringYear <- ifelse(result$MonitoringYear=="3 Year Post", paste(result$MonitoringYear,"\nBaseline",sep=""),
                                  as.character(result$MonitoringYear))
  result$Monitoryear.year <- c(NA)
  for(i in 1:length(result$MonitoringYear)){
    result$Monitoryear.year[i] <- paste(result$MonitoringYear[i],"\n","(",result$InterviewYear[i],")",sep="")
  }
  result.final <- c(unique(result$Monitoryear.year))
  result.final
}



# +++++++++++++++++++++++++++++++++++++++++
# 
# FUNCTIONS FOR SEASCAPE-LEVEL PLOTS
# 
# +++++++++++++++++++++++++++++++++++++++++


# Define number of asterisks & reference settlement -- FOR SEASCAPE-LEVEL status plots
define.seascape.statusplot.asterisks <- function(x) {
  result <- x
  reference <- x
  for(a in colnames(x[1,2:8])){
    for(i in 1:length(x$MPAName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
      reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
    }
  }
  colnames(result) <- c("SettlementName","FS","MA","PA","MT","SE","Time","Unwell")
  colnames(reference) <- c("Settle","FS.ref","MA.ref","PA.ref","MT.ref","SE.ref","Time.ref","Unwell.ref")
  result <- left_join(result,reference,by="MPAName")
  result
}


# Define (x,y) position of asterisks & reference settlement "R" -- FOR SEASCAPE-LEVEL status plots
define.seascape.statusplot.asterisk.pos <- function(x,asterisks) {
  result <- asterisks[,1:8]
  ref <- asterisks[,c(1,9:15)]
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,2:8] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks[,2:8],
                         d=c(1:7),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,2:8] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks[,9:15],
                      d=c(1:7),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="MPAName")
  result
}

# Define number of asterisks & reference settlement -- FOR SEASCAPE-LEVEL BASELINE plots
define.seascape.baselineplot.asterisks <- function(x) {
  result <- x
  reference <- x
  for(a in colnames(x[2:7])){
    for(i in 1:length(x$MPAName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
      reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
    }
  }
  colnames(result) <- c("MPAName","FS","MA","PA","MT","SE","Unwell")
  colnames(reference) <- c("MPAName","FS.ref","MA.ref","PA.ref","MT.ref","SE.ref","Unwell.ref")
  result <- left_join(result,reference,by="MPAName")
  result
}


# Define (x,y) position of asterisks & reference settlement "R" -- FOR SEASCAPE-LEVEL BASELINE plots
define.seascape.baselineplot.asterisk.pos <- function(x,asterisks) {
  result <- asterisks[,1:7]
  ref <- asterisks[,c(1,8:13)]
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,1:7] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks[,1:7],
                         d=c(1:7),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,1:7] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks[,8:13],
                      d=c(1:7),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="MPAName")
  result
}


# Define MPA Name labels, with asterisks -- FOR SEASCAPE-LEVEL annex plots
define.annexplot.MPAname.labels <- function(x) {
  result <- x
  sett.names <- x$MPAName
  for(a in colnames(x[2:8])) {
    for(i in 1:length(x$MPAName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,
                            paste("***",as.character(sett.names[i]),sep=" "),
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,
                                   paste("**",as.character(sett.names[i]),sep=" "),
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,
                                          paste("*",as.character(sett.names[i]),sep=" "),
                                          as.character(sett.names[i]))))
    }
  }
  colnames(result) <- c("MPAName","FS","MA","PA","MT","SE","Time","Unwell")
  result
}

