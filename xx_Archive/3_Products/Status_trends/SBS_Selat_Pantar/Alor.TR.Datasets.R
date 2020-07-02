# 
# code:   Alor Technical Report Datasets
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2015
# modified: November 2017
# 
# 
# ---- inputs ----
#  1) Source: Alor.TR.SigTest.R
#     - Dependencies: 
#                     After_Calculate_BigFive.R
#                     Calculate_BigFive.R
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

source("C:/Users/HP/Dropbox/NotThisOne/Source_social_data_flat_files.R")
source("C:/Users/HP/Dropbox/NotThisOne/Calculate_BigFive.R")
source("C:/Users/HP/Dropbox/NotThisOne/AFTER_CALCULATE_BIGFIVE.R")
source("C:/Users/HP/Dropbox/NotThisOne/Alor.TR.SigTest.R")


# ---- 1.3 Subset Proportional Data of Age/Gender for Alor----

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
             Male.3Year=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==15 &
                                                 AgeGenderDemos.ByMPA$MonitoringYear=="3 Year Post",
                                               seq(3,41,by=2)]),
             Female.3Year=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MPAID==15 &
                                                   AgeGenderDemos.ByMPA$MonitoringYear=="3 Year Post",
                                                 seq(4,42,by=2)]),
             row.names=NULL)


# ---- 1.4 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report) ----

Alor.level.PropData.status <- 
  rbind.data.frame(data.frame(MonitoringYear="3 Year Post",
                              SettlementName="Control Settlements",
                              Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==15 & Techreport.ByMPA.control$MonitoringYear=="3 Year Post",c("HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
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
                              SettlementName="Alor MPA",
                              Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==15 &  Techreport.Trend.ByMPA$Treatment==1 & Techreport.Trend.ByMPA$MonitoringYear=="3 Year Post",c("HHH.female", "HHH.male", "Percent.Rel.Christian", "Percent.Rel.Muslim", 
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
  matrix(rep(NA,length(Alor.level.PropData.status)),ncol=length(Alor.level.PropData.status),dimnames=list(NULL,colnames(Alor.level.PropData.status)))



# ---- 1.5 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report) ----


Alor.level.ContData.status <- 
  rbind.data.frame(cbind.data.frame(SettlementID=0,SettlementName="Control Settlements",
                                    Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==15 & Techreport.ByMPA.control$MonitoringYear=="3 Year Post",c("MonitoringYear","FSMean", "FSErr",
                                                                                                                                                           "MAMean", "MAErr", "MTMean","MTErr", "PAMean", "PAErr", "SEMean", "SEErr","TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")]),
                   cbind.data.frame(SettlementID=0,SettlementName="MPA Settlements",
                                    Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==15 & Techreport.Trend.ByMPA$Treatment==1 & 
                                                             Techreport.Trend.ByMPA$MonitoringYear=="3 Year Post",c("MonitoringYear","FSMean", "FSErr", "MAMean", 
                                                                                                                    "MAErr", "MTMean","MTErr","PAMean", "PAErr", "SEMean", "SEErr","TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")]))

Alor.level.ContData.control.annex <- 
  cbind.data.frame(SettlementID=0,SettlementName="Control Settlements",
                   Techreport.ByMPA.control[Techreport.ByMPA.control$MPAID==15,c("MonitoringYear","FSMean", "FSErr", "MAMean", 
                                                                                 "MAErr", "MTMean","MTErr","PAMean", "PAErr", "SEMean", "SEErr","TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")])

Alor.level.ContData.annex <-
  cbind.data.frame(SettlementName="MPA Settlements", SettlementID=0,
                   Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==15 & Techreport.Trend.ByMPA$Treatment==1,c("MonitoringYear","FSMean", "FSErr", "MAMean", 
                                                                                                                   "MAErr", "MTMean","MTErr","PAMean", "PAErr", "SEMean", "SEErr","TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")])

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,length(Alor.level.ContData.status)),ncol=17,dimnames=list(NULL,colnames(Alor.level.ContData.status))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for Alor, proportional data ----

Alor.PropData.Techreport.status <- 
  (Techreport.Status.BySett[Techreport.Status.BySett$MPAID==15 & Techreport.Status.BySett$Treatment==1 & 
                              Techreport.Status.BySett$MonitoringYear=="3 Year Post",
                            c( "SettlementName", "HHH.female", "HHH.male", 
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
                               "ProteinFish.Half", "ProteinFish.Most", "ProteinFish.All",
                               "Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")])



Alor.PropData.Techreport.status <- 
  Alor.PropData.Techreport.status[rev(order(Alor.PropData.Techreport.status$SettlementName)),]

Alor.PropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Alor.level.PropData.status[c("SettlementName", "HHH.female", "HHH.male", 
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

Alor.ContData.Techreport.status  <- 
  Techreport.Status.BySett[Techreport.Status.BySett$Treatment==1 &
                             Techreport.Status.BySett$MPAID==15 &
                             Techreport.Status.BySett$MonitoringYear=="3 Year Post",
                           c("SettlementID", "SettlementName", "MonitoringYear", "FSMean", "FSErr", "MAMean", 
                             "MAErr", "MTMean","MTErr", "PAMean", "PAErr", "SEMean", "SEErr","TimeMarketMean","TimeMarketErr", "UnwellMean", "UnwellErr"
                           )]


Alor.ContData.Techreport.status <- 
  Alor.ContData.Techreport.status[rev(order(Alor.ContData.Techreport.status$SettlementName)),]

Alor.ContData.Techreport.status.withMPA <- 
  rbind.data.frame(Alor.level.ContData.status,
                   null.row.ContData,
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


# ---- 2.3 Trend dataset for Alor, MPA-level proportional data ----

Alor.TrendPropData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Techreport.Trend.ByMPA[Techreport.Trend.ByMPA$MPAID==15 ,c("Treatment","MonitoringYear","HHH.female", "HHH.male", 
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
        null.row.PropData[c("Treatment","MonitoringYear","HHH.female", "HHH.male", 
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
                    "ProteinFish.Most", "ProteinFish.All","Percent.FoodInsecure.NoHunger","Percent.FoodInsecure.YesHunger","Percent.FoodSecure")])

Alor.TrendPropData.Techreport.PLOTFORMAT$order <- c(1,4,2,5,3)
Alor.TrendPropData.Techreport.PLOTFORMAT$year<-c("Baseline\n(2014)","3 Year Post\nBaseline\n(2017)","","Baseline\n(2014)","3 Year Post\nBaseline\n(2017)")

# ---- 2.4 Trend dataset for Alor, MPA-level continuous data (with p values) ----


Alor.TrendContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Alor.level.ContData.control.annex[,c(2:17)],
                   Alor.level.ContData.annex[,c(3,1,4:17)],
                   trend.sigvals.Alor)

# - make MonitoringYear an ordered factor for plotting
Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("Baseline","3 Year Post"),
         ordered=T)
Alor.TrendContData.Techreport.PLOTFORMAT$SettLevel <- ifelse(is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MAMean),"Dummy","NotDummy")

Alor.TrendContData.Techreport.PLOTFORMAT <- Alor.TrendContData.Techreport.PLOTFORMAT %>%
  filter(!is.na(MonitoringYear))
Alor.TrendContData.Techreport.PLOTFORMAT$order<-c(1,2,3,4)

# ---- 2.6 Annex dataset for Alor, Settlement-level continuous data (with p values) ----

#ADJUST FOR REMOVAL OF SETTLEMENTS, DETERMINE AFTER FINAL RECORD CHECKS ARE IN 
Techreport.Status.BySett <- Techreport.Status.BySett %>%
  filter(SettlementID != 128 & SettlementID !=125 
         & SettlementID !=131 & SettlementID !=116)

Alor.AnnexContData.Techreport <- 
  Techreport.Status.BySett[Techreport.Status.BySett$Treatment==1 &
                             Techreport.Status.BySett$MPAID==15,
                           c("SettlementID", "SettlementName", "MonitoringYear","FSMean", "FSErr", "MAMean", 
                             "MAErr", "MTMean","MTErr", "PAMean", "PAErr", "SEMean", "SEErr","TimeMarketMean","TimeMarketErr", "UnwellMean", "UnwellErr"
                           )]


Alor.AnnexContData.Techreport$MonitoringYear <- 
  factor(Alor.AnnexContData.Techreport$MonitoringYear,
         levels=c("3 Year Post","Baseline"),ordered=T)

Alor.AnnexContData.Techreport <- 
  Alor.AnnexContData.Techreport[rev(order(Alor.AnnexContData.Techreport$SettlementName,
                                            Alor.AnnexContData.Techreport$MonitoringYear)),]


Alor.AnnexContData.Techreport.PLOTFORMAT <- 
  rbind.data.frame(Alor.level.ContData.annex[Alor.level.ContData.annex$MonitoringYear=="3 Year Post",],
                   Alor.level.ContData.annex[Alor.level.ContData.annex$MonitoringYear=="Baseline",],
                   Alor.level.ContData.control.annex[Alor.level.ContData.control.annex$MonitoringYear=="3 Year Post",],
                   Alor.level.ContData.control.annex[Alor.level.ContData.control.annex$MonitoringYear=="Baseline",],
                   null.row.ContData,
                   Alor.AnnexContData.Techreport)

# - make MonitoringYear an ordered factor for plotting
Alor.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear <-
  factor(Alor.AnnexContData.Techreport.PLOTFORMAT$MonitoringYear,
         levels=c("3 Year Post","Baseline"),
         ordered=T)

# - make SettlementName an ordered factor for plotting

Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName),"",
         Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName)

Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName <-
  factor(Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName,
         levels=unique(Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName),
         ordered=T)


# - add row for plot fill colour formatting
Alor.AnnexContData.Techreport.PLOTFORMAT$SettLevel <- 
  ifelse(Alor.AnnexContData.Techreport.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 4: Function to define asterisks and reference settlements ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

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

