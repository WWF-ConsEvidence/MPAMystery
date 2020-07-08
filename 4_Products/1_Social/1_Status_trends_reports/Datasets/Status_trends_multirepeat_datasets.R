# 
# code:   Status & Trends Datasets, for data with repeats (e.g., status AND trend)
# 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2020
# modified: 
# 
# 
# ---- inputs ----
#  1) Source Status_trends_multirepeat_sigtests.R
# 
# ---- code sections ----
#  1) Data Sourcing, Configuration, and Subsetting
#  2) Define Datasets for Status, Trend, and Annex Plots for Export
#  3) Synthesize other social data for interpretation/context
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Data Sourcing, Configuration, and Subsetting ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Source or run statistical test results  ----



# ---- 1.2 Subset Age/Gender data ----

# this script currently only allows for up to 4 repeats (or 5 unique monitoring years) to produce the necessary data frames.  

AgeGender <- 
  if(num.years==5) {
    data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                               "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                             levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                             ordered=T),
               Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(3,41,by=2)]),
               Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                      seq(4,42,by=2)]),
               Male.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                     seq(3,41,by=2)]),
               Female.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                       seq(4,42,by=2)]),
               Male.RepeatTwo=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[3],
                                                     seq(3,41,by=2)]),
               Female.RepeatTwo=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[3],
                                                       seq(4,42,by=2)]),
               Male.RepeatThree=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[4],
                                                       seq(3,41,by=2)]),
               Female.RepeatThree=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[4],
                                                         seq(4,42,by=2)]),
               Male.RepeatFour=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[5],
                                                       seq(3,41,by=2)]),
               Female.RepeatFour=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[5],
                                                         seq(4,42,by=2)]),
               row.names=NULL) 
  } else { if(num.years==4) {
    data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                               "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                             levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                             ordered=T),
               Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(3,41,by=2)]),
               Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                      seq(4,42,by=2)]),
               Male.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                     seq(3,41,by=2)]),
               Female.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                       seq(4,42,by=2)]),
               Male.RepeatTwo=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[3],
                                                     seq(3,41,by=2)]),
               Female.RepeatTwo=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[3],
                                                       seq(4,42,by=2)]),
               Male.RepeatThree=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[4],
                                                       seq(3,41,by=2)]),
               Female.RepeatThree=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[4],
                                                         seq(4,42,by=2)]),
               row.names=NULL) 
    } else { if(num.years==3) {
  data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                           ordered=T),
             Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                  seq(3,41,by=2)]),
             Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                    seq(4,42,by=2)]),
             Male.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                   seq(3,41,by=2)]),
             Female.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                     seq(4,42,by=2)]),
             Male.RepeatTwo=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[3],
                                                   seq(3,41,by=2)]),
             Female.RepeatTwo=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[3],
                                                     seq(4,42,by=2)]),
             row.names=NULL) 
      } else { data.frame(AgeCat=factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                                    levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                             "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                                    ordered=T),
                      Male.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                           seq(3,41,by=2)]),
                      Female.Baseline=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear=="Baseline",
                                                             seq(4,42,by=2)]),
                      Male.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                            seq(3,41,by=2)]),
                      Female.RepeatOne=t(AgeGenderDemos.ByMPA[AgeGenderDemos.ByMPA$MonitoringYear==levels(AgeGenderDemos.ByMPA$MonitoringYear)[2],
                                                              seq(4,42,by=2)]),
                      row.names=NULL) }
  }
}


# ---- 1.3 MPA-level proportional data (row to be added to bottom of status and annex plots in tech report) ----

MPA.level.PropData.status <- 
  if(MPA.name$MPAID==21) { # this allows for Wakatobi data to be calculated with separate aggregations for No Take vs. Use zones (because there are no controls)
    data.frame(SettlementName=paste(MPA.Level.Means.byZone$Zone,"Settlements",sep=" "),
               MPA.Level.Means.byZone %>% filter(MonitoringYear==levels(HHData$MonitoringYear)[num.years]) %>%
                 select(Zone, HHH.female, HHH.male, Percent.Rel.Christian, Percent.Rel.Muslim, Percent.Rel.Other, Percent.Rel.Buddhist,
                        Percent.PrimaryOcc.Fish, Percent.PrimaryOcc.Farm, Percent.PrimaryOcc.WageLabor, 
                        Percent.PrimaryOcc.HarvestForest, Percent.PrimaryOcc.Tourism, 
                        Percent.PrimaryOcc.Aquaculture, Percent.PrimaryOcc.Extraction,
                        Percent.PrimaryOcc.Other, Prop.Fish.AlmostNever, Prop.Fish.FewTimesPer6Mo, 
                        Prop.Fish.FewTimesPerMo, Prop.Fish.FewTimesPerWk, Prop.Fish.MoreFewTimesWk, 
                        Prop.SellFish.AlmostNever, Prop.SellFish.FewTimesPer6Mo, 
                        Prop.SellFish.FewTimesPerMo, Prop.SellFish.FewTimesPerWk, 
                        Prop.SellFish.MoreFewTimesWk, Prop.IncFish.None, Prop.IncFish.Some,
                        Prop.IncFish.Half, Prop.IncFish.Most, Prop.IncFish.All, 
                        Prop.FishTech.ByHand, Prop.FishTech.StatNet, Prop.FishTech.MobileNet, 
                        Prop.FishTech.StatLine, Prop.FishTech.MobileLine, Child.FS.no, 
                        Child.FS.yes, ProteinFish.None, ProteinFish.Some, 
                        ProteinFish.Half, ProteinFish.Most, ProteinFish.All,Percent.FoodInsecure.NoHunger,
                        Percent.FoodInsecure.YesHunger,Percent.FoodSecure, Percent.SecondaryOcc.Fish, 
                        Percent.SecondaryOcc.Farm, Percent.SecondaryOcc.WageLabor, Percent.SecondaryOcc.HarvestForest,
                        Percent.SecondaryOcc.Tourism, Percent.SecondaryOcc.Aquaculture, Percent.SecondaryOcc.Extraction,
                        Percent.SecondaryOcc.Other, Percent.OneOcc.Diverse, Percent.MultipleOcc.Diverse, Econ.Status.Much.Worse,
                        Econ.Status.Slighly.Worse, Econ.Status.Neutral, Econ.Status.Slightly.Better, Econ.Status.Much.Better,
                        Threat.None, Threat.One, Threat.Two, Threat.Three, Threat.Four, Threat.Minimum.Five,
                        MarineMember.No, MarineMember.Yes, MarineMeeting.No, MarineMeeting.Yes, MarineContribution, Percent.GreatlyIncreased.SocConflict,
                        Percent.Increased.SocConflict, Percent.Same.SocConflict, Percent.Decreased.SocConflict, 
                        Percent.GreatlyDecreased.SocConflict))
  } else {
    data.frame(SettlementName=c("Control Settlements", MPA.name$MPAName),
               MPA.Level.Means %>% filter(MonitoringYear==levels(HHData$MonitoringYear)[num.years]) %>% # this pulls out the most recent monitoring year for the status data
                 select(HHH.female, HHH.male, Percent.Rel.Christian, Percent.Rel.Muslim, Percent.Rel.Other, 
                        Percent.PrimaryOcc.Fish, Percent.PrimaryOcc.Farm, Percent.PrimaryOcc.WageLabor, 
                        Percent.PrimaryOcc.HarvestForest, Percent.PrimaryOcc.Tourism, 
                        Percent.PrimaryOcc.Aquaculture, Percent.PrimaryOcc.Extraction,
                        Percent.PrimaryOcc.Other, Prop.Fish.AlmostNever, Prop.Fish.FewTimesPer6Mo, 
                        Prop.Fish.FewTimesPerMo, Prop.Fish.FewTimesPerWk, Prop.Fish.MoreFewTimesWk, 
                        Prop.SellFish.AlmostNever, Prop.SellFish.FewTimesPer6Mo, 
                        Prop.SellFish.FewTimesPerMo, Prop.SellFish.FewTimesPerWk, 
                        Prop.SellFish.MoreFewTimesWk, Prop.IncFish.None, Prop.IncFish.Some,
                        Prop.IncFish.Half, Prop.IncFish.Most, Prop.IncFish.All, 
                        Prop.FishTech.ByHand, Prop.FishTech.StatNet, Prop.FishTech.MobileNet, 
                        Prop.FishTech.StatLine, Prop.FishTech.MobileLine, Child.FS.no, 
                        Child.FS.yes, ProteinFish.None, ProteinFish.Some, 
                        ProteinFish.Half, ProteinFish.Most, ProteinFish.All,Percent.FoodInsecure.NoHunger,
                        Percent.FoodInsecure.YesHunger,Percent.FoodSecure, Percent.SecondaryOcc.Fish, 
                        Percent.SecondaryOcc.Farm, Percent.SecondaryOcc.WageLabor, Percent.SecondaryOcc.HarvestForest,
                        Percent.SecondaryOcc.Tourism, Percent.SecondaryOcc.Aquaculture, Percent.SecondaryOcc.Extraction,
                        Percent.SecondaryOcc.Other, Percent.OneOcc.Diverse, Percent.MultipleOcc.Diverse, Econ.Status.Much.Worse,
                        Econ.Status.Slighly.Worse, Econ.Status.Neutral, Econ.Status.Slightly.Better, Econ.Status.Much.Better,
                        Threat.None, Threat.One, Threat.Two, Threat.Three, Threat.Four, Threat.Minimum.Five,
                        MarineMember.No, MarineMember.Yes, MarineMeeting.No, MarineMeeting.Yes, MarineContribution, Percent.GreatlyIncreased.SocConflict,
                        Percent.Increased.SocConflict, Percent.Same.SocConflict, Percent.Decreased.SocConflict, 
                        Percent.GreatlyDecreased.SocConflict))
  }

# ---- 1.4 MPA-level continuous data (row to be added to bottom of status and annex plots in tech report) ----

MPA.level.ContData.status <- 
  if(MPA.name$MPAID==21) {
    data.frame(SettlementName=paste(MPA.Level.Means.byZone$Zone,"Settlements",sep=" "),
               MPA.Level.Means.byZone %>% filter(MonitoringYear==levels(MonitoringYear)[num.years]) %>% 
                 select(Zone, FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, PAMean, PAErr, 
                        SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr)) 
  } else {
    data.frame(SettlementName=c("Control Settlements",as.character(MPA.name$MPAName)),
               MPA.Level.Means %>% filter(MonitoringYear==levels(MonitoringYear)[num.years]) %>% 
                 select(FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, PAMean, PAErr, 
                        SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr))
  } 
    
    
MPA.level.ContData.annex <- 
  if(MPA.name$MPAID==21) {
    data.frame(MonitoringYear=MPA.Level.Means.byZone$MonitoringYear,
               SettlementID=0,
               SettlementName=paste(MPA.Level.Means.byZone$Zone,"Settlements",sep=" "),
               MPA.Level.Means.byZone %>% 
                 select(FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, PAMean, PAErr, 
                        SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr)) %>%
      mutate(SettlementName=factor(SettlementName,
                                   levels=unique(SettlementName),
                                   ordered=T),
             MonitoringYear=factor(MonitoringYear,
                                   levels=unique(MonitoringYear),
                                   ordered=T)) %>%
      .[order(.$SettlementName,.$MonitoringYear),]
  } else {
    data.frame(MonitoringYear=MPA.Level.Means$MonitoringYear,
               SettlementID=0,
               SettlementName=rep(c("Control Settlements",as.character(MPA.name$MPAName)),num.years),
               MPA.Level.Means %>% 
                 select(FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, PAMean, PAErr, 
                        SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr)) %>%
      mutate(SettlementName=factor(SettlementName,
                                   levels=unique(SettlementName),
                                   ordered=T),
             MonitoringYear=factor(MonitoringYear,
                                   levels=unique(MonitoringYear),
                                   ordered=T)) %>%
      .[order(.$SettlementName,.$MonitoringYear),]
  }

# ---- 1.5 Define null rows to be added to plotting data frames for formatting purposes ----

null.row.PropData <- 
  data.frame(matrix(rep(NA,length(colnames(MPA.level.PropData.status))),
                    ncol=length(colnames(MPA.level.PropData.status)),
                    dimnames=list(NULL,colnames(MPA.level.PropData.status))))

null.row.ContData <- 
  cbind.data.frame(matrix(rep(NA,length(colnames(MPA.level.ContData.status))),
                          ncol=length(colnames(MPA.level.ContData.status)),
                          dimnames=list(NULL,colnames(MPA.level.ContData.status))))

null.row.ContData.annex <-
  cbind.data.frame(matrix(rep(NA,length(colnames(MPA.level.ContData.annex))),
                          ncol=length(colnames(MPA.level.ContData.annex)),
                          dimnames=list(NULL,colnames(MPA.level.ContData.annex))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Datasets for Status, Trend, and Annex Plots and for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Status dataset for settlements, proportional data ----

Sett.level.PropData.status <- 
  if(MPA.name$MPAID==21) {
    Sett.Level.Means.byZone %>%
      filter(MonitoringYear==levels(MonitoringYear)[num.years]) %>%
      ungroup() %>%
      select(SettlementName, Zone, HHH.female, HHH.male, 
             Percent.Rel.Christian, Percent.Rel.Muslim, Percent.Rel.Other, Percent.Rel.Buddhist,
             Percent.PrimaryOcc.Fish, Percent.PrimaryOcc.Farm, Percent.PrimaryOcc.WageLabor, 
             Percent.PrimaryOcc.HarvestForest, Percent.PrimaryOcc.Tourism, 
             Percent.PrimaryOcc.Aquaculture, Percent.PrimaryOcc.Extraction,
             Percent.PrimaryOcc.Other, Prop.Fish.AlmostNever, Prop.Fish.FewTimesPer6Mo, 
             Prop.Fish.FewTimesPerMo, Prop.Fish.FewTimesPerWk, Prop.Fish.MoreFewTimesWk, 
             Prop.SellFish.AlmostNever, Prop.SellFish.FewTimesPer6Mo, 
             Prop.SellFish.FewTimesPerMo, Prop.SellFish.FewTimesPerWk, 
             Prop.SellFish.MoreFewTimesWk, Prop.IncFish.None, Prop.IncFish.Some,
             Prop.IncFish.Half, Prop.IncFish.Most, Prop.IncFish.All, 
             Prop.FishTech.ByHand, Prop.FishTech.StatNet, Prop.FishTech.MobileNet, 
             Prop.FishTech.StatLine, Prop.FishTech.MobileLine, Child.FS.no, 
             Child.FS.yes, ProteinFish.None, ProteinFish.Some, 
             ProteinFish.Half, ProteinFish.Most, ProteinFish.All,Percent.FoodInsecure.NoHunger,
             Percent.FoodInsecure.YesHunger,Percent.FoodSecure, Percent.SecondaryOcc.Fish, 
             Percent.SecondaryOcc.Farm, Percent.SecondaryOcc.WageLabor, Percent.SecondaryOcc.HarvestForest,
             Percent.SecondaryOcc.Tourism, Percent.SecondaryOcc.Aquaculture, Percent.SecondaryOcc.Extraction,
             Percent.SecondaryOcc.Other, Percent.OneOcc.Diverse, Percent.MultipleOcc.Diverse, Econ.Status.Much.Worse,
             Econ.Status.Slighly.Worse, Econ.Status.Neutral, Econ.Status.Slightly.Better, Econ.Status.Much.Better,
             Threat.None, Threat.One, Threat.Two, Threat.Three, Threat.Four, Threat.Minimum.Five,
             MarineMember.No, MarineMember.Yes, MarineMeeting.No, MarineMeeting.Yes, MarineContribution, Percent.GreatlyIncreased.SocConflict,
             Percent.Increased.SocConflict, Percent.Same.SocConflict, Percent.Decreased.SocConflict, 
             Percent.GreatlyDecreased.SocConflict) %>%
      .[rev(order(.$Zone,.$SettlementName)),] %>% 
      .[order(.$Zone),]
  } else {
    Sett.Level.Means %>%
      filter(Treatment==1 & MonitoringYear==levels(MonitoringYear)[num.years]) %>%
      ungroup() %>%
      select(SettlementName, HHH.female, HHH.male, 
             Percent.Rel.Christian, Percent.Rel.Muslim, Percent.Rel.Other, 
             Percent.PrimaryOcc.Fish, Percent.PrimaryOcc.Farm, Percent.PrimaryOcc.WageLabor, 
             Percent.PrimaryOcc.HarvestForest, Percent.PrimaryOcc.Tourism, 
             Percent.PrimaryOcc.Aquaculture, Percent.PrimaryOcc.Extraction,
             Percent.PrimaryOcc.Other, Prop.Fish.AlmostNever, Prop.Fish.FewTimesPer6Mo, 
             Prop.Fish.FewTimesPerMo, Prop.Fish.FewTimesPerWk, Prop.Fish.MoreFewTimesWk, 
             Prop.SellFish.AlmostNever, Prop.SellFish.FewTimesPer6Mo, 
             Prop.SellFish.FewTimesPerMo, Prop.SellFish.FewTimesPerWk, 
             Prop.SellFish.MoreFewTimesWk, Prop.IncFish.None, Prop.IncFish.Some,
             Prop.IncFish.Half, Prop.IncFish.Most, Prop.IncFish.All, 
             Prop.FishTech.ByHand, Prop.FishTech.StatNet, Prop.FishTech.MobileNet, 
             Prop.FishTech.StatLine, Prop.FishTech.MobileLine, Child.FS.no, 
             Child.FS.yes, ProteinFish.None, ProteinFish.Some, 
             ProteinFish.Half, ProteinFish.Most, ProteinFish.All,Percent.FoodInsecure.NoHunger,
             Percent.FoodInsecure.YesHunger,Percent.FoodSecure, Percent.SecondaryOcc.Fish, 
             Percent.SecondaryOcc.Farm, Percent.SecondaryOcc.WageLabor, Percent.SecondaryOcc.HarvestForest,
             Percent.SecondaryOcc.Tourism, Percent.SecondaryOcc.Aquaculture, Percent.SecondaryOcc.Extraction,
             Percent.SecondaryOcc.Other, Percent.OneOcc.Diverse, Percent.MultipleOcc.Diverse, Econ.Status.Much.Worse,
             Econ.Status.Slighly.Worse, Econ.Status.Neutral, Econ.Status.Slightly.Better, Econ.Status.Much.Better,
             Threat.None, Threat.One, Threat.Two, Threat.Three, Threat.Four, Threat.Minimum.Five,
             MarineMember.No, MarineMember.Yes, MarineMeeting.No, MarineMeeting.Yes, MarineContribution, Percent.GreatlyIncreased.SocConflict,
             Percent.Increased.SocConflict, Percent.Same.SocConflict, Percent.Decreased.SocConflict, 
             Percent.GreatlyDecreased.SocConflict) %>%
      .[rev(order(.$SettlementName)),]
  }


if(MPA.name$MPAID==21) {
  Sett.level.PropData.status <- rbind.data.frame(Sett.level.PropData.status %>% filter(Zone=="Use"),
                                                 data.frame(SettlementName=" ",
                                                            null.row.PropData[-1]),
                                                 Sett.level.PropData.status %>% filter(Zone=="No Take"))
}

# - PLOT FORMAT DATA FRAME
Sett.level.PropData.status.PLOTFORMAT <- 
  rbind.data.frame(MPA.level.PropData.status,
                   null.row.PropData,
                   Sett.level.PropData.status) %>%
  mutate(SettlementName=ifelse(is.na(SettlementName), 
                               "", 
                               as.character(SettlementName)),
         SettlementName=factor(SettlementName,levels=unique(SettlementName),ordered=T),
         SettLevel=ifelse(SettlementName=="","Dummy","NotDummy"),
         SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]], 
                                      ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]],
                                             ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
                                                    ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name$MPAName.bahasa,as.character(SettlementName))))),
         SettlementName.bahasa=factor(SettlementName.bahasa,levels=unique(SettlementName.bahasa),ordered=T))


# ---- 2.2 Status dataset for settlements, continuous data (with p values) ----

Sett.level.ContData.status  <- 
  if(MPA.name$MPAID==21) {
    Sett.Level.Means.byZone %>%
      filter(MonitoringYear==levels(MonitoringYear)[num.years]) %>%
      ungroup() %>%
      select(SettlementName, Zone, FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, PAMean, PAErr, 
             SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr) %>%
      .[rev(order(.$Zone,.$SettlementName)),] %>%
      .[order(.$Zone),]
  } else {
    Sett.Level.Means %>%
      filter(Treatment==1 & MonitoringYear==levels(MonitoringYear)[num.years]) %>%
      ungroup() %>%
      select(SettlementName, FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, PAMean, PAErr, 
             SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr) %>%
      .[rev(order(.$SettlementName)),]
  }


if(MPA.name$MPAID==21) {
  Sett.level.ContData.status <- rbind.data.frame(Sett.level.ContData.status%>%filter(Zone=="Use"),
                                                 data.frame(SettlementName=" ",
                                                            null.row.ContData[-1]),
                                                 Sett.level.ContData.status%>%filter(Zone=="No Take"))
}

# - PLOT FORMAT DATA FRAME
Sett.level.ContData.status.PLOTFORMAT <- 
  rbind.data.frame(MPA.level.ContData.status,
                   null.row.ContData,
                   Sett.level.ContData.status) %>%
  left_join(sigvals,by="SettlementName") %>%
  mutate(SettlementName=ifelse(is.na(SettlementName),"",SettlementName),
         SettlementName=factor(SettlementName,levels=unique(SettlementName),ordered=T),
         SettLevel=ifelse(SettlementName=="","Dummy","NotDummy"),
         SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]], 
                                      ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]],
                                             ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
                                                    ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name$MPAName.bahasa,as.character(SettlementName))))),
         SettlementName.bahasa=factor(SettlementName.bahasa,levels=unique(SettlementName.bahasa),ordered=T))



# ---- 2.3 Trend dataset for MPA-level proportional data ----

MPA.level.PropData.trend.PLOTFORMAT <- 
  if(MPA.name$MPAID==21) {
    rbind.data.frame(data.frame(MPAName=paste(MPA.Level.Means.byZone$Zone,"Settlements",sep=" "),
                                MPA.Level.Means.byZone %>% 
                                  select(MonitoringYear, HHH.female, HHH.male, 
                                         Percent.Rel.Christian, Percent.Rel.Muslim, Percent.Rel.Other, 
                                         Percent.PrimaryOcc.Fish, Percent.PrimaryOcc.Farm, Percent.PrimaryOcc.WageLabor, 
                                         Percent.PrimaryOcc.HarvestForest, Percent.PrimaryOcc.Tourism, 
                                         Percent.PrimaryOcc.Aquaculture, Percent.PrimaryOcc.Extraction,
                                         Percent.PrimaryOcc.Other, Prop.Fish.AlmostNever, Prop.Fish.FewTimesPer6Mo, 
                                         Prop.Fish.FewTimesPerMo, Prop.Fish.FewTimesPerWk, Prop.Fish.MoreFewTimesWk, 
                                         Prop.SellFish.AlmostNever, Prop.SellFish.FewTimesPer6Mo, 
                                         Prop.SellFish.FewTimesPerMo, Prop.SellFish.FewTimesPerWk, 
                                         Prop.SellFish.MoreFewTimesWk, Prop.IncFish.None, Prop.IncFish.Some,
                                         Prop.IncFish.Half, Prop.IncFish.Most, Prop.IncFish.All, 
                                         Prop.FishTech.ByHand, Prop.FishTech.StatNet, Prop.FishTech.MobileNet, 
                                         Prop.FishTech.StatLine, Prop.FishTech.MobileLine, Child.FS.no, 
                                         Child.FS.yes, ProteinFish.None, ProteinFish.Some, 
                                         ProteinFish.Half, ProteinFish.Most, ProteinFish.All,Percent.FoodInsecure.NoHunger,
                                         Percent.FoodInsecure.YesHunger,Percent.FoodSecure, Percent.SecondaryOcc.Fish, 
                                         Percent.SecondaryOcc.Farm, Percent.SecondaryOcc.WageLabor, Percent.SecondaryOcc.HarvestForest,
                                         Percent.SecondaryOcc.Tourism, Percent.SecondaryOcc.Aquaculture, Percent.SecondaryOcc.Extraction,
                                         Percent.SecondaryOcc.Other, Percent.OneOcc.Diverse, Percent.MultipleOcc.Diverse, Econ.Status.Much.Worse,
                                         Econ.Status.Slighly.Worse, Econ.Status.Neutral, Econ.Status.Slightly.Better, Econ.Status.Much.Better,
                                         Threat.None, Threat.One, Threat.Two, Threat.Three, Threat.Four, Threat.Minimum.Five,
                                         MarineMember.No, MarineMember.Yes, MarineMeeting.No, MarineMeeting.Yes, MarineContribution, Percent.GreatlyIncreased.SocConflict,
                                         Percent.Increased.SocConflict, Percent.Same.SocConflict, Percent.Decreased.SocConflict, 
                                         Percent.GreatlyDecreased.SocConflict)),
                     data.frame(MPAName=NA,MonitoringYear=NA,null.row.PropData%>%select(-SettlementName))) %>%
      left_join(.,define.year.monitoryear.column(MPA.Level.Means.byZone),by="MonitoringYear") %>%
      left_join(.,define.year.monitoryear.column.bahasa(MPA.Level.Means.byZone),by="MonitoringYear")
  } else {
  rbind.data.frame(data.frame(MPAName=rep(c("Control Settlements",as.character(MPA.name$MPAName)), num.years),
                              MPA.Level.Means %>% 
                                select(MonitoringYear, HHH.female, HHH.male, 
                                       Percent.Rel.Christian, Percent.Rel.Muslim, Percent.Rel.Other, 
                                       Percent.PrimaryOcc.Fish, Percent.PrimaryOcc.Farm, Percent.PrimaryOcc.WageLabor, 
                                       Percent.PrimaryOcc.HarvestForest, Percent.PrimaryOcc.Tourism, 
                                       Percent.PrimaryOcc.Aquaculture, Percent.PrimaryOcc.Extraction,
                                       Percent.PrimaryOcc.Other, Prop.Fish.AlmostNever, Prop.Fish.FewTimesPer6Mo, 
                                       Prop.Fish.FewTimesPerMo, Prop.Fish.FewTimesPerWk, Prop.Fish.MoreFewTimesWk, 
                                       Prop.SellFish.AlmostNever, Prop.SellFish.FewTimesPer6Mo, 
                                       Prop.SellFish.FewTimesPerMo, Prop.SellFish.FewTimesPerWk, 
                                       Prop.SellFish.MoreFewTimesWk, Prop.IncFish.None, Prop.IncFish.Some,
                                       Prop.IncFish.Half, Prop.IncFish.Most, Prop.IncFish.All, 
                                       Prop.FishTech.ByHand, Prop.FishTech.StatNet, Prop.FishTech.MobileNet, 
                                       Prop.FishTech.StatLine, Prop.FishTech.MobileLine, Child.FS.no, 
                                       Child.FS.yes, ProteinFish.None, ProteinFish.Some, 
                                       ProteinFish.Half, ProteinFish.Most, ProteinFish.All,Percent.FoodInsecure.NoHunger,
                                       Percent.FoodInsecure.YesHunger,Percent.FoodSecure, Percent.SecondaryOcc.Fish, 
                                       Percent.SecondaryOcc.Farm, Percent.SecondaryOcc.WageLabor, Percent.SecondaryOcc.HarvestForest,
                                       Percent.SecondaryOcc.Tourism, Percent.SecondaryOcc.Aquaculture, Percent.SecondaryOcc.Extraction,
                                       Percent.SecondaryOcc.Other, Percent.OneOcc.Diverse, Percent.MultipleOcc.Diverse, Econ.Status.Much.Worse,
                                       Econ.Status.Slighly.Worse, Econ.Status.Neutral, Econ.Status.Slightly.Better, Econ.Status.Much.Better,
                                       Threat.None, Threat.One, Threat.Two, Threat.Three, Threat.Four, Threat.Minimum.Five,
                                       MarineMember.No, MarineMember.Yes, MarineMeeting.No, MarineMeeting.Yes, MarineContribution, Percent.GreatlyIncreased.SocConflict,
                                       Percent.Increased.SocConflict, Percent.Same.SocConflict, Percent.Decreased.SocConflict, 
                                       Percent.GreatlyDecreased.SocConflict)),
                   data.frame(MPAName=NA,MonitoringYear=NA,null.row.PropData%>%select(-SettlementName))) %>%
  left_join(.,define.year.monitoryear.column(MPA.Level.Means),by="MonitoringYear") %>%
  left_join(.,define.year.monitoryear.column.bahasa(MPA.Level.Means),by="MonitoringYear") 
  } %>% 
  mutate(
    order=if(num.years==2) { c(1,4,2,5,3) 
    } else { if(num.years==3) { c(1,5,2,6,3,7,4) 
    } else { if(num.years==4) { c(1,6,2,7,3,8,4,9,5) 
    } else { if(num.years==5) { c(1,7,2,8,3,9,4,10,5,11,6) 
    } else{ NA }}}},
    Label=ifelse(is.na(Label),"",as.character(Label)),
    Label.bahasa=ifelse(is.na(Label.bahasa),"",as.character(Label.bahasa)))


# ---- 2.4 Trend dataset for MPA-level continuous data (with p values) ----

MPA.level.ContData.trend.PLOTFORMAT <- 
  if(MPA.name$MPAID==21) {
    rbind.data.frame(MPA.level.ContData.annex %>% select(-SettlementID),
                     trend.sigvals) %>%
      left_join(.,define.year.monitoryear.column(MPA.Level.Means.byZone),by="MonitoringYear") %>%
      left_join(.,define.year.monitoryear.column.bahasa(MPA.Level.Means.byZone),by="MonitoringYear") 
  } else {
    rbind.data.frame(MPA.level.ContData.annex %>% select(-SettlementID),
                     trend.sigvals) %>%
      left_join(.,define.year.monitoryear.column(MPA.Level.Means),by="MonitoringYear") %>%
      left_join(.,define.year.monitoryear.column.bahasa(MPA.Level.Means),by="MonitoringYear") 
  } %>%
  mutate(MonitoringYear=factor(MonitoringYear,
                               levels=unique(MonitoringYear),
                               ordered=T),
         SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]], 
                                      ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]],
                                             ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
                                                    ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name$MPAName.bahasa,as.character(SettlementName))))),
         order=if(num.years==2) { c(1,2,4,5,3) 
         } else { if(num.years==3) { c(1,2,3,5,6,7,4) 
         } else { if(num.years==4) { c(1,2,3,4,6,7,8,9,5) 
         } else { if(num.years==5) { c(1,2,3,4,5,7,8,9,10,11,6) 
         } else{ NA }}}},
         Treatment=ifelse(SettlementName=="Control Settlements" | SettlementName=="Use Settlements","Control","MPA"),
         Label=ifelse(is.na(Label),"",as.character(Label)),
         Label.bahasa=ifelse(is.na(Label.bahasa),"",as.character(Label.bahasa)))



# ---- 2.5 Annex dataset for Settlement-level continuous data ----

Sett.level.ContData.annex <- 
  Sett.Level.Means %>%
  filter(Treatment==1) %>%
  ungroup() %>%
  select(MonitoringYear, SettlementID, SettlementName, FSMean, FSErr, MAMean, MAErr, MTMean, MTErr, 
         PAMean, PAErr, SEMean, SEErr, TimeMarketMean, TimeMarketErr, UnwellMean, UnwellErr) %>%
  mutate(MonitoringYear=factor(MonitoringYear,
                               levels=rev(unique(MonitoringYear)),
                               ordered=T)) %>%
  .[rev(order(.$SettlementName,.$MonitoringYear)),]

# - PLOT FORMAT DATA FRAME
Sett.level.ContData.annex.PLOTFORMAT <- 
  rbind.data.frame(MPA.level.ContData.annex,
                   null.row.ContData.annex, 
                   Sett.level.ContData.annex) %>%
  mutate(SettlementName=ifelse(is.na(SettlementName),"",as.character(SettlementName)),
         SettlementName=factor(SettlementName,levels=unique(SettlementName),ordered=T),
         SettLevel=ifelse(SettlementName=="","Dummy","NotDummy"),
         SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]], 
                                      ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]],
                                             ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
                                                    ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name$MPAName.bahasa,as.character(SettlementName))))),
         SettlementName.bahasa=factor(SettlementName.bahasa,levels=unique(SettlementName.bahasa),ordered=T))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Extra Datasets for Export ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


Detailed.FishTechnique <-
  if(MPA.name$MPAID==21) {
    rbind.data.frame(data.frame(SettlementID=NA,
                                SettlementName=c("Use Settlements","No Take Settlements"),
                                MPA.Level.Means.byZone %>%
                                  select(MPAID, MonitoringYear, InterviewYear, Zone,
                                         Primary.FishTech.Hand, Primary.FishTech.StatLine, Primary.FishTech.MobileLine, Primary.FishTech.Glean,
                                         Primary.FishTech.GrapWound, Primary.FishTech.FallGear, Primary.FishTech.StatGill, Primary.FishTech.Trammel, Primary.FishTech.MobileGill,
                                         Primary.FishTech.Trap, Primary.FishTech.Fence, Primary.FishTech.LiftNet, Primary.FishTech.MobileDredge, Primary.FishTech.Trawl, 
                                         Primary.FishTech.Seine, Primary.FishTech.BombPoison, Primary.FishTech.Other,
                                         N.Primary.FishTech.Hand, N.Primary.FishTech.StatLine, N.Primary.FishTech.MobileLine, N.Primary.FishTech.Glean,
                                         N.Primary.FishTech.GrapWound, N.Primary.FishTech.FallGear, N.Primary.FishTech.StatGill, N.Primary.FishTech.Trammel, N.Primary.FishTech.MobileGill,
                                         N.Primary.FishTech.Trap, N.Primary.FishTech.Fence, N.Primary.FishTech.LiftNet, N.Primary.FishTech.MobileDredge, N.Primary.FishTech.Trawl, 
                                         N.Primary.FishTech.Seine, N.Primary.FishTech.BombPoison, N.Primary.FishTech.Other)),
                     Sett.Level.Means.byZone %>%
                       select(SettlementID, SettlementName, MPAID, MonitoringYear, InterviewYear, Zone,
                              Primary.FishTech.Hand, Primary.FishTech.StatLine, Primary.FishTech.MobileLine, Primary.FishTech.Glean,
                              Primary.FishTech.GrapWound, Primary.FishTech.FallGear, Primary.FishTech.StatGill, Primary.FishTech.Trammel, Primary.FishTech.MobileGill,
                              Primary.FishTech.Trap, Primary.FishTech.Fence, Primary.FishTech.LiftNet, Primary.FishTech.MobileDredge, Primary.FishTech.Trawl, 
                              Primary.FishTech.Seine, Primary.FishTech.BombPoison, Primary.FishTech.Other,
                              N.Primary.FishTech.Hand, N.Primary.FishTech.StatLine, N.Primary.FishTech.MobileLine, N.Primary.FishTech.Glean,
                              N.Primary.FishTech.GrapWound, N.Primary.FishTech.FallGear, N.Primary.FishTech.StatGill, N.Primary.FishTech.Trammel, N.Primary.FishTech.MobileGill,
                              N.Primary.FishTech.Trap, N.Primary.FishTech.Fence, N.Primary.FishTech.LiftNet, N.Primary.FishTech.MobileDredge, N.Primary.FishTech.Trawl, 
                              N.Primary.FishTech.Seine, N.Primary.FishTech.BombPoison, N.Primary.FishTech.Other) %>%
                       .[rev(order(.$Zone,.$SettlementName)),] %>% 
                       .[order(.$Zone),])
  } else {
    NA
  }
