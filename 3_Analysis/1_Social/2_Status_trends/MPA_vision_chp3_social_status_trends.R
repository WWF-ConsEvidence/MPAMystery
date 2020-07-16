# 
# code: Social status and trends analysis for MPA Vision report, Chp 3
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: March 2020
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE AND WRANGLE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- source data ----

source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files.R')
source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_household_indices.R')
source('2_Functions/3_Plotting/Function_plotthemes.R')

pacman::p_load(grid, gridExtra, cowplot)


# ---- add a couple of columns, especially to remove TV from MAIndex & group aspects of MAIndex into subgroups ----

HHData <-
  HHData %>% group_by(MPAID) %>% summarise(CurrentYear=max(InterviewYear)) %>% ungroup() %>% left_join(HHData,.,by="MPAID") %>%
  mutate(MAIndex.noTV=ifelse(RemoveMA=="No",
                             rowSums(select(., "CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard",
                                            "BoatInboard", "PhoneCombined", "Entertain", "Satellite", "Generator"),
                                     na.rm = TRUE),
                             NA),
         MAIndex.boats=ifelse(RemoveMA=="No",
                              rowSums(select(.,"BoatNoMotor","BoatOutboard","BoatInboard"),
                                      na.rm=T),
                              NA),
         MAIndex.vehicles=ifelse(RemoveMA=="No",
                                 rowSums(select(.,"CarTruck","Motorcycle","Bicycle"),
                                         na.rm=T),
                                 NA),
         MAIndex.household=ifelse(RemoveMA=="No",
                                  rowSums(select(.,"Entertain","Satellite","Generator"),
                                          na.rm=T),
                                  NA),
         RepeatYear=ifelse(Seascape==1, # Repeat year provides a standard way to group MPAs post-baseline, since some MPAs have t2 vs. t3 for their first repeat, etc. 
                           ifelse(MonitoringYear=="2 Year Post","First Repeat",
                                  ifelse(MonitoringYear=="4 Year Post","Second Repeat",
                                         as.character(MonitoringYear))),
                           ifelse(MonitoringYear=="2 Year Post" | MonitoringYear=="3 Year Post","First Repeat",as.character(MonitoringYear))),
         Status=ifelse(InterviewYear==CurrentYear,"Yes","No"),
         Province=ifelse(MPAID%in%c(1,2,3,4,5,6),"West Papua",
                         ifelse(MPAID%in%c(15,16),"East Nusa Tenggara",
                                ifelse(MPAID%in%c(17,18,19),"Maluku",
                                       ifelse(MPAID%in%c(20,21),"Southeast Sulawesi",NA)))))


# ---- Calculate status and trends for province and national-level ----

# NOTE: Calculating material assets mean WITHOUT TV since it is missing from a few of the MPAs.

Province.Level.ContData.Means <- 
  HHData %>% filter(MPAID!= 19 & MPAID!=21) %>% # Remove Yamdena and Wakatobi from provincial level means. Yamdena doesn't have trend, so cannot be compared across time to other Maluku MPAs, and Wakatobi is not a true baseline.
  dplyr::group_by(Province,RepeatYear,Treatment) %>%
  dplyr::summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
                   FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
                   MAMean=round(mean(MAIndex.noTV,na.rm=T),2),
                   MAErr=round(sd(MAIndex.noTV,na.rm=T)/sqrt(length(MAIndex.noTV)),2),
                   PAMean=round(mean(PAIndex,na.rm=T),2),
                   PAErr=round(sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),2),
                   MTMean=round(mean(MTIndex,na.rm=T),2),
                   MTErr=round(sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),2),
                   SEMean=round(mean(SERate,na.rm=T),2),
                   SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2),
                   Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   MAMean.boats=round(mean(MAIndex.boats,na.rm=T),2),
                   MAMean.vehicles=round(mean(MAIndex.vehicles,na.rm=T),2),
                   MAMean.household=round(mean(MAIndex.household,na.rm=T),2))


# REMOVED all fishing characteristic responses for households that do not identify fishing as one of their 1/2/3 occupation.  To remain consistent across MPA monitoring protocols
# NOTE: keep Yamdena and Wakatobi data in the proportional means analyses, because we are not displaying trends, rather just the most recently collected data (e.g., status) from each MPA
Province.Level.PropData.Means <-
  HHData %>% filter(Status=="Yes" & Treatment==1) %>% mutate(Check.NA=paste(PrimaryLivelihood,SecondaryLivelihood,TertiaryLivelihood,sep=""),
                                                             Fisher=ifelse(grepl("3",Check.NA),1,
                                                                           ifelse(Check.NA=="NANANA",NA,0))) %>%
  dplyr::group_by(Province) %>%
  dplyr::summarise(YrResident=mean(YrResident,na.rm=T),
                   HHH.female=(length(HHHGender[HHHGender==0 &
                                                  !is.na(HHHGender)])/length(HHHGender[!is.na(HHHGender)]))*100,
                   HHH.male=(length(HHHGender[HHHGender==1 &
                                                !is.na(HHHGender)])/length(HHHGender[!is.na(HHHGender)]))*100,
                   Percent.Rel.Christian=(length(Religion[Religion==1 &
                                                            !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                   Percent.Rel.Muslim=(length(Religion[Religion==2 &
                                                         !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                   Percent.Rel.Other=(length(Religion[Religion!=1 & Religion!=2 &
                                                        !is.na(Religion)])/length(Religion[!is.na(Religion)]))*100,
                   Percent.PrimaryOcc.Fish=(length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                                       !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.Farm=(length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                                       !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                                            !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                                !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                                          !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.Aquaculture=(length(PrimaryLivelihood[PrimaryLivelihood==4 &
                                                                              !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.Extraction=(length(PrimaryLivelihood[PrimaryLivelihood==5 &
                                                                             !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Percent.PrimaryOcc.Other=(length(PrimaryLivelihood[PrimaryLivelihood==996 & !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
                   Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish) & Fisher==1])/length(FreqFish[!is.na(FreqFish) & Fisher==1]))*100,
                   Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish) & Fisher==1])/length(FreqFish[!is.na(FreqFish) & Fisher==1]))*100,
                   Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish) & Fisher==1])/length(FreqFish[!is.na(FreqFish) & Fisher==1]))*100,
                   Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish) & Fisher==1])/length(FreqFish[!is.na(FreqFish) & Fisher==1]))*100,
                   Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish) & Fisher==1])/length(FreqFish[!is.na(FreqFish) & Fisher==1]))*100,
                   Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish) & Fisher==1])/length(FreqSaleFish[!is.na(FreqSaleFish) & Fisher==1]))*100,
                   Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish) & Fisher==1])/length(FreqSaleFish[!is.na(FreqSaleFish) & Fisher==1]))*100,
                   Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish) & Fisher==1])/length(FreqSaleFish[!is.na(FreqSaleFish) & Fisher==1]))*100,
                   Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish) & Fisher==1])/length(FreqSaleFish[!is.na(FreqSaleFish) & Fisher==1]))*100,
                   Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish) & Fisher==1])/length(FreqSaleFish[!is.na(FreqSaleFish) & Fisher==1]))*100,
                   Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique) & Fisher==1])/length(MajFishTechnique[!is.na(MajFishTechnique) & Fisher==1]))*100,
                   Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique) & Fisher==1])/length(MajFishTechnique[!is.na(MajFishTechnique) & Fisher==1]))*100,
                   Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique) & Fisher==1])/length(MajFishTechnique[!is.na(MajFishTechnique) & Fisher==1]))*100,
                   Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique) & Fisher==1])/length(MajFishTechnique[!is.na(MajFishTechnique) & Fisher==1]))*100,
                   Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique) & Fisher==1])/length(MajFishTechnique[!is.na(MajFishTechnique) & Fisher==1]))*100,
                   Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                                     !is.na(FreqEatFish) & Fisher==1])/length(FreqEatFish[!is.na(FreqEatFish) & Fisher==1]))*100,
                   Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                        !is.na(FreqEatFish) & Fisher==1])/length(FreqEatFish[!is.na(FreqEatFish) & Fisher==1]))*100,
                   Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                       !is.na(FreqEatFish) & Fisher==1])/length(FreqEatFish[!is.na(FreqEatFish) & Fisher==1]))*100,
                   Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                       !is.na(FreqEatFish) & Fisher==1])/length(FreqEatFish[!is.na(FreqEatFish) & Fisher==1]))*100,
                   Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                        !is.na(FreqEatFish) & Fisher==1])/length(FreqEatFish[!is.na(FreqEatFish) & Fisher==1]))*100,
                   ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                                 !is.na(PercentProteinFish) & Fisher==1])/length(PercentProteinFish[!is.na(PercentProteinFish) & Fisher==1]))*100,
                   ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                                 !is.na(PercentProteinFish) & Fisher==1])/length(PercentProteinFish[!is.na(PercentProteinFish) & Fisher==1]))*100,
                   ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                                 !is.na(PercentProteinFish) & Fisher==1])/length(PercentProteinFish[!is.na(PercentProteinFish) & Fisher==1]))*100,
                   ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                                 !is.na(PercentProteinFish) & Fisher==1])/length(PercentProteinFish[!is.na(PercentProteinFish) & Fisher==1]))*100,
                   ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                                !is.na(PercentProteinFish) & Fisher==1])/length(PercentProteinFish[!is.na(PercentProteinFish) & Fisher==1]))*100,
                   UnwellMean=mean(DaysUnwell,na.rm=T),
                   UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)),
                   TimeMarketMean=mean(TimeMarket,na.rm=T),
                   TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)),
                   MTManage=mean(RightsManage,na.rm=T),
                   MTHarvest=mean(RightsHarvest,na.rm=T),
                   MTAccess=mean(RightsAccess,na.rm=T),
                   MTTransfer=mean(RightsTransfer,na.rm=T),
                   MTExclude=mean(RightsExclude,na.rm=T),
                   MatAssets.gini=gini(MAIndex.noTV),
                   Percent.SecondaryOcc.Fish=(length(SecondaryLivelihood[SecondaryLivelihood==3 &
                                                                           !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.Farm=(length(SecondaryLivelihood[SecondaryLivelihood==1 &
                                                                           !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.WageLabor=(length(SecondaryLivelihood[SecondaryLivelihood==7 &
                                                                                !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.HarvestForest=(length(SecondaryLivelihood[SecondaryLivelihood==2 &
                                                                                    !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.Tourism=(length(SecondaryLivelihood[SecondaryLivelihood==6 &
                                                                              !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.Aquaculture=(length(SecondaryLivelihood[SecondaryLivelihood==4 &
                                                                                  !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.Extraction=(length(SecondaryLivelihood[SecondaryLivelihood==5 &
                                                                                 !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.SecondaryOcc.Other=(length(SecondaryLivelihood[SecondaryLivelihood==996 & !is.na(SecondaryLivelihood)])/length(SecondaryLivelihood[!is.na(SecondaryLivelihood)]))*100,
                   Percent.OneOcc.Diverse=(length(HouseholdID[!is.na(PrimaryLivelihood) & is.na(SecondaryLivelihood) & is.na(TertiaryLivelihood)])/length(HouseholdID[!is.na(PrimaryLivelihood)]))*100,
                   Percent.MultipleOcc.Diverse=(length(HouseholdID[!is.na(PrimaryLivelihood) & (!is.na(SecondaryLivelihood) | !is.na(TertiaryLivelihood))])/length(HouseholdID[!is.na(PrimaryLivelihood)]))*100,
                   Econ.Status.Much.Worse=(length(EconStatusTrend[EconStatusTrend==1 & !is.na(EconStatusTrend)])/length(EconStatusTrend[!is.na(EconStatusTrend)]))*100,
                   Econ.Status.Slighly.Worse=(length(EconStatusTrend[EconStatusTrend==2 & !is.na(EconStatusTrend)])/length(EconStatusTrend[!is.na(EconStatusTrend)]))*100,
                   Econ.Status.Neutral=(length(EconStatusTrend[EconStatusTrend==3 & !is.na(EconStatusTrend)])/length(EconStatusTrend[!is.na(EconStatusTrend)]))*100,
                   Econ.Status.Slightly.Better=(length(EconStatusTrend[EconStatusTrend==4 & !is.na(EconStatusTrend)])/length(EconStatusTrend[!is.na(EconStatusTrend)]))*100,
                   Econ.Status.Much.Better=(length(EconStatusTrend[EconStatusTrend==5 & !is.na(EconStatusTrend)])/length(EconStatusTrend[!is.na(EconStatusTrend)]))*100,
                   Threat.Mean=mean(NumLocalThreat, na.rm = T),
                   Threat.None=(length(NumLocalThreat[NumLocalThreat==0 & !is.na(NumLocalThreat)])/length(NumLocalThreat[!is.na(NumLocalThreat)]))*100,
                   Threat.One=(length(NumLocalThreat[NumLocalThreat==1 & !is.na(NumLocalThreat)])/length(NumLocalThreat[!is.na(NumLocalThreat)]))*100,
                   Threat.Two=(length(NumLocalThreat[NumLocalThreat==2 & !is.na(NumLocalThreat)])/length(NumLocalThreat[!is.na(NumLocalThreat)]))*100,
                   Threat.Three=(length(NumLocalThreat[NumLocalThreat==3 & !is.na(NumLocalThreat)])/length(NumLocalThreat[!is.na(NumLocalThreat)]))*100,
                   Threat.Four=(length(NumLocalThreat[NumLocalThreat==4 & !is.na(NumLocalThreat)])/length(NumLocalThreat[!is.na(NumLocalThreat)]))*100,
                   Threat.Minimum.Five =(length(NumLocalThreat[NumLocalThreat>=5 & !is.na(NumLocalThreat)])/length(NumLocalThreat[!is.na(NumLocalThreat)]))*100,
                   MarineMember.No=(length(MarineGroup[MarineGroup==0 & !is.na(MarineGroup)])/
                                      length(MarineGroup[!is.na(MarineGroup)]))*100,
                   MarineMember.Yes=(length(MarineGroup[MarineGroup==1 &
                                                          !is.na(MarineGroup)])/
                                       length(MarineGroup[!is.na(MarineGroup)]))*100,
                   Num.MarineMember.Yes=length(MarineGroup[MarineGroup>0 & !is.na(MarineGroup)]),
                   Num.MarineMember.No=length(MarineGroup[MarineGroup==0 & !is.na(MarineGroup)]),
                   MarineMeeting.No=(length(MarineMeetingSum[MarineMeetingSum==0 &
                                                               !is.na(MarineMeetingSum)])/
                                       length(MarineMeetingSum[!is.na(MarineMeetingSum)]))*100,
                   MarineMeeting.Yes=(length(MarineMeetingSum[MarineMeetingSum>0 &
                                                                !is.na(MarineMeetingSum)])/
                                        length(MarineMeetingSum[!is.na(MarineMeetingSum)]))*100,
                   Num.MarineMeeting.Yes=length(MarineMeetingSum[MarineMeetingSum>0 & !is.na(MarineMeetingSum)]),
                   Num.MarineMeeting.No=length(MarineMeetingSum[MarineMeetingSum==0 & !is.na(MarineMeetingSum)]),
                   MarineContribution=mean(MarineContribution, na.rm = T),
                   Percent.GreatlyIncreased.SocConflict=(length(SocialConflict[(SocialConflict==1) &
                                                                                 !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                   Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==2) &
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                   Percent.Same.SocConflict=(length(SocialConflict[(SocialConflict==3) &
                                                                     !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                   Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4) &
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                   Percent.GreatlyDecreased.SocConflict=(length(SocialConflict[SocialConflict==5 &
                                                                                 !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: STATISTICAL MONOTONIC TREND TESTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- province level statistical tests (monotonic trend) ----

PapuaBarat.trend.test  <- 
  cbind.data.frame(Province="West Papua",
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Province=="West Papua" & HHData$Treatment==1 &
                                                                HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                                       HHData[HHData$Province=="West Papua" & HHData$Treatment==1 &
                                                                HHData$RepeatYear==unique(HHData$RepeatYear)[2],a], 
                                                       HHData[HHData$Province=="West Papua" & HHData$Treatment==1 &
                                                                HHData$RepeatYear==unique(HHData$RepeatYear)[3],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 

NusaTenggara.trend.test  <- 
  cbind.data.frame(Province="East Nusa Tenggara",
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Province=="East Nusa Tenggara" & HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                            HHData[HHData$Province=="East Nusa Tenggara" & HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[2],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 

Maluku.trend.test <-
  cbind.data.frame(Province="Maluku",
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Province=="Maluku" & HHData$Treatment==1 & HHData$MPAID!=19 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                            HHData[HHData$Province=="Maluku" & HHData$Treatment==1 & HHData$MPAID!=19 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[2],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 


trend.pvals.byProvince <-
  rbind.data.frame(PapuaBarat.trend.test,
                   NusaTenggara.trend.test,
                   Maluku.trend.test,
                   data.frame(Province="Southeast Sulawesi",
                              FS.trend.pval=1,
                              MA.trend.pval=1,
                              MT.trend.pval=1,
                              PA.trend.pval=1,
                              SE.trend.pval=1))

Plotting.Province.ContData <-
  Province.Level.ContData.Means %>% filter(Treatment==1) %>% ungroup() %>%
  rbind.data.frame(data.frame(Province=c("Maluku","East Nusa Tenggara","Southeast Sulawesi","Southeast Sulawesi"),
                              RepeatYear=c("Second Repeat","Second Repeat","First Repeat","Second Repeat"),
                              Treatment=rep(1,4),
                              matrix(rep(NA,length(colnames(.))-3),
                                     ncol=length(colnames(.))-3,
                                     dimnames=list(NULL,colnames(.)[4:length(colnames(.))])))) %>% 
  left_join(trend.pvals.byProvince,by="Province") %>%
  mutate(Asterisk.FS=ifelse(FS.trend.pval<0.01,"***",
                            ifelse(FS.trend.pval<0.05 & FS.trend.pval>=0.01,"**",
                                   ifelse(FS.trend.pval<0.1 & FS.trend.pval>=0.05,"*",""))),
         Asterisk.MA=ifelse(MA.trend.pval<0.01,"***",
                            ifelse(MA.trend.pval<0.05 & MA.trend.pval>=0.01,"**",
                                   ifelse(MA.trend.pval<0.1 & MA.trend.pval>=0.05,"*",""))),
         Asterisk.MT=ifelse(MT.trend.pval<0.01,"***",
                            ifelse(MT.trend.pval<0.05 & MT.trend.pval>=0.01,"**",
                                   ifelse(MT.trend.pval<0.1 & MT.trend.pval>=0.05,"*",""))),
         Asterisk.PA=ifelse(PA.trend.pval<0.01,"***",
                            ifelse(PA.trend.pval<0.05 & PA.trend.pval>=0.01,"**",
                                   ifelse(PA.trend.pval<0.1 & PA.trend.pval>=0.05,"*",""))),
         Asterisk.SE=ifelse(Province=="East Nusa Tenggara","", # East Nusa Tenggara's school enrollment rate is really variable between MPAs, causing monotonic trend results to be funky.  Removing asterisks.
                            ifelse(SE.trend.pval<0.01,"***",
                                   ifelse(SE.trend.pval<0.05 & SE.trend.pval>=0.01,"**",
                                          ifelse(SE.trend.pval<0.1 & SE.trend.pval>=0.05,"*","")))),
         ProvinceName.FS=ifelse(Asterisk.FS=="",as.character(Province),paste(Province,Asterisk.FS,sep=" ")),
         ProvinceName.MA=ifelse(Asterisk.MA=="",as.character(Province),paste(Province,Asterisk.MA,sep=" ")),
         ProvinceName.MT=ifelse(Asterisk.MT=="",as.character(Province),paste(Province,Asterisk.MT,sep=" ")),
         ProvinceName.PA=ifelse(Asterisk.PA=="",as.character(Province),paste(Province,Asterisk.PA,sep=" ")),
         ProvinceName.SE=ifelse(Asterisk.SE=="",as.character(Province),paste(Province,Asterisk.SE,sep=" ")),
         Province=factor(Province,levels=c("Southeast Sulawesi","East Nusa Tenggara","Maluku","West Papua"),ordered=T))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PLOTTING FOR PROVINCE LEVEL DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


plot.theme.1 <- theme(axis.ticks=element_blank(),
                      panel.background=element_rect(fill="white",
                                                    colour="#909090"),
                      panel.border=element_rect(fill=NA,
                                                size=0.25,
                                                colour="#C0C0C0"),
                      panel.grid.major.x=element_line(colour="#C0C0C0",
                                                      size=0.25,
                                                      linetype=3),
                      panel.grid.major.y=element_blank(),
                      plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                      axis.title=element_text(size=13,
                                              angle=0,
                                              face="bold",
                                              colour="#303030",
                                              hjust=0.5),
                      axis.text.y=element_text(size=rel(0.9),
                                               angle=0,
                                               colour="#303030",
                                               lineheight=0.7),
                      axis.text.x=element_text(size=11,
                                               angle=55,
                                               colour="#303030",
                                               lineheight=0.9,
                                               hjust=1),
                      legend.position="top",
                      legend.justification="right",
                      legend.box.spacing=unit(0.1,"cm"))

plot.guide <- guides(alpha=guide_legend(ncol=3))


# --- Material assets

MA.province <- 
  Plotting.Province.ContData %>%
  ggplot(aes(x=Province,y=MAMean)) +
  geom_bar(aes(group=RepeatYear, alpha=RepeatYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,ymax=MAMean+MAErr,group=RepeatYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=1.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=2.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=3.5),linetype=2,size=0.25,colour="#505050") +
  # geom_vline(aes(xintercept=4.5),linetype=1,size=0.5,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,30),
                     breaks=seq(0,30,by=5)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","First Repeat","Second Repeat"),
                     values=c("Baseline"=0.3,"First Repeat"=0.6,"Second Repeat"=0.9)) +
  scale_x_discrete(labels=unique(Plotting.Province.ContData$ProvinceName.MA[order(Plotting.Province.ContData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household material assets")


# --- Food security

FS.province <- 
  rbind.data.frame(Plotting.Province.ContData[,c("FSMean","FSErr","RepeatYear","Province","ProvinceName.FS")],
                   data.frame(FSMean=0,
                              FSErr=NA,
                              RepeatYear="Baseline",
                              Province=" ",
                              ProvinceName.FS=" ")) %>%
  ungroup() %>%
  mutate(Province=factor(Province,levels=c("Southeast Sulawesi","East Nusa Tenggara","Maluku","West Papua"," "),ordered=T)) %>%
  ggplot(aes(x=Province,y=FSMean)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#909090") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#909090") +
  geom_bar(aes(group=RepeatYear, alpha=RepeatYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=FSMean-FSErr,ymax=FSMean+FSErr,group=RepeatYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=1.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=2.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=3.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=4.5),linetype=2,size=0.25,colour="#505050") +
  geom_text(aes(x=5,y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=5,y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=5,y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","First Repeat","Second Repeat"),
                     values=c("Baseline"=0.3,"First Repeat"=0.6,"Second Repeat"=0.9)) +
  scale_x_discrete(labels=c(unique(Plotting.Province.ContData$ProvinceName.FS[order(Plotting.Province.ContData$Province)])," ")) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household food security")


# --- Marine tenure

MT.province <- 
  Plotting.Province.ContData %>%
  ggplot(aes(x=Province,y=MTMean)) +
  geom_bar(aes(group=RepeatYear, alpha=RepeatYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,ymax=MTMean+MTErr,group=RepeatYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=1.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=2.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=3.5),linetype=2,size=0.25,colour="#505050") +
  # geom_vline(aes(xintercept=4.5),linetype=1,size=0.5,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","First Repeat","Second Repeat"),
                     values=c("Baseline"=0.3,"First Repeat"=0.6,"Second Repeat"=0.9)) +
  scale_x_discrete(labels=unique(Plotting.Province.ContData$ProvinceName.MT[order(Plotting.Province.ContData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household marine tenure")


# --- Place attachment

PA.province <- 
  Plotting.Province.ContData %>%
  ggplot(aes(x=Province,y=PAMean)) +
  geom_bar(aes(group=RepeatYear, alpha=RepeatYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=PAMean-PAErr,ymax=PAMean+PAErr,group=RepeatYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=1.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=2.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=3.5),linetype=2,size=0.25,colour="#505050") +
  # geom_vline(aes(xintercept=4.5),linetype=1,size=0.5,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5.2),
                     breaks=seq(0,5,by=0.5)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","First Repeat","Second Repeat"),
                     values=c("Baseline"=0.3,"First Repeat"=0.6,"Second Repeat"=0.9)) +
  scale_x_discrete(labels=unique(Plotting.Province.ContData$ProvinceName.PA[order(Plotting.Province.ContData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Place attachment")


# --- School enrollment

SE.province <- 
  Plotting.Province.ContData %>%
  ggplot(aes(x=Province,y=SEMean)) +
  geom_bar(aes(group=RepeatYear, alpha=RepeatYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=SEMean-SEErr,ymax=SEMean+SEErr,group=RepeatYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=1.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=2.5),linetype=2,size=0.25,colour="#505050") +
  geom_vline(aes(xintercept=3.5),linetype=2,size=0.25,colour="#505050") +
  # geom_vline(aes(xintercept=4.5),linetype=1,size=0.5,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     breaks=seq(0,1,by=0.2),
                     labels=scales::percent_format()) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","First Repeat","Second Repeat"),
                     values=c("Baseline"=0.3,"First Repeat"=0.6,"Second Repeat"=0.9)) +
  scale_x_discrete(labels=unique(Plotting.Province.ContData$ProvinceName.SE[order(Plotting.Province.ContData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="School enrollment rate")


# ---- export province level big five plots ----

FigureFileName.chp3 <- 'x_Flat_data_files/1_Social/Outputs/MPA_Vision_Chp3/Trends/Analyzed_20200327'

# ---- Material assets ----

png(paste(FigureFileName.chp3,"MA.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(MA.province)
dev.off()

# ---- Food security ----

png(paste(FigureFileName.chp3,"FS.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(FS.province)
dev.off()

# ---- Marine tenure ----

png(paste(FigureFileName.chp3,"MT.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(MT.province)
dev.off()

# ---- Place attachment ----

png(paste(FigureFileName.chp3,"PA.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(PA.province)
dev.off()

# ---- School enrollment  ----

png(paste(FigureFileName.chp3,"SE.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(SE.province)
dev.off()


# ---- export big five plotting data frame, with pvals ----

export(Plotting.Province.ContData%>%filter(!is.na(FSMean)),paste(FigureFileName.chp3,"Province_level_contdata_means.xlsx",sep="/"))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: PLOTTING FISHING CHARACTERISTIC VARIABLES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


plot.theme.2 <- theme(axis.ticks=element_blank(),
                      panel.background=element_rect(fill="white",
                                                    colour="#909090"),
                      panel.border=element_rect(fill=NA,
                                                size=0.25,
                                                colour="#C0C0C0"),
                      panel.grid.major.x=element_line(colour="#C0C0C0",
                                                      size=0.25,
                                                      linetype=3),
                      panel.grid.major.y=element_blank(),
                      plot.title=element_text(size=rel(1.2),
                                              face="bold",
                                              hjust=0.5),
                      plot.margin=margin(t=30,r=30,b=5,l=5,unit="pt"),
                      axis.title=element_text(size=rel(0.9),
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
                      axis.text.y=element_text(size=rel(0.9),
                                               angle=0,
                                               colour="#303030",
                                               lineheight=0.7),
                      axis.text.x=element_text(size=rel(0.8),
                                               angle=0,
                                               colour="#303030",
                                               lineheight=0.9,
                                               hjust=1),
                      legend.position="top",
                      legend.justification="right",
                      legend.box.spacing=unit(0.1,"cm"))

plot.guides.2 <- guides(fill=guide_legend(ncol=1,reverse=T))


# ---- PRIMARY OCCUPATION ----

Primaryocc.province.status <-
  melt(Province.Level.PropData.Means,
       id.vars="Province",measure.vars=c("Percent.PrimaryOcc.Other",
                                                         "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
                                                         "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
                                                         "Percent.PrimaryOcc.HarvestForest",
                                                         "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
  mutate(Province=factor(Province,levels=c("West Papua","Maluku","East Nusa Tenggara","Southeast Sulawesi"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=Province,y=value,fill=variable),
           stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
                             "Harvest Forest Products","Fishing", "Farming")) +
  coord_flip() + plot.theme + labs(x="Province",y="Primary Occupation (% households)") + plot.guides.techreport


# # Primary occ trends, for each province, in a facet 
# 
# Primaryocc.PapuaBarat.trend <-
#   melt(Province.Level.PropData.Means %>% filter(Treatment==1 & Province=="Papua Barat"),
#        id.vars=c("RepeatYear"),measure.vars=c("Percent.PrimaryOcc.Other",
#                                                          "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
#                                                          "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
#                                                          "Percent.PrimaryOcc.HarvestForest",
#                                                          "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
#   mutate(RepeatYear=factor(RepeatYear,levels=c("Baseline","First Repeat","Second Repeat"),ordered=T)) %>%
#   ggplot() +
#   geom_bar(aes(x=RepeatYear,y=value,fill=variable),
#            stat="identity",
#            position="fill",
#            width=0.8,
#            size=0.15,
#            colour="#505050",
#            show.legend=F) +
#   scale_y_continuous(expand=c(0,0),
#                      labels=scales::percent_format()) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["PrimaryOcc"]],
#                     labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
#                              "Harvest Forest Products","Fishing", "Farming")) +
#   coord_flip() + plot.theme.2 + labs(x="Monitoring Year",y="Primary Occupation (% households)",title="Papua Barat")
# 
# 
# Primaryocc.Maluku.trend <-
#   melt(Province.Level.PropData.Means %>% filter(Treatment==1 & Province=="Maluku"),
#        id.vars=c("RepeatYear"),measure.vars=c("Percent.PrimaryOcc.Other",
#                                               "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
#                                               "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
#                                               "Percent.PrimaryOcc.HarvestForest",
#                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
#   mutate(RepeatYear=factor(RepeatYear,levels=c("Baseline","First Repeat","Second Repeat"),ordered=T)) %>%
#   ggplot() +
#   geom_bar(aes(x=RepeatYear,y=value,fill=variable),
#            stat="identity",
#            position="fill",
#            width=0.8,
#            size=0.15,
#            colour="#505050",
#            show.legend=F) +
#   scale_y_continuous(expand=c(0,0),
#                      labels=scales::percent_format()) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["PrimaryOcc"]],
#                     labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
#                              "Harvest Forest Products","Fishing", "Farming")) +
#   coord_flip() + plot.theme.2 + labs(x="Monitoring Year",y="Primary Occupation (% households)",title="Maluku")
# 
# 
# Primaryocc.NusaTenggara.trend <-
#   melt(Province.Level.PropData.Means %>% filter(Treatment==1 & Province=="Nusa Tenggara Timur"),
#        id.vars=c("RepeatYear"),measure.vars=c("Percent.PrimaryOcc.Other",
#                                               "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
#                                               "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
#                                               "Percent.PrimaryOcc.HarvestForest",
#                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
#   mutate(RepeatYear=factor(RepeatYear,levels=c("Baseline","First Repeat","Second Repeat"),ordered=T)) %>%
#   ggplot() +
#   geom_bar(aes(x=RepeatYear,y=value,fill=variable),
#            stat="identity",
#            position="fill",
#            width=0.8,
#            size=0.15,
#            colour="#505050",
#            show.legend=F) +
#   scale_y_continuous(expand=c(0,0),
#                      labels=scales::percent_format()) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["PrimaryOcc"]],
#                     labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
#                              "Harvest Forest Products","Fishing", "Farming")) +
#   coord_flip() + plot.theme.2 + labs(x="Monitoring Year",y="Primary Occupation (% households)",title="Nusa Tenggara Timur")
# 
# 
# Primaryocc.Sulawesi.trend <-
#   melt(Province.Level.PropData.Means %>% filter(Treatment==1 & Province=="Sulawesi Tenggara"),
#        id.vars=c("RepeatYear"),measure.vars=c("Percent.PrimaryOcc.Other",
#                                               "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
#                                               "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
#                                               "Percent.PrimaryOcc.HarvestForest",
#                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
#   mutate(RepeatYear=factor(RepeatYear,levels=c("Baseline","First Repeat","Second Repeat"),ordered=T)) %>%
#   ggplot() +
#   geom_bar(aes(x=RepeatYear,y=value,fill=variable),
#            stat="identity",
#            position="fill",
#            width=0.8,
#            size=0.15,
#            colour="#505050",
#            show.legend=F) +
#   scale_y_continuous(expand=c(0,0),
#                      labels=scales::percent_format()) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["PrimaryOcc"]],
#                     labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
#                              "Harvest Forest Products","Fishing", "Farming")) +
#   coord_flip() + plot.theme.2 + labs(x="Monitoring Year",y="Primary Occupation (% households)",title="Sulawesi Tenggara")
# 
# 
# Primaryocc.legend.plot <-
#   melt(Province.Level.PropData.Means %>% filter(Treatment==1 & Province=="Papua Barat"),
#        id.vars=c("RepeatYear"),measure.vars=c("Percent.PrimaryOcc.Other",
#                                               "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
#                                               "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
#                                               "Percent.PrimaryOcc.HarvestForest",
#                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
#   mutate(RepeatYear=factor(RepeatYear,levels=c("Baseline","First Repeat","Second Repeat"),ordered=T)) %>%
#   ggplot() +
#   geom_bar(aes(x=RepeatYear,y=value,fill=variable),
#            stat="identity",
#            position="fill",
#            width=0.8,
#            size=0.15,
#            colour="#505050") +
#   scale_y_continuous(expand=c(0,0),
#                      labels=scales::percent_format()) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["PrimaryOcc"]],
#                     labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
#                              "Harvest Forest Products","Fishing", "Farming")) +
#   coord_flip() + plot.theme.2 + labs(x="Monitoring Year",y="Primary Occupation (% households)",title="Papua Barat") + plot.guides.2
# 
# 
# PrimaryOcc.legend <- g_legend(Primaryocc.legend.plot)
# 
# 
# # ---- Arrange grob PrimaryOcc ----
# 
# PrimaryOcc.trend.plot <- 
#   plot_grid(arrangeGrob(
#                  Primaryocc.Sulawesi.trend,
#                  Primaryocc.NusaTenggara.trend,
#                  Primaryocc.Maluku.trend,
#                  Primaryocc.PapuaBarat.trend,ncol=2),
#                PrimaryOcc.legend,
#             ncol=2,rel_heights=c(1,0.2),rel_widths=c(1,0.35))
# 
# png(paste(FigureFileName.chp3,"PrimaryOcc.trend.byprovince.png",sep="/"),
#     units="in",height=6,width=12,res=400)
# plot(PrimaryOcc.trend.plot)
# dev.off()


# ---- FISHING FREQUENCY ----

FishFreq.province.status <-
  melt(Province.Level.PropData.Means,
       id.vars="Province",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                                         "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                                         "Prop.Fish.AlmostNever")) %>%
  mutate(Province=factor(Province,levels=c("West Papua","Maluku","East Nusa Tenggara","Southeast Sulawesi"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=Province,y=value,fill=variable),
           stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + labs(x="Province",y="Fishing Frequency (% fishing households)") + plot.guides.techreport


# ---- SELL FISH FREQUENCY ----

SellFishFreq.province.status <-
  melt(Province.Level.PropData.Means,
       id.vars="Province",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                                         "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                                         "Prop.SellFish.AlmostNever")) %>%
  mutate(Province=factor(Province,levels=c("West Papua","Maluku","East Nusa Tenggara","Southeast Sulawesi"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=Province,y=value,fill=variable),
           stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + labs(x="Province",y="Frequency of selling fish (% fishing households)") + plot.guides.techreport


# ---- FISH PROTEIN ----

ProteinFish.province.status <-
  melt(Province.Level.PropData.Means,
       id.vars="Province",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                                         "ProteinFish.Half","ProteinFish.Some",
                                                         "ProteinFish.None")) %>%
  mutate(Province=factor(Province,levels=c("West Papua","Maluku","East Nusa Tenggara","Southeast Sulawesi"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=Province,y=value,fill=variable),
           stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + labs(x="Province",y="Dietary Protein from fish (% fishing households)") + plot.guides.techreport


# ---- export proportional data plots ----

# - primary occupation
png(paste(FigureFileName.chp3,"PrimaryOcc.status.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Primaryocc.province.status)
dev.off()

# - frequency fish (only fishing households)
png(paste(FigureFileName.chp3,"FishFreq.status.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(FishFreq.province.status)
dev.off()

# - frequency sell fish (only fishing households)
png(paste(FigureFileName.chp3,"SellFishFreq.status.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(SellFishFreq.province.status)
dev.off()

# - fish protein consumption (only fishing households)
png(paste(FigureFileName.chp3,"ProteinFish.status.byprovince.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(ProteinFish.province.status)
dev.off()


# ---- export proportional data excel ----

export(Province.Level.PropData.Means,paste(FigureFileName.chp3,"Province_level_propdata_means.xlsx",sep="/"))




# ---- METADATA ----

Num.HH.Setts <-
  HHData %>% filter(Treatment==1) %>%
  group_by(MPAID,InterviewYear) %>%
  summarise(Num.Sett=length(unique(SettlementID)),
            Num.HH=length(HouseholdID))

export(Num.HH.Setts,'x_Flat_data_files/1_Social/Outputs/MPA_Vision_Chp3/MPA-specific_metadata.xlsx')
