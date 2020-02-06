# 
# ---- code: Social status and trends analysis for MPA Vision report, Chp 3 ----
# 
# 

# ---- source data ----

source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files.R')
source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_household_indices.R')
source('2_Functions/3_Plotting/Function_plotthemes.R')


# ---- add a couple of columns ----

HHData <-
  HHData %>%
  mutate(MAIndex.noTV=ifelse(RemoveMA=="No",
                             rowSums(select(., "CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard",
                                            "BoatInboard", "PhoneCombined", "Entertain", "Satellite", "Generator"),
                                     na.rm = TRUE),
                             NA),
         RepeatYear=ifelse(Seascape==1,
                           ifelse(MonitoringYear=="2 Year Post","First Repeat",
                                  ifelse(MonitoringYear=="4 Year Post","Second Repeat",
                                         as.character(MonitoringYear))),
                           ifelse(MonitoringYear=="2 Year Post" | MonitoringYear=="3 Year Post","First Repeat",as.character(MonitoringYear))),
         Province=ifelse(MPAID%in%c(1,2,3,4,5,6),"Papua Barat",
                         ifelse(MPAID%in%c(15,16),"Nusa Tenggara Timur",
                                ifelse(MPAID%in%c(17,18,19),"Maluku",
                                       ifelse(MPAID%in%c(20,21),"Sulawesi Tenggara",NA)))))


# ---- Calculate status and trends for seascape and national-level ----

# CONSIDER removing all fishing characteristic responses for households that do not identify fishing as one of their 1/2/3 occupation.  To remain consistent across MPAs
Province.Level.Means <- 
  HHData %>%
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
                   YrResident=mean(YrResident,na.rm=T),
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
                   Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Child.FS.no=(length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                   Child.FS.yes=(length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                   Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                                     !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                        !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                       !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                       !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                        !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                                !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
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
                   Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
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
                                                                                 !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100) %>%
  mutate(Status=ifelse((Province=="Papua Barat" & RepeatYear=="Second Repeat") |
                         (Province=="Maluku" & RepeatYear=="First Repeat") |
                         (Province=="Nusa Tenggara Timur" & RepeatYear=="First Repeat") |
                         (Province=="Sulawesi Tenggara" & RepeatYear=="Baseline"),1,0))


National.Level.Means <- 
  HHData %>%
  dplyr::group_by(RepeatYear,Treatment) %>%
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
                   YrResident=mean(YrResident,na.rm=T),
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
                   Prop.Fish.AlmostNever=(length(FreqFish[FreqFish==1 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.FewTimesPer6Mo=(length(FreqFish[FreqFish==2 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.FewTimesPerMo=(length(FreqFish[FreqFish==3 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.FewTimesPerWk=(length(FreqFish[FreqFish==4 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.Fish.MoreFewTimesWk=(length(FreqFish[FreqFish==5 & !is.na(FreqFish)])/length(FreqFish[!is.na(FreqFish)]))*100,
                   Prop.SellFish.AlmostNever=(length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.FewTimesPer6Mo=(length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.FewTimesPerMo=(length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.FewTimesPerWk=(length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.SellFish.MoreFewTimesWk=(length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)])/length(FreqSaleFish[!is.na(FreqSaleFish)]))*100,
                   Prop.FishTech.ByHand=(length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.StatNet=(length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.MobileNet=(length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.StatLine=(length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Prop.FishTech.MobileLine=(length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)])/length(MajFishTechnique[!is.na(MajFishTechnique)]))*100,
                   Child.FS.no=(length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                   Child.FS.yes=(length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)])/length(cat.cFS[!is.na(cat.cFS)]))*100,
                   Percent.EatFish.RareOrNever=(length(FreqEatFish[FreqEatFish==1 &
                                                                     !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.FewTimesPer6Mo=(length(FreqEatFish[FreqEatFish==2 &
                                                                        !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.FewTimesPerMo=(length(FreqEatFish[FreqEatFish==3 &
                                                                       !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.FewTimesPerWk=(length(FreqEatFish[FreqEatFish==4 &
                                                                       !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   Percent.EatFish.MoreFewTimesWk=(length(FreqEatFish[FreqEatFish==5 &
                                                                        !is.na(FreqEatFish)])/length(FreqEatFish[!is.na(FreqEatFish)]))*100,
                   ProteinFish.None=(length(PercentProteinFish[PercentProteinFish==1 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.Some=(length(PercentProteinFish[PercentProteinFish==2 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.Half=(length(PercentProteinFish[PercentProteinFish==3 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.Most=(length(PercentProteinFish[PercentProteinFish==4 &
                                                                 !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
                   ProteinFish.All=(length(PercentProteinFish[PercentProteinFish==5 &
                                                                !is.na(PercentProteinFish)])/length(PercentProteinFish[!is.na(PercentProteinFish)]))*100,
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
                   Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                   Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
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
                                                                                 !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100) %>%
  mutate(Status=ifelse(RepeatYear=="Second Repeat",1,0))

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: STATISTICAL MONOTONIC TREND TESTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- province level statistical tests (monotonic trend) ----

PapuaBarat.trend.test  <- 
  cbind.data.frame(Province="Papua Barat",
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Province=="Papua Barat" & HHData$Treatment==1 &
                                                                HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                                       HHData[HHData$Province=="Papua Barat" & HHData$Treatment==1 &
                                                                HHData$RepeatYear==unique(HHData$RepeatYear)[2],a], 
                                                       HHData[HHData$Province=="Papua Barat" & HHData$Treatment==1 &
                                                                HHData$RepeatYear==unique(HHData$RepeatYear)[3],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 

NusaTenggara.trend.test  <- 
  cbind.data.frame(Province="Nusa Tenggara Timur",
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Province=="Nusa Tenggara Timur" & HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                            HHData[HHData$Province=="Nusa Tenggara Timur" & HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[2],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 

Maluku.trend.test <-
  cbind.data.frame(Province="Maluku",
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Province=="Maluku" & HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                            HHData[HHData$Province=="Maluku" & HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[2],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 


trend.pvals.byProvince <-
  rbind.data.frame(PapuaBarat.trend.test,
                   NusaTenggara.trend.test,
                   Maluku.trend.test,
                   data.frame(Province="Sulawesi Tenggara",
                              FS.trend.pval=1,
                              MA.trend.pval=1,
                              MT.trend.pval=1,
                              PA.trend.pval=1,
                              SE.trend.pval=1))

Plotting.Province.HHData <-
  Province.Level.Means %>% filter(Treatment==1) %>%
  rbind.data.frame(data.frame(Province=c("Maluku","Nusa Tenggara Timur","Sulawesi Tenggara","Sulawesi Tenggara"),
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
         Asterisk.SE=ifelse(SE.trend.pval<0.01,"***",
                            ifelse(SE.trend.pval<0.05 & SE.trend.pval>=0.01,"**",
                                   ifelse(SE.trend.pval<0.1 & SE.trend.pval>=0.05,"*",""))),
         ProvinceName.FS=ifelse(Asterisk.FS=="",as.character(Province),paste(Province,Asterisk.FS,sep=" ")),
         ProvinceName.MA=ifelse(Asterisk.MA=="",as.character(Province),paste(Province,Asterisk.MA,sep=" ")),
         ProvinceName.MT=ifelse(Asterisk.MT=="",as.character(Province),paste(Province,Asterisk.MT,sep=" ")),
         ProvinceName.PA=ifelse(Asterisk.PA=="",as.character(Province),paste(Province,Asterisk.PA,sep=" ")),
         ProvinceName.SE=ifelse(Asterisk.SE=="",as.character(Province),paste(Province,Asterisk.SE,sep=" ")))


# ---- national level statistical tests (monotonic trend) ----

National.trend.test <-
  cbind.data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate"),
                          function(a){
                            t(MannKendall(c(HHData[HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[1],a],
                                            HHData[HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[2],a],
                                            HHData[HHData$Treatment==1 &
                                                     HHData$RepeatYear==unique(HHData$RepeatYear)[3],a]))["sl"])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex, SE.trend.pval = SERate) 

Plotting.National.HHData <-
  National.Level.Means %>% filter(Treatment==1) %>%
  cbind.data.frame(rbind.data.frame(National.trend.test,
                                    National.trend.test,
                                    National.trend.test)) %>%
  mutate(Province="Indonesia",
         Asterisk.FS=ifelse(FS.trend.pval<0.01,"***",
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
         Asterisk.SE=ifelse(SE.trend.pval<0.01,"***",
                            ifelse(SE.trend.pval<0.05 & SE.trend.pval>=0.01,"**",
                                   ifelse(SE.trend.pval<0.1 & SE.trend.pval>=0.05,"*",""))),
         ProvinceName.FS=ifelse(Asterisk.FS=="","Indonesia",paste("Indonesia",Asterisk.FS,sep=" ")),
         ProvinceName.MA=ifelse(Asterisk.MA=="","Indonesia",paste("Indonesia",Asterisk.MA,sep=" ")),
         ProvinceName.MT=ifelse(Asterisk.MT=="","Indonesia",paste("Indonesia",Asterisk.MT,sep=" ")),
         ProvinceName.PA=ifelse(Asterisk.PA=="","Indonesia",paste("Indonesia",Asterisk.PA,sep=" ")),
         ProvinceName.SE=ifelse(Asterisk.SE=="","Indonesia",paste("Indonesia",Asterisk.SE,sep=" "))) 

Plotting.HHData <-
  rbind.data.frame(Plotting.National.HHData,
                   Plotting.Province.HHData) %>%
  ungroup() %>%
  mutate(Province=factor(Province,levels=c("Sulawesi Tenggara","Nusa Tenggara Timur","Maluku","Papua Barat","Indonesia"),ordered=T))



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
                      axis.title=element_text(size=rel(0.9),
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
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
  Plotting.Province.HHData %>%
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
  scale_x_discrete(labels=unique(Plotting.Province.HHData$ProvinceName.MA[order(Plotting.Province.HHData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household material assets")


# --- Food security

FS.province <- 
  rbind.data.frame(Plotting.Province.HHData[,c("FSMean","FSErr","RepeatYear","Province","ProvinceName.FS")],
                   data.frame(FSMean=0,
                              FSErr=NA,
                              RepeatYear="Baseline",
                              Province=" ",
                              ProvinceName.FS=" ")) %>%
  ungroup() %>%
  mutate(Province=factor(Province,levels=c("Sulawesi Tenggara","Nusa Tenggara Timur","Maluku","Papua Barat"," "),ordered=T)) %>%
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
  geom_text(aes(x=6,y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=6,y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=6,y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","First Repeat","Second Repeat"),
                     values=c("Baseline"=0.3,"First Repeat"=0.6,"Second Repeat"=0.9)) +
  scale_x_discrete(labels=c(unique(Plotting.Province.HHData$ProvinceName.FS[order(Plotting.Province.HHData$Province)])," ")) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household food security")


# --- Marine tenure

MT.province <- 
  Plotting.Province.HHData %>%
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
  scale_x_discrete(labels=unique(Plotting.Province.HHData$ProvinceName.MT[order(Plotting.Province.HHData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household marine tenure")


# --- Place attachment

PA.province <- 
  Plotting.Province.HHData %>%
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
  scale_x_discrete(labels=unique(Plotting.Province.HHData$ProvinceName.PA[order(Plotting.Province.HHData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Place attachment")


# --- School enrollment

SE.province <- 
  Plotting.Province.HHData %>%
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
  scale_x_discrete(labels=unique(Plotting.Province.HHData$ProvinceName.SE[order(Plotting.Province.HHData$Province)])) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="School enrollment rate")


# ---- export province level big five plots ----

FigureFileName.chp3 <- 'x_Flat_data_files/1_Social/Outputs/MPA_Vision_Chp3/Trends'

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

# export(Plotting.Province.HHData%>%filter(!is.na(FSMean)),paste(FigureFileName.chp3,"Province_level_means.xlsx",sep="/"))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: PLOTTING FISHING CHARACTERISTIC VARIABLES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# --- PRIMARY OCCUPATION

Primaryocc.province.status <-
  melt(Province.Level.Means %>% filter(Treatment==1 & Status==1),
       id.vars=c("RepeatYear","Province"),measure.vars=c("Percent.PrimaryOcc.Other",
                                                         "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
                                                         "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
                                                         "Percent.PrimaryOcc.HarvestForest",
                                                         "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
  mutate(Province=factor(Province,levels=c("Papua Barat","Maluku","Nusa Tenggara Timur","Sulawesi Tenggara"),ordered=T)) %>%
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


# Primary occ trends, for each province, in a facet 

Primaryocc.PapuaBarat.trend <-
  melt(Province.Level.Means %>% filter(Treatment==1 & Province=="Papua Barat"),
       id.vars=c("RepeatYear"),measure.vars=c("Percent.PrimaryOcc.Other",
                                                         "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
                                                         "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
                                                         "Percent.PrimaryOcc.HarvestForest",
                                                         "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
  mutate(RepeatYear=factor(RepeatYear,levels=c("Baseline","First Repeat","Second Repeat"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=RepeatYear,y=value,fill=variable),
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
  coord_flip() + plot.theme + labs(x="Monitoring Year",y="Primary Occupation (% households)",title="Papua Barat") + plot.guides.techreport


# - FISHING FREQUENCY
Freqfish.trendplot <-
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                      "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                      "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),size=0.25,colour="#505050") +
  geom_text(aes(x=3.25,y=.91,label="Treatment",fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=2.95,y=.91,label="Control",fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Trendplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Freqsellfish.trendplot <-
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                      "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                      "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),size=0.25,colour="#505050") +
  geom_text(aes(x=3.25,y=.91,label="Treatment",fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=2.95,y=.91,label="Control",fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Trendplot.labs["FreqSellFish"] + plot.guides.techreport