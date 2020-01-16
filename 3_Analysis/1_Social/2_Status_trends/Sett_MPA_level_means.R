#
# code: Settlement level and MPA level means/proportions for household data
# 
# author: Amari Bauer
# created: June 2019
# Adapted from New_SBS_MPA_Mystery.R
# modified: Kelly Claborn, October 2019

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Settlement-level & MPA-level means ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Settlement-level analysis, for status and annex plots ----

# Sett.Level.Means is a dataframe that includes means/proportions for all settlements, for each monitoring year.
# It then calculates the percent of respondents in each settlement that gave each response for the proportional
# data and also calculates the mean and error terms for a few variables

Sett.Level.Means <- 
  HHData %>%
  dplyr::group_by(SettlementID,SettlementName,MPAID,MonitoringYear,InterviewYear,Treatment) %>%
  dplyr::summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
            FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
            MAMean=round(mean(MAIndex,na.rm=T),2),
            MAErr=round(sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),2),
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
            Prop.IncFish.None=(length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Some=(length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Half=(length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Most=(length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.All=(length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
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
            MatAssets.gini=gini(MAIndex),
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
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100)
# Sett.Level.Means <- Sett.Level.Means[!is.na(Sett.Level.Means$SettlementID),]


# ---- 1.2 MPA-level analysis, for status, trend, and annex plots ----

# MPA.Level.Means is a dataframe that calculates the percent of respondents in each MPA, for each year
# that gave each response for the proportional data and also calculates the mean and error terms for a few variables

MPA.Level.Means <- 
  HHData %>%
  dplyr::group_by(MPAID,MonitoringYear,InterviewYear,Treatment) %>%
  dplyr::summarise(FSMean=round(mean(FSIndex,na.rm=T),2),
            FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
            MAMean=round(mean(MAIndex,na.rm=T),2),
            MAErr=round(sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),2),
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
            Prop.IncFish.None=(length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Some=(length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Half=(length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.Most=(length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
            Prop.IncFish.All=(length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)])/length(PercentIncFish[!is.na(PercentIncFish)]))*100,
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
            MatAssets.gini=gini(MAIndex),
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
            MarineContributionErr=sd(MarineContribution,na.rm=T)/sqrt(length(MarineContribution)),
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
  ungroup() %>%
  mutate(MonitoringYearBahasa=ifelse(!grepl("Baseline",MonitoringYear),
                                     paste(substr(MonitoringYear,1,1),"Tahun Setelah",sep=" "),
                                     as.character(MonitoringYear)))

MPA.Level.Means <- MPA.Level.Means[!is.na(MPA.Level.Means$MPAID),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Age and Gender Breakdown by MPA, settlement, and treatment  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ----  2.1 Calculating Age and Gender breakdown by MPA, settlement, and treatment ----

AgeGenderDemos <- left_join(HHData[,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "InterviewYear", 
                                      "SettlementName", "Treatment")],
                            IndDemos[,c("HouseholdID", "IndividualGender", "IndividualAge")],
                            by="HouseholdID")


AgeGender.AvgAge.byMPA <-
  AgeGenderDemos %>%
  filter(Treatment==1)%>%
  group_by(MPAID,MonitoringYear,InterviewYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))

AgeGender.AvgAge.bySett <-
  AgeGenderDemos %>%
  filter(Treatment==1) %>%
  group_by(SettlementName,MPAID,MonitoringYear,InterviewYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))

AgeGender.AvgAge.control <-
  AgeGenderDemos %>%
  filter(Treatment==0) %>%
  group_by(MPAID,MonitoringYear,InterviewYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))



#Determining the percent of individuals of each gender that fall within each of our age categories
AgeGenderDemos.ByMPA <- 
  AgeGenderDemos[AgeGenderDemos$Treatment==1,] %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(Male.0.4=(length(IndividualAge[IndividualAge<=4 &
                                             !is.na(IndividualAge) &
                                             IndividualGender==1&
                                             !is.na(IndividualGender)])/
                        length(IndividualAge[!is.na(IndividualAge)&
                                               !is.na(IndividualGender)]))*-100,
            Female.0.4=(length(IndividualAge[IndividualAge<=4 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==0&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*100,
            Male.5.9=(length(IndividualAge[IndividualAge>4 & 
                                             IndividualAge<=9 &
                                             !is.na(IndividualAge) &
                                             IndividualGender==1&
                                             !is.na(IndividualGender)])/
                        length(IndividualAge[!is.na(IndividualAge)&
                                               !is.na(IndividualGender)]))*-100,
            Female.5.9=(length(IndividualAge[IndividualAge>4 & 
                                               IndividualAge<=9 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==0&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*100,
            Male.10.14=(length(IndividualAge[IndividualAge>9 &
                                               IndividualAge<=14 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.10.14=(length(IndividualAge[IndividualAge>9 &
                                                 IndividualAge<=14 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.15.19=(length(IndividualAge[IndividualAge>14 &
                                               IndividualAge<=19 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.15.19=(length(IndividualAge[IndividualAge>14 &
                                                 IndividualAge<=19 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.20.24=(length(IndividualAge[IndividualAge>19 &
                                               IndividualAge<=24 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.20.24=(length(IndividualAge[IndividualAge>19 &
                                                 IndividualAge<=24 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.25.29=(length(IndividualAge[IndividualAge>24 &
                                               IndividualAge<=29 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.25.29=(length(IndividualAge[IndividualAge>24 &
                                                 IndividualAge<=29 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.30.34=(length(IndividualAge[IndividualAge>29 &
                                               IndividualAge<=34 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.30.34=(length(IndividualAge[IndividualAge>29 &
                                                 IndividualAge<=34 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.35.39=(length(IndividualAge[IndividualAge>34 &
                                               IndividualAge<=39 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.35.39=(length(IndividualAge[IndividualAge>34 &
                                                 IndividualAge<=39 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.40.44=(length(IndividualAge[IndividualAge>39 &
                                               IndividualAge<=44 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge) &
                                                 !is.na(IndividualGender)]))*-100,
            Female.40.44=(length(IndividualAge[IndividualAge>39 &
                                                 IndividualAge<=44 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.45.49=(length(IndividualAge[IndividualAge>44 &
                                               IndividualAge<=49 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.45.49=(length(IndividualAge[IndividualAge>44 &
                                                 IndividualAge<=49 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.50.54=(length(IndividualAge[IndividualAge>49 &
                                               IndividualAge<=54 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.50.54=(length(IndividualAge[IndividualAge>49 &
                                                 IndividualAge<=54 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.55.59=(length(IndividualAge[IndividualAge>54 &
                                               IndividualAge<=59 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.55.59=(length(IndividualAge[IndividualAge>54 &
                                                 IndividualAge<=59 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.60.64=(length(IndividualAge[IndividualAge>59 &
                                               IndividualAge<=64 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.60.64=(length(IndividualAge[IndividualAge>59 &
                                                 IndividualAge<=64 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.65.69=(length(IndividualAge[IndividualAge>64 &
                                               IndividualAge<=69 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.65.69=(length(IndividualAge[IndividualAge>64 &
                                                 IndividualAge<=69 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.70.74=(length(IndividualAge[IndividualAge>69 &
                                               IndividualAge<=74 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.70.74=(length(IndividualAge[IndividualAge>69 &
                                                 IndividualAge<=74 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.75.79=(length(IndividualAge[IndividualAge>74 &
                                               IndividualAge<=79 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.75.79=(length(IndividualAge[IndividualAge>74 &
                                                 IndividualAge<=79 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.80.84=(length(IndividualAge[IndividualAge>79 &
                                               IndividualAge<=84 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.80.84=(length(IndividualAge[IndividualAge>79 &
                                                 IndividualAge<=84 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.85.89=(length(IndividualAge[IndividualAge>84 &
                                               IndividualAge<=89 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.85.89=(length(IndividualAge[IndividualAge>84 &
                                                 IndividualAge<=89 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.90.94=(length(IndividualAge[IndividualAge>89 &
                                               IndividualAge<=94 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.90.94=(length(IndividualAge[IndividualAge>89 &
                                                 IndividualAge<=94 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0&
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100,
            Male.95.99=(length(IndividualAge[IndividualAge>94 &
                                               IndividualAge<=99 &
                                               !is.na(IndividualAge) &
                                               IndividualGender==1&
                                               !is.na(IndividualGender)])/
                          length(IndividualAge[!is.na(IndividualAge)&
                                                 !is.na(IndividualGender)]))*-100,
            Female.95.99=(length(IndividualAge[IndividualAge>94 &
                                                 IndividualAge<=99 &
                                                 !is.na(IndividualAge) &
                                                 IndividualGender==0 &
                                                 !is.na(IndividualGender)])/
                            length(IndividualAge[!is.na(IndividualAge)&
                                                   !is.na(IndividualGender)]))*100)

AgeGenderDemos.ByMPA <- AgeGenderDemos.ByMPA[!is.na(AgeGenderDemos.ByMPA$MPAID),]



