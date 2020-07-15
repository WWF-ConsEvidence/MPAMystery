#
# code: Settlement level and MPA level means/proportions for household data by zone (for Wakatobi data ONLY)
# 
# author: Kelly Claborn
# created: April 2020
# modified: 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Settlement-level & MPA-level means ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

HHData <-
  HHData %>% 
  mutate(Zone=ifelse(Zone=="Take","Use",Zone),
         Zone=factor(Zone,levels=c("Use","No Take"),ordered=T))


# ---- 1.1 Settlement-level analysis, for status and annex plots ----

# Sett.Level.Means is a dataframe that includes means/proportions for all settlements, for each monitoring year.
# It then calculates the percent of respondents in each settlement that gave each response for the proportional
# data and also calculates the mean and error terms for a few variables

Sett.Level.Means.byZone <- 
  HHData %>%
  dplyr::group_by(SettlementID,SettlementName,MPAID,MonitoringYear,InterviewYear,Zone) %>%
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
            Percent.Rel.Buddhist=(length(Religion[Religion==4 &
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
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            # percent of respondents (who fish) using which type of gear primarly to fish
            Primary.FishTech.Hand=(length(PrimaryFishTechnique[PrimaryFishTechnique==1 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.StatLine=(length(PrimaryFishTechnique[PrimaryFishTechnique==2 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.MobileLine=(length(PrimaryFishTechnique[PrimaryFishTechnique==3 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Glean=(length(PrimaryFishTechnique[PrimaryFishTechnique==4 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.GrapWound=(length(PrimaryFishTechnique[PrimaryFishTechnique==5 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.FallGear=(length(PrimaryFishTechnique[PrimaryFishTechnique==6 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.StatGill=(length(PrimaryFishTechnique[PrimaryFishTechnique==7 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Trammel=(length(PrimaryFishTechnique[PrimaryFishTechnique==8 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.MobileGill=(length(PrimaryFishTechnique[PrimaryFishTechnique==9 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Trap=(length(PrimaryFishTechnique[PrimaryFishTechnique==10 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Fence=(length(PrimaryFishTechnique[PrimaryFishTechnique==11 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.LiftNet=(length(PrimaryFishTechnique[PrimaryFishTechnique==12 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.MobileDredge=(length(PrimaryFishTechnique[PrimaryFishTechnique==13 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Trawl=(length(PrimaryFishTechnique[PrimaryFishTechnique==14 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Seine=(length(PrimaryFishTechnique[PrimaryFishTechnique==15 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.BombPoison=(length(PrimaryFishTechnique[PrimaryFishTechnique==16 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Other=(length(PrimaryFishTechnique[PrimaryFishTechnique==996 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            # number of responses per primary fishing technique category
            N.Primary.FishTech.Hand=length(PrimaryFishTechnique[PrimaryFishTechnique==1 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.StatLine=length(PrimaryFishTechnique[PrimaryFishTechnique==2 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.MobileLine=length(PrimaryFishTechnique[PrimaryFishTechnique==3 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Glean=length(PrimaryFishTechnique[PrimaryFishTechnique==4 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.GrapWound=length(PrimaryFishTechnique[PrimaryFishTechnique==5 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.FallGear=length(PrimaryFishTechnique[PrimaryFishTechnique==6 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.StatGill=length(PrimaryFishTechnique[PrimaryFishTechnique==7 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Trammel=length(PrimaryFishTechnique[PrimaryFishTechnique==8 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.MobileGill=length(PrimaryFishTechnique[PrimaryFishTechnique==9 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Trap=length(PrimaryFishTechnique[PrimaryFishTechnique==10 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Fence=length(PrimaryFishTechnique[PrimaryFishTechnique==11 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.LiftNet=length(PrimaryFishTechnique[PrimaryFishTechnique==12 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.MobileDredge=length(PrimaryFishTechnique[PrimaryFishTechnique==13 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Trawl=length(PrimaryFishTechnique[PrimaryFishTechnique==14 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Seine=length(PrimaryFishTechnique[PrimaryFishTechnique==15 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.BombPoison=length(PrimaryFishTechnique[PrimaryFishTechnique==16 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Other=length(PrimaryFishTechnique[PrimaryFishTechnique==996 & !is.na(PrimaryFishTechnique)]))

# Sett.Level.Means <- Sett.Level.Means[!is.na(Sett.Level.Means$SettlementID),]


# ---- 1.2 MPA-level analysis, for status, trend, and annex plots ----

# MPA.Level.Means is a dataframe that calculates the percent of respondents in each MPA, for each year
# that gave each response for the proportional data and also calculates the mean and error terms for a few variables

MPA.Level.Means.byZone <- 
  HHData %>%
  dplyr::group_by(MPAID,MonitoringYear,InterviewYear,Zone) %>%
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
            Percent.Rel.Buddhist=(length(Religion[Religion==4 &
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
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            # percent of respondents (who fish) using which type of gear primarly to fish
            Primary.FishTech.Hand=(length(PrimaryFishTechnique[PrimaryFishTechnique==1 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.StatLine=(length(PrimaryFishTechnique[PrimaryFishTechnique==2 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.MobileLine=(length(PrimaryFishTechnique[PrimaryFishTechnique==3 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Glean=(length(PrimaryFishTechnique[PrimaryFishTechnique==4 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.GrapWound=(length(PrimaryFishTechnique[PrimaryFishTechnique==5 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.FallGear=(length(PrimaryFishTechnique[PrimaryFishTechnique==6 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.StatGill=(length(PrimaryFishTechnique[PrimaryFishTechnique==7 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Trammel=(length(PrimaryFishTechnique[PrimaryFishTechnique==8 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.MobileGill=(length(PrimaryFishTechnique[PrimaryFishTechnique==9 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Trap=(length(PrimaryFishTechnique[PrimaryFishTechnique==10 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Fence=(length(PrimaryFishTechnique[PrimaryFishTechnique==11 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.LiftNet=(length(PrimaryFishTechnique[PrimaryFishTechnique==12 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.MobileDredge=(length(PrimaryFishTechnique[PrimaryFishTechnique==13 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Trawl=(length(PrimaryFishTechnique[PrimaryFishTechnique==14 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Seine=(length(PrimaryFishTechnique[PrimaryFishTechnique==15 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.BombPoison=(length(PrimaryFishTechnique[PrimaryFishTechnique==16 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            Primary.FishTech.Other=(length(PrimaryFishTechnique[PrimaryFishTechnique==996 & !is.na(PrimaryFishTechnique)])/length(PrimaryFishTechnique[!is.na(PrimaryFishTechnique)]))*100,
            # number of responses per primary fishing technique category
            N.Primary.FishTech.Hand=length(PrimaryFishTechnique[PrimaryFishTechnique==1 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.StatLine=length(PrimaryFishTechnique[PrimaryFishTechnique==2 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.MobileLine=length(PrimaryFishTechnique[PrimaryFishTechnique==3 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Glean=length(PrimaryFishTechnique[PrimaryFishTechnique==4 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.GrapWound=length(PrimaryFishTechnique[PrimaryFishTechnique==5 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.FallGear=length(PrimaryFishTechnique[PrimaryFishTechnique==6 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.StatGill=length(PrimaryFishTechnique[PrimaryFishTechnique==7 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Trammel=length(PrimaryFishTechnique[PrimaryFishTechnique==8 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.MobileGill=length(PrimaryFishTechnique[PrimaryFishTechnique==9 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Trap=length(PrimaryFishTechnique[PrimaryFishTechnique==10 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Fence=length(PrimaryFishTechnique[PrimaryFishTechnique==11 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.LiftNet=length(PrimaryFishTechnique[PrimaryFishTechnique==12 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.MobileDredge=length(PrimaryFishTechnique[PrimaryFishTechnique==13 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Trawl=length(PrimaryFishTechnique[PrimaryFishTechnique==14 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Seine=length(PrimaryFishTechnique[PrimaryFishTechnique==15 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.BombPoison=length(PrimaryFishTechnique[PrimaryFishTechnique==16 & !is.na(PrimaryFishTechnique)]),
            N.Primary.FishTech.Other=length(PrimaryFishTechnique[PrimaryFishTechnique==996 & !is.na(PrimaryFishTechnique)])) %>%
  ungroup() %>%
  mutate(MonitoringYearBahasa=ifelse(!grepl("Baseline",MonitoringYear),
                                     paste(substr(MonitoringYear,1,1),"Tahun Setelah",sep=" "),
                                     as.character(MonitoringYear)))

MPA.Level.Means.byZone <- MPA.Level.Means.byZone[!is.na(MPA.Level.Means.byZone$MPAID),]

