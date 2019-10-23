#
# code: Social MPA Mystery Analysis
# 
# author: Amari Bauer
# created: June 2019
# Adapted from New_SBS_MPA_Mystery.R


source("C:/Users/HP/Dropbox/NotThisOne/Source_social_data_flat_files.R")
source("C:/Users/HP/Dropbox/NotThisOne/Calculate_BigFive.R")

library(reldist,dplyr)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Tech. Report Datasets ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

#---- 1.1 Deriving additional variables for the HHData table ----

#Summing child food variables to construct child food security index on the household level
HHData$cFS <- ifelse(HHData$RemoveFS=="No",
                  rowSums(HHData[,c("LowCostFood", "ChildBalancedMeal", "ChildNotEnough", 
                                 "ChildPortion", "ChildHungry", "ChildSkip", "FreqChildSkip", 
                                 "NoMealChild")],
                          na.rm=TRUE),
                  NA)

#Adding a column that classifies whether they meet the standard for child food insecurity 
HHData$cat.cFS <- ifelse(HHData$cFS>=6.9,"Evidence",
                      ifelse(HHData$cFS<6.9,"No or insufficient evidence",NA))


#Subsetting table to just include the household heads, removing NA's in the relation to household head column
HHData <- right_join(HHData,IndDemos[IndDemos$RelationHHH==0 &
            !is.na(IndDemos$RelationHHH),c("HouseholdID","IndividualGender",
            "IndividualEducation","IndividualAge")],by="HouseholdID")

#Making InterviewYear a factor
HHData$InterviewYear <- factor(HHData$InterviewYear,
                                          levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018",
                                                   "2019","2020","2021","2022","2023","2024","2025","2026","2027",
                                                   "2028","2029","2030"),
                                          ordered=T)

#Creating a variable (HHsize) that includes the household size, calculated from the length of the IndDemos householdID column
HHData$HHsize <- sapply(HHData$HouseholdID,
                                   function(i){
                                     c(length(IndDemos$HouseholdID[which(IndDemos$HouseholdID==i)]))
                                   })

#Creating the variable Unwell, that is the average days unwell, on a household basis
HHData<- left_join(HHData,IndDemos %>% group_by(HouseholdID) %>% summarise(DaysUnwell=sum(DaysUnwell,na.rm=T)/length(HouseholdID)),by="HouseholdID")

# ---- 1.2 Settlement-level analysis, for status and annex plots ----

#Techreport.Status.BySett is a dataframe that includes the treatment settlements, for each monitoring year.
#It then calculates the percent of respondents in each settlement that gave each response for the proportional
#data and also calculates the mean and error terms for a few variables

Techreport.Status.BySett <- 
  HHData %>%
  group_by(SettlementID,MonitoringYear,Treatment) %>%
  summarise(MPAID=unique(MPAID),
            SettlementName=unique(SettlementName),
            YrResident=mean(YrResident,na.rm=T),
            HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                  !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
            HHH.male=(length(IndividualGender[IndividualGender==1 &
                                                !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
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
            SERate=mean(SERate,na.rm=T),
            UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)),
            TimeMarketMean=mean(TimeMarket,na.rm=T),
            TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)),
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
                                                                  !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            MTManage=mean(RightsManage,na.rm=T),
            MTHarvest=mean(RightsHarvest,na.rm=T),
            MTAccess=mean(RightsAccess,na.rm=T),
            MTTransfer=mean(RightsTransfer,na.rm=T),
            MTExclude=mean(RightsExclude,na.rm=T),
            MatAssets.gini=gini(MAIndex),
            MAIndex=mean(MAIndex,na.rm=T),
            Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100
            )

Techreport.Status.BySett <- Techreport.Status.BySett[!is.na(Techreport.Status.BySett$SettlementID),]


# ---- 1.3 MPA-level analysis, for status, trend, and annex plots ----

#Techreport.Trend.ByMPA is a dataframe that calculates the percent of respondents in each MPA, for each year
#that gave each response for the proportional data and also calculates the mean and error terms for a few variables

Techreport.Trend.ByMPA <- 
  HHData %>%
  group_by(MPAID,MonitoringYear,Treatment) %>%
  summarise(YrResident=mean(YrResident,na.rm=T),
        HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                  !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
            HHH.male=(length(IndividualGender[IndividualGender==1 &
                                                !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
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
            SERate=mean(SERate,na.rm=T),
            UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)),
            TimeMarketMean=mean(TimeMarket,na.rm=T),
            TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)),
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
                                                                  !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            MTManage=mean(RightsManage,na.rm=T),
            MTHarvest=mean(RightsHarvest,na.rm=T),
            MTAccess=mean(RightsAccess,na.rm=T),
            MTTransfer=mean(RightsTransfer,na.rm=T),
            MTExclude=mean(RightsExclude,na.rm=T),
            MatAssets.gini=gini(MAIndex),
            MAIndex=mean(MAIndex,na.rm=T),
            Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
            Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)

Techreport.Trend.ByMPA <- Techreport.Trend.ByMPA[!is.na(Techreport.Trend.ByMPA$MPAID),]

# --- 1.4 MPA-level analysis for CONTROLS ----

#Techreport.ByMPA.control is a dataframe that calculates the percent of respondents in the control 
#settlements for each MPA, for each monitoring year, that gave each response for the proportional data and 
#also calculates the mean and error terms for a few variables
Techreport.ByMPA.control <- HHData %>%
  filter(Treatment==0) %>%
  group_by(MPAID,MonitoringYear) %>%
                        summarise(
                          YrResident=mean(YrResident,na.rm=T),
                                  HHH.female=(length(IndividualGender[IndividualGender==0 &
                                                                            !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
                                      HHH.male=(length(IndividualGender[IndividualGender==1 &
                                                                          !is.na(IndividualGender)])/length(IndividualGender[!is.na(IndividualGender)]))*100,
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
                                      SERate=mean(SERate,na.rm=T),
                                  UnwellMean=mean(DaysUnwell,na.rm=T),
                                  UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)),
                                      TimeMarketMean=mean(TimeMarket,na.rm=T),
                                      TimeMarketErr=sd(TimeMarket,na.rm=T)/sqrt(length(TimeMarket)),
                                  Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==1 | SocialConflict==2) &
                                                                                         !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                                  Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4 | SocialConflict==5) &
                                                                                         !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                                  Percent.NoChange.SocConflict=(length(SocialConflict[SocialConflict==3 &
                                                                                        !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
                                  MTManage=mean(RightsManage,na.rm=T),
                                  MTHarvest=mean(RightsHarvest,na.rm=T),
                                  MTAccess=mean(RightsAccess,na.rm=T),
                                  MTTransfer=mean(RightsTransfer,na.rm=T),
                                  MTExclude=mean(RightsExclude,na.rm=T),
                                  MatAssets.gini=gini(MAIndex),
                                  MAIndex=mean(MAIndex,na.rm=T),
                                  Percent.FoodSecure=(length(HouseholdID[FSIndex>=4.02 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                                  Percent.FoodInsecure.NoHunger=(length(HouseholdID[FSIndex<4.02 & FSIndex>=1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100,
                                  Percent.FoodInsecure.YesHunger=(length(HouseholdID[FSIndex<1.56 & !is.na(FSIndex)])/length(HouseholdID[!is.na(FSIndex)]))*100)

Techreport.ByMPA.control <- Techreport.ByMPA.control[!is.na(Techreport.ByMPA.control$MPAID),]

#Joining each of the techreport dataframes we just created with their respective BigFive data frames
#BigFive data frames contain the primary continuous indicators (food security, material assets, etc)

Techreport.Status.BySett <-left_join(Techreport.Status.BySett,BigFive.SettleGroup)


Techreport.Trend.ByMPA <-left_join(Techreport.Trend.ByMPA,BigFive.MPAGroup)


Techreport.ByMPA.control <-left_join(Techreport.ByMPA.control,BigFive.ControlGroup)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Age and Gender Breakdown by MPA, settlement, and treatment  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ----  2.1 Calculating Age and Gender breakdown by MPA, settlement, and treatment ----

AgeGenderDemos <- left_join(AllData[,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear", "MPAName", "SettlementName","Treatment")],
                            IndDemos[,c("HouseholdID", "IndividualGender", "IndividualAge")])


AgeGender.AvgAge.byMPA <-
  AgeGenderDemos %>%
  filter(Treatment==1)%>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))

AgeGender.AvgAge.bySett <-
  AgeGenderDemos %>%
  filter(Treatment==1) %>%
  group_by(SettlementName,MPAID,MonitoringYear) %>%
  summarise(AvgAge=mean(IndividualAge,na.rm=T))

AgeGender.AvgAge.control <-
  AgeGenderDemos %>%
  filter(Treatment==0) %>%
  group_by(MPAID,MonitoringYear) %>%
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



