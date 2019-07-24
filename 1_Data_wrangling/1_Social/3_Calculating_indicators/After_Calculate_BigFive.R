#
# code: Social MPA Mystery Analysis, Bird's Head Seascape
# 
# author: Amari Bauer
# created: June 2019
# Adapted from New_SBS_MPA_Mystery.R


source("C:/Users/HP/Dropbox/NotThisOne/Source_social_data_flat_files.R")
source("C:/Users/HP/Dropbox/NotThisOne/Calculate_BigFive.R")

library(reldist)

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

#Assignig the MPAID's to their respective MPA name, then joining that with the HHData table
MPANames <- data.frame(MPAID=c(1:6,15:19),
                       MPAName=c("Mayalibit","TNTC","Kaimana","Kofiau","Dampier","Misool","Alor","Flotim","Kei","Koon","Yamdena"))

HHData <- left_join(HHData,
                      MPANames,
                      by="MPAID")

AllData <- HHData

#Subsetting table to just include the household heads, removing NA's in the relation to household head column
HHData <- right_join(HHData,IndDemos[IndDemos$RelationHHH==0 &
            !is.na(IndDemos$RelationHHH),c("HouseholdID","IndividualGender",
            "IndividualEducation","IndividualAge")],by="HouseholdID")

#Making InterviewYear a factor
HHData$InterviewYear <- factor(HHData$InterviewYear,
                                          levels=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),
                                          ordered=T)

#Creating a variable (HHsize) that includes the household size, calculated from the length of the IndDemos householdID column
HHData$HHsize <- sapply(HHData$HouseholdID,
                                   function(i){
                                     c(length(IndDemos$HouseholdID[which(IndDemos$HouseholdID==i)]))
                                   })

#Creating the variable Unwell, that is the average days unwell, on a household basis
HHData<- left_join(HHData,(IndDemos %>%
  group_by(HouseholdID) %>% 
  mutate(DaysUnwell=sum(DaysUnwell,na.rm=T)/length(HouseholdID))))

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
                          length(IndividualAge[!is.na(IndividualAge)]))*-100,
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



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Report Plot Themes, Legends, and Guides  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- 3.1 MPA Technical Report plot themes ----

plot.theme <- theme(axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.x=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.y=element_blank(),
                    plot.margin=margin(t=0,r=20,b=5,l=5,unit="pt"),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                           angle=0,
                                           colour="#303030",
                                           lineheight=0.7),
                    legend.position="top",
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"))

age.gender.plot.theme <- theme(axis.ticks=element_blank(),
                               panel.background=element_rect(fill="white",
                                                             colour="#909090"),
                               panel.border=element_rect(fill=NA,
                                                         size=0.25,
                                                         colour="#C0C0C0"),
                               panel.grid.major.x=element_line(colour="#C0C0C0",
                                                               size=0.25,
                                                               linetype=3),
                               panel.grid.major.y=element_blank(),
                               axis.title=element_text(size=rel(0.8),
                                                       angle=0,
                                                       face="bold",
                                                       colour="#303030"),
                               axis.text=element_text(size=rel(0.6),
                                                      angle=0,
                                                      colour="#303030"),
                               legend.position="top",
                               legend.justification="right",
                               legend.box.spacing=unit(0.1,"cm"))



fillcols.status <- c("NotDummy"=alpha("#2C7FB8",0.95),"Dummy"=alpha("#FFFFFF",0))
fillcols.trend <- c(alpha("#2C7FB8",0.95))

errcols.status <- c("NotDummy"=alpha("#21577C",0.95),"Dummy"=alpha("#FFFFFF",0))
errcols.trend <- c(alpha("#21577C",0.95))

multianswer.fillcols.status <- list(Gender=c("HHH.male"=alpha("#253494",0.95),
                                             "HHH.female"=alpha("#7FCDBB",0.95)),
                                    Religion=c("Percent.Rel.Christian"=alpha("#253494",0.95),
                                               "Percent.Rel.Muslim"=alpha("#7FCDBB",0.95),
                                               "Percent.Rel.Other"=alpha("#FDC086",0.95)),
                                    PrimaryOcc=c("Percent.PrimaryOcc.Farm"=alpha("#A6CEE3",0.95), # light blue 
                                                 "Percent.PrimaryOcc.HarvestForest"=alpha("#7FC97F",0.95), # light green
                                                 "Percent.PrimaryOcc.Fish"=alpha("#1F78B4",0.95), # blue                                                  "Percent.PrimaryOcc.Tourism"=alpha("#C23737",0.95), # red
                                                 "Percent.PrimaryOcc.WageLabor"=alpha("#33A02C",0.95), # green 
                                                 "Percent.PrimaryOcc.Tourism"=alpha("#E31A1C",0.95), # red 
                                                 "Percent.PrimaryOcc.Aquaculture"=alpha("#FDBF6F",0.95), # blue (X)
                                                 "Percent.PrimaryOcc.Extraction"=alpha("#FB9A99",0.95), # light red (X)
                                                 "Percent.PrimaryOcc.Other"=alpha("#FF7F00",0.95)), # orange 
                                    FreqFish=c("Prop.Fish.AlmostNever"=alpha("#E1E198",0.95),
                                               "Prop.Fish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                               "Prop.Fish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                               "Prop.Fish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                               "Prop.Fish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    FreqSellFish=c("Prop.SellFish.AlmostNever"=alpha("#E1E198",0.95),
                                                   "Prop.SellFish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                                   "Prop.SellFish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                                   "Prop.SellFish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                                   "Prop.SellFish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    IncFish=c("Prop.IncFish.None"=alpha("#E1E198",0.95),
                                              "Prop.IncFish.Some"=alpha("#7FCDBB",0.95),
                                              "Prop.IncFish.Half"=alpha("#2CA9B8",0.95),
                                              "Prop.IncFish.Most"=alpha("#2C7FB8",0.95),
                                              "Prop.IncFish.All"=alpha("#253494",0.95)),
                                    FishTech=c("Prop.FishTech.ByHand"=alpha("#7FC97F",0.95),
                                               "Prop.FishTech.StatNet"=alpha("#BEAED4",0.95),
                                               "Prop.FishTech.MobileNet"=alpha("#FDC086",0.95),
                                               "Prop.FishTech.StatLine"=alpha("#E1E198",0.95),
                                               "Prop.FishTech.MobileLine"=alpha("#386CB0",0.95)),
                                    ChildFS=c("Child.FS.no"=alpha("#253494",0.95),
                                              "Child.FS.yes"=alpha("#7FCDBB",0.95)),
                                    Protein=c("ProteinFish.None"=alpha("#E1E198",0.95),
                                              "ProteinFish.Some"=alpha("#7FCDBB",0.95),
                                              "ProteinFish.Half"=alpha("#2CA9B8",0.95),
                                              "ProteinFish.Most"=alpha("#2C7FB8",0.95),
                                              "ProteinFish.All"=alpha("#253494",0.95)),
                                    Member=c("Member.Yes"=alpha("#253494",0.95),
                                             "Member.No"=alpha("#7FCDBB",0.95)),
                                    PropRules=c("PropRuleHab"=alpha("#253494",0.95),
                                                "PropRuleSpp"=alpha("#7FCDBB",0.95)),
                                    Attendance=c("Prop.Member.Yes.Meeting.Yes"=alpha("#253494",0.95),
                                                 "Prop.Member.Yes.Meeting.No"=alpha("#7FCDBB",0.95)),
                                    FSCategorical=c("Percent.FoodInsecure.YesHunger"=alpha("#E1E198",0.95),
                                                    "Percent.FoodInsecure.NoHunger"=alpha("#7FCDBB",0.95),
                                                    "Percent.FoodSecure"=alpha("#253494",0.95)),
                                    Illness=c("Percent.Ill"=alpha("#253494",0.95),
                                              "Percent.Not.Ill"=alpha("#7FCDBB",0.95)),
                                    EconStatus=c("Econ.Status.Much.Worse"=alpha("#E1E198",0.95),
                                                 "Econ.Status.Slighly.Worse"=alpha("#7FCDBB",0.95),
                                                 "Econ.Status.Neutral"=alpha("#2CA9B8",0.95),
                                                 "Econ.Status.Slightly.Better"=alpha("#2C7FB8",0.95),
                                                 "Econ.Status.Much.Better"=alpha("#253494",0.95)),
                                    SocialConflict=c("Percent.GreatlyDecreased.SocConflict"=alpha("#E1E198",0.95),
                                                     "Percent.Decreased.SocConflict"=alpha("#7FCDBB",0.95),
                                                     "Percent.Same.SocConflict"=alpha("#2CA9B8",0.95),
                                                     "Percent.Increased.SocConflict"=alpha("#2C7FB8",0.95),
                                                     "Percent.GreatlyIncreased.SocConflict"=alpha("#253494",0.95)),
                                    NumThreats=c("Threat.None"=alpha("#7FC97F",0.95),
                                                 "Threat.One"=alpha("#BEAED4",0.95),
                                                 "Threat.Two"=alpha("#FDC086",0.95),
                                                 "Threat.Three"=alpha("#FB9A99",0.95),
                                                 "Threat.Four"=alpha("#E1E198",0.95),
                                                 "Threat.Minimum.Five"=alpha("#E31A1C", 0.95)),
                                    ThreatType=c("Pollution"=alpha("#A6CEE3",0.95), # light blue 
                                                 "DestructiveFishing"=alpha("#1F78B4",0.95), # blue
                                                 "IllegalFishing"=alpha("#7FC97F",0.95), # light green
                                                 "ClimateChange"=alpha("#33A02C",0.95), # green
                                                 "HabitatLoss"=alpha("#FB9A99",0.95), # light red
                                                 "NaturalProcesses"=alpha("#E31A1C",0.95), # red 
                                                 "OtherMarineUses"=alpha("#FDBF6F",0.95), # light orange
                                                 "Other"=alpha("#FF7F00",0.95)), # orange 
                                    Participate=c("ParticipateOrg"=alpha("#7FC97F",0.95),
                                                  "ParticipateEstablish"=alpha("#FFFFB3",0.95),
                                                  "ParticipateBnd"=alpha("#2C7FB8",0.95),
                                                  "ParticipateRules"=alpha("#253494",0.95)),
                                    HHHEducation=c("HHHEducNone"=alpha("#7FC97F",0.95),
                                                   "HHHEducPre"=alpha("#FFFFB3",0.95),
                                                   "HHHEducPrim"=alpha("#BEBADA",0.95),
                                                   "HHHEducMid"=alpha("#2C7FB8",0.95),
                                                   "HHHEducSec"=alpha("#253494",0.95),
                                                   "HHHEducHigher"=alpha("#FDB462", 0.95)),
                                    AdultEducation=c("AdultEducNone"=alpha("#7FC97F",0.95),
                                                     "AdultEducPre"=alpha("#FFFFB3",0.95),
                                                     "AdultEducPrim"=alpha("#BEBADA",0.95),
                                                     "AdultEducMid"=alpha("#2C7FB8",0.95),
                                                     "AdultEducSec"=alpha("#253494",0.95),
                                                     "AdultEducHigher"=alpha("#FDB462", 0.95)),
                                    Gender=c("HHH.male"=alpha("#253494",0.95),
                                             "HHH.female"=alpha("#7FCDBB",0.95)),
                                    Religion=c("Percent.Rel.Christian"=alpha("#253494",0.95),
                                               "Percent.Rel.Muslim"=alpha("#7FCDBB",0.95),
                                               "Percent.Rel.Other"=alpha("#FDC086",0.95)),
                                    PrimaryOcc=c("Percent.PrimaryOcc.Farm"=alpha("#7FC97F",0.95),
                                                 "Percent.PrimaryOcc.HarvestForest"=alpha("#BEAED4",0.95),
                                                 "Percent.PrimaryOcc.Fish"=alpha("#FDC086",0.95),
                                                 "Percent.PrimaryOcc.Tourism"=alpha("#E1E198",0.95),
                                                 "Percent.PrimaryOcc.WageLabor"=alpha("#386CB0",0.95),
                                                 "Percent.PrimaryOcc.Other"=alpha("#C23737",0.95)),
                                    FreqFish=c("Prop.Fish.AlmostNever"=alpha("#E1E198",0.95),
                                               "Prop.Fish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                               "Prop.Fish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                               "Prop.Fish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                               "Prop.Fish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    FreqSellFish=c("Prop.SellFish.AlmostNever"=alpha("#E1E198",0.95),
                                                   "Prop.SellFish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                                   "Prop.SellFish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                                   "Prop.SellFish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                                   "Prop.SellFish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    IncFish=c("Prop.IncFish.None"=alpha("#E1E198",0.95),
                                              "Prop.IncFish.Some"=alpha("#7FCDBB",0.95),
                                              "Prop.IncFish.Half"=alpha("#2CA9B8",0.95),
                                              "Prop.IncFish.Most"=alpha("#2C7FB8",0.95),
                                              "Prop.IncFish.All"=alpha("#253494",0.95)),
                                    FishTech=c("Prop.FishTech.ByHand"=alpha("#7FC97F",0.95),
                                               "Prop.FishTech.StatNet"=alpha("#BEAED4",0.95),
                                               "Prop.FishTech.MobileNet"=alpha("#FDC086",0.95),
                                               "Prop.FishTech.StatLine"=alpha("#E1E198",0.95),
                                               "Prop.FishTech.MobileLine"=alpha("#386CB0",0.95)),
                                    ChildFS=c("Child.FS.no"=alpha("#253494",0.95),
                                              "Child.FS.yes"=alpha("#7FCDBB",0.95)),
                                    Protein=c("ProteinFish.None"=alpha("#E1E198",0.95),
                                              "ProteinFish.Some"=alpha("#7FCDBB",0.95),
                                              "ProteinFish.Half"=alpha("#2CA9B8",0.95),
                                              "ProteinFish.Most"=alpha("#2C7FB8",0.95),
                                              "ProteinFish.All"=alpha("#253494",0.95)))





# ---- 3.2 MPA Impact Summary plot themes ----

fill.cols.MPAimpact.summ <- c("MPA"=alpha("#1B448B",0.85),"Control"=alpha("#6B6B6B",0.4))
err.cols.MPAimpact.summ <- c(alpha("#0A1D4E",0.5),alpha("#242424",0.25))

snapshot.plot.theme.MPAimpact.summ <- theme(panel.background=element_blank(),
                                            panel.grid.major.x=element_line(size=0.25,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.major.y=element_line(size=0.5,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.minor.x=element_blank(),
                                            axis.ticks=element_blank(),
                                            axis.text=element_text(size=11,
                                                                   angle=0,
                                                                   colour="#505050",
                                                                   hjust=0.5),
                                            axis.title=element_text(size=12,
                                                                    angle=0,
                                                                    face="bold",
                                                                    colour="#505050"),
                                            plot.title=element_text(colour="#505050",
                                                                    face="bold",
                                                                    size=14),
                                            panel.border=element_rect(size=0.5,
                                                                      colour="#D0D0D0",
                                                                      linetype=3,
                                                                      fill=NA))

plot.theme.MPAimpact.summ <- theme(axis.ticks=element_blank(),
                                   axis.text=element_text(vjust=0.5,
                                                          size=rel(1.1),
                                                          colour="#505050"),
                                   axis.title.y=element_text(face="bold",
                                                             size=rel(1.15),
                                                             angle=90,
                                                             colour="#505050"),
                                   axis.title.x=element_blank(),
                                   plot.title=element_blank(),
                                   panel.background=element_rect(fill="white",
                                                                 colour="#D0D0D0"),
                                   panel.grid.major.x=element_blank(),
                                   panel.grid.major.y=element_line(size=0.5,
                                                                   colour="#808080",
                                                                   linetype=3),
                                   panel.grid.minor.y=element_blank(),
                                   panel.border=element_rect(fill=NA,
                                                             colour="#D0D0D0"))

fs.st.plot.theme.MPAimpact.summ <- theme(axis.ticks.x=element_blank(),
                                         axis.ticks.y=element_line(colour="#505050"),
                                         axis.text=element_text(vjust=0.5,
                                                                size=rel(1.1),
                                                                colour="#505050"),
                                         axis.title.y=element_text(face="bold",
                                                                   size=rel(1.15),
                                                                   angle=90,
                                                                   colour="#505050"),
                                         axis.title.x=element_blank(),
                                         plot.title=element_blank(),
                                         panel.background=element_rect(fill="white",
                                                                       colour="#D0D0D0"),
                                         panel.grid.major.x=element_blank(),
                                         panel.grid.major.y=element_blank(),
                                         panel.grid.minor.y=element_blank(),
                                         panel.border=element_rect(fill=NA,
                                                                   colour="#D0D0D0"))


# ---- 3.3 MPA Technical Report plot legend guide ----


plot.guides.techreport <- guides(alpha=guide_legend(title.hjust=1,
                                                    title.theme=element_text(face="bold",
                                                                             size=rel(9),
                                                                             angle=0,
                                                                             colour="#505050",
                                                                             lineheight=0.75),
                                                    label.vjust=0.5,
                                                    label.theme=element_text(size=rel(8),
                                                                             angle=0,
                                                                             colour="#505050",
                                                                             lineheight=0.75),
                                                    direction="horizontal",
                                                    ncol=3,
                                                    title.position="left",
                                                    label.position="right",
                                                    keywidth=unit(0.75,"cm"),
                                                    keyheight=unit(0.5,"cm")),
                                 fill=guide_legend(title.hjust=1,
                                                   title.theme=element_text(face="bold",
                                                                            size=rel(9),
                                                                            angle=0,
                                                                            colour="#505050",
                                                                            lineheight=0.75),
                                                   label.vjust=0.5,
                                                   label.theme=element_text(size=rel(9),
                                                                            angle=0,
                                                                            colour="#505050",
                                                                            lineheight=0.75),
                                                   direction="horizontal",
                                                   ncol=2,
                                                   title.position="left",
                                                   label.position="right",
                                                   keywidth=unit(0.75,"cm"),
                                                   keyheight=unit(0.5,"cm"),
                                                   reverse=T),
                                 colour=guide_legend(title=element_blank(),
                                                     label.position="bottom",
                                                     label.theme=element_text(size=9,
                                                                              angle=0,
                                                                              colour="#505050"),
                                                     label.hjust=0.5,
                                                     keywidth=unit(2.5,"cm")),
                                 shape=guide_legend(title=element_blank(),
                                                    label.position="right",
                                                    label.theme=element_text(size=9,
                                                                             angle=0,
                                                                             colour="#505050"),
                                                    label.hjust=0.6))

# - Function to create common legend between multiple ggplots
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---- 3.4 MPA Impact Summary plot legend guide ----

plot.guides.MPAimpact.summ <- guides(size=guide_legend(title=element_blank(),
                                                       label.theme=element_text(size=9,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       label.position="bottom",
                                                       keywidth=unit(2.5,"cm"),
                                                       label.hjust=0.5,
                                                       order=2),
                                     fill=guide_legend(title.hjust=0.5,
                                                       title.theme=element_text(face="bold",
                                                                                size=10,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       label.theme=element_text(size=9,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       order=1,
                                                       ncol=2,
                                                       nrow=1,
                                                       title.position="top",
                                                       label.position="bottom",
                                                       keywidth=unit(1.2,"cm"),
                                                       label.hjust=0.5),
                                     colour=guide_legend(title=element_blank(),
                                                         label.position="bottom",
                                                         label.theme=element_text(size=9,
                                                                                  angle=0,
                                                                                  colour="#505050"),
                                                         label.hjust=0.5,
                                                         keywidth=unit(2.5,"cm"),
                                                         order=3),
                                     shape=guide_legend(title=element_blank(),
                                                        label.position="right",
                                                        label.theme=element_text(size=9,
                                                                                 angle=0,
                                                                                 colour="#505050"),
                                                        label.hjust=0.6,
                                                        order=4))

snapshot.plot.guide.MPAimpact.summ <- guides(fill=guide_legend(order=1,
                                                               keywidth=unit(1,"cm"),
                                                               label.theme=element_text(size=9,
                                                                                        angle=0,
                                                                                        colour="#505050"),
                                                               label.position="right",
                                                               label.hjust=0.5,
                                                               label.vjust=0.5,
                                                               title.theme=element_text(size=10,
                                                                                        angle=0,
                                                                                        colour="#505050",
                                                                                        face="bold",
                                                                                        lineheight=0.8),
                                                               title.hjust=0.5),
                                             linetype=guide_legend(order=3,
                                                                   keywidth=unit(2.5,"cm"),
                                                                   title.theme=element_text(size=10,
                                                                                            angle=0,
                                                                                            colour="#505050",
                                                                                            lineheight=0.8,
                                                                                            face="bold"),
                                                                   title.hjust=0.5,
                                                                   label=F))


# ---- 3.5 MPA continuous variables distribution plot theme ----

dist.plot.theme <- theme(axis.ticks=element_blank(),
                         plot.title=element_text(face="bold",
                                                 colour="#303030",
                                                 hjust=0.5),
                         panel.background=element_rect(fill="white",
                                                       colour="#909090"),
                         panel.border=element_rect(fill=NA,
                                                   size=0.25,
                                                   colour="#C0C0C0"),
                         panel.grid.major.x=element_line(colour="#C0C0C0",
                                                         size=0.25),
                         panel.grid.major.y=element_line(colour="#C0C0C0",
                                                         size=0.25,
                                                         linetype=3),
                         axis.title=element_text(size=11,
                                                 angle=0,
                                                 face="bold",
                                                 colour="#303030"),
                         axis.text=element_text(size=10,
                                                angle=0,
                                                colour="#303030"))

# ---- 3.6 MPA Technical Report plot labels ----

Statusplot.labs <- list(FS=labs(y="Mean household food security",x="Settlement"),
                        MA=labs(y="Mean household assets",x="Settlement"),
                        PA=labs(y="Mean place attachment",x="Settlement"),
                        MT=labs(y="Mean household marine tenure",x="Settlement"),
                        SE=labs(y="School enrollment (% children ages 5-18 years old)",x="Settlement"),
                        Time=labs(y="Distance to market (hours)",x="Settlement"),
                        Unwell=labs(y="Mean time suffering from illness or injury in past 4 weeks (days)",
                                    x="Settlement"),
                        Gender=labs(y="Gender (% head of household)",x="Settlement"),
                        Religion=labs(y="Religion (% head of household)",x="Settlement"),
                        PrimaryOcc=labs(y="Primary occupation (% households)",x="Settlement"),
                        FreqFish=labs(y="Frequency of fishing (% households)",x="Settlement"),
                        FreqSellFish=labs(y="Frequency of selling at least some catch (% households)",
                                          x="Settlement"),
                        FishProtein=labs(y="Dietary protein from fish in past 6 months (% households)",
                                         x="Settlement"),
                        IncFish=labs(y="Income from fishing in past 6 months (% households)",
                                     x="Settlement"),
                        FishTech=labs(y="Fishing technique most often used in past 6 months (% households)",
                                      x="Settlement"),
                        ChildFS=labs(y="Child hunger (% households)",x="Settlement"),
                        Ethnicity=labs(y="Mean number of ethnicities (Settlement ethnicity frequency)",x="Settlement"),
                        AdultEduc=labs(y="Education completed (% adults 18 years and older)",x="Settlement"),
                        HHHEduc=labs(y="Education completed (% household heads)",x="Settlement"),
                        EconStatus=labs(y="Change in economic status of fishing households (% households)",x="Settlement"),
                        Conflict=labs(y="Perceive change in level of social conflict over marine resources in past 12 months (% households)",x="Settlement"),
                        NumLocalThreats=labs(y="Percent of respondents identifying threats to marine environement",x="Settlement"),
                        ThreatTypes=labs(y="Percent of local threats to marine environment identified",x="Settlement"),
                        Member=labs(y="Households who are a member of an organization",x="Settlement"),
                        Attendance=labs(y="Households who are a member of an organization and have attended a meeting",x="Settlement"),
                        Contribution=labs(y="Mean household contribution (Indonesian Rupiah)",x="Settlement"),
                        FSCategorical= labs(y="Food security by category (% households)",x="Settlement"),
                        Rules= labs(y="Percent of important species or habitats subject to specific harvest rule",x="Settlement"),
                        Ill = labs(y="Illness or Injury in the past 4 weeks (% individuals)", x="Settlement"),
                        Participation= labs(y="Percent of important user groups participating in marine resource decisions",x="Settlement"))


continuous.variables.plotlabs <- c("Mean household food security","Mean household assets",
                                   "Mean place attachment","Mean household marine tenure",
                                   "School enrollment (% children ages 5-18 years old)", "Distance to market (hours)",
                                   "Mean time suffering from illness or injury in past 4 weeks (days)")


# ---- 3.7 MPA Impact Summary "Big Five" plot labels ----

plot.fs.labs.st <- labs(y="Household Food Security\n ",title="STATUS AND TREND")
plot.ma.labs.st <- labs(y="Household Material Assets\n ",title="STATUS AND TREND")
plot.pa.labs.st <- labs(y="Household Place Attachment\n ",title="STATUS AND TREND")
plot.mt.labs.st <- labs(y="Household Marine Tenure\n ",title="STATUS AND TREND")
plot.se.labs.st <- labs(y="Enrollment Rate\n ",title="STATUS AND TREND")

plot.fs.labs.i <- labs(y="Change in Household Food Security\nsince Baseline",title="IMPACT")
plot.ma.labs.i <- labs(y="Change in Household Material Assets\nsince Baseline",title="IMPACT")
plot.pa.labs.i <- labs(y="Change in Household Place Attachment\nsince Baseline",title="IMPACT")
plot.mt.labs.i <- labs(y="Change in Household Marine Tenure\nsince Baseline",title="IMPACT")
plot.se.labs.i <- labs(y="Change in Enrollment Rate\nsince Baseline",title="IMPACT")

impact.x.labs <- c("MPA\nHouseholds","Control\nHouseholds")


