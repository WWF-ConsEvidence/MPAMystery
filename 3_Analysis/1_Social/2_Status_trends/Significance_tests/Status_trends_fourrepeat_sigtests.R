# 
# code:  Status & Trends Significance Tests, for data with four repeats
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2020
# modified: 
# 
# 
# ---- code sections ----
#  1) Import Data
#  2) Define Lists of Settlements, to be used in functions
#  3) Plot Variable Distributions (to test normality assumption)
#  4) Non-parametric Significance Test Functions (using Mann-Whitney U test)
#  5) Chi-square Tests for Categorical Variables
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Import and Subset Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Subset datasets to only include status year ----

# - "MPA Household Data" dataset -- includes household level data for only treatment households from most recent year
MPA.HHData <- 
  HHData %>%
  filter(Treatment==1 & InterviewYear==status) %>% 
  mutate(SettlementName=factor(SettlementName))


# - "MPA versus Control" dataset -- includes household level data for treatment and control, for most recent year only
MPA.v.Control <- 
  HHData %>%
  filter(InterviewYear==status)


# - "MPA Settlement Means" dataset -- includes settlement level data for only treatment settlements from most recent year
MPA.Sett.Means <-
  if(MPA.name$MPAID==21) {
    Sett.Level.Means.byZone %>% filter(InterviewYear==status)
  } else { Sett.Level.Means %>% filter(Treatment==1 & InterviewYear==status)
  }

# Removing the settlement with an NA in order for function to run
MPA.Sett.Means <- 
  MPA.Sett.Means[!is.na(MPA.Sett.Means$SettlementName),]


# - Frequency tables for chi-square tests

FreqTables <- 
  left_join(HHData,Settlements[,c("SettlementID","Zone")], by= "SettlementID") %>%
  filter(if(MPA.name$MPAID==21) { Zone=="NoTake" } else { Treatment==1 }) %>%
  group_by(MonitoringYear) %>%
  summarise(PrimaryOcc.Fish=length(PrimaryLivelihood[PrimaryLivelihood==3 &  !is.na(PrimaryLivelihood)]),
            PrimaryOcc.Farm=length(PrimaryLivelihood[PrimaryLivelihood==1 & !is.na(PrimaryLivelihood)]),
            PrimaryOcc.WageLabor=length(PrimaryLivelihood[PrimaryLivelihood==7 & !is.na(PrimaryLivelihood)]),
            PrimaryOcc.HarvestForest=length(PrimaryLivelihood[PrimaryLivelihood==2 & !is.na(PrimaryLivelihood)]),
            PrimaryOcc.Tourism=length(PrimaryLivelihood[PrimaryLivelihood==6 & !is.na(PrimaryLivelihood)]),
            PrimaryOcc.Other=length(PrimaryLivelihood[(PrimaryLivelihood==996 | PrimaryLivelihood==4 |  PrimaryLivelihood==5) & !is.na(PrimaryLivelihood)]),
            FreqFish.AlmostNever=length(FreqFish[FreqFish==1 & !is.na(FreqFish)]),
            FreqFish.FewTimesPer6Mo=length(FreqFish[FreqFish==2 & !is.na(FreqFish)]),
            FreqFish.FewTimesPerMo=length(FreqFish[FreqFish==3 & !is.na(FreqFish)]),
            FreqFish.FewTimesPerWk=length(FreqFish[FreqFish==4 & !is.na(FreqFish)]),
            FreqFish.MoreFewTimesWk=length(FreqFish[FreqFish==5 & !is.na(FreqFish)]),
            SellFish.AlmostNever=length(FreqSaleFish[FreqSaleFish==1 & !is.na(FreqSaleFish)]),
            SellFish.FewTimesPer6Mo=length(FreqSaleFish[FreqSaleFish==2 & !is.na(FreqSaleFish)]),
            SellFish.FewTimesPerMo=length(FreqSaleFish[FreqSaleFish==3 & !is.na(FreqSaleFish)]),
            SellFish.FewTimesPerWk=length(FreqSaleFish[FreqSaleFish==4 & !is.na(FreqSaleFish)]),
            SellFish.MoreFewTimesWk=length(FreqSaleFish[FreqSaleFish==5 & !is.na(FreqSaleFish)]),
            IncFish.None=length(PercentIncFish[PercentIncFish==1 & !is.na(PercentIncFish)]),
            IncFish.Some=length(PercentIncFish[PercentIncFish==2 & !is.na(PercentIncFish)]),
            IncFish.Half=length(PercentIncFish[PercentIncFish==3 & !is.na(PercentIncFish)]),
            IncFish.Most=length(PercentIncFish[PercentIncFish==4 & !is.na(PercentIncFish)]),
            IncFish.All=length(PercentIncFish[PercentIncFish==5 & !is.na(PercentIncFish)]),
            FishTech.ByHand=length(MajFishTechnique[MajFishTechnique==1 & !is.na(MajFishTechnique)]),
            FishTech.StatNet=length(MajFishTechnique[MajFishTechnique==2 & !is.na(MajFishTechnique)]),
            FishTech.MobileNet=length(MajFishTechnique[MajFishTechnique==3 & !is.na(MajFishTechnique)]),
            FishTech.StatLine=length(MajFishTechnique[MajFishTechnique==4 & !is.na(MajFishTechnique)]),
            FishTech.MobileLine=length(MajFishTechnique[MajFishTechnique==5 & !is.na(MajFishTechnique)]),
            child.FS.no=length(cat.cFS[cat.cFS=="No or insufficient evidence" & !is.na(cat.cFS)]),
            child.FS.yes=length(cat.cFS[cat.cFS=="Evidence" & !is.na(cat.cFS)]),
            ProteinFish.None=length(PercentProteinFish[PercentProteinFish==1 & !is.na(PercentProteinFish)]),
            ProteinFish.Some=length(PercentProteinFish[PercentProteinFish==2 & !is.na(PercentProteinFish)]),
            ProteinFish.Half=length(PercentProteinFish[PercentProteinFish==3 & !is.na(PercentProteinFish)]),
            ProteinFish.Most=length(PercentProteinFish[PercentProteinFish==4 & !is.na(PercentProteinFish)]),
            ProteinFish.All=length(PercentProteinFish[PercentProteinFish==5 &  !is.na(PercentProteinFish)]),
            Econ.Status.Much.Worse=length(EconStatusTrend[EconStatusTrend==1 & !is.na(EconStatusTrend)]),
            Econ.Status.Slighly.Worse=length(EconStatusTrend[EconStatusTrend==2 & !is.na(EconStatusTrend)]),
            Econ.Status.Neutral=length(EconStatusTrend[EconStatusTrend==3 & !is.na(EconStatusTrend)]),
            Econ.Status.Slightly.Better=length(EconStatusTrend[EconStatusTrend==4 & !is.na(EconStatusTrend)]),
            Econ.Status.Much.Better=length(EconStatusTrend[EconStatusTrend==5 & !is.na(EconStatusTrend)]),
            Threat.None=length(NumLocalThreat[NumLocalThreat==0 & !is.na(NumLocalThreat)]),
            Threat.One=length(NumLocalThreat[NumLocalThreat==1 & !is.na(NumLocalThreat)]),
            Threat.Two=length(NumLocalThreat[NumLocalThreat==2 & !is.na(NumLocalThreat)]),
            Threat.Three=length(NumLocalThreat[NumLocalThreat==3 & !is.na(NumLocalThreat)]),
            Threat.Four=length(NumLocalThreat[NumLocalThreat==4 & !is.na(NumLocalThreat)]),
            Threat.Minimum.Five =length(NumLocalThreat[NumLocalThreat>=5 & !is.na(NumLocalThreat)]),
            Percent.SecondaryOcc.Fish=length(SecondaryLivelihood[SecondaryLivelihood==3 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.Farm=length(SecondaryLivelihood[SecondaryLivelihood==1 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.WageLabor=length(SecondaryLivelihood[SecondaryLivelihood==7 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.HarvestForest=length(SecondaryLivelihood[SecondaryLivelihood==2 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.Tourism=length(SecondaryLivelihood[SecondaryLivelihood==6 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.Aquaculture=length(SecondaryLivelihood[SecondaryLivelihood==4 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.Extraction=length(SecondaryLivelihood[SecondaryLivelihood==5 & !is.na(SecondaryLivelihood)]),
            Percent.SecondaryOcc.Other=length(SecondaryLivelihood[SecondaryLivelihood==996 & !is.na(SecondaryLivelihood)]),
            Percent.OneOcc.Diverse=length(HouseholdID[!is.na(PrimaryLivelihood) & is.na(SecondaryLivelihood) & is.na(TertiaryLivelihood)]),
            Percent.MultipleOcc.Diverse=length(HouseholdID[!is.na(PrimaryLivelihood) & (!is.na(SecondaryLivelihood) | !is.na(TertiaryLivelihood))]))

FreqTables <- 
  as.data.frame(t(FreqTables[,-1]))

colnames(FreqTables) <- c("t0","repeat1","repeat2","repeat3","repeat4")

FreqTables$Category <- rownames(FreqTables)
FreqTables$Variable <- ifelse(grepl("PrimaryOcc",FreqTables$Category)==T,"PrimaryOcc",
                              ifelse(grepl("SellFish",FreqTables$Category)==T,"SellFish",
                                     ifelse(grepl("IncFish",FreqTables$Category)==T,"IncFish",
                                            ifelse(grepl("FishTech",FreqTables$Category)==T,"FishTech",
                                                   ifelse(grepl("FreqFish",FreqTables$Category)==T,"FreqFish",
                                                          ifelse(grepl("child",FreqTables$Category)==T,"child",
                                                                 ifelse(grepl("Protein",FreqTables$Category)==T,"Protein",
                                                                        ifelse(grepl("Econ.Status",FreqTables$Category)==T,"EconStatus",
                                                                               ifelse(grepl("Threat",FreqTables$Category)==T,"NumLocalThreats",
                                                                                      ifelse(grepl("SecondaryOcc",FreqTables$Category)==T,"SecondaryOcc",
                                                                                             ifelse(grepl("Occ.Diverse",FreqTables$Category)==T,"OccDiverse",NA)))))))))))


# ---- 1.2 Define list of settlement names in MPA ----

sett.names <- factor(MPA.Sett.Means$SettlementName)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function <- 
  mapply(a=MPA.Sett.Means[,c("FSMean","MAMean","MTMean","PAMean","SEMean","TimeMarketMean","UnwellMean")],
         b=MPA.HHData[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           sett.equal.med <- c(MPA.Sett.Means$SettlementName[which(a==med)])
           upper.sett <- MPA.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)==2,
                                ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)==3,
                                       ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       ifelse(length(upper.sett)>3,
                                              as.character(upper.sett[1]),
                                              as.character(upper.sett))))
           lower.sett <- MPA.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)==2,
                                ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)==3,
                                       ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       ifelse(length(lower.sett)>3,
                                              as.character(lower.sett[1]),
                                              as.character(lower.sett))))
           sett.equal.med <- ifelse(length(sett.equal.med)==2,
                                    ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)==3,
                                           ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[MPA.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[MPA.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(length(sett.equal.med)>3,
                                                  as.character(sett.equal.med[1]),
                                                  ifelse(is.na(sett.equal.med),
                                                         NA,
                                                         as.character(sett.equal.med)))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[MPA.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[MPA.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts <- 
  mapply(i=MPA.Sett.Means[,c("FSMean","MAMean","MTMean","PAMean","SEMean","TimeMarketMean","UnwellMean")],
         j=names(even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names)%%2!=0,
                                      as.character(MPA.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function[j])),
                               levels=levels(sett.names))})
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# # ---- 3.1 Food security score distribution ----
# 
# dist.FS <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=FSIndex,y=..density..),
#                  bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$FSIndex),
#                                     sd=sd(MPA.HHData$FSIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Food Security Scores\n(per household)",
#        y="Density",
#        title=paste("Food Security Score Distribution",unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.FS
# 
# qqnorm(MPA.HHData$FSIndex)
# qqline(MPA.HHData$FSIndex,col="green")
# 
# 
# # ---- 3.2 Material assets score distribution ----
# 
# dist.MA <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=MAIndex,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$MAIndex),
#                                     sd=sd(MPA.HHData$MAIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Material Assets\n(per household)",
#        y="Density",
#        title=paste("HH Material Assets Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.MA
# 
# log.MA <- log(MPA.HHData$MAIndex[MPA.HHData$MAIndex!=0])
# 
# qqnorm(log.MA)
# qqline(log.MA,col="green")
# 
# 
# # ---- 3.3 Place attachment score distribution ----
# 
# dist.PA <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=PAIndex,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$PAIndex),
#                                     sd=sd(MPA.HHData$PAIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Place Attachment Score\nPer Household",
#        y="Density",
#        title=paste("Place Attachment Score Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.PA
# 
# qqnorm(MPA.HHData$PAIndex)
# qqline(MPA.HHData$PAIndex,col="green")
# 
# 
# # ---- 3.4 Marine tenure score distribution ----
# dist.MT <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=MTIndex,y=..density..),
#                  binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$MTIndex),
#                                     sd=sd(MPA.HHData$MTIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Marine Tenure Score\n(per household)",
#        y="Density",
#        title=paste("Marine Tenure Score Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.MT
# 
# qqnorm(MPA.HHData$MTIndex)
# qqline(MPA.HHData$MTIndex,col="green")
# 
# 
# # ---- 3.5 School enrollment rate distribution ----
# 
# dist.SE <-
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=SERate,y=..density..),
#                  bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$SERate),
#                                     sd=sd(MPA.HHData$SERate)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="School Enrollment Rate\n(per household)",
#        y="Density",
#        title=paste("School Enrollment Rate Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.SE
# 
# qqnorm(MPA.HHData$SERate)
# qqline(MPA.HHData$SERate,col="green")
# 
# 
# # ---- 3.6 Time to market distribution ----
# 
# dist.TimeMarket <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=TimeMarket,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$TimeMarket),
#                                     sd=sd(MPA.HHData$TimeMarket)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Average Time to Market\n(in hours)",
#        y="Density",
#        title=paste("Time to Market Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.TimeMarket
# 
# qqnorm(MPA.HHData$TimeMarket)
# qqline(MPA.HHData$TimeMarket,col="green")
# 
# 
# # ---- 3.7 Days unwell distribution ----
# 
# dist.DaysUnwell <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=DaysUnwell,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$DaysUnwell),
#                                     sd=sd(MPA.HHData$DaysUnwell)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Days Unwell\n(per household)",
#        y="Density",
#        title=paste("HH Days Unwell Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.DaysUnwell
# 
# qqnorm(MPA.HHData$DaysUnwell)
# qqline(MPA.HHData$DaysUnwell,col="green")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Non-parametric Significance Test Functions (using Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# NOTE: Typically, food security, material assets, marine tenure, enrollment rate, and place attachment end up being non-parametric; 
#       however, if the distribution appears to be normal, then PARAMETRIC tests are more powerful and the better choice.


# ---- 4.1 Create function that will output significance values for non-parametric variables, BY SETTLEMENT ----
#          (for status plots)
non.parametric.test.settlements <- 
  data.frame(mapply(a=c("FSMean","MAMean","MTMean","PAMean","SEMean","TimeMarketMean","UnwellMean"),
                    b=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a,b){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names[which(sett.names!=median.setts[a])]),
                                                               as.character(median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names[which(sett.names!=median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       MPA.HHData[MPA.HHData$SettlementName==i | MPA.HHData$SettlementName==median.setts[a],b]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=MPA.HHData[MPA.HHData$SettlementName==i | MPA.HHData$SettlementName==median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett  <- 
  cbind.data.frame(non.parametric.test.settlements[order(non.parametric.test.settlements$"FSMean.SettlementName"),
                                                   c("FSMean.SettlementName","FSMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"MAMean.SettlementName"),
                                                   c("MAMean.SettlementName","MAMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"MTMean.SettlementName"),
                                                   c("MTMean.SettlementName","MTMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"PAMean.SettlementName"),
                                                   c("PAMean.SettlementName","PAMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"SEMean.SettlementName"),
                                                   c("SEMean.SettlementName","SEMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"TimeMarketMean.SettlementName"),
                                                   c("TimeMarketMean.SettlementName","TimeMarketMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"UnwellMean.SettlementName"),
                                                   c("UnwellMean.SettlementName","UnwellMean.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett <- 
  sigvals.Sett %>%
  select(FSMean.SettlementName, FSMean.p.value, MAMean.p.value, MTMean.p.value, 
         PAMean.p.value, SEMean.p.value, TimeMarketMean.p.value, UnwellMean.p.value) %>%
  rename(SettlementName = FSMean.SettlementName, FS.pval = FSMean.p.value, MA.pval = MAMean.p.value, MT.pval = MTMean.p.value,
         PA.pval = PAMean.p.value, SE.pval = SEMean.p.value, TimeMarket.pval = TimeMarketMean.p.value, Unwell.pval = UnwellMean.p.value) %>%
  .[rev(order(.$SettlementName)),]


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl  <-
  if(MPA.name$MPAID==21) {
    data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                      function(a){
                        var <- MPA.v.Control[,a]
                        wilcox.test(var~Zone,
                                    data=MPA.v.Control,
                                    exact=F)}))["p.value",]
  } else {
    data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                      function(a){
                        var <- MPA.v.Control[,a]
                        wilcox.test(var~Treatment,
                                    data=MPA.v.Control,
                                    exact=F)}))["p.value",]
  }

sigvals.MPA  <- 
  if(MPA.name$MPAID==21) {
    cbind.data.frame("No Take Settlements",non.parametric.test.MPAvControl)
  } else {
    cbind.data.frame(MPA.name$MPAName,non.parametric.test.MPAvControl)
  } 

colnames(sigvals.MPA) <- colnames(sigvals.Sett)

null.row.sigvals  <- 
  matrix(rep(NA,length(sigvals.MPA)),ncol=length(sigvals.MPA),
         dimnames=list(1,colnames(sigvals.Sett)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)

sigvals <- rbind.data.frame(sigvals.MPA,
                            null.row.sigvals,
                            sigvals.Sett)


sigvals[,c("FS.pval", "MA.pval", "MT.pval" , "PA.pval", "SE.pval", "TimeMarket.pval", "Unwell.pval")] <- 
  unlist(sigvals[,c("FS.pval", "MA.pval", "MT.pval", "PA.pval","SE.pval","TimeMarket.pval","Unwell.pval")])

# ---- 4.3 Define function for trend data significance ---- 

trend.non.parametric.test.byMPA  <- 
  if(MPA.name$MPAID==21) {
    data.frame(mapply(i=left_join(HHData,SETTLEMENT[,c("SettlementID","Zone")], by="SettlementID") %>% filter(Zone=="No Take") %>% select(FSIndex,MAIndex,MTIndex,PAIndex,SERate,TimeMarket,DaysUnwell),
                      function(i){
                        MannKendall(c(i[HHData$InterviewYear==unique(HHData$InterviewYear)[1]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[2]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[3]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[4]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[5]]))
                      }))  %>%
      rename(FS.pval = FSIndex, MA.pval = MAIndex, MT.pval = MTIndex, PA.pval = PAIndex, SE.pval = SERate, 
             TimeMarket.pval = TimeMarket, Unwell.pval = DaysUnwell) 
  } else {
    data.frame(mapply(i=HHData[HHData$Treatment==1,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
                      function(i){
                        MannKendall(c(i[HHData$InterviewYear==unique(HHData$InterviewYear)[1]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[2]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[3]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[4]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[5]]))
                      }))  %>%
      rename(FS.pval = FSIndex, MA.pval = MAIndex, MT.pval = MTIndex, PA.pval = PAIndex, SE.pval = SERate, 
             TimeMarket.pval = TimeMarket, Unwell.pval = DaysUnwell) 
  }

trend.non.parametric.test.byControl  <- 
  if(MPA.name$MPAID==21) {
    data.frame(mapply(i=left_join(HHData,SETTLEMENT[,c("SettlementID","Zone")], by="SettlementID") %>% filter(Zone=="Take") %>% select(FSIndex,MAIndex,MTIndex,PAIndex,SERate,TimeMarket,DaysUnwell),
                      function(i){
                        MannKendall(c(i[HHData$InterviewYear==unique(HHData$InterviewYear)[1]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[2]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[3]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[4]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[5]]))
                      }))  %>%
      rename(FS.pval = FSIndex, MA.pval = MAIndex, MT.pval = MTIndex, PA.pval = PAIndex, SE.pval = SERate, 
             TimeMarket.pval = TimeMarket, Unwell.pval = DaysUnwell) 
  } else {
    data.frame(mapply(i=HHData[HHData$Treatment==0,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
                      function(i){
                        MannKendall(c(i[HHData$InterviewYear==unique(HHData$InterviewYear)[1]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[2]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[3]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[4]],
                                      i[HHData$InterviewYear==unique(HHData$InterviewYear)[5]]))
                      }))  %>%
      rename(FS.pval = FSIndex, MA.pval = MAIndex, MT.pval = MTIndex, PA.pval = PAIndex, SE.pval = SERate, 
             TimeMarket.pval = TimeMarket, Unwell.pval = DaysUnwell) 
  }



trend.sigvals  <- 
  cbind.data.frame(trend.non.parametric.test.byMPA ["sl","FS.pval"],NA,trend.non.parametric.test.byMPA ["sl","MA.pval"],
                   NA,trend.non.parametric.test.byMPA ["sl","MT.pval"],NA,trend.non.parametric.test.byMPA ["sl","PA.pval"],NA,
                   trend.non.parametric.test.byMPA ["sl","SE.pval"],NA,trend.non.parametric.test.byMPA ["sl","TimeMarket.pval"],NA,
                   trend.non.parametric.test.byMPA ["sl","Unwell.pval"],NA)

colnames(trend.sigvals ) <- c("FSMean","FSErr","MAMean","MAErr","MTMean","MTErr","PAMean","PAErr","SEMean","SEErr",
                              "TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")

trend.sigvals  <- cbind.data.frame(MonitoringYear="p.value", SettlementName=NA, as.data.frame(t(unlist(trend.sigvals))))


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett  <- 
  cbind.data.frame(SettlementName=as.character(sett.names),
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names),
                                                function(i){
                                                  MannKendall(c(HHData[HHData$SettlementName==i &
                                                                         HHData$InterviewYear==unique(HHData$InterviewYear)[1],a],
                                                                HHData[HHData$SettlementName==i &
                                                                         HHData$InterviewYear==unique(HHData$InterviewYear)[2],a],
                                                                HHData[HHData$SettlementName==i &
                                                                         HHData$InterviewYear==unique(HHData$InterviewYear)[3],a],
                                                                HHData[HHData$SettlementName==i &
                                                                         HHData$InterviewYear==unique(HHData$InterviewYear)[4],a],
                                                                HHData[HHData$SettlementName==i &
                                                                         HHData$InterviewYear==unique(HHData$InterviewYear)[5],a]))
                                                }))["sl",])})) %>%
  rename(FS.pval = FSIndex, MA.pval = MAIndex, MT.pval = MTIndex, PA.pval = PAIndex, SE.pval = SERate, 
         TimeMarket.pval = TimeMarket, Unwell.pval = DaysUnwell) %>%
  .[rev(order(.$SettlementName)),]



# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals  <- 
  rbind.data.frame(cbind.data.frame(SettlementName=if(MPA.name$MPAID==21) { "Use Settlements" } else { c("Control Settlements") },
                                    trend.non.parametric.test.byControl ["sl",]),
                   cbind.data.frame(SettlementName=if(MPA.name$MPAID==21) { sett.names.bahasa[["NoTake"]]} else { MPA.name$MPAName },
                                    trend.non.parametric.test.byMPA ["sl",]),
                   null.row.sigvals,
                   trend.non.parametric.test.bySett[rev(order(trend.non.parametric.test.bySett$SettlementName)),])

annex.sigvals[2:8] <- unlist(annex.sigvals[2:8])

annex.sigvals.bahasa  <- 
  rbind.data.frame(cbind.data.frame(SettlementName=if(MPA.name$MPAID==21) { sett.names.bahasa[["Use"]] } else { sett.names.bahasa[["Control"]] },
                                    trend.non.parametric.test.byControl ["sl",]),
                   cbind.data.frame(SettlementName=if(MPA.name$MPAID==21) { sett.names.bahasa[["NoTake"]] } else { MPA.name$MPAName.bahasa },
                                    trend.non.parametric.test.byMPA ["sl",]),
                   null.row.sigvals,
                   trend.non.parametric.test.bySett[rev(order(trend.non.parametric.test.bySett$SettlementName)),])

annex.sigvals.bahasa[2:8] <- unlist(annex.sigvals.bahasa[2:8])



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: Chi-square Tests for Categorical Variables ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- 5.1 Status plot, settlement-level chi-square tests ----
#          (with MPA-level proportions serving as expected values/probabilities)

# !!! Have not figured out a viable statistical test for settlement-level variation from MPA-level proportions
#     BECAUSE, sample sizes are too small (and many times zero) per category, at the settlement level


# ---- 5.2 Trend plot, chi-square tests on most recent monitoring year ----
#          (with baseline proportions serving as expected values/probabilities)

propdata.trend.test  <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA,
                                   EconStatus=NA,NumLocalThreats=NA,SecondaryOcc=NA,OccDiverse=NA)
p.for.function <- NA
data.for.function <- NA


propdata.trend.test  <- 
  if(sum(FreqTables$repeat2[FreqTables$Variable=="IncFish"])==0){
    as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","FishTech","child","Protein","EconStatus","NumLocalThreats","SecondaryOcc","OccDiverse"),
                         function(a) {
                           p.for.function <- 
                             if(sum(FreqTables$t0[FreqTables$Variable==a])==0) {
                               FreqTables$repeat4[FreqTables$Variable==a &
                                                    FreqTables$repeat4!=0] 
                             } else {FreqTables$t0[FreqTables$Variable==a &
                                                     FreqTables$t0!=0] }
                           data.for.function <- 
                             if(sum(FreqTables$t0[FreqTables$Variable==a])==0) {
                               FreqTables$repeat4[FreqTables$Variable==a]
                             } else {FreqTables$repeat4[FreqTables$Variable==a &
                                                          FreqTables$t0!=0]}
                           propdata.trend.test [a] <- ifelse(length(data.for.function)>1,
                                                             chisq.test(data.for.function,
                                                                        p=p.for.function,
                                                                        rescale.p=TRUE,correct=TRUE)["p.value"],
                                                             NA)
                           propdata.trend.test [a] <- ifelse(is.na(propdata.trend.test [a]),100,propdata.trend.test [a])
                         })) %>%
      mutate(IncFish=1) %>%
      select(PrimaryOcc,FreqFish,SellFish,IncFish,FishTech,child,Protein,EconStatus,NumLocalThreats,SecondaryOcc,OccDiverse)
  } else{as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","child","Protein","EconStatus","NumLocalThreats","SecondaryOcc","OccDiverse"),
                              function(a) {
                                p.for.function <- 
                                  if(sum(FreqTables$t0[FreqTables$Variable==a])==0) {
                                    FreqTables$repeat4[FreqTables$Variable==a &
                                                         FreqTables$repeat4!=0] 
                                  } else {FreqTables$t0[FreqTables$Variable==a &
                                                          FreqTables$t0!=0] }
                                data.for.function <- 
                                  if(sum(FreqTables$t0[FreqTables$Variable==a])==0) {
                                    FreqTables$repeat4[FreqTables$Variable==a]
                                  } else {FreqTables$repeat4[FreqTables$Variable==a &
                                                               FreqTables$t0!=0]}
                                propdata.trend.test [a] <- ifelse(length(data.for.function)>1,
                                                                  chisq.test(data.for.function,
                                                                             p=p.for.function,
                                                                             rescale.p=TRUE,correct=TRUE)["p.value"],
                                                                  NA)
                                propdata.trend.test [a] <- ifelse(is.na(propdata.trend.test [a]),100,propdata.trend.test [a])
                              }))}

propdata.trend.test <-
  rbind(propdata.trend.test, 
        prop.variables.labs,
        prop.variables.labs.bahasa)
