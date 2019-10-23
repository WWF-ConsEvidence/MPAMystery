# 
# code:  Kaimana Technical Report Significance Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/BHS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: December 2017
# 
# 
# ---- inputs ----
#  Dependencies: BHS_MPA_Mystery.R 
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
# ---- SECTION 1: Import Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Subset datasets from BHS_MPA_Mystery.R ----

MPA.TechReport.SigTest.Data <- 
  left_join(BigFive[,c(1,2,4:11,14)],
            HHLivelihood[,c(1,15)],
            by="HouseholdID")

# - "MPA Household Data" dataset
Kai.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                          MPA.TechReport.SigTest.Data$MPAID==3,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==3 &
                                    Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Kai.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Kai.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus BHS" dataset
Kai.TechReport.MPAvBHS <- 
  rbind.data.frame(cbind.data.frame(left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==3 &
                                                                            MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post" &
                                                                            MPA.TechReport.SigTest.Data$Treatment==1,],
                                              Days.unwell[Days.unwell$MPAID==3 &
                                                            Days.unwell$MonitoringYear=="4 Year Post" &
                                                            Days.unwell$Treatment==1,
                                                          c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID"),MPAvBHS="MPA"),
                   cbind.data.frame(left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post" &
                                                                            MPA.TechReport.SigTest.Data$Treatment==1,],
                                              Days.unwell[Days.unwell$MonitoringYear=="4 Year Post" & Days.unwell$Treatment==1,
                                                          c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID"),MPAvBHS="BHS"))

# - "Settlement Means" dataset
Kai.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==3,],
            Techreport.BySett[Techreport.BySett$MPAID==3 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c("SettlementID","SettlementName","TimeMarketMean")],  
            by=c("SettlementID","SettlementName"))

Kai.TechReport.SettlementMeans <- 
  left_join(Kai.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==3 &
                                 Days.unwell.BySett$MonitoringYear=="4 Year Post",c(1,4)],
            by="SettlementID")

Kai.TechReport.SettlementMeans <- 
  Kai.TechReport.SettlementMeans[!is.na(Kai.TechReport.SettlementMeans$SettlementName),]

colnames(Kai.TechReport.SettlementMeans) <- c(colnames(Kai.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")

# - "Trend" dataset
Kai.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==3,],
            Days.unwell[Days.unwell$MPAID==3 &
                          Days.unwell$Treatment==1,1:2],
            by="HouseholdID") 


# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Kai <- factor(Kai.TechReport.SettlementMeans$SettlementName)


# ---- 1.3 Subset categorical variable frequency tables from BHS_MPA_Mystery.R ----

# - "Trend" dataset
Kai.FreqTables <- 
  HHDemos.context[HHDemos.context$MPAID==3,] %>%
  group_by(MonitoringYear) %>%
  summarise(PrimaryOcc.Fish=length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==3 &
                                                            !is.na(PrimaryLivelihoodClean)]),
            PrimaryOcc.Farm=length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==1 &
                                                            !is.na(PrimaryLivelihoodClean)]),
            PrimaryOcc.WageLabor=length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==7 &
                                                                 !is.na(PrimaryLivelihoodClean)]),
            PrimaryOcc.HarvestForest=length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==2 &
                                                                     !is.na(PrimaryLivelihoodClean)]),
            PrimaryOcc.Tourism=length(PrimaryLivelihoodClean[PrimaryLivelihoodClean==6 &
                                                               !is.na(PrimaryLivelihoodClean)]),
            PrimaryOcc.Other=length(PrimaryLivelihoodClean[(PrimaryLivelihoodClean==996 | PrimaryLivelihoodClean==4 | 
                                                              PrimaryLivelihoodClean==5) & !is.na(PrimaryLivelihoodClean)]),
            FreqFish.AlmostNever=length(FreqFishClean[FreqFishClean==1 & !is.na(FreqFishClean)]),
            FreqFish.FewTimesPer6Mo=length(FreqFishClean[FreqFishClean==2 & !is.na(FreqFishClean)]),
            FreqFish.FewTimesPerMo=length(FreqFishClean[FreqFishClean==3 & !is.na(FreqFishClean)]),
            FreqFish.FewTimesPerWk=length(FreqFishClean[FreqFishClean==4 & !is.na(FreqFishClean)]),
            FreqFish.MoreFewTimesWk=length(FreqFishClean[FreqFishClean==5 & !is.na(FreqFishClean)]),
            SellFish.AlmostNever=length(FreqSaleFishClean[FreqSaleFishClean==1 & !is.na(FreqSaleFishClean)]),
            SellFish.FewTimesPer6Mo=length(FreqSaleFishClean[FreqSaleFishClean==2 & !is.na(FreqSaleFishClean)]),
            SellFish.FewTimesPerMo=length(FreqSaleFishClean[FreqSaleFishClean==3 & !is.na(FreqSaleFishClean)]),
            SellFish.FewTimesPerWk=length(FreqSaleFishClean[FreqSaleFishClean==4 & !is.na(FreqSaleFishClean)]),
            SellFish.MoreFewTimesWk=length(FreqSaleFishClean[FreqSaleFishClean==5 & !is.na(FreqSaleFishClean)]),
            IncFish.None=length(PercentIncFishClean[PercentIncFishClean==1 & !is.na(PercentIncFishClean)]),
            IncFish.Some=length(PercentIncFishClean[PercentIncFishClean==2 & !is.na(PercentIncFishClean)]),
            IncFish.Half=length(PercentIncFishClean[PercentIncFishClean==3 & !is.na(PercentIncFishClean)]),
            IncFish.Most=length(PercentIncFishClean[PercentIncFishClean==4 & !is.na(PercentIncFishClean)]),
            IncFish.All=length(PercentIncFishClean[PercentIncFishClean==5 & !is.na(PercentIncFishClean)]),
            FishTech.ByHand=length(MajFishTechniqueClean[MajFishTechniqueClean==1 & !is.na(MajFishTechniqueClean)]),
            FishTech.StatNet=length(MajFishTechniqueClean[MajFishTechniqueClean==2 & !is.na(MajFishTechniqueClean)]),
            FishTech.MobileNet=length(MajFishTechniqueClean[MajFishTechniqueClean==3 & !is.na(MajFishTechniqueClean)]),
            FishTech.StatLine=length(MajFishTechniqueClean[MajFishTechniqueClean==4 & !is.na(MajFishTechniqueClean)]),
            FishTech.MobileLine=length(MajFishTechniqueClean[MajFishTechniqueClean==5 & !is.na(MajFishTechniqueClean)]),
            Child.FS.no=length(Child.FS.category[Child.FS.category=="No or insufficient evidence" & !is.na(Child.FS.category)]),
            Child.FS.yes=length(Child.FS.category[Child.FS.category=="Evidence" & !is.na(Child.FS.category)]),
            ProteinFish.None=length(PercentProteinFishClean[PercentProteinFishClean==1 & !is.na(PercentProteinFishClean)]),
            ProteinFish.Some=length(PercentProteinFishClean[PercentProteinFishClean==2 & !is.na(PercentProteinFishClean)]),
            ProteinFish.Half=length(PercentProteinFishClean[PercentProteinFishClean==3 & !is.na(PercentProteinFishClean)]),
            ProteinFish.Most=length(PercentProteinFishClean[PercentProteinFishClean==4 & !is.na(PercentProteinFishClean)]),
            ProteinFish.All=length(PercentProteinFishClean[PercentProteinFishClean==5 &  !is.na(PercentProteinFishClean)]))

Kai.FreqTables <- 
  as.data.frame(t(Kai.FreqTables[,-1]))
colnames(Kai.FreqTables) <- c("t0","t2","t4")

Kai.FreqTables$Category <- rownames(Kai.FreqTables)
Kai.FreqTables$Variable <- ifelse(grepl("PrimaryOcc",Kai.FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",Kai.FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",Kai.FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",Kai.FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",Kai.FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("Child",Kai.FreqTables$Category)==T,"ChildFS",
                                                                      ifelse(grepl("Protein",Kai.FreqTables$Category)==T,"Protein",NA)))))))

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Kai <- 
  mapply(a=Kai.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Kai.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Kai.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Kai.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Kai.TechReport.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Kai.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.Kai <- 
  mapply(i=Kai.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.Kai),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Kai)%%2!=0,
                                      as.character(Kai.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Kai[j])),
                               levels=levels(sett.names.Kai))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.Kai.FS <- 
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Kai: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Kai.TechReport.MPAHouseholdData$FSIndex)
qqline(Kai.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Kai.MA <- 
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Kai: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(Kai.TechReport.MPAHouseholdData$MAIndex[Kai.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Kai.PA <- 
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Kai: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Kai.TechReport.MPAHouseholdData$PAIndex)
qqline(Kai.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Kai.MT <- 
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Kai: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Kai.TechReport.MPAHouseholdData$MTIndex)
qqline(Kai.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Kai.SE <-
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Kai: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(Kai.TechReport.MPAHouseholdData$SERate)
qqline(Kai.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Kai.TimeMarket <- 
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Kai: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(Kai.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(Kai.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Kai.DaysUnwell <- 
  ggplot(Kai.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kai.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Kai.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Kai: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(Kai.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Kai.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.Kai <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Kai[which(sett.names.Kai!=median.setts.Kai[a])]),
                                                               as.character(median.setts.Kai[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Kai[which(sett.names.Kai!=median.setts.Kai[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Kai.TechReport.MPAHouseholdData[Kai.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Kai.TechReport.MPAHouseholdData$SettlementName==median.setts.Kai[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Kai.TechReport.MPAHouseholdData[Kai.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Kai.TechReport.MPAHouseholdData$SettlementName==median.setts.Kai[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Kai <- 
  cbind.data.frame(non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.Kai[order(non.parametric.test.settlements.Kai$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Kai <- 
  sigvals.Sett.Kai[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Kai) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. BHS ----
#          (for status plots, comparing MPA households to all BHS households)

non.parametric.test.MPAvBHS.Kai <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Kai.TechReport.MPAvBHS[,a]
                      wilcox.test(var~MPAvBHS,
                                  data=Kai.TechReport.MPAvBHS,
                                  exact=F)}))["p.value",]

sigvals.MPA.Kai <- 
  cbind.data.frame("Kaimana MPA",non.parametric.test.MPAvBHS.Kai)

colnames(sigvals.MPA.Kai) <- colnames(sigvals.Sett.Kai)

null.row.sigvals.Kai <- 
  matrix(rep(NA,length(sigvals.MPA.Kai)),ncol=length(sigvals.MPA.Kai),
         dimnames=list(NULL,colnames(sigvals.MPA.Kai)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann-Whitney U test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann Whitney U-Test)
sigvals.Kai <- 
  rbind.data.frame(sigvals.MPA.Kai,
                   null.row.sigvals.Kai,
                   sigvals.Sett.Kai[rev(order(sigvals.Sett.Kai$SettlementName)),])

sigvals.Kai[,2:8] <- unlist(sigvals.Kai[,2:8])


# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for trend plots)

trend.non.parametric.test.byMPA.Kai <- 
  data.frame(mapply(i=Kai.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Kai.Trend.Data$InterviewYear==unique(Kai.Trend.Data$InterviewYear)[1]],
                                    i[Kai.Trend.Data$InterviewYear==unique(Kai.Trend.Data$InterviewYear)[2]],
                                    i[Kai.Trend.Data$InterviewYear==unique(Kai.Trend.Data$InterviewYear)[3]]))
                    }))

colnames(trend.non.parametric.test.byMPA.Kai) <- colnames(sigvals.Kai[2:8])

# - Define data frame with p-values for trend plots
#   (all MPA households from each year of sampling are compared across time for the given variable, 
#   using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.Kai <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Kai["sl",1],NA,trend.non.parametric.test.byMPA.Kai["sl",2],
                   NA,trend.non.parametric.test.byMPA.Kai["sl",3],NA,trend.non.parametric.test.byMPA.Kai["sl",4],NA,trend.non.parametric.test.byMPA.Kai["sl",5],
                   NA,trend.non.parametric.test.byMPA.Kai["sl",6],NA,trend.non.parametric.test.byMPA.Kai["sl",7],NA)

colnames(trend.sigvals.Kai) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                 "TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")

trend.sigvals.Kai <- unlist(trend.sigvals.Kai)


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.Kai <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Kai),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Kai),
                                                function(i){
                                                  MannKendall(c(Kai.Trend.Data[Kai.Trend.Data$SettlementName==i &
                                                                                  Kai.Trend.Data$InterviewYear==unique(Kai.Trend.Data$InterviewYear)[1],a],
                                                                Kai.Trend.Data[Kai.Trend.Data$SettlementName==i &
                                                                                  Kai.Trend.Data$InterviewYear==unique(Kai.Trend.Data$InterviewYear)[2],a],
                                                                Kai.Trend.Data[Kai.Trend.Data$SettlementName==i &
                                                                                  Kai.Trend.Data$InterviewYear==unique(Kai.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.Kai) <- colnames(sigvals.Kai)

# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Kai <- 
  rbind.data.frame(cbind.data.frame(SettlementName="Kaimana MPA",trend.non.parametric.test.byMPA.Kai["sl",]),
                   null.row.sigvals.Kai,
                   trend.non.parametric.test.bySett.Kai[rev(order(trend.non.parametric.test.bySett.Kai$SettlementName)),])

annex.sigvals.Kai[2:8] <- unlist(annex.sigvals.Kai[2:8])


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

propdata.trend.test.Kai <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA

propdata.trend.test.Kai <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(Kai.FreqTables$t0[Kai.FreqTables$Variable==a])==0) {
                             Kai.FreqTables$t2[Kai.FreqTables$Variable==a &
                                                  Kai.FreqTables$t2!=0] 
                           } else {Kai.FreqTables$t0[Kai.FreqTables$Variable==a &
                                                        Kai.FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(Kai.FreqTables$t0[Kai.FreqTables$Variable==a])==0) {
                             Kai.FreqTables$t4[Kai.FreqTables$Variable==a &
                                                  Kai.FreqTables$t2!=0]
                           } else {Kai.FreqTables$t4[Kai.FreqTables$Variable==a &
                                                        Kai.FreqTables$t0!=0]}
                         propdata.trend.test.Kai[a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test.Kai[a] <- ifelse(is.na(propdata.trend.test.Kai[a]),100,propdata.trend.test.Kai[a])
                       }))

colnames(propdata.trend.test.Kai) <- c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein")




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Kai.TechReport.MPAHouseholdData)
rm(Kai.TechReport.MPAvBHS)
rm(Kai.TechReport.SettlementMeans)
rm(Kai.Trend.Data)
rm(Kai.FreqTables)
rm(even.number.setts.function.Kai)
rm(non.parametric.test.settlements.Kai)
rm(non.parametric.test.MPAvBHS.Kai)
rm(trend.non.parametric.test.byMPA.Kai)
rm(trend.non.parametric.test.bySett.Kai)
rm(null.row.sigvals.Kai)
rm(sigvals.MPA.Kai)
rm(sigvals.Sett.Kai)
rm(p.for.function)
rm(data.for.function)