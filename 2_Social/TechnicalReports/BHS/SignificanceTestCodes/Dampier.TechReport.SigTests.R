# 
# code:  Dampier Technical Report Significance Tests
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


# ---- 1.1 Subset continuous variable datasets from BHS_MPA_Mystery.R ----

MPA.TechReport.SigTest.Data <- 
  left_join(BigFive[,c(1,2,4:11,14)],
            HHLivelihood[,c(1,15)],
            by="HouseholdID")

# - "MPA Household Data" dataset
Damp.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                          MPA.TechReport.SigTest.Data$MPAID==5,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==5 &
                                    Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Damp.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Damp.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus BHS" dataset
Damp.TechReport.MPAvBHS <- 
  rbind.data.frame(cbind.data.frame(left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==5 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post" &
                                            MPA.TechReport.SigTest.Data$Treatment==1,],
            Days.unwell[Days.unwell$MPAID==5 &
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
Damp.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==5,],
            Techreport.BySett[Techreport.BySett$MPAID==5 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c("SettlementID","SettlementName","TimeMarketMean")],  
            by=c("SettlementID","SettlementName"))

Damp.TechReport.SettlementMeans <- 
  left_join(Damp.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==5 &
                                 Days.unwell.BySett$MonitoringYear=="4 Year Post",c(1,4)],
            by="SettlementID")

Damp.TechReport.SettlementMeans <- 
  Damp.TechReport.SettlementMeans[!is.na(Damp.TechReport.SettlementMeans$SettlementName),]

colnames(Damp.TechReport.SettlementMeans) <- c(colnames(Damp.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")

# - "Trend" dataset
Damp.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==5,],
            Days.unwell[Days.unwell$MPAID==5 &
                          Days.unwell$Treatment==1,1:2],
            by="HouseholdID") 


# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Damp <- factor(Damp.TechReport.SettlementMeans$SettlementName)


# ---- 1.3 Subset categorical variable frequency tables from BHS_MPA_Mystery.R ----

# - "Trend" dataset
Damp.FreqTables <- 
  HHDemos.context[HHDemos.context$MPAID==5,] %>%
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

Damp.FreqTables <- 
  as.data.frame(t(Damp.FreqTables[,-1]))
colnames(Damp.FreqTables) <- c("t0","t2","t4")

Damp.FreqTables$Category <- rownames(Damp.FreqTables)
Damp.FreqTables$Variable <- ifelse(grepl("PrimaryOcc",Damp.FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",Damp.FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",Damp.FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",Damp.FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",Damp.FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("Child",Damp.FreqTables$Category)==T,"ChildFS",
                                                                      ifelse(grepl("Protein",Damp.FreqTables$Category)==T,"Protein",NA)))))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Damp <- 
  mapply(a=Damp.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Damp.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Damp.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Damp.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Damp.TechReport.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Damp.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.Damp <- 
  mapply(i=Damp.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.Damp),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Damp)%%2!=0,
                                      as.character(Damp.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Damp[j])),
                               levels=levels(sett.names.Damp))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.Damp.FS <- 
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Damp: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Damp.TechReport.MPAHouseholdData$FSIndex)
qqline(Damp.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Damp.MA <- 
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Damp: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(Damp.TechReport.MPAHouseholdData$MAIndex[Damp.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Damp.PA <- 
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Damp: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Damp.TechReport.MPAHouseholdData$PAIndex)
qqline(Damp.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Damp.MT <- 
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Damp: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Damp.TechReport.MPAHouseholdData$MTIndex)
qqline(Damp.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Damp.SE <-
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Damp: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(Damp.TechReport.MPAHouseholdData$SERate)
qqline(Damp.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Damp.TimeMarket <- 
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Damp: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(Damp.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(Damp.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Damp.DaysUnwell <- 
  ggplot(Damp.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Damp.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Damp.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Damp: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(Damp.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Damp.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.Damp <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Damp[which(sett.names.Damp!=median.setts.Damp[a])]),
                                                               as.character(median.setts.Damp[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Damp[which(sett.names.Damp!=median.setts.Damp[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Damp.TechReport.MPAHouseholdData[Damp.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Damp.TechReport.MPAHouseholdData$SettlementName==median.setts.Damp[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Damp.TechReport.MPAHouseholdData[Damp.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Damp.TechReport.MPAHouseholdData$SettlementName==median.setts.Damp[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Damp <- 
  cbind.data.frame(non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Damp <- 
  sigvals.Sett.Damp[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Damp) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. BHS ----
#          (for status plots, comparing MPA households to all BHS households)

non.parametric.test.MPAvBHS.Damp <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Damp.TechReport.MPAvBHS[,a]
                      wilcox.test(var~MPAvBHS,
                                  data=Damp.TechReport.MPAvBHS,
                                  exact=F)}))["p.value",]

sigvals.MPA.Damp <- 
  cbind.data.frame("Selat Dampier\nMPA",non.parametric.test.MPAvBHS.Damp)

colnames(sigvals.MPA.Damp) <- colnames(sigvals.Sett.Damp)

null.row.sigvals.Damp <- 
  matrix(rep(NA,length(sigvals.MPA.Damp)),ncol=length(sigvals.MPA.Damp),
         dimnames=list(NULL,colnames(sigvals.MPA.Damp)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)
sigvals.Damp <- 
  rbind.data.frame(sigvals.MPA.Damp,
                   null.row.sigvals.Damp,
                   sigvals.Sett.Damp[rev(order(sigvals.Sett.Damp$SettlementName)),])

sigvals.Damp[,2:8] <- unlist(sigvals.Damp[,2:8])


# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for trend plots)

trend.non.parametric.test.byMPA.Damp <- 
  data.frame(mapply(i=Damp.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[1]],
                                    i[Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[2]],
                                    i[Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[3]]))
                    }))

colnames(trend.non.parametric.test.byMPA.Damp) <- colnames(sigvals.Damp[2:8])

# - Define data frame with p-values for trend plots
#   (all MPA households from each year of sampling are compared across time for the given variable, 
#   using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.Damp <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Damp["sl",1],NA,trend.non.parametric.test.byMPA.Damp["sl",2],
                   NA,trend.non.parametric.test.byMPA.Damp["sl",3],NA,trend.non.parametric.test.byMPA.Damp["sl",4],NA,trend.non.parametric.test.byMPA.Damp["sl",5],
                   NA,trend.non.parametric.test.byMPA.Damp["sl",6],NA,trend.non.parametric.test.byMPA.Damp["sl",7],NA)

colnames(trend.sigvals.Damp) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")

trend.sigvals.Damp <- unlist(trend.sigvals.Damp)


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.Damp <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Damp),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Damp),
                                                function(i){
                                                  MannKendall(c(Damp.Trend.Data[Damp.Trend.Data$SettlementName==i &
                                                                                  Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[1],a],
                                                                Damp.Trend.Data[Damp.Trend.Data$SettlementName==i &
                                                                                  Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[2],a],
                                                                Damp.Trend.Data[Damp.Trend.Data$SettlementName==i &
                                                                                  Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.Damp) <- colnames(sigvals.Damp)

# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Damp <- 
  rbind.data.frame(cbind.data.frame(SettlementName="Selat Dampier\nMPA",trend.non.parametric.test.byMPA.Damp["sl",]),
                   null.row.sigvals.Damp,
                   trend.non.parametric.test.bySett.Damp[rev(order(trend.non.parametric.test.bySett.Damp$SettlementName)),])

annex.sigvals.Damp[2:8] <- unlist(annex.sigvals.Damp[2:8])


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

propdata.trend.test.Damp <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA

propdata.trend.test.Damp <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(Damp.FreqTables$t0[Damp.FreqTables$Variable==a])==0) {
                             Damp.FreqTables$t2[Damp.FreqTables$Variable==a &
                                                  Damp.FreqTables$t2!=0] 
                           } else {Damp.FreqTables$t0[Damp.FreqTables$Variable==a &
                                                        Damp.FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(Damp.FreqTables$t0[Damp.FreqTables$Variable==a])==0) {
                             Damp.FreqTables$t4[Damp.FreqTables$Variable==a &
                                                  Damp.FreqTables$t2!=0]
                           } else {Damp.FreqTables$t4[Damp.FreqTables$Variable==a &
                                                        Damp.FreqTables$t0!=0]}
                         propdata.trend.test.Damp[a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test.Damp[a] <- ifelse(is.na(propdata.trend.test.Damp[a]),100,propdata.trend.test.Damp[a])
                       }))

colnames(propdata.trend.test.Damp) <- c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein")




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Damp.TechReport.MPAHouseholdData)
rm(Damp.TechReport.MPAvBHS)
rm(Damp.TechReport.SettlementMeans)
rm(Damp.Trend.Data)
rm(Damp.FreqTables)
rm(even.number.setts.function.Damp)
rm(non.parametric.test.settlements.Damp)
rm(non.parametric.test.MPAvBHS.Damp)
rm(trend.non.parametric.test.byMPA.Damp)
rm(trend.non.parametric.test.bySett.Damp)
rm(null.row.sigvals.Damp)
rm(sigvals.MPA.Damp)
rm(sigvals.Sett.Damp)
rm(p.for.function)
rm(data.for.function)