# 
# code:  Cenderawasih Technical Report Significance Tests
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
TNTC.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                          MPA.TechReport.SigTest.Data$MPAID==2,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==2 &
                                    Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

TNTC.TechReport.MPAHouseholdData$SettlementName <- 
  factor(TNTC.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus BHS" dataset
TNTC.TechReport.MPAvBHS <- 
  rbind.data.frame(cbind.data.frame(left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==2 &
                                                                            MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post" &
                                                                            MPA.TechReport.SigTest.Data$Treatment==1,],
                                              Days.unwell[Days.unwell$MPAID==2 &
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
TNTC.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==2,],
            Techreport.BySett[Techreport.BySett$MPAID==2 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c("SettlementID","SettlementName","TimeMarketMean")],  
            by=c("SettlementID","SettlementName"))

TNTC.TechReport.SettlementMeans <- 
  left_join(TNTC.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==2 &
                                 Days.unwell.BySett$MonitoringYear=="4 Year Post",c(1,4)],
            by="SettlementID")

TNTC.TechReport.SettlementMeans <- 
  TNTC.TechReport.SettlementMeans[!is.na(TNTC.TechReport.SettlementMeans$SettlementName),]

colnames(TNTC.TechReport.SettlementMeans) <- c(colnames(TNTC.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")

# - "Trend" dataset
TNTC.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==2,],
            Days.unwell[Days.unwell$MPAID==2 &
                          Days.unwell$Treatment==1,1:2],
            by="HouseholdID") 

# ---- 1.2 Define list of settlement names in MPA ----

sett.names.TNTC <- factor(TNTC.TechReport.SettlementMeans$SettlementName)


# ---- 1.3 Subset categorical variable frequency tables from BHS_MPA_Mystery.R ----

# - "Trend" dataset
TNTC.FreqTables <- 
  HHDemos.context[HHDemos.context$MPAID==2,] %>%
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

TNTC.FreqTables <- 
  as.data.frame(t(TNTC.FreqTables[,-1]))
colnames(TNTC.FreqTables) <- c("t0","t2","t4")

TNTC.FreqTables$Category <- rownames(TNTC.FreqTables)
TNTC.FreqTables$Variable <- ifelse(grepl("PrimaryOcc",TNTC.FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",TNTC.FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",TNTC.FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",TNTC.FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",TNTC.FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("Child",TNTC.FreqTables$Category)==T,"ChildFS",
                                                                      ifelse(grepl("Protein",TNTC.FreqTables$Category)==T,"Protein",NA)))))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.TNTC <- 
  mapply(a=TNTC.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=TNTC.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- TNTC.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- TNTC.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- TNTC.TechReport.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[TNTC.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.TNTC <- 
  mapply(i=TNTC.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.TNTC),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.TNTC)%%2!=0,
                                      as.character(TNTC.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.TNTC[j])),
                               levels=levels(sett.names.TNTC))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.TNTC.FS <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="TNTC: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$FSIndex)
qqline(TNTC.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.TNTC.MA <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="TNTC: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(TNTC.TechReport.MPAHouseholdData$MAIndex[TNTC.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.TNTC.PA <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="TNTC: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$PAIndex)
qqline(TNTC.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.TNTC.MT <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="TNTC: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$MTIndex)
qqline(TNTC.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.TNTC.SE <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="TNTC: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$SERate)
qqline(TNTC.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.TNTC.TimeMarket <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="TNTC: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(TNTC.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.TNTC.DaysUnwell <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="TNTC: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$DaysUnwell)
qqline(TNTC.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.TNTC <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.TNTC[which(sett.names.TNTC!=median.setts.TNTC[a])]),
                                                               as.character(median.setts.TNTC[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.TNTC[which(sett.names.TNTC!=median.setts.TNTC[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       TNTC.TechReport.MPAHouseholdData[TNTC.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          TNTC.TechReport.MPAHouseholdData$SettlementName==median.setts.TNTC[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=TNTC.TechReport.MPAHouseholdData[TNTC.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           TNTC.TechReport.MPAHouseholdData$SettlementName==median.setts.TNTC[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.TNTC <- 
  cbind.data.frame(non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.TNTC <-
  sigvals.Sett.TNTC[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.TNTC) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. BHS ----
#          (for status plots, comparing MPA households to all BHS households)

non.parametric.test.MPAvBHS.TNTC <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- TNTC.TechReport.MPAvBHS[,a]
                      wilcox.test(var~MPAvBHS,
                                  data=TNTC.TechReport.MPAvBHS,
                                  exact=F)}))["p.value",]

sigvals.MPA.TNTC <- 
  cbind.data.frame("Teluk Cenderawasih\nNational Park",non.parametric.test.MPAvBHS.TNTC)

colnames(sigvals.MPA.TNTC) <- colnames(sigvals.Sett.TNTC)

null.row.sigvals.TNTC <- 
  matrix(rep(NA,length(sigvals.MPA.TNTC)),ncol=length(sigvals.MPA.TNTC),
         dimnames=list(NULL,colnames(sigvals.MPA.TNTC)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann Whitney U-Test)
sigvals.TNTC <- 
  rbind.data.frame(sigvals.MPA.TNTC,
                   null.row.sigvals.TNTC,
                   sigvals.Sett.TNTC[rev(order(sigvals.Sett.TNTC$SettlementName)),])

sigvals.TNTC[,2:8] <- unlist(sigvals.TNTC[,2:8])


# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for trend plots)

trend.non.parametric.test.byMPA.TNTC <- 
  data.frame(mapply(i=TNTC.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[1]],
                                    i[TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[2]],
                                    i[TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[3]]))
                    }))

colnames(trend.non.parametric.test.byMPA.TNTC) <- colnames(sigvals.TNTC[2:8])

# - Define data frame with p-values for trend plots
#   (all MPA households from each year of sampling are compared across time for the given variable, 
#   using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.TNTC <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.TNTC["sl",1],NA,trend.non.parametric.test.byMPA.TNTC["sl",2],
                   NA,trend.non.parametric.test.byMPA.TNTC["sl",3],NA,trend.non.parametric.test.byMPA.TNTC["sl",4],NA,trend.non.parametric.test.byMPA.TNTC["sl",5],
                   NA,trend.non.parametric.test.byMPA.TNTC["sl",6],NA,trend.non.parametric.test.byMPA.TNTC["sl",7],NA)

colnames(trend.sigvals.TNTC) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")

trend.sigvals.TNTC <- unlist(trend.sigvals.TNTC)


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.TNTC <- 
  cbind.data.frame(SettlementName=as.character(sett.names.TNTC),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.TNTC),
                                                function(i){
                                                  MannKendall(c(TNTC.Trend.Data[TNTC.Trend.Data$SettlementName==i &
                                                                                  TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[1],a],
                                                                TNTC.Trend.Data[TNTC.Trend.Data$SettlementName==i &
                                                                                  TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[2],a],
                                                                TNTC.Trend.Data[TNTC.Trend.Data$SettlementName==i &
                                                                                  TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.TNTC) <- colnames(sigvals.TNTC)

# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.TNTC <- 
  rbind.data.frame(cbind.data.frame(SettlementName="Teluk Cenderawasih\nNational Park",trend.non.parametric.test.byMPA.TNTC["sl",]),
                   null.row.sigvals.TNTC,
                   trend.non.parametric.test.bySett.TNTC[rev(order(trend.non.parametric.test.bySett.TNTC$SettlementName)),])

annex.sigvals.TNTC[2:8] <- unlist(annex.sigvals.TNTC[2:8])


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

propdata.trend.test.TNTC <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA

propdata.trend.test.TNTC <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(TNTC.FreqTables$t0[TNTC.FreqTables$Variable==a])==0) {
                             TNTC.FreqTables$t2[TNTC.FreqTables$Variable==a &
                                                  TNTC.FreqTables$t2!=0] 
                           } else {TNTC.FreqTables$t0[TNTC.FreqTables$Variable==a &
                                                        TNTC.FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(TNTC.FreqTables$t0[TNTC.FreqTables$Variable==a])==0) {
                             TNTC.FreqTables$t4[TNTC.FreqTables$Variable==a &
                                                  TNTC.FreqTables$t2!=0]
                           } else {TNTC.FreqTables$t4[TNTC.FreqTables$Variable==a &
                                                        TNTC.FreqTables$t0!=0]}
                         propdata.trend.test.TNTC[a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test.TNTC[a] <- ifelse(is.na(propdata.trend.test.TNTC[a]),100,propdata.trend.test.TNTC[a])
                       }))

colnames(propdata.trend.test.TNTC) <- c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein")


# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(TNTC.TechReport.MPAHouseholdData)
rm(TNTC.TechReport.MPAvBHS)
rm(TNTC.TechReport.SettlementMeans)
rm(TNTC.Trend.Data)
rm(TNTC.FreqTables)
rm(even.number.setts.function.TNTC)
rm(non.parametric.test.settlements.TNTC)
rm(non.parametric.test.MPAvBHS.TNTC)
rm(trend.non.parametric.test.byMPA.TNTC)
rm(trend.non.parametric.test.bySett.TNTC)
rm(null.row.sigvals.TNTC)
rm(sigvals.MPA.TNTC)
rm(sigvals.Sett.TNTC)
rm(p.for.function)
rm(data.for.function)