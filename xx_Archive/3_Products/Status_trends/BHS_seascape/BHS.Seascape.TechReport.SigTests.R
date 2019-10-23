# 
# code:  Aggregated BHS-level Technical Report Significance Tests
# 
# github: WWF-ConsEvidence/MPAMystery/3_Analysis/1_Social/2_Status_trends/
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: April 2018
# modified: 
# 
# 
# ---- inputs ----
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
# ---- SECTION 1: Import Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Subset continuous variable datasets from Calcualte_BigFive.R ----

BHS.Seascape.TechReport.SigTest.Data <- 
  HHData %>%
  mutate(MPAName=ifelse(BHS.Seascape.TechReport.SigTest.Data$MPAID==1,
                                                       "Teluk Mayalibit",
                                                       ifelse(BHS.Seascape.TechReport.SigTest.Data$MPAID==2,
                                                              "Teluk Cenderawasih",
                                                              ifelse(BHS.Seascape.TechReport.SigTest.Data$MPAID==3,
                                                                     "Kaimana",
                                                                     ifelse(BHS.Seascape.TechReport.SigTest.Data$MPAID==4,
                                                                            "Kofiau dan Pulau Boo",
                                                                            ifelse(BHS.Seascape.TechReport.SigTest.Data$MPAID==5,
                                                                                   "Selat Dampier",
                                                                                   ifelse(BHS.Seascape.TechReport.SigTest.Data$MPAID==6,
                                                                                          "Misool Selatan Timur",
                                                                                          NA)))))))

# - "BHS Household Data" dataset
BHS.TechReport.SeascapeHouseholdData <- 
  left_join(BHS.Seascape.TechReport.SigTest.Data[BHS.Seascape.TechReport.SigTest.Data$Treatment==1 &
                                                   BHS.Seascape.TechReport.SigTest.Data$MonitoringYear== "4 Year Post",], 
            Days.unwell.treatment[Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

BHS.TechReport.SeascapeHouseholdData$MPAName <- 
  factor(BHS.TechReport.SeascapeHouseholdData$MPAName)

# - "MPA Means" dataset
BHS.TechReport.MPAMeans <- 
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="4 Year Post",-c(1:2)],
            Techreport.ByMPA[Techreport.ByMPA$MonitoringYear=="4 Year Post",
                             c("MPAID","TimeMarketMean")],  
            by=c("MPAID")) %>%
  left_join(Days.unwell.ByMPA[Days.unwell.ByMPA$MonitoringYear=="4 Year Post",c("MPAID","UnwellMean")],
            by="MPAID") %>%
  left_join(data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                              "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")),
            by="MPAID")

colnames(BHS.TechReport.MPAMeans) <- c(colnames(BHS.TechReport.MPAMeans)[1:3],
                                       "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                       "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell","MPAName")

# - "Trend" dataset
BHS.Trend.Data <- 
  left_join(BHS.Seascape.TechReport.SigTest.Data[BHS.Seascape.TechReport.SigTest.Data$Treatment==1,],
            Days.unwell[Days.unwell$Treatment==1,1:2],
            by="HouseholdID") 

# --- BASELINE DATA
# - "BHS Household Data" dataset
BHS.Baseline.TechReport.SeascapeHouseholdData <- 
  left_join(BHS.Seascape.TechReport.SigTest.Data[BHS.Seascape.TechReport.SigTest.Data$Treatment==1 &
                                                   BHS.Seascape.TechReport.SigTest.Data$MonitoringYear== "Baseline",], 
            Days.unwell.treatment[Days.unwell.treatment$MonitoringYear=="Baseline",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName <- 
  factor(BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName)

# - "MPA Means" dataset
BHS.Baseline.TechReport.MPAMeans <- 
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",-c(1:2)],
            Techreport.ByMPA[Techreport.ByMPA$MonitoringYear=="Baseline",
                             c("MPAID","TimeMarketMean")],  
            by=c("MPAID")) %>%
  left_join(Days.unwell.ByMPA[Days.unwell.ByMPA$MonitoringYear=="Baseline",c("MPAID","UnwellMean")],
            by="MPAID") %>%
  left_join(data.frame(MPAID=c(1:6),MPAName=c("Teluk Mayalibit","Teluk Cenderawasih","Kaimana",
                                              "Kofiau dan Pulau Boo","Selat Dampier","Misool Selatan Timur")),
            by="MPAID")

colnames(BHS.Baseline.TechReport.MPAMeans) <- c(colnames(BHS.Baseline.TechReport.MPAMeans)[1:3],
                                       "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                       "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell","MPAName")


# ---- 1.2 Define list of MPA names in Seascape ----

MPA.names.BHS <- factor(BHS.TechReport.MPAMeans$MPAName)


# ---- 1.3 Subset categorical variable frequency tables from BHS_MPA_Mystery.R ----

# - "Trend" dataset
BHS.FreqTables <- 
  HHDemos.context %>%
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

BHS.FreqTables <- 
  as.data.frame(t(BHS.FreqTables[,-1]))
colnames(BHS.FreqTables) <- c("t0","t2","t4")

BHS.FreqTables$Category <- rownames(BHS.FreqTables)
BHS.FreqTables$Variable <- ifelse(grepl("PrimaryOcc",BHS.FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",BHS.FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",BHS.FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",BHS.FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",BHS.FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("Child",BHS.FreqTables$Category)==T,"ChildFS",
                                                                      ifelse(grepl("Protein",BHS.FreqTables$Category)==T,"Protein",NA)))))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of MPAs, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median MPA for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.MPA.function.BHS <- 
  mapply(a=BHS.TechReport.MPAMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=BHS.TechReport.SeascapeHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- BHS.TechReport.MPAMeans$MPAName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- BHS.TechReport.MPAMeans$MPAName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- BHS.TechReport.MPAMeans$MPAName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==upper.sett & !is.na(b)])))<
                                          (sd(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett],na.rm=T)/sqrt(length(b[BHS.TechReport.SeascapeHouseholdData$MPAName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.MPA.BHS <- 
  mapply(i=BHS.TechReport.MPAMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.MPA.function.BHS),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(MPA.names.BHS)%%2!=0,
                                      as.character(BHS.TechReport.MPAMeans$MPAName[which(i==med)]),
                                      as.character(even.number.MPA.function.BHS[j])),
                               levels=levels(MPA.names.BHS))})

# ---- 2.1 Create list of median MPA for each continuous variable AT BASELINE (whether the variable is parametric or non-parametric) ----

even.number.MPA.function.BHS.baseline <- 
  mapply(a=BHS.Baseline.TechReport.MPAMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","DaysUnwell")],
         b=BHS.Baseline.TechReport.SeascapeHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- BHS.Baseline.TechReport.MPAMeans$MPAName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- BHS.Baseline.TechReport.MPAMeans$MPAName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- BHS.Baseline.TechReport.MPAMeans$MPAName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==upper.sett & !is.na(b)])))<
                                          (sd(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett],na.rm=T)/sqrt(length(b[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.MPA.BHS.baseline <- 
  mapply(i=BHS.Baseline.TechReport.MPAMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","DaysUnwell")],
         j=names(even.number.MPA.function.BHS.baseline),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(MPA.names.BHS)%%2!=0,
                                      as.character(BHS.Baseline.TechReport.MPAMeans$MPAName[which(i==med)]),
                                      as.character(even.number.MPA.function.BHS.baseline[j])),
                               levels=levels(MPA.names.BHS))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.BHS.FS <- 
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$FSIndex),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="BHS: Food Security Score Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

qqnorm(BHS.TechReport.SeascapeHouseholdData$FSIndex)
qqline(BHS.TechReport.SeascapeHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.BHS.MA <- 
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$MAIndex),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="BHS: HH Material Assets Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

log.MA <- log(BHS.TechReport.SeascapeHouseholdData$MAIndex[BHS.TechReport.SeascapeHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.BHS.PA <- 
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$PAIndex),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="BHS: Place Attachment Score Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

qqnorm(BHS.TechReport.SeascapeHouseholdData$PAIndex)
qqline(BHS.TechReport.SeascapeHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.BHS.MT <- 
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$MTIndex),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="BHS: Marine Tenure Score Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

qqnorm(BHS.TechReport.SeascapeHouseholdData$MTIndex)
qqline(BHS.TechReport.SeascapeHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.BHS.SE <-
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$SERate),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="BHS: School Enrollment Rate Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

qqnorm(BHS.TechReport.SeascapeHouseholdData$SERate)
qqline(BHS.TechReport.SeascapeHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.BHS.TimeMarket <- 
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$TimeMarketClean),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="BHS: Time to Market Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

qqnorm(BHS.TechReport.SeascapeHouseholdData$TimeMarketClean)
qqline(BHS.TechReport.SeascapeHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.BHS.DaysUnwell <- 
  ggplot(BHS.TechReport.SeascapeHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BHS.TechReport.SeascapeHouseholdData$DaysUnwell),
                                    sd=sd(BHS.TechReport.SeascapeHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="BHS: HH Days Unwell Distribution,\n4 Year Post Baseline") +
  dist.plot.theme

qqnorm(BHS.TechReport.SeascapeHouseholdData$DaysUnwell)
qqline(BHS.TechReport.SeascapeHouseholdData$DaysUnwell,col="green")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Non-parametric Significance Test Functions (using Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# NOTE: Typically, food security, material assets, marine tenure, enrollment rate, and place attachment end up being non-parametric; 
#       however, if the distribution appears to be normal, then PARAMETRIC tests are more powerful and the better choice.


# ---- 4.1 Create function that will output significance values for non-parametric variables, BY MPA ----
#          (for status plots)

non.parametric.test.MPA.BHS <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(MPAName=c(as.character(MPA.names.BHS[which(MPA.names.BHS!=median.MPA.BHS[a])]),
                                                               as.character(median.MPA.BHS[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=MPA.names.BHS[which(MPA.names.BHS!=median.MPA.BHS[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       BHS.TechReport.SeascapeHouseholdData[BHS.TechReport.SeascapeHouseholdData$MPAName==i |
                                                                                                                          BHS.TechReport.SeascapeHouseholdData$MPAName==median.MPA.BHS[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~MPAName,
                                                                                                   data=BHS.TechReport.SeascapeHouseholdData[BHS.TechReport.SeascapeHouseholdData$MPAName==i |
                                                                                                                                           BHS.TechReport.SeascapeHouseholdData$MPAName==median.MPA.BHS[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of MPA names.  Now all MPA names are in same order.
sigvals.BHS <- 
  cbind.data.frame(non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"FSIndex.MPAName"),
                                                        c("FSIndex.MPAName","FSIndex.p.value")],
                   non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"MAIndex.MPAName"),
                                                        c("MAIndex.MPAName","MAIndex.p.value")],
                   non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"PAIndex.MPAName"),
                                                        c("PAIndex.MPAName","PAIndex.p.value")],
                   non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"MTIndex.MPAName"),
                                                        c("MTIndex.MPAName","MTIndex.p.value")],
                   non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"SERate.MPAName"),
                                                        c("SERate.MPAName","SERate.p.value")],
                   non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"TimeMarketClean.MPAName"),
                                                        c("TimeMarketClean.MPAName","TimeMarketClean.p.value")],
                   non.parametric.test.MPA.BHS[order(non.parametric.test.MPA.BHS$"DaysUnwell.MPAName"),
                                                        c("DaysUnwell.MPAName","DaysUnwell.p.value")])

# - Remove all MPA name columns except for one. 
sigvals.BHS <- 
  sigvals.BHS[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.BHS) <- c("MPAName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


null.row.sigvals.BHS <- 
  matrix(rep(NA,length(sigvals.BHS)),ncol=length(sigvals.BHS),
         dimnames=list(NULL,colnames(sigvals.BHS)))

sigvals.BHS <-
  rbind.data.frame(data.frame(MPAName="Bird's Head Seascape",as.data.frame(null.row.sigvals.BHS)[,2:8]),
                   null.row.sigvals.BHS,
                   sigvals.BHS[rev(order(sigvals.BHS$MPAName)),])

sigvals.BHS[,2:8] <- unlist(sigvals.BHS[,2:8])


# ---- 4.1b FOR BASELINE, create function that will output significance values for non-parametric variables, BY MPA ----
#          

non.parametric.test.MPA.BHS.baseline <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(MPAName=c(as.character(MPA.names.BHS[which(MPA.names.BHS!=median.MPA.BHS.baseline[a])]),
                                                        as.character(median.MPA.BHS.baseline[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=MPA.names.BHS[which(MPA.names.BHS!=median.MPA.BHS.baseline[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       BHS.Baseline.TechReport.SeascapeHouseholdData[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==i |
                                                                                                                              BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==median.MPA.BHS.baseline[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~MPAName,
                                                                                                   data=BHS.Baseline.TechReport.SeascapeHouseholdData[BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==i |
                                                                                                                                               BHS.Baseline.TechReport.SeascapeHouseholdData$MPAName==median.MPA.BHS.baseline[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - FOR BASELINE, alphabetize each column of MPA names.  Now all MPA names are in same order.
sigvals.BHS.baseline <- 
  cbind.data.frame(non.parametric.test.MPA.BHS.baseline[order(non.parametric.test.MPA.BHS.baseline$"FSIndex.MPAName"),
                                               c("FSIndex.MPAName","FSIndex.p.value")],
                   non.parametric.test.MPA.BHS.baseline[order(non.parametric.test.MPA.BHS.baseline$"MAIndex.MPAName"),
                                               c("MAIndex.MPAName","MAIndex.p.value")],
                   non.parametric.test.MPA.BHS.baseline[order(non.parametric.test.MPA.BHS.baseline$"PAIndex.MPAName"),
                                               c("PAIndex.MPAName","PAIndex.p.value")],
                   non.parametric.test.MPA.BHS.baseline[order(non.parametric.test.MPA.BHS.baseline$"MTIndex.MPAName"),
                                               c("MTIndex.MPAName","MTIndex.p.value")],
                   non.parametric.test.MPA.BHS.baseline[order(non.parametric.test.MPA.BHS.baseline$"SERate.MPAName"),
                                               c("SERate.MPAName","SERate.p.value")],
                   non.parametric.test.MPA.BHS.baseline[order(non.parametric.test.MPA.BHS.baseline$"DaysUnwell.MPAName"),
                                               c("DaysUnwell.MPAName","DaysUnwell.p.value")])

# - FOR BASELINE, remove all MPA name columns except for one. 
sigvals.BHS.baseline <- 
  sigvals.BHS.baseline[,c(1,2,4,6,8,10,12)]

colnames(sigvals.BHS.baseline) <- c("MPAName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Unwell.pval")

null.row.sigvals.BHS.baseline <- 
  matrix(rep(NA,length(sigvals.BHS.baseline)),ncol=length(sigvals.BHS.baseline),
         dimnames=list(NULL,colnames(sigvals.BHS.baseline)))

sigvals.BHS.baseline <-
  rbind.data.frame(data.frame(MPAName="Bird's Head Seascape",as.data.frame(null.row.sigvals.BHS.baseline)[,2:7]),
                   null.row.sigvals.BHS.baseline,
                   sigvals.BHS.baseline[rev(order(sigvals.BHS.baseline$MPAName)),])

sigvals.BHS.baseline[,2:7] <- unlist(sigvals.BHS.baseline[,2:7])


# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY SEASCAPE ----
#          (for trend plots)

trend.non.parametric.test.BHS <- 
  data.frame(mapply(i=BHS.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[BHS.Trend.Data$MonitoringYear==unique(BHS.Trend.Data$MonitoringYear)[1]],
                                    i[BHS.Trend.Data$MonitoringYear==unique(BHS.Trend.Data$MonitoringYear)[2]],
                                    i[BHS.Trend.Data$MonitoringYear==unique(BHS.Trend.Data$MonitoringYear)[3]]))
                    }))

colnames(trend.non.parametric.test.BHS) <- colnames(sigvals.BHS[2:8])

# - Define data frame with p-values for trend plots
#   (all BHS treatment households from each year of sampling are compared across time for the given variable, 
#   using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the seascape")
trend.sigvals.BHS <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.BHS["sl",1],NA,trend.non.parametric.test.BHS["sl",2],
                   NA,trend.non.parametric.test.BHS["sl",3],NA,trend.non.parametric.test.BHS["sl",4],NA,trend.non.parametric.test.BHS["sl",5],
                   NA,trend.non.parametric.test.BHS["sl",6],NA,trend.non.parametric.test.BHS["sl",7],NA)

colnames(trend.sigvals.BHS) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarketMean","TimeMarketErr","UnwellMean","UnwellErr")


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for annex plots)

trend.non.parametric.test.byMPA.BHS <- 
  cbind.data.frame(MPAName=as.character(MPA.names.BHS),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(MPA.names.BHS),
                                                function(i){
                                                  MannKendall(c(BHS.Trend.Data[BHS.Trend.Data$MPAName==i &
                                                                                  BHS.Trend.Data$MonitoringYear==unique(BHS.Trend.Data$MonitoringYear)[1],a],
                                                                BHS.Trend.Data[BHS.Trend.Data$MPAName==i &
                                                                                  BHS.Trend.Data$MonitoringYear==unique(BHS.Trend.Data$MonitoringYear)[2],a],
                                                                BHS.Trend.Data[BHS.Trend.Data$MPAName==i &
                                                                                  BHS.Trend.Data$MonitoringYear==unique(BHS.Trend.Data$MonitoringYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.byMPA.BHS) <- colnames(sigvals.BHS)

# - Define data frame with p-values for annex plots
#   (households within each MPA from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the MPA)


annex.sigvals.BHS <- 
  rbind.data.frame(cbind.data.frame(MPAName="Bird's Head Seascape",trend.non.parametric.test.BHS["sl",]),
                   null.row.sigvals.BHS,
                   trend.non.parametric.test.byMPA.BHS[rev(order(trend.non.parametric.test.byMPA.BHS$MPAName)),])

annex.sigvals.BHS[2:8] <- unlist(annex.sigvals.BHS[2:8])


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: Chi-square Tests for Categorical Variables ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Trend plot, chi-square tests on most recent monitoring year ----
#          (with baseline proportions serving as expected values/probabilities)

propdata.trend.test.BHS <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA

propdata.trend.test.BHS <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(BHS.FreqTables$t0[BHS.FreqTables$Variable==a])==0) {
                             BHS.FreqTables$t2[BHS.FreqTables$Variable==a &
                                                  BHS.FreqTables$t2!=0] 
                           } else {BHS.FreqTables$t0[BHS.FreqTables$Variable==a &
                                                        BHS.FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(BHS.FreqTables$t0[BHS.FreqTables$Variable==a])==0) {
                             BHS.FreqTables$t4[BHS.FreqTables$Variable==a &
                                                  BHS.FreqTables$t2!=0]
                           } else {BHS.FreqTables$t4[BHS.FreqTables$Variable==a &
                                                        BHS.FreqTables$t0!=0]}
                         propdata.trend.test.BHS[a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test.BHS[a] <- ifelse(is.na(propdata.trend.test.BHS[a]),100,propdata.trend.test.BHS[a])
                       }))

colnames(propdata.trend.test.BHS) <- c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","ChildFS","Protein")




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(BHS.Seascape.TechReport.SigTest.Data)
rm(BHS.TechReport.SeascapeHouseholdData)
rm(BHS.TechReport.MPAMeans)
rm(BHS.Trend.Data)
rm(BHS.FreqTables)
rm(even.number.MPA.function.BHS)
rm(non.parametric.test.MPA.BHS)
rm(trend.non.parametric.test.byMPA.BHS)
rm(null.row.sigvals.BHS)
rm(p.for.function)
rm(data.for.function)