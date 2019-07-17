# 
# code:  Flotim Technical Report Significance Tests
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: December 2017
# modified for Flotim: Amari Bauer, June 2019
# 
# ---- inputs ----
#  Dependencies: SBS_TechReport_Calculations.R
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
source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/SBS_TechReport_Calculations.R")

# ---- 1.1 Subset continuous variable datasets from SBS.TechReport.Calculations.R ----


MPA.TechReport.SigTest.Data <- 
  left_join(dplyr:: select(BigFive,"HouseholdID", "MPAID", "SettlementName", "Treatment", "InterviewYear", 
                           "MonitoringYear","MTIndex","MAIndex", "FSIndex", "PAIndex",
                           "SERate"),
            dplyr:: select(HHData, "HouseholdID", "TimeMarket"))

MPA.TechReport.SigTest.Trend.Data <- MPA.TechReport.SigTest.Data


MPA.TechReport.SigTest.Data <- MPA.TechReport.SigTest.Data %>%
  filter(MonitoringYear=="3 Year Post")


# - "MPA Household Data" dataset
Flotim.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==16 &
                                       MPA.TechReport.SigTest.Data$MonitoringYear=="3 Year Post" ,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==16,
                                  c("HouseholdID","DaysUnwell")])


Flotim.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Flotim.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus Control" dataset
Flotim.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==16,],
            Days.unwell[Days.unwell$MPAID==16,
                        c("HouseholdID","DaysUnwell")])

Techreport.ByMPA.control.annex <- Techreport.ByMPA.control.annex %>% 
  filter(MPAID==16)
BigFive.ControlGroup.annex <-BigFive.ControlGroup.annex %>% filter(MPAID==16)

Techreport.ByMPA.control.annex <- left_join(BigFive.ControlGroup.annex,Techreport.ByMPA.control.annex,)


Days.unwell.Alor.control.annex <- Days.unwell.Alor.control.annex %>%
  group_by(MPAID,MonitoringYear) %>%
  filter(MPAID==15) %>%
  summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))

Techreport.ByMPA.control.annex <- left_join(Techreport.ByMPA.control.annex,Days.unwell.Alor.control.annex[2:4])

# - "Settlement Means" dataset
Flotim.TechReport.Status.SettlementMeans<-
  left_join(BigFive.bySett[BigFive.bySett$Treatment==1 &
                             BigFive.bySett$MonitoringYear=="3 Year Post" &
                                  BigFive.bySett$MPAID==16,],
            Techreport.Status.BySett[Techreport.Status.BySett$MPAID==16 & Techreport.Status.BySett$MonitoringYear=="3 Year Post",
                              c("SettlementID","SettlementName","TimeMarketMean")])


Days.unwell.BySett.Status <- Days.unwell.BySett %>% 
  filter(MonitoringYear=="3 Year Post" & MPAID==16)

Flotim.TechReport.Status.SettlementMeans <- 
  left_join(Flotim.TechReport.Status.SettlementMeans,
            Days.unwell.BySett.Status[Days.unwell.BySett.Status$MPAID==16 ,c("SettlementID", "UnwellMean","MonitoringYear")])


#Removing the settlement with an NA in order for function to run
Flotim.TechReport.Status.SettlementMeans <- 
  Flotim.TechReport.Status.SettlementMeans[!is.na(Flotim.TechReport.Status.SettlementMeans$SettlementName),]


colnames(Flotim.TechReport.Status.SettlementMeans) <- c(colnames(Flotim.TechReport.Status.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr", "MTIndex","MTErr",
                                              "SERate","SEErr","TimeMarket","DaysUnwell")

# - "Trend" dataset
Flotim.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Trend.Data[MPA.TechReport.SigTest.Trend.Data$MPAID==16,],
            Days.unwell[Days.unwell$MPAID==16 ,1:2],
            by="HouseholdID") 

Flotim.FreqTables <- 
  HHDemos.context[HHDemos.context$MPAID==16,] %>%
  group_by(MonitoringYear) %>%
  summarise(PrimaryOcc.Fish=length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                       !is.na(PrimaryLivelihood)]),
            PrimaryOcc.Farm=length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                       !is.na(PrimaryLivelihood)]),
            PrimaryOcc.WageLabor=length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                            !is.na(PrimaryLivelihood)]),
            PrimaryOcc.HarvestForest=length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                !is.na(PrimaryLivelihood)]),
            PrimaryOcc.Tourism=length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                          !is.na(PrimaryLivelihood)]),
            PrimaryOcc.Other=length(PrimaryLivelihood[(PrimaryLivelihood==996 | PrimaryLivelihood==4 | 
                                                         PrimaryLivelihood==5) & !is.na(PrimaryLivelihood)]),
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
            ProteinFish.All=length(PercentProteinFish[PercentProteinFish==5 &  !is.na(PercentProteinFish)]))

Flotim.FreqTables <- 
  as.data.frame(t(Flotim.FreqTables[,-1]))
colnames(Flotim.FreqTables) <- c("t0","t3")

Flotim.FreqTables$Category <- rownames(Flotim.FreqTables)
Flotim.FreqTables$Variable <- ifelse(grepl("PrimaryOcc",Flotim.FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",Flotim.FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",Flotim.FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",Flotim.FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",Flotim.FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("child",Flotim.FreqTables$Category)==T,"child",
                                                                      ifelse(grepl("Protein",Flotim.FreqTables$Category)==T,"Protein",NA)))))))
                                   

# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Flotim <- factor(Flotim.TechReport.Status.SettlementMeans$SettlementName)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Flotim <- 
  mapply(a=Flotim.TechReport.Status.SettlementMeans[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         b=Flotim.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Flotim.TechReport.Status.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Flotim.TechReport.Status.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Flotim.TechReport.Status.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
    
        
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Flotim.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.Flotim <- 
  mapply(i=Flotim.TechReport.Status.SettlementMeans[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         j=names(even.number.setts.function.Flotim),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Flotim)%%2!=0,
                                      as.character(Flotim.TechReport.Status.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Flotim[j])),
                               levels=levels(sett.names.Flotim))})
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Food security score distribution ----

dist.Flotim.FS <- 
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Flotim: Food Security Score Distribution, 2017") +
  dist.plot.theme
dist.Flotim.FS

qqnorm(Flotim.TechReport.MPAHouseholdData$FSIndex)
qqline(Flotim.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Flotim.MA <- 
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Flotim: HH Material Assets Distribution, 2017") +
  dist.plot.theme
dist.Flotim.MA

log.MA <- log(Flotim.TechReport.MPAHouseholdData$MAIndex[Flotim.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Flotim.PA <- 
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Flotim: Place Attachment Score Distribution, 2017") +
  dist.plot.theme
dist.Flotim.PA

qqnorm(Flotim.TechReport.MPAHouseholdData$PAIndex)
qqline(Flotim.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----
dist.Flotim.MT <- 
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Flotim: Marine Tenure Score Distribution, 2017") +
  dist.plot.theme
dist.Flotim.MT

qqnorm(Flotim.TechReport.MPAHouseholdData$MTIndex)
qqline(Flotim.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Flotim.SE <-
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Flotim: School Enrollment Rate Distribution, 2017") +
  dist.plot.theme
dist.Flotim.SE

qqnorm(Flotim.TechReport.MPAHouseholdData$SERate)
qqline(Flotim.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Flotim.TimeMarket <- 
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarket,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$TimeMarket),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$TimeMarket)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Flotim: Time to Market Distribution, 2017") +
  dist.plot.theme
dist.Flotim.TimeMarket

qqnorm(Flotim.TechReport.MPAHouseholdData$TimeMarket)
qqline(Flotim.TechReport.MPAHouseholdData$TimeMarket,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Flotim.DaysUnwell <- 
  ggplot(Flotim.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Flotim.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Flotim.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Flotim: HH Days Unwell Distribution, 2017") +
  dist.plot.theme
dist.Flotim.DaysUnwell

qqnorm(Flotim.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Flotim.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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
non.parametric.test.settlements.Flotim <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Flotim[which(sett.names.Flotim!=median.setts.Flotim[a])]),
                                                               as.character(median.setts.Flotim[a])),
                                    
                                          rbind.data.frame(t(data.frame(mapply(i=sett.names.Flotim[which(sett.names.Flotim!=median.setts.Flotim[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Flotim.TechReport.MPAHouseholdData[Flotim.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Flotim.TechReport.MPAHouseholdData$SettlementName==median.setts.Flotim[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Flotim.TechReport.MPAHouseholdData[Flotim.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Flotim.TechReport.MPAHouseholdData$SettlementName==median.setts.Flotim[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                    )}))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Flotim <- 
  cbind.data.frame(non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"MTIndex.SettlementName"),
                                                          c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"PAIndex.SettlementName"),
                                                          c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"TimeMarket.SettlementName"),
                                                          c("TimeMarket.SettlementName","TimeMarket.p.value")],
                   non.parametric.test.settlements.Flotim[order(non.parametric.test.settlements.Flotim$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Flotim <- 
  dplyr:: select(sigvals.Sett.Flotim,"FSIndex.SettlementName", "FSIndex.p.value", "MAIndex.p.value",  
  "PAIndex.p.value","MTIndex.p.value", "SERate.p.value", "TimeMarket.p.value",
  "DaysUnwell.p.value")


colnames(sigvals.Sett.Flotim) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","TimeMarket.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Flotim <-
  data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a){
                      var <- Flotim.TechReport.MPAvControl[,a]
                      wilcox.test(var~Treatment,
                                  data=Flotim.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Flotim <- 
  cbind.data.frame("Flores Timur\n MPA",non.parametric.test.MPAvControl.Flotim)

colnames(sigvals.MPA.Flotim) <- colnames(sigvals.Sett.Flotim)

null.row.sigvals.Flotim <- 
  matrix(rep(NA,length(sigvals.MPA.Flotim)),ncol=length(sigvals.MPA.Flotim),
         dimnames=list(1,colnames(sigvals.Sett.Flotim)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)
sigvals.Sett.Flotim <- sigvals.Sett.Flotim %>%
  arrange(rev(order(sigvals.Sett.Flotim$SettlementName)))

sigvals.Flotim <- rbind.data.frame(sigvals.MPA.Flotim,
                   null.row.sigvals.Flotim,
                   sigvals.Sett.Flotim)


sigvals.Flotim[,c("FS.pval", "MA.pval", "MT.pval" , "PA.pval", "SE.pval", "TimeMarket.pval",
                "Unwell.pval")] <- unlist(sigvals.Flotim[,c("FS.pval", "MA.pval", "MT.pval", "PA.pval","SE.pval","TimeMarket.pval",
                                                          "Unwell.pval")])

# ---- 4.3 Define function for trend data significance ---- 

trend.non.parametric.test.byMPA.Flotim <- 
  data.frame(mapply(i=Flotim.Trend.Data[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Flotim.Trend.Data$InterviewYear==unique(Flotim.Trend.Data$InterviewYear)[1]],
                                    i[Flotim.Trend.Data$InterviewYear==unique(Flotim.Trend.Data$InterviewYear)[2]]))
                    }))

trend.non.parametric.test.byControl.Flotim <- 
  data.frame(mapply(i=Flotim.Trend.Data[Flotim.Trend.Data$Treatment==0,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Flotim.Trend.Data$InterviewYear==unique(Flotim.Trend.Data$InterviewYear)[1]],
                                    i[Flotim.Trend.Data$InterviewYear==unique(Flotim.Trend.Data$InterviewYear)[2]]))
                    }))


colnames(trend.non.parametric.test.byMPA.Flotim) <- colnames(sigvals.Flotim[2:8])
colnames(trend.non.parametric.test.byControl.Flotim) <- colnames(sigvals.Flotim[2:8])


trend.sigvals.Flotim <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Flotim["sl",1],NA,trend.non.parametric.test.byMPA.Flotim["sl",2],
                   NA,trend.non.parametric.test.byMPA.Flotim["sl",3],NA,trend.non.parametric.test.byMPA.Flotim["sl",4],NA,trend.non.parametric.test.byMPA.Flotim["sl",5],
                   NA,trend.non.parametric.test.byMPA.Flotim["sl",6],NA,trend.non.parametric.test.byMPA.Flotim["sl",7],NA)

colnames(trend.sigvals.Flotim) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "MarketMean","MarketErr","UnwellMean","UnwellErr")

trend.sigvals.Flotim <- unlist(trend.sigvals.Flotim)


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.Flotim <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Flotim),
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Flotim),
                                                function(i){
                                                  MannKendall(c(Flotim.Trend.Data[Flotim.Trend.Data$SettlementName==i &
                                                                                  Flotim.Trend.Data$InterviewYear==unique(Flotim.Trend.Data$InterviewYear)[1],a],
                                                                Flotim.Trend.Data[Flotim.Trend.Data$SettlementName==i &
                                                                                  Flotim.Trend.Data$InterviewYear==unique(Flotim.Trend.Data$InterviewYear)[2],a]))
                                                }))["sl",])}))



colnames(trend.non.parametric.test.bySett.Flotim) <- colnames(sigvals.Flotim[1:8])


# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Flotim <- 
  rbind.data.frame(cbind.data.frame(SettlementName="Flores Timur\nMPA",trend.non.parametric.test.byMPA.Flotim["sl",]),
                   cbind.data.frame(SettlementName=c("Control\n Settlements"),trend.non.parametric.test.byControl.Flotim["sl",]),
                   null.row.sigvals.Flotim,
                   trend.non.parametric.test.bySett.Flotim[rev(order(trend.non.parametric.test.bySett.Flotim$SettlementName)),])

annex.sigvals.Flotim[2:8] <- unlist(annex.sigvals.Flotim[2:8])


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

propdata.trend.test.Flotim <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA


propdata.trend.test.Flotim <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","child","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(Flotim.FreqTables$t0[Flotim.FreqTables$Variable==a])==0) {
                             Flotim.FreqTables$t3[Flotim.FreqTables$Variable==a &
                                                  Flotim.FreqTables$t3!=0] 
                           } else {Flotim.FreqTables$t0[Flotim.FreqTables$Variable==a &
                                                        Flotim.FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(Flotim.FreqTables$t0[Flotim.FreqTables$Variable==a])==0) {
                             Flotim.FreqTables$t3[Flotim.FreqTables$Variable==a]
                           } else {Flotim.FreqTables$t3[Flotim.FreqTables$Variable==a &
                                                        Flotim.FreqTables$t0!=0]}
                         propdata.trend.test.Flotim[a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test.Flotim[a] <- ifelse(is.na(propdata.trend.test.Flotim[a]),100,propdata.trend.test.Flotim[a])
                       }))
colnames(propdata.trend.test.Flotim) <- c("Primary occupation (% households)","Frequency of fishing (% households)","Frequency of selling at least some catch (% households)",
                                          "Income from fishing in past 6 months (% households)","Fishing technique most often used in past 6 months (% households)","Child hunger (% households)","Dietary protein from fish in past 6 months (% households)")

