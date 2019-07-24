# 
# code:  Alor Technical Report Significance Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: December 2017
# modified for Alor: Amari Bauer, June 2019
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
source("C:/Users/HP/Dropbox/NotThisOne/Source_social_data_flat_files.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/SBS_TechReport_Calculations.R")

# ---- 1.1 Subset continuous variable datasets from SBS.TechReport.Calculations.R ----


MPA.TechReport.SigTest.Data <- 
  left_join(dplyr:: select(BigFive,"HouseholdID", "MPAID", "SettlementName", "Treatment", "InterviewYear", 
                           "MonitoringYear", "MAIndex", "FSIndex", "PAIndex",
                           "SERate"),
            dplyr:: select(HHData, "HouseholdID", "TimeMarket"))

MPA.TechReport.SigTest.Trend.Data <- MPA.TechReport.SigTest.Data


MPA.TechReport.SigTest.Data <- MPA.TechReport.SigTest.Data %>%
  filter(MonitoringYear=="3 Year Post")


# - "MPA Household Data" dataset
Alor.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==15,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==15,
                                  c("HouseholdID","DaysUnwell")])

Alor.TechReport.MPAHouseholdData <- Alor.TechReport.MPAHouseholdData %>% filter(SettlementName != "Alila Timur") 

Alor.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Alor.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus Control" datasets
Alor.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==15,],
            Days.unwell[Days.unwell$MPAID==15,
                        c("HouseholdID","DaysUnwell")])

Techreport.ByMPA.control.annex <- Techreport.ByMPA.control.annex %>% 
  filter(MPAID==15)
BigFive.ControlGroup.annex <-BigFive.ControlGroup.annex %>% filter(MPAID==15)

Techreport.ByMPA.control.annex <- left_join(Techreport.ByMPA.control.annex,BigFive.ControlGroup.annex[2:12])


Days.unwell.Alor.control.annex <- Days.unwell.Alor.control.annex %>%
  group_by(MPAID,MonitoringYear) %>%
  filter(MPAID==15) %>%
  summarise(UnwellMean=mean(DaysUnwell,na.rm=T),
            UnwellErr=sd(DaysUnwell,na.rm=T)/sqrt(length(DaysUnwell)))

Techreport.ByMPA.control.annex <- left_join(Techreport.ByMPA.control.annex,Days.unwell.Alor.control.annex[2:4])

# - "Settlement Means" dataset
Alor.TechReport.Status.SettlementMeans<-
  left_join(BigFive.bySett[BigFive.bySett$Treatment==1 &
                             BigFive.bySett$MonitoringYear=="3 Year Post" &
                             BigFive.bySett$MPAID==15,],
            Techreport.Status.BySett[Techreport.Status.BySett$MPAID==15 & Techreport.Status.BySett$MonitoringYear=="3 Year Post",
                                     c("SettlementID","SettlementName","TimeMarketMean")])

Alor.TechReport.Status.SettlementMeans <- Alor.TechReport.Status.SettlementMeans %>% filter(SettlementID != 116) 

Days.unwell.BySett.Status <- Days.unwell.BySett %>% 
  filter(MonitoringYear=="3 Year Post" & MPAID==15)

Alor.TechReport.Status.SettlementMeans <- 
  left_join(Alor.TechReport.Status.SettlementMeans,
            Days.unwell.BySett.Status[Days.unwell.BySett.Status$MPAID==15,c("SettlementID", "UnwellMean","MonitoringYear")])


#Removing the settlement with an NA in order for function to run
Alor.TechReport.Status.SettlementMeans <- 
  Alor.TechReport.Status.SettlementMeans[!is.na(Alor.TechReport.Status.SettlementMeans$SettlementName),]



colnames(Alor.TechReport.Status.SettlementMeans) <- c(colnames(Alor.TechReport.Status.SettlementMeans)[1:5],
                                                      "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr", "MTMean","MTErr",
                                                      "SERate","SEErr","TimeMarket","DaysUnwell")

# - "Trend" dataset
Alor.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Trend.Data[
    MPA.TechReport.SigTest.Trend.Data$MPAID==15,],
    Days.unwell[Days.unwell$MPAID==15,1:2],
    by="HouseholdID","Treatment") 

Alor.FreqTables <- 
  HHDemos.context[HHDemos.context$MPAID==15,] %>%
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

Alor.FreqTables <- 
  as.data.frame(t(Alor.FreqTables[,-1]))
colnames(Alor.FreqTables) <- c("t0","t3")

Alor.FreqTables$Category <- rownames(Alor.FreqTables)
Alor.FreqTables$Variable <- ifelse(grepl("PrimaryOcc",Alor.FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",Alor.FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",Alor.FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",Alor.FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",Alor.FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("child",Alor.FreqTables$Category)==T,"child",
                                                                      ifelse(grepl("Protein",Alor.FreqTables$Category)==T,"Protein",NA)))))))


# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Alor <- factor(Alor.TechReport.Status.SettlementMeans$SettlementName)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Alor <- 
  mapply(a=Alor.TechReport.Status.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell")],
         b=Alor.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Alor.TechReport.Status.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Alor.TechReport.Status.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Alor.TechReport.Status.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Alor.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.Alor <- 
  mapply(i=Alor.TechReport.Status.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell")],
         j=names(even.number.setts.function.Alor),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Alor)%%2!=0,
                                      as.character(Alor.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Alor[j])),
                               levels=levels(sett.names.Alor))})
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Food security score distribution ----

dist.Alor.FS <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Alor: Food Security Score Distribution, 2017") +
  dist.plot.theme
dist.Alor.FS

qqnorm(Alor.TechReport.MPAHouseholdData$FSIndex)
qqline(Alor.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Alor.MA <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Alor: HH Material Assets Distribution, 2017") +
  dist.plot.theme
dist.Alor.MA

log.MA <- log(Alor.TechReport.MPAHouseholdData$MAIndex[Alor.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Alor.PA <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Alor: Place Attachment Score Distribution, 2017") +
  dist.plot.theme
dist.Alor.PA

qqnorm(Alor.TechReport.MPAHouseholdData$PAIndex)
qqline(Alor.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----
#currently not included, #CHANGE BACK TO ALOR TECHREPORT DATA WHEN ALL IS FIGURED OUT
dist.Alor.MT <- 
  ggplot(BigFive %>% filter(MonitoringYear=="3 Year Post" & MPAID==15)) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(BigFive$MTIndex),
                                    sd=sd(BigFive$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Alor: Marine Tenure Score Distribution, 2017") +
  dist.plot.theme
dist.Alor.MT

qqnorm(Alor.TechReport.MPAHouseholdData$MTIndex)
qqline(Alor.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Alor.SE <-
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Alor: School Enrollment Rate Distribution, 2017") +
  dist.plot.theme
dist.Alor.SE

qqnorm(Alor.TechReport.MPAHouseholdData$SERate)
qqline(Alor.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Alor.TimeMarket <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarket,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$TimeMarket),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$TimeMarket)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Alor: Time to Market Distribution, 2017") +
  dist.plot.theme
dist.Alor.TimeMarket

qqnorm(Alor.TechReport.MPAHouseholdData$TimeMarket)
qqline(Alor.TechReport.MPAHouseholdData$TimeMarket,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Alor.DaysUnwell <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Alor: HH Days Unwell Distribution, 2017") +
  dist.plot.theme
dist.Alor.DaysUnwell

qqnorm(Alor.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Alor.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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
non.parametric.test.settlements.Alor <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Alor[which(sett.names.Alor!=median.setts.Alor[a])]),
                                                               as.character(median.setts.Alor[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Alor[which(sett.names.Alor!=median.setts.Alor[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Alor.TechReport.MPAHouseholdData[Alor.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Alor.TechReport.MPAHouseholdData$SettlementName==median.setts.Alor[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Alor.TechReport.MPAHouseholdData[Alor.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Alor.TechReport.MPAHouseholdData$SettlementName==median.setts.Alor[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Alor <- 
  cbind.data.frame(non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Alor <- 
  dplyr:: select(sigvals.Sett.Alor,"FSIndex.SettlementName", "FSIndex.p.value", "MAIndex.p.value", 
                 "PAIndex.p.value", "SERate.p.value", 
                 "DaysUnwell.p.value")


colnames(sigvals.Sett.Alor) <- c("SettlementName","FS.pval","MA.pval","PA.pval","SE.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Alor <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell"),
                    function(a){
                      var <- Alor.TechReport.MPAvControl[,a]
                      wilcox.test(var~Treatment,
                                  data=Alor.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Alor <- 
  cbind.data.frame("Alor MPA",non.parametric.test.MPAvControl.Alor)

colnames(sigvals.MPA.Alor) <- colnames(sigvals.Sett.Alor)

null.row.sigvals.Alor <- 
  matrix(rep(NA,length(sigvals.MPA.Alor)),ncol=length(sigvals.MPA.Alor),
         dimnames=list(1,colnames(sigvals.Sett.Alor)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)
sigvals.Sett.Alor <- sigvals.Sett.Alor %>%
  arrange(rev(order(sigvals.Sett.Alor$SettlementName)))

sigvals.Alor <- rbind.data.frame(sigvals.MPA.Alor,
                                 null.row.sigvals.Alor,
                                 sigvals.Sett.Alor)


sigvals.Alor[,c("FS.pval", "MA.pval", "PA.pval",  "SE.pval", 
                "Unwell.pval")] <- unlist(sigvals.Alor[,c("FS.pval", "MA.pval", "PA.pval", "SE.pval", 
                                                          "Unwell.pval")])

# ---- 4.3 Define function for trend data significance ---- 

trend.non.parametric.test.byMPA.Alor <- 
  data.frame(mapply(i=Alor.Trend.Data[Alor.Trend.Data$Treatment==1,c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Alor.Trend.Data$InterviewYear==unique(Alor.Trend.Data$InterviewYear)[1]],
                                    i[Alor.Trend.Data$InterviewYear==unique(Alor.Trend.Data$InterviewYear)[2]]))
                    }))

trend.non.parametric.test.byControl.Alor <- 
  data.frame(mapply(i=Alor.Trend.Data[Alor.Trend.Data$Treatment==0,c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Alor.Trend.Data$InterviewYear==unique(Alor.Trend.Data$InterviewYear)[1]],
                                    i[Alor.Trend.Data$InterviewYear==unique(Alor.Trend.Data$InterviewYear)[2]]))
                    }))

colnames(trend.non.parametric.test.byMPA.Alor) <- colnames(sigvals.Alor[2:6])
colnames(trend.non.parametric.test.byControl.Alor) <- colnames(sigvals.Alor[2:6])

trend.sigvals.Alor <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Alor["sl",1],NA,trend.non.parametric.test.byMPA.Alor["sl",2],
                   NA,trend.non.parametric.test.byMPA.Alor["sl",3],NA,trend.non.parametric.test.byMPA.Alor["sl",4],NA,trend.non.parametric.test.byMPA.Alor["sl",5],
                   NA)

colnames(trend.sigvals.Alor) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","SEMean","SEErr",
                                  "UnwellMean","UnwellErr")

trend.sigvals.Alor <- unlist(trend.sigvals.Alor)


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.Alor <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Alor),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","SERate","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Alor),
                                                function(i){
                                                  MannKendall(c(Alor.Trend.Data[Alor.Trend.Data$SettlementName==i &
                                                                                  Alor.Trend.Data$InterviewYear==unique(Alor.Trend.Data$InterviewYear)[1],a],
                                                                Alor.Trend.Data[Alor.Trend.Data$SettlementName==i &
                                                                                  Alor.Trend.Data$InterviewYear==unique(Alor.Trend.Data$InterviewYear)[2],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.Alor) <- colnames(sigvals.Alor)

# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Alor <- 
  rbind.data.frame(cbind.data.frame(SettlementName=c("Alor MPA"),trend.non.parametric.test.byMPA.Alor["sl",]),
                   cbind.data.frame(SettlementName=c("Control Settlements"),trend.non.parametric.test.byControl.Alor["sl",]),
                   null.row.sigvals.Alor,
                   trend.non.parametric.test.bySett.Alor[rev(order(trend.non.parametric.test.bySett.Alor$SettlementName)),])

annex.sigvals.Alor[2:6] <- unlist(annex.sigvals.Alor[2:6])


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

propdata.trend.test.Alor <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA

propdata.trend.test.Alor <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","child","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(Alor.FreqTables$t0[Alor.FreqTables$Variable==a])==0) {
                             Alor.FreqTables$t3[Alor.FreqTables$Variable==a &
                                                  Alor.FreqTables$t3!=0] 
                           } else {Alor.FreqTables$t0[Alor.FreqTables$Variable==a &
                                                        Alor.FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(Alor.FreqTables$t0[Alor.FreqTables$Variable==a])==0) {
                             Alor.FreqTables$t3[Alor.FreqTables$Variable==a]
                           } else {Alor.FreqTables$t3[Alor.FreqTables$Variable==a &
                                                        Alor.FreqTables$t0!=0]}
                         propdata.trend.test.Alor[a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test.Alor[a] <- ifelse(is.na(propdata.trend.test.Alor[a]),100,propdata.trend.test.Alor[a])
                       }))
colnames(propdata.trend.test.Alor) <- c("Primary occupation (% households)","Frequency of fishing (% households)",
                                        "Frequency of selling at least some catch (% households)","Income from fishing in past 6 months (% households)",
                                        "Fishing technique most often used in past 6 months (% households)","Child hunger (% households)","Dietary protein from fish in past 6 months (% households)")

