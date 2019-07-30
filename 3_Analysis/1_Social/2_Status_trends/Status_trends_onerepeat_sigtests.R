# 
# code:  Status & Trends Significance Tests
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: July 2019
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
  filter(.,Treatment==1 & InterviewYear==2017)

MPA.HHData$SettlementName <- 
  factor(MPA.HHData$SettlementName)


# - "MPA versus Control" dataset -- includes household level data for treatment and control, for most recent year only
MPA.v.Control <- 
  HHData %>%
  filter(InterviewYear==2017)


# - "MPA Settlement Means" dataset -- includes settlement level data for only treatment settlements from most recent year
MPA.Sett.Means<-
  Sett.Level.Means %>%
  filter(.,Treatment==1 & InterviewYear==2017)

# Removing the settlement with an NA in order for function to run
MPA.Sett.Means <- 
  MPA.Sett.Means[!is.na(MPA.Sett.Means$SettlementName),]


# - Frequency tables for chi-square tests

FreqTables <- 
  HHData %>%
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

FreqTables <- 
  as.data.frame(t(FreqTables[,-1]))

colnames(FreqTables) <- c("t0","repeat1")

FreqTables$Category <- rownames(FreqTables)
FreqTables$Variable <- ifelse(grepl("PrimaryOcc",FreqTables$Category)==T,"PrimaryOcc",
                                   ifelse(grepl("SellFish",FreqTables$Category)==T,"SellFish",
                                          ifelse(grepl("IncFish",FreqTables$Category)==T,"IncFish",
                                                 ifelse(grepl("FishTech",FreqTables$Category)==T,"FishTech",
                                                        ifelse(grepl("FreqFish",FreqTables$Category)==T,"FreqFish",
                                                               ifelse(grepl("child",FreqTables$Category)==T,"child",
                                                                      ifelse(grepl("Protein",FreqTables$Category)==T,"Protein",NA)))))))
                                   

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
  mapply(a=MPA.Sett.Means[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         b=MPA.HHData[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- MPA.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
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
                                       as.character(upper.sett)))
           lower.sett <- MPA.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
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
                                       as.character(lower.sett)))
           sett.equal.med <- MPA.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
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
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
    
        
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[MPA.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[MPA.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts <- 
  mapply(i=MPA.Sett.Means[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
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

# ---- 3.1 Food security score distribution ----

dist.FS <- 
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$FSIndex),
                                    sd=sd(MPA.HHData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title=paste("Food Security Score Distribution",unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.FS

qqnorm(MPA.HHData$FSIndex)
qqline(MPA.HHData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.MA <- 
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$MAIndex),
                                    sd=sd(MPA.HHData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title=paste("HH Material Assets Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.MA

log.MA <- log(MPA.HHData$MAIndex[MPA.HHData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.PA <- 
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$PAIndex),
                                    sd=sd(MPA.HHData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title=paste("Place Attachment Score Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.PA

qqnorm(MPA.HHData$PAIndex)
qqline(MPA.HHData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----
dist.MT <- 
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$MTIndex),
                                    sd=sd(MPA.HHData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title=paste("Marine Tenure Score Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.MT

qqnorm(MPA.HHData$MTIndex)
qqline(MPA.HHData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.SE <-
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$SERate),
                                    sd=sd(MPA.HHData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title=paste("School Enrollment Rate Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.SE

qqnorm(MPA.HHData$SERate)
qqline(MPA.HHData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.TimeMarket <- 
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=TimeMarket,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$TimeMarket),
                                    sd=sd(MPA.HHData$TimeMarket)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title=paste("Time to Market Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.TimeMarket

qqnorm(MPA.HHData$TimeMarket)
qqline(MPA.HHData$TimeMarket,col="green")


# ---- 3.7 Days unwell distribution ----

dist.DaysUnwell <- 
  ggplot(MPA.HHData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$DaysUnwell),
                                    sd=sd(MPA.HHData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title=paste("HH Days Unwell Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
  dist.plot.theme
dist.DaysUnwell

qqnorm(MPA.HHData$DaysUnwell)
qqline(MPA.HHData$DaysUnwell,col="green")


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
  data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names[which(sett.names!=median.setts[a])]),
                                                               as.character(median.setts[a])),
                                    
                                          rbind.data.frame(t(data.frame(mapply(i=sett.names[which(sett.names!=median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       MPA.HHData[MPA.HHData$SettlementName==i | MPA.HHData$SettlementName==median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=MPA.HHData[MPA.HHData$SettlementName==i | MPA.HHData$SettlementName==median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                    )}))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett  <- 
  cbind.data.frame(non.parametric.test.settlements[order(non.parametric.test.settlements$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"MTIndex.SettlementName"),
                                                          c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"PAIndex.SettlementName"),
                                                          c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"TimeMarket.SettlementName"),
                                                          c("TimeMarket.SettlementName","TimeMarket.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett <- 
  dplyr:: select(sigvals.Sett,"FSIndex.SettlementName", "FSIndex.p.value", "MAIndex.p.value",  
  "PAIndex.p.value","MTIndex.p.value", "SERate.p.value", "TimeMarket.p.value",
  "DaysUnwell.p.value")


colnames(sigvals.Sett) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","TimeMarket.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl  <-
  data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a){
                      var <- MPA.v.Control[,a]
                      wilcox.test(var~Treatment,
                                  data=MPA.v.Control,
                                  exact=F)}))["p.value",]

sigvals.MPA  <- 
  cbind.data.frame(MPA.name$MPAName,non.parametric.test.MPAvControl )

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
sigvals.Sett <- sigvals.Sett %>%
  arrange(rev(order(sigvals.Sett$SettlementName)))

sigvals <- rbind.data.frame(sigvals.MPA,
                   null.row.sigvals,
                   sigvals.Sett)


sigvals[,c("FS.pval", "MA.pval", "MT.pval" , "PA.pval", "SE.pval", "TimeMarket.pval", "Unwell.pval")] <- 
  unlist(sigvals[,c("FS.pval", "MA.pval", "MT.pval", "PA.pval","SE.pval","TimeMarket.pval","Unwell.pval")])

# ---- 4.3 Define function for trend data significance ---- 

trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=HHData[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[HHData$InterviewYear==unique(HHData$InterviewYear)[1]],
                                    i[HHData$InterviewYear==unique(HHData$InterviewYear)[2]]))
                    }))

trend.non.parametric.test.byControl  <- 
  data.frame(mapply(i=HHData[HHData$Treatment==0,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[HHData$InterviewYear==unique(HHData$InterviewYear)[1]],
                                    i[HHData$InterviewYear==unique(HHData$InterviewYear)[2]]))
                    }))


colnames(trend.non.parametric.test.byMPA ) <- colnames(sigvals[2:8])
colnames(trend.non.parametric.test.byControl ) <- colnames(sigvals[2:8])


trend.sigvals  <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA ["sl",1],NA,trend.non.parametric.test.byMPA ["sl",2],
                   NA,trend.non.parametric.test.byMPA ["sl",3],NA,trend.non.parametric.test.byMPA ["sl",4],NA,trend.non.parametric.test.byMPA ["sl",5],
                   NA,trend.non.parametric.test.byMPA ["sl",6],NA,trend.non.parametric.test.byMPA ["sl",7],NA)

colnames(trend.sigvals ) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "MarketMean","MarketErr","UnwellMean","UnwellErr")

trend.sigvals  <- unlist(trend.sigvals )


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
                                                                                  HHData$InterviewYear==unique(HHData$InterviewYear)[2],a]))
                                                }))["sl",])}))



colnames(trend.non.parametric.test.bySett) <- colnames(sigvals[1:8])


# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals  <- 
  rbind.data.frame(cbind.data.frame(SettlementName=MPA.name$MPAName,trend.non.parametric.test.byMPA ["sl",]),
                   cbind.data.frame(SettlementName=c("Control\n Settlements"),trend.non.parametric.test.byControl ["sl",]),
                   null.row.sigvals ,
                   trend.non.parametric.test.bySett[rev(order(trend.non.parametric.test.bySett $SettlementName)),])

annex.sigvals [2:8] <- unlist(annex.sigvals [2:8])


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

propdata.trend.test  <- data.frame(PrimaryOcc=NA,FreqFish=NA,SellFish=NA,IncFish=NA,FishTech=NA,ChildFS=NA,Protein=NA)
p.for.function <- NA
data.for.function <- NA


propdata.trend.test  <- 
  as.data.frame(mapply(a=c("PrimaryOcc","FreqFish","SellFish","IncFish","FishTech","child","Protein"),
                       function(a) {
                         p.for.function <- 
                           if(sum(FreqTables$t0[FreqTables$Variable==a])==0) {
                             FreqTables$repeat1[FreqTables$Variable==a &
                                                  FreqTables$repeat1!=0] 
                           } else {FreqTables$t0[FreqTables$Variable==a &
                                                        FreqTables$t0!=0] }
                         data.for.function <- 
                           if(sum(FreqTables$t0[FreqTables$Variable==a])==0) {
                             FreqTables$repeat1[FreqTables$Variable==a]
                           } else {FreqTables$repeat1[FreqTables$Variable==a &
                                                        FreqTables$t0!=0]}
                         propdata.trend.test [a] <- ifelse(length(data.for.function)>1,
                                                               chisq.test(data.for.function,
                                                                          p=p.for.function,
                                                                          rescale.p=TRUE,correct=TRUE)["p.value"],
                                                               NA)
                         propdata.trend.test [a] <- ifelse(is.na(propdata.trend.test [a]),100,propdata.trend.test [a])
                       }))
colnames(propdata.trend.test ) <- c("Primary occupation (% households)","Frequency of fishing (% households)","Frequency of selling at least some catch (% households)",
                                          "Income from fishing in past 6 months (% households)","Fishing technique most often used in past 6 months (% households)","Child hunger (% households)","Dietary protein from fish in past 6 months (% households)")

