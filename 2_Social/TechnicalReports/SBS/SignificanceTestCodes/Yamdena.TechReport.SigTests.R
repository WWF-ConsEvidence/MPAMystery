# 
# code:  Yamdena Technical Report Significance Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: December 2017
# 
# 
# ---- inputs ----
#  Dependencies: SBS_MPA_Mystery.R
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
Yamdena.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==19,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==19,
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Yamdena.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Yamdena.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus Control" dataset
Yamdena.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==19,],
                                              Days.unwell[Days.unwell$MPAID==19,
                                                          c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID")

# - "Settlement Means" dataset
Yamdena.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MPAID==19,],
            Techreport.BySett[Techreport.BySett$MPAID==19,
                              c("SettlementID","SettlementName","TimeMarketMean")],  
            by=c("SettlementID","SettlementName"))

Yamdena.TechReport.SettlementMeans <- 
  left_join(Yamdena.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==19,c(1,4)],
            by="SettlementID")

Yamdena.TechReport.SettlementMeans <- 
  Yamdena.TechReport.SettlementMeans[!is.na(Yamdena.TechReport.SettlementMeans$SettlementName),]

colnames(Yamdena.TechReport.SettlementMeans) <- c(colnames(Yamdena.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")


# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Yamdena <- factor(Yamdena.TechReport.SettlementMeans$SettlementName)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Yamdena <- 
  mapply(a=Yamdena.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Yamdena.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Yamdena.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Yamdena.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Yamdena.TechReport.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Yamdena.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.Yamdena <- 
  mapply(i=Yamdena.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.Yamdena),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Yamdena)%%2!=0,
                                      as.character(Yamdena.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Yamdena[j])),
                               levels=levels(sett.names.Yamdena))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.Yamdena.FS <- 
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Yamdena: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Yamdena.TechReport.MPAHouseholdData$FSIndex)
qqline(Yamdena.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Yamdena.MA <- 
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Yamdena: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(Yamdena.TechReport.MPAHouseholdData$MAIndex[Yamdena.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Yamdena.PA <- 
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Yamdena: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Yamdena.TechReport.MPAHouseholdData$PAIndex)
qqline(Yamdena.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Yamdena.MT <- 
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Yamdena: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Yamdena.TechReport.MPAHouseholdData$MTIndex)
qqline(Yamdena.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Yamdena.SE <-
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Yamdena: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(Yamdena.TechReport.MPAHouseholdData$SERate)
qqline(Yamdena.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Yamdena.TimeMarket <- 
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Yamdena: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(Yamdena.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(Yamdena.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Yamdena.DaysUnwell <- 
  ggplot(Yamdena.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Yamdena.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Yamdena.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Yamdena: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(Yamdena.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Yamdena.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.Yamdena <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Yamdena[which(sett.names.Yamdena!=median.setts.Yamdena[a])]),
                                                               as.character(median.setts.Yamdena[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Yamdena[which(sett.names.Yamdena!=median.setts.Yamdena[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Yamdena.TechReport.MPAHouseholdData[Yamdena.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Yamdena.TechReport.MPAHouseholdData$SettlementName==median.setts.Yamdena[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Yamdena.TechReport.MPAHouseholdData[Yamdena.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Yamdena.TechReport.MPAHouseholdData$SettlementName==median.setts.Yamdena[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Yamdena <- 
  cbind.data.frame(non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.Yamdena[order(non.parametric.test.settlements.Yamdena$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Yamdena <- 
  sigvals.Sett.Yamdena[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Yamdena) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Yamdena <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Yamdena.TechReport.MPAvControl[,a]
                      wilcox.test(var~Treatment,
                                  data=Yamdena.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Yamdena <- 
  cbind.data.frame("Selat Pantar MPA",non.parametric.test.MPAvControl.Yamdena)

colnames(sigvals.MPA.Yamdena) <- colnames(sigvals.Sett.Yamdena)

null.row.sigvals.Yamdena <- 
  matrix(rep(NA,length(sigvals.MPA.Yamdena)),ncol=length(sigvals.MPA.Yamdena),
         dimnames=list(NULL,colnames(sigvals.MPA.Yamdena)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)
sigvals.Yamdena <- 
  rbind.data.frame(sigvals.MPA.Yamdena,
                   null.row.sigvals.Yamdena,
                   sigvals.Sett.Yamdena[rev(order(sigvals.Sett.Yamdena$SettlementName)),])

sigvals.Yamdena[,2:8] <- unlist(sigvals.Yamdena[,2:8])





# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Yamdena.TechReport.MPAHouseholdData)
rm(Yamdena.TechReport.MPAvSBS)
rm(Yamdena.TechReport.SettlementMeans)
rm(Yamdena.Trend.Data)
rm(Yamdena.FreqTables)
rm(even.number.setts.function.Yamdena)
rm(non.parametric.test.settlements.Yamdena)
rm(non.parametric.test.MPAvBHS.Yamdena)
rm(trend.non.parametric.test.byMPA.Yamdena)
rm(trend.non.parametric.test.bySett.Yamdena)
rm(null.row.sigvals.Yamdena)
rm(sigvals.MPA.Yamdena)
rm(sigvals.Sett.Yamdena)
rm(p.for.function)
rm(data.for.function)