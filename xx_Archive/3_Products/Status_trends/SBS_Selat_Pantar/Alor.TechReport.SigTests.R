# 
# code:  Alorier Technical Report Significance Tests
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
Alor.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==15,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==15,
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Alor.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Alor.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus Control" dataset
Alor.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==15,],
                                              Days.unwell[Days.unwell$MPAID==15,
                                                          c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID")

# - "Settlement Means" dataset
Alor.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MPAID==15,],
            Techreport.BySett[Techreport.BySett$MPAID==15,
                              c("SettlementID","SettlementName","TimeMarketMean")],  
            by=c("SettlementID","SettlementName"))

Alor.TechReport.SettlementMeans <- 
  left_join(Alor.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==15,c(1,4)],
            by="SettlementID")

Alor.TechReport.SettlementMeans <- 
  Alor.TechReport.SettlementMeans[!is.na(Alor.TechReport.SettlementMeans$SettlementName),]

colnames(Alor.TechReport.SettlementMeans) <- c(colnames(Alor.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")


# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Alor <- factor(Alor.TechReport.SettlementMeans$SettlementName)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Alor <- 
  mapply(a=Alor.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Alor.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Alor.TechReport.SettlementMeans$SettlementName[a==upper]
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
           lower.sett <- Alor.TechReport.SettlementMeans$SettlementName[a==lower]
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
           sett.equal.med <- Alor.TechReport.SettlementMeans$SettlementName[a==equal]
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
  mapply(i=Alor.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
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
       title="Alor: Food Security Score Distribution, 2014") +
  dist.plot.theme

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
       title="Alor: HH Material Assets Distribution, 2014") +
  dist.plot.theme

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
       title="Alor: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Alor.TechReport.MPAHouseholdData$PAIndex)
qqline(Alor.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Alor.MT <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Alor: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

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
       title="Alor: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(Alor.TechReport.MPAHouseholdData$SERate)
qqline(Alor.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Alor.TimeMarket <- 
  ggplot(Alor.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Alor.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(Alor.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Alor: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(Alor.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(Alor.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


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
       title="Alor: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

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
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
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
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Alor <- 
  cbind.data.frame(non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.Alor[order(non.parametric.test.settlements.Alor$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Alor <- 
  sigvals.Sett.Alor[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Alor) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Alor <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Alor.TechReport.MPAvControl[,a]
                      wilcox.test(var~Treatment,
                                  data=Alor.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Alor <- 
  cbind.data.frame("Selat Pantar MPA",non.parametric.test.MPAvControl.Alor)

colnames(sigvals.MPA.Alor) <- colnames(sigvals.Sett.Alor)

null.row.sigvals.Alor <- 
  matrix(rep(NA,length(sigvals.MPA.Alor)),ncol=length(sigvals.MPA.Alor),
         dimnames=list(NULL,colnames(sigvals.MPA.Alor)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)
sigvals.Alor <- 
  rbind.data.frame(sigvals.MPA.Alor,
                   null.row.sigvals.Alor,
                   sigvals.Sett.Alor[rev(order(sigvals.Sett.Alor$SettlementName)),])

sigvals.Alor[,2:8] <- unlist(sigvals.Alor[,2:8])





# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Alor.TechReport.MPAHouseholdData)
rm(Alor.TechReport.MPAvSBS)
rm(Alor.TechReport.SettlementMeans)
rm(Alor.Trend.Data)
rm(Alor.FreqTables)
rm(even.number.setts.function.Alor)
rm(non.parametric.test.settlements.Alor)
rm(non.parametric.test.MPAvBHS.Alor)
rm(trend.non.parametric.test.byMPA.Alor)
rm(trend.non.parametric.test.bySett.Alor)
rm(null.row.sigvals.Alor)
rm(sigvals.MPA.Alor)
rm(sigvals.Sett.Alor)
rm(p.for.function)
rm(data.for.function)