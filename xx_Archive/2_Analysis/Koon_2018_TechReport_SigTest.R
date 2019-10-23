# 
# code:  Koon 2018 Technical Report Significance Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: April 2019
# 
# 
# ---- inputs ----
#  Dependencies: Koon_2018_calculate_indicators.R
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


# ---- 1.1 Subset continuous variable datasets from SBS_MPA_Mystery.R ----


# - "MPA Household Data" dataset
Koon.2018.TechReport.MPAHouseholdData <- 
  dplyr::select(BigFive,c("HouseholdID", "MPAID", "SettlementName", "Treatment", "InterviewYear",
         "MAIndex", "FSIndex", "PAIndex", "MTIndex", "SERate")) %>%
  filter(.,Treatment==1)


Koon.2018.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Koon.2018.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus Control" dataset
Koon.2018.TechReport.MPAvControl <- 
  dplyr::select(BigFive,c("HouseholdID", "MPAID", "SettlementName", "Treatment", "InterviewYear",
                          "MAIndex", "FSIndex", "PAIndex", "MTIndex", "SERate"))

# - "Settlement Means" dataset
Koon.2018.TechReport.SettlementMeans <- 
  BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1,]

Koon.2018.TechReport.SettlementMeans <- 
  Koon.2018.TechReport.SettlementMeans[!is.na(Koon.2018.TechReport.SettlementMeans$SettlementName),]

Koon.2018.TechReport.SettlementMeans$SettlementName <- factor(Koon.2018.TechReport.SettlementMeans$SettlementName)

colnames(Koon.2018.TechReport.SettlementMeans) <- c(colnames(Koon.2018.TechReport.SettlementMeans)[1:4],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr")


# ---- 1.2 Define list of settlement names in MPA ----

sett.names.Koon <- factor(Koon.2018.TechReport.SettlementMeans$SettlementName)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Koon.2018 <- 
  mapply(a=Koon.2018.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate")],
         b=Koon.2018.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Koon.2018.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Koon.2018.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Koon.2018.TechReport.SettlementMeans$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Koon.2018.TechReport.MPAHouseholdData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts.Koon.2018 <- 
  mapply(i=Koon.2018.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate")],
         j=names(even.number.setts.function.Koon.2018),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Koon)%%2!=0,
                                      as.character(Koon.2018.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Koon.2018[j])),
                               levels=levels(sett.names.Koon))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.Koon.2018.FS <- 
  ggplot(Koon.2018.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Koon.2018.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Koon.2018.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Koon.2018: Food Security Score Distribution, 2016") +
  dist.plot.theme

qqnorm(Koon.2018.TechReport.MPAHouseholdData$FSIndex)
qqline(Koon.2018.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Koon.2018.MA <- 
  ggplot(Koon.2018.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Koon.2018.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Koon.2018.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Koon.2018: HH Material Assets Distribution, 2016") +
  dist.plot.theme

log.MA <- log(Koon.2018.TechReport.MPAHouseholdData$MAIndex[Koon.2018.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Koon.2018.PA <- 
  ggplot(Koon.2018.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Koon.2018.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Koon.2018.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Koon.2018: Place Attachment Score Distribution, 2016") +
  dist.plot.theme

qqnorm(Koon.2018.TechReport.MPAHouseholdData$PAIndex)
qqline(Koon.2018.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Koon.2018.MT <- 
  ggplot(Koon.2018.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Koon.2018.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Koon.2018.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Koon.2018: Marine Tenure Score Distribution, 2016") +
  dist.plot.theme

qqnorm(Koon.2018.TechReport.MPAHouseholdData$MTIndex)
qqline(Koon.2018.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Koon.2018.SE <-
  ggplot(Koon.2018.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Koon.2018.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Koon.2018.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Koon.2018: School Enrollment Rate Distribution, 2016") +
  dist.plot.theme

qqnorm(Koon.2018.TechReport.MPAHouseholdData$SERate)
qqline(Koon.2018.TechReport.MPAHouseholdData$SERate,col="green")



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

non.parametric.test.settlements.Koon.2018 <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Koon[which(sett.names.Koon!=median.setts.Koon.2018[a])]),
                                                               as.character(median.setts.Koon.2018[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Koon[which(sett.names.Koon!=median.setts.Koon.2018[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Koon.2018.TechReport.MPAHouseholdData[Koon.2018.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Koon.2018.TechReport.MPAHouseholdData$SettlementName==median.setts.Koon.2018[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Koon.2018.TechReport.MPAHouseholdData[Koon.2018.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Koon.2018.TechReport.MPAHouseholdData$SettlementName==median.setts.Koon.2018[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Koon.2018 <- 
  cbind.data.frame(non.parametric.test.settlements.Koon.2018[order(non.parametric.test.settlements.Koon.2018$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Koon.2018[order(non.parametric.test.settlements.Koon.2018$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Koon.2018[order(non.parametric.test.settlements.Koon.2018$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Koon.2018[order(non.parametric.test.settlements.Koon.2018$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Koon.2018[order(non.parametric.test.settlements.Koon.2018$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Koon.2018 <- 
  dplyr:: select(sigvals.Sett.Koon.2018,"FSIndex.SettlementName", "FSIndex.p.value", "MAIndex.p.value", 
  "PAIndex.p.value", "MTIndex.p.value", "SERate.p.value")


colnames(sigvals.Sett.Koon.2018) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Koon.2018 <-
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate"),
                    function(a){
                      var <- Koon.2018.TechReport.MPAvControl[,a]
                      wilcox.test(var~Treatment,
                                  data=Koon.2018.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Koon.2018 <- 
  cbind.data.frame("Koon MPA",non.parametric.test.MPAvControl.Koon.2018)

colnames(sigvals.MPA.Koon.2018) <- colnames(sigvals.Sett.Koon.2018)

null.row.sigvals.Koon.2018 <- 
  matrix(rep(NA,length(sigvals.MPA.Koon.2018)),ncol=length(sigvals.MPA.Koon.2018),
         dimnames=list(NULL,colnames(sigvals.MPA.Koon.2018)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)
sigvals.Koon.2018 <- 
  rbind.data.frame(sigvals.MPA.Koon.2018,
                   null.row.sigvals.Koon.2018,
                   sigvals.Sett.Koon.2018[rev(order(sigvals.Sett.Koon.2018$SettlementName)),])



sigvals.Koon.2018[,c("FS.pval", "MA.pval", "PA.pval", "MT.pval", "SE.pval")] <- 
  unlist(sigvals.Koon.2018[,c("FS.pval", "MA.pval", "PA.pval", "MT.pval", "SE.pval")])




# ---- Remove all unneeded dataframes from environment, to reduce clutter ----
rm(Koon.2018.TechReport.MPAHouseholdData)
rm(Koon.2018.TechReport.MPAvSBS)
rm(Koon.2018.TechReport.SettlementMeans)
rm(Koon.2018.Trend.Data)
rm(Koon.2018.FreqTables)
rm(even.number.setts.function.Koon.2018)
rm(non.parametric.test.settlements.Koon.2018)
rm(non.parametric.test.MPAvBHS.Koon.2018)
rm(trend.non.parametric.test.byMPA.Koon.2018)
rm(trend.non.parametric.test.bySett.Koon.2018)
rm(null.row.sigvals.Koon.2018)
rm(sigvals.MPA.Koon.2018)
rm(sigvals.Sett.Koon.2018)
rm(p.for.function)
rm(data.for.function)
