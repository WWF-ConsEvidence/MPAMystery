# 
# code:  Kaimana Technical Report Significance Tests
# 
# github: kaclaborn/WWF-conservation-evidence/MPAMystery/Social/TechnicalReports/BHS
# --- Duplicate all code from "Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: October 2017
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
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Import Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Subset datasets from MPA.Mystery.R ----

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

# - "MPA versus Control" dataset
Kai.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==3 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
            Days.unwell[Days.unwell$MPAID==3 &
                          Days.unwell$MonitoringYear=="4 Year Post",
                        c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Kai.TechReport.MPAvControl$MPA.v.Control <- 
  factor(ifelse(Kai.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

# - "Settlement Means" dataset
Kai.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==3,],
            Techreport.BySett[Techreport.BySett$MPAID==3 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c(1,4,41)],  
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Define list of settlement names in MPA ----

sett.names.Kai <- factor(Kai.TechReport.SettlementMeans$SettlementName)


# ---- 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Kai <- 
  mapply(a=Kai.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Kai.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Kai.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1,
                                ifelse((sd(b[Kai.TechReport.SettlementMeans$SettlementName==upper.sett[1]],na.rm=T)/
                                          sqrt(length(b[Kai.TechReport.SettlementMeans$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Kai.TechReport.SettlementMeans$SettlementName==upper.sett[2]],na.rm=T)/
                                            sqrt(length(b[Kai.TechReport.SettlementMeans$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                as.character(upper.sett))
           lower.sett <- Kai.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1,
                                ifelse((sd(b[Kai.TechReport.SettlementMeans$SettlementName==lower.sett[1]],na.rm=T)/
                                          sqrt(length(b[Kai.TechReport.SettlementMeans$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Kai.TechReport.SettlementMeans$SettlementName==lower.sett[2]],na.rm=T)/
                                            sqrt(length(b[Kai.TechReport.SettlementMeans$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                as.character(lower.sett))
           median.sett <- ifelse((sd(b[Kai.TechReport.SettlementMeans$SettlementName==upper.sett],na.rm=T)/
                                    sqrt(length(b[Kai.TechReport.SettlementMeans$SettlementName==upper.sett & !is.na(b)])))<
                                   (sd(b[Kai.TechReport.SettlementMeans$SettlementName==lower.sett],na.rm=T)/
                                      sqrt(length(b[Kai.TechReport.SettlementMeans$SettlementName==lower.sett & !is.na(b)]))),
                                 as.character(upper.sett),as.character(lower.sett))
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


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. CONTROL ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Kai <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Kai.TechReport.MPAvControl[,a]
                      wilcox.test(var~MPA.v.Control,
                                  data=Kai.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Kai <- 
  cbind.data.frame("MPA",non.parametric.test.MPAvControl.Kai)

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
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")


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
  rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Kai["sl",]),
                   null.row.sigvals.Kai,
                   trend.non.parametric.test.bySett.Kai[rev(order(trend.non.parametric.test.bySett.Kai$SettlementName)),])





# ---- 4.5 Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Kai.TechReport.MPAHouseholdData)
rm(Kai.TechReport.MPAvControl)
rm(Kai.TechReport.SettlementMeans)
rm(Kai.Trend.Data)
rm(even.number.setts.function.Kai)
rm(median.setts.Kai)
rm(non.parametric.test.settlements.Kai)
rm(non.parametric.test.MPAvControl.Kai)
rm(trend.non.parametric.test.byMPA.Kai)
rm(trend.non.parametric.test.bySett.Kai)
rm(null.row.sigvals.Kai)
rm(sigvals.MPA.Kai)
rm(sigvals.Sett.Kai)