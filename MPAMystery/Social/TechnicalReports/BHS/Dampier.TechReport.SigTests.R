# 
# code:  Dampier Technical Report Significance Tests
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
#  1) Source MPA.Mystery.R 
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

# --- "MPA Household Data" dataset
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

# --- "MPA versus Control" dataset
Damp.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==5 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
            Days.unwell[Days.unwell$MPAID==5 &
                          Days.unwell$MonitoringYear=="4 Year Post",
                        c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Damp.TechReport.MPAvControl$MPA.v.Control <- 
  factor(ifelse(Damp.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

# --- "Settlement Means" dataset
Damp.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==5,],
            Techreport.BySett[Techreport.BySett$MPAID==5 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c(1,4,41)],  
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

# --- "Trend" dataset
Damp.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==5,],
            Days.unwell[Days.unwell$MPAID==5 &
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

sett.names.Damp <- factor(Damp.TechReport.SettlementMeans$SettlementName)


# ---- 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Damp <- 
  mapply(a=Damp.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Damp.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Damp.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1,
                                ifelse((sd(b[Damp.TechReport.SettlementMeans$SettlementName==upper.sett[1]],na.rm=T)/
                                          sqrt(length(b[Damp.TechReport.SettlementMeans$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Damp.TechReport.SettlementMeans$SettlementName==upper.sett[2]],na.rm=T)/
                                            sqrt(length(b[Damp.TechReport.SettlementMeans$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                as.character(upper.sett))
           lower.sett <- Damp.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1,
                                ifelse((sd(b[Damp.TechReport.SettlementMeans$SettlementName==lower.sett[1]],na.rm=T)/
                                          sqrt(length(b[Damp.TechReport.SettlementMeans$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Damp.TechReport.SettlementMeans$SettlementName==lower.sett[2]],na.rm=T)/
                                            sqrt(length(b[Damp.TechReport.SettlementMeans$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                as.character(lower.sett))
           median.sett <- ifelse((sd(b[Damp.TechReport.SettlementMeans$SettlementName==upper.sett],na.rm=T)/
                                    sqrt(length(b[Damp.TechReport.SettlementMeans$SettlementName==upper.sett & !is.na(b)])))<
                                   (sd(b[Damp.TechReport.SettlementMeans$SettlementName==lower.sett],na.rm=T)/
                                      sqrt(length(b[Damp.TechReport.SettlementMeans$SettlementName==lower.sett & !is.na(b)]))),
                                 as.character(upper.sett),as.character(lower.sett))
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


# --- Alphabetize each column of settlement names.  Now all settlement names are in same order.
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

# --- Remove all settlement name columns except for one. 
sigvals.Sett.Damp <- 
  sigvals.Sett.Damp[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Damp) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. CONTROL ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Damp <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Damp.TechReport.MPAvControl[,a]
                      wilcox.test(var~MPA.v.Control,
                                  data=Damp.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Damp <- 
  cbind.data.frame("MPA",non.parametric.test.MPAvControl.Damp)

colnames(sigvals.MPA.Damp) <- colnames(sigvals.Sett.Damp)

null.row.sigvals.Damp <- 
  matrix(rep(NA,length(sigvals.MPA.Damp)),ncol=length(sigvals.MPA.Damp),
         dimnames=list(NULL,colnames(sigvals.MPA.Damp)))

# --- Define data frame with p-values for status plots
#     (households in each settlement are compared to those in the median settlement for the given variable,
#      using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#      [is/is not] significantly different")
# 
#      (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#      also using Mann Whitney U-Test)
sigvals.Damp <- 
  rbind.data.frame(sigvals.MPA.Damp,
                   null.row.sigvals.Damp,
                   sigvals.Sett.Damp[rev(order(sigvals.Sett.Damp$SettlementName)),])


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

# --- Define data frame with p-values for trend plots
#     (all MPA households from each year of sampling are compared across time for the given variable, 
#      using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#      there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.Damp <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Damp["sl",1],NA,trend.non.parametric.test.byMPA.Damp["sl",2],
                   NA,trend.non.parametric.test.byMPA.Damp["sl",3],NA,trend.non.parametric.test.byMPA.Damp["sl",4],NA,trend.non.parametric.test.byMPA.Damp["sl",5],
                   NA,trend.non.parametric.test.byMPA.Damp["sl",6],NA,trend.non.parametric.test.byMPA.Damp["sl",7],NA)

colnames(trend.sigvals.Damp) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")


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

# --- Define data frame with p-values for annex plots
#     (households within each settlement from each year of sampling are compared across time for the given 
#      variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#      there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Damp <- 
  rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Damp["sl",]),
                   null.row.sigvals.Damp,
                   trend.non.parametric.test.bySett.Damp[rev(order(trend.non.parametric.test.bySett.Damp$SettlementName)),])





# ---- 4.5 Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Damp.TechReport.MPAHouseholdData)
rm(Damp.TechReport.MPAvControl)
rm(Damp.TechReport.SettlementMeans)
rm(Damp.Trend.Data)
rm(even.number.setts.function.Damp)
rm(median.setts.Damp)
rm(non.parametric.test.settlements.Damp)
rm(non.parametric.test.MPAvControl.Damp)
rm(trend.non.parametric.test.byMPA.Damp)
rm(trend.non.parametric.test.bySett.Damp)
rm(null.row.sigvals.Damp)
rm(sigvals.MPA.Damp)
rm(sigvals.Sett.Damp)