# 
# code:  Mayalibit Technical Report Significance Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/BHS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
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


# ---- 1.1 Subset datasets from BHS_MPA_Mystery.R ----

MPA.TechReport.SigTest.Data <- 
  left_join(BigFive[,c(1,2,4:11,14)],
            HHLivelihood[,c(1,15)],
            by="HouseholdID")

# - "MPA Household Data" dataset
Maya.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                          MPA.TechReport.SigTest.Data$MPAID==1,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==1 &
                                    Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Maya.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Maya.TechReport.MPAHouseholdData$SettlementName)

# - "MPA versus Control" dataset
Maya.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
            Days.unwell[Days.unwell$MPAID==1 &
                          Days.unwell$MonitoringYear=="4 Year Post",
                        c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Maya.TechReport.MPAvControl$MPA.v.Control <- 
  factor(ifelse(Maya.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

# - "Settlement Means" dataset
Maya.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==1,],
            Techreport.BySett[Techreport.BySett$MPAID==1 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c(1,4,41)],  
            by=c("SettlementID","SettlementName"))

Maya.TechReport.SettlementMeans <- 
  left_join(Maya.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==1 &
                                 Days.unwell.BySett$MonitoringYear=="4 Year Post",c(1,4)],
            by="SettlementID")

Maya.TechReport.SettlementMeans <- 
  Maya.TechReport.SettlementMeans[!is.na(Maya.TechReport.SettlementMeans$SettlementName),]

colnames(Maya.TechReport.SettlementMeans) <- c(colnames(Maya.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")

# - "Trend" dataset
Maya.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==1,],
            Days.unwell[Days.unwell$MPAID==1 &
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

sett.names.Maya <- factor(Maya.TechReport.SettlementMeans$SettlementName)


# ---- 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Maya <- 
  mapply(a=Maya.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Maya.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Maya.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1,
                                ifelse((sd(b[Maya.TechReport.SettlementMeans$SettlementName==upper.sett[1]],na.rm=T)/
                                          sqrt(length(b[Maya.TechReport.SettlementMeans$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Maya.TechReport.SettlementMeans$SettlementName==upper.sett[2]],na.rm=T)/
                                            sqrt(length(b[Maya.TechReport.SettlementMeans$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                as.character(upper.sett))
           lower.sett <- Maya.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1,
                                ifelse((sd(b[Maya.TechReport.SettlementMeans$SettlementName==lower.sett[1]],na.rm=T)/
                                          sqrt(length(b[Maya.TechReport.SettlementMeans$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Maya.TechReport.SettlementMeans$SettlementName==lower.sett[2]],na.rm=T)/
                                            sqrt(length(b[Maya.TechReport.SettlementMeans$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                as.character(lower.sett))
           median.sett <- ifelse((sd(b[Maya.TechReport.SettlementMeans$SettlementName==upper.sett],na.rm=T)/
                                    sqrt(length(b[Maya.TechReport.SettlementMeans$SettlementName==upper.sett & !is.na(b)])))<
                                   (sd(b[Maya.TechReport.SettlementMeans$SettlementName==lower.sett],na.rm=T)/
                                      sqrt(length(b[Maya.TechReport.SettlementMeans$SettlementName==lower.sett & !is.na(b)]))),
                                 as.character(upper.sett),as.character(lower.sett))
         })

median.setts.Maya <- 
  mapply(i=Maya.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.Maya),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Maya)%%2!=0,
                                      as.character(Maya.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Maya[j])),
                               levels=levels(sett.names.Maya))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.Maya.FS <- 
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Maya: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Maya.TechReport.MPAHouseholdData$FSIndex)
qqline(Maya.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Maya.MA <- 
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Maya: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(Maya.TechReport.MPAHouseholdData$MAIndex[Maya.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Maya.PA <- 
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Maya: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Maya.TechReport.MPAHouseholdData$PAIndex)
qqline(Maya.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Maya.MT <- 
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Maya: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Maya.TechReport.MPAHouseholdData$MTIndex)
qqline(Maya.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Maya.SE <-
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Maya: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(Maya.TechReport.MPAHouseholdData$SERate)
qqline(Maya.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Maya.TimeMarket <- 
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Maya: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(Maya.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(Maya.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Maya.DaysUnwell <- 
  ggplot(Maya.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Maya.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Maya.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Maya: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(Maya.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Maya.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.Maya <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Maya[which(sett.names.Maya!=median.setts.Maya[a])]),
                                                               as.character(median.setts.Maya[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Maya[which(sett.names.Maya!=median.setts.Maya[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Maya.TechReport.MPAHouseholdData[Maya.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Maya.TechReport.MPAHouseholdData$SettlementName==median.setts.Maya[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Maya.TechReport.MPAHouseholdData[Maya.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Maya.TechReport.MPAHouseholdData$SettlementName==median.setts.Maya[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Maya <- 
  cbind.data.frame(non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett.Maya <- 
  sigvals.Sett.Maya[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Maya) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. CONTROL ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Maya <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Maya.TechReport.MPAvControl[,a]
                      wilcox.test(var~MPA.v.Control,
                                  data=Maya.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Maya <- 
  cbind.data.frame("MPA",non.parametric.test.MPAvControl.Maya)

colnames(sigvals.MPA.Maya) <- colnames(sigvals.Sett.Maya)

null.row.sigvals.Maya <- 
  matrix(rep(NA,length(sigvals.MPA.Maya)),ncol=length(sigvals.MPA.Maya),
         dimnames=list(NULL,colnames(sigvals.MPA.Maya)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann Whitney U-Test)
sigvals.Maya <- 
  rbind.data.frame(sigvals.MPA.Maya,
                   null.row.sigvals.Maya,
                   sigvals.Sett.Maya[rev(order(sigvals.Sett.Maya$SettlementName)),])



# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for trend plots)

trend.non.parametric.test.byMPA.Maya <- 
  data.frame(mapply(i=Maya.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[1]],
                                    i[Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[2]],
                                    i[Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[3]]))
                    }))

colnames(trend.non.parametric.test.byMPA.Maya) <- colnames(sigvals.Maya[2:8])

# - Define data frame with p-values for trend plots
#   (all MPA households from each year of sampling are compared across time for the given variable, 
#   using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.Maya <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Maya["sl",1],NA,trend.non.parametric.test.byMPA.Maya["sl",2],
                   NA,trend.non.parametric.test.byMPA.Maya["sl",3],NA,trend.non.parametric.test.byMPA.Maya["sl",4],NA,trend.non.parametric.test.byMPA.Maya["sl",5],
                   NA,trend.non.parametric.test.byMPA.Maya["sl",6],NA,trend.non.parametric.test.byMPA.Maya["sl",7],NA)

colnames(trend.sigvals.Maya) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.Maya <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Maya),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Maya),
                                                function(i){
                                                  MannKendall(c(Maya.Trend.Data[Maya.Trend.Data$SettlementName==i &
                                                                                  Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[1],a],
                                                                Maya.Trend.Data[Maya.Trend.Data$SettlementName==i &
                                                                                  Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[2],a],
                                                                Maya.Trend.Data[Maya.Trend.Data$SettlementName==i &
                                                                                  Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.Maya) <- colnames(sigvals.Maya)

# - Define data frame with p-values for annex plots
#   (households within each settlement from each year of sampling are compared across time for the given 
#   variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#   there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Maya <- 
  rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Maya["sl",]),
                   null.row.sigvals.Maya,
                   trend.non.parametric.test.bySett.Maya[rev(order(trend.non.parametric.test.bySett.Maya$SettlementName)),])





# ---- 4.5 Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Maya.TechReport.MPAHouseholdData)
rm(Maya.TechReport.MPAvControl)
rm(Maya.TechReport.SettlementMeans)
rm(Maya.Trend.Data)
rm(even.number.setts.function.Maya)
rm(median.setts.Maya)
rm(non.parametric.test.settlements.Maya)
rm(non.parametric.test.MPAvControl.Maya)
rm(trend.non.parametric.test.byMPA.Maya)
rm(trend.non.parametric.test.bySett.Maya)
rm(null.row.sigvals.Maya)
rm(sigvals.MPA.Maya)
rm(sigvals.Sett.Maya)