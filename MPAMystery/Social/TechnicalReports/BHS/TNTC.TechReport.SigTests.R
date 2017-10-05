# 
# code:  Cenderawasih Technical Report Significance Tests
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
TNTC.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                          MPA.TechReport.SigTest.Data$MPAID==2,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==2 &
                                    Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

TNTC.TechReport.MPAHouseholdData$SettlementName <- 
  factor(TNTC.TechReport.MPAHouseholdData$SettlementName)

# --- "MPA versus Control" dataset
TNTC.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==2 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
            Days.unwell[Days.unwell$MPAID==2 &
                          Days.unwell$MonitoringYear=="4 Year Post",
                        c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

TNTC.TechReport.MPAvControl$MPA.v.Control <- 
  factor(ifelse(TNTC.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

# --- "Settlement Means" dataset
TNTC.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==2,],
            Techreport.BySett[Techreport.BySett$MPAID==2 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c(1,4,41)],  
            by=c("SettlementID","SettlementName"))

TNTC.TechReport.SettlementMeans <- 
  left_join(TNTC.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==2 &
                                 Days.unwell.BySett$MonitoringYear=="4 Year Post",c(1,4)],
            by="SettlementID")

TNTC.TechReport.SettlementMeans <- 
  TNTC.TechReport.SettlementMeans[!is.na(TNTC.TechReport.SettlementMeans$SettlementName),]

colnames(TNTC.TechReport.SettlementMeans) <- c(colnames(TNTC.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")

# --- "Trend" dataset
TNTC.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==2,],
            Days.unwell[Days.unwell$MPAID==2 &
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

sett.names.TNTC <- factor(TNTC.TechReport.SettlementMeans$SettlementName)


# ---- 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.TNTC <- 
  mapply(a=TNTC.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=TNTC.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- TNTC.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1,
                                ifelse((sd(b[TNTC.TechReport.SettlementMeans$SettlementName==upper.sett[1]],na.rm=T)/
                                          sqrt(length(b[TNTC.TechReport.SettlementMeans$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[TNTC.TechReport.SettlementMeans$SettlementName==upper.sett[2]],na.rm=T)/
                                            sqrt(length(b[TNTC.TechReport.SettlementMeans$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                as.character(upper.sett))
           lower.sett <- TNTC.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1,
                                ifelse((sd(b[TNTC.TechReport.SettlementMeans$SettlementName==lower.sett[1]],na.rm=T)/
                                          sqrt(length(b[TNTC.TechReport.SettlementMeans$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[TNTC.TechReport.SettlementMeans$SettlementName==lower.sett[2]],na.rm=T)/
                                            sqrt(length(b[TNTC.TechReport.SettlementMeans$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                as.character(lower.sett))
           median.sett <- ifelse((sd(b[TNTC.TechReport.SettlementMeans$SettlementName==upper.sett],na.rm=T)/
                                    sqrt(length(b[TNTC.TechReport.SettlementMeans$SettlementName==upper.sett & !is.na(b)])))<
                                   (sd(b[TNTC.TechReport.SettlementMeans$SettlementName==lower.sett],na.rm=T)/
                                      sqrt(length(b[TNTC.TechReport.SettlementMeans$SettlementName==lower.sett & !is.na(b)]))),
                                 as.character(upper.sett),as.character(lower.sett))
         })

median.setts.TNTC <- 
  mapply(i=TNTC.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.TNTC),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.TNTC)%%2!=0,
                                      as.character(TNTC.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.TNTC[j])),
                               levels=levels(sett.names.TNTC))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.TNTC.FS <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="TNTC: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$FSIndex)
qqline(TNTC.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.TNTC.MA <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="TNTC: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(TNTC.TechReport.MPAHouseholdData$MAIndex[TNTC.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.TNTC.PA <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="TNTC: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$PAIndex)
qqline(TNTC.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.TNTC.MT <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="TNTC: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$MTIndex)
qqline(TNTC.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.TNTC.SE <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="TNTC: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$SERate)
qqline(TNTC.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.TNTC.TimeMarket <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="TNTC: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(TNTC.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.TNTC.DaysUnwell <- 
  ggplot(TNTC.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(TNTC.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(TNTC.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="TNTC: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(TNTC.TechReport.MPAHouseholdData$DaysUnwell)
qqline(TNTC.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.TNTC <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.TNTC[which(sett.names.TNTC!=median.setts.TNTC[a])]),
                                                               as.character(median.setts.TNTC[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.TNTC[which(sett.names.TNTC!=median.setts.TNTC[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       TNTC.TechReport.MPAHouseholdData[TNTC.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          TNTC.TechReport.MPAHouseholdData$SettlementName==median.setts.TNTC[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=TNTC.TechReport.MPAHouseholdData[TNTC.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           TNTC.TechReport.MPAHouseholdData$SettlementName==median.setts.TNTC[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# --- Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.TNTC <- 
  cbind.data.frame(non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.TNTC[order(non.parametric.test.settlements.TNTC$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# --- Remove all settlement name columns except for one. 
sigvals.Sett.TNTC <-
  sigvals.Sett.TNTC[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.TNTC) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. CONTROL ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.TNTC <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- TNTC.TechReport.MPAvControl[,a]
                      wilcox.test(var~MPA.v.Control,
                                  data=TNTC.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.TNTC <- 
  cbind.data.frame("MPA",non.parametric.test.MPAvControl.TNTC)

colnames(sigvals.MPA.TNTC) <- colnames(sigvals.Sett.TNTC)

null.row.sigvals.TNTC <- 
  matrix(rep(NA,length(sigvals.MPA.TNTC)),ncol=length(sigvals.MPA.TNTC),
         dimnames=list(NULL,colnames(sigvals.MPA.TNTC)))

# --- Define data frame with p-values for status plots
#     (households in each settlement are compared to those in the median settlement for the given variable,
#      using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#      [is/is not] significantly different")
# 
#      (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#      also using Mann Whitney U-Test)
sigvals.TNTC <- 
  rbind.data.frame(sigvals.MPA.TNTC,
                   null.row.sigvals.TNTC,
                   sigvals.Sett.TNTC[rev(order(sigvals.Sett.TNTC$SettlementName)),])


# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for trend plots)

trend.non.parametric.test.byMPA.TNTC <- 
  data.frame(mapply(i=TNTC.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[1]],
                                    i[TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[2]],
                                    i[TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[3]]))
                    }))

colnames(trend.non.parametric.test.byMPA.TNTC) <- colnames(sigvals.TNTC[2:8])

# --- Define data frame with p-values for trend plots
#     (all MPA households from each year of sampling are compared across time for the given variable, 
#      using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#      there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.TNTC <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.TNTC["sl",1],NA,trend.non.parametric.test.byMPA.TNTC["sl",2],
                   NA,trend.non.parametric.test.byMPA.TNTC["sl",3],NA,trend.non.parametric.test.byMPA.TNTC["sl",4],NA,trend.non.parametric.test.byMPA.TNTC["sl",5],
                   NA,trend.non.parametric.test.byMPA.TNTC["sl",6],NA,trend.non.parametric.test.byMPA.TNTC["sl",7],NA)

colnames(trend.sigvals.TNTC) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.TNTC <- 
  cbind.data.frame(SettlementName=as.character(sett.names.TNTC),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.TNTC),
                                                function(i){
                                                  MannKendall(c(TNTC.Trend.Data[TNTC.Trend.Data$SettlementName==i &
                                                                                  TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[1],a],
                                                                TNTC.Trend.Data[TNTC.Trend.Data$SettlementName==i &
                                                                                  TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[2],a],
                                                                TNTC.Trend.Data[TNTC.Trend.Data$SettlementName==i &
                                                                                  TNTC.Trend.Data$InterviewYear==unique(TNTC.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.TNTC) <- colnames(sigvals.TNTC)

# --- Define data frame with p-values for annex plots
#     (households within each settlement from each year of sampling are compared across time for the given 
#      variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#      there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.TNTC <- 
  rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.TNTC["sl",]),
                   null.row.sigvals.TNTC,
                   trend.non.parametric.test.bySett.TNTC[rev(order(trend.non.parametric.test.bySett.TNTC$SettlementName)),])





# ---- 4.5 Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(TNTC.TechReport.MPAHouseholdData)
rm(TNTC.TechReport.MPAvControl)
rm(TNTC.TechReport.SettlementMeans)
rm(TNTC.Trend.Data)
rm(even.number.setts.function.TNTC)
rm(median.setts.TNTC)
rm(non.parametric.test.settlements.TNTC)
rm(non.parametric.test.MPAvControl.TNTC)
rm(trend.non.parametric.test.byMPA.TNTC)
rm(trend.non.parametric.test.bySett.TNTC)
rm(null.row.sigvals.TNTC)
rm(sigvals.MPA.TNTC)
rm(sigvals.Sett.TNTC)