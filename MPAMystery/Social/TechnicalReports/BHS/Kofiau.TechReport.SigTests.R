# 
# code:  Kofiau Technical Report Significance Tests
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
Kof.TechReport.MPAHouseholdData <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                          MPA.TechReport.SigTest.Data$MPAID==4,], 
            Days.unwell.treatment[Days.unwell.treatment$MPAID==4 &
                                    Days.unwell.treatment$MonitoringYear=="4 Year Post",
                                  c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Kof.TechReport.MPAHouseholdData$SettlementName <- 
  factor(Kof.TechReport.MPAHouseholdData$SettlementName)

# --- "MPA versus Control" dataset
Kof.TechReport.MPAvControl <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==4 &
                                          MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
            Days.unwell[Days.unwell$MPAID==4 &
                          Days.unwell$MonitoringYear=="4 Year Post",
                        c("HouseholdID","DaysUnwell")],
            by="HouseholdID")

Kof.TechReport.MPAvControl$MPA.v.Control <- 
  factor(ifelse(Kof.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

# --- "Settlement Means" dataset
Kof.TechReport.SettlementMeans <- 
  left_join(BigFive.SettleGroup[BigFive.SettleGroup$Treatment==1 &
                                  BigFive.SettleGroup$MonitoringYear=="4 Year Post" &
                                  BigFive.SettleGroup$MPAID==4,],
            Techreport.BySett[Techreport.BySett$MPAID==4 &
                                Techreport.BySett$MonitoringYear=="4 Year Post",
                              c(1,4,41)],  
            by=c("SettlementID","SettlementName"))

Kof.TechReport.SettlementMeans <- 
  left_join(Kof.TechReport.SettlementMeans,
            Days.unwell.BySett[Days.unwell.BySett$MPAID==4 &
                                 Days.unwell.BySett$MonitoringYear=="4 Year Post",c(1,4)],
            by="SettlementID")

Kof.TechReport.SettlementMeans <- 
  Kof.TechReport.SettlementMeans[!is.na(Kof.TechReport.SettlementMeans$SettlementName),]

colnames(Kof.TechReport.SettlementMeans) <- c(colnames(Kof.TechReport.SettlementMeans)[1:5],
                                               "FSIndex","FSErr","MAIndex","MAErr","PAIndex","PAErr",
                                               "MTIndex","MTErr","SERate","SEErr","TimeMarketClean","DaysUnwell")

# --- "Trend" dataset
Kof.Trend.Data <- 
  left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                          MPA.TechReport.SigTest.Data$MPAID==4,],
            Days.unwell[Days.unwell$MPAID==4 &
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

sett.names.Kof <- factor(Kof.TechReport.SettlementMeans$SettlementName)


# ---- 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function.Kof <- 
  mapply(a=Kof.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         b=Kof.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)]) 
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Kof.TechReport.SettlementMeans$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1,
                                ifelse((sd(b[Kof.TechReport.SettlementMeans$SettlementName==upper.sett[1]],na.rm=T)/
                                          sqrt(length(b[Kof.TechReport.SettlementMeans$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Kof.TechReport.SettlementMeans$SettlementName==upper.sett[2]],na.rm=T)/
                                            sqrt(length(b[Kof.TechReport.SettlementMeans$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                as.character(upper.sett))
           lower.sett <- Kof.TechReport.SettlementMeans$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1,
                                ifelse((sd(b[Kof.TechReport.SettlementMeans$SettlementName==lower.sett[1]],na.rm=T)/
                                          sqrt(length(b[Kof.TechReport.SettlementMeans$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Kof.TechReport.SettlementMeans$SettlementName==lower.sett[2]],na.rm=T)/
                                            sqrt(length(b[Kof.TechReport.SettlementMeans$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                as.character(lower.sett))
           median.sett <- ifelse((sd(b[Kof.TechReport.SettlementMeans$SettlementName==upper.sett],na.rm=T)/
                                    sqrt(length(b[Kof.TechReport.SettlementMeans$SettlementName==upper.sett & !is.na(b)])))<
                                   (sd(b[Kof.TechReport.SettlementMeans$SettlementName==lower.sett],na.rm=T)/
                                      sqrt(length(b[Kof.TechReport.SettlementMeans$SettlementName==lower.sett & !is.na(b)]))),
                                 as.character(upper.sett),as.character(lower.sett))
         })

median.setts.Kof <- 
  mapply(i=Kof.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
         j=names(even.number.setts.function.Kof),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names.Kof)%%2!=0,
                                      as.character(Kof.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function.Kof[j])),
                               levels=levels(sett.names.Kof))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Food security score distribution ----

dist.Kof.FS <- 
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$FSIndex),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security Scores\n(per household)",
       y="Density",
       title="Kof: Food Security Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Kof.TechReport.MPAHouseholdData$FSIndex)
qqline(Kof.TechReport.MPAHouseholdData$FSIndex,col="green")


# ---- 3.2 Material assets score distribution ----

dist.Kof.MA <- 
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$MAIndex),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets\n(per household)",
       y="Density",
       title="Kof: HH Material Assets Distribution, 2014") +
  dist.plot.theme

log.MA <- log(Kof.TechReport.MPAHouseholdData$MAIndex[Kof.TechReport.MPAHouseholdData$MAIndex!=0])

qqnorm(log.MA)
qqline(log.MA,col="green")


# ---- 3.3 Place attachment score distribution ----

dist.Kof.PA <- 
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$PAIndex),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment Score\nPer Household",
       y="Density",
       title="Kof: Place Attachment Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Kof.TechReport.MPAHouseholdData$PAIndex)
qqline(Kof.TechReport.MPAHouseholdData$PAIndex,col="green")


# ---- 3.4 Marine tenure score distribution ----

dist.Kof.MT <- 
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$MTIndex),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure Score\n(per household)",
       y="Density",
       title="Kof: Marine Tenure Score Distribution, 2014") +
  dist.plot.theme

qqnorm(Kof.TechReport.MPAHouseholdData$MTIndex)
qqline(Kof.TechReport.MPAHouseholdData$MTIndex,col="green")


# ---- 3.5 School enrollment rate distribution ----

dist.Kof.SE <-
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$SERate),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment Rate\n(per household)",
       y="Density",
       title="Kof: School Enrollment Rate Distribution, 2014") +
  dist.plot.theme

qqnorm(Kof.TechReport.MPAHouseholdData$SERate)
qqline(Kof.TechReport.MPAHouseholdData$SERate,col="green")


# ---- 3.6 Time to market distribution ----

dist.Kof.TimeMarket <- 
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=TimeMarketClean,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$TimeMarketClean),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$TimeMarketClean)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Average Time to Market\n(in hours)",
       y="Density",
       title="Kof: Time to Market Distribution, 2014") +
  dist.plot.theme

qqnorm(Kof.TechReport.MPAHouseholdData$TimeMarketClean)
qqline(Kof.TechReport.MPAHouseholdData$TimeMarketClean,col="green")


# ---- 3.7 Days unwell distribution ----

dist.Kof.DaysUnwell <- 
  ggplot(Kof.TechReport.MPAHouseholdData) +
  geom_histogram(aes(x=DaysUnwell,y=..density..),
                 bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(Kof.TechReport.MPAHouseholdData$DaysUnwell),
                                    sd=sd(Kof.TechReport.MPAHouseholdData$DaysUnwell)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Days Unwell\n(per household)",
       y="Density",
       title="Kof: HH Days Unwell Distribution, 2014") +
  dist.plot.theme

qqnorm(Kof.TechReport.MPAHouseholdData$DaysUnwell)
qqline(Kof.TechReport.MPAHouseholdData$DaysUnwell,col="green")


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

non.parametric.test.settlements.Kof <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Kof[which(sett.names.Kof!=median.setts.Kof[a])]),
                                                               as.character(median.setts.Kof[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Kof[which(sett.names.Kof!=median.setts.Kof[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Kof.TechReport.MPAHouseholdData[Kof.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Kof.TechReport.MPAHouseholdData$SettlementName==median.setts.Kof[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Kof.TechReport.MPAHouseholdData[Kof.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Kof.TechReport.MPAHouseholdData$SettlementName==median.setts.Kof[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))


# --- Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett.Kof <- 
  cbind.data.frame(non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"FSIndex.SettlementName"),
                                                        c("FSIndex.SettlementName","FSIndex.p.value")],
                   non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"MAIndex.SettlementName"),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"PAIndex.SettlementName"),
                                                        c("PAIndex.SettlementName","PAIndex.p.value")],
                   non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"MTIndex.SettlementName"),
                                                        c("MTIndex.SettlementName","MTIndex.p.value")],
                   non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"SERate.SettlementName"),
                                                        c("SERate.SettlementName","SERate.p.value")],
                   non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"TimeMarketClean.SettlementName"),
                                                        c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                   non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"DaysUnwell.SettlementName"),
                                                        c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

# --- Remove all settlement name columns except for one. 
sigvals.Sett.Kof <- 
  sigvals.Sett.Kof[,c(1,2,4,6,8,10,12,14)]

colnames(sigvals.Sett.Kof) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. CONTROL ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl.Kof <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      var <- Kof.TechReport.MPAvControl[,a]
                      wilcox.test(var~MPA.v.Control,
                                  data=Kof.TechReport.MPAvControl,
                                  exact=F)}))["p.value",]

sigvals.MPA.Kof <- 
  cbind.data.frame("MPA",non.parametric.test.MPAvControl.Kof)

colnames(sigvals.MPA.Kof) <- colnames(sigvals.Sett.Kof)

null.row.sigvals.Kof <- 
  matrix(rep(NA,length(sigvals.MPA.Kof)),ncol=length(sigvals.MPA.Kof),
         dimnames=list(NULL,colnames(sigvals.MPA.Kof)))

# --- Define data frame with p-values for status plots
#     (households in each settlement are compared to those in the median settlement for the given variable,
#      using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#      [is/is not] significantly different")
# 
#      (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#      also using Mann Whitney U-Test)
sigvals.Kof <- 
  rbind.data.frame(sigvals.MPA.Kof,
                   null.row.sigvals.Kof,
                   sigvals.Sett.Kof[rev(order(sigvals.Sett.Kof$SettlementName)),])


# ---- 4.3 Create function that will output TREND significance values for non-parametric variables, BY MPA ----
#          (for trend plots)

trend.non.parametric.test.byMPA.Kof <- 
  data.frame(mapply(i=Kof.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                    function(i){
                      MannKendall(c(i[Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[1]],
                                    i[Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[2]],
                                    i[Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[3]]))
                    }))

colnames(trend.non.parametric.test.byMPA.Kof) <- colnames(sigvals.Kof[2:8])

# --- Define data frame with p-values for trend plots
#     (all MPA households from each year of sampling are compared across time for the given variable, 
#      using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#      there [is/is not] a significant difference in this variable across the MPA")
trend.sigvals.Kof <- 
  cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Kof["sl",1],NA,trend.non.parametric.test.byMPA.Kof["sl",2],
                   NA,trend.non.parametric.test.byMPA.Kof["sl",3],NA,trend.non.parametric.test.byMPA.Kof["sl",4],NA,trend.non.parametric.test.byMPA.Kof["sl",5],
                   NA,trend.non.parametric.test.byMPA.Kof["sl",6],NA,trend.non.parametric.test.byMPA.Kof["sl",7],NA)

colnames(trend.sigvals.Kof) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")


# ---- 4.4 Create function that will output TREND significance values for non-parametric variables, BY SETTLEMENT ----
#          (for annex plots)

trend.non.parametric.test.bySett.Kof <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Kof),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Kof),
                                                function(i){
                                                  MannKendall(c(Kof.Trend.Data[Kof.Trend.Data$SettlementName==i &
                                                                                  Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[1],a],
                                                                Kof.Trend.Data[Kof.Trend.Data$SettlementName==i &
                                                                                  Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[2],a],
                                                                Kof.Trend.Data[Kof.Trend.Data$SettlementName==i &
                                                                                  Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))

colnames(trend.non.parametric.test.bySett.Kof) <- colnames(sigvals.Kof)

# --- Define data frame with p-values for annex plots
#     (households within each settlement from each year of sampling are compared across time for the given 
#      variable, using monotonic trend test, Mann-Kendall -- so, interpretation is "across the sampling years, 
#      there [is/is not] a significant difference in this variable across the settlement)
annex.sigvals.Kof <- 
  rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Kof["sl",]),
                   null.row.sigvals.Kof,
                   trend.non.parametric.test.bySett.Kof[rev(order(trend.non.parametric.test.bySett.Kof$SettlementName)),])





# ---- 4.5 Remove all unneeded dataframes from environment, to reduce clutter ----
rm(MPA.TechReport.SigTest.Data)
rm(Kof.TechReport.MPAHouseholdData)
rm(Kof.TechReport.MPAvControl)
rm(Kof.TechReport.SettlementMeans)
rm(Kof.Trend.Data)
rm(even.number.setts.function.Kof)
rm(median.setts.Kof)
rm(non.parametric.test.settlements.Kof)
rm(non.parametric.test.MPAvControl.Kof)
rm(trend.non.parametric.test.byMPA.Kof)
rm(trend.non.parametric.test.bySett.Kof)
rm(null.row.sigvals.Kof)
rm(sigvals.MPA.Kof)
rm(sigvals.Sett.Kof)