# ----
# code:  Misool Technical Report Significance Tests
# git branch: MPAMystery --> Social --> TechnicalReports --> Misool
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: February 2017
# 
# ----
# inputs:
#  1) MPA.Mystery.R must be run in its entirety to run this code
# 
# code sections:
#  1) Import Data
#  2) Define Lists of Settlements, to be used in functions
#  3) Non-parametric Significance Test Functions (using Mann-Whitney U test)
# 
# ----
############################################################
#
# SECTION 1: Import Data
#
############################################################
# ----

# 1.1 Subset datasets from MPA.Mystery.R

MPA.TechReport.SigTest.Data <- left_join(BigFive[,c(1,2,4:11,14)],
                                         HHLivelihood[,c(1,15)],
                                         by="HouseholdID")

### "MPA Household Data" dataset
Mis.TechReport.MPAHouseholdData <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                                            MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                                                            MPA.TechReport.SigTest.Data$MPAID==6,], 
                                              Days.unwell.status[Days.unwell.status$MPAID==6,c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID")
Mis.TechReport.MPAHouseholdData$SettlementName <- factor(Mis.TechReport.MPAHouseholdData$SettlementName)

### "MPA versus Control" dataset
Mis.TechReport.MPAvControl <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==6 &
                                                                       MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
                                         Days.unwell[Days.unwell$MPAID==6 &
                                                       Days.unwell$MonitoringYear=="4 Year Post",
                                                     c("HouseholdID","DaysUnwell")],
                                         by="HouseholdID")
Mis.TechReport.MPAvControl$MPA.v.Control <- factor(ifelse(Mis.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

### "Settlement Means" dataset
Mis.TechReport.SettlementMeans <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                 BigFive.YearGroup$Year=="4 Year Post" &
                                                                 BigFive.YearGroup$MPAID==6,
                                                               c(2,4,6,8,10,13)],
                                             Techreport.status[Techreport.status$MPAID==6,
                                                               c(1,3,6)],  
                                             by="SettlementID")
Mis.TechReport.SettlementMeans <- left_join(Mis.TechReport.SettlementMeans,
                                             Days.unwell.BySett[Days.unwell.BySett$MPAID==6,c(1,3,2)],
                                             by="SettlementID")

Mis.TechReport.SettlementMeans <- Mis.TechReport.SettlementMeans[!is.na(Mis.TechReport.SettlementMeans$SettlementName),]
colnames(Mis.TechReport.SettlementMeans) <- c("FSIndex","MAIndex","PAIndex","MTIndex","SERate",
                                               "SettlementID","SettlementName","TimeMarketClean","DaysUnwell","MPAID")

### "Trend" dataset
Mis.Trend.Data <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                           MPA.TechReport.SigTest.Data$MPAID==6,],
                             Days.unwell[Days.unwell$MPAID==6 &
                                           Days.unwell$Treatment==1,1:2],
                             by="HouseholdID") 

# ----
############################################################
#
# SECTION 2: Define Lists of Settlements, to be used in functions
#
############################################################
# ----

# 2.1 Define list of settlement names in MPA

sett.names.Mis <- factor(Mis.TechReport.SettlementMeans$SettlementName)

# 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric)

even.number.setts.function.Mis <- mapply(a=Mis.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                                          b=Mis.TechReport.MPAHouseholdData[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                                          function(a,b){
                                            med <- median(a,na.rm=T)
                                            upper <- c(a[which(a>med)]) 
                                            upper <- min(upper,na.rm=T)
                                            lower <- c(a[which(a<med)]) 
                                            lower <- max(lower,na.rm=T)
                                            upper.sett <- Mis.TechReport.SettlementMeans$SettlementName[a==upper]
                                            upper.sett <- ifelse(length(upper.sett)>1,
                                                                 ifelse((sd(b[Mis.TechReport.SettlementMeans$SettlementName==upper.sett[1]],na.rm=T)/
                                                                           sqrt(length(b[Mis.TechReport.SettlementMeans$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                                          (sd(b[Mis.TechReport.SettlementMeans$SettlementName==upper.sett[2]],na.rm=T)/
                                                                             sqrt(length(b[Mis.TechReport.SettlementMeans$SettlementName==upper.sett[2] & !is.na(b)]))),
                                                                        as.character(upper.sett[1]),as.character(upper.sett[2])),
                                                                 as.character(upper.sett))
                                            lower.sett <- Mis.TechReport.SettlementMeans$SettlementName[a==lower]
                                            lower.sett <- ifelse(length(lower.sett)>1,
                                                                 ifelse((sd(b[Mis.TechReport.SettlementMeans$SettlementName==lower.sett[1]],na.rm=T)/
                                                                           sqrt(length(b[Mis.TechReport.SettlementMeans$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                                          (sd(b[Mis.TechReport.SettlementMeans$SettlementName==lower.sett[2]],na.rm=T)/
                                                                             sqrt(length(b[Mis.TechReport.SettlementMeans$SettlementName==lower.sett[2] & !is.na(b)]))),
                                                                        as.character(lower.sett[1]),as.character(lower.sett[2])),
                                                                 as.character(lower.sett))
                                            median.sett <- ifelse((sd(b[Mis.TechReport.SettlementMeans$SettlementName==upper.sett],na.rm=T)/
                                                                     sqrt(length(b[Mis.TechReport.SettlementMeans$SettlementName==upper.sett & !is.na(b)])))<
                                                                    (sd(b[Mis.TechReport.SettlementMeans$SettlementName==lower.sett],na.rm=T)/
                                                                       sqrt(length(b[Mis.TechReport.SettlementMeans$SettlementName==lower.sett & !is.na(b)]))),
                                                                  as.character(upper.sett),as.character(lower.sett))
                                          })

median.setts.Mis <- mapply(i=Mis.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                            j=names(even.number.setts.function.Mis),
                            function(i,j){
                              med <- median(i,na.rm=T)
                              med.setts <- factor(ifelse(length(sett.names.Mis)%%2!=0,
                                                         as.character(Mis.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                                         as.character(even.number.setts.function.Mis[j])),
                                                  levels=levels(sett.names.Mis))})

# ----
############################################################
#
# SECTION 3: Non-parametric Significance Test Functions (using Mann-Whitney U test)
#
############################################################
# ----

# NOTE: Typically, food security, material assets, marine tenure, enrollment rate, and place attachment end up being non-parametric; 
#       however, if the distribution appears to be normal, then PARAMETRIC tests are more powerful and the better choice.

# 3.1 Create function that will output significance values for non-parametric variables
#     BY SETTLEMENT (for status plots)

non.parametric.test.settlements.Mis <- 
  data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names.Mis[which(sett.names.Mis!=median.setts.Mis[a])]),
                                                               as.character(median.setts.Mis[a])),
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names.Mis[which(sett.names.Mis!=median.setts.Mis[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Mis.TechReport.MPAHouseholdData[Mis.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                          Mis.TechReport.MPAHouseholdData$SettlementName==median.setts.Mis[a],a]
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Mis.TechReport.MPAHouseholdData[Mis.TechReport.MPAHouseholdData$SettlementName==i |
                                                                                                                                           Mis.TechReport.MPAHouseholdData$SettlementName==median.setts.Mis[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median")))
                    }))

sigvals.Sett.Mis <- cbind.data.frame(non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"FSIndex.SettlementName"),
                                                                           c("FSIndex.SettlementName","FSIndex.p.value")],
                                      non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"MAIndex.SettlementName"),
                                                                           c("MAIndex.SettlementName","MAIndex.p.value")],
                                      non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"PAIndex.SettlementName"),
                                                                           c("PAIndex.SettlementName","PAIndex.p.value")],
                                      non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"MTIndex.SettlementName"),
                                                                           c("MTIndex.SettlementName","MTIndex.p.value")],
                                      non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"SERate.SettlementName"),
                                                                           c("SERate.SettlementName","SERate.p.value")],
                                      non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"TimeMarketClean.SettlementName"),
                                                                           c("TimeMarketClean.SettlementName","TimeMarketClean.p.value")],
                                      non.parametric.test.settlements.Mis[order(non.parametric.test.settlements.Mis$"DaysUnwell.SettlementName"),
                                                                           c("DaysUnwell.SettlementName","DaysUnwell.p.value")])

sigvals.Sett.Mis <- sigvals.Sett.Mis[,c(1,2,4,6,8,10,12,14)]
colnames(sigvals.Sett.Mis) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# 3.2 Create function that will output significance values for non-parametric variables
#     COMPARING MPA HOUSEHOLDS TO CONTROL HOUSEHOLDS

non.parametric.test.MPAvControl.Mis <- data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                                                          function(a){
                                                            var <- Mis.TechReport.MPAvControl[,a]
                                                            wilcox.test(var~MPA.v.Control,
                                                                        data=Mis.TechReport.MPAvControl,
                                                                        exact=F)}))["p.value",]

sigvals.MPA.Mis <- cbind.data.frame("MPA",non.parametric.test.MPAvControl.Mis)
colnames(sigvals.MPA.Mis) <- colnames(sigvals.Sett.Mis)

null.row.sigvals.Mis <- matrix(rep(NA,length(sigvals.MPA.Mis)),ncol=length(sigvals.MPA.Mis),dimnames=list(NULL,colnames(sigvals.MPA.Mis)))

sigvals.Mis <- rbind.data.frame(sigvals.MPA.Mis,
                                 null.row.sigvals.Mis,
                                 sigvals.Sett.Mis[rev(order(sigvals.Sett.Mis$SettlementName)),])

# 3.3 Create function that will output TREND significance values for non-parametric variables
#     BY MPA (for trend plots)

trend.non.parametric.test.byMPA.Mis <- data.frame(mapply(i=Mis.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                                                          function(i){
                                                            MannKendall(c(i[Mis.Trend.Data$InterviewYear==unique(Mis.Trend.Data$InterviewYear)[1]],
                                                                          i[Mis.Trend.Data$InterviewYear==unique(Mis.Trend.Data$InterviewYear)[2]],
                                                                          i[Mis.Trend.Data$InterviewYear==unique(Mis.Trend.Data$InterviewYear)[3]]))
                                                          }))
colnames(trend.non.parametric.test.byMPA.Mis) <- colnames(sigvals.Mis[2:8])

trend.sigvals.Mis <- cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Mis["sl",1],NA,trend.non.parametric.test.byMPA.Mis["sl",2],
                                       NA,trend.non.parametric.test.byMPA.Mis["sl",3],NA,trend.non.parametric.test.byMPA.Mis["sl",4],NA,trend.non.parametric.test.byMPA.Mis["sl",5],
                                       NA,trend.non.parametric.test.byMPA.Mis["sl",6],NA,trend.non.parametric.test.byMPA.Mis["sl",7],NA)
colnames(trend.sigvals.Mis) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")

# 3.4 Create function that will output TREND significance values for non-parametric variables
#     BY SETTLEMENT (for annex plots)

trend.non.parametric.test.bySett.Mis <- 
  cbind.data.frame(SettlementName=as.character(sett.names.Mis),
                   mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                          function(a){
                            t(data.frame(mapply(i=as.character(sett.names.Mis),
                                                function(i){
                                                  MannKendall(c(Mis.Trend.Data[Mis.Trend.Data$SettlementName==i &
                                                                                  Mis.Trend.Data$InterviewYear==unique(Mis.Trend.Data$InterviewYear)[1],a],
                                                                Mis.Trend.Data[Mis.Trend.Data$SettlementName==i &
                                                                                  Mis.Trend.Data$InterviewYear==unique(Mis.Trend.Data$InterviewYear)[2],a],
                                                                Mis.Trend.Data[Mis.Trend.Data$SettlementName==i &
                                                                                  Mis.Trend.Data$InterviewYear==unique(Mis.Trend.Data$InterviewYear)[3],a]))
                                                }))["sl",])}))
colnames(trend.non.parametric.test.bySett.Mis) <- colnames(sigvals.Mis)

annex.sigvals.Mis <- rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Mis["sl",]),
                                       null.row.sigvals.Mis,
                                       trend.non.parametric.test.bySett.Mis[rev(order(trend.non.parametric.test.bySett.Mis$SettlementName)),])
