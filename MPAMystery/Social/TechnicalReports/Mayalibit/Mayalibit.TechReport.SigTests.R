# ----
# code:  Mayalibit Technical Report Significance Tests
# git branch: MPAMystery --> Social --> TechnicalReports --> Mayalibit
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
Maya.TechReport.MPAHouseholdData <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                                            MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                                                            MPA.TechReport.SigTest.Data$MPAID==1,], 
                                              Days.unwell.status[Days.unwell.status$MPAID==1,c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID")
Maya.TechReport.MPAHouseholdData$SettlementName <- factor(Maya.TechReport.MPAHouseholdData$SettlementName)

### "MPA versus Control" dataset
Maya.TechReport.MPAvControl <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==1 &
                                                                       MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
                                         Days.unwell[Days.unwell$MPAID==1 &
                                                       Days.unwell$MonitoringYear=="4 Year Post",
                                                     c("HouseholdID","DaysUnwell")],
                                         by="HouseholdID")
Maya.TechReport.MPAvControl$MPA.v.Control <- factor(ifelse(Maya.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

### "Settlement Means" dataset
Maya.TechReport.SettlementMeans <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                 BigFive.YearGroup$Year=="4 Year Post" &
                                                                 BigFive.YearGroup$MPAID==1,
                                                               c(2,4,6,8,10,13)],
                                             Techreport.status[Techreport.status$MPAID==1,
                                                               c(1,3,6)],  
                                             by="SettlementID")
Maya.TechReport.SettlementMeans <- left_join(Maya.TechReport.SettlementMeans,
                                             Days.unwell.BySett[Days.unwell.BySett$MPAID==1,c(1,3,2)],
                                             by="SettlementID")

Maya.TechReport.SettlementMeans <- Maya.TechReport.SettlementMeans[!is.na(Maya.TechReport.SettlementMeans$SettlementName),]
colnames(Maya.TechReport.SettlementMeans) <- c("FSIndex","MAIndex","PAIndex","MTIndex","SERate",
                                               "SettlementID","SettlementName","TimeMarketClean","DaysUnwell","MPAID")

### "Trend" dataset
Maya.Trend.Data <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                           MPA.TechReport.SigTest.Data$MPAID==1,],
                             Days.unwell[Days.unwell$MPAID==1 &
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

sett.names.Maya <- factor(Maya.TechReport.SettlementMeans$SettlementName)

# 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric)

even.number.setts.function.Maya <- mapply(a=Maya.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
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

median.setts.Maya <- mapply(i=Maya.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                            j=names(even.number.setts.function.Maya),
                            function(i,j){
                              med <- median(i,na.rm=T)
                              med.setts <- factor(ifelse(length(sett.names.Maya)%%2!=0,
                                                         as.character(Maya.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                                         as.character(even.number.setts.function.Maya[j])),
                                                  levels=levels(sett.names.Maya))})

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

sigvals.Sett.Maya <- cbind.data.frame(non.parametric.test.settlements.Maya[order(non.parametric.test.settlements.Maya$"FSIndex.SettlementName"),
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

sigvals.Sett.Maya <- sigvals.Sett.Maya[,c(1,2,4,6,8,10,12,14)]
colnames(sigvals.Sett.Maya) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# 3.2 Create function that will output significance values for non-parametric variables
#     COMPARING MPA HOUSEHOLDS TO CONTROL HOUSEHOLDS

non.parametric.test.MPAvControl.Maya <- data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                                                          function(a){
                                                            var <- Maya.TechReport.MPAvControl[,a]
                                                            wilcox.test(var~MPA.v.Control,
                                                                        data=Maya.TechReport.MPAvControl,
                                                                        exact=F)}))["p.value",]

sigvals.MPA.Maya <- cbind.data.frame("MPA",non.parametric.test.MPAvControl.Maya)
colnames(sigvals.MPA.Maya) <- colnames(sigvals.Sett.Maya)

null.row.sigvals.Maya <- matrix(rep(NA,length(sigvals.MPA.Maya)),ncol=length(sigvals.MPA.Maya),dimnames=list(NULL,colnames(sigvals.MPA.Maya)))

sigvals.Maya <- rbind.data.frame(sigvals.MPA.Maya,
                                 null.row.sigvals.Maya,
                                 sigvals.Sett.Maya[rev(order(sigvals.Sett.Maya$SettlementName)),])

# 3.3 Create function that will output TREND significance values for non-parametric variables
#     BY MPA (for trend plots)

trend.non.parametric.test.byMPA.Maya <- data.frame(mapply(i=Maya.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                                                          function(i){
                                                            MannKendall(c(i[Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[1]],
                                                                          i[Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[2]],
                                                                          i[Maya.Trend.Data$InterviewYear==unique(Maya.Trend.Data$InterviewYear)[3]]))
                                                          }))
colnames(trend.non.parametric.test.byMPA.Maya) <- colnames(sigvals.Maya[2:8])

trend.sigvals.Maya <- cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Maya["sl",1],NA,trend.non.parametric.test.byMPA.Maya["sl",2],
                                       NA,trend.non.parametric.test.byMPA.Maya["sl",3],NA,trend.non.parametric.test.byMPA.Maya["sl",4],NA,trend.non.parametric.test.byMPA.Maya["sl",5],
                                       NA,trend.non.parametric.test.byMPA.Maya["sl",6],NA,trend.non.parametric.test.byMPA.Maya["sl",7],NA)
colnames(trend.sigvals.Maya) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")

# 3.4 Create function that will output TREND significance values for non-parametric variables
#     BY SETTLEMENT (for annex plots)

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

annex.sigvals.Maya <- rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Maya["sl",]),
                                       null.row.sigvals.Maya,
                                       trend.non.parametric.test.bySett.Maya[rev(order(trend.non.parametric.test.bySett.Maya$SettlementName)),])
