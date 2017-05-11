# ----
# code:  Dampier Technical Report Significance Tests
# git branch: MPAMystery --> Social --> TechnicalReports --> Dampier
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
Damp.TechReport.MPAHouseholdData <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                         MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                                         MPA.TechReport.SigTest.Data$MPAID==5,], 
                                              Days.unwell.status[Days.unwell.status$MPAID==5,c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID")
Damp.TechReport.MPAHouseholdData$SettlementName <- factor(Damp.TechReport.MPAHouseholdData$SettlementName)

### "MPA versus Control" dataset
Damp.TechReport.MPAvControl <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==5 &
                                                    MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
                                         Days.unwell[Days.unwell$MPAID==5 &
                                                       Days.unwell$MonitoringYear=="4 Year Post",
                                                     c("HouseholdID","DaysUnwell")],
                                         by="HouseholdID")
Damp.TechReport.MPAvControl$MPA.v.Control <- factor(ifelse(Damp.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

### "Settlement Means" dataset
Damp.TechReport.SettlementMeans <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                 BigFive.YearGroup$Year=="4 Year Post" &
                                                                 BigFive.YearGroup$MPAID==5,
                                                               c(2,4,6,8,10,13)],
                                             Techreport.status[Techreport.status$MPAID==5,
                                                               c(1,3,6)],  
                                             by="SettlementID")
Damp.TechReport.SettlementMeans <- left_join(Damp.TechReport.SettlementMeans,
                                             Days.unwell.BySett[Days.unwell.BySett$MPAID==5,c(1,3,2)],
                                             by="SettlementID")

Damp.TechReport.SettlementMeans <- Damp.TechReport.SettlementMeans[!is.na(Damp.TechReport.SettlementMeans$SettlementName),]
colnames(Damp.TechReport.SettlementMeans) <- c("FSIndex","MAIndex","PAIndex","MTIndex","SERate",
                                               "SettlementID","SettlementName","TimeMarketClean","DaysUnwell","MPAID")

### "Trend" dataset
Damp.Trend.Data <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                        MPA.TechReport.SigTest.Data$MPAID==5,],
                             Days.unwell[Days.unwell$MPAID==5 &
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

sett.names.Damp <- factor(Damp.TechReport.SettlementMeans$SettlementName)

# 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric)

even.number.setts.function.Damp <- mapply(a=Damp.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
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

median.setts.Damp <- mapply(i=Damp.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                            j=names(even.number.setts.function.Damp),
                            function(i,j){
                              med <- median(i,na.rm=T)
                              med.setts <- factor(ifelse(length(sett.names.Damp)%%2!=0,
                                                         as.character(Damp.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                                         as.character(even.number.setts.function.Damp[j])),
                                                  levels=levels(sett.names.Damp))})

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

sigvals.Sett.Damp <- cbind.data.frame(non.parametric.test.settlements.Damp[order(non.parametric.test.settlements.Damp$"FSIndex.SettlementName"),
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

sigvals.Sett.Damp <- sigvals.Sett.Damp[,c(1,2,4,6,8,10,12,14)]
colnames(sigvals.Sett.Damp) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# 3.2 Create function that will output significance values for non-parametric variables
#     COMPARING MPA HOUSEHOLDS TO CONTROL HOUSEHOLDS

non.parametric.test.MPAvControl.Damp <- data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                                                          function(a){
                                                            var <- Damp.TechReport.MPAvControl[,a]
                                                            wilcox.test(var~MPA.v.Control,
                                                                        data=Damp.TechReport.MPAvControl,
                                                                        exact=F)}))["p.value",]

sigvals.MPA.Damp <- cbind.data.frame("MPA",non.parametric.test.MPAvControl.Damp)
colnames(sigvals.MPA.Damp) <- colnames(sigvals.Sett.Damp)

null.row.sigvals.Damp <- matrix(rep(NA,length(sigvals.MPA.Damp)),ncol=length(sigvals.MPA.Damp),dimnames=list(NULL,colnames(sigvals.MPA.Damp)))

sigvals.Damp <- rbind.data.frame(sigvals.MPA.Damp,
                                 null.row.sigvals.Damp,
                                 sigvals.Sett.Damp[rev(order(sigvals.Sett.Damp$SettlementName)),])

# 3.3 Create function that will output TREND significance values for non-parametric variables
#     BY MPA (for trend plots)

trend.non.parametric.test.byMPA.Damp <- data.frame(mapply(i=Damp.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                                                          function(i){
                                                            MannKendall(c(i[Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[1]],
                                                                          i[Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[2]],
                                                                          i[Damp.Trend.Data$InterviewYear==unique(Damp.Trend.Data$InterviewYear)[3]]))
                                                          }))
colnames(trend.non.parametric.test.byMPA.Damp) <- colnames(sigvals.Damp[2:8])

trend.sigvals.Damp <- cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Damp["sl",1],NA,trend.non.parametric.test.byMPA.Damp["sl",2],
                                       NA,trend.non.parametric.test.byMPA.Damp["sl",3],NA,trend.non.parametric.test.byMPA.Damp["sl",4],NA,trend.non.parametric.test.byMPA.Damp["sl",5],
                                       NA,trend.non.parametric.test.byMPA.Damp["sl",6],NA,trend.non.parametric.test.byMPA.Damp["sl",7],NA)
colnames(trend.sigvals.Damp) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")

# 3.4 Create function that will output TREND significance values for non-parametric variables
#     BY SETTLEMENT (for annex plots)

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

annex.sigvals.Damp <- rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Damp["sl",]),
                                       null.row.sigvals.Damp,
                                       trend.non.parametric.test.bySett.Damp[rev(order(trend.non.parametric.test.bySett.Damp$SettlementName)),])
