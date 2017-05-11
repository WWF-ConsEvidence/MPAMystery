# ----
# code:  Kofiau Technical Report Significance Tests
# git branch: MPAMystery --> Social --> TechnicalReports --> Kofiau
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
Kof.TechReport.MPAHouseholdData <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                                            MPA.TechReport.SigTest.Data$MonitoringYear== "4 Year Post" &
                                                                            MPA.TechReport.SigTest.Data$MPAID==4,], 
                                              Days.unwell.status[Days.unwell.status$MPAID==4,c("HouseholdID","DaysUnwell")],
                                              by="HouseholdID")
Kof.TechReport.MPAHouseholdData$SettlementName <- factor(Kof.TechReport.MPAHouseholdData$SettlementName)

### "MPA versus Control" dataset
Kof.TechReport.MPAvControl <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$MPAID==4 &
                                                                       MPA.TechReport.SigTest.Data$MonitoringYear=="4 Year Post",],
                                         Days.unwell[Days.unwell$MPAID==4 &
                                                       Days.unwell$MonitoringYear=="4 Year Post",
                                                     c("HouseholdID","DaysUnwell")],
                                         by="HouseholdID")
Kof.TechReport.MPAvControl$MPA.v.Control <- factor(ifelse(Kof.TechReport.MPAvControl$Treatment==1,"MPA","Control"))

### "Settlement Means" dataset
Kof.TechReport.SettlementMeans <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                 BigFive.YearGroup$Year=="4 Year Post" &
                                                                 BigFive.YearGroup$MPAID==4,
                                                               c(2,4,6,8,10,13)],
                                             Techreport.status[Techreport.status$MPAID==4,
                                                               c(1,3,6)],  
                                             by="SettlementID")
Kof.TechReport.SettlementMeans <- left_join(Kof.TechReport.SettlementMeans,
                                             Days.unwell.BySett[Days.unwell.BySett$MPAID==4,c(1,3,2)],
                                             by="SettlementID")

Kof.TechReport.SettlementMeans <- Kof.TechReport.SettlementMeans[!is.na(Kof.TechReport.SettlementMeans$SettlementName),]
colnames(Kof.TechReport.SettlementMeans) <- c("FSIndex","MAIndex","PAIndex","MTIndex","SERate",
                                               "SettlementID","SettlementName","TimeMarketClean","DaysUnwell","MPAID")

### "Trend" dataset
Kof.Trend.Data <- left_join(MPA.TechReport.SigTest.Data[MPA.TechReport.SigTest.Data$Treatment==1 &
                                                           MPA.TechReport.SigTest.Data$MPAID==4,],
                             Days.unwell[Days.unwell$MPAID==4 &
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

sett.names.Kof <- factor(Kof.TechReport.SettlementMeans$SettlementName)

# 2.2 Create list of median settlement for each variable (whether the variable is parametric or non-parametric)

even.number.setts.function.Kof <- mapply(a=Kof.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
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

median.setts.Kof <- mapply(i=Kof.TechReport.SettlementMeans[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                            j=names(even.number.setts.function.Kof),
                            function(i,j){
                              med <- median(i,na.rm=T)
                              med.setts <- factor(ifelse(length(sett.names.Kof)%%2!=0,
                                                         as.character(Kof.TechReport.SettlementMeans$SettlementName[which(i==med)]),
                                                         as.character(even.number.setts.function.Kof[j])),
                                                  levels=levels(sett.names.Kof))})

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

sigvals.Sett.Kof <- cbind.data.frame(non.parametric.test.settlements.Kof[order(non.parametric.test.settlements.Kof$"FSIndex.SettlementName"),
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

sigvals.Sett.Kof <- sigvals.Sett.Kof[,c(1,2,4,6,8,10,12,14)]
colnames(sigvals.Sett.Kof) <- c("SettlementName","FS.pval","MA.pval","PA.pval","MT.pval","SE.pval","Time.pval","Unwell.pval")


# 3.2 Create function that will output significance values for non-parametric variables
#     COMPARING MPA HOUSEHOLDS TO CONTROL HOUSEHOLDS

non.parametric.test.MPAvControl.Kof <- data.frame(mapply(a=c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell"),
                                                          function(a){
                                                            var <- Kof.TechReport.MPAvControl[,a]
                                                            wilcox.test(var~MPA.v.Control,
                                                                        data=Kof.TechReport.MPAvControl,
                                                                        exact=F)}))["p.value",]

sigvals.MPA.Kof <- cbind.data.frame("MPA",non.parametric.test.MPAvControl.Kof)
colnames(sigvals.MPA.Kof) <- colnames(sigvals.Sett.Kof)

null.row.sigvals.Kof <- matrix(rep(NA,length(sigvals.MPA.Kof)),ncol=length(sigvals.MPA.Kof),dimnames=list(NULL,colnames(sigvals.MPA.Kof)))

sigvals.Kof <- rbind.data.frame(sigvals.MPA.Kof,
                                 null.row.sigvals.Kof,
                                 sigvals.Sett.Kof[rev(order(sigvals.Sett.Kof$SettlementName)),])

# 3.3 Create function that will output TREND significance values for non-parametric variables
#     BY MPA (for trend plots)

trend.non.parametric.test.byMPA.Kof <- data.frame(mapply(i=Kof.Trend.Data[,c("FSIndex","MAIndex","PAIndex","MTIndex","SERate","TimeMarketClean","DaysUnwell")],
                                                          function(i){
                                                            MannKendall(c(i[Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[1]],
                                                                          i[Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[2]],
                                                                          i[Kof.Trend.Data$InterviewYear==unique(Kof.Trend.Data$InterviewYear)[3]]))
                                                          }))
colnames(trend.non.parametric.test.byMPA.Kof) <- colnames(sigvals.Kof[2:8])

trend.sigvals.Kof <- cbind.data.frame(MonitoringYear="p.value",trend.non.parametric.test.byMPA.Kof["sl",1],NA,trend.non.parametric.test.byMPA.Kof["sl",2],
                                       NA,trend.non.parametric.test.byMPA.Kof["sl",3],NA,trend.non.parametric.test.byMPA.Kof["sl",4],NA,trend.non.parametric.test.byMPA.Kof["sl",5],
                                       NA,trend.non.parametric.test.byMPA.Kof["sl",6],NA,trend.non.parametric.test.byMPA.Kof["sl",7],NA)
colnames(trend.sigvals.Kof) <- c("MonitoringYear","FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                  "TimeMarket","TimeMarketErr","Days.unwell","Days.unwell.err")

# 3.4 Create function that will output TREND significance values for non-parametric variables
#     BY SETTLEMENT (for annex plots)

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

annex.sigvals.Kof <- rbind.data.frame(cbind.data.frame(SettlementName="MPA",trend.non.parametric.test.byMPA.Kof["sl",]),
                                       null.row.sigvals.Kof,
                                       trend.non.parametric.test.bySett.Kof[rev(order(trend.non.parametric.test.bySett.Kof$SettlementName)),])
