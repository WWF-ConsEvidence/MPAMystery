# ----
# code:  Cenderawasih Technical Report Datasets
# git branch: MPAMystery --> Social --> TechnicalReports --> Cenderawasih
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
#  1) Data Configuration and Subsetting
#  2) Define Datasets for Status, Trend, and Annex Plots
# 
# ----
############################################################
#
# SECTION 1: Data Configuration and Subsetting
#
############################################################
# ----

# 1.1 MPA-level Proportional data (row to be added to bottom of status and annex plots in tech report)
TNTC.level.PropData.status <- data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                                           Techreport.status.MPA[Techreport.status.MPA$MPAID==2,
                                                                 c(2,3,5:35,37:38)]))
TNTC.level.PropData.annex <- cbind.data.frame(SettlementID=0,SettlementName="MPA",
                                              Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==2,
                                                                            c(2,3,5:35,37:38)],
                                              Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==2,
                                                                       c(2,3,5:35,37:38)],
                                              Techreport.status.MPA[Techreport.status.MPA$MPAID==2,
                                                                    c(2,3,5:35,37:38)])
colnames(TNTC.level.PropData.annex) <- c("SettlementID","SettlementName",
                                         paste(colnames(TNTC.level.PropData.annex)[3:37],".base",sep=""),
                                         paste(colnames(TNTC.level.PropData.annex)[38:72],".2yr",sep=""),
                                         paste(colnames(TNTC.level.PropData.annex)[73:107],".4yr",sep=""))

null.row.PropData <- matrix(rep(NA,38),ncol=38,dimnames=list(NULL,colnames(TNTC.level.PropData.status)))
null.row.PropData.annex <- matrix(rep(NA,107),ncol=107,dimnames=list(NULL,colnames(TNTC.level.PropData.annex)))

# 1.2 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report)
TNTC.level.ContData.status <- cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                                               BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,c(5:6,11:12,17:18,23:24,29:30)],
                                               Techreport.status.MPA[Techreport.status.MPA$MPAID==2,c(4,36)],
                                               matrix(Days.unwell.TNTC.ByMPA[1,"Status"],
                                                      dimnames=list(NULL,"Days.unwell")),
                                               matrix(Days.unwell.TNTC.ByMPA[2,"Status"],
                                                      dimnames=list(NULL,"Days.unwell.err")))
TNTC.level.ContData.annex <- cbind.data.frame(SettlementID=0,SettlementName="MPA",
                                              BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,c(1:30)],
                                              Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==2,c(4,36)],
                                              Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==2,c(4,36)],
                                              Techreport.status.MPA[Techreport.status.MPA$MPAID==2,c(4,36)],
                                              matrix(Days.unwell.TNTC.ByMPA[1,"Baseline"],
                                                     dimnames=list(NULL,"Days.unwell")),
                                              matrix(Days.unwell.TNTC.ByMPA[2,"Baseline"],
                                                     dimnames=list(NULL,"Days.unwell.err")),
                                              matrix(Days.unwell.TNTC.ByMPA[1,"2Yr"],
                                                     dimnames=list(NULL,"Days.unwell")),
                                              matrix(Days.unwell.TNTC.ByMPA[2,"2Yr"],
                                                     dimnames=list(NULL,"Days.unwell.err")),
                                              matrix(Days.unwell.TNTC.ByMPA[1,"Status"],
                                                     dimnames=list(NULL,"Days.unwell")),
                                              matrix(Days.unwell.TNTC.ByMPA[2,"Status"],
                                                     dimnames=list(NULL,"Days.unwell.err")))

colnames(TNTC.level.ContData.status) <- c(colnames(TNTC.level.ContData.status[1:3]),
                                          "FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                          colnames(TNTC.level.ContData.status[14:17]))
colnames(TNTC.level.ContData.annex) <- c(colnames(TNTC.level.ContData.annex[1:32]),
                                         "TimeMarket.base","TimeMarketErr.base","TimeMarket.2yr","TimeMarketErr.2yr","TimeMarket.4yr",
                                         "TimeMarketErr.4yr","DaysUnwell.base","DaysUnwellErr.base","DaysUnwell.2yr","DaysUnwellErr.2yr",
                                         "DaysUnwell.4yr","DaysUnwellErr.4yr")

null.row.ContData <- matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(TNTC.level.ContData.status)))
null.row.ContData.annex <- matrix(rep(NA,44),ncol=44,dimnames=list(NULL,colnames(TNTC.level.ContData.annex)))

# 1.3 Subset Days Unwell variable by settlement and MPA
Days.unwell.TNTC.BySett <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                             Days.unwell.baseline.BySett[Days.unwell.baseline.BySett$MPAID==2 &
                                                                                           !is.na(Days.unwell.baseline.BySett$SettlementID),c(1,3,4)]),
                                            cbind.data.frame(MonitoringYear="Baseline",
                                                             SettlementID=c(104:112),
                                                             Days.unwell=rep(NA,9),
                                                             Days.unwell.err=rep(NA,9)),
                                            cbind.data.frame(MonitoringYear="2 Year Post",
                                                             Days.unwell.2yr.BySett[Days.unwell.2yr.BySett$MPAID==2 &
                                                                                      !is.na(Days.unwell.2yr.BySett$SettlementID),c(1,3,4)]),
                                            cbind.data.frame(MonitoringYear="4 Year Post",
                                                             Days.unwell.BySett[Days.unwell.BySett$MPAID==2 &
                                                                                  !is.na(Days.unwell.BySett$SettlementID),c(1,3,4)]))

Days.unwell.TNTC.ByMPA <- as.data.frame(matrix(c(Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==2 &
                                                                     !is.na(Days.unwell.ByMPA$MPAID),c(2,3)],
                                                 Days.unwell.baseline.ByMPA[Days.unwell.baseline.ByMPA$MPAID==2 &
                                                                              !is.na(Days.unwell.baseline.ByMPA$MPAID),c(2,3)],
                                                 Days.unwell.2yr.ByMPA[Days.unwell.2yr.ByMPA$MPAID==2 &
                                                                         !is.na(Days.unwell.2yr.ByMPA$MPAID),c(2,3)]),
                                               ncol=3,dimnames=list(NULL,c("Status","Baseline","2Yr"))))

# 1.4 Subset Proportional Data of Age/Gender for TNTC
TNTC.AgeGender <- data.frame(AgeCat=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                      "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                             Male.Baseline=t(AgeGenderDemos.base.ByMPA[AgeGenderDemos.base.ByMPA$MPAID==2,seq(2,40,by=2)]),
                             Female.Baseline=t(AgeGenderDemos.base.ByMPA[AgeGenderDemos.base.ByMPA$MPAID==2,seq(3,41,by=2)]),
                             Male.2yr=t(AgeGenderDemos.2yr.ByMPA[AgeGenderDemos.2yr.ByMPA$MPAID==2,seq(2,40,by=2)]),
                             Female.2yr=t(AgeGenderDemos.2yr.ByMPA[AgeGenderDemos.2yr.ByMPA$MPAID==2,seq(3,41,by=2)]),
                             Male.4yr=t(AgeGenderDemos.status.ByMPA[AgeGenderDemos.status.ByMPA$MPAID==2,seq(2,40,by=2)]),
                             Female.4yr=t(AgeGenderDemos.status.ByMPA[AgeGenderDemos.status.ByMPA$MPAID==2,seq(3,41,by=2)]),
                             row.names=NULL)

# 1.5 Source Statistical Test Results from "TNTC.TechReport.SigTests.R"
source()

# ----
############################################################
#
# SECTION 2: Define Datasets for Status, Trend, and Annex Plots
#
############################################################
# ----

# 2.1 Status dataset for TNTC, proportional data
TNTC.PropData.Techreport.status <- Techreport.status[Techreport.status$MPAID==2,c(1,3,4,5,7:37,39:40)]
TNTC.PropData.Techreport.status <- TNTC.PropData.Techreport.status[rev(order(TNTC.PropData.Techreport.status$SettlementName)),]

TNTC.PropData.Techreport.status.PLOTFORMAT <- rbind.data.frame(TNTC.level.PropData.status[2:38],
                                                               null.row.PropData[1:37],
                                                               TNTC.PropData.Techreport.status)

# 2.2 Status dataset for TNTC, continuous data (with p values)
TNTC.ContData.Techreport.status <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                 BigFive.YearGroup$Year=="4 Year Post" &
                                                                 BigFive.YearGroup$MPAID==2,
                                                               c(2:11,13)],
                                             Techreport.status[Techreport.status$MPAID==2,c(1,3,6,38)],
                                             by="SettlementID")
TNTC.ContData.Techreport.status <- left_join(TNTC.ContData.Techreport.status,
                                             Days.unwell.TNTC.BySett[Days.unwell.TNTC.BySett$MonitoringYear=="4 Year Post",
                                                                     2:4],
                                             by="SettlementID")
TNTC.ContData.Techreport.status <- cbind.data.frame(TNTC.ContData.Techreport.status[rev(order(TNTC.ContData.Techreport.status$SettlementName)),11:12],
                                                    TNTC.ContData.Techreport.status[rev(order(TNTC.ContData.Techreport.status$SettlementName)),c(1:10,13:16)])
TNTC.ContData.Techreport.status.withMPA <- rbind.data.frame(TNTC.level.ContData.status[2:17],
                                                            null.row.ContData[2:17],
                                                            TNTC.ContData.Techreport.status)
TNTC.ContData.Techreport.status.withMPA$SettlementName <- factor(TNTC.ContData.Techreport.status.withMPA$SettlementName)
TNTC.ContData.Techreport.status.withMPA$SettlementName <- factor(TNTC.ContData.Techreport.status.withMPA$SettlementName)
TNTC.ContData.Techreport.status.PLOTFORMAT <- left_join(TNTC.ContData.Techreport.status.withMPA,
                                                        sigvals.TNTC,by="SettlementName")

# 2.3 Trend dataset for TNTC, MPA-level proportional data
TNTC.TrendPropData.Techreport.PLOTFORMAT <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                              Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==2,c(2:3,5:35,37:38)]),
                                                             cbind.data.frame(MonitoringYear="2 Year Post",
                                                                              Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==2,c(2:3,5:35,37:38)]),
                                                             cbind.data.frame(MonitoringYear="4 Year Post",
                                                                              Techreport.status.MPA[Techreport.status.MPA$MPAID==2,c(2:3,5:35,37:38)]))


# 2.4 Trend dataset for TNTC, MPA-level continuous data (with p values)
TNTC.TrendContData.Techreport <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                   matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,c(1:2,7:8,13:14,19:20,25:26)],
                                                                          ncol=10,dimnames=list(NULL,
                                                                                                colnames(TNTC.ContData.Techreport.status[3:12]))),
                                                                   Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==2,c(4,36)],
                                                                   matrix(Days.unwell.TNTC.ByMPA[1,"Baseline"],dimnames=list(NULL,"Days.unwell")),
                                                                   matrix(Days.unwell.TNTC.ByMPA[2,"Baseline"],dimnames=list(NULL,"Days.unwell.err"))),
                                                  cbind.data.frame(MonitoringYear="2 Year Post",
                                                                   matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,c(3:4,9:10,15:16,21:22,27:28)],
                                                                          ncol=10,dimnames=list(NULL,
                                                                                                colnames(TNTC.ContData.Techreport.status[3:12]))),
                                                                   Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==2,c(4,36)],
                                                                   matrix(Days.unwell.TNTC.ByMPA[1,"2Yr"],dimnames=list(NULL,"Days.unwell")),
                                                                   matrix(Days.unwell.TNTC.ByMPA[2,"2Yr"],dimnames=list(NULL,"Days.unwell.err"))),
                                                  cbind.data.frame(MonitoringYear="4 Year Post",
                                                                   matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==2,c(5:6,11:12,17:18,23:24,29:30)],
                                                                          ncol=10,dimnames=list(NULL,
                                                                                                colnames(TNTC.ContData.Techreport.status[3:12]))),
                                                                   Techreport.status.MPA[Techreport.status.MPA$MPAID==2,c(4,36)],
                                                                   matrix(Days.unwell.TNTC.ByMPA[1,"Status"],dimnames=list(NULL,"Days.unwell")),
                                                                   matrix(Days.unwell.TNTC.ByMPA[2,"Status"],dimnames=list(NULL,"Days.unwell.err"))))
TNTC.TrendContData.Techreport.PLOTFORMAT <- rbind.data.frame(TNTC.TrendContData.Techreport,
                                                             trend.sigvals.TNTC)

# 2.5 Annex dataset for TNTC, Settlement-level proportional data
TNTC.AnnexPropData.Techreport <- cbind.data.frame(Techreport.trend.Baseline.Sett[Techreport.trend.Baseline.Sett$MPAID==2,c(1,3:5,7:37,39:40)],
                                                  Techreport.trend.2yr.Sett[Techreport.trend.2yr.Sett$MPAID==2,c(4:5,7:37,39:40)],
                                                  Techreport.status[Techreport.status$MPAID==2,c(4:5,7:37,39:40)])
colnames(TNTC.AnnexPropData.Techreport) <- c("SettlementID","SettlementName",
                                             paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".base",sep=""),
                                             paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".2yr",sep=""),
                                             paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".4yr",sep=""))
TNTC.AnnexPropData.Techreport <- TNTC.AnnexPropData.Techreport[rev(order(TNTC.AnnexPropData.Techreport$SettlementName)),]
TNTC.AnnexPropData.Techreport.PLOTFORMAT <- rbind.data.frame(TNTC.level.PropData.annex,
                                                             null.row.PropData.annex,
                                                             TNTC.AnnexPropData.Techreport)

# 2.6 Annex dataset for TNTC, Settlement-level continuous data (with p values)
TNTC.TimeMarket.AnnexData <- cbind.data.frame(Techreport.trend.Baseline.Sett[Techreport.trend.Baseline.Sett$MPAID==2,c(1,6,38)],
                                              Techreport.trend.2yr.Sett[Techreport.trend.2yr.Sett$MPAID==2,c(6,38)],
                                              Techreport.status[Techreport.status$MPAID==2,c(6,38)])
colnames(TNTC.TimeMarket.AnnexData) <- c("SettlementID","TimeMarket.base","TimeMarketErr.base","TimeMarket.2yr",
                                         "TimeMarketErr.2yr","TimeMarket.4yr","TimeMarketErr.4yr")
TNTC.DaysUnwell.AnnexData <- cbind.data.frame(Days.unwell.TNTC.BySett[Days.unwell.TNTC.BySett$MonitoringYear=="Baseline",2:4],
                                              Days.unwell.TNTC.BySett[Days.unwell.TNTC.BySett$MonitoringYear=="2 Year Post",3:4],
                                              Days.unwell.TNTC.BySett[Days.unwell.TNTC.BySett$MonitoringYear=="4 Year Post",3:4])
colnames(TNTC.DaysUnwell.AnnexData) <- c("SettlementID","DaysUnwell.base","DaysUnwellErr.base","DaysUnwell.2yr",
                                         "DaysUnwellErr.2yr","DaysUnwell.4yr","DaysUnwellErr.4yr")

TNTC.AnnexContData.Techreport <- left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==2 &
                                                                 BigFive.SettleGroup$Treatment==1,
                                                               c(1:30,32:33)],
                                           TNTC.TimeMarket.AnnexData,
                                           by="SettlementID")
TNTC.AnnexContData.Techreport <- left_join(TNTC.AnnexContData.Techreport,
                                           TNTC.DaysUnwell.AnnexData,
                                           by="SettlementID")

TNTC.AnnexContData.Techreport <- TNTC.AnnexContData.Techreport[rev(order(TNTC.AnnexContData.Techreport$SettlementName)),c(31:32,1:30,33:44)]
TNTC.AnnexContData.Techreport.PLOTFORMAT <- rbind.data.frame(TNTC.level.ContData.annex,
                                                             null.row.ContData.annex,
                                                             TNTC.AnnexContData.Techreport)

# write.xlsx(TNTC.PropData.Techreport.status.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Proportional.data.statusplots.xlsx",row.names=F)
# write.xlsx(TNTC.ContData.Techreport.status.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Continuous.data.statusplots.withpvals.xlsx",row.names=F)
# write.xlsx(TNTC.TrendPropData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Proportional.data.trendplots.xlsx",row.names=F)
# write.xlsx(TNTC.TrendContData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Continuous.data.trendplots.withpvals.xlsx",row.names=F)
# write.xlsx(TNTC.AnnexPropData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Proportional.data.annexplots.xlsx",row.names=F)
# write.xlsx(TNTC.AnnexContData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Continuous.data.annexplots.xlsx",row.names=F)
# write.xlsx(annex.sigvals.TNTC,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/Pvals.continuous.data.annexplots.xlsx",row.names=F)
# write.xlsx(TNTC.AgeGender,"Social impacts, BHS -- Kelly/R codes & data/Phil TNTC Tech Report Data/AgeGender.xlsx",row.names=F)
