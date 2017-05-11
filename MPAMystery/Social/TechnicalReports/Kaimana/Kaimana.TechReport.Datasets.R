# ----
# code:  Kaimana Technical Report Datasets
# git branch: MPAMystery --> Social --> TechnicalReports --> Kaimana
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
Kaimana.level.PropData.status <- data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                                              Techreport.status.MPA[Techreport.status.MPA$MPAID==3,
                                                                    c(2,3,5:35,37:38)]))
Kaimana.level.PropData.annex <- cbind.data.frame(SettlementID=0,SettlementName="MPA",
                                                 Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==3,
                                                                               c(2,3,5:35,37:38)],
                                                 Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==3,
                                                                          c(2,3,5:35,37:38)],
                                                 Techreport.status.MPA[Techreport.status.MPA$MPAID==3,
                                                                       c(2,3,5:35,37:38)])
colnames(Kaimana.level.PropData.annex) <- c("SettlementID","SettlementName",
                                            paste(colnames(Kaimana.level.PropData.annex)[3:37],".base",sep=""),
                                            paste(colnames(Kaimana.level.PropData.annex)[38:72],".2yr",sep=""),
                                            paste(colnames(Kaimana.level.PropData.annex)[73:107],".4yr",sep=""))

null.row.PropData <- matrix(rep(NA,38),ncol=38,dimnames=list(NULL,colnames(Kaimana.level.PropData.status)))
null.row.PropData.annex <- matrix(rep(NA,107),ncol=107,dimnames=list(NULL,colnames(Kaimana.level.PropData.annex)))

# 1.2 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report)
Kaimana.level.ContData.status <- cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                                                  BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,c(5:6,11:12,17:18,23:24,29:30)],
                                                  Techreport.status.MPA[Techreport.status.MPA$MPAID==3,c(4,36)],
                                                  matrix(Days.unwell.Kaimana.ByMPA[1,"Status"],
                                                         dimnames=list(NULL,"Days.unwell")),
                                                  matrix(Days.unwell.Kaimana.ByMPA[2,"Status"],
                                                         dimnames=list(NULL,"Days.unwell.err")))
Kaimana.level.ContData.annex <- cbind.data.frame(SettlementID=0,SettlementName="MPA",
                                                 BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,c(1:30)],
                                                 Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==3,c(4,36)],
                                                 Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==3,c(4,36)],
                                                 Techreport.status.MPA[Techreport.status.MPA$MPAID==3,c(4,36)],
                                                 matrix(Days.unwell.Kaimana.ByMPA[1,"Baseline"],
                                                        dimnames=list(NULL,"Days.unwell")),
                                                 matrix(Days.unwell.Kaimana.ByMPA[2,"Baseline"],
                                                        dimnames=list(NULL,"Days.unwell.err")),
                                                 matrix(Days.unwell.Kaimana.ByMPA[1,"2Yr"],
                                                        dimnames=list(NULL,"Days.unwell")),
                                                 matrix(Days.unwell.Kaimana.ByMPA[2,"2Yr"],
                                                        dimnames=list(NULL,"Days.unwell.err")),
                                                 matrix(Days.unwell.Kaimana.ByMPA[1,"Status"],
                                                        dimnames=list(NULL,"Days.unwell")),
                                                 matrix(Days.unwell.Kaimana.ByMPA[2,"Status"],
                                                        dimnames=list(NULL,"Days.unwell.err")))

colnames(Kaimana.level.ContData.status) <- c(colnames(Kaimana.level.ContData.status[1:3]),
                                             "FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                             colnames(Kaimana.level.ContData.status[14:17]))
colnames(Kaimana.level.ContData.annex) <- c(colnames(Kaimana.level.ContData.annex[1:32]),
                                            "TimeMarket.base","TimeMarketErr.base","TimeMarket.2yr","TimeMarketErr.2yr","TimeMarket.4yr",
                                            "TimeMarketErr.4yr","DaysUnwell.base","DaysUnwellErr.base","DaysUnwell.2yr","DaysUnwellErr.2yr",
                                            "DaysUnwell.4yr","DaysUnwellErr.4yr")

null.row.ContData <- matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Kaimana.level.ContData.status)))
null.row.ContData.annex <- matrix(rep(NA,44),ncol=44,dimnames=list(NULL,colnames(Kaimana.level.ContData.annex)))

# 1.3 Subset Days Unwell variable by settlement and MPA
Days.unwell.Kaimana.BySett <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                Days.unwell.baseline.BySett[Days.unwell.baseline.BySett$MPAID==3 &
                                                                                              !is.na(Days.unwell.baseline.BySett$SettlementID),c(1,3,4)]),
                                               cbind.data.frame(MonitoringYear="Baseline",
                                                                SettlementID=c(113:115),
                                                                Days.unwell=rep(NA,3),
                                                                Days.unwell.err=rep(NA,3)),
                                               cbind.data.frame(MonitoringYear="2 Year Post",
                                                                Days.unwell.2yr.BySett[Days.unwell.2yr.BySett$MPAID==3 &
                                                                                         !is.na(Days.unwell.2yr.BySett$SettlementID),c(1,3,4)]),
                                               cbind.data.frame(MonitoringYear="4 Year Post",
                                                                Days.unwell.BySett[Days.unwell.BySett$MPAID==3 &
                                                                                     !is.na(Days.unwell.BySett$SettlementID),c(1,3,4)]))

Days.unwell.Kaimana.ByMPA <- as.data.frame(matrix(c(Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==3 &
                                                                        !is.na(Days.unwell.ByMPA$MPAID),c(2,3)],
                                                    Days.unwell.baseline.ByMPA[Days.unwell.baseline.ByMPA$MPAID==3 &
                                                                                 !is.na(Days.unwell.baseline.ByMPA$MPAID),c(2,3)],
                                                    Days.unwell.2yr.ByMPA[Days.unwell.2yr.ByMPA$MPAID==3 &
                                                                            !is.na(Days.unwell.2yr.ByMPA$MPAID),c(2,3)]),
                                                  ncol=3,dimnames=list(NULL,c("Status","Baseline","2Yr"))))

# 1.4 Subset Proportional Data of Age/Gender for Kaimana
Kaimana.AgeGender <- data.frame(AgeCat=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                         "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                                Male.Baseline=t(AgeGenderDemos.base.ByMPA[AgeGenderDemos.base.ByMPA$MPAID==3,seq(2,40,by=2)]),
                                Female.Baseline=t(AgeGenderDemos.base.ByMPA[AgeGenderDemos.base.ByMPA$MPAID==3,seq(3,41,by=2)]),
                                Male.2yr=t(AgeGenderDemos.2yr.ByMPA[AgeGenderDemos.2yr.ByMPA$MPAID==3,seq(2,40,by=2)]),
                                Female.2yr=t(AgeGenderDemos.2yr.ByMPA[AgeGenderDemos.2yr.ByMPA$MPAID==3,seq(3,41,by=2)]),
                                Male.4yr=t(AgeGenderDemos.status.ByMPA[AgeGenderDemos.status.ByMPA$MPAID==3,seq(2,40,by=2)]),
                                Female.4yr=t(AgeGenderDemos.status.ByMPA[AgeGenderDemos.status.ByMPA$MPAID==3,seq(3,41,by=2)]),
                                row.names=NULL)

# 1.5 Source Statistical Test Results from "Kaimana.TechReport.SigTests.R"
source()

# ----
############################################################
#
# SECTION 2: Define Datasets for Status, Trend, and Annex Plots
#
############################################################
# ----

# 2.1 Status dataset for Kaimana, proportional data
Kaimana.PropData.Techreport.status <- Techreport.status[Techreport.status$MPAID==3,c(1,3,4,5,7:37,39:40)]
Kaimana.PropData.Techreport.status <- Kaimana.PropData.Techreport.status[rev(order(Kaimana.PropData.Techreport.status$SettlementName)),]

Kaimana.PropData.Techreport.status.PLOTFORMAT <- rbind.data.frame(Kaimana.level.PropData.status[2:38],
                                                                  null.row.PropData[1:37],
                                                                  Kaimana.PropData.Techreport.status)

# 2.2 Status dataset for Kaimana, continuous data (with p values)
Kaimana.ContData.Techreport.status <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                    BigFive.YearGroup$Year=="4 Year Post" &
                                                                    BigFive.YearGroup$MPAID==3,
                                                                  c(2:11,13)],
                                                Techreport.status[Techreport.status$MPAID==3,c(1,3,6,38)],
                                                by="SettlementID")
Kaimana.ContData.Techreport.status <- left_join(Kaimana.ContData.Techreport.status,
                                                Days.unwell.Kaimana.BySett[Days.unwell.Kaimana.BySett$MonitoringYear=="4 Year Post",
                                                                           2:4],
                                                by="SettlementID")
Kaimana.ContData.Techreport.status <- cbind.data.frame(Kaimana.ContData.Techreport.status[rev(order(Kaimana.ContData.Techreport.status$SettlementName)),11:12],
                                                       Kaimana.ContData.Techreport.status[rev(order(Kaimana.ContData.Techreport.status$SettlementName)),c(1:10,13:16)])
Kaimana.ContData.Techreport.status.withMPA <- rbind.data.frame(Kaimana.level.ContData.status[2:17],
                                                               null.row.ContData[2:17],
                                                               Kaimana.ContData.Techreport.status)
Kaimana.ContData.Techreport.status.withMPA$SettlementName <- factor(Kaimana.ContData.Techreport.status.withMPA$SettlementName)
Kaimana.ContData.Techreport.status.PLOTFORMAT <- left_join(Kaimana.ContData.Techreport.status.withMPA,
                                                           sigvals.Kai,by="SettlementName")


# 2.3 Trend dataset for Kaimana, MPA-level proportional data
Kaimana.TrendPropData.Techreport.PLOTFORMAT <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                                 Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==3,c(2:3,5:35,37:38)]),
                                                                cbind.data.frame(MonitoringYear="2 Year Post",
                                                                                 Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==3,c(2:3,5:35,37:38)]),
                                                                cbind.data.frame(MonitoringYear="4 Year Post",
                                                                                 Techreport.status.MPA[Techreport.status.MPA$MPAID==3,c(2:3,5:35,37:38)]))


# 2.4 Trend dataset for Kaimana, MPA-level continuous data (with p values)
Kaimana.TrendContData.Techreport <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                      matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,c(1:2,7:8,13:14,19:20,25:26)],
                                                                             ncol=10,dimnames=list(NULL,
                                                                                                   colnames(Kaimana.ContData.Techreport.status[3:12]))),
                                                                      Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==3,c(4,36)],
                                                                      matrix(Days.unwell.Kaimana.ByMPA[1,"Baseline"],dimnames=list(NULL,"Days.unwell")),
                                                                      matrix(Days.unwell.Kaimana.ByMPA[2,"Baseline"],dimnames=list(NULL,"Days.unwell.err"))),
                                                     cbind.data.frame(MonitoringYear="2 Year Post",
                                                                      matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,c(3:4,9:10,15:16,21:22,27:28)],
                                                                             ncol=10,dimnames=list(NULL,
                                                                                                   colnames(Kaimana.ContData.Techreport.status[3:12]))),
                                                                      Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==3,c(4,36)],
                                                                      matrix(Days.unwell.Kaimana.ByMPA[1,"2Yr"],dimnames=list(NULL,"Days.unwell")),
                                                                      matrix(Days.unwell.Kaimana.ByMPA[2,"2Yr"],dimnames=list(NULL,"Days.unwell.err"))),
                                                     cbind.data.frame(MonitoringYear="4 Year Post",
                                                                      matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==3,c(5:6,11:12,17:18,23:24,29:30)],
                                                                             ncol=10,dimnames=list(NULL,
                                                                                                   colnames(Kaimana.ContData.Techreport.status[3:12]))),
                                                                      Techreport.status.MPA[Techreport.status.MPA$MPAID==3,c(4,36)],
                                                                      matrix(Days.unwell.Kaimana.ByMPA[1,"Status"],dimnames=list(NULL,"Days.unwell")),
                                                                      matrix(Days.unwell.Kaimana.ByMPA[2,"Status"],dimnames=list(NULL,"Days.unwell.err"))))
Kaimana.TrendContData.Techreport.PLOTFORMAT <- rbind.data.frame(Kaimana.TrendContData.Techreport,
                                                                trend.sigvals.Kai)

# 2.5 Annex dataset for Kaimana, Settlement-level proportional data
Kaimana.AnnexPropData.Techreport <- cbind.data.frame(Techreport.trend.Baseline.Sett[Techreport.trend.Baseline.Sett$MPAID==3,c(1,3:5,7:37,39:40)],
                                                     Techreport.trend.2yr.Sett[Techreport.trend.2yr.Sett$MPAID==3,c(4:5,7:37,39:40)],
                                                     Techreport.status[Techreport.status$MPAID==3,c(4:5,7:37,39:40)])
colnames(Kaimana.AnnexPropData.Techreport) <- c("SettlementID","SettlementName",
                                                paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".base",sep=""),
                                                paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".2yr",sep=""),
                                                paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".4yr",sep=""))
Kaimana.AnnexPropData.Techreport <- Kaimana.AnnexPropData.Techreport[rev(order(Kaimana.AnnexPropData.Techreport$SettlementName)),]
Kaimana.AnnexPropData.Techreport.PLOTFORMAT <- rbind.data.frame(Kaimana.level.PropData.annex,
                                                                null.row.PropData.annex,
                                                                Kaimana.AnnexPropData.Techreport)

# 2.6 Annex dataset for Kaimana, Settlement-level continuous data (with p values)
Kaimana.TimeMarket.AnnexData <- cbind.data.frame(Techreport.trend.Baseline.Sett[Techreport.trend.Baseline.Sett$MPAID==3,c(1,6,38)],
                                                 Techreport.trend.2yr.Sett[Techreport.trend.2yr.Sett$MPAID==3,c(6,38)],
                                                 Techreport.status[Techreport.status$MPAID==3,c(6,38)])
colnames(Kaimana.TimeMarket.AnnexData) <- c("SettlementID","TimeMarket.base","TimeMarketErr.base","TimeMarket.2yr",
                                            "TimeMarketErr.2yr","TimeMarket.4yr","TimeMarketErr.4yr")
Kaimana.DaysUnwell.AnnexData <- cbind.data.frame(Days.unwell.Kaimana.BySett[Days.unwell.Kaimana.BySett$MonitoringYear=="Baseline",2:4],
                                                 Days.unwell.Kaimana.BySett[Days.unwell.Kaimana.BySett$MonitoringYear=="2 Year Post",3:4],
                                                 Days.unwell.Kaimana.BySett[Days.unwell.Kaimana.BySett$MonitoringYear=="4 Year Post",3:4])
colnames(Kaimana.DaysUnwell.AnnexData) <- c("SettlementID","DaysUnwell.base","DaysUnwellErr.base","DaysUnwell.2yr",
                                            "DaysUnwellErr.2yr","DaysUnwell.4yr","DaysUnwellErr.4yr")

Kaimana.AnnexContData.Techreport <- left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==3 &
                                                                    BigFive.SettleGroup$Treatment==1,
                                                                  c(1:30,32:33)],
                                              Kaimana.TimeMarket.AnnexData,
                                              by="SettlementID")
Kaimana.AnnexContData.Techreport <- left_join(Kaimana.AnnexContData.Techreport,
                                              Kaimana.DaysUnwell.AnnexData,
                                              by="SettlementID")

Kaimana.AnnexContData.Techreport <- Kaimana.AnnexContData.Techreport[rev(order(Kaimana.AnnexContData.Techreport$SettlementName)),c(31:32,1:30,33:44)]
Kaimana.AnnexContData.Techreport.PLOTFORMAT <- rbind.data.frame(Kaimana.level.ContData.annex,
                                                                null.row.ContData.annex,
                                                                Kaimana.AnnexContData.Techreport)

# write.xlsx(Kaimana.PropData.Techreport.status.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Proportional.data.statusplots.xlsx",row.names=F)
# write.xlsx(Kaimana.ContData.Techreport.status.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Continuous.data.statusplots.withpvals.xlsx",row.names=F)
# write.xlsx(Kaimana.TrendPropData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Proportional.data.trendplots.xlsx",row.names=F)
# write.xlsx(Kaimana.TrendContData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Continuous.data.trendplots.withpvals.xlsx",row.names=F)
# write.xlsx(Kaimana.AnnexPropData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Proportional.data.annexplots.xlsx",row.names=F)
# write.xlsx(Kaimana.AnnexContData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Continuous.data.annexplots.xlsx",row.names=F)
# write.xlsx(annex.sigvals.Kai,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/Pvals.continuous.data.annexplots.xlsx",row.names=F)
# write.xlsx(Kaimana.AgeGender,"Social impacts, BHS -- Kelly/R codes & data/Phil Kaimana Tech Report Data/AgeGender.xlsx",row.names=F)
