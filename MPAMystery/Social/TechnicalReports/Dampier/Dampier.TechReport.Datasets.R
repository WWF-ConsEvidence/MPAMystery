# ----
# code:  Dampier Technical Report Datasets
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
Dampier.level.PropData.status <- data.frame(c(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                                              Techreport.status.MPA[Techreport.status.MPA$MPAID==5,
                                                                    c(2,3,5:35,37:38)]))
Dampier.level.PropData.annex <- cbind.data.frame(SettlementID=0,SettlementName="MPA",
                                                 Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==5,
                                                                               c(2,3,5:35,37:38)],
                                                 Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==5,
                                                                          c(2,3,5:35,37:38)],
                                                 Techreport.status.MPA[Techreport.status.MPA$MPAID==5,
                                                                       c(2,3,5:35,37:38)])
colnames(Dampier.level.PropData.annex) <- c("SettlementID","SettlementName",
                                            paste(colnames(Dampier.level.PropData.annex)[3:37],".base",sep=""),
                                            paste(colnames(Dampier.level.PropData.annex)[38:72],".2yr",sep=""),
                                            paste(colnames(Dampier.level.PropData.annex)[73:107],".4yr",sep=""))

null.row.PropData <- matrix(rep(NA,38),ncol=38,dimnames=list(NULL,colnames(Dampier.level.PropData.status)))
null.row.PropData.annex <- matrix(rep(NA,107),ncol=107,dimnames=list(NULL,colnames(Dampier.level.PropData.annex)))

# 1.2 MPA-level Continuous data (row to be added to bottom of status and annex plots in tech report)
Dampier.level.ContData.status <- cbind.data.frame(MonitoringYear="4 Year Post",SettlementID=0,SettlementName="MPA",
                                                  BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5,c(5:6,11:12,17:18,23:24,29:30)],
                                                  Techreport.status.MPA[Techreport.status.MPA$MPAID==5,c(4,36)],
                                                  matrix(Days.unwell.Dampier.ByMPA[1,"Status"],
                                                         dimnames=list(NULL,"Days.unwell")),
                                                  matrix(Days.unwell.Dampier.ByMPA[2,"Status"],
                                                         dimnames=list(NULL,"Days.unwell.err")))
Dampier.level.ContData.annex <- cbind.data.frame(SettlementID=0,SettlementName="MPA",
                                                 BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5,c(1:30)],
                                                 Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==5,c(4,36)],
                                                 Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==5,c(4,36)],
                                                 Techreport.status.MPA[Techreport.status.MPA$MPAID==5,c(4,36)],
                                                 matrix(Days.unwell.Dampier.ByMPA[1,"Baseline"],
                                                        dimnames=list(NULL,"Days.unwell")),
                                                 matrix(Days.unwell.Dampier.ByMPA[2,"Baseline"],
                                                        dimnames=list(NULL,"Days.unwell.err")),
                                                 matrix(Days.unwell.Dampier.ByMPA[1,"2Yr"],
                                                        dimnames=list(NULL,"Days.unwell")),
                                                 matrix(Days.unwell.Dampier.ByMPA[2,"2Yr"],
                                                        dimnames=list(NULL,"Days.unwell.err")),
                                                 matrix(Days.unwell.Dampier.ByMPA[1,"Status"],
                                                        dimnames=list(NULL,"Days.unwell")),
                                                 matrix(Days.unwell.Dampier.ByMPA[2,"Status"],
                                                        dimnames=list(NULL,"Days.unwell.err")))

colnames(Dampier.level.ContData.status) <- c(colnames(Dampier.level.ContData.status[1:3]),
                                             "FSMean","FSErr","MAMean","MAErr","PAMean","PAErr","MTMean","MTErr","SEMean","SEErr",
                                             colnames(Dampier.level.ContData.status[14:17]))
colnames(Dampier.level.ContData.annex) <- c(colnames(Dampier.level.ContData.annex[1:32]),
                                            "TimeMarket.base","TimeMarketErr.base","TimeMarket.2yr","TimeMarketErr.2yr","TimeMarket.4yr",
                                            "TimeMarketErr.4yr","DaysUnwell.base","DaysUnwellErr.base","DaysUnwell.2yr","DaysUnwellErr.2yr",
                                            "DaysUnwell.4yr","DaysUnwellErr.4yr")

null.row.ContData <- matrix(rep(NA,17),ncol=17,dimnames=list(NULL,colnames(Dampier.level.ContData.status)))
null.row.ContData.annex <- matrix(rep(NA,44),ncol=44,dimnames=list(NULL,colnames(Dampier.level.ContData.annex)))

# 1.3 Subset Days Unwell variable by settlement and MPA
Days.unwell.Dampier.BySett <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                Days.unwell.baseline.BySett[Days.unwell.baseline.BySett$MPAID==5 &
                                                                                              !is.na(Days.unwell.baseline.BySett$SettlementID),c(1,3,4)]),
                                               cbind.data.frame(MonitoringYear="Baseline",
                                                                SettlementID=72,
                                                                Days.unwell=NA,
                                                                Days.unwell.err=NA),
                                               cbind.data.frame(MonitoringYear="2 Year Post",
                                                                Days.unwell.2yr.BySett[Days.unwell.2yr.BySett$MPAID==5 &
                                                                                         !is.na(Days.unwell.2yr.BySett$SettlementID),c(1,3,4)]),
                                               cbind.data.frame(MonitoringYear="4 Year Post",
                                                                Days.unwell.BySett[Days.unwell.BySett$MPAID==5 &
                                                                                     !is.na(Days.unwell.BySett$SettlementID),c(1,3,4)]))

Days.unwell.Dampier.ByMPA <- as.data.frame(matrix(c(Days.unwell.ByMPA[Days.unwell.ByMPA$MPAID==5 &
                                                                        !is.na(Days.unwell.ByMPA$MPAID),c(2,3)],
                                                    Days.unwell.baseline.ByMPA[Days.unwell.baseline.ByMPA$MPAID==5 &
                                                                                 !is.na(Days.unwell.baseline.ByMPA$MPAID),c(2,3)],
                                                    Days.unwell.2yr.ByMPA[Days.unwell.2yr.ByMPA$MPAID==5 &
                                                                            !is.na(Days.unwell.2yr.ByMPA$MPAID),c(2,3)]),
                                                  ncol=3,dimnames=list(NULL,c("Status","Baseline","2Yr"))))

# 1.4 Subset Proportional Data of Age/Gender for Dampier
Dampier.AgeGender <- data.frame(AgeCat=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                         "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"),
                                Male.Baseline=t(AgeGenderDemos.base.ByMPA[AgeGenderDemos.base.ByMPA$MPAID==5,seq(2,40,by=2)]),
                                Female.Baseline=t(AgeGenderDemos.base.ByMPA[AgeGenderDemos.base.ByMPA$MPAID==5,seq(3,41,by=2)]),
                                Male.2yr=t(AgeGenderDemos.2yr.ByMPA[AgeGenderDemos.2yr.ByMPA$MPAID==5,seq(2,40,by=2)]),
                                Female.2yr=t(AgeGenderDemos.2yr.ByMPA[AgeGenderDemos.2yr.ByMPA$MPAID==5,seq(3,41,by=2)]),
                                Male.4yr=t(AgeGenderDemos.status.ByMPA[AgeGenderDemos.status.ByMPA$MPAID==5,seq(2,40,by=2)]),
                                Female.4yr=t(AgeGenderDemos.status.ByMPA[AgeGenderDemos.status.ByMPA$MPAID==5,seq(3,41,by=2)]),
                                row.names=NULL)

# 1.5 Source Statistical Test Results from "Dampier.TechReport.SigTests.R"
source()

# ----
############################################################
#
# SECTION 2: Define Datasets for Status, Trend, and Annex Plots
#
############################################################
# ----

# 2.1 Status dataset for Dampier, proportional data
Dampier.PropData.Techreport.status <- Techreport.status[Techreport.status$MPAID==5,c(1,3,4,5,7:37,39:40)]
Dampier.PropData.Techreport.status <- Dampier.PropData.Techreport.status[rev(order(Dampier.PropData.Techreport.status$SettlementName)),]

Dampier.PropData.Techreport.status.PLOTFORMAT <- rbind.data.frame(Dampier.level.PropData.status[2:38],
                                                                  null.row.PropData[1:37],
                                                                  Dampier.PropData.Techreport.status)

# 2.2 Status dataset for Dampier, continuous data (with p values)
Dampier.ContData.Techreport.status <- left_join(BigFive.YearGroup[BigFive.YearGroup$Treatment==1 &
                                                                    BigFive.YearGroup$Year=="4 Year Post" &
                                                                    BigFive.YearGroup$MPAID==5,
                                                                  c(2:11,13)],
                                                Techreport.status[Techreport.status$MPAID==5,c(1,3,6,38)],
                                                by="SettlementID")
Dampier.ContData.Techreport.status <- left_join(Dampier.ContData.Techreport.status,
                                                Days.unwell.Dampier.BySett[Days.unwell.Dampier.BySett$MonitoringYear=="4 Year Post",
                                                                           2:4],
                                                by="SettlementID")
Dampier.ContData.Techreport.status <- cbind.data.frame(Dampier.ContData.Techreport.status[rev(order(Dampier.ContData.Techreport.status$SettlementName)),11:12],
                                                       Dampier.ContData.Techreport.status[rev(order(Dampier.ContData.Techreport.status$SettlementName)),c(1:10,13:16)])
Dampier.ContData.Techreport.status.withMPA <- rbind.data.frame(Dampier.level.ContData.status[2:17],
                                                               null.row.ContData[2:17],
                                                               Dampier.ContData.Techreport.status)
Dampier.ContData.Techreport.status.withMPA$SettlementName <- factor(Dampier.ContData.Techreport.status.withMPA$SettlementName)
Dampier.ContData.Techreport.status.PLOTFORMAT <- left_join(Dampier.ContData.Techreport.status.withMPA,
                                                           sigvals.Damp,by="SettlementName")

# 2.3 Trend dataset for Dampier, MPA-level proportional data
Dampier.TrendPropData.Techreport.PLOTFORMAT <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                                 Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==5,c(2:3,5:35,37:38)]),
                                                                cbind.data.frame(MonitoringYear="2 Year Post",
                                                                                 Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==5,c(2:3,5:35,37:38)]),
                                                                cbind.data.frame(MonitoringYear="4 Year Post",
                                                                                 Techreport.status.MPA[Techreport.status.MPA$MPAID==5,c(2:3,5:35,37:38)]))


# 2.4 Trend dataset for Dampier, MPA-level continuous data (with p values)
Dampier.TrendContData.Techreport <- rbind.data.frame(cbind.data.frame(MonitoringYear="Baseline",
                                                                      matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5,c(1:2,7:8,13:14,19:20,25:26)],
                                                                             ncol=10,dimnames=list(NULL,
                                                                                                   colnames(Dampier.ContData.Techreport.status[3:12]))),
                                                                      Techreport.trend.Baseline.MPA[Techreport.trend.Baseline.MPA$MPAID==5,c(4,36)],
                                                                      matrix(Days.unwell.Dampier.ByMPA[1,"Baseline"],dimnames=list(NULL,"Days.unwell")),
                                                                      matrix(Days.unwell.Dampier.ByMPA[2,"Baseline"],dimnames=list(NULL,"Days.unwell.err"))),
                                                     cbind.data.frame(MonitoringYear="2 Year Post",
                                                                      matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5,c(3:4,9:10,15:16,21:22,27:28)],
                                                                             ncol=10,dimnames=list(NULL,
                                                                                                   colnames(Dampier.ContData.Techreport.status[3:12]))),
                                                                      Techreport.trend.2yr.MPA[Techreport.trend.2yr.MPA$MPAID==5,c(4,36)],
                                                                      matrix(Days.unwell.Dampier.ByMPA[1,"2Yr"],dimnames=list(NULL,"Days.unwell")),
                                                                      matrix(Days.unwell.Dampier.ByMPA[2,"2Yr"],dimnames=list(NULL,"Days.unwell.err"))),
                                                     cbind.data.frame(MonitoringYear="4 Year Post",
                                                                      matrix(BigFive.MPAGroup[BigFive.MPAGroup$MPAID==5,c(5:6,11:12,17:18,23:24,29:30)],
                                                                             ncol=10,dimnames=list(NULL,
                                                                                                   colnames(Dampier.ContData.Techreport.status[3:12]))),
                                                                      Techreport.status.MPA[Techreport.status.MPA$MPAID==5,c(4,36)],
                                                                      matrix(Days.unwell.Dampier.ByMPA[1,"Status"],dimnames=list(NULL,"Days.unwell")),
                                                                      matrix(Days.unwell.Dampier.ByMPA[2,"Status"],dimnames=list(NULL,"Days.unwell.err"))))
Dampier.TrendContData.Techreport.PLOTFORMAT <- rbind.data.frame(Dampier.TrendContData.Techreport,
                                                                trend.sigvals.Damp)

# 2.5 Annex dataset for Dampier, Settlement-level proportional data
Dampier.AnnexPropData.Techreport <- cbind.data.frame(Techreport.trend.Baseline.Sett[Techreport.trend.Baseline.Sett$MPAID==5,c(1,3:5,7:37,39:40)],
                                                     Techreport.trend.2yr.Sett[Techreport.trend.2yr.Sett$MPAID==5,c(4:5,7:37,39:40)],
                                                     Techreport.status[Techreport.status$MPAID==5,c(4:5,7:37,39:40)])
colnames(Dampier.AnnexPropData.Techreport) <- c("SettlementID","SettlementName",
                                                paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".base",sep=""),
                                                paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".2yr",sep=""),
                                                paste(colnames(Techreport.status)[c(4:5,7:37,39:40)],".4yr",sep=""))
Dampier.AnnexPropData.Techreport <- Dampier.AnnexPropData.Techreport[rev(order(Dampier.AnnexPropData.Techreport$SettlementName)),]
Dampier.AnnexPropData.Techreport.PLOTFORMAT <- rbind.data.frame(Dampier.level.PropData.annex,
                                                                null.row.PropData.annex,
                                                                Dampier.AnnexPropData.Techreport)

# 2.6 Annex dataset for Dampier, Settlement-level continuous data (with p values)
Dampier.TimeMarket.AnnexData <- cbind.data.frame(Techreport.trend.Baseline.Sett[Techreport.trend.Baseline.Sett$MPAID==5,c(1,6,38)],
                                                 Techreport.trend.2yr.Sett[Techreport.trend.2yr.Sett$MPAID==5,c(6,38)],
                                                 Techreport.status[Techreport.status$MPAID==5,c(6,38)])
colnames(Dampier.TimeMarket.AnnexData) <- c("SettlementID","TimeMarket.base","TimeMarketErr.base","TimeMarket.2yr",
                                            "TimeMarketErr.2yr","TimeMarket.4yr","TimeMarketErr.4yr")
Dampier.DaysUnwell.AnnexData <- cbind.data.frame(Days.unwell.Dampier.BySett[Days.unwell.Dampier.BySett$MonitoringYear=="Baseline",2:4],
                                                 Days.unwell.Dampier.BySett[Days.unwell.Dampier.BySett$MonitoringYear=="2 Year Post",3:4],
                                                 Days.unwell.Dampier.BySett[Days.unwell.Dampier.BySett$MonitoringYear=="4 Year Post",3:4])
colnames(Dampier.DaysUnwell.AnnexData) <- c("SettlementID","DaysUnwell.base","DaysUnwellErr.base","DaysUnwell.2yr",
                                            "DaysUnwellErr.2yr","DaysUnwell.4yr","DaysUnwellErr.4yr")

Dampier.AnnexContData.Techreport <- left_join(BigFive.SettleGroup[BigFive.SettleGroup$MPAID==5 &
                                                                    BigFive.SettleGroup$Treatment==1,
                                                                  c(1:30,32:33)],
                                              Dampier.TimeMarket.AnnexData,
                                              by="SettlementID")
Dampier.AnnexContData.Techreport <- left_join(Dampier.AnnexContData.Techreport,
                                              Dampier.DaysUnwell.AnnexData,
                                              by="SettlementID")

Dampier.AnnexContData.Techreport <- Dampier.AnnexContData.Techreport[rev(order(Dampier.AnnexContData.Techreport$SettlementName)),c(31:32,1:30,33:44)]
Dampier.AnnexContData.Techreport.PLOTFORMAT <- rbind.data.frame(Dampier.level.ContData.annex,
                                                                null.row.ContData.annex,
                                                                Dampier.AnnexContData.Techreport)

# write.xlsx(Dampier.PropData.Techreport.status.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Proportional.data.statusplots.xlsx",row.names=F)
# write.xlsx(Dampier.ContData.Techreport.status.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Continuous.data.statusplots.withpvals.xlsx",row.names=F)
# write.xlsx(Dampier.TrendPropData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Proportional.data.trendplots.xlsx",row.names=F)
# write.xlsx(Dampier.TrendContData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Continuous.data.trendplots.withpvals.xlsx",row.names=F)
# write.xlsx(Dampier.AnnexPropData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Proportional.data.annexplots.xlsx",row.names=F)
# write.xlsx(Dampier.AnnexContData.Techreport.PLOTFORMAT,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Continuous.data.annexplots.xlsx",row.names=F)
# write.xlsx(annex.sigvals.Damp,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/Pvals.continuous.data.annexplots.xlsx",row.names=F)
# write.xlsx(Dampier.AgeGender,"Social impacts, BHS -- Kelly/R codes & data/Phil Dampier Tech Report Data/AgeGender.xlsx",row.names=F)
