library(plyr)
library(dplyr)
library(xlsx)

BHS.MPAMysteryDB <- odbcConnect("BHS.MPAMysteryDB")

# Source data for QAQC test comparing old database to newly recombined database
NewHHData.forQAQC <- read.delim('Social impacts, BHS -- Kelly/R codes & data/ExportForQAQC.txt',header=T,sep=',')
OldHHData.forQAQC <- read.delim('Social impacts, BHS -- Kelly/R codes & data/OldData_ForExport.txt',header=T,sep=',')

NewHHData.forSpotCheck <- read.delim('Social impacts, BHS -- Kelly/R codes & data/ExportForQAQC_Spotcheck.txt',header=T,sep=',')
NewDemosData.forQAQC <- read.delim('Social impacts, BHS -- Kelly/R codes & data/ExportForQAQC_Demos.txt',header=T,sep=',')
NewDemosData.forQAQC <- left_join(NewDemosData.forQAQC,
                                  NewHHData.forSpotCheck[,c(1,4,23,30)],
                                  by="HouseholdID")
NewDemosData.forQAQC <- NewDemosData.forQAQC[!is.na(NewDemosData.forQAQC$IndividualAge) &
                                               !is.null(NewDemosData.forQAQC$IndividualName),]

NewDemosData.forQAQC.15.16 <- NewDemosData.forQAQC[NewDemosData.forQAQC$InterviewYear>2014,]

RawHHData.Dahlia.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Dahlia_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Dahlia.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Dahlia_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Dahlia.Dampier <- left_join(RawHHDemos.Dahlia.Dampier,
                                       RawHHData.Dahlia.Dampier[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Aflia1.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Aflia1_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Aflia1.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Aflia1_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Aflia1.Dampier <- left_join(RawHHDemos.Aflia1.Dampier,
                                       RawHHData.Aflia1.Dampier[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Aflia2.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Aflia2_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Aflia2.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Aflia2_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Aflia2.Dampier <- left_join(RawHHDemos.Aflia2.Dampier,
                                       RawHHData.Aflia2.Dampier[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Rizal.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Rizal_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Rizal.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Rizal_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Rizal.Dampier <- left_join(RawHHDemos.Rizal.Dampier,
                                       RawHHData.Rizal.Dampier[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Ronald.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Ronald_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Ronald.Dampier <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Ronald_Dampier_QAQC.txt',header=T,sep=',')
RawHHDemos.Ronald.Dampier <- left_join(RawHHDemos.Ronald.Dampier,
                                       RawHHData.Ronald.Dampier[,c(1,25)],
                                       by="HouseholdID")

RawHHDemos.AllDampier <- rbind.data.frame(cbind.data.frame(RawDataSource="Dahlia",RawHHDemos.Dahlia.Dampier),
                                          cbind.data.frame(RawDataSource="Aflia1",RawHHDemos.Aflia1.Dampier),
                                          cbind.data.frame(RawDataSource="Aflia2",RawHHDemos.Aflia2.Dampier),
                                          cbind.data.frame(RawDataSource="Rizal",RawHHDemos.Rizal.Dampier),
                                          cbind.data.frame(RawDataSource="Ronald",RawHHDemos.Ronald.Dampier))
RawHHData.AllDampier <- rbind.data.frame(cbind.data.frame(RawDataSource="Dahlia",RawHHData.Dahlia.Dampier),
                                         cbind.data.frame(RawDataSource="Aflia1",RawHHData.Aflia1.Dampier),
                                         cbind.data.frame(RawDataSource="Aflia2",RawHHData.Aflia2.Dampier),
                                         cbind.data.frame(RawDataSource="Rizal",RawHHData.Rizal.Dampier),
                                         cbind.data.frame(RawDataSource="Ronald",RawHHData.Ronald.Dampier))


SpotCheckMatching.Dampier <- t(as.data.frame(mapply(a=paste(RawHHDemos.AllDampier$IndividualName,
                                                            RawHHDemos.AllDampier$IndividualAge,
                                                            RawHHDemos.AllDampier$RelationHHH),
                                                    function(a){
                                                      matches <- match(a,paste(NewDemosData.forQAQC.15.16$IndividualName,
                                                                               NewDemosData.forQAQC.15.16$IndividualAge,
                                                                               NewDemosData.forQAQC.15.16$RelationHHH))
                                                      stringmatches <- rbind.data.frame(NewDemosData.forQAQC.15.16$HouseholdID[matches],
                                                                                        RawHHDemos.AllDampier$HouseholdID
                                                                                        [paste(RawHHDemos.AllDampier$IndividualName,
                                                                                               RawHHDemos.AllDampier$IndividualAge,
                                                                                               RawHHDemos.AllDampier$RelationHHH)==a],
                                                                                        NewDemosData.forQAQC.15.16$SettlementID[matches])
                                                    })))
SpotCheckMatching.Dampier <- as.data.frame(SpotCheckMatching.Dampier)
colnames(SpotCheckMatching.Dampier) <- c("HouseholdID.new","HouseholdID.raw","SettlementID")
SpotCheckMatching.Dampier1 <- cbind.data.frame(unique(SpotCheckMatching.Dampier$HouseholdID.new[!is.na(SpotCheckMatching.Dampier$HouseholdID.new)]),
                                        SpotCheckMatching.Dampier[!duplicated(SpotCheckMatching.Dampier[c("HouseholdID.raw","SettlementID")]) &
                                                            !is.na(SpotCheckMatching.Dampier$SettlementID),2:3])
colnames(SpotCheckMatching.Dampier1) <- c("HouseholdID.new","HouseholdID.raw","SettlementID")


SpotCheck.Dampier <- t(as.data.frame(mapply(a=SpotCheckMatching.Dampier1$HouseholdID.new,
                                  b=SpotCheckMatching.Dampier1$HouseholdID.raw,
                                  c=SpotCheckMatching.Dampier1$SettlementID,
                                  function(a,b,c){
                                    compare <- rbind.data.frame(NewHHData.forSpotCheck[NewHHData.forSpotCheck$HouseholdID==a,c(1,3,4,23,30:55,57:97)],
                                                                RawHHData.AllDampier[RawHHData.AllDampier$HouseholdID==b &
                                                                                       RawHHData.AllDampier$SettlementID==c,
                                                                                     c(2,5:6,26,33:37,39:59,77:79,75:76,73,72,74,69:71,66,61:65,67:68,80:101)])
                                    mapply(d=compare[1,],
                                           e=compare[2,],
                                           function(d,e){
                                             ifelse(d==e,"Good","ALERT!!")
                                           })
                                  })))
SpotCheck.Dampier <- cbind.data.frame(SpotCheckMatching.Dampier1$HouseholdID.new,SpotCheckMatching.Dampier1$HouseholdID.raw,SpotCheck.Dampier)
colnames(SpotCheck.Dampier) <- c("HouseholdID.new","HouseholdID.raw",colnames(SpotCheck.Dampier)[3:71])

# Checking # cross-sections per sett and # HH per sett, by cross-section.
NewHHData.forQAQC$MonitoringYear <- ifelse((NewHHData.forQAQC$InterviewYear==2010 & (NewHHData.forQAQC$MPAID==1 | NewHHData.forQAQC$MPAID==2)) |
                                   (NewHHData.forQAQC$InterviewYear==2011 & (NewHHData.forQAQC$MPAID==4 | NewHHData.forQAQC$MPAID==6)) |
                                   (NewHHData.forQAQC$InterviewYear==2012 & (NewHHData.forQAQC$MPAID==3 | NewHHData.forQAQC$MPAID==5)),"Baseline",
                                 ifelse((NewHHData.forQAQC$InterviewYear==2012 & (NewHHData.forQAQC$MPAID==1 | NewHHData.forQAQC$MPAID==2)) |
                                          (NewHHData.forQAQC$InterviewYear==2013 & (NewHHData.forQAQC$MPAID==4 | NewHHData.forQAQC$MPAID==6)) |
                                          (NewHHData.forQAQC$InterviewYear==2014 & (NewHHData.forQAQC$MPAID==3 | NewHHData.forQAQC$MPAID==5)),"2 Year Post",
                                        ifelse((NewHHData.forQAQC$InterviewYear==2014 & (NewHHData.forQAQC$MPAID==1 | NewHHData.forQAQC$MPAID==2)) |
                                                 (NewHHData.forQAQC$InterviewYear==2015 & (NewHHData.forQAQC$MPAID==4 | NewHHData.forQAQC$MPAID==6)) |
                                                 (NewHHData.forQAQC$InterviewYear==2016 & (NewHHData.forQAQC$MPAID==3 | NewHHData.forQAQC$MPAID==5)),"4 Year Post","NA")))
OldHHData.forQAQC$MonitoringYear <- ifelse((OldHHData.forQAQC$InterviewYear==2010 & (OldHHData.forQAQC$MPAID==1 | OldHHData.forQAQC$MPAID==2)) |
                                             (OldHHData.forQAQC$InterviewYear==2011 & (OldHHData.forQAQC$MPAID==4 | OldHHData.forQAQC$MPAID==6)) |
                                             (OldHHData.forQAQC$InterviewYear==2012 & (OldHHData.forQAQC$MPAID==3 | OldHHData.forQAQC$MPAID==5)),"Baseline",
                                           ifelse((OldHHData.forQAQC$InterviewYear==2012 & (OldHHData.forQAQC$MPAID==1 | OldHHData.forQAQC$MPAID==2)) |
                                                    (OldHHData.forQAQC$InterviewYear==2013 & (OldHHData.forQAQC$MPAID==4 | OldHHData.forQAQC$MPAID==6)) |
                                                    (OldHHData.forQAQC$InterviewYear==2014 & (OldHHData.forQAQC$MPAID==3 | OldHHData.forQAQC$MPAID==5)),"2 Year Post",
                                                  ifelse((OldHHData.forQAQC$InterviewYear==2014 & (OldHHData.forQAQC$MPAID==1 | OldHHData.forQAQC$MPAID==2)) |
                                                           (OldHHData.forQAQC$InterviewYear==2015 & (OldHHData.forQAQC$MPAID==4 | OldHHData.forQAQC$MPAID==6)) |
                                                           (OldHHData.forQAQC$InterviewYear==2016 & (OldHHData.forQAQC$MPAID==3 | OldHHData.forQAQC$MPAID==5)),"4 Year Post","NA")))

Setts.HH.NewDataTest <- group_by(NewHHData.forQAQC,SettlementID)
Setts.HH.NewDataTest <- summarise(Setts.HH.NewDataTest,
                           MPAID=unique(MPAID),
                           NumberCrossSections.new=length(unique(InterviewYear)),
                           NumberHH.base.new=length(HouseholdID[MonitoringYear=="Baseline"]),
                           NumberHH.2yr.new=length(HouseholdID[MonitoringYear=="2 Year Post"]),
                           NumberHH.4yr.new=length(HouseholdID[MonitoringYear=="4 Year Post"]))

Setts.HH.OldDataTest <- group_by(OldHHData.forQAQC,SettlementID)
Setts.HH.OldDataTest <- summarise(Setts.HH.OldDataTest,
                                  NumberCrossSections.old=length(unique(InterviewYear)),
                                  NumberHH.base.old=length(HouseholdID[MonitoringYear=="Baseline"]),
                                  NumberHH.2yr.old=length(HouseholdID[MonitoringYear=="2 Year Post"]),
                                  NumberHH.4yr.old=length(HouseholdID[MonitoringYear=="4 Year Post"]))

NewHHData.forQAQC$HouseholdID[is.na(NewHHData.forQAQC$MPAID)]

Setts.HH.Test <- left_join(Setts.HH.NewDataTest,
                           Setts.HH.OldDataTest,
                           by="SettlementID")
Setts.HH.Test <- data.frame(Setts.HH.Test[,c(2,1,7,3,8,4)],
                            BaselineHHComparison=ifelse(Setts.HH.Test$NumberHH.base.new!=Setts.HH.Test$NumberHH.base.old,"ALERT!!","Good"),
                            Setts.HH.Test[,c(9,5)],
                            TwoYearHHComparison=ifelse(Setts.HH.Test$NumberHH.2yr.new!=Setts.HH.Test$NumberHH.2yr.old,"ALERT!!","Good"),
                            Setts.HH.Test[,c(10,6)],
                            FourYearHHComparison=ifelse(Setts.HH.Test$NumberHH.4yr.new!=Setts.HH.Test$NumberHH.4yr.old,"ALERT!!","Good"))