RawHHData.Ibonk.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Ibonk_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Ibonk.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Ibonk_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Ibonk.KofMis <- left_join(RawHHDemos.Ibonk.KofMis,
                                       RawHHData.Ibonk.KofMis[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Ibonk.KofMis <- RawHHData.Ibonk.KofMis[-142,]
RawHHData.Other.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Other_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Other.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Other_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Other.KofMis <- left_join(RawHHDemos.Other.KofMis,
                                       RawHHData.Other.KofMis[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Other.KofMis <- RawHHData.Other.KofMis[-c(1:2),]
RawHHDemos.Other.KofMis <- RawHHDemos.Other.KofMis[-c(1:5),]
RawHHData.Riris1.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Riris1_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Riris1.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Riris1_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Riris1.KofMis <- left_join(RawHHDemos.Riris1.KofMis,
                                       RawHHData.Riris1.KofMis[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Riris2.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Riris2_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Riris2.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Riris2_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Riris2.KofMis <- left_join(RawHHDemos.Riris2.KofMis,
                                      RawHHData.Riris2.KofMis[,c(1,25)],
                                      by="HouseholdID")
RawHHData.Ronald.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Ronald_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Ronald.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Ronald_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Ronald.KofMis <- left_join(RawHHDemos.Ronald.KofMis,
                                       RawHHData.Ronald.KofMis[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Ronald.KofMis <- RawHHData.Ronald.KofMis[-c(81:136),]
RawHHDemos.Ronald.KofMis <- RawHHDemos.Ronald.KofMis[-c(513:859),]
RawHHData.Tress.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Tress_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Tress.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Tress_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Tress.KofMis <- left_join(RawHHDemos.Tress.KofMis,
                                      RawHHData.Tress.KofMis[,c(1,25)],
                                      by="HouseholdID")
RawHHData.Yudha.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Yudha_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Yudha.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Yudha_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Yudha.KofMis <- left_join(RawHHDemos.Yudha.KofMis,
                                      RawHHData.Yudha.KofMis[,c(1,25)],
                                      by="HouseholdID")
RawHHData.Jefri.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Jefri_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Jefri.KofMis <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Jefri_KofMis_QAQC.txt',header=T,sep=',')
RawHHDemos.Jefri.KofMis <- left_join(RawHHDemos.Jefri.KofMis,
                                     RawHHData.Jefri.KofMis[,c(1,25)],
                                     by="HouseholdID")
RawHHData.Jefri.KofMis <- RawHHData.Jefri.KofMis[-c(35,93,133),]
RawHHDemos.Jefri.KofMis <- RawHHDemos.Jefri.KofMis[-c(218:222),]

RawHHDemos.AllKofMis <- rbind.data.frame(cbind.data.frame(RawDataSource="Ibonk",RawHHDemos.Ibonk.KofMis),
                                          cbind.data.frame(RawDataSource="Other",RawHHDemos.Other.KofMis),
                                          cbind.data.frame(RawDataSource="Riris1",RawHHDemos.Riris1.KofMis),
                                          cbind.data.frame(RawDataSource="Riris2",RawHHDemos.Riris2.KofMis),
                                          cbind.data.frame(RawDataSource="Ronald",RawHHDemos.Ronald.KofMis),
                                         cbind.data.frame(RawDataSource="Tress",RawHHDemos.Tress.KofMis),
                                         cbind.data.frame(RawDataSource="Yudha",RawHHDemos.Yudha.KofMis),
                                         cbind.data.frame(RawDataSource="Jefri",RawHHDemos.Jefri.KofMis))
RawHHDemos.AllKofMis <- RawHHDemos.AllKofMis[!is.na(RawHHDemos.AllKofMis$IndividualAge) &
                                               !is.null(RawHHDemos.AllKofMis$IndividualName),]
RawHHData.AllKofMis <- rbind.data.frame(cbind.data.frame(RawDataSource="Ibonk",RawHHData.Ibonk.KofMis),
                                         cbind.data.frame(RawDataSource="Other",RawHHData.Other.KofMis),
                                         cbind.data.frame(RawDataSource="Riris1",RawHHData.Riris1.KofMis),
                                         cbind.data.frame(RawDataSource="Riris2",RawHHData.Riris2.KofMis),
                                         cbind.data.frame(RawDataSource="Ronald",RawHHData.Ronald.KofMis),
                                        cbind.data.frame(RawDataSource="Tress",RawHHData.Tress.KofMis),
                                        cbind.data.frame(RawDataSource="Yudha",RawHHData.Yudha.KofMis),
                                        cbind.data.frame(RawDataSource="Jefri",RawHHData.Jefri.KofMis))


SpotCheckMatching.KofMis <- t(as.data.frame(mapply(a=paste(RawHHDemos.AllKofMis$IndividualName,
                                                            RawHHDemos.AllKofMis$IndividualAge,
                                                            RawHHDemos.AllKofMis$RelationHHH),
                                                    function(a){
                                                      matches <- match(a,paste(NewDemosData.forQAQC.15.16$IndividualName,
                                                                               NewDemosData.forQAQC.15.16$IndividualAge,
                                                                               NewDemosData.forQAQC.15.16$RelationHHH))
                                                      stringmatches <- rbind.data.frame(NewDemosData.forQAQC.15.16$HouseholdID[matches],
                                                                                        RawHHDemos.AllKofMis$HouseholdID
                                                                                        [paste(RawHHDemos.AllKofMis$IndividualName,
                                                                                               RawHHDemos.AllKofMis$IndividualAge,
                                                                                               RawHHDemos.AllKofMis$RelationHHH)==a],
                                                                                        NewDemosData.forQAQC.15.16$SettlementID[matches],
                                                                                        NewDemosData.forQAQC.15.16$YearsResident[matches])
                                                    })))
SpotCheckMatching.KofMis <- as.data.frame(SpotCheckMatching.KofMis)
SpotCheckMatching.KofMis <- SpotCheckMatching.KofMis[!is.na(SpotCheckMatching.KofMis$`1`) &
                                                       !is.na(SpotCheckMatching.KofMis$`3`),]
colnames(SpotCheckMatching.KofMis) <- c("HouseholdID.new","HouseholdID.raw","SettlementID","YearsResident")
SpotCheckMatching.KofMis1 <- cbind.data.frame(unique(SpotCheckMatching.KofMis$HouseholdID.new[!is.na(SpotCheckMatching.KofMis$HouseholdID.new)]),
                                               SpotCheckMatching.KofMis[!duplicated(SpotCheckMatching.KofMis["HouseholdID.new"]) &
                                                                           !is.na(SpotCheckMatching.KofMis$SettlementID),2:3])
colnames(SpotCheckMatching.KofMis1) <- c("HouseholdID.new","HouseholdID.raw","SettlementID")

SpotCheck.KofMis <- t(as.data.frame(mapply(a=SpotCheckMatching.KofMis1$HouseholdID.new,
                                            b=SpotCheckMatching.KofMis1$HouseholdID.raw,
                                            c=SpotCheckMatching.KofMis1$SettlementID,
                                            function(a,b,c){
                                              compare <- rbind.data.frame(NewHHData.forSpotCheck[NewHHData.forSpotCheck$HouseholdID==a,c(1,3,4,23,30:55,57:97)],
                                                                          RawHHData.AllKofMis[RawHHData.AllKofMis$HouseholdID==b &
                                                                                                 RawHHData.AllKofMis$SettlementID==c,
                                                                                               c(2,5:6,26,33:37,39:59,77:79,75:76,73,72,74,69:71,66,61:65,67:68,80:101)])
                                              mapply(d=compare[1,],
                                                     e=compare[2,],
                                                     function(d,e){
                                                       ifelse(d==e,"Good","ALERT!!")
                                                     })
                                            })))
SpotCheck.KofMis <- cbind.data.frame(SpotCheckMatching.KofMis1$HouseholdID.new,SpotCheckMatching.KofMis1$HouseholdID.raw,SpotCheck.KofMis)
colnames(SpotCheck.KofMis) <- c("HouseholdID.new","HouseholdID.raw",colnames(SpotCheck.KofMis)[3:71])

