RawHHData.Aflia.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Aflia_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Aflia.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Aflia_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Aflia.Kaimana <- left_join(RawHHDemos.Aflia.Kaimana,
                                       RawHHData.Aflia.Kaimana[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Jefri.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Jefri_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Jefri.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Jefri_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Jefri.Kaimana <- left_join(RawHHDemos.Jefri.Kaimana,
                                       RawHHData.Jefri.Kaimana[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Nima.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Nima_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Nima.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Nima_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Nima.Kaimana <- left_join(RawHHDemos.Nima.Kaimana,
                                       RawHHData.Nima.Kaimana[,c(1,25)],
                                       by="HouseholdID")
RawHHData.Wir.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Wir_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Wir.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Wir_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Wir.Kaimana <- left_join(RawHHDemos.Wir.Kaimana,
                                      RawHHData.Wir.Kaimana[,c(1,25)],
                                      by="HouseholdID")
RawHHData.Ronald.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHData_Ronald_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Ronald.Kaimana <- read.delim('Social impacts, BHS -- Kelly/R codes & data/HHDemos_Ronald_Kaimana_QAQC.txt',header=T,sep=',')
RawHHDemos.Ronald.Kaimana <- left_join(RawHHDemos.Ronald.Kaimana,
                                       RawHHData.Ronald.Kaimana[,c(1,25)],
                                       by="HouseholdID")

RawHHDemos.AllKaimana <- rbind.data.frame(cbind.data.frame(RawDataSource="Aflia",RawHHDemos.Aflia.Kaimana),
                                          cbind.data.frame(RawDataSource="Jefri",RawHHDemos.Jefri.Kaimana),
                                          cbind.data.frame(RawDataSource="Nima",RawHHDemos.Nima.Kaimana),
                                          cbind.data.frame(RawDataSource="Wir",RawHHDemos.Wir.Kaimana),
                                          cbind.data.frame(RawDataSource="Ronald",RawHHDemos.Ronald.Kaimana))
RawHHData.AllKaimana <- rbind.data.frame(cbind.data.frame(RawDataSource="Aflia",RawHHData.Aflia.Kaimana),
                                         cbind.data.frame(RawDataSource="Jefri",RawHHData.Jefri.Kaimana),
                                         cbind.data.frame(RawDataSource="Nima",RawHHData.Nima.Kaimana),
                                         cbind.data.frame(RawDataSource="Wir",RawHHData.Wir.Kaimana),
                                         cbind.data.frame(RawDataSource="Ronald",RawHHData.Ronald.Kaimana))


SpotCheckMatching.Kaimana <- t(as.data.frame(mapply(a=paste(RawHHDemos.AllKaimana$IndividualName,
                                                            RawHHDemos.AllKaimana$IndividualAge,
                                                            RawHHDemos.AllKaimana$RelationHHH),
                                                    function(a){
                                                      matches <- match(a,paste(NewDemosData.forQAQC.15.16$IndividualName,
                                                                               NewDemosData.forQAQC.15.16$IndividualAge,
                                                                               NewDemosData.forQAQC.15.16$RelationHHH))
                                                      stringmatches <- rbind.data.frame(NewDemosData.forQAQC.15.16$HouseholdID[matches],
                                                                                        RawHHDemos.AllKaimana$HouseholdID
                                                                                        [paste(RawHHDemos.AllKaimana$IndividualName,
                                                                                               RawHHDemos.AllKaimana$IndividualAge,
                                                                                               RawHHDemos.AllKaimana$RelationHHH)==a],
                                                                                        NewDemosData.forQAQC.15.16$SettlementID[matches])
                                                    })))
SpotCheckMatching.Kaimana <- as.data.frame(SpotCheckMatching.Kaimana)
colnames(SpotCheckMatching.Kaimana) <- c("HouseholdID.new","HouseholdID.raw","SettlementID")
SpotCheckMatching.Kaimana1 <- cbind.data.frame(unique(SpotCheckMatching.Kaimana$HouseholdID.new[!is.na(SpotCheckMatching.Kaimana$HouseholdID.new)]),
                                               SpotCheckMatching.Kaimana[!duplicated(SpotCheckMatching.Kaimana[c("HouseholdID.raw","SettlementID")]) &
                                                                           !is.na(SpotCheckMatching.Kaimana$SettlementID),2:3])
colnames(SpotCheckMatching.Kaimana1) <- c("HouseholdID.new","HouseholdID.raw","SettlementID")


SpotCheck.Kaimana <- t(as.data.frame(mapply(a=SpotCheckMatching.Kaimana1$HouseholdID.new,
                                            b=SpotCheckMatching.Kaimana1$HouseholdID.raw,
                                            c=SpotCheckMatching.Kaimana1$SettlementID,
                                            function(a,b,c){
                                              compare <- rbind.data.frame(NewHHData.forSpotCheck[NewHHData.forSpotCheck$HouseholdID==a,c(1,3,4,23,30:55,57:97)],
                                                                          RawHHData.AllKaimana[RawHHData.AllKaimana$HouseholdID==b &
                                                                                                 RawHHData.AllKaimana$SettlementID==c,
                                                                                               c(2,5:6,26,33:37,39:59,77:79,75:76,73,72,74,69:71,66,61:65,67:68,80:101)])
                                              mapply(d=compare[1,],
                                                     e=compare[2,],
                                                     function(d,e){
                                                       ifelse(d==e,"Good","ALERT!!")
                                                     })
                                            })))
SpotCheck.Kaimana <- cbind.data.frame(SpotCheckMatching.Kaimana1$HouseholdID.new,SpotCheckMatching.Kaimana1$HouseholdID.raw,SpotCheck.Kaimana)
colnames(SpotCheck.Kaimana) <- c("HouseholdID.new","HouseholdID.raw",colnames(SpotCheck.Kaimana)[3:71])


