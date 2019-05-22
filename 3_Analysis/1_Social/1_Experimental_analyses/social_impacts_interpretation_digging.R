# code: Additional analyses for interpretation of social impacts data, BHS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2018
# modified: 
# 


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Call Libraries, Source Scripts ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Call libraries ----
pacman::p_load()

# Call in script to get BigFive data frame
# Call in script to calculate HH ATT (using Function_outcome_ATT_method1.R)

#-- Master pairs lists, t2 and t4
master.t2.A <- read.xlsx('2_Social/FlatDataFiles/BHS/t2_impacts/master_t2_panelA.xlsx', sheetName = 'Sheet 1')
master.t4.A <- read.xlsx('2_Social/FlatDataFiles/BHS/t4_impacts/master_t4_panelA.xlsx', sheetName = 'Sheet 1')

#-- BigFive, t2
hfs.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = BigFive, var = FSIndex)
asset.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = BigFive, var = MAIndex)
tenure.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = BigFive, var = MTIndex)
attach.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = BigFive, var = PAIndex)
enrol.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = BigFive, var = SERate)

#-- BigFive, t4
hfs.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = BigFive, var = FSIndex)
asset.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = BigFive, var = MAIndex)
tenure.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = BigFive, var = MTIndex)
attach.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = BigFive, var = PAIndex)
enrol.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = BigFive, var = SERate)

# Call in script to aggregate ATT to MPA level

plot.theme.extraplots <- theme(axis.ticks=element_blank(),
                               panel.background=element_rect(fill="white",
                                                             colour="#909090"),
                               panel.border=element_rect(fill=NA,
                                                         size=0.25,
                                                         colour="#C0C0C0"),
                               panel.grid.major=element_line(colour="#C0C0C0",
                                                               size=0.25,
                                                               linetype=3),
                               plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                               plot.title=element_text(size=rel(1.1),
                                                       face="bold",
                                                       colour="#303030",
                                                       hjust=0.5),
                               axis.title=element_text(size=rel(0.9),
                                                       angle=0,
                                                       face="bold",
                                                       colour="#303030"),
                               axis.text=element_text(size=rel(0.9),
                                                      angle=0,
                                                      colour="#303030"),
                               legend.position="top",
                               legend.justification="right",
                               legend.box.spacing=unit(0.1,"cm"))


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Wealth ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 How does baseline level of household assets affect impact of Big Five? ----

# At HH level (baseline assets pulled from tr1t0 household for each matched pair)
# -- asset 
t0.wealth.assetATT.byHH <-
  asset.outcome.t4[,c("tr1tx","tr0tx","tr1t0","tr0t0","ATT")] %>%
  left_join(BigFive[,c("HouseholdID","MPAID","MAIndex")],by=c("tr1tx"="HouseholdID")) %>%
  transmute(HouseholdID=tr1tx,
            MPAID=MPAID,
            asset.t0=MAIndex,
            ATT=ATT)

ggplot(t0.wealth.assetATT.byHH,aes(x=ATT,y=asset.t0)) +
  geom_point(colour="blue") +
  plot.theme.extraplots + labs(title="Baseline Wealth vs. Material Assets ATT")

# -- hfs
t0.wealth.hfsATT.byHH <-
  hfs.outcome.t4[,c("tr1tx","tr0tx","tr1t0","tr0t0","ATT")] %>%
  left_join(BigFive[,c("HouseholdID","MPAID","MAIndex")],by=c("tr1tx"="HouseholdID")) %>%
  transmute(HouseholdID=tr1tx,
            MPAID=MPAID,
            asset.t0=MAIndex,
            ATT=ATT)

ggplot(t0.wealth.hfsATT.byHH,aes(x=ATT,y=asset.t0)) +
  geom_point(colour="blue") +
  plot.theme.extraplots +labs(title="Baseline Wealth vs. Food Security ATT")

# -- tenure
t0.wealth.tenureATT.byHH <-
  tenure.outcome.t4[,c("tr1tx","tr0tx","tr1t0","tr0t0","ATT")] %>%
  left_join(BigFive[,c("HouseholdID","MPAID","MAIndex")],by=c("tr1tx"="HouseholdID")) %>%
  transmute(HouseholdID=tr1tx,
            MPAID=MPAID,
            asset.t0=MAIndex,
            ATT=ATT)

ggplot(t0.wealth.tenureATT.byHH,aes(x=ATT,y=asset.t0)) +
  geom_point(colour="blue") +
  plot.theme.extraplots +labs(title="Baseline Wealth vs. Marine Tenure ATT")

# -- enrollment
t0.wealth.enrolATT.byHH <-
  enrol.outcome.t4[,c("tr1tx","tr0tx","tr1t0","tr0t0","ATT")] %>%
  left_join(BigFive[,c("HouseholdID","MPAID","MAIndex")],by=c("tr1tx"="HouseholdID")) %>%
  transmute(HouseholdID=tr1tx,
            MPAID=MPAID,
            asset.t0=MAIndex,
            ATT=ATT)

ggplot(t0.wealth.enrolATT.byHH,aes(x=ATT,y=asset.t0)) +
  geom_point(colour="blue") +
  plot.theme.extraplots +labs(title="Baseline Wealth vs. School Enrollment ATT")

# -- attachment
t0.wealth.attachATT.byHH <-
  attach.outcome.t4[,c("tr1tx","tr0tx","tr1t0","tr0t0","ATT")] %>%
  left_join(BigFive[,c("HouseholdID","MPAID","MAIndex")],by=c("tr1tx"="HouseholdID")) %>%
  transmute(HouseholdID=tr1tx,
            MPAID=MPAID,
            asset.t0=MAIndex,
            ATT=ATT)

ggplot(t0.wealth.attachATT.byHH,aes(x=ATT,y=asset.t0)) +
  geom_point(colour="blue") +
  plot.theme.extraplots +labs(title="Baseline Wealth vs. Place Attachment ATT")



# At MPA level
# -- asset
t0.wealth.assetATT.byMPA <- 
  asset.att.byMPA[asset.att.byMPA$year=="t4",] %>%
  transmute(MPAID=mpa,
            ATT=est,
            p.value=p.val) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",
                                    c("MPAID","MAMean")],
                   by="MPAID")

ggplot(t0.wealth.assetATT.byMPA,aes(x=ATT,y=MAMean)) +
  geom_point(colour="blue") +
  plot.theme.extraplots + labs(title="Baseline average wealth vs. material asset ATT/nby MPA")

# -- hfs 
t0.wealth.hfsATT.byMPA <-
  hfs.att.byMPA[hfs.att.byMPA$year=="t4",] %>%
  transmute(MPAID=mpa,
            ATT=est,
            p.value=p.val) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",
                             c("MPAID","MAMean")],
            by="MPAID")

ggplot(t0.wealth.hfsATT.byMPA,aes(x=ATT,y=MAMean)) +
  geom_point(colour="blue") +
  plot.theme.extraplots + labs(title="Baseline average wealth vs. food security ATT/nby MPA")

# -- tenure
t0.wealth.tenureATT.byMPA <-
  tenure.att.byMPA[tenure.att.byMPA$year=="t4",] %>%
  transmute(MPAID=mpa,
            ATT=est,
            p.value=p.val) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",
                             c("MPAID","MAMean")],
            by="MPAID")

ggplot(t0.wealth.tenureATT.byMPA,aes(x=ATT,y=MAMean)) +
  geom_point(colour="blue") +
  plot.theme.extraplots + labs(title="Baseline average wealth vs. marine tenure ATT/nby MPA")

# -- enrollment
t0.wealth.enrolATT.byMPA <-
  enrol.att.byMPA[enrol.att.byMPA$year=="t4",] %>%
  transmute(MPAID=mpa,
            ATT=est,
            p.value=p.val) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",
                             c("MPAID","MAMean")],
            by="MPAID")

ggplot(t0.wealth.enrolATT.byMPA,aes(x=ATT,y=MAMean)) +
  geom_point(colour="blue") +
  plot.theme.extraplots + labs(title="Baseline average wealth vs. school enrollment ATT/nby MPA")

# -- attachment
t0.wealth.attachATT.byMPA <-
  attach.att.byMPA[attach.att.byMPA$year=="t4",] %>%
  transmute(MPAID=mpa,
            ATT=est,
            p.value=p.val) %>%
  left_join(BigFive.MPAGroup[BigFive.MPAGroup$MonitoringYear=="Baseline",
                             c("MPAID","MAMean")],
            by="MPAID")

ggplot(t0.wealth.attachATT.byMPA,aes(x=ATT,y=MAMean)) +
  geom_point(colour="blue") +
  plot.theme.extraplots + labs(title="Baseline average wealth vs. place attachment ATT/nby MPA")


# ---- 2.2 At baseline, what percentage of households had different asset goods (e.g., a car, a radio)? ----
PercentHH.assets.bytype.byMPA <-
  HHData %>%
  transmute(HouseholdID=HouseholdID,
            MPAID=as.numeric(MPAID),
            SettlementID=SettlementID,
            MonitoringYear=MonitoringYear,
            CarTruck=CarTruck/11,
            Bicycle=Bicycle/9,
            Motorcycle=Motorcycle/10,
            BoatNoMotor=BoatNoMotor/6,
            BoatOutboard=BoatOutboard/7,
            BoatInboard=BoatInboard/8,
            PhoneCombined=PhoneCombined/4,
            TV=TV/2,
            Entertain=Entertain,
            Satellite=Satellite/3,
            Generator=Generator/5) %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(NoCarTruck=length(HouseholdID[CarTruck==0 & !is.na(CarTruck)])/length(HouseholdID[!is.na(CarTruck)])*100,
            OneCarTruck=length(HouseholdID[CarTruck==1 & !is.na(CarTruck)])/length(HouseholdID[!is.na(CarTruck)])*100,
            MoreOneCarTruck=length(HouseholdID[CarTruck>1 & !is.na(CarTruck)])/length(HouseholdID[!is.na(CarTruck)])*100,
            NoMotorcycle=length(HouseholdID[Motorcycle==0 & !is.na(Motorcycle)])/length(HouseholdID[!is.na(Motorcycle)])*100,
            OneMotorcycle=length(HouseholdID[Motorcycle==1 & !is.na(Motorcycle)])/length(HouseholdID[!is.na(Motorcycle)])*100,
            MoreOneMotorcycle=length(HouseholdID[Motorcycle>1 & !is.na(Motorcycle)])/length(HouseholdID[!is.na(Motorcycle)])*100,
            NoBoatNoMotor=length(HouseholdID[BoatNoMotor==0 & !is.na(BoatNoMotor)])/length(HouseholdID[!is.na(BoatNoMotor)])*100,
            OneBoatNoMotor=length(HouseholdID[BoatNoMotor==1 & !is.na(BoatNoMotor)])/length(HouseholdID[!is.na(BoatNoMotor)])*100,
            MoreOneBoatNoMotor=length(HouseholdID[BoatNoMotor>1 & !is.na(BoatNoMotor)])/length(HouseholdID[!is.na(BoatNoMotor)])*100,
            NoBoatOutboard=length(HouseholdID[BoatOutboard==0 & !is.na(BoatOutboard)])/length(HouseholdID[!is.na(BoatOutboard)])*100,
            OneBoatOutboard=length(HouseholdID[BoatOutboard==1 & !is.na(BoatOutboard)])/length(HouseholdID[!is.na(BoatOutboard)])*100,
            MoreOneBoatOutboard=length(HouseholdID[BoatOutboard>1 & !is.na(BoatOutboard)])/length(HouseholdID[!is.na(BoatOutboard)])*100,
            NoBoatInboard=length(HouseholdID[BoatInboard==0 & !is.na(BoatInboard)])/length(HouseholdID[!is.na(BoatInboard)])*100,
            OneBoatInboard=length(HouseholdID[BoatInboard==1 & !is.na(BoatInboard)])/length(HouseholdID[!is.na(BoatInboard)])*100,
            MoreOneBoatInboard=length(HouseholdID[BoatInboard>1 & !is.na(BoatInboard)])/length(HouseholdID[!is.na(BoatInboard)])*100,
            NoGenerator=length(HouseholdID[Generator==0 & !is.na(Generator)])/length(HouseholdID[!is.na(Generator)])*100,
            OneGenerator=length(HouseholdID[Generator==1 & !is.na(Generator)])/length(HouseholdID[!is.na(Generator)])*100,
            MoreOneGenerator=length(HouseholdID[Generator>1 & !is.na(Generator)])/length(HouseholdID[!is.na(Generator)])*100,
            NoPhone=length(HouseholdID[PhoneCombined==0 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            OnePhone=length(HouseholdID[PhoneCombined==1 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            TwoPhone=length(HouseholdID[PhoneCombined==2 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            MoreTwoPhone=length(HouseholdID[PhoneCombined>2 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            NoSatellite=length(HouseholdID[Satellite==0 & !is.na(Satellite)])/length(HouseholdID[!is.na(Satellite)])*100,
            OneSatellite=length(HouseholdID[Satellite==1 & !is.na(Satellite)])/length(HouseholdID[!is.na(Satellite)])*100,
            MoreOneSatellite=length(HouseholdID[Satellite>1 & !is.na(Satellite)])/length(HouseholdID[!is.na(Satellite)])*100)


PercentHH.assets.bytype <-
  HHData %>%
  transmute(HouseholdID=HouseholdID,
            MPAID=as.factor(MPAID),
            SettlementID=SettlementID,
            MonitoringYear=MonitoringYear,
            CarTruck=CarTruck/11,
            Bicycle=Bicycle/9,
            Motorcycle=Motorcycle/10,
            BoatNoMotor=BoatNoMotor/6,
            BoatOutboard=BoatOutboard/7,
            BoatInboard=BoatInboard/8,
            PhoneCombined=PhoneCombined/4,
            TV=TV/2,
            Entertain=Entertain,
            Satellite=Satellite/3,
            Generator=Generator/5) %>%
  group_by(MonitoringYear) %>%
  summarise(NoCarTruck=length(HouseholdID[CarTruck==0 & !is.na(CarTruck)])/length(HouseholdID[!is.na(CarTruck)])*100,
            OneCarTruck=length(HouseholdID[CarTruck==1 & !is.na(CarTruck)])/length(HouseholdID[!is.na(CarTruck)])*100,
            MoreOneCarTruck=length(HouseholdID[CarTruck>1 & !is.na(CarTruck)])/length(HouseholdID[!is.na(CarTruck)])*100,
            NoMotorcycle=length(HouseholdID[Motorcycle==0 & !is.na(Motorcycle)])/length(HouseholdID[!is.na(Motorcycle)])*100,
            OneMotorcycle=length(HouseholdID[Motorcycle==1 & !is.na(Motorcycle)])/length(HouseholdID[!is.na(Motorcycle)])*100,
            MoreOneMotorcycle=length(HouseholdID[Motorcycle>1 & !is.na(Motorcycle)])/length(HouseholdID[!is.na(Motorcycle)])*100,
            NoBoatNoMotor=length(HouseholdID[BoatNoMotor==0 & !is.na(BoatNoMotor)])/length(HouseholdID[!is.na(BoatNoMotor)])*100,
            OneBoatNoMotor=length(HouseholdID[BoatNoMotor==1 & !is.na(BoatNoMotor)])/length(HouseholdID[!is.na(BoatNoMotor)])*100,
            MoreOneBoatNoMotor=length(HouseholdID[BoatNoMotor>1 & !is.na(BoatNoMotor)])/length(HouseholdID[!is.na(BoatNoMotor)])*100,
            NoBoatOutboard=length(HouseholdID[BoatOutboard==0 & !is.na(BoatOutboard)])/length(HouseholdID[!is.na(BoatOutboard)])*100,
            OneBoatOutboard=length(HouseholdID[BoatOutboard==1 & !is.na(BoatOutboard)])/length(HouseholdID[!is.na(BoatOutboard)])*100,
            MoreOneBoatOutboard=length(HouseholdID[BoatOutboard>1 & !is.na(BoatOutboard)])/length(HouseholdID[!is.na(BoatOutboard)])*100,
            NoBoatInboard=length(HouseholdID[BoatInboard==0 & !is.na(BoatInboard)])/length(HouseholdID[!is.na(BoatInboard)])*100,
            OneBoatInboard=length(HouseholdID[BoatInboard==1 & !is.na(BoatInboard)])/length(HouseholdID[!is.na(BoatInboard)])*100,
            MoreOneBoatInboard=length(HouseholdID[BoatInboard>1 & !is.na(BoatInboard)])/length(HouseholdID[!is.na(BoatInboard)])*100,
            NoGenerator=length(HouseholdID[Generator==0 & !is.na(Generator)])/length(HouseholdID[!is.na(Generator)])*100,
            OneGenerator=length(HouseholdID[Generator==1 & !is.na(Generator)])/length(HouseholdID[!is.na(Generator)])*100,
            MoreOneGenerator=length(HouseholdID[Generator>1 & !is.na(Generator)])/length(HouseholdID[!is.na(Generator)])*100,
            NoPhone=length(HouseholdID[PhoneCombined==0 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            OnePhone=length(HouseholdID[PhoneCombined==1 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            TwoPhone=length(HouseholdID[PhoneCombined==2 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            MoreTwoPhone=length(HouseholdID[PhoneCombined>2 & !is.na(PhoneCombined)])/length(HouseholdID[!is.na(PhoneCombined)])*100,
            NoSatellite=length(HouseholdID[Satellite==0 & !is.na(Satellite)])/length(HouseholdID[!is.na(Satellite)])*100,
            OneSatellite=length(HouseholdID[Satellite==1 & !is.na(Satellite)])/length(HouseholdID[!is.na(Satellite)])*100,
            MoreOneSatellite=length(HouseholdID[Satellite>1 & !is.na(Satellite)])/length(HouseholdID[!is.na(Satellite)])*100)

change.assets.plots <-
  ggplot(PercentHH.assets.bytype.byMPA[PercentHH.assets.bytype.byMPA$MPAID<7,],
         aes(x=MPAID,group=MonitoringYear,y=NoPhone/100)) +
  geom_bar(aes(fill=MonitoringYear),
           stat="identity",
           position="dodge") +
  scale_fill_manual(name="",
                    values=c(alpha("#2C7FB8",0.4),alpha("#2C7FB8",0.7),alpha("#2C7FB8",1))) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     labels=scales::percent_format()) +
  plot.theme.extraplots + labs(x="MPA",y="Percent Households",title="No Phone")

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Education ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Length of residency, sign of immigration of a more highly educated middle class? ----

residency.byMPA <-
  left_join(HHData,HeadOfHH,by="HouseholdID") %>%
  mutate(MPAID=as.factor(MPAID)) %>%
  group_by(MPAID, MonitoringYear) %>%
  summarise(YrResident=mean(YrResident,na.rm=T),
            YrResident.sd=sd(YrResident,na.rm=T),
            Percent.majoritylife=(length(HouseholdID[YrResident/IndividualAge>=0.5 & 
                                                      !is.na(YrResident) & 
                                                      !is.na(IndividualAge)])/length(HouseholdID[!is.na(YrResident) &
                                                                                                        !is.na(IndividualAge)]))*100,
            Percent.movedlast2yr=(length(HouseholdID[YrResident<=2 & !is.na(YrResident)])/length(HouseholdID[!is.na(YrResident)]))*100)

ggplot(residency.byMPA,aes(x=MPAID,y=YrResident)) +
  geom_bar(aes(group=MonitoringYear,fill=MonitoringYear),
            stat="identity",
           position="dodge") +
  geom_errorbar(aes(ymin=YrResident-YrResident.sd,ymax=YrResident+YrResident.sd,
                    group=MonitoringYear,colour=MonitoringYear),
                width=0.2,
                position=position_dodge(0.9),
                show.legend=F) +
  scale_fill_manual(name="",
                    labels=c("Baseline","2 Year","4 Year"),
                    values=c(alpha("#0B4D8B",0.3),alpha("#0B4D8B",0.6),alpha("#0B4D8B",0.9))) +
  scale_colour_manual(values=c(alpha("#07325A",0.4),alpha("#07325A",0.7),alpha("#07325A",1))) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0)) +
  plot.theme.extraplots + labs(x="MPA",y="Avg Years Resident,\nHead of Household\n")


ggplot(residency.byMPA,aes(x=MPAID,y=Percent.movedlast2yr/100)) +
  geom_bar(aes(group=MonitoringYear,fill=MonitoringYear),
           stat="identity",
           position="dodge") +
  scale_fill_manual(name="",
                    labels=c("Baseline","2 Year","4 Year"),
                    values=c(alpha("#0B4D8B",0.3),alpha("#0B4D8B",0.6),alpha("#0B4D8B",0.9))) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,0.1),
                     labels=scales::percent_format()) +
  plot.theme.extraplots + labs(x="MPA",y="Percent of household heads",
                               title="Percent of household heads\nresident in settlement 2 years or less")

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Customary Rights ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Create data frame subsetting to only those who are lifelong residents ----

lifelong.residents <-
  left_join(HHData[,c("HouseholdID","MPAID","SettlementID","MonitoringYear","YrResident")],
            HeadOfHH,by="HouseholdID") %>%
  subset(.,YrResident==IndividualAge)

adat.holders <-
  left_join(lifelong.residents,HHData[,c("HouseholdID","RightsManage",
                                         "RightsExclude","RightsTransfer")],
            by="HouseholdID") %>%
  subset(.,RightsManage==1 &
           RightsExclude==1 &
           RightsTransfer==1) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(Num.Adat=length(HouseholdID))

# ---- 4.2 Customary rights data set ----

customary.rights.groups <-
  left_join(HHData[,c("HouseholdID","MPAID","SettlementID","MonitoringYear","YrResident")],
          HeadOfHH,by="HouseholdID") %>%
  left_join(HHData[,c("HouseholdID","RightsManage","RightsExclude","RightsTransfer")]) %>%
  left_join(asset) %>%
  mutate(group=ifelse(YrResident==IndividualAge & 
                       RightsManage==1 &
                       RightsExclude==1 &
                       RightsTransfer==1,"adat",
                      ifelse(YrResident==IndividualAge & 
                               (RightsManage!=1 | 
                                  RightsExclude!=1 |
                                  RightsTransfer!=1),"long-term, non-adat",
                             "other")))


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: Gender HHH ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

BHS.demographics.byMPA.byGender <-
  left_join(HeadOfHH,BigFive,by="HouseholdID") %>%
  left_join(HHLivelihood[,c("HouseholdID","PrimaryLivelihood")],by="HouseholdID") %>%
  left_join(HHData[,c("HouseholdID","MarineGroup")],by="HouseholdID") %>%
  mutate(MPAID=as.factor(MPAID),
         IndividualGender=as.factor(IndividualGender)) %>%
  group_by(MPAID, MonitoringYear, IndividualGender) %>%
  summarise(Num.Individuals=length(HouseholdID),
            FSMean=round(mean(FSIndex,na.rm=T),2),
            FSErr=round(sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),2),
            MAMean=round(mean(MAIndex,na.rm=T),2),
            MAErr=round(sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),2),
            PAMean=round(mean(PAIndex,na.rm=T),2),
            PAErr=round(sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),2),
            MTMean=round(mean(MTIndex,na.rm=T),2),
            MTErr=round(sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),2),
            SEMean=round(mean(SERate,na.rm=T),2),
            SEErr=round(sd(SERate,na.rm=T)/sqrt(length(SERate)),2),
            Percent.PrimaryOcc.Fish=(length(PrimaryLivelihood[PrimaryLivelihood==3 &
                                                                     !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Farm=(length(PrimaryLivelihood[PrimaryLivelihood==1 &
                                                                     !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.WageLabor=(length(PrimaryLivelihood[PrimaryLivelihood==7 &
                                                                          !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.HarvestForest=(length(PrimaryLivelihood[PrimaryLivelihood==2 &
                                                                              !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Tourism=(length(PrimaryLivelihood[PrimaryLivelihood==6 &
                                                                        !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.PrimaryOcc.Other=(length(PrimaryLivelihood[(PrimaryLivelihood==996 | PrimaryLivelihood==4 | 
                                                                       PrimaryLivelihood==5) & !is.na(PrimaryLivelihood)])/length(PrimaryLivelihood[!is.na(PrimaryLivelihood)]))*100,
            Percent.MarineGroup=(length(MarineGroup[MarineGroup==1 &
                                                      !is.na(MarineGroup)])/length(MarineGroup[!is.na(MarineGroup)]))*100)

# ---- 5.2 Assets, by HHH gender ----
MA.baselineplot.bygender <- 
  ggplot(data=BHS.demographics.byMPA.byGender[BHS.demographics.byMPA.byGender$MonitoringYear=="Baseline",],
         aes(x=MPAID)) +
  geom_bar(aes(y=MAMean,
               fill=IndividualGender,
               group=IndividualGender),
           stat="identity",
           position="dodge",
           width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=IndividualGender),
                position=position_dodge(0.75),
                width=0.25,
                size=0.5,
                show.legend=F) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,40)) +
  scale_fill_manual(name="",
                    labels=c("Female HHH","Male HHH"),
                    values= c(alpha("#7FCDBB",0.7),alpha("#253494",0.7))) +
  scale_colour_manual(values=c(alpha("#7FCDBB",1), alpha("#253494", 1))) +
  plot.theme.extraplots + labs(x="MPA",y="Household Material Assets",title="Material Assets, at baseline")


MA.4yrplot.bygender <- 
  ggplot(data=BHS.demographics.byMPA.byGender[BHS.demographics.byMPA.byGender$MonitoringYear=="4 Year Post",],
         aes(x=MPAID)) +
  geom_bar(aes(y=MAMean,
               fill=IndividualGender,
               group=IndividualGender),
           stat="identity",
           position="dodge",
           width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=IndividualGender),
                position=position_dodge(0.75),
                width=0.25,
                size=0.5,
                show.legend=F) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,40)) +
  scale_fill_manual(name="",
                    labels=c("Female HHH","Male HHH"),
                    values= c(alpha("#7FCDBB",0.7),alpha("#253494",0.7))) +
  scale_colour_manual(values=c(alpha("#7FCDBB",1), alpha("#253494", 1))) +
  plot.theme.extraplots + labs(x="MPA",y="Household Material Assets",title="Material Assets, 4 year post-baseline")



# ---- 5.3 Tenure, by HHH gender ----
MT.baselineplot.bygender <- 
  ggplot(data=BHS.demographics.byMPA.byGender[BHS.demographics.byMPA.byGender$MonitoringYear=="Baseline",],
         aes(x=MPAID)) +
  geom_bar(aes(y=MTMean,
               fill=IndividualGender,
               group=IndividualGender),
           stat="identity",
           position="dodge",
           width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=IndividualGender),
                position=position_dodge(0.75),
                width=0.25,
                size=0.5,
                show.legend=F) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(name="",
                    labels=c("Female HHH","Male HHH"),
                    values= c(alpha("#7FCDBB",0.7),alpha("#253494",0.7))) +
  scale_colour_manual(values=c(alpha("#7FCDBB",1), alpha("#253494", 1))) +
  plot.theme.extraplots + labs(x="MPA",y="Marine Tenure",title="Marine Tenure, at baseline")


MT.4yrplot.bygender <- 
  ggplot(data=BHS.demographics.byMPA.byGender[BHS.demographics.byMPA.byGender$MonitoringYear=="4 Year Post",],
         aes(x=MPAID)) +
  geom_bar(aes(y=MTMean,
               fill=IndividualGender,
               group=IndividualGender),
           stat="identity",
           position="dodge",
           width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=IndividualGender),
                position=position_dodge(0.75),
                width=0.25,
                size=0.5,
                show.legend=F) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(name="",
                    labels=c("Female HHH","Male HHH"),
                    values= c(alpha("#7FCDBB",0.7),alpha("#253494",0.7))) +
  scale_colour_manual(values=c(alpha("#7FCDBB",1), alpha("#253494", 1))) +
  plot.theme.extraplots + labs(x="MPA",y="Marine Tenure",title="Marine Tenure, 4 year post-baseline")


# ---- 5.4 Primary occupation, by HHH gender ----

PrimaryOccFish.baselineplot.bygender <- 
  ggplot(data=BHS.demographics.byMPA.byGender[BHS.demographics.byMPA.byGender$MonitoringYear=="Baseline",],
         aes(x=MPAID)) +
  geom_bar(aes(y=Percent.PrimaryOcc.Fish,
               fill=IndividualGender,
               group=IndividualGender),
           stat="identity",
           position="dodge",
           width=0.75) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,60)) +
  scale_fill_manual(name="",
                    labels=c("Female HHH","Male HHH"),
                    values= c(alpha("#7FCDBB",0.7),alpha("#253494",0.7))) +
  scale_colour_manual(values=c(alpha("#7FCDBB",1), alpha("#253494", 1))) +
  plot.theme.extraplots + labs(x="MPA",y="Percent of HH",title="Primary Occupation of fishing, at baseline")


PrimaryOccFish.4yrplot.bygender <- 
  ggplot(data=BHS.demographics.byMPA.byGender[BHS.demographics.byMPA.byGender$MonitoringYear=="4 Year Post",],
         aes(x=MPAID)) +
  geom_bar(aes(y=Percent.PrimaryOcc.Fish,
               fill=IndividualGender,
               group=IndividualGender),
           stat="identity",
           position="dodge",
           width=0.75) +
  scale_x_discrete(labels=c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,60)) +
  scale_fill_manual(name="",
                    labels=c("Female HHH","Male HHH"),
                    values= c(alpha("#7FCDBB",0.7),alpha("#253494",0.7))) +
  scale_colour_manual(values=c(alpha("#7FCDBB",1), alpha("#253494", 1))) +
  plot.theme.extraplots + labs(x="MPA",y="Percent of HH",title="Primary Occupation of fishing, 4 year post-baseline")
