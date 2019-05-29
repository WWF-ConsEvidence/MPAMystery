# Comparing matched vs. non-matched data AND baselines for Tr=1 on t2 and t4 panels.

# ---- source data ----

source('Calculate_ATTs_BHS.R')


# ---- Compare baseline data for treatment households in t2 and t4 fake the panels, Big Five ----

# --Food security

hfs.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(hfs.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% hfs.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(hfs.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% hfs.outcome.t4$tr1t0))) %>%
  filter(!is.na(year))


dist.matched.baseline.hfs <- 
  ggplot(hfs.compare.baselines[hfs.compare.baselines$year=="t2",]) +
  geom_histogram(aes(x=FSIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(hfs.compare.baselines$FSIndex),
                                    sd=sd(hfs.compare.baselines$FSIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Food Security \n(per household)",
       y="Density",
       title="BHS: Food Security Score Distribution,\nmatched t2 baseline")

qqnorm(hfs.compare.baselines$FSIndex)
qqline(hfs.compare.baselines$FSIndex,col="green")

# t test
hfs.baseline.panels.test <- NULL

for(i in unique(hfs.compare.baselines$MPAID)){
  test.data <- hfs.compare.baselines[hfs.compare.baselines$MPAID==i,]
  hfs.baseline.panels.test[i] <- t.test(test.data$FSIndex~test.data$year)["p.value"]
}

# output data frame with mean baselines by MPA
hfs.compare.baselines.MPA <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(hfs.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% hfs.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(hfs.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% hfs.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(hfs.baseline.mean=mean(FSIndex,na.rm=T)) %>%
  .[order(.$MPAID),]


number.repeats.hfs <- rbind.data.frame(cbind.data.frame(year=rep("t2",length(hfs.outcome.t2$tr1t0)),
                                  left_join(hfs.outcome.t2,HHData,by=c("tr1t0"="HouseholdID"))),
                 cbind.data.frame(year=rep("t4",length(hfs.outcome.t4$tr1t0)),
                                  left_join(hfs.outcome.t4,HHData,by=c("tr1t0"="HouseholdID")))) %>%
  group_by(tr1t0,year,MPAID) %>%
  summarise(number.repeats=length(tr1t0))


# --Material assets

asset.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(asset.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(asset.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t4$tr1t0)))%>%
  filter(!is.na(year))


dist.matched.baseline.asset <- 
  ggplot(asset.compare.baselines[asset.compare.baselines$year=="t2",]) +
  geom_histogram(aes(x=MAIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(asset.compare.baselines$MAIndex),
                                    sd=sd(asset.compare.baselines$MAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Material Assets \n(per household)",
       y="Density",
       title="BHS: Material Assets Score Distribution,\nmatched t2 baseline")

qqnorm(asset.compare.baselines$MAIndex)
qqline(asset.compare.baselines$MAIndex,col="green")

# t test
asset.baseline.panels.test <- NULL

for(i in unique(asset.compare.baselines$MPAID)){
  test.data <- asset.compare.baselines[asset.compare.baselines$MPAID==i,]
  asset.baseline.panels.test[i] <- wilcox.test(test.data$MAIndex~test.data$year)["p.value"]
}

# output data frame with mean baselines by MPA
asset.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(asset.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(asset.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(asset.baseline.mean=mean(MAIndex,na.rm=T)) %>%
  .[order(.$MPAID),]

number.repeats.asset <- rbind.data.frame(cbind.data.frame(year=rep("t2",length(asset.outcome.t2$tr1t0)),
                                                        left_join(asset.outcome.t2,HHData,by=c("tr1t0"="HouseholdID"))),
                                       cbind.data.frame(year=rep("t4",length(asset.outcome.t4$tr1t0)),
                                                        left_join(asset.outcome.t4,HHData,by=c("tr1t0"="HouseholdID")))) %>%
  group_by(tr1t0,year,MPAID) %>%
  summarise(number.repeats=length(tr1t0))


# --Marine tenure

tenure.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(tenure.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% tenure.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(tenure.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% tenure.outcome.t4$tr1t0))) %>%
  filter(!is.na(year))


dist.matched.baseline.tenure <- 
  ggplot(tenure.compare.baselines[tenure.compare.baselines$year=="t2",]) +
  geom_histogram(aes(x=MTIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(tenure.compare.baselines$MTIndex),
                                    sd=sd(tenure.compare.baselines$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Marine Tenure \n(per household)",
       y="Density",
       title="BHS: Marine Tenure Score Distribution,\nmatched t2 baseline")

qqnorm(tenure.compare.baselines$MTIndex)
qqline(tenure.compare.baselines$MTIndex,col="green")

# t test
tenure.baseline.panels.test <- NULL

for(i in unique(tenure.compare.baselines$MPAID)){
  test.data <- tenure.compare.baselines[tenure.compare.baselines$MPAID==i,]
  tenure.baseline.panels.test[i] <- wilcox.test(test.data$MTIndex~test.data$year)["p.value"]
}

# output data frame with mean baselines by MPA
tenure.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(tenure.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% tenure.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(tenure.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% tenure.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(tenure.baseline.mean=mean(MTIndex,na.rm=T)) %>%
  .[order(.$MPAID),]



# ---- TEST: are the 'time-invariant' covariats truly time-invariant? ----

ethnic.lkp<- read.delim("x_Flat_data_files/1_Social/Inputs/BHS/eth_output_kc_2017_1217.txt")
education_lkp <- read.delim("x_Flat_data_files/1_Social/Inputs/BHS/education_lkp.txt")


# Time-invariant covariates should be: dominant ethnicity, number of children, individual gender, education level, years resident, individual age
                                      # NOTE: TimeMarket used to be included, but has been shown to be time-variant.

# Age, number children, gender

covariates <- 
  IndDemos[IndDemos$MPAID<7,] %>%
  group_by(HouseholdID) %>%
  summarise(Age=IndividualAge[RelationHHH==0],
            Gender=IndividualGender[RelationHHH==0],
            NumChild=length(DemographicID[IndividualAge<19 & !is.na(IndividualAge)])) %>%
  left_join(.,HHData[,c("HouseholdID","MPAID","MonitoringYear","YrResident")], by="HouseholdID")


# create empty data frame for p values of covariate t tests below
covariate.tests <- data.frame(MPAID=c(1:6),
                              Age.t0.t2.pval=rep(NA,6),
                              Age.t2.t4.pval=rep(NA,6),
                              Age.t0.t4.pval=rep(NA,6),
                              YrRes.t0.t2.pval=rep(NA,6),
                              YrRes.t2.t4.pval=rep(NA,6),
                              YrRes.t0.t4.pval=rep(NA,6),
                              Gender.t0.t2.pval=rep(NA,6),
                              Gender.t2.t4.pval=rep(NA,6),
                              Gender.t0.t4.pval=rep(NA,6),
                              NumChild.t0.t2.pval=rep(NA,6),
                              NumChild.t2.t4.pval=rep(NA,6),
                              NumChild.t0.t4.pval=rep(NA,6))

# t tests on age

dist.age.t2 <- 
  ggplot(covariates[covariates$MonitoringYear=="2 Year Post",]) +
  geom_histogram(aes(x=Age,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(covariates$MTIndex),
                                    sd=sd(covariates$MTIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Age Head of Household",
       y="Density",
       title="BHS: Age Head of Household Distribution,\n unmatched t2")

qqnorm(covariates$Age[covariates$MonitoringYear=="2 Year Post"])
qqline(covariates$Age,col="green")


for(i in unique(covariates$MPAID)){
  test.data.t0.t2 <- covariates[(covariates$MonitoringYear=="Baseline" |
                             covariates$MonitoringYear=="2 Year Post") & covariates$MPAID==i,]
  covariate.tests$Age.t0.t2.pval[i] <- as.numeric(t.test(test.data.t0.t2$Age~test.data.t0.t2$MonitoringYear)["p.value"])
  
  
  test.data.t2.t4 <- covariates[(covariates$MonitoringYear=="2 Year Post" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$Age.t2.t4.pval[i] <- as.numeric(t.test(test.data.t2.t4$Age~test.data.t2.t4$MonitoringYear)["p.value"])
  
  
  test.data.t0.t4 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$Age.t0.t4.pval[i] <- as.numeric(t.test(test.data.t0.t4$Age~test.data.t0.t4$MonitoringYear)["p.value"])
}


# t tests on residency

dist.residency.t2 <- 
  ggplot(covariates[covariates$MonitoringYear=="2 Year Post",]) +
  geom_histogram(aes(x=YrResident,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(covariates$YrResident),
                                    sd=sd(covariates$YrResident)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Years Resident",
       y="Density",
       title="BHS: Residency Distribution,\n unmatched t2")

qqnorm(covariates$YrResident[covariates$MonitoringYear=="2 Year Post"])
qqline(covariates$YrResident,col="green")


for(i in unique(covariates$MPAID)){
  test.data.t0.t2 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="2 Year Post") & covariates$MPAID==i,]
  covariate.tests$YrRes.t0.t2.pval[i] <- as.numeric(t.test(test.data.t0.t2$YrResident~test.data.t0.t2$MonitoringYear)["p.value"])
  
  
  test.data.t2.t4 <- covariates[(covariates$MonitoringYear=="2 Year Post" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$YrRes.t2.t4.pval[i] <- as.numeric(t.test(test.data.t2.t4$YrResident~test.data.t2.t4$MonitoringYear)["p.value"])
  
  
  test.data.t0.t4 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$YrRes.t0.t4.pval[i] <- as.numeric(t.test(test.data.t0.t4$YrResident~test.data.t0.t4$MonitoringYear)["p.value"])
}


# t tests on gender

for(i in unique(covariates$MPAID)){
  test.data.t0.t2 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="2 Year Post") & covariates$MPAID==i,]
  covariate.tests$Gender.t0.t2.pval[i] <- as.numeric(wilcox.test(test.data.t0.t2$Gender~test.data.t0.t2$MonitoringYear)["p.value"])
  
  
  test.data.t2.t4 <- covariates[(covariates$MonitoringYear=="2 Year Post" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$Gender.t2.t4.pval[i] <- as.numeric(wilcox.test(test.data.t2.t4$Gender~test.data.t2.t4$MonitoringYear)["p.value"])
  
  
  test.data.t0.t4 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$Gender.t0.t4.pval[i] <- as.numeric(wilcox.test(test.data.t0.t4$Gender~test.data.t0.t4$MonitoringYear)["p.value"])
}


# t tests on number children

for(i in unique(covariates$MPAID)){
  test.data.t0.t2 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="2 Year Post") & covariates$MPAID==i,]
  covariate.tests$NumChild.t0.t2.pval[i] <- as.numeric(t.test(test.data.t0.t2$NumChild~test.data.t0.t2$MonitoringYear)["p.value"])
  
  
  test.data.t2.t4 <- covariates[(covariates$MonitoringYear=="2 Year Post" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$NumChild.t2.t4.pval[i] <- as.numeric(t.test(test.data.t2.t4$NumChild~test.data.t2.t4$MonitoringYear)["p.value"])
  
  
  test.data.t0.t4 <- covariates[(covariates$MonitoringYear=="Baseline" |
                                   covariates$MonitoringYear=="4 Year Post") & covariates$MPAID==i,]
  covariate.tests$NumChild.t0.t4.pval[i] <- as.numeric(t.test(test.data.t0.t4$NumChild~test.data.t0.t4$MonitoringYear)["p.value"])
}


# Trends at MPA level (unmatched, across all households (treatment and control together))
covariates.means <-
  covariates %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(Age=mean(Age,na.rm=T),
            Gender=mean(Gender,na.rm=T),
            NumChild=mean(NumChild,na.rm=T),
            YrResident=mean(YrResident,na.rm=T))

covariate.age.plot <-
  ggplot(covariates.means) +
  geom_bar(aes(x=MPAID,
               y=Age,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,50)) +
  scale_x_continuous(breaks=c(1:6)) +
  plot.theme + labs(x="MPA",y="Mean Age Household Head")


covariate.gender.plot <-
  ggplot(covariates.means) +
  geom_bar(aes(x=MPAID,
               y=Gender,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1.05)) +
  scale_x_continuous(breaks=c(1:6)) +
  plot.theme + labs(x="MPA",y="Prop Male Household Heads")

covariate.numchild.plot <-
  ggplot(covariates.means) +
  geom_bar(aes(x=MPAID,
               y=NumChild,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0.3,5)) +
  scale_x_continuous(breaks=c(1:6)) +
  plot.theme + labs(x="MPA",y="Mean Number Children")

covariate.yrres.plot <-
  ggplot(covariates.means) +
  geom_bar(aes(x=MPAID,
               y=YrResident,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,40)) +
  scale_x_continuous(breaks=c(1:6)) +
  plot.theme + labs(x="MPA",y="Mean Years Resident")



# Trends at seascape level, broken down by treatment vs. control
covariates.seascape.trends <-
  left_join(covariates,HHData[,c("HouseholdID","Treatment")],by="HouseholdID") %>%
  group_by(Treatment,MonitoringYear) %>%
  summarise(Age=mean(Age,na.rm=T),
            Gender=mean(Gender,na.rm=T),
            NumChild=mean(NumChild,na.rm=T),
            YrResident=mean(YrResident,na.rm=T))


covariate.age.seascape.plot <-
  ggplot(covariates.seascape.trends) +
  geom_bar(aes(x=Treatment,
               y=Age,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,50)) +
  scale_x_continuous(breaks=c(0,1),
                     labels=c("Control","MPA")) +
  plot.theme + labs(x="Treatment",y="Mean Age Household Head")


covariate.gender.seascape.plot <-
  ggplot(covariates.seascape.trends) +
  geom_bar(aes(x=Treatment,
               y=Gender,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1.05)) +
  scale_x_continuous(breaks=c(0,1),
                     labels=c("Control","MPA")) +
  plot.theme + labs(x="Treatment",y="Prop Male Household Heads")


covariate.numchild.seascape.plot <-
  ggplot(covariates.seascape.trends) +
  geom_bar(aes(x=Treatment,
               y=NumChild,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,3.5)) +
  scale_x_continuous(breaks=c(0,1),
                     labels=c("Control","MPA")) +
  plot.theme + labs(x="Treatment",y="Mean Number Children")


covariate.yrres.seascape.plot <-
  ggplot(covariates.seascape.trends) +
  geom_bar(aes(x=Treatment,
               y=YrResident,
               group=MonitoringYear,
               alpha=MonitoringYear),
           stat = "identity",
           position = "dodge",
           width = 0.5) + 
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,35)) +
  scale_x_continuous(breaks=c(0,1),
                     labels=c("Control","MPA")) +
  plot.theme + labs(x="Treatment",y="Mean Years Resident")
