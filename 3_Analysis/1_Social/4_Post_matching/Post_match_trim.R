# code: Apply trim to baseline data across t2 and t4 panels to ensure all analyses are pulling from same sample population


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & DATA, PRE-PROCESS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- Source functions ----

source('2_Functions/2_Analysis/Function_process_covariates.R')
source('2_Functions/2_Analysis/Function_outcome_ATT_method1.R')

# ---- Load in master pairs from matching, fake the panel A ----

master.t2.A <- xlsx::read.xlsx('x_Flat_data_files/1_Social/Inputs/BHS/t2_impacts/master_t2_panelA.xlsx', sheetName='Sheet 1')
master.t4.A <- xlsx::read.xlsx('x_Flat_data_files/1_Social/Inputs/BHS/t4_impacts/master_t4_panelA.xlsx', sheetName='Sheet 1')


# ---- Process covariates and prepare data frame for propensity scoring and trim, per outcome variable ----

# t2 panel baseline data, process covariates
HH.data.baseline.t2.panel <- 
  melt(master.t2.A,
       measure.vars=c("HouseholdID.tr1.t0","HouseholdID.tr0.t0")) %>%
  transmute(HouseholdID=value) %>%
  .[!duplicated(.),]

HH.data.baseline.t2.panel <- HHData %>% filter(HouseholdID%in%HH.data.baseline.t2.panel)

DE.data.baseline.t2.panel <-
  left_join(HH.data.baseline.t2.panel[,c("HouseholdID","MPAID")],IndDemos,by=c("HouseholdID","MPAID"))

covariates.baseline.t2.panel <- 
  process_covariates(HH.data.baseline.t2.panel, DE.data.baseline.t2.panel) %>%
  left_join(HHData[,c("HouseholdID","FSIndex","MAIndex","MTIndex","PAIndex","SERate")],by="HouseholdID")


# t4 panel baseline data, process covariates
HH.data.baseline.t4.panel <- 
  melt(master.t4.A,
       measure.vars=c("HouseholdID.tr1.t0","HouseholdID.tr0.t0")) %>%
  transmute(HouseholdID=value) %>%
  .[!duplicated(.),]

HH.data.baseline.t4.panel <- HHData %>% filter(HouseholdID%in%HH.data.baseline.t4.panel)


DE.data.baseline.t4.panel <-
  left_join(HH.data.baseline.t4.panel[,c("HouseholdID","MPAID")],IndDemos,by=c("HouseholdID","MPAID"))

covariates.baseline.t4.panel <- 
  process_covariates(HH.data.baseline.t4.panel, DE.data.baseline.t4.panel) %>%
  left_join(HHData[,c("HouseholdID","FSIndex","MAIndex","MTIndex","PAIndex","SERate")],by="HouseholdID")

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: HOUSEHOLD FOOD SECURITY ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- hfs pscore calculations & trim ----

# t2 panel baseline data
hfs.p.score.t2  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + FSIndex, 
                  data=covariates.baseline.t2.panel%>%filter(!is.na(FSIndex)), family= binomial())$fitted.values

hfs.covariates.t2.pscore <- cbind(covariates.baseline.t2.panel%>%filter(!is.na(FSIndex)),hfs.p.score.t2)

# t4 panel baseline data
hfs.p.score.t4  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + FSIndex, 
                                  data=covariates.baseline.t4.panel%>%filter(!is.na(FSIndex)), family= binomial())$fitted.values

hfs.covariates.t4.pscore <- cbind(covariates.baseline.t4.panel%>%filter(!is.na(FSIndex)),hfs.p.score.t4)

# get ranges
hfs.p.score.range <- 
  data.frame(panel=c("t2","t4"),
             minimum=c(range(hfs.p.score.t2)[1],
                        range(hfs.p.score.t4)[1]),
             maximum=c(range(hfs.p.score.t2)[2],
                        range(hfs.p.score.t4)[2])) %>%
  summarise(min=max(minimum),
            max=min(maximum))

# p.score.sd <- sd((c(p.score.t0,p.score.t2,p.score.t4)))
# p.score.range.sd <- range (p.score.range + (0.5 * p.score.sd),p.score.range - (0.5 * p.score.sd))

# output trimmed data frames for t2 and t4 panel baseline data
hfs.t2.panel.trim <- hfs.covariates.t2.pscore %>% 
  filter(hfs.p.score.t2>=hfs.p.score.range$min & hfs.p.score.t2<=hfs.p.score.range$max)

hfs.t4.panel.trim <- hfs.covariates.t4.pscore %>% 
  filter(hfs.p.score.t4>=hfs.p.score.range$min & hfs.p.score.t4<=hfs.p.score.range$max)


nrow(covariates.baseline.t2.panel%>%filter(!is.na(FSIndex)))
nrow(hfs.t2.panel.trim)

nrow(covariates.baseline.t4.panel%>%filter(!is.na(FSIndex)))
nrow(hfs.t4.panel.trim)


# ---- Define data frame for all hfs outcomes and ATT analyses ----

hfs.t2.pairs <-
  master.t2.A %>%
  filter(HouseholdID.tr1.t0 %in% hfs.t2.panel.trim[hfs.t2.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% hfs.t2.panel.trim[hfs.t2.panel.trim$Treatment==0,"HouseholdID"])

hfs.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = hfs.t2.pairs, outcomes = HHData, var = FSIndex)


hfs.t4.pairs <-
  master.t4.A %>%
  filter(HouseholdID.tr1.t0 %in% hfs.t4.panel.trim[hfs.t4.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% hfs.t4.panel.trim[hfs.t4.panel.trim$Treatment==0,"HouseholdID"])

hfs.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = hfs.t4.pairs, outcomes = HHData, var = FSIndex)



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



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: HOUSEHOLD MATERIAL ASSETS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- asset pscore calculations & trim ----

# t2 panel baseline data
asset.p.score.t2  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + MAIndex, 
                       data=covariates.baseline.t2.panel%>%filter(!is.na(MAIndex)), family= binomial())$fitted.values

asset.covariates.t2.pscore <- cbind(covariates.baseline.t2.panel%>%filter(!is.na(MAIndex)),asset.p.score.t2)

# t4 panel baseline data
asset.p.score.t4  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + MAIndex, 
                       data=covariates.baseline.t4.panel%>%filter(!is.na(MAIndex)), family= binomial())$fitted.values

asset.covariates.t4.pscore <- cbind(covariates.baseline.t4.panel%>%filter(!is.na(MAIndex)),asset.p.score.t4)

# get ranges
asset.p.score.range <- 
  data.frame(panel=c("t2","t4"),
             minimum=c(range(asset.p.score.t2)[1],
                       range(asset.p.score.t4)[1]),
             maximum=c(range(asset.p.score.t2)[2],
                       range(asset.p.score.t4)[2])) %>%
  summarise(min=max(minimum),
            max=min(maximum))

# p.score.sd <- sd((c(p.score.t0,p.score.t2,p.score.t4)))
# p.score.range.sd <- range (p.score.range + (0.5 * p.score.sd),p.score.range - (0.5 * p.score.sd))

# output trimmed data frames for t2 and t4 panel baseline data
asset.t2.panel.trim <- asset.covariates.t2.pscore %>% 
  filter(asset.p.score.t2>=asset.p.score.range$min & asset.p.score.t2<=asset.p.score.range$max)

asset.t4.panel.trim <- asset.covariates.t4.pscore %>% 
  filter(asset.p.score.t4>=asset.p.score.range$min & asset.p.score.t4<=asset.p.score.range$max)


nrow(covariates.baseline.t2.panel%>%filter(!is.na(MAIndex)))
nrow(asset.t2.panel.trim)

nrow(covariates.baseline.t4.panel%>%filter(!is.na(MAIndex)))
nrow(asset.t4.panel.trim)


# ---- Define data frame for all asset outcomes and ATT analyses ----

asset.t2.pairs <-
  master.t2.A %>%
  filter(HouseholdID.tr1.t0 %in% asset.t2.panel.trim[asset.t2.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% asset.t2.panel.trim[asset.t2.panel.trim$Treatment==0,"HouseholdID"])

asset.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = asset.t2.pairs, outcomes = HHData, var = MAIndex)


asset.t4.pairs <-
  master.t4.A %>%
  filter(HouseholdID.tr1.t0 %in% asset.t4.panel.trim[asset.t4.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% asset.t4.panel.trim[asset.t4.panel.trim$Treatment==0,"HouseholdID"])

asset.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = asset.t4.pairs, outcomes = HHData, var = MAIndex)



asset.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(asset.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(asset.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t4$tr1t0))) %>%
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
       title="BHS: Material Assets Distribution,\nmatched t2 baseline")

qqnorm(asset.compare.baselines$MAIndex)
qqline(asset.compare.baselines$MAIndex,col="green")

# t test
asset.baseline.panels.test <- NULL

for(i in unique(asset.compare.baselines$MPAID)){
  test.data <- asset.compare.baselines[asset.compare.baselines$MPAID==i,]
  asset.baseline.panels.test[i] <- t.test(test.data$MAIndex~test.data$year)["p.value"]
}


# output data frame with mean baselines by MPA
asset.compare.baselines.MPA <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(asset.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(asset.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% asset.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(asset.baseline.mean=mean(MAIndex,na.rm=T)) %>%
  .[order(.$MPAID),]




# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: MARINE TENURE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- tenure pscore calculations & trim ----

# t2 panel baseline data
tenure.p.score.t2  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + MTIndex, 
                         data=covariates.baseline.t2.panel%>%filter(!is.na(MTIndex)), family= binomial())$fitted.values

tenure.covariates.t2.pscore <- cbind(covariates.baseline.t2.panel%>%filter(!is.na(MTIndex)),tenure.p.score.t2)

# t4 panel baseline data
tenure.p.score.t4  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + MTIndex, 
                         data=covariates.baseline.t4.panel%>%filter(!is.na(MTIndex)), family= binomial())$fitted.values

tenure.covariates.t4.pscore <- cbind(covariates.baseline.t4.panel%>%filter(!is.na(MTIndex)),tenure.p.score.t4)

# get ranges
tenure.p.score.range <- 
  data.frame(panel=c("t2","t4"),
             minimum=c(range(tenure.p.score.t2)[1],
                       range(tenure.p.score.t4)[1]),
             maximum=c(range(tenure.p.score.t2)[2],
                       range(tenure.p.score.t4)[2])) %>%
  summarise(min=max(minimum),
            max=min(maximum))

# p.score.sd <- sd((c(p.score.t0,p.score.t2,p.score.t4)))
# p.score.range.sd <- range (p.score.range + (0.5 * p.score.sd),p.score.range - (0.5 * p.score.sd))

# output trimmed data frames for t2 and t4 panel baseline data
tenure.t2.panel.trim <- tenure.covariates.t2.pscore %>% 
  filter(tenure.p.score.t2>=tenure.p.score.range$min & tenure.p.score.t2<=tenure.p.score.range$max)

tenure.t4.panel.trim <- tenure.covariates.t4.pscore %>% 
  filter(tenure.p.score.t4>=tenure.p.score.range$min & tenure.p.score.t4<=tenure.p.score.range$max)


nrow(covariates.baseline.t2.panel%>%filter(!is.na(MTIndex)))
nrow(tenure.t2.panel.trim)

nrow(covariates.baseline.t4.panel%>%filter(!is.na(MTIndex)))
nrow(tenure.t4.panel.trim)


# ---- Define data frame for all tenure outcomes and ATT analyses ----

tenure.t2.pairs <-
  master.t2.A %>%
  filter(HouseholdID.tr1.t0 %in% tenure.t2.panel.trim[tenure.t2.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% tenure.t2.panel.trim[tenure.t2.panel.trim$Treatment==0,"HouseholdID"])

tenure.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = tenure.t2.pairs, outcomes = HHData, var = MTIndex)


tenure.t4.pairs <-
  master.t4.A %>%
  filter(HouseholdID.tr1.t0 %in% tenure.t4.panel.trim[tenure.t4.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% tenure.t4.panel.trim[tenure.t4.panel.trim$Treatment==0,"HouseholdID"])

tenure.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = tenure.t4.pairs, outcomes = HHData, var = MTIndex)



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
       title="BHS: Marine Tenure Distribution,\nmatched t2 baseline")

qqnorm(tenure.compare.baselines$MTIndex)
qqline(tenure.compare.baselines$MTIndex,col="green")

# t test
tenure.baseline.panels.test <- NULL

for(i in unique(tenure.compare.baselines$MPAID)){
  test.data <- tenure.compare.baselines[tenure.compare.baselines$MPAID==i,]
  tenure.baseline.panels.test[i] <- t.test(test.data$MTIndex~test.data$year)["p.value"]
}


# output data frame with mean baselines by MPA
tenure.compare.baselines.MPA <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(tenure.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% tenure.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(tenure.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% tenure.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(tenure.baseline.mean=mean(MTIndex,na.rm=T)) %>%
  .[order(.$MPAID),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: PLACE ATTACHMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- attach pscore calculations & trim ----

# t2 panel baseline data
attach.p.score.t2  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + PAIndex, 
                          data=covariates.baseline.t2.panel%>%filter(!is.na(PAIndex)), family= binomial())$fitted.values

attach.covariates.t2.pscore <- cbind(covariates.baseline.t2.panel%>%filter(!is.na(PAIndex)),attach.p.score.t2)

# t4 panel baseline data
attach.p.score.t4  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + PAIndex, 
                          data=covariates.baseline.t4.panel%>%filter(!is.na(PAIndex)), family= binomial())$fitted.values

attach.covariates.t4.pscore <- cbind(covariates.baseline.t4.panel%>%filter(!is.na(PAIndex)),attach.p.score.t4)

# get ranges
attach.p.score.range <- 
  data.frame(panel=c("t2","t4"),
             minimum=c(range(attach.p.score.t2)[1],
                       range(attach.p.score.t4)[1]),
             maximum=c(range(attach.p.score.t2)[2],
                       range(attach.p.score.t4)[2])) %>%
  summarise(min=max(minimum),
            max=min(maximum))

# p.score.sd <- sd((c(p.score.t0,p.score.t2,p.score.t4)))
# p.score.range.sd <- range (p.score.range + (0.5 * p.score.sd),p.score.range - (0.5 * p.score.sd))

# output trimmed data frames for t2 and t4 panel baseline data
attach.t2.panel.trim <- attach.covariates.t2.pscore %>% 
  filter(attach.p.score.t2>=attach.p.score.range$min & attach.p.score.t2<=attach.p.score.range$max)

attach.t4.panel.trim <- attach.covariates.t4.pscore %>% 
  filter(attach.p.score.t4>=attach.p.score.range$min & attach.p.score.t4<=attach.p.score.range$max)


nrow(covariates.baseline.t2.panel%>%filter(!is.na(PAIndex)))
nrow(attach.t2.panel.trim)

nrow(covariates.baseline.t4.panel%>%filter(!is.na(PAIndex)))
nrow(attach.t4.panel.trim)


# ---- Define data frame for all attach outcomes and ATT analyses ----

attach.t2.pairs <-
  master.t2.A %>%
  filter(HouseholdID.tr1.t0 %in% attach.t2.panel.trim[attach.t2.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% attach.t2.panel.trim[attach.t2.panel.trim$Treatment==0,"HouseholdID"])

attach.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = attach.t2.pairs, outcomes = HHData, var = PAIndex)


attach.t4.pairs <-
  master.t4.A %>%
  filter(HouseholdID.tr1.t0 %in% attach.t4.panel.trim[attach.t4.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% attach.t4.panel.trim[attach.t4.panel.trim$Treatment==0,"HouseholdID"])

attach.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = attach.t4.pairs, outcomes = HHData, var = PAIndex)



attach.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(attach.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% attach.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(attach.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% attach.outcome.t4$tr1t0))) %>%
  filter(!is.na(year))


dist.matched.baseline.attach <- 
  ggplot(attach.compare.baselines[attach.compare.baselines$year=="t2",]) +
  geom_histogram(aes(x=PAIndex,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(attach.compare.baselines$PAIndex),
                                    sd=sd(attach.compare.baselines$PAIndex)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="Place Attachment \n(per household)",
       y="Density",
       title="BHS: Place Attachment Distribution,\nmatched t2 baseline")

qqnorm(attach.compare.baselines$PAIndex)
qqline(attach.compare.baselines$PAIndex,col="green")

# t test
attach.baseline.panels.test <- NULL

for(i in unique(attach.compare.baselines$MPAID)){
  test.data <- attach.compare.baselines[attach.compare.baselines$MPAID==i,]
  attach.baseline.panels.test[i] <- t.test(test.data$PAIndex~test.data$year)["p.value"]
}


# output data frame with mean baselines by MPA
attach.compare.baselines.MPA <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(attach.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% attach.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(attach.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% attach.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(attach.baseline.mean=mean(PAIndex,na.rm=T)) %>%
  .[order(.$MPAID),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: SCHOOL ENROLLMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- enrol pscore calculations & trim ----

# t2 panel baseline data
enrol.p.score.t2  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + SERate, 
                          data=covariates.baseline.t2.panel%>%filter(!is.na(SERate)), family= binomial())$fitted.values

enrol.covariates.t2.pscore <- cbind(covariates.baseline.t2.panel%>%filter(!is.na(SERate)),enrol.p.score.t2)

# t4 panel baseline data
enrol.p.score.t4  <- glm(Treatment~ n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + SERate, 
                          data=covariates.baseline.t4.panel%>%filter(!is.na(SERate)), family= binomial())$fitted.values

enrol.covariates.t4.pscore <- cbind(covariates.baseline.t4.panel%>%filter(!is.na(SERate)),enrol.p.score.t4)

# get ranges
enrol.p.score.range <- 
  data.frame(panel=c("t2","t4"),
             minimum=c(range(enrol.p.score.t2)[1],
                       range(enrol.p.score.t4)[1]),
             maximum=c(range(enrol.p.score.t2)[2],
                       range(enrol.p.score.t4)[2])) %>%
  summarise(min=max(minimum),
            max=min(maximum))

# p.score.sd <- sd((c(p.score.t0,p.score.t2,p.score.t4)))
# p.score.range.sd <- range (p.score.range + (0.5 * p.score.sd),p.score.range - (0.5 * p.score.sd))

# output trimmed data frames for t2 and t4 panel baseline data
enrol.t2.panel.trim <- enrol.covariates.t2.pscore %>% 
  filter(enrol.p.score.t2>=enrol.p.score.range$min & enrol.p.score.t2<=enrol.p.score.range$max)

enrol.t4.panel.trim <- enrol.covariates.t4.pscore %>% 
  filter(enrol.p.score.t4>=enrol.p.score.range$min & enrol.p.score.t4<=enrol.p.score.range$max)


nrow(covariates.baseline.t2.panel%>%filter(!is.na(SERate)))
nrow(enrol.t2.panel.trim)

nrow(covariates.baseline.t4.panel%>%filter(!is.na(SERate)))
nrow(enrol.t4.panel.trim)


# ---- Define data frame for all enrol outcomes and ATT analyses ----

enrol.t2.pairs <-
  master.t2.A %>%
  filter(HouseholdID.tr1.t0 %in% enrol.t2.panel.trim[enrol.t2.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% enrol.t2.panel.trim[enrol.t2.panel.trim$Treatment==0,"HouseholdID"])

enrol.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = enrol.t2.pairs, outcomes = HHData, var = SERate)


enrol.t4.pairs <-
  master.t4.A %>%
  filter(HouseholdID.tr1.t0 %in% enrol.t4.panel.trim[enrol.t4.panel.trim$Treatment==1,"HouseholdID"]) %>%
  filter(HouseholdID.tr0.t0 %in% enrol.t4.panel.trim[enrol.t4.panel.trim$Treatment==0,"HouseholdID"])

enrol.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = enrol.t4.pairs, outcomes = HHData, var = SERate)



enrol.compare.baselines <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(enrol.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% enrol.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(enrol.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% enrol.outcome.t4$tr1t0))) %>%
  filter(!is.na(year))


dist.matched.baseline.enrol <- 
  ggplot(enrol.compare.baselines[enrol.compare.baselines$year=="t2",]) +
  geom_histogram(aes(x=SERate,y=..density..),
                 bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
  stat_function(fun=dnorm,args=list(mean=mean(enrol.compare.baselines$SERate),
                                    sd=sd(enrol.compare.baselines$SERate)),
                colour="#262F52",size=1,na.rm=T) +
  labs(x="School Enrollment \n(per household)",
       y="Density",
       title="BHS: School Enrollment Distribution,\nmatched t2 baseline")

qqnorm(enrol.compare.baselines$SERate)
qqline(enrol.compare.baselines$SERate,col="green")

# t test
enrol.baseline.panels.test <- NULL

for(i in unique(enrol.compare.baselines$MPAID)){
  test.data <- enrol.compare.baselines[enrol.compare.baselines$MPAID==i,]
  enrol.baseline.panels.test[i] <- t.test(test.data$SERate~test.data$year)["p.value"]
}


# output data frame with mean baselines by MPA
enrol.compare.baselines.MPA <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(unique(enrol.outcome.t2$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% enrol.outcome.t2$tr1t0)),
                   cbind.data.frame(year=rep("t4",length(unique(enrol.outcome.t4$tr1t0))),
                                    HHData%>%filter(HouseholdID %in% enrol.outcome.t4$tr1t0))) %>%
  group_by(year,MPAID) %>%
  summarise(enrol.baseline.mean=mean(SERate,na.rm=T)) %>%
  .[order(.$MPAID),]