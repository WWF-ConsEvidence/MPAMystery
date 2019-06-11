# ---
# code:  Medium-term social impacts of marine protected areas -BHS 
# author: Louise Glew, louise.glew@gmail.com
# created: May 2016
# modified: April 2018

# -----
# inputs: 
# HH.data - data on MPA outcomes, plus identifying variables
# DE.data - individual level demographic information
# district.data - treatment identifier for each settlement, plus other district level information
# Ethnic - processed paternal ethnicity data for each household
# -----
# outputs:
# std.diff.seascape.txt --Standardized mean differences for ATTs aggregated to Seascape level
# smd_by_MPA.xlsx --Standard mean differences for ATTs by MPA
# typology_by_MPA.xlsx -- mean outcomes for MPA and controls for each outcome, by MPA

# -----

# -----
# Code sections
#  1) Call libraries and import raw data


# =================
#
# ----SECTION 1: Call libraries, functions and import raw data----

# ===
# 1.1 Call in libraries
# 1.2 Create functions
# 1.3 Import raw data
#===

# 1.1 Call in libraries
pacman::p_load(twang,tm,NLP,readxl,cobalt,CBPS,Matching,optmatch,tidyr,RItools,Hmisc,MBESS,rbounds,reshape2,Matching,openxlsx,rio,dplyr)

# ---
# 1.2 
source('2_Functions/2_Analysis/Function_process_covariates.R')

# ---
# 1.3 Import raw data
HH.data <- HHData %>% filter(MPAID<7) 
DE.data <- IndDemos %>% filter(MPAID<7)
SE.data <- Settlements %>% filter(MPAID<7)

# ---
# 1.4 Import lookup tables
ethnic.lkp<- import("x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx")
education.lkp <- import("x_Flat_data_files/1_Social/Inputs/education_lkp_BHS.xlsx")


# ----SECTION 2: Pre-process matching covariates ----

# ---
# 2.1 Run data prep function
match.covariate <- process_covariates(HH.data, DE.data)

# --- pscore calculations
#baseline 
cov.t0 <- filter(match.covariate,MonitoringYear=="Baseline")
p.score.t0 <- glm(Treatment~ TimeMarket + n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge, 
                   data=cov.t0, family= binomial())$fitted.values
cov.t0.pscore <- cbind(cov.t0,p.score.t0)

#t2
cov.t2 <- filter(match.covariate,MonitoringYear=="2 Year Post")
p.score.t2 <- glm(Treatment~ TimeMarket + n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge, 
                  data=cov.t2, family= binomial())$fitted.values
cov.t2.pscore <- cbind(cov.t2,p.score.t2)

#t4
cov.t4 <- filter(match.covariate,MonitoringYear=="4 Year Post")
p.score.t4 <- glm(Treatment~ TimeMarket + n.child + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge, 
                  data=cov.t4, family= binomial())$fitted.values
cov.t4.pscore <- cbind(cov.t4,p.score.t4)

# get ranges
p.score.range <- 
  data.frame(year=c("t0","t2","t4"),
             minimums=c(range(p.score.t0)[1],
                        range(p.score.t2)[1],
                        range(p.score.t4)[1]),
             maximums=c(range(p.score.t0)[2],
                        range(p.score.t2)[2],
                        range(p.score.t4)[2])) %>%
  summarise(min=max(minimums),
            max=min(maximums))

p.score.sd <- sd((c(p.score.t0,p.score.t2,p.score.t4)))
p.score.range.sd <- range (p.score.range + (0.5 * p.score.sd),p.score.range - (0.5 * p.score.sd))

n.t0.tr.trim <- cov.t0.pscore %>% 
  filter(p.score.t0>=p.score.range$min & p.score.t0<=p.score.range$max)

n.t2.tr.trim <- cov.t2.pscore %>% 
  filter(p.score.t2>=p.score.range$min & p.score.t2<=p.score.range$max)

n.t4.tr.trim <- cov.t4.pscore %>% 
  filter(p.score.t4>=p.score.range$min & p.score.t4<=p.score.range$max)

nrow(cov.t0)
nrow(n.t0.tr.trim)

nrow(cov.t2)
nrow(n.t2.tr.trim)

nrow(cov.t4)
nrow(n.t4.tr.trim)


  length(cov.t4$HouseholdID[cov.t4$Treatment==0])/length(cov.t4$HouseholdID)
  length(n.t4.tr.trim$HouseholdID[n.t4.tr.trim$Treatment==0])/length(n.t4.tr.trim$HouseholdID)

  length(cov.t2$HouseholdID[cov.t2$Treatment==0])/length(cov.t2$HouseholdID)
  length(n.t2.tr.trim$HouseholdID[n.t2.tr.trim$Treatment==0])/length(n.t2.tr.trim$HouseholdID)
  
  length(cov.t0$HouseholdID[cov.t0$Treatment==0])/length(cov.t0$HouseholdID)
  length(n.t0.tr.trim$HouseholdID[n.t0.tr.trim$Treatment==0])/length(n.t0.tr.trim$HouseholdID)

  match.covariate[match.covariate$MPAID==3 & match.covariate$Treatment==0,]
  HHData[HHData$MPAID==3 & HHData$Treatment==0,]
  
  nrows.perMPA <- data.frame(MPAID=c(1:6),
                             nrows.pretrim.t0=NA,
                             nrows.posttrim.t0=NA,
                             prop.control.pretrim.t0=NA,
                             prop.control.posttrim.t0=NA,
                             nrows.pretrim.t2=NA,
                             nrows.posttrim.t2=NA,
                             prop.control.pretrim.t2=NA,
                             prop.control.posttrim.t2=NA,
                             nrows.pretrim.t4=NA,
                             nrows.posttrim.t4=NA,
                             prop.control.pretrim.t4=NA,
                             prop.control.posttrim.t4=NA)
  
  for(i in unique(nrows.perMPA$MPAID)) {
    
    t0.pretrim.data <- cov.t0[cov.t0$MPAID==i,]
    t0.posttrim.data <- n.t0.tr.trim[n.t0.tr.trim$MPAID==i,]
    
    t0.pretrim.control <- cov.t0[cov.t0$MPAID==i & cov.t0$Treatment==0,]
    t0.posttrim.control <- n.t0.tr.trim[n.t0.tr.trim$MPAID==i & n.t0.tr.trim$Treatment==0,]
    
    t2.pretrim.data <- cov.t2[cov.t2$MPAID==i,]
    t2.posttrim.data <- n.t2.tr.trim[n.t2.tr.trim$MPAID==i,]
    
    t2.pretrim.control <- cov.t2[cov.t2$MPAID==i & cov.t2$Treatment==0,]
    t2.posttrim.control <- n.t2.tr.trim[n.t2.tr.trim$MPAID==i & n.t2.tr.trim$Treatment==0,]
    
    t4.pretrim.data <- cov.t4[cov.t4$MPAID==i,]
    t4.posttrim.data <- n.t4.tr.trim[n.t4.tr.trim$MPAID==i,]
    
    t4.pretrim.control <- cov.t4[cov.t4$MPAID==i & cov.t4$Treatment==0,]
    t4.posttrim.control <- n.t4.tr.trim[n.t4.tr.trim$MPAID==i & n.t4.tr.trim$Treatment==0,]
    
    nrows.perMPA$nrows.pretrim.t0[i] <- nrow(t0.pretrim.data)
    nrows.perMPA$nrows.posttrim.t0[i] <- nrow(t0.posttrim.data)
    nrows.perMPA$prop.control.pretrim.t0[i] <- nrow(t0.pretrim.control)/nrow(t0.pretrim.data)
    nrows.perMPA$prop.control.posttrim.t0[i] <- nrow(t0.posttrim.control)/nrow(t0.posttrim.data)
    nrows.perMPA$nrows.pretrim.t2[i] <- nrow(t2.pretrim.data)
    nrows.perMPA$nrows.posttrim.t2[i] <- nrow(t2.posttrim.data)
    nrows.perMPA$prop.control.pretrim.t2[i] <- nrow(t2.pretrim.control)/nrow(t2.pretrim.data)
    nrows.perMPA$prop.control.posttrim.t2[i] <- nrow(t2.posttrim.control)/nrow(t2.posttrim.data)
    nrows.perMPA$nrows.pretrim.t4[i] <- nrow(t4.pretrim.data)
    nrows.perMPA$nrows.posttrim.t4[i] <- nrow(t4.posttrim.data)
    nrows.perMPA$prop.control.pretrim.t4[i] <- nrow(t4.pretrim.control)/nrow(t4.pretrim.data)
    nrows.perMPA$prop.control.posttrim.t4[i] <- nrow(t4.posttrim.control)/nrow(t4.posttrim.data)
  }
  
  
#--- 
# 2.2 Subset to t0.t4.data
t0.t4.match.covariate <- subset(match.covariate, (match.covariate$MonitoringYear=="t0"|match.covariate$MonitoringYear=="t4") & Treatment==1)
t0.t4.match.covariate$MonitoringYear <- as.factor(recode(t0.t4.match.covariate$MonitoringYear, "t0"="0","t4"="1"))

#---
# 2.3 Extract non-longitudinal households
non.longitudinal.pairs <-subset(t0.t4.match.covariate, t0.t4.match.covariate$t0.t4.pair==99999)
non.longitudinal.pairs <- as.data.frame(non.longitudinal.pairs)
row.names(non.longitudinal.pairs) <- non.longitudinal.pairs$HouseholdID

non.longitudinal.pairs["8753","IndividualGender"] <- 0
non.longitudinal.pairs["8753","ed.level"] <- 0
non.longitudinal.pairs["8753","IndividualAge"] <- 5

non.longitudinal.pairs$YearsResident[is.na(non.longitudinal.pairs$YearsResident)] <- 1

# ---- Section 3: Match Tr1t4 -> Tr1t0 (time)

# ---
# 2.5 Create X
X <- subset (non.longitudinal.pairs, select=c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket"))
X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")] <- as.numeric(as.character(unlist(X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")])))

# ======
# 5.10 Extract Tr
Tr.longitudinal<-as.vector(as.factor(non.longitudinal.pairs$MonitoringYear))

# ======
# 5.11  Extract MatchBy factor (grouping factor)
MPAID<-as.numeric(as.character(non.longitudinal.pairs$MPAID))

# ======
# 3.2 Set variable states for MatchBalance routine
#non.longitudinal.pairs[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")] <- as.factor(as.character(unlist(X[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")])))

#---- Section 3: Propensity score matching (t0.t4, non.longitudinal.pairs)----

# ---
# 3.1 Compute propensity score
psm.time <- glm (MonitoringYear~ TimeMarket + dom.eth + n.child + YearsResident+IndividualGender+ed.level+IndividualAge, data=non.longitudinal.pairs, family= binomial())
ps <-psm.time$fitted.values
X <- cbind(X, ps)
rm(ps)

#---
# 3.2 Propensity score matching
caliper <-c(100,100,100,100,100,100,100,100)
m1<-Matchby(Y=NULL,Tr=Tr.longitudinal,X=X,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE, caliper=caliper)

#---
# 3.3 Covariate balance
mb1 <- MatchBalance(Tr.longitudinal~ MPAID+TimeMarket+ dom.eth+n.child+IndividualGender +ed.level+YearsResident + IndividualAge, match.out=m1, data=non.longitudinal.pairs,ks=TRUE, nboots=100, digits=3, paired=TRUE)
t4.mb.time.out <- MatchBal.unlist(mb1,psm.time)
# ---
# 3.4 Create matched HouseholdID pairs
i.treat<-m1$index.treated
i.control<-m1$index.control
HouseholdID.t4<-non.longitudinal.pairs[i.treat,'HouseholdID']
HouseholdID.t0<-non.longitudinal.pairs [i.control,'HouseholdID']
non.longitudinal.pairs<-data.frame(cbind(HouseholdID.t4,HouseholdID.t0))
colnames(non.longitudinal.pairs)<-c("HouseholdID.tr1.t4", "HouseholdID.tr1.t0")

rm(i.treat,i.control,HouseholdID.t4,HouseholdID.t0)

# ---
# 3.6 Extract longitudinal pairs
longitudinal.pairs <- subset (t0.t4.match.covariate, select=c("HouseholdID", "MonitoringYear", "t0.t4.pair"), t0.t4.match.covariate$t0.t4.pair!=99999)
longitudinal.pairs.t0 <- subset(longitudinal.pairs, MonitoringYear==0)
longitudinal.pairs.t4 <- subset(longitudinal.pairs, MonitoringYear==1)
longitudinal.pairs <- left_join(longitudinal.pairs.t0,longitudinal.pairs.t4, by="t0.t4.pair")
longitudinal.pairs<-subset(longitudinal.pairs, select=c("HouseholdID.y","HouseholdID.x"))
colnames (longitudinal.pairs) <- c("HouseholdID.tr1.t4", "HouseholdID.tr1.t0")

#---
# 3.7 Combine longitudinal and non longitudinal pairs
time.pairs <- rbind(non.longitudinal.pairs,longitudinal.pairs)


#---
#3.8 Compute covariate balance for all time.pairs (both longitudinal and non longitudinal)
#time.pairs$index <-seq.int(1:nrow(time.pairs))
#exact.match <- c(1)
#dummy.time.match <- Match(Y=NULL, X=X, Tr=Tr,M=1, exact=exact.match)
#MPA.time.pairs.match$years.resident.longitudinal <- as.factor(MPA.time.pairs.match$years.resident.longitudinal)
#MPA.time.pairs.match$individual.age.longitudinal <-as.factor(MPA.time.pairs.match$individual.age.longitudinal)
#dummy.balance <- MatchBalance(Tr~ SettlementID+ dom.eth+n.child+occ.dependence+individual.gender +ed.level+years.resident.longitudinal + individual.age.longitudinal, match.out=dummy.time.match, data=MPA.time.pairs.match,ks=TRUE, nboots=100, digits=3, paired=TRUE)

# ---- Section 4: Propensity score matching Tr1t4 - Tr0t4 (cross-treatment match) ----

#--
# 4.1 Subset data
t4.covariate <- data.frame(subset(match.covariate, MonitoringYear=="t4"))

# ---
# 4.2 Recode Kaimana controls
#t4.covariate <- t4.covariate %>% mutate_at(.vars = c("Treatment"), funs(ifelse(SettlementID.x == 83 |SettlementID.x == 91 |SettlementID.x == 92, 1, .)))
# ---
#4.3 Row names
# Remove erroneous pairs  <----- HERE GO BACK TO PAIRS CODE AND FORCE OUT


t4.covariate <- subset(t4.covariate,(t4.covariate$t0.t4.pair!=604 &
                                       t4.covariate$t0.t4.pair!=670 &
                                       t4.covariate$t0.t4.pair!=671 &
                                       t4.covariate$t0.t4.pair!=90 &
                                       t4.covariate$t0.t4.pair!=375 &
                                       t4.covariate$t0.t4.pair!=374 &
                                       t4.covariate$t0.t4.pair!=213 &
                                       t4.covariate$t0.t4.pair!=661 &
                                       t4.covariate$t0.t4.pair!=663))

t4.covariate ["2107", "HouseholdID"]<-20001

row.names(t4.covariate) <-t4.covariate$HouseholdID

t4.covariate$YearsResident[is.na(t4.covariate$YearsResident)] <- 1
 
t4.covariate[ "8753", "IndividualGender"] <- 0
t4.covariate[ "8753", "ed.level"] <- 0
t4.covariate[ "8753", "IndividualAge"] <- 5

# ---
# 4.4 Create X
X <- subset (t4.covariate, select=c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket"))
X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")] <- as.numeric(as.character(unlist(X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")])))

# ---
# 4.5 Extract Tr
Tr <- t4.covariate$Treatment

#temp <- t4.covariate$Treatment
#temp2[which(temp=='2')] <- 'T'
#temp2[is.na(temp2)] <- 'F'
#Tr<- as.logical(temp2)


# ---
# 4.6 Extract MatchBy factor (grouping factor)
MPAID<-as.numeric(t4.covariate$MPAID)

# ======
# 4.7 Set variable states for MatchBalance routine
#non.longitudinal.pairs[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")] <- as.factor(as.character(unlist(X[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")])))


# ---
# 4.8 Compute propensity score
#t4.covariate$Treatment <-as.numeric(as.character(t4.covariate$Treatment))
#t4.covariate$Treatment<-as.factor(recode(t4.covariate$Treatment,'1'='0', '2'='1'))
t4.covariate$Treatment <-as.factor(t4.covariate$Treatment)
psm.treatment <- glm (Treatment~ TimeMarket + dom.eth + n.child + YearsResident+IndividualGender+ed.level+IndividualAge, data=t4.covariate, family= binomial())
ps <-psm.treatment$fitted.values
X <- cbind(X, ps)
rm(ps)

#---
# 4.9 Propensity score matching
caliper <-c(100,100,100,100,100,100,100,0.5)
m1<-Matchby(Y=NULL,Tr=Tr,X=X,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE, caliper=NULL)

#---
# 4.10 Covariate balance
t4.covariate$Treatment <- as.numeric(as.character(t4.covariate$Treatment))
mb1 <- MatchBalance(Treatment~ MPAID+TimeMarket+ dom.eth+n.child+IndividualGender +ed.level+YearsResident + IndividualAge, match.out=m1, data=t4.covariate,ks=TRUE, nboots=100, digits=3, paired=TRUE)
t4.mb.treatment.out <- MatchBal.unlist(mb1,psm.treatment)
#---
# 4.11 Extract dropped cases

#no droppped cases

#---
# 4.12 
i.treat<-m1$index.treated
i.control<-m1$index.control
HouseholdID.t4<-t4.covariate[i.treat,'HouseholdID']
HouseholdID.t0<-t4.covariate [i.control,'HouseholdID']
treatment.pairs<-data.frame(cbind(HouseholdID.t4,HouseholdID.t0))
colnames(treatment.pairs)<-c("HouseholdID.tr1.t4", "HouseholdID.tr0.t4")


#---- Section 5: Propensity score matching Tr1t4 - Tr0t0 (interaction pairs)

#--
# 5.1 Subset data
interaction.covariate <- subset(match.covariate, ((MonitoringYear=="t4" & Treatment ==1)|(MonitoringYear=="t0" & Treatment ==0)))

#--
# 5.2 Recode Kaimana controls
#interaction.covariate <- interaction.covariate %>% 
#  mutate_at(.vars = c("Treatment"), funs(ifelse(SettlementID.x == 83 |SettlementID.x == 91 |SettlementID.x == 92, 1, .)))

#--
#5.3 Row names

interaction.covariate <- subset(interaction.covariate,(interaction.covariate$t0.t4.pair!=604 &
                                       interaction.covariate$t0.t4.pair!=670 &
                                       interaction.covariate$t0.t4.pair!=671 &
                                       interaction.covariate$t0.t4.pair!=90 &
                                       interaction.covariate$t0.t4.pair!=375 &
                                       interaction.covariate$t0.t4.pair!=374 &
                                       interaction.covariate$t0.t4.pair!=213 &
                                       interaction.covariate$t0.t4.pair!=661 &
                                       interaction.covariate$t0.t4.pair!=663))

interaction.covariate ["2328", "HouseholdID"]<-20000

interaction.covariate <- data.frame(interaction.covariate)
row.names(interaction.covariate) <-interaction.covariate$HouseholdID

interaction.covariate$YearsResident[is.na(interaction.covariate$YearsResident)] <- 1

interaction.covariate[ "553", "IndividualGender"] <- 0
interaction.covariate[ "553", "ed.level"] <- 0
interaction.covariate[ "553", "IndividualAge"] <- 5

interaction.covariate[ "8753", "IndividualGender"] <- 0
interaction.covariate[ "8753", "ed.level"] <- 0
interaction.covariate[ "8753", "IndividualAge"] <- 5

# ======
# 5.4 Create X
X <- subset (interaction.covariate, select=c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket"))
X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")] <- as.numeric(as.character(unlist(X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")])))

# ======
# 5.5 Extract Tr
Tr<-as.vector(interaction.covariate$Treatment)

# ======
# 5.6 Extract MatchBy factor (grouping factor)
MPAID<-as.numeric(interaction.covariate$MPAID)

# ======
# 5.7 Set variable states for MatchBalance routine
#non.longitudinal.pairs[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")] <- as.factor(as.character(unlist(X[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")])))


# ---
# 5.8 Compute propensity score
interaction.covariate$Treatment <-as.factor(interaction.covariate$Treatment)
psm.interaction <- glm (Treatment~ TimeMarket + dom.eth + n.child + YearsResident+IndividualGender+ed.level+IndividualAge, data=interaction.covariate, family= binomial())
ps <-psm.interaction$fitted.values
X <- cbind(X, ps)
rm(ps)

#---
# 5.9 Propensity score matching
caliper <-c(100,100,100,100,100,100,100,100)
m1<-Matchby(Y=NULL,Tr=Tr,X=X,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE, caliper=NULL)

#---
# 5.10 Covariate balance
interaction.covariate$Treatment <- as.numeric(as.character(interaction.covariate$Treatment))
mb1 <- MatchBalance(Treatment~ MPAID+TimeMarket+ dom.eth+n.child+IndividualGender +ed.level+YearsResident + IndividualAge, match.out=m1, data=interaction.covariate,ks=TRUE, nboots=100, digits=3, paired=TRUE)
t4.mb.interaction.out <- MatchBal.unlist(mb1,psm.interaction)
#---
# 5.11 Extract dropped cases

#no droppped cases

#---
# 5.12  Create interaction term pairs
i.treat<-m1$index.treated
i.control<-m1$index.control
HouseholdID.tr1.t4<-interaction.covariate[i.treat,'HouseholdID']
HouseholdID.tr0.t0<-interaction.covariate [i.control,'HouseholdID']
interaction.pairs<-data.frame(cbind(HouseholdID.tr1.t4,HouseholdID.tr0.t0))
colnames(interaction.pairs)<-c("HouseholdID.tr1.t4", "HouseholdID.tr0.t0")

rm(i.treat, i.control, HouseholdID.tr0.t0, HouseholdID.tr1.t4, m1, mb1, caliper, X)

#--- Section 6 Combine pairs into master dataframe ----

# 6.1 Compute outcome variables 

# Big Five

hfs.outcome.t4 <- outcome_ATT_method1(pairs=master.t4.A ,outcomes=BigFive,var=FSIndex)
asset.outcome.t4 <- outcome_ATT_method1(pairs=master.t4.A ,outcomes=BigFive,var=MAIndex)
tenure.outcome.t4 <- outcome_ATT_method1(pairs=master.t4.A ,outcomes=BigFive,var=MTIndex)
attach.outcome.t4 <- outcome_ATT_method1(pairs=master.t4.A ,outcomes=BigFive,var=PAIndex)
enrol.outcome.t4 <- outcome_ATT_method1(pairs=master.t4.A ,outcomes=BigFive,var=SERate)

#Middle Ten
chfs.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = CFSIndex.inv)
access.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = Acc.Harv)
manage.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = Man.Excl.Trans)
OD.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = ODIndex)
MP.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = MarineGroup)                                     #
OP.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = OtherGroup)
econ.decline.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = EconDecline)
econ.stable.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = EconStable)
econ.increase.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = EconIncrease)
#con.decrease.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = CFSIndex.inv)
#con.stable.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = CFSIndex.inv)
#con.increase.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = CFSIndex.inv)
morbidity.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = DaysUnwell)
f.enrol.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = FemaleSERate)
m.enrol.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = MaleSERate)
attain.4.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = Attain.4)
attain.5.outcome.t4 <- outcome_ATT_method1(pairs = master.t4.A, outcomes = Middle15, var = Attain.5)

# 6.2 Seascape-wide treatment effects
# Big Five
hfs.sig.t4 <- att.significance(outcomes=hfs.outcome.t4, weights=(rep_len(1,nrow(hfs.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
asset.sig.t4 <- att.significance(outcomes=asset.outcome.t4, weights=(rep_len(1,nrow(asset.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
tenure.sig.t4 <- att.significance(outcomes=tenure.outcome.t4, weights=(rep_len(1,nrow(tenure.outcome.t4))),conf=0.95, sig.test=TRUE,p.value = 0.05)
attach.sig.t4 <- att.significance(outcomes=attach.outcome.t4, weights=(rep_len(1,nrow(attach.outcome.t4))),conf=0.95, sig.test=TRUE,p.value = 0.05)
enrol.sig.t4 <- att.significance(outcomes=enrol.outcome.t4, weights=(rep_len(1,nrow(enrol.outcome.t4))),conf=0.95, sig.test=TRUE,p.value = 0.05)

# Middle Ten
chfs.sig <- att.significance(outcomes=chfs.outcome.t4, weights=(rep_len(1,nrow(chfs.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
access.sig <- att.significance(outcomes=access.outcome.t4, weights=(rep_len(1,nrow(access.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
manage.sig <- att.significance(outcomes=manage.outcome.t4, weights=(rep_len(1,nrow(manage.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
OD.sig <- att.significance(outcomes=OD.outcome.t4, weights=(rep_len(1,nrow(OD.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
MP.sig <- att.significance(outcomes=MP.outcome.t4, weights=(rep_len(1,nrow(MP.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
OP.sig <- att.significance(outcomes=OP.outcome.t4, weights=(rep_len(1,nrow(OP.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
econ.decline.sig <- att.significance(outcomes=econ.decline.outcome.t4, weights=(rep_len(1,nrow(econ.decline.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
econ.stable.sig <- att.significance(outcomes=econ.stable.outcome.t4, weights=(rep_len(1,nrow(econ.stable.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
econ.increase.sig <- att.significance(outcomes=econ.increase.outcome.t4, weights=(rep_len(1,nrow(econ.increase.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
morbidity.sig <- att.significance(outcomes=morbidity.outcome.t4, weights=(rep_len(1,nrow(morbidity.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
f.enrol.sig <- att.significance(outcomes=f.enrol.outcome.t4, weights=(rep_len(1,nrow(f.enrol.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
m.enrol.sig <- att.significance(outcomes=m.enrol.outcome.t4, weights=(rep_len(1,nrow(m.enrol.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
attain.4.sig <- att.significance(outcomes=attain.4.outcome.t4, weights=(rep_len(1,nrow(attain.4.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)
attain.5.sig <- att.significance(outcomes=attain.5.outcome.t4, weights=(rep_len(1,nrow(attain.5.outcome.t4))),conf=0.95,sig.test=TRUE,p.value = 0.05)

# 6.3 Create matrix for all Seascape-wide treatment effects
ATT.Seascape.BigFive.t4 <- rbind(hfs.sig.t4,asset.sig.t4,tenure.sig.t4,attach.sig.t4,enrol.sig.t4)
row.names(ATT.Seascape.BigFive.t4) <-c("hfs", "asset","tenure","attach","enrol")

ATT.Seascape.Middle10.t4 <-rbind(chfs.sig, access.sig, manage.sig,OD.sig, MP.sig, OP.sig,econ.decline.sig, econ.stable.sig,econ.increase.sig,morbidity.sig, f.enrol.sig, m.enrol.sig,attain.4.sig,attain.5.sig)
row.names(ATT.Seascape.Middle10.t4) <-c("chfs", "access", "manage", "occ.dependence", "marine.groups","other.groups", "econ.decline","econ.stable", "econ.increase", "morbidity", "female.enrol", "male.enrol","attain4","attain5")


# 6.4 MPA-specific treatment effects
hfs.sig.byMPA.t4 <- att.significance.by.group(outcomes=hfs.outcome.t4,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
asset.sig.byMPA.t4 <- att.significance.by.group(outcomes=asset.outcome.t4,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
tenure.sig.byMPA.t4 <- att.significance.by.group(outcomes=tenure.outcome.t4,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
attach.sig.byMPA.t4 <- att.significance.by.group(outcomes=attach.outcome.t4,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
enrol.sig.byMPA.t4 <- att.significance.by.group(outcomes=enrol.outcome.t4,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)



#--- Section 8: Export objects ----

# ---
# 8.1  Export matched objects
write.xlsx(master.t4.A , "master_t4_panelA.xlsx")

# ---
# 8.2 Export matched objects with outcome
list_of_datasets <- list("hfs_outcome_t4" = hfs.outcome.t4, "asset_outcome_t4" = asset.outcome.t4, "tenure_outcome_t4" = tenure.outcome.t4, "enrol_outcome.t4"=enrol.outcome.t4, "attach_outcome_t4"=attach.outcome.t4)
write.xlsx(list_of_datasets, file = "BigFive_panel_outcomes_t4.xlsx")

# ---
# 8.3 Export sig tests
list_of_datasets<-list("Seascape_ATT" = ATT.Seascape.BigFive.t4,"hfs_by_MPA"= hfs.sig,"asset_by_MPA" = asset.sig,"tenure_by_MPA" = tenure.sig, "attach_by_MPA" = attach.sig,"enrol_by_MPA" = enrol.sig)
write.xlsx(list_of_datasets, file = "BigFive_panel_sig_t4.xlsx", row.names=TRUE)

# ---
# 8.4 Export match balance object
list_of_datasets <- list("t4_time" =t4.mb.time.out, "t4_treatment"= t4.mb.treatment.out, "t4_interaction" = t4.mb.interaction.out)
write.xlsx(list_of_datasets, file = "t4_panelA_matchbalance.xlsx", row.names=TRUE)


View(hfs.outcome.t2.subgroup)
