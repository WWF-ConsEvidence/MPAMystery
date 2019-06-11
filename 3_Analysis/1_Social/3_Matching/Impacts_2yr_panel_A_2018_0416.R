# ---
# code:  Short-term social impacts of marine protected areas -BHS  (Fake the panel A)
# author: Louise Glew, louise.glew@gmail.com
# created: May 2016
# modified: July 2015

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
pacman::p_load(twang,tm,NLP,readxl,cobalt,CBPS,Matching,optmatch,tidyr,RItools,Hmisc,MBESS,rbounds,reshape2,Matching,dplyr)

# ---
# 1.2 
source(source('~/Social_impacts_2017_1230/function_process_covariates.R'))
source('~/Dropbox/CLEAN_start/MatchBal.unlist.function.R')

# ---
# 1.3 Import raw data
HH.data <- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/HH_tbl_WELLBEING.xlsx") #redirect to final file destination
DE.data <- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/HH_tbl_DEMOGRAPHIC.xlsx") #redirect to final file destination
SE.data<- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/HH_tbl_SETTLEMENT.xlsx")

# ---
# 1.4 Import lookup tables
ethnic.lkp<- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/master_ethnic_lookup_2017_117.xlsx")
education_lkp <- read_excel("LKP/education_lkp.xlsx")


# ----SECTION 2: Pre-process matching covariates ----

# ---
# 2.1 Run data prep function
match.covariate <- process_covariates(HH.data, DE.data,t0.t2.pairs, t0.t4.pairs)

#--- 
# 2.2 Subset to t0.t2.data
t0.t2.match.covariate <- subset(match.covariate, (match.covariate$MonitoringYear=="t0"|match.covariate$MonitoringYear=="t2")&(Treatment==1))

#---
#2.3 Row names
non.longitudinal.pairs <-subset(t0.t2.match.covariate, t0.t2.match.covariate$t0.t2.pair==99999)
non.longitudinal.pairs$t0.t4.pair<-NULL
non.longitudinal.pairs <- unique(as.data.frame(non.longitudinal.pairs))
row.names(non.longitudinal.pairs) <- non.longitudinal.pairs$HouseholdID

#non.longitudinal.pairs["553","IndividualGender"] <- 0
#non.longitudinal.pairs["553","ed.level"] <- 0
#non.longitudinal.pairs["553","IndividualAge"] <- 5

#non.longitudinal.pairs["4032","n.child"] <- 3
non.longitudinal.pairs["4563","n.child"] <- 4

non.longitudinal.pairs$YearsResident[is.na(non.longitudinal.pairs$YearsResident)] <- 1

#---- SECTION 3:  Match Tr1t2 -> Tr1t0 (time)

# ----
# 3.1  Create X
X <- subset (non.longitudinal.pairs, select=c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket"))
X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")] <- as.numeric(as.character(unlist(X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")])))

# ---
# 3.2 Extract Tr
non.longitudinal.pairs$MonitoringYear <-as.factor(recode(non.longitudinal.pairs$MonitoringYear,"t0"="0","t2"="1","t4"="2"))
Tr.longitudinal<-as.vector(non.longitudinal.pairs$MonitoringYear)

# ======
# 3.3 Extract MatchBy factor (grouping factor)
MPAID<-as.numeric(non.longitudinal.pairs$MPAID)

# ======
# 3.2 Set variable states for MatchBalance routine
#non.longitudinal.pairs[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")] <- as.factor(as.character(unlist(X[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")])))



# ---
# 3.4 Compute propensity score
psm.time <- glm (MonitoringYear~ TimeMarket + dom.eth + n.child + YearsResident+IndividualGender+ed.level+IndividualAge, data=non.longitudinal.pairs, family= binomial())
ps <-psm.time$fitted.values
X <- cbind(X, ps)
rm(ps)

#---
# 3.5 Propensity score matching
caliper <-c(100,1.0,100,1.0,100,1.0,100,100)
m1<-Matchby(Y=NULL,Tr=Tr.longitudinal,X=X,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE, caliper=caliper)

#---
# 3.3 Covariate balance
mb1 <- MatchBalance(Tr.longitudinal~ MPAID+TimeMarket+ dom.eth+n.child+IndividualGender +ed.level+YearsResident + IndividualAge, match.out=m1, data=non.longitudinal.pairs,ks=TRUE, nboots=100, digits=3, paired=TRUE)
t2.mb.time.out <- MatchBal.unlist(mb1,psm.time)
# ---
# 3.4 Create matched HouseholdID pairs
i.treat<-m1$index.treated
i.control<-m1$index.control
HouseholdID.t2<-non.longitudinal.pairs[i.treat,'HouseholdID']
HouseholdID.t0<-non.longitudinal.pairs [i.control,'HouseholdID']
non.longitudinal.pairs<-data.frame(cbind(HouseholdID.t2,HouseholdID.t0))
colnames(non.longitudinal.pairs)<-c("HouseholdID.tr1.t2", "HouseholdID.tr1.t0")

rm(i.treat,i.control,HouseholdID.t4,HouseholdID.t0)

# ---
# 3.6 Extract longitudinal pairs
longitudinal.pairs <- subset (t0.t2.match.covariate, select=c("HouseholdID", "MonitoringYear", "t0.t2.pair"), t0.t2.match.covariate$t0.t2.pair!=99999)
longitudinal.pairs.t0 <- subset(longitudinal.pairs, MonitoringYear=="t0")
longitudinal.pairs.t2 <- subset(longitudinal.pairs, MonitoringYear=="t2")
longitudinal.pairs <- left_join(longitudinal.pairs.t0,longitudinal.pairs.t2, by="t0.t2.pair")
longitudinal.pairs<-subset(longitudinal.pairs, select=c("HouseholdID.y","HouseholdID.x"))
colnames (longitudinal.pairs) <- c("HouseholdID.tr1.t2", "HouseholdID.tr1.t0")

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

#---- Section 4: Propensity score matching Tr1t2 - Tr0t2

t2.covariate <- data.frame(subset(match.covariate, MonitoringYear=="t2"))

# ---
# 4.1 Recode Kaimana controls

#t2.covariate <- t2.covariate %>% mutate_at(.vars = c("Treatment"), funs(ifelse(SettlementID.x == 83 |SettlementID.x == 91 |SettlementID.x == 92, 1, .)))


# ---
#4.3 Row names
# Remove erroneous pairs  <----- HERE GO BACK TO PAIRS CODE AND FORCE OUT
t2.covariate <- subset(t2.covariate,(t2.covariate$t0.t2.pair!=23 &
                                       t2.covariate$t0.t2.pair!=291 &
                                       t2.covariate$t0.t2.pair!=554 &
                                       t2.covariate$t0.t2.pair!=597 &
                                       t2.covariate$t0.t2.pair!=831 &
                                       t2.covariate$t0.t2.pair!=805 ))
t2.covariate ["674", "HouseholdID"]<-20000 # temp recode HouseholdID 3149 to unique HH ID for match. 
t2.covariate ["701", "HouseholdID"]<-20001 # temp recode HouseholdID 3175 to unique HH ID for match. 
t2.covariate <- data.frame(t2.covariate)
row.names(t2.covariate) <- t2.covariate$HouseholdID

t2.covariate["4761","IndividualGender"] <- 0
t2.covariate["4761","ed.level"] <- 4
t2.covariate["4761","IndividualAge"] <- 4


t2.covariate["4032","n.child"] <- 3
t2.covariate["4563","n.child"] <- 4

t2.covariate$YearsResident[is.na(t2.covariate$YearsResident)] <- 1

t2.covariate$Treatment <- as.factor(as.character(t2.covariate$Treatment))




# ======
# 4.4 Create X
X <- subset (t2.covariate, select=c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket"))
X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")] <- as.numeric(as.character(unlist(X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident","IndividualAge","TimeMarket")])))

# ======
# 4.5 Extract Tr
Tr<-as.numeric(as.character(t2.covariate$Treatment))

# ======
# 4.6 Extract MatchBy factor (grouping factor)
MPAID<-as.numeric(t2.covariate$MPAID)

# ======
# 4.7 Set variable states for MatchBalance routine
#non.longitudinal.pairs[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")] <- as.factor(as.character(unlist(X[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")])))


# ---
# 4.8 Compute propensity score
psm.treatment <- glm (Treatment~ TimeMarket + dom.eth + n.child + YearsResident+IndividualGender+ed.level+IndividualAge, data=t2.covariate, family= binomial())
ps <-psm.treatment$fitted.values
X <- cbind(X, ps)
rm(ps)

#---
# 4.9 Propensity score matching
caliper <-c(100,100,100,100,100,100,100,0.5)
#Tr<-as.numeric(as.character(t2.covariate$Treatment))
m1<-Matchby(Y=NULL,Tr=Tr,X=X,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE, caliper=NULL)

#---
# 4.10 Covariate balance

t2.covariate$Treatment <- as.numeric(as.character(t2.covariate$Treatment))
mb1 <- MatchBalance(Treatment~ MPAID+TimeMarket+ dom.eth+n.child+IndividualGender +ed.level+YearsResident + IndividualAge, match.out=m1, data=t2.covariate,ks=TRUE, nboots=100, digits=3, paired=TRUE)
t2.mb.treatment.out <- MatchBal.unlist(mb1,psm.treatment)
#---
# 4.11 Extract dropped cases

#no droppped cases

#---
# 4.12 
i.treat<-m1$index.treated
i.control<-m1$index.control
HouseholdID.t2<-t2.covariate[i.treat,'HouseholdID']
HouseholdID.t0<-t2.covariate [i.control,'HouseholdID']
treatment.pairs<-data.frame(cbind(HouseholdID.t2,HouseholdID.t0))
colnames(treatment.pairs)<-c("HouseholdID.tr1.t2", "HouseholdID.tr0.t2")

#---- Section 5: Propensity score matching Tr1t4 - Tr0t0

#--
# 5.1 Subset data
interaction.covariate <- subset(match.covariate, ((MonitoringYear=="t2" & Treatment ==1)|(MonitoringYear=="t0" & Treatment ==0)))

#--
# 5.2 Recode Kaimana controls
#interaction.covariate <- interaction.covariate %>% mutate_at(.vars = c("Treatment"), funs(ifelse(SettlementID.x == 83 |SettlementID.x == 91 |SettlementID.x == 92, 1, .)))
#interaction.covariate$Treatment <- as.factor(recode(interaction.covariate$Treatment, "1"="0", "2"="1"))

#--
#5.3 Row names
interaction.covariate <- subset(interaction.covariate,(interaction.covariate$t0.t2.pair!=23 &
                                       interaction.covariate$t0.t2.pair!=291 &
                                       interaction.covariate$t0.t2.pair!=554 &
                                       interaction.covariate$t0.t2.pair!=597 &
                                       interaction.covariate$t0.t2.pair!=831 &
                                       interaction.covariate$t0.t2.pair!=805 ))
interaction.covariate ["807", "HouseholdID"]<-20000 # temp recode HouseholdID 3149 to unique HH ID for match. 
interaction.covariate ["1421", "HouseholdID"]<-20001 # temp recode HouseholdID 3175 to unique HH ID for match. 
interaction.covariate ["702", "HouseholdID"]<-20002 # temp recode HouseholdID 3175 to unique HH ID for match.
interaction.covariate <- data.frame(interaction.covariate)
row.names(interaction.covariate) <- interaction.covariate$HouseholdID


interaction.covariate["553","IndividualGender"] <- 0
interaction.covariate["553","ed.level"] <- 0
interaction.covariate["553","IndividualAge"] <- 5

interaction.covariate["4563","n.child"] <- 4

interaction.covariate$YearsResident[is.na(interaction.covariate$YearsResident)] <- 1

interaction.covariate$Treatment <- as.factor(as.character(interaction.covariate$Treatment))

# ======
# 5.4 Create X
X <- subset (interaction.covariate, select=c("dom.eth","n.child","IndividualGender","ed.level","YearsResident", "IndividualAge","TimeMarket"))
X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident", "IndividualAge", "TimeMarket")] <- as.numeric(as.character(unlist(X[,c("dom.eth","n.child","IndividualGender","ed.level","YearsResident", "IndividualAge","TimeMarket")])))


# ======
# 5.5 Extract Tr

#temp <- interaction.covariate$Treatment
#temp2[which(temp=='0')] <- 'F'
#temp2[which(temp=='1')] <- 'T'

#Tr <- as.logical(temp2)
#interaction.covariate$Treatment <- Tr
Tr <-as.numeric(as.character(interaction.covariate$Treatment))
# ======
# 5.6 Extract MatchBy factor (grouping factor)
MPAID<-as.numeric(interaction.covariate$MPAID)

# ======
# 5.7 Set variable states for MatchBalance routine
#non.longitudinal.pairs[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")] <- as.factor(as.character(unlist(X[,c("dom.eth","IndividualGender","ed.level","YearsResident","IndividualAge")])))


# ---
# 5.8 Compute propensity score
psm.interaction <- glm (Treatment~ TimeMarket + dom.eth + n.child + YearsResident+IndividualGender+ed.level+IndividualAge, data=interaction.covariate, family= binomial())
ps <-psm.interaction$fitted.values
X <- cbind(X, ps)
rm(ps)


#---
# 5.9 Propensity score matching
caliper <-c(100,100,100,100,100,100,100,100)
m1<-Matchby(Y=NULL,Tr=Tr,X=X,by=MPAID, estimand="ATT", M=1, ties=TRUE, replace=TRUE, caliper=caliper)
summary(m1)
#---
# 5.10 Covariate balance
interaction.covariate$Treatment <-as.numeric(as.character(interaction.covariate$Treatment))
mb1 <- MatchBalance(Treatment~ MPAID+TimeMarket+ dom.eth+n.child+IndividualGender +ed.level+YearsResident + IndividualAge, match.out=m1, data=interaction.covariate,ks=TRUE, nboots=100, digits=3, paired=TRUE)
t2.mb.interaction.out <- MatchBal.unlist(mb1,psm.treatment)
#---
# 5.11 Extract dropped cases

#no droppped cases

#---
# 5.12  Create interaction term pairs
i.treat<-m1$index.treated
i.control<-m1$index.control
HouseholdID.tr1.t2<-interaction.covariate[i.treat,'HouseholdID']
HouseholdID.tr0.t0<-interaction.covariate [i.control,'HouseholdID']
interaction.pairs<-data.frame(cbind(HouseholdID.tr1.t2,HouseholdID.tr0.t0))
colnames(interaction.pairs)<-c("HouseholdID.tr1.t2", "HouseholdID.tr0.t0")

rm(i.treat, i.control, HouseholdID.tr0.t0, HouseholdID.tr1.t2, m1, mb1, caliper, X)

#--- Section 6 Combine pairs into master dataframe ----

#--
# 6.1 Combine into master dataframe
HouseholdID.index<-data.frame(t0.t2.match.covariate$HouseholdID)
master.t2.A<- left_join(time.pairs,treatment.pairs, by=("HouseholdID.t2"="HouseholdID.tr1.t2"))
master.t2.A <-left_join(master.t2.A,interaction.pairs, by=("HouseholdID.t2"="HouseholdID.tr1.t2"))



# ---- SECTION 6: Compute outcome variables for t0.t2 ----

# 6.1 Compute outcome variables 

# Big Five

hfs.outcome.t2 <- outcome_ATT_method1(pairs=master.t2.A ,outcomes=BigFive,var=FSIndex)
asset.outcome.t2 <- outcome_ATT_method1(pairs=master.t2.A ,outcomes=BigFive,var=MAIndex)
tenure.outcome.t2 <- outcome_ATT_method1(pairs=master.t2.A ,outcomes=BigFive,var=MTIndex)
attach.outcome.t2 <- outcome_ATT_method1(pairs=master.t2.A ,outcomes=BigFive,var=PAIndex)
enrol.outcome.t2 <- outcome_ATT_method1(pairs=master.t2.A ,outcomes=BigFive,var=SERate)

#Middle Ten
chfs.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = CFSIndex.inv)
access.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = Acc.Harv)
manage.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = Man.Excl.Trans)
OD.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = ODIndex)
MP.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = MarineGroup)                                     #
OP.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = OtherGroup)
econ.decline.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = EconDecline)
econ.stable.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = EconStable)
econ.increase.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = EconIncrease)
#con.decrease.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = CFSIndex.inv)
#con.stable.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = CFSIndex.inv)
#con.increase.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = CFSIndex.inv)
morbidity.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = DaysUnwell)
f.enrol.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = FemaleSERate)
m.enrol.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = MaleSERate)
attain.4.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = Attain.4)
attain.5.outcome.t2 <- outcome_ATT_method1(pairs = master.t2.A, outcomes = Middle15, var = Attain.5)

# 6.2 Seascape-wide treatment effects
# Big Five
hfs.sig.t2 <- att.significance(outcomes=hfs.outcome.t2, weights=(rep_len(1,nrow(hfs.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
asset.sig.t2 <- att.significance(outcomes=asset.outcome.t2, weights=(rep_len(1,nrow(asset.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
tenure.sig.t2 <- att.significance(outcomes=tenure.outcome.t2, weights=(rep_len(1,nrow(tenure.outcome.t2))),conf=0.95, sig.test=TRUE,p.value = 0.05)
attach.sig.t2 <- att.significance(outcomes=attach.outcome.t2, weights=(rep_len(1,nrow(attach.outcome.t2))),conf=0.95, sig.test=TRUE,p.value = 0.05)
enrol.sig.t2 <- att.significance(outcomes=enrol.outcome.t2, weights=(rep_len(1,nrow(enrol.outcome.t2))),conf=0.95, sig.test=TRUE,p.value = 0.05)

# Middle Ten
chfs.sig <- att.significance(outcomes=chfs.outcome.t2, weights=(rep_len(1,nrow(chfs.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
access.sig <- att.significance(outcomes=access.outcome.t2, weights=(rep_len(1,nrow(access.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
manage.sig <- att.significance(outcomes=manage.outcome.t2, weights=(rep_len(1,nrow(manage.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
OD.sig <- att.significance(outcomes=OD.outcome.t2, weights=(rep_len(1,nrow(OD.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
MP.sig <- att.significance(outcomes=MP.outcome.t2, weights=(rep_len(1,nrow(MP.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
OP.sig <- att.significance(outcomes=OP.outcome.t2, weights=(rep_len(1,nrow(OP.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
econ.decline.sig <- att.significance(outcomes=econ.decline.outcome.t2, weights=(rep_len(1,nrow(econ.decline.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
econ.stable.sig <- att.significance(outcomes=econ.stable.outcome.t2, weights=(rep_len(1,nrow(econ.stable.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
econ.increase.sig <- att.significance(outcomes=econ.increase.outcome.t2, weights=(rep_len(1,nrow(econ.increase.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
morbidity.sig <- att.significance(outcomes=morbidity.outcome.t2, weights=(rep_len(1,nrow(morbidity.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
f.enrol.sig <- att.significance(outcomes=f.enrol.outcome.t2, weights=(rep_len(1,nrow(f.enrol.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
m.enrol.sig <- att.significance(outcomes=m.enrol.outcome.t2, weights=(rep_len(1,nrow(m.enrol.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
attain.4.sig <- att.significance(outcomes=attain.4.outcome.t2, weights=(rep_len(1,nrow(attain.4.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)
attain.5.sig <- att.significance(outcomes=attain.5.outcome.t2, weights=(rep_len(1,nrow(attain.5.outcome.t2))),conf=0.95,sig.test=TRUE,p.value = 0.05)

# 6.3 Create matrix for all Seascape-wide treatment effects
ATT.Seascape.BigFive.t2 <- rbind(hfs.sig.t2,asset.sig.t2,tenure.sig.t2,attach.sig.t2,enrol.sig.t2)
row.names(ATT.Seascape.BigFive.t2) <-c("hfs", "asset","tenure","attach","enrol")

ATT.Seascape.Middle10.t2 <-rbind(chfs.sig, access.sig, manage.sig,OD.sig, MP.sig, OP.sig,econ.decline.sig, econ.stable.sig,econ.increase.sig,morbidity.sig, f.enrol.sig, m.enrol.sig,attain.4.sig,attain.5.sig)
row.names(ATT.Seascape.Middle10.t2) <-c("chfs", "access", "manage", "occ.dependence", "marine.groups","other.groups", "econ.decline","econ.stable", "econ.increase", "morbidity", "female.enrol", "male.enrol","attain4","attain5")

# 6.4 MPA-specific treatment effects

# Big Five
hfs.sig.byMPA.t2 <- att.significance.by.group(outcomes=hfs.outcome.t2,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
asset.sig.byMPA.t2 <- att.significance.by.group(outcomes=asset.outcome.t2,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
tenure.sig.byMPA.t2 <- att.significance.by.group(outcomes=tenure.outcome.t2,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
attach.sig.byMPA.t2 <- att.significance.by.group(outcomes=attach.outcome.t2,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)
enrol.sig.byMPA.t2 <- att.significance.by.group(outcomes=enrol.outcome.t2,HHData=BHS_HHData, grouping_var = "MPAID", weights=NULL, conf=0.95,sig.test=TRUE,p.value = 0.05)

#Middle Ten

#--- Section 8: Export objects ----

# ---
# 8.1  Export matched objects
write.xlsx(master.t2.A , "master_t2_panelA.xlsx")

# ---
# 8.2 Export matched objects with outcome
list_of_datasets <- list("hfs_outcome_t2" = hfs.outcome.t2, "asset_outcome_t2" = asset.outcome.t2, "tenure_outcome_t2" = tenure.outcome.t2, "enrol_outcome.t2"=enrol.outcome.t2, "attach_outcome_t2"=attach.outcome.t2)
write.xlsx(list_of_datasets, file = "BigFive_panel_outcomes_t2.xlsx")

# ---
# 8.3 Export sig tests
list_of_datasets<-list("Seascape_ATT" = ATT.Seascape.BigFive.t2,"hfs_by_MPA"= hfs.sig,"asset_by_MPA" = asset.sig,"tenure_by_MPA" = tenure.sig, "attach_by_MPA" = attach.sig,"enrol_by_MPA" = enrol.sig)
write.xlsx(list_of_datasets, file = "BigFive_panel_sig_t2.xlsx", row.names=TRUE)

list_of_datasets <- list("Seascape_Middle10" = ATT.Seascape.Middle10.t2)
write.xlsx(list_of_datasets, file = "Middle10_panel_sig_t2.xlsx", row.names=TRUE)

# ---
# 8.4 Export match balance object
list_of_datasets <- list("t2_time" =t2.mb.time.out, "t2_treatment"= t2.mb.treatment.out, "t2_interaction" = t2.mb.interaction.out)
write.xlsx(list_of_datasets, file = "t2_panelA_matchbalance.xlsx", row.names=TRUE)

