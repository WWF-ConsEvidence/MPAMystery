# ---
# code:  Causal mechanisms SEM 
# author: Louise Glew, louise.glew@gmail.com
# created: June 2016
# modified: 

# -----
# inputs: 

# -----
# outputs:


# -----
# Code sections
#  1) Call libraries and import raw data

# ================================================================================================
#
# SECTION 1: Call libraries, functions and import raw data
#
# ================================================================================================
# 1.1 Call in libraries
# 1.2 Import raw data

# -----
# 1.1 Call in libraries
library(plyr)
library(dplyr)
library(lavaan)

# -----
# 1.2 Source relevant R code 

source('~/Dropbox/DATA_ANALYSIS/5_SOCIAL_IMPACTS2YR/Variable_outcome.R')
# -----
# 1.2 Import raw.data
# 2 year impact data
HH.data <- read.delim("~/Dropbox/DATA_ANALYSIS/2_PROJECTS/1_COMPUTE_HWB_INDICATORS/Raw_data/HHdata_raw_2015_0704.txt")

# -----
# 2.1 Compute fish consumption indicator

fish.consume<-subset(HH.data, select=c("HouseholdID", "FreqEatFish"))
### Convert all blind codes to missing values. Note '994' in this context represents non-fishers.
is.na(fish.consume[,2])<-fish.consume[,2]>=995
is.na(fish.consume[,2])<-fish.consume[,2]==993
fish.consume[is.na(fish.consume)]<-990
fish.consume$fish.consume<-as.factor(fish.consume$FreqEatFish)
fish.consume$FreqEatFish<-NULL

# -----
# 2.2 Compute fish sale indicator
fish.freq.sale<-subset(HH.data, select=c("HouseholdID", "FreqSaleFish"))
### Convert all blind codes to missing values. Note '994' in this context represents non-fishers.
is.na(fish.freq.sale[,2])<-fish.freq.sale[,2]>=995
is.na(fish.freq.sale[,2])<-fish.freq.sale[,2]==993
fish.freq.sale[is.na(fish.freq.sale)]<-990
fish.freq.sale$FreqSaleFish<-as.factor(fish.freq.sale$FreqSaleFish)
fish.freq.sale$fish.freq.sale<-fish.freq.sale$FreqSaleFish
fish.freq.sale$FreqSaleFish<-NULL
fish.freq.sale$fish.freq.sale<-as.numeric(as.character(fish.freq.sale$fish.freq.sale))

# Pre-process data
fish.sale.outcome <- subset(fish.freq.sale, select=c("HouseholdID","fish.freq.sale"))
fish.sale.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
fish.sale.MPA.t2 <- left_join(fish.sale.MPA.t2,fish.sale.outcome,by=c("HouseholdID"))

# Time outcome (trend over time in MPA)
fish.sale.time <-Variable_outcome(MPA.time.pairs,fish.sale.outcome)

# Treatment outcome (across treatment and control)
fish.sale.tr <-Variable_outcome(MPA.rpt.xsection.pairs,fish.sale.outcome)

# Interaction outcome 
fish.sale.int <-Variable_outcome(MPA.int.xsection.pairs,fish.sale.outcome)

# Join three outcome sets
fish.sale.ATT <- left_join(fish.sale.time,fish.sale.tr, by="HouseholdID")
fish.sale.ATT <- left_join(fish.sale.ATT, fish.sale.int, by="HouseholdID")

# Join to MPA at t2 data
fish.sale.ATT <- left_join(fish.sale.ATT,fish.sale.MPA.t2, by="HouseholdID")
colnames(fish.sale.ATT) <- c("HouseholdID","sale.mpa.t0","sale.c.t2","sale.c.t0", "sale.mpa.t2")

# Compute outcomes
fish.sale.ATT <- mutate(fish.sale.ATT, MPA.outcome=(sale.mpa.t2 -sale.mpa.t0))
fish.sale.ATT <- mutate(fish.sale.ATT, control.outcome=(sale.c.t2 -sale.c.t0))


# -----
# 2.3 Process household food security data
hfoodsec.sem <-rbind(hfoodsec.mpa, hfoodsec.control)
colnames(hfoodsec.sem)<-c("HouseholdID","hfs.outcome","MPAID","Tr")
# -----
# 2.4 Process marine tenure data
tenure.mpa <-subset(tenure.ATT,select=c("HouseholdID","MPA.outcome"))
tenure.mpa$Tr<-1
tenure.control <-subset(tenure.ATT,select=c("HouseholdID","control.outcome"))
tenure.control$Tr<-0
colnames(tenure.mpa)<-c("HouseholdID","tenure.outcome","Tr")
colnames(tenure.control)<-c("HouseholdID","tenure.outcome","Tr")
tenure.sem <-rbind(tenure.mpa, tenure.control)

HouseholdID <-as.numeric(unique(hfoodsec.sem$HouseholdID))
HouseholdID <-data.frame(HouseholdID)
test <-left_join(HouseholdID,tenure.sem)


# ================================================================================================
#
# SECTION 1: Structural equation model 
#
# ================================================================================================
sem.data <-read.csv("sem_data.csv", sep=",")
colnames(sem.data)<-c("HouseholdID","Y","MPAID","X","M1","M2","local.fish")
sem.data.reduced <-na.omit(sem.data)
model1 <-' # direct effect
          Y ~ a*X
          # mediator
          M1 ~ b*X
          M2 ~d*X
          Y ~ c*M1
          Y ~e*M2
          #indirect effects
          bcde := b*c*d*e
          #total effect
          total := a+(b*c*d*e)
'
fit1 <-sem(model1,data=sem.data, std.lv=TRUE)
summary(fit1)

# Y: food security
# a: MPA to food security
# b: MPA to biomass (M1)
# d: MPA to tenure.control (M2)
# c: biomass to food security
# e: tenure to food security










# -----
# 2.1 Create SEM objects

# -----
# 2.2 Structural equation model

model <- '# direct effect
          Y ~ a*X

          # mediators
          M1 ~ d*X
          M2 ~ c*X
          M3 ~ b*X
          M5 ~(e*M1)+(f*M2)
          M6 ~(g*M2)+(h*M3)
          Y ~ (j*M5)+(i*M6)
          
          # indirect effect      
          indirect := b*c*d*e*f*g*h*i*j

          #total effect
          total :=c +(b*c*d*e*f*g*h*i*j)
'
fit <-sem(model, data=sem.data, fit.measures=TRUE)
summary(fit)