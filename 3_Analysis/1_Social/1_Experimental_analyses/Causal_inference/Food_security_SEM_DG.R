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
#library(plyr)
#library(dplyr)
library(lavaan)
library(semPlot)
script.dir <- 'C:/Users/LocalAdmin/Documents/OneDrive - Conservation International 1/Data analysis/BHS/'

# -----
# 1.2 Source relevant R code 
source('2_Social/SourcedScripts/BHS_MPA_Mystery.R')
source('2_Social/SourcedScripts/Function_variable_outcome.R') # function to calculate outcomes
#source('2_Social/SourcedScripts/Matching 2yr_DG.R') # get match pairs
source(paste0(script.dir,'Matching 2yr_DG.R')) # get food security table
source(paste0(script.dir,'Food security original variables_DG.R')) # get food security table
source('2_Social/SourcedScripts/Function_summarise_bigfive_impacts.R')

# -----
# 1.2 Import raw.data
# 2 year impact data
#HH.data <- read.delim("C:/Users/LocalAdmin/Dropbox/BHS/HHdata_raw_2015_0704.txt")

# -----
# 2.1 Compute fish consumption indicator
fish.consume<-HHData %>% 
  select(HouseholdID,FreqEatFishClean) %>%
### Convert all blind codes to missing values. Note '994' in this context represents non-fishers.
  mutate(FreqEatFishClean = ifelse(FreqEatFishClean>=995 | FreqEatFishClean==993 |is.na(FreqEatFishClean),
                              990,FreqEatFishClean),
         FreqEatFishClean=as.factor(FreqEatFishClean)) %>%
  rename(fish.consume=FreqEatFishClean)
summary(fish.consume$fish.consume)

# -----
# 2.2 Compute fish sale indicator
fish.sale.outcome<-HHData %>% 
  select(HouseholdID,FreqSaleFishClean) %>%
  ### Convert all blind codes to missing values. Note '994' in this context represents non-fishers.
  mutate(FreqSaleFishClean = ifelse(FreqSaleFishClean>=995 |FreqSaleFishClean==993 |is.na(FreqSaleFishClean),
                              990,FreqSaleFishClean)) %>%
  rename(fish.freq.sale=FreqSaleFishClean)
summary(fish.sale.outcome$fish.freq.sale)

# Pre-process data
fish.sale.MPA.t2 <- MPA.time.pairs %>%
  select(HouseholdID) %>%
  left_join(fish.sale.outcome,by=c("HouseholdID"))

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
sem.data <-read.csv("C:/Users/LocalAdmin/Documents/OneDrive - Conservation International 1/Data analysis/BHS/sem_data.csv", sep=",")
add.food.data <-read.csv("C:/Users/LocalAdmin/Documents/OneDrive - Conservation International 1/Data analysis/BHS/additional_foodsec_data.csv", sep=",")
head(sem.data)
# colnames(sem.data)<-c("HouseholdID","Y","MPAID","X","M1","M2","local.fish")
# sem.data.reduced <-na.omit(sem.data)
# model1 <-' # direct effect
#           Y ~ a*X
#           # mediator
#           M1 ~ b*X
#           M2 ~d*X
#           Y ~ c*M1
#           Y ~e*M2
#           #indirect effects
#           bcde := b*c*d*e
#           #total effect
#           total := a+(b*c*d*e)
# '
colnames(sem.data)<-c("HouseholdID","food.sec","MPAID","MPA","tenure","biomass","local.fish")
#rescale biomass
sem.data$biomass1 <- sem.data$biomass/10
head(sem.data)
model1 <-' # direct effect
          food.sec ~ a*MPA
          # mediator
          biomass1 ~ b*MPA
          tenure ~d*MPA
          food.sec ~ c*biomass1
          food.sec ~e*tenure
          #indirect effects
          bcde := b*c*d*e
          #total effect
          total := a+(b*c*d*e)'
fit1 <-sem(model1,data=sem.data, std.lv=TRUE)
summary(fit1)
semPaths(fit1, "std",edge.label.cex=1.5, curvePivot=T, layout ="spring")

# Y: food security
# a: MPA to food security
# b: MPA to biomass (M1)
# d: MPA to tenure.control (M2)
# c: biomass to food security
# e: tenure to food security




sem.data1 <- left_join(sem.data,add.food.data,by='HouseholdID')
names(sem.data1)
model2 <-' 
# measurement model-food security definition
    food.sec1 =~ FSDidNotLast.ATT + FSBalancedDiet.ATT + FSAdultSkip.ATT + FSEatLess.ATT + FSFreqAdultSkip.ATT + FSHungry.ATT
          # direct effect
          food.sec1 ~ a*MPA
          # mediator
          biomass1 ~ b*MPA
          tenure ~d*MPA
          food.sec1 ~ c*biomass1
          food.sec1 ~e*tenure
          #indirect effects
          bcde := b*c*d*e
          #total effect
          total := a+(b*c*d*e)'

fit2 <-sem(model2,data=sem.data1, std.lv=TRUE)
summary(fit2)
semPaths(fit1, "std",edge.label.cex=1.5, curvePivot=T, layout ="spring")





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

