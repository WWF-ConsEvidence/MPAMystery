# ---
# code:  Short-term social impacts of marine protected areas -BHS 
# author: Louise Glew, louise.glew@gmail.com
# created: July 2015
# modified: October 2017 (by Kelly Claborn, clabornkelly@gmail.com)

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
# Code sections
#  1) Import raw data
#  2) Compute matching covariates
#  3) Create matching objects
#  4) Match MPA at two years -> MPA at baseline
#  5) Match MPA at two years -> Control at two years
#  6) Match MPA at two years -> Control at baseline
#  7) Compute treatment effects (ATT) for each MPA
#  8) Compute sub-group treatment effects

# ================================================================================================
#
# SECTION 1: Import raw data
#
# ================================================================================================

# ======
# 1.1 Import raw data
# HH.data - data on MPA outcomes, plus identifying variables
# DE.data - individual level demographic information
# district.data - treatment identifier for each settlement, plus other district level information
# Ethnic - processed paternal ethnicity data for each household

HH.data <- read.delim("2_Social/FlatDataFiles/BHS/HHdata_raw_2015_0704.txt")
DE.data <-read.delim("2_Social/FlatDataFiles/BHS/Demographic_raw_2015_0704.txt")
district.data <-read.delim("2_Social/FlatDataFiles/BHS/District_codes_2015_0704.txt")
Ethnic<- read.delim ("2_Social/FlatDataFiles/BHS/Ethnic_2015_0705.txt")

# ================================================================================================
#
# SECTION 2: Compute matching covariates
#
# ================================================================================================
# 2.1 Create master identification data frame
# 2.2 Extract treatment and mainland/island information
# 2.3 Compute distance to market
# 2.4 Compute ethnic fractional index
# 2.5 Compute dominant ethnicity
# 2.6 Compute age of household head
# 2.7 Compute education level
# 2.8 Compute residency
# 2.9  Compute number of children in household
# 2.10 Compute gender of household head
# 2.12 Compute occupational dependence on fisheries
# 2.12 Computer fisher vs. non-fisher variable
# 2.13 Extract primary occupation
# 2.14 Remove intermediate variables

# ======
# 2.1 Create master identification data frame
HH.identification <-subset(HH.data, select=c("HouseholdID","MPAID","SettlementID","InterviewYear","Baseline_t2_pairs"))

# ======
# 2.2 Extract treatment and mainland/island information
district.data<-subset(district.data, select=c("Treatment","Mainland","District.Code", "SettlementID"))
district.data$treatment <- district.data$Treatment
district.data$mainland <- district.data$Mainland
district.data$district.code <- district.data$District.Code
district.data <- subset(district.data, select=c("treatment","mainland","district.code","SettlementID"))

# ======
# 2.3 Compute distance to market (travel time in hours)
market.distance<-subset(HH.data,select=c("HouseholdID","TimeMarket", "InterviewYear","SettlementID", "MPAID"))
is.na(market.distance[,2])<-market.distance[,2]>=990
market.distance[is.na(market.distance)]<-990
valid.market<-subset(market.distance, market.distance$TimeMarket<990) #  extract valid market distances 
market.mean <- ddply(valid.market, .(SettlementID,InterviewYear), summarise, market.mean=mean(TimeMarket)) # subsequent rows handle blind codes, and missing data
no.market<-subset(market.distance, market.distance$TimeMarket==990) #  extract households with missing TimeMarket data
no.market$TimeMarket<-NULL #  remove TimeMarket field with missing data
no.market.baseline<-subset(no.market, ((no.market$MPAID==1|no.market$MPAID==2) & no.market$InterviewYear ==2010))# subset baseline data from MPA ==1 and ==2.
market.mean.baseline<-subset(market.mean, InterviewYear==2012)# fill missing baseline data with mean values from 2012
no.market.baseline<-left_join(no.market.baseline, market.mean.baseline, by="SettlementID")
no.market.baseline$InterviewYear<-no.market.baseline$InterviewYear.x
no.market.baseline$InterviewYear.x<-NULL #remove duplicate year values
no.market.baseline$InterviewYear.y<-NULL 
no.market$InterviewYear<-as.numeric(as.character(no.market$InterviewYear))
no.market.later<-subset(no.market, (no.market$InterviewYear>2010))# subset all other MPA data
no.market.later<-left_join(no.market.later,market.mean, by=c("SettlementID","InterviewYear")) # fill missing data with mean values from same year, and same settlement
no.market.baseline$InterviewYear<-as.factor(no.market.baseline$InterviewYear)
no.market<-rbind(no.market.later,no.market.baseline)
no.market$TimeMarket<-no.market$market.mean
no.market$market.mean<-NULL
market.distance<-rbind(valid.market,no.market)
market.distance <-market.distance[order(market.distance$HouseholdID),]
market.distance <-subset(market.distance, select=c("HouseholdID","TimeMarket")) 
market.distance$market.distance <-market.distance$TimeMarket
market.distance$TimeMarket <-NULL

# ======
# 2.4 Compute fractional index 
InterviewYear <- subset(HH.data, select=c("HouseholdID","InterviewYear"))
Ethnic<-subset(Ethnic,select=c("HouseholdID","SettlementID","Eth.Iso", "Eth.Num"))
Ethnic$Eth.Num<-as.vector(as.character(Ethnic$Eth.Num))
Ethnic <-left_join(Ethnic,InterviewYear, by="HouseholdID")
Eth.Frac <- ddply(Ethnic, .(InterviewYear,SettlementID,Eth.Iso), summarise, Freq.Eth=length(Eth.Num))
Eth.Frac <- ddply(Eth.Frac,.(InterviewYear,SettlementID),mutate, Sett.HH=sum(Freq.Eth))
Eth.Frac <-mutate(Eth.Frac, Prop.Eth=((Freq.Eth/Sett.HH)*(Freq.Eth/Sett.HH)))
Eth.Frac <- ddply(Eth.Frac,.(InterviewYear,SettlementID),summarise,frac=(1-sum(Prop.Eth)))
HH.Year <- subset(Ethnic,select=c("HouseholdID","InterviewYear","SettlementID"))
Eth.Frac <- merge(HH.Year,Eth.Frac, by=c("InterviewYear","SettlementID")) #  Final Ethnic Fractional Index
Eth.Frac <-subset(Eth.Frac,select=c("HouseholdID","frac"))

# ======
# 2.5 Compute dominant ethnicity binary variable
InterviewYear <- subset(HH.data, select=c("HouseholdID","InterviewYear"))
Ethnic<-subset(Ethnic,select=c("HouseholdID","SettlementID","Eth.Iso", "Eth.Num"))
Ethnic$Eth.Num<-as.vector(as.character(Ethnic$Eth.Num))
Ethnic <-left_join(Ethnic,InterviewYear, by="HouseholdID")
Freq.Eth <- ddply(Ethnic, .(InterviewYear,SettlementID,Eth.Num), summarise, Freq.Eth=length(Eth.Num)) # compute dominant ethnicity per settlement
Freq.Eth<-group_by(Freq.Eth,SettYear=SettlementID+InterviewYear)
Max.Eth <- top_n(Freq.Eth, 1, Freq.Eth)
Ethnic <- within(Ethnic, Dom.Eth<-paste(Ethnic$SettlementID, Ethnic$InterviewYear, Ethnic$Eth.Num, sep="."))
Max.Eth<- within(Max.Eth,Dom.Eth<-paste(Max.Eth$SettlementID, Max.Eth$InterviewYear, Max.Eth$Eth.Num, sep="."))
Dom.Eth<-full_join(Ethnic, Max.Eth, by="Dom.Eth")
Dom.Eth$InterviewYear.y<-as.numeric(Dom.Eth$InterviewYear.y)
Dom.Eth$InterviewYear.y[is.na(Dom.Eth$InterviewYear.y)] <-0
Dom.Eth$InterviewYear.y<-as.factor(Dom.Eth$InterviewYear.y)
Dom.Eth$dom.eth <- revalue(Dom.Eth$InterviewYear.y, c("2010"="1","2011"="1","2012"="1","2013"="1","2014"="1","0"="0"))
Dom.Eth<-subset(Dom.Eth, select=c("HouseholdID", "dom.eth")) #  Final dominant ethnicity binary variable

# ======
# 2.6 Compute age of household head 
HHHead<-subset(DE.data, select=c("HouseholdID","DemographicCode","RelationHHH", "IndividualGender", "IndividualAge"))
HHHead<-subset(HHHead,HHHead$DemographicCode==1) #  identify household heads
is.na(HHHead[,5])<-HHHead[,5]>=990
HHHead$IndividualAge[is.na(HHHead$IndividualAge)]<-990
age.bin<-c(0,20,30,40,50,60,70,990)
HHH.age.baseline<-HHHead$IndividualAge 
HouseholdID<-HHHead$HouseholdID
individual.age.baseline<-.bincode(HHH.age.baseline,age.bin,TRUE,TRUE)
HH.age.baseline <- cbind(HouseholdID,individual.age.baseline)
HHH.age.repeat <- HHHead$IndividualAge
HHH.age.repeat <- ifelse(HHH.age.repeat<990,(HHH.age.repeat+2),990)
individual.age.repeat <-.bincode(HHH.age.repeat, age.bin, TRUE,TRUE)
HH.age.repeat <- cbind(HouseholdID,individual.age.repeat)
HH.age.repeat<-data.frame(HH.age.repeat)
HH.age.baseline<-data.frame(HH.age.baseline)

# ======
# 2.7 Compute education level of household head
HHHead<-subset(DE.data, select=c("HouseholdID","RelationHHH", "IndividualGender", "IndividualAge", "IndividualEdLevel", "IndividualEducation", "DemographicCode"))
HHHead<-subset(HHHead,HHHead$DemographicCode==1) #  identify household heads
ed.level.unique<-unique(subset(HHHead,select=c("IndividualEducation")))
write.table(ed.level.unique, "2_Social/FlatDataFiles/BHS/ed.level.unique.txt", sep=",")
ed.level.unique <-read.delim("2_Social/FlatDataFiles/BHS/ed.level.unique.coded.txt")
HHHead$IndividualEducation<-as.character(HHHead$IndividualEducation)
ed.level.unique$IndividualEducation<-as.character(ed.level.unique$IndividualEducation)
HH.ed.level <- inner_join(HHHead,ed.level.unique, by="IndividualEducation") 
HH.ed.level$ed.level[is.na(HH.ed.level$ed.level)]<-6
HH.ed.level$ed.level<-as.factor(HH.ed.level$ed.level)
HH.ed.level <- subset (HH.ed.level, select =c("HouseholdID", "ed.level"))

# ======
# 2.8 Compute household residency
residency<-subset(HH.data,select=c("HouseholdID","YearsResident"))
is.na(residency[,2])<-residency[,2]>=990
residency[is.na(residency)]<-990
resident.bin<-c(0,10,20,30,40,50,60,990)
years.resident<-residency$YearsResident
years.resident.repeat <-residency$YearsResident
years.resident.repeat <- ifelse(years.resident.repeat<990,(years.resident.repeat+2),990)
years.resident.baseline<-.bincode(years.resident,resident.bin,TRUE,TRUE)
years.resident.repeat<-.bincode(years.resident.repeat,resident.bin,TRUE,TRUE)
residency<-cbind(residency,years.resident.baseline, years.resident.repeat)

residency$YearsResident<-NULL

# ======
# 2.9 Compute number of children in household
DE.data$Child<-ifelse(DE.data$IndividualAge<19,1,0) #  create new variable, child/adult
N.Children <- ddply(DE.data, .(HouseholdID), summarise, n.child=sum(Child)) #
N.Children$n.child[is.na(N.Children$n.child)] <- 0

# ======
# 2.10 Compute gender of household head
HHHead<-subset(DE.data, select=c("HouseholdID","DemographicCode","RelationHHH", "IndividualGender"))
HHHead<-subset(HHHead,HHHead$DemographicCode==1) #  identify household heads
gender.HHH<-subset(HHHead,select=c("HouseholdID","IndividualGender"))
gender.HHH$individual.gender <- gender.HHH$IndividualGender
gender.HHH$IndividualGender <- NULL

# ======
# 2.11 Occupational dependence on fisheries
fishing.dependence<-subset(HH.data, select=c("HouseholdID","PrimaryLivelihood","SecondaryLivelihood","TertiaryLivelihood")) #  extract data
is.na(fishing.dependence[,2:4])<-fishing.dependence[,2:4]>=990 #  set all blind codes to NA
fishing.dependence[is.na(fishing.dependence)]<-990 #  set all NAs to 990
fishing.dependence$PrimaryLivelihood<-as.factor(fishing.dependence$PrimaryLivelihood) # set variable states
fishing.dependence$SecondaryLivelihood<-as.factor(fishing.dependence$SecondaryLivelihood) # set variable states
fishing.dependence$TertiaryLivelihood<-as.factor(fishing.dependence$TertiaryLivelihood) # set variable states
fishing.dependence$primaryfisher<-ifelse(fishing.dependence$PrimaryLivelihood=="3",3,0) # recode to fisher/non-fisher
fishing.dependence$secondaryfisher<-ifelse(fishing.dependence$SecondaryLivelihood=="3",2,0)
fishing.dependence$tertiaryfisher<-ifelse(fishing.dependence$TertiaryLivelihood=="3",2,0)
fishing.dependence<-mutate(fishing.dependence, occ.dependence=primaryfisher+secondaryfisher+tertiaryfisher)
fishing.dependence<-data.frame(fishing.dependence)
fishing.dependence$occ.dependence<-as.factor(fishing.dependence$occ.dependence)
fishing.dependence$occ.dependence<-revalue(fishing.dependence$occ.dependence, c("4"="2","5"="3","0"="1"))
occ.dependence<-subset(fishing.dependence, select=c("HouseholdID","occ.dependence"))

# ======
# 2.12 Fisher vs. Non-fisher
fishing.dependence<-subset(HH.data, select=c("HouseholdID","PrimaryLivelihood","SecondaryLivelihood","TertiaryLivelihood")) #  extract data
is.na(fishing.dependence[,2:4])<-fishing.dependence[,2:4]>=990 #  set all blind codes to NA
fishing.dependence[is.na(fishing.dependence)]<-990 #  set all NAs to 990
fishing.dependence$PrimaryLivelihood<-as.factor(fishing.dependence$PrimaryLivelihood) # set variable states
fishing.dependence$SecondaryLivelihood<-as.factor(fishing.dependence$SecondaryLivelihood) # set variable states
fishing.dependence$TertiaryLivelihood<-as.factor(fishing.dependence$TertiaryLivelihood) # set variable states
fishing.dependence$fisher<-ifelse(fishing.dependence$PrimaryLivelihood=="3"|fishing.dependence$SecondaryLivelihood=="3"|fishing.dependence$TertiaryLivelihood=="3",1,0) # recode to fisher/non-fisher
fisher<-subset(fishing.dependence,select=c("HouseholdID", "fisher"))

# ======
# 2.13 Extract primary occupation
primary.occupation<-subset(fishing.dependence, select=c("HouseholdID","PrimaryLivelihood")) 
primary.occupation$PrimaryLivelihood<-as.factor(primary.occupation$PrimaryLivelihood)
primary.occupation$PrimaryLivelihood <- revalue(primary.occupation$PrimaryLivelihood,c("2"="996","6"="996","990"="996","4"="996","5"="996"))
primary.occupation$primary.occupation<-primary.occupation$PrimaryLivelihood
primary.occupation$PrimaryLivelihood <- NULL
primary.occupation$primary.occupation<-as.numeric(as.character(primary.occupation$primary.occupation))

# ======
# 2.14 Remove intermediate variable objects
rm(HH.data, HH.Year, market.mean, valid.market, fishing.dependence,HHHead,DE.data,years.resident, years.resident.baseline, ed.level.unique,Ethnic,Max.Eth, Freq.Eth,InterviewYear,no.market,no.market.later,no.market.baseline,market.mean.baseline, HHH.age.repeat, individual.age.baseline,HouseholdID, age.bin)



# ================================================================================================
#
# SECTION 3: Create covariate dataframe
#
# ================================================================================================
# 3.1 Combine match covariates into a single data frame
# 3.2 Remove intermediate match covariate objects
# ======
# 3.1 Combine covariates into single dataframe
match.covariate<-left_join(HH.identification,district.data, by="SettlementID")
match.covariate<-left_join(match.covariate, market.distance, by="HouseholdID")
match.covariate<-left_join(match.covariate, Eth.Frac, by="HouseholdID")
match.covariate<-left_join(match.covariate, Dom.Eth, by="HouseholdID")
match.covariate<-left_join(match.covariate, residency, by="HouseholdID")
match.covariate<-left_join(match.covariate, N.Children, by="HouseholdID")
match.covariate<-left_join(match.covariate, primary.occupation, by="HouseholdID")
match.covariate<-left_join(match.covariate, occ.dependence, by="HouseholdID")
match.covariate<-left_join(match.covariate, HH.age.baseline, by="HouseholdID")
match.covariate<-left_join(match.covariate, HH.age.repeat, by="HouseholdID")
match.covariate<-left_join(match.covariate, gender.HHH, by="HouseholdID")
match.covariate<-left_join(match.covariate, HH.ed.level, by="HouseholdID") 
match.covariate<-left_join(match.covariate, fisher, by="HouseholdID")

# ======
# 3.2 Remove intermediate variable objects
rm(HH.identification,district.data, market.distance, Eth.Frac, Dom.Eth, residency, N.Children, primary.occupation, occ.dependence,HH.age.baseline, HH.age.repeat, gender.HHH, HH.ed.level, fisher, resident.bin)


# ================================================================================================
#
# SECTION 5: Match MPA at two years -> MPA at baseline
#
# ================================================================================================
#  X = matrix containing variables to match on
#  Tr = vector indicating treatment vs. control (logical or real)
#  Y = vector containing outcome of interest (usually NULL)

# 5.1  Subset by monitoring round to create baseline and repeat data frames
# 5.2  For variables affected by time lapse (e.g., age and years resident) harmonize variables into one column.
# 5.3  Create a longitudinal dummy treatment variable (Tr=1 [repeat])
# 5.4  Rejoin repeat and baseline data
# 5.5  Recode Kaimana MPA
# 5.6  Remove settlements not monitored at baseline and two years
# 5.7  Set row names to HouseholdID

# ======
# 5.1 Subset to MPA only
time.match.covariate <- subset(match.covariate, treatment==1) 

# ======
# 5.2  Subset by monitoring round to create baseline and repeat data frames
repeat.data <-subset(time.match.covariate,(time.match.covariate$MPAID==1 & time.match.covariate$InterviewYear==2012)| (time.match.covariate$MPAID==2 & time.match.covariate$InterviewYear==2012) |(time.match.covariate$MPAID==3 & time.match.covariate$InterviewYear==2014)|(time.match.covariate$MPAID==4 & time.match.covariate$InterviewYear==2013)|(time.match.covariate$MPAID==5 & time.match.covariate$InterviewYear==2014)|(time.match.covariate$MPAID==6 & time.match.covariate$InterviewYear==2013))
baseline.data <-subset(time.match.covariate,(time.match.covariate$MPAID==1 & time.match.covariate$InterviewYear==2010)| (time.match.covariate$MPAID==2 & time.match.covariate$InterviewYear==2010) |(time.match.covariate$MPAID==3 & time.match.covariate$InterviewYear==2012)|(time.match.covariate$MPAID==4 & time.match.covariate$InterviewYear==2011)|(time.match.covariate$MPAID==5 & time.match.covariate$InterviewYear==2012)|(time.match.covariate$MPAID==6 & time.match.covariate$InterviewYear==2011))

# ======
# 5.3  For variables affected by time lapse (e.g., age and years resident) harmonize variables into one column.
repeat.data$years.resident.longitudinal <- repeat.data$years.resident.repeat
baseline.data$years.resident.longitudinal <- baseline.data$years.resident.baseline
repeat.data$individual.age.longitudinal <- repeat.data$individual.age.repeat
baseline.data$individual.age.longitudinal <- baseline.data$individual.age.baseline
repeat.data$years.resident.repeat <-NULL
baseline.data$years.resident.baseline <- NULL
repeat.data$individual.age.repeat <- NULL
baseline.data$individual.age.baseline <-NULL
baseline.data$years.resident.repeat <-NULL
repeat.data$years.resident.baseline <- NULL
baseline.data$individual.age.repeat <- NULL
repeat.data$individual.age.baseline <-NULL

# ======
# 5.4  Create a longitudinal dummy treatment variable (Tr=1 [repeat])
repeat.data$Tr.longitudinal<-1
baseline.data$Tr.longitudinal<-0


# ======
# 5.5 Rejoin repeat and baseline data
time.match.covariate <- rbind(baseline.data,repeat.data)
time.match.covariate<- time.match.covariate[order(time.match.covariate$HouseholdID),]

# ======
# 5.6 Recode Kaimana
settlement.data <-read.delim("2_Social/FlatDataFiles/BHS/Settlement_2015_0715.txt")
MPA.recode <- subset(settlement.data, select=c("SettlementID","MPAID"))
time.match.covariate$MPAID<-NULL
time.match.covariate<-left_join(time.match.covariate, MPA.recode, by="SettlementID")
#time.match.covariate <- merge(time.match.covariate,MPA.recode,by="SettlementID")

# ======
# 5.7 Eliminate settlements not monitored at both time periods 
#Remove Dampier settlement not surveyed at t0 (72)
time.match.covariate <-subset(time.match.covariate,(SettlementID!=72))

#Remove Kaimana controls/MPA settlementd not sampled at t2 (84,96 - 101)
time.match.covariate <-subset(time.match.covariate,(SettlementID!=96))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=97))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=98))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=99))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=100))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=101))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=84))

# Remove new Kaimana settlements, not samplied at t0 (113 -115) -ALL ETNA (no impacts possible for ETNA)
time.match.covariate <-subset(time.match.covariate,(SettlementID!=113))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=114))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=115))

# Remove TNTC settlements not surveyed at t0 (104-112)
time.match.covariate <-subset(time.match.covariate,(SettlementID!=104))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=105))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=106))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=107))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=108))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=109))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=110))
time.match.covariate <-subset(time.match.covariate,(SettlementID!=111))

# ======
# 5.8 Set row names to HouseholdID
row.names(time.match.covariate)<-time.match.covariate$HouseholdID


# ======
# 5.9 Extract non-longitudinal households

nonbaseline.pairs.t2 <- subset (time.match.covariate, time.match.covariate$Baseline_t2_pairs==0)
row.names(nonbaseline.pairs.t2)<-nonbaseline.pairs.t2$HouseholdID
# ======
# 5.9 Create X
X<- subset (nonbaseline.pairs.t2, select=c("SettlementID","dom.eth","n.child","occ.dependence","individual.gender","ed.level","years.resident.longitudinal","individual.age.longitudinal","Baseline_t2_pairs"))

# ======
# 5.10 Extract Tr
Tr.longitudinal<-nonbaseline.pairs.t2$Tr.longitudinal

# ======
# 5.11  Extract MatchBy factor (grouping factor)
MPAID<-as.factor(nonbaseline.pairs.t2$MPAID)

# ======
# 5.12 Set variable states for MatchBalance routine
nonbaseline.pairs.t2$fisher <-as.factor(nonbaseline.pairs.t2$fisher)
nonbaseline.pairs.t2$mainland <- as.factor(nonbaseline.pairs.t2$mainland)
nonbaseline.pairs.t2$dom.eth <- as.factor(nonbaseline.pairs.t2$dom.eth)
nonbaseline.pairs.t2$primary.occupation <- as.factor(nonbaseline.pairs.t2$primary.occupation)
nonbaseline.pairs.t2$occ.dependence <- as.factor(nonbaseline.pairs.t2$occ.dependence)
nonbaseline.pairs.t2$individual.gender <- as.factor(nonbaseline.pairs.t2$individual.gender)
nonbaseline.pairs.t2$ed.level <- as.factor(nonbaseline.pairs.t2$ed.level)
nonbaseline.pairs.t2$years.resident.longitudinal <- as.factor(nonbaseline.pairs.t2$years.resident.longitudinal)
nonbaseline.pairs.t2$individual.age.longitudinal<- as.factor(nonbaseline.pairs.t2$individual.age.longitudinal)



# ======
# 5.14 Compute propensity score
psm.time <- glm (Tr.longitudinal~ market.distance + dom.eth + n.child + occ.dependence + years.resident.longitudinal+individual.gender+ed.level+individual.age.longitudinal, data=nonbaseline.pairs.t2, family= binomial())

# ======
# 5.15  Propensity score matching
row.names(nonbaseline.pairs.t2)<-nonbaseline.pairs.t2$HouseholdID
X <- subset(nonbaseline.pairs.t2, select =c("SettlementID","dom.eth","n.child","occ.dependence" ,"individual.gender" , "ed.level" , "years.resident.longitudinal" , "individual.age.longitudinal", "MPAID"))
#X$fisher<-as.numeric(as.character(X$fisher))
X$dom.eth<-as.numeric(as.character(X$dom.eth))
#X$primary.occupation<-as.numeric(as.character(X$primary.occupation))
X$occ.dependence<-as.numeric(as.character(X$occ.dependence))
X$ed.level<-as.numeric(as.character(X$ed.level))
X$individual.gender<-as.numeric(as.character(X$individual.gender))
X$years.resident.longitudinal<-as.numeric(as.character(X$years.resident.longitudinal))
X$individual.age.longitudinal<-as.numeric(as.character(X$individual.age.longitudinal))
#X$MPAID<-as.numeric(as.character(X$MPAID))
ps <-psm.time$fitted
X <- cbind(X, ps)
#exact.match1 <- c(0,0,0,0,0,0,0,0,1,0)
#caliper1 <- c(100,100,100,100,100,100,100,100,100,0.5)
caliper2 <- c(100,1.5,1.5,1.5,100,100,100,100,100,0.5)
#m1<-Match(Y=NULL,Tr=Tr.longitudinal,X=X,estimand="ATT", M=1, ties=TRUE, replace=TRUE,caliper=caliper1)
m2<-Matchby(Y=NULL,Tr=Tr.longitudinal,X=X,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE,caliper=caliper2)
#m3<-Match(Y=NULL,Tr=Tr.longitudinal,X=X,estimand="ATT", M=1, ties=TRUE, replace=TRUE,caliper=caliper2, exact=exact.match1)
#X4 <-subset(X, select=c("SettlementID","dom.eth","n.child","occ.dependence", "individual.gender"))
#X4 <-cbind(X4,ps)
#caliper4<- c(100,1.5,1.5,1.3,1.5,0.5)
#m4 <-Matchby(Y=NULL,Tr=Tr.longitudinal,X=X4,by=MPAID,estimand="ATT", M=1, ties=TRUE, replace=TRUE,caliper=caliper4)

# ======
# 5.16 Compute covariate balance 

#mb1 <- MatchBalance(Tr.longitudinal~ SettlementID+ dom.eth + n.child + occ.dependence +individual.gender + ed.level + 
#years.resident.longitudinal + individual.age.longitudinal, match.out=m1,data=nonbaseline.pairs.t2,ks=TRUE, nboots=100, digits=3, paired=TRUE)

mb2 <- MatchBalance(Tr.longitudinal~ SettlementID+ dom.eth + n.child + occ.dependence +individual.gender + ed.level + 
                      years.resident.longitudinal + individual.age.longitudinal, match.out=m2, data=nonbaseline.pairs.t2,ks=TRUE, nboots=100, digits=3, paired=TRUE)

#mb4 <- MatchBalance(Tr.longitudinal~ SettlementID+ dom.eth + n.child + occ.dependence +individual.gender + ed.level + 
#                      years.resident.longitudinal + individual.age.longitudinal+market.distance, match.out=m4,data=nonbaseline.pairs.t2,ks=TRUE, nboots=100, digits=3, paired=TRUE)

rm(psm.time,ps, Tr.longitudinal, repeat.data, baseline.data, MPA.recode, individual.age.repeat, years.resident.repeat, HHH.age.baseline, caliper2)

# ======
# 5.17 Extract dropped cases
index.rows <-seq.int(1:nrow(X))
index.treated <- data.frame(unique(m2$index.treated))
colnames(index.treated) <-c("index.rows")
index.dropped <- subset(nonbaseline.pairs.t2, select=c("HouseholdID","SettlementID","MPAID","Tr.longitudinal"))
index.dropped <- cbind(index.rows,index.dropped)
index.dropped <- subset(index.dropped, Tr.longitudinal==1)
index.dropped <-anti_join(index.dropped,index.treated, by="index.rows")
ndrops.settlement <- ddply(index.dropped, .(SettlementID),summarise, ndrops=length(unique(HouseholdID)))
sample.settlement <- subset(nonbaseline.pairs.t2, select=c("SettlementID", "HouseholdID"))
sample.settlement <- ddply(sample.settlement,.(SettlementID),summarise, sample.size = length(unique(HouseholdID)))
ndrops.settlement <- left_join(ndrops.settlement, sample.settlement, by="SettlementID")
ndrops.settlement <-ddply (ndrops.settlement,.(SettlementID), summarise, percent.drop =(ndrops/sample.size)*100)
ndrops.settlement<- ndrops.settlement[order(ndrops.settlement$SettlementID),]

ndrops.mpa <- ddply(index.dropped, .(MPAID),summarise, ndrops=length(unique(HouseholdID)))
sample.MPA <- subset(nonbaseline.pairs.t2, select=c("MPAID", "HouseholdID"))
sample.MPA <- ddply(sample.MPA,.(MPAID),summarise, sample.size = length(unique(HouseholdID)))
ndrops.mpa <- left_join(ndrops.mpa, sample.MPA, by="MPAID")
ndrops.mpa <-ddply (ndrops.mpa,.(MPAID), summarise, percent.drop =(ndrops/sample.size)*100)
ndrops.mpa<- ndrops.mpa[order(ndrops.mpa$MPAID),]

write.table(ndrops.mpa, "2_Social/FlatDataFiles/BHS/MPA_time_ndrops_byMPA.txt", sep=",", row.names=FALSE)
write.table(ndrops.settlement, "2_Social/FlatDataFiles/BHS/MPA_time_ndrops_bySettlement.txt", sep=",", row.names=FALSE)

rm(sample.settlement, sample.MPA,index.rows, index.treated, ndrops.mpa, ndrops.settlement, index.dropped, X)

# ======
# 5.18 Create matched HouseholdID pairs
i.treat<-m2$index.treated
i.control<-m2$index.control
HouseholdID.t2<-nonbaseline.pairs.t2[i.treat,'HouseholdID']
HouseholdID.bl<-nonbaseline.pairs.t2 [i.control,'HouseholdID']
MPA.time.pairs<-data.frame(cbind(HouseholdID.t2,HouseholdID.bl))

rm(i.treat,i.control,HouseholdID.t2,HouseholdID.bl)

# ======
# 5.19 Extract longitudinal pairs
baseline.pairs.t2 <- subset (time.match.covariate, time.match.covariate$Baseline_t2_pairs!=0)
baseline.pairs.t2 <- subset (baseline.pairs.t2, Tr.longitudinal==0)
baseline.pairs.t2 <- subset (baseline.pairs.t2, select=c("Baseline_t2_pairs","HouseholdID"))
colnames(baseline.pairs.t2) <- c("HouseholdID.t2","HouseholdID.bl")

# ======
# 5.19 Extract longitudinal pairs
MPA.time.pairs <- rbind(MPA.time.pairs,baseline.pairs.t2)
pair.index <- seq.int(1:nrow(MPA.time.pairs))
MPA.time.pairs <-cbind(MPA.time.pairs,pair.index)

rm(baseline.pairs.t2,pair.index)

# ======
# 5.20 Compute covariate balance for full longitudinal sample

MPA.time.pairs.bl <- subset(MPA.time.pairs, select=c("HouseholdID.bl","pair.index"))
MPA.time.pairs.t2 <- subset(MPA.time.pairs, select=c("HouseholdID.t2","pair.index"))
MPA.time.pairs.bl$Tr <- 0
MPA.time.pairs.t2$Tr <- 1
colnames(MPA.time.pairs.bl) <- c("HouseholdID", "pair.index", "Tr")
colnames(MPA.time.pairs.t2) <- c("HouseholdID", "pair.index", "Tr")
MPA.time.pairs.match <- rbind(MPA.time.pairs.bl, MPA.time.pairs.t2)
MPA.time.pairs.match <- subset(MPA.time.pairs.match, HouseholdID!=4030)

#Create matching Tr
Tr <-as.vector(MPA.time.pairs.match$Tr)

#Create matching X
X <- as.matrix(MPA.time.pairs.match$pair.index)

#Create covariate balance object
MPA.time.pairs.match <- left_join(MPA.time.pairs.match, time.match.covariate, by="HouseholdID")

#Run dummy match
exact.match <- c(1)
dummy.time.match <- Match(Y=NULL, X=X, Tr=Tr,M=1, exact=exact.match)
MPA.time.pairs.match$years.resident.longitudinal <- as.factor(MPA.time.pairs.match$years.resident.longitudinal)
MPA.time.pairs.match$individual.age.longitudinal <-as.factor(MPA.time.pairs.match$individual.age.longitudinal)
dummy.balance <- MatchBalance(Tr~ SettlementID+ dom.eth+n.child+occ.dependence+individual.gender +ed.level+years.resident.longitudinal + individual.age.longitudinal, match.out=dummy.time.match, data=MPA.time.pairs.match,ks=TRUE, nboots=100, digits=3, paired=TRUE)

rm(MPA.time.pairs.t2, MPA.time.pairs.bl, MPA.time.pairs.match, exact.match, X, Tr, dummy.time.match)

# ================================================================================================
#
# SECTION 6: Compute Tr1t2 --> Tr0t2
#
# ================================================================================================
# 6.1 Extract repeat data from match.covariate for Tr1t2 -Tr0t2



# ======
# 6.1 Extract repeat data from match.covariate for Tr1t2 -Tr0t2

rpt.xsection.covariate<-subset(match.covariate,(match.covariate$MPAID==1 & match.covariate$InterviewYear==2012)| (match.covariate$MPAID==2 & match.covariate$InterviewYear==2012) |(match.covariate$MPAID==3 & match.covariate$InterviewYear==2014)|(match.covariate$MPAID==4 & match.covariate$InterviewYear==2013)|(match.covariate$MPAID==5 & match.covariate$InterviewYear==2014)|(match.covariate$MPAID==6 & match.covariate$InterviewYear==2013))

# Recode Kaimana controls to Tro (at baseline --counted as MPA in original matrix)
kaimana.controls <- subset(rpt.xsection.covariate, (SettlementID==83|SettlementID==91|SettlementID==92))
rpt.xsection.covariate <- subset(rpt.xsection.covariate, (SettlementID!=83 & SettlementID!=91 & SettlementID!=92))
kaimana.controls$treatment <- 0
rpt.xsection.covariate <- rbind(rpt.xsection.covariate,kaimana.controls)
row.names(rpt.xsection.covariate) <- rpt.xsection.covariate$HouseholdID
# Do not recode Kaimana into separate MPAs this case,-simplifies matching algorithm.

# ======
# 6.2 Create propensity score model
xpsm1 <- glm (treatment~ market.distance+ dom.eth + n.child + occ.dependence + years.resident.repeat +individual.gender+ed.level+individual.age.repeat, data=rpt.xsection.covariate, family= binomial())
xps <-xpsm1$fitted.values


# ======
# 6.2 Create X

X <- subset(rpt.xsection.covariate, select=c("market.distance", "dom.eth", "years.resident.repeat", "n.child", "occ.dependence", "individual.age.repeat", "individual.gender", "ed.level"))
X$dom.eth<-as.numeric(as.character(X$dom.eth))
X$occ.dependence<-as.numeric(as.character(X$occ.dependence))
X$individual.gender<-as.numeric(as.character(X$individual.gender))
X$years.resident.repeat<-as.numeric(as.character(X$years.resident.repeat))
X$ed.level<-as.numeric(as.character(X$ed.level))
X$individual.age.repeat<-as.numeric(as.character(X$individual.age.repeat))
MPAID <- rpt.xsection.covariate$MPAID

X.ps <-subset(X, select=c("market.distance","dom.eth", "n.child", "occ.dependence", "individual.gender"))
X.ps <- cbind(X.ps, xps)
# ======
# 6.3 Create Tr

Tr <- rpt.xsection.covariate$treatment

# ======
# 6.4 Set variable states for match balance

rpt.xsection.covariate$individual.gender <- as.factor(rpt.xsection.covariate$individual.gender)
rpt.xsection.covariate$individual.age.repeat <-as.factor(rpt.xsection.covariate$individual.age.repeat)
rpt.xsection.covariate$years.resident.repeat <-as.factor(rpt.xsection.covariate$years.resident.repeat)

# ======
# 6.5 Matching
#caliper2 <-c(100,1.5,100,1.5,1.5,100,1.5,100)
#xm2 <- Matchby(Y=NULL, Tr=Tr, X=X, replace=TRUE, ties=TRUE, M=1, by=MPAID, caliper=caliper2)
#caliper3 <-c(2,1.5,100,1.5,1.5,100,1.5,100)
#xm3 <- Matchby(Y=NULL, Tr=Tr, X=X, replace=TRUE, ties=TRUE, M=1, by=MPAID, caliper=caliper3)
#caliper4 <-c(100,1.5,100,1.5,1.5,1.5,1.5,100)
#xm4 <- Matchby(Y=NULL, Tr=Tr, X=X, replace=TRUE, ties=TRUE, M=1, by=MPAID, caliper=caliper4)
#xm5 <- Matchby(Y=NULL, Tr=Tr, X=X.ps, replace=TRUE, ties=TRUE, M=1, by=MPAID)
#caliper6 <- c(100,100,100,100,100,0.5)
#xm6 <- Matchby(Y=NULL, Tr=Tr, X=X.ps, replace=TRUE, ties=TRUE, M=1, by=MPAID, caliper=caliper6)
#caliper8 <- c(100,1.5,100,1.5,1.5,0.5)
#xm8 <- Matchby(Y=NULL, Tr=Tr, X=X.ps, replace=TRUE, ties=TRUE, M=1, by=MPAID, caliper=caliper8)

caliper7 <- c(100,1.5,100,100,1.5,0.5)
xm7 <- Matchby(Y=NULL, Tr=Tr, X=X.ps, replace=TRUE, ties=TRUE, M=1, by=MPAID, caliper=caliper7)

# ======
# 6.6 Compute covariate balance

xmb7 <- MatchBalance(Tr~ MPAID+market.distance+ dom.eth+n.child+occ.dependence+individual.gender +ed.level+years.resident.repeat + individual.age.repeat, match.out=xm7, data=rpt.xsection.covariate,ks=TRUE, nboots=100, digits=3, paired=TRUE)

# ======
# 6.7 Extract list of dropped cases
# index.rows <-seq.int(1:nrow(X.ps))
# index.treated <- data.frame(unique(xm8$index.treated))
# colnames(index.treated) <-c("index.rows")
# index.dropped <- subset(rpt.xsection.covariate, select=c("HouseholdID","SettlementID","MPAID","treatment"))
# index.dropped <- cbind(index.rows,index.dropped)
# index.dropped <- subset(index.dropped, treatment==1)
# index.dropped <-anti_join(index.dropped,index.treated, by="index.rows")
# ndrops.settlement <- ddply(index.dropped, .(SettlementID),summarise, ndrops=length(unique(HouseholdID)))
# sample.settlement <- subset(rpt.xsection.covariate, select=c("SettlementID", "HouseholdID"))
# sample.settlement <- ddply(sample.settlement,.(SettlementID),summarise, sample.size = length(unique(HouseholdID)))
# ndrops.settlement <- left_join(ndrops.settlement, sample.settlement, by="SettlementID")
# ndrops.settlement <-ddply (ndrops.settlement,.(SettlementID), summarise, percent.drop =(ndrops/sample.size)*100)
# ndrops.settlement <- ndrops.settlement[order(ndrops.settlement$SettlementID),]

# ndrops.mpa <- ddply(index.dropped, .(MPAID),summarise, ndrops=length(unique(HouseholdID)))
# sample.MPA <- subset(rpt.xsection.covariate, select=c("MPAID", "HouseholdID"))
# sample.MPA <- ddply(sample.MPA,.(MPAID),summarise, sample.size = length(unique(HouseholdID)))
# ndrops.mpa <- left_join(ndrops.mpa, sample.MPA, by="MPAID")
# ndrops.mpa <-ddply (ndrops.mpa,.(MPAID), summarise, percent.drop =(ndrops/sample.size)*100)
# ndrops.mpa<- ndrops.mpa[order(ndrops.mpa$MPAID),]

# rm(sample.settlement, sample.MPA,index.rows, index.treated, ndrops.mpa, ndrops.settlement, index.dropped)

# ======
# 6.9 Create matched HouseholdID pairs
i.treat<-xm7$index.treated
i.control<-xm7$index.control
HouseholdID.MPA<-rpt.xsection.covariate[i.treat,'HouseholdID']
HouseholdID.Control<-rpt.xsection.covariate[i.control,'HouseholdID']
MPA.rpt.xsection.pairs<-data.frame(cbind(HouseholdID.MPA,HouseholdID.Control))

rm(i.treat,i.control,HouseholdID.MPA,HouseholdID.Control)


# ================================================================================================
#
# SECTION 7: Match MPA at two years -> Control at baseline
#
# ================================================================================================

# ======
# 7.1 Extract data from match covariate
int.xsection.covariate.MPA <- rpt.xsection.covariate<-subset(match.covariate,(match.covariate$MPAID==1 & match.covariate$InterviewYear==2012)| (match.covariate$MPAID==2 & match.covariate$InterviewYear==2012) |(match.covariate$MPAID==3 & match.covariate$InterviewYear==2014)|(match.covariate$MPAID==4 & match.covariate$InterviewYear==2013)|(match.covariate$MPAID==5 & match.covariate$InterviewYear==2014)|(match.covariate$MPAID==6 & match.covariate$InterviewYear==2013))
int.xsection.covariate.MPA <- subset(int.xsection.covariate.MPA, treatment ==1)

# ======
# 7.2 Remove Kaimana controls from MPA at 2 years
int.xsection.covariate.MPA <- subset(int.xsection.covariate.MPA, (SettlementID!=83 & SettlementID!=91 & SettlementID!=92))

# ======
# 7.3 Recode Kaimana controls at baseline
int.xsection.covariate.Control <- subset(match.covariate,(match.covariate$MPAID==1 & match.covariate$InterviewYear==2010)| (match.covariate$MPAID==2 & match.covariate$InterviewYear==2010) |(match.covariate$MPAID==3 & match.covariate$InterviewYear==2012)|(match.covariate$MPAID==4 & match.covariate$InterviewYear==2011)|(match.covariate$MPAID==5 & match.covariate$InterviewYear==2012)|(match.covariate$MPAID==6 & match.covariate$InterviewYear==2011))
kaimana.controls <- subset(int.xsection.covariate.Control, (SettlementID==83|SettlementID==91|SettlementID==92))
int.xsection.covariate.Control<- subset(int.xsection.covariate.Control, (SettlementID!=83 & SettlementID!=91 & SettlementID!=92))
kaimana.controls$treatment <- 0
int.xsection.covariate.Control<- rbind(int.xsection.covariate.Control,kaimana.controls)
int.xsection.covariate.Control <-subset(int.xsection.covariate.Control, treatment==0)
# Do not recode Kaimana into separate MPAs this case,-simplifies matching algorithm.

# ======
#7.4 Handle time variant covariates (age of household head, years resident)

#MPA data -select repeat
int.xsection.covariate.MPA$individual.age <- int.xsection.covariate.MPA$individual.age.repeat
int.xsection.covariate.MPA$years.resident <- int.xsection.covariate.MPA$years.resident.repeat

int.xsection.covariate.MPA$years.resident.repeat <- NULL
int.xsection.covariate.MPA$years.resident.baseline <- NULL

int.xsection.covariate.MPA$individual.age.repeat <-NULL
int.xsection.covariate.MPA$individual.age.baseline <-NULL

# Control data -select repeat
int.xsection.covariate.Control$individual.age <- int.xsection.covariate.Control$individual.age.repeat
int.xsection.covariate.Control$years.resident <- int.xsection.covariate.Control$years.resident.repeat

int.xsection.covariate.Control$years.resident.repeat <- NULL
int.xsection.covariate.Control$years.resident.baseline <- NULL

int.xsection.covariate.Control$individual.age.repeat <-NULL
int.xsection.covariate.Control$individual.age.baseline <-NULL


# ======
# 7.4 Combine MPA and control datasets
int.xsection.covariate <- rbind(int.xsection.covariate.MPA,int.xsection.covariate.Control)

rm(int.xsection.covariate.Control,int.xsection.covariate.MPA,kaimana.controls)

# ======
# 7.5 Create propensity score model
ipsm1 <- glm (treatment~ market.distance+ dom.eth + n.child + occ.dependence + years.resident+individual.gender+ed.level+individual.age, data=int.xsection.covariate, family= binomial())
ips <-ipsm1$fitted
# ======
# 7.6 Create X
X <- subset(int.xsection.covariate, select=c("market.distance", "dom.eth", "years.resident", "n.child", "occ.dependence", "individual.age", "individual.gender", "ed.level"))

X$dom.eth<-as.numeric(as.character(X$dom.eth))
X$occ.dependence<-as.numeric(as.character(X$occ.dependence))
X$individual.gender<-as.numeric(as.character(X$individual.gender))
X$years.resident<-as.numeric(as.character(X$years.resident))
X$ed.level<-as.numeric(as.character(X$ed.level))
X$individual.age<-as.numeric(as.character(X$individual.age))

i.ps <-subset(X, select=c("market.distance","dom.eth", "n.child", "occ.dependence", "individual.gender"))
i.ps <- cbind(i.ps, ips)


# ======
# 7.7 Create Tr and By

Tr <- int.xsection.covariate$treatment
MPAID <- as.factor(int.xsection.covariate$MPAID)

# ======
# 7.9 Set variable states for covariate balance
int.xsection.covariate$individual.gender <- as.factor(int.xsection.covariate$individual.gender)
int.xsection.covariate$individual.age <-as.factor(int.xsection.covariate$individual.age)
int.xsection.covariate$years.resident <-as.factor(int.xsection.covariate$years.resident)

# ======
# 7.9 Match

#im1 <- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1)
#caliper3 <-c(100,100,100,1.5,100,0.5)
#im3 <- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper3)
#caliper4 <-c(100,100,100,100,100,0.5)
#im4 <- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper4)
#caliper5 <-c(100,1.5,100,1.5,100,0.5)
#im5 <- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper5)
#caliper6 <-c(100,100,100,1.5,100,1.5)
#im6 <- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper6)
#caliper7 <-c(100,100,100,1.0,100,1.0)
#im7<- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper7)
#caliper8 <-c(100,100,100,1.0,1.5,0.7)
#im8<- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper8)
#caliper9 <-c(100,1.5,100,1.0,1.5,0.7)
#im9<- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper9)

caliper10 <-c(100,1.5,100,1.0,2.0,1.1)
im10<- Matchby(Y=NULL, X=i.ps,Tr=Tr,by=MPAID,replace=TRUE, ties=TRUE,M=1, caliper=caliper10)

# ======
# 7.10 Compute covariate balance

imb1 <- MatchBalance(Tr~ MPAID+market.distance+ dom.eth+n.child+occ.dependence+individual.gender +ed.level+years.resident + individual.age, match.out=im10, data=int.xsection.covariate,ks=TRUE, nboots=100, digits=3, paired=TRUE)

 # ======
# 7.11 Extract dropped cases

index.rows <-seq.int(1:nrow(i.ps))
index.treated <- data.frame(unique(im10$index.treated))
colnames(index.treated) <-c("index.rows")
index.dropped <- subset(int.xsection.covariate, select=c("HouseholdID","SettlementID","MPAID","treatment"))
index.dropped <- cbind(index.rows,index.dropped)
index.dropped <- subset(index.dropped, treatment==1)
index.dropped <-anti_join(index.dropped,index.treated, by="index.rows")
ndrops.settlement <- ddply(index.dropped, .(SettlementID),summarise, ndrops=length(unique(HouseholdID)))
sample.settlement <- subset(rpt.xsection.covariate, select=c("SettlementID", "HouseholdID"))
sample.settlement <- ddply(sample.settlement,.(SettlementID),summarise, sample.size = length(unique(HouseholdID)))
ndrops.settlement <- left_join(ndrops.settlement, sample.settlement, by="SettlementID")
ndrops.settlement <-ddply (ndrops.settlement,.(SettlementID), summarise, percent.drop =(ndrops/sample.size)*100)
ndrops.settlement <- ndrops.settlement[order(ndrops.settlement$SettlementID),]

ndrops.mpa <- ddply(index.dropped, .(MPAID),summarise, ndrops=length(unique(HouseholdID)))
sample.MPA <- subset(int.xsection.covariate, select=c("MPAID", "HouseholdID"))
sample.MPA <- ddply(sample.MPA,.(MPAID),summarise, sample.size = length(unique(HouseholdID)))
ndrops.mpa <- left_join(ndrops.mpa, sample.MPA, by="MPAID")
ndrops.mpa <-ddply (ndrops.mpa,.(MPAID), summarise, percent.drop =(ndrops/sample.size)*100)
ndrops.mpa<- ndrops.mpa[order(ndrops.mpa$MPAID),]

rm(sample.settlement, sample.MPA,index.rows, index.treated, ndrops.mpa, ndrops.settlement, index.dropped)


# ======
# 7.12 Create matrix of matched pairs
i.treat<-im10$index.treated
i.control<-im10$index.control
HouseholdID.MPA<-int.xsection.covariate[i.treat,'HouseholdID']
HouseholdID.Control<-int.xsection.covariate[i.control,'HouseholdID']
MPA.int.xsection.pairs<-data.frame(cbind(HouseholdID.MPA,HouseholdID.Control))

rm(i.treat,i.control,HouseholdID.MPA,HouseholdID.Control)



# ================================================================================================
#
# SECTION 8: Combine matched pairs from all 3 matching routines, and assess missing data.
#
# ================================================================================================

# ======
# 8.1  Create master list of MPA households at t2
master.MPA.t2<-subset(match.covariate,(match.covariate$MPAID==1 & match.covariate$InterviewYear==2012)| (match.covariate$MPAID==2 & match.covariate$InterviewYear==2012) |(match.covariate$MPAID==3 & match.covariate$InterviewYear==2014)|(match.covariate$MPAID==4 & match.covariate$InterviewYear==2013)|(match.covariate$MPAID==5 & match.covariate$InterviewYear==2014)|(match.covariate$MPAID==6 & match.covariate$InterviewYear==2013))
master.MPA.t2 <- subset(master.MPA.t2, (SettlementID!=83 & SettlementID!=91 & SettlementID!=92))
master.MPA.t2 <- subset (master.MPA.t2, treatment==1) 
master.MPA.t2 <- subset (master.MPA.t2, select=c("HouseholdID", "SettlementID", "MPAID"))


# ======
# 8.2 Create master list of households in MPAt2 with matches across all three matching algorithms
MPA.time.matched <-unique(subset(MPA.time.pairs, select=c("HouseholdID.t2")))
MPA.time.matched$BaselineMatch <-1
colnames(MPA.time.matched) <-c("HouseholdID","BaselineMatch")                          

MPA.xsection.matched <-unique(subset(MPA.rpt.xsection.pairs, select=c("HouseholdID.MPA")))
MPA.xsection.matched$XsectionMatch<-1
colnames(MPA.xsection.matched) <-c("HouseholdID","XsectionMatch")   

MPA.int.xsection.matched <-unique(subset(MPA.int.xsection.pairs, select=c("HouseholdID.MPA")))
MPA.int.xsection.matched$IntMatch <- 1
colnames(MPA.int.xsection.matched) <-c("HouseholdID","IntMatch")     

#write.table(master.MPA.t2, "master_MPA_t2.txt", sep=",", row.names = FALSE)
#write.table(MPA.time.matched, "MPA_time_matched.txt", sep=",", row.names = FALSE)
#write.table(MPA.xsection.matched, "MPA_xsection_matched.txt", sep=",", row.names = FALSE)
#write.table(MPA.int.xsection.matched, "MPA_int_section_matched.txt", sep=",", row.names = FALSE)

master.MPA.t2<-left_join(master.MPA.t2,MPA.time.matched, by="HouseholdID")
master.MPA.t2<-left_join(master.MPA.t2,MPA.xsection.matched, by="HouseholdID")
master.MPA.t2<-left_join(master.MPA.t2,MPA.int.xsection.matched, by="HouseholdID")
master.MPA.t2 <- ddply(master.MPA.t2,.(HouseholdID), summarise, count.matches = sum(BaselineMatch+XsectionMatch+IntMatch))
master.MPA.t2 <- subset(master.MPA.t2, count.matches ==3)
master.MPA.t2$count.matches <-NULL


rm(MPA.time.matched, MPA.xsection.matched,MPA.int.xsection.matched)
# ======
# 8.3 Subset matched pairs matrices by master list.
MPA.time.pairs <- left_join(master.MPA.t2,MPA.time.pairs, by=c("HouseholdID"="HouseholdID.t2"))
MPA.rpt.xsection.pairs <- left_join(master.MPA.t2,MPA.rpt.xsection.pairs, by=c("HouseholdID"="HouseholdID.MPA"))
MPA.int.xsection.pairs <- left_join(master.MPA.t2,MPA.int.xsection.pairs, by=c("HouseholdID"="HouseholdID.MPA"))

# ================================================================================================
#
# SECTION 4: Compute  outcomes (big five)
#
# ================================================================================================

# 4.1 Compute MPA outcomes

# ======
# 4.1  Compute MPA outcomes
# Sources code that runs place attachment, household food security, household assets, marine tenure, HFS.

source("2_Social/SourcedScripts/Compute_bigfive_matching.R")

# ======
# 4.2 Source variable outcome function

source('2_Social/SourcedScripts/Function_variable_outcome.R')


# ======
# 4.3 Household food security

# Pre-process data
hfoodsec.outcome <- subset(hfoodsec, select=c("HouseholdID","HFS.Con"))
hfoodsec.outcome <- mutate(hfoodsec.outcome, HFS.Con =6.06 -HFS.Con)
hfoodsec.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
hfoodsec.MPA.t2 <- left_join(hfoodsec.MPA.t2,hfoodsec.outcome,by=c("HouseholdID"))

# Time outcome
hfoodsec.time <-Variable_outcome(MPA.time.pairs,hfoodsec.outcome)

# Treatment outcome
hfoodsec.tr <-Variable_outcome(MPA.rpt.xsection.pairs,hfoodsec.outcome)

# Interaction outcome
hfoodsec.int <-Variable_outcome(MPA.int.xsection.pairs,hfoodsec.outcome)

# Join three paired outcome sets
hfoodsec.ATT <- left_join(hfoodsec.time,hfoodsec.tr, by="HouseholdID")
hfoodsec.ATT <- left_join(hfoodsec.ATT, hfoodsec.int, by="HouseholdID")

# Join to MPA at t2 data
hfoodsec.ATT <- left_join(hfoodsec.ATT,hfoodsec.MPA.t2, by="HouseholdID")
colnames(hfoodsec.ATT) <- c("HouseholdID", "HFS.mpa.t0","HFS.c.t2","HFS.c.t0", "HFS.mpa.t2")

# Compute outcomes
hfoodsec.ATT <- mutate(hfoodsec.ATT, MPA.outcome=(HFS.mpa.t2 -HFS.mpa.t0))
hfoodsec.ATT <- mutate(hfoodsec.ATT, control.outcome=(HFS.c.t2 -HFS.c.t0))

# Compute treatment effect
hfoodsec.ATT <- mutate(hfoodsec.ATT, ATT= (HFS.mpa.t2 - HFS.mpa.t0)-(HFS.c.t2 - HFS.c.t0))
hfoodsec.ATT <- na.omit (hfoodsec.ATT)

rm(hfoodsec.time, hfoodsec.tr, hfoodsec.int, hfoodsec.MPA.t2, hfoodsec.outcome, hfoodsec)

# ======
# 4.4 Household material assets

# Pre-process data
assets.outcome <- subset(household.assets, select=c("HouseholdID","asset.index"))
assets.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
assets.MPA.t2 <- left_join(assets.MPA.t2,assets.outcome,by=c("HouseholdID"))

# Time outcome
assets.time <-Variable_outcome(MPA.time.pairs,assets.outcome)

# Treatment outcome
assets.tr <-Variable_outcome(MPA.rpt.xsection.pairs,assets.outcome)

# Interaction outcome
assets.int <-Variable_outcome(MPA.int.xsection.pairs,assets.outcome)

# Join three outcome sets
assets.ATT <- left_join(assets.time,assets.tr, by="HouseholdID")
assets.ATT <- left_join(assets.ATT, assets.int, by="HouseholdID")

# Join to MPA at t2 data
assets.ATT <- left_join(assets.ATT,assets.MPA.t2, by="HouseholdID")
colnames(assets.ATT) <- c("HouseholdID", "assets.mpa.t0","assets.c.t2","assets.c.t0", "assets.mpa.t2")

# Compute outcomes
assets.ATT <- mutate(assets.ATT, MPA.outcome=(assets.mpa.t2 -assets.mpa.t0))
assets.ATT <- mutate(assets.ATT, control.outcome=(assets.c.t2 -assets.c.t0))

# Compute treatment effect
assets.ATT <- mutate(assets.ATT, ATT= (assets.mpa.t2 - assets.mpa.t0)-(assets.c.t2 - assets.c.t0))
assets.ATT <- na.omit (assets.ATT)

rm(assets.time, assets.tr, assets.int, assets.MPA.t2, assets.outcome)


# ======
# 4.5 Household marine tenure

# Pre-process data
tenure.outcome <- subset(marine.tenure.index, select=c("HouseholdID","Rights"))
tenure.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
tenure.MPA.t2 <- left_join(tenure.MPA.t2,tenure.outcome,by=c("HouseholdID"))

# Time outcome
tenure.time <-Variable_outcome(MPA.time.pairs,tenure.outcome)

# Treatment outcome
tenure.tr <-Variable_outcome(MPA.rpt.xsection.pairs,tenure.outcome)

# Interaction outcome
tenure.int <-Variable_outcome(MPA.int.xsection.pairs,tenure.outcome)

# Join three outcome sets
tenure.ATT <- left_join(tenure.time,tenure.tr, by="HouseholdID")
tenure.ATT <- left_join(tenure.ATT, tenure.int, by="HouseholdID")

# Join to MPA at t2 data
tenure.ATT <- left_join(tenure.ATT,tenure.MPA.t2, by="HouseholdID")
colnames(tenure.ATT) <- c("HouseholdID", "tenure.mpa.t0","tenure.c.t2","tenure.c.t0", "tenure.mpa.t2")

# Compute outcomes
tenure.ATT <- mutate(tenure.ATT, MPA.outcome=(tenure.mpa.t2 -tenure.mpa.t0))
tenure.ATT <- mutate(tenure.ATT, control.outcome=(tenure.c.t2 -tenure.c.t0))

# Compute treatment effect
tenure.ATT <- mutate(tenure.ATT, ATT= (tenure.mpa.t2 - tenure.mpa.t0)-(tenure.c.t2 - tenure.c.t0))
tenure.ATT <- na.omit (tenure.ATT)

rm(tenure.time, tenure.tr, tenure.int, tenure.MPA.t2, tenure.outcome, marine.tenure.index)

# ======
# 4.6 Place attachment

# Pre-process data
attach.outcome <- subset(place.attach.index, select=c("HouseholdID","P.Attach"))
attach.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
attach.MPA.t2 <- left_join(attach.MPA.t2,attach.outcome,by=c("HouseholdID"))

# Time outcome
attach.time <-Variable_outcome(MPA.time.pairs,attach.outcome)

# Treatment outcome
attach.tr <-Variable_outcome(MPA.rpt.xsection.pairs,attach.outcome)

# Interaction outcome
attach.int <-Variable_outcome(MPA.int.xsection.pairs,attach.outcome)

# Join three outcome sets
attach.ATT <- left_join(attach.time,attach.tr, by="HouseholdID")
attach.ATT <- left_join(attach.ATT, attach.int, by="HouseholdID")

# Join to MPA at t2 data
attach.ATT <- left_join(attach.ATT,attach.MPA.t2, by="HouseholdID")
colnames(attach.ATT) <- c("HouseholdID", "attach.mpa.t0","attach.c.t2","attach.c.t0", "attach.mpa.t2")

# Compute outcomes
attach.ATT <- mutate(attach.ATT, MPA.outcome=(attach.mpa.t2 -attach.mpa.t0))
attach.ATT <- mutate(attach.ATT, control.outcome=(attach.c.t2 -attach.c.t0))

# Compute treatment effect
attach.ATT <- mutate(attach.ATT, ATT= (attach.mpa.t2 - attach.mpa.t0)-(attach.c.t2 - attach.c.t0))
attach.ATT <- na.omit (attach.ATT)

rm(attach.time, attach.tr, attach.int, attach.MPA.t2, attach.outcome, place.attach.index)


# ======
# 4.6 School enrollment

# Pre-process data
enrol.outcome <- subset(Enrol, select=c("HouseholdID","Percent.Enrolled"))
enrol.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
enrol.MPA.t2 <- left_join(enrol.MPA.t2,enrol.outcome,by=c("HouseholdID"))
n.child <- subset(match.covariate, select=c("HouseholdID", "n.child"))
enrol.MPA.t2 <-left_join(enrol.MPA.t2,n.child, by="HouseholdID")
enrol.MPA.t2 <- subset(enrol.MPA.t2, n.child>0)
enrol.MPA.t2$n.child <- NULL


# Time outcome
enrol.time <-Variable_outcome(MPA.time.pairs,enrol.outcome)

# Treatment outcome
enrol.tr <-Variable_outcome(MPA.rpt.xsection.pairs,enrol.outcome)

# Interaction outcome
enrol.int <-Variable_outcome(MPA.int.xsection.pairs,enrol.outcome)

# Join three outcome sets
enrol.ATT <- left_join(enrol.time,enrol.tr, by="HouseholdID")
enrol.ATT <- left_join(enrol.ATT, enrol.int, by="HouseholdID")

# Join to MPA at t2 data
enrol.ATT <- left_join(enrol.ATT,enrol.MPA.t2, by="HouseholdID")
colnames(enrol.ATT) <- c("HouseholdID", "enrol.mpa.t0","enrol.c.t2","enrol.c.t0", "enrol.mpa.t2")

# Compute outcomes
enrol.ATT <- mutate(enrol.ATT, MPA.outcome=(enrol.mpa.t2 -enrol.mpa.t0))
enrol.ATT <- mutate(enrol.ATT, control.outcome=(enrol.c.t2 -enrol.c.t0))

# Compute treatment effect
enrol.ATT <- mutate(enrol.ATT, ATT= (enrol.mpa.t2 - enrol.mpa.t0)-(enrol.c.t2 - enrol.c.t0))
enrol.ATT <- na.omit (enrol.ATT)

rm(enrol.time, enrol.tr, enrol.int, enrol.MPA.t2, enrol.outcome, Enrol)


# ================================================================================================
#
# SECTION 9: Analysis of treatment effects
#
# ================================================================================================

# ======
# 9.1 Compute wealth quntiles
assets.outcome <- subset(household.assets, select=c("HouseholdID","asset.index"))
wealth.quintiles <- subset(MPA.time.pairs, select=c("HouseholdID"))
wealth.quintiles <- left_join(wealth.quintiles,assets.outcome,by=c("HouseholdID"))
wealth.quintiles$quintile <- with(wealth.quintiles,cut2(asset.index, g=5))
wealth.quintiles$quintile <- as.numeric(wealth.quintiles$quintile)

# ======
# 9.2 Join with MPA and other explanatory variables
# include  fisher/non-fisher, gender HHH,and wealth quntile
covariates <- left_join(MPA.time.pairs,wealth.quintiles, by="HouseholdID")
covariates <- left_join(covariates, match.covariate, by="HouseholdID")
covariates <- subset(covariates, select=c("HouseholdID","quintile","fisher", "individual.gender", "MPAID", "SettlementID"))

# ======
# 9.3 Create bias adjustment matrix
# include dominant ethnicity, occ.dependence, gender, market distance
bias.adjust <- left_join (MPA.time.pairs, match.covariate, by="HouseholdID")
bias.adjust <- subset(bias.adjust, select = c("HouseholdID", "dom.eth","occ.dependence", "market.distance"))

# ======
# 9.4 Household food security
dummy.match <- left_join(hfoodsec.ATT,bias.adjust, by="HouseholdID")
dummy.match <- unique(left_join (dummy.match, covariates, by="HouseholdID"))
dummy.match.mpa <- subset(dummy.match, select=c("HouseholdID","MPA.outcome","dom.eth", "occ.dependence", "market.distance", "MPAID"))
dummy.match.mpa$Tr <-1
dummy.match.control <- subset(dummy.match, select=c("HouseholdID","control.outcome","dom.eth", "occ.dependence", "market.distance","MPAID"))
dummy.match.control$Tr <- 0
colnames (dummy.match.mpa) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance","MPAID","Tr")
colnames (dummy.match.control) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance", "MPAID","Tr")
dummy.match<- rbind(dummy.match.mpa,dummy.match.control)

# Seascape
Y <- dummy.match$outcome
X <-dummy.match$HouseholdID
Tr <- dummy.match$Tr
Z <- subset(dummy.match, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.seascape <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
hfs.seascape.sens <- psens(hfs.seascape, Gamma =2, GammaInc = 0.1)

# Kaimana MPA
kaimana <- subset(dummy.match, MPAID==3)
Y <- kaimana$outcome
X <-kaimana$HouseholdID
Tr <- kaimana$Tr
Z <- subset(kaimana, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.kaimana <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
hfs.kaimana.sens <- psens(hfs.kaimana, Gamma =2, GammaInc = 0.1)

# Kofiau MPA
kofiau <- subset(dummy.match, MPAID==4)
Y <- kofiau$outcome
X <-kofiau$HouseholdID
Tr <- kofiau$Tr
Z <- subset(kofiau, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.kofiau <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
hfs.kofiau.sens <- psens(hfs.kofiau, Gamma =2, GammaInc = 0.1)

# Misool MPA
misool <- subset(dummy.match, MPAID==6)
Y <- misool$outcome
X <-misool$HouseholdID
Tr <- misool$Tr
Z <- subset(misool, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.misool <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
hfs.misool.sens <- psens(hfs.misool, Gamma =2, GammaInc = 0.1)

# T. Cenderawasih MPA
tntc <- subset(dummy.match, MPAID==2)
Y <- tntc$outcome
X <-tntc$HouseholdID
Tr <- tntc$Tr
Z <- subset(tntc, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.tntc <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# T. Mayalibit MPA
telma <- subset(dummy.match, MPAID==1)
Y <- telma$outcome
X <-telma$HouseholdID
Tr <- telma$Tr
Z <- subset(telma, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.telma <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
hfs.telma.sens <- psens(hfs.telma, Gamma =2, GammaInc = 0.1)

# S. Dampier MPA
dampier <- subset(dummy.match, MPAID==5)
Y <- dampier$outcome
X <-dampier$HouseholdID
Tr <- dampier$Tr
Z <- subset(dampier, select=c("dom.eth", "occ.dependence", "market.distance"))
hfs.dampier <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
hfs.dampier.sens <- psens(hfs.dampier, Gamma =2, GammaInc = 0.1)


# ======
# 9.5 Household material assets
dummy.match <- left_join(assets.ATT,bias.adjust, by="HouseholdID")
dummy.match <- unique(left_join (dummy.match, covariates, by="HouseholdID"))
dummy.match.mpa <- subset(dummy.match, select=c("HouseholdID","MPA.outcome","dom.eth", "occ.dependence", "market.distance", "MPAID"))
dummy.match.mpa$Tr <-1
dummy.match.control <- subset(dummy.match, select=c("HouseholdID","control.outcome","dom.eth", "occ.dependence", "market.distance","MPAID"))
dummy.match.control$Tr <- 0
colnames (dummy.match.mpa) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance","MPAID","Tr")
colnames (dummy.match.control) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance", "MPAID","Tr")
dummy.match<- rbind(dummy.match.mpa,dummy.match.control)

# Seascape
Y <- dummy.match$outcome
X <-dummy.match$HouseholdID
Tr <- dummy.match$Tr
Z <- subset(dummy.match, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.seascape <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
assets.seascape.sens <- psens(assets.seascape, Gamma =2, GammaInc = 0.1)

# Kaimana MPA
kaimana <- subset(dummy.match, MPAID==3)
Y <- kaimana$outcome
X <-kaimana$HouseholdID
Tr <- kaimana$Tr
Z <- subset(kaimana, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.kaimana <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
assets.kaimana.sens <- psens(assets.kaimana, Gamma =2, GammaInc = 0.1)

# Kofiau MPA
kofiau <- subset(dummy.match, MPAID==4)
Y <- kofiau$outcome
X <-kofiau$HouseholdID
Tr <- kofiau$Tr
Z <- subset(kofiau, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.kofiau <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# Misool MPA
misool <- subset(dummy.match, MPAID==6)
Y <- misool$outcome
X <-misool$HouseholdID
Tr <- misool$Tr
Z <- subset(misool, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.misool <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
assets.misool.sens <- psens(assets.misool, Gamma =2, GammaInc = 0.1)

# T. Cenderawasih MPA
tntc <- subset(dummy.match, MPAID==2)
Y <- tntc$outcome
X <-tntc$HouseholdID
Tr <- tntc$Tr
Z <- subset(tntc, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.tntc <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# T. Mayalibit MPA
telma <- subset(dummy.match, MPAID==1)
Y <- telma$outcome
X <-telma$HouseholdID
Tr <- telma$Tr
Z <- subset(telma, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.telma <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
assets.telma.sens <- psens(assets.telma, Gamma =2, GammaInc = 0.1)

# S. Dampier MPA
dampier <- subset(dummy.match, MPAID==5)
Y <- dampier$outcome
X <-dampier$HouseholdID
Tr <- dampier$Tr
Z <- subset(dampier, select=c("dom.eth", "occ.dependence", "market.distance"))
assets.dampier <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
assets.dampier.sens <- psens(assets.dampier, Gamma =2, GammaInc = 0.1)

# ======
# 9.6 Household marine tenure
dummy.match <- left_join(tenure.ATT,bias.adjust, by="HouseholdID")
dummy.match <- unique(left_join (dummy.match, covariates, by="HouseholdID"))
dummy.match.mpa <- subset(dummy.match, select=c("HouseholdID","MPA.outcome","dom.eth", "occ.dependence", "market.distance", "MPAID"))
dummy.match.mpa$Tr <-1
dummy.match.control <- subset(dummy.match, select=c("HouseholdID","control.outcome","dom.eth", "occ.dependence", "market.distance","MPAID"))
dummy.match.control$Tr <- 0
colnames (dummy.match.mpa) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance","MPAID","Tr")
colnames (dummy.match.control) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance", "MPAID","Tr")
dummy.match<- rbind(dummy.match.mpa,dummy.match.control)

# Seascape
Y <- dummy.match$outcome
X <-dummy.match$HouseholdID
Tr <- dummy.match$Tr
Z <- subset(dummy.match, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.seascape <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
tenure.seascape.sens <- psens(tenure.seascape, Gamma =2, GammaInc = 0.1)

# Kaimana MPA
kaimana <- subset(dummy.match, MPAID==3)
Y <- kaimana$outcome
X <-kaimana$HouseholdID
Tr <- kaimana$Tr
Z <- subset(kaimana, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.kaimana <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
tenure.kaimana.sens <- psens(tenure.kaimana, Gamma =2, GammaInc = 0.1)

# Kofiau MPA
kofiau <- subset(dummy.match, MPAID==4)
Y <- kofiau$outcome
X <-kofiau$HouseholdID
Tr <- kofiau$Tr
Z <- subset(kofiau, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.kofiau <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# Misool MPA
misool <- subset(dummy.match, MPAID==6)
Y <- misool$outcome
X <-misool$HouseholdID
Tr <- misool$Tr
Z <- subset(misool, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.misool <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
tenure.misool.sens <- psens(tenure.misool, Gamma =2, GammaInc = 0.1)

# T. Cenderawasih MPA
tntc <- subset(dummy.match, MPAID==2)
Y <- tntc$outcome
X <-tntc$HouseholdID
Tr <- tntc$Tr
Z <- subset(tntc, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.tntc <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
tenure.tntc.sens <- psens(tenure.tntc, Gamma =2, GammaInc = 0.1)

# T. Mayalibit MPA
telma <- subset(dummy.match, MPAID==1)
Y <- telma$outcome
X <-telma$HouseholdID
Tr <- telma$Tr
Z <- subset(telma, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.telma <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
tenure.telma.sens <- psens(tenure.telma, Gamma =2, GammaInc = 0.1)

# S. Dampier MPA
dampier <- subset(dummy.match, MPAID==5)
Y <- dampier$outcome
X <-dampier$HouseholdID
Tr <- dampier$Tr
Z <- subset(dampier, select=c("dom.eth", "occ.dependence", "market.distance"))
tenure.dampier <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
tenure.dampier.sens <- psens(tenure.dampier, Gamma =2, GammaInc = 0.1)

# ======
# Place attachment
dummy.match <- left_join(attach.ATT,bias.adjust, by="HouseholdID")
dummy.match <- unique(left_join (dummy.match, covariates, by="HouseholdID"))
dummy.match.mpa <- subset(dummy.match, select=c("HouseholdID","MPA.outcome","dom.eth", "occ.dependence", "market.distance", "MPAID"))
dummy.match.mpa$Tr <-1
dummy.match.control <- subset(dummy.match, select=c("HouseholdID","control.outcome","dom.eth", "occ.dependence", "market.distance","MPAID"))
dummy.match.control$Tr <- 0
colnames (dummy.match.mpa) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance","MPAID","Tr")
colnames (dummy.match.control) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance", "MPAID","Tr")
dummy.match<- rbind(dummy.match.mpa,dummy.match.control)

# Seascape
Y <- dummy.match$outcome
X <-dummy.match$HouseholdID
Tr <- dummy.match$Tr
Z <- subset(dummy.match, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.seascape <- Match(Y=Y,X=X,Tr=Tr,Z=Z)
attach.seascape.sens <- psens(attach.seascape, Gamma =2, GammaInc = 0.1)

# Kaimana MPA
kaimana <- subset(dummy.match, MPAID==3)
Y <- kaimana$outcome
X <-kaimana$HouseholdID
Tr <- kaimana$Tr
Z <- subset(kaimana, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.kaimana <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# Kofiau MPA
kofiau <- subset(dummy.match, MPAID==4)
Y <- kofiau$outcome
X <-kofiau$HouseholdID
Tr <- kofiau$Tr
Z <- subset(kofiau, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.kofiau <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# Misool MPA
misool <- subset(dummy.match, MPAID==6)
Y <- misool$outcome
X <-misool$HouseholdID
Tr <- misool$Tr
Z <- subset(misool, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.misool <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# T. Cenderawasih MPA
tntc <- subset(dummy.match, MPAID==2)
Y <- tntc$outcome
X <-tntc$HouseholdID
Tr <- tntc$Tr
Z <- subset(tntc, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.tntc <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
attach.tntc.sens <- psens(attach.tntc, Gamma =2, GammaInc = 0.1)

# T. Mayalibit MPA
telma <- subset(dummy.match, MPAID==1)
Y <- telma$outcome
X <-telma$HouseholdID
Tr <- telma$Tr
Z <- subset(telma, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.telma <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
attach.telma.sens <- psens(attach.telma, Gamma =2, GammaInc = 0.1)

# S. Dampier MPA
dampier <- subset(dummy.match, MPAID==5)
Y <- dampier$outcome
X <-dampier$HouseholdID
Tr <- dampier$Tr
Z <- subset(dampier, select=c("dom.eth", "occ.dependence", "market.distance"))
attach.dampier <- Match(Y=Y,X=X,Tr=Tr,Z=Z )



# ======
# 9.8 Enrolment
dummy.match <- left_join(enrol.ATT,bias.adjust, by="HouseholdID")
dummy.match <- unique(left_join (dummy.match, covariates, by="HouseholdID"))
dummy.match.mpa <- subset(dummy.match, select=c("HouseholdID","MPA.outcome","dom.eth", "occ.dependence", "market.distance", "MPAID"))
dummy.match.mpa$Tr <-1
dummy.match.control <- subset(dummy.match, select=c("HouseholdID","control.outcome","dom.eth", "occ.dependence", "market.distance","MPAID"))
dummy.match.control$Tr <- 0
colnames (dummy.match.mpa) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance","MPAID","Tr")
colnames (dummy.match.control) <-c("HouseholdID","outcome","dom.eth", "occ.dependence", "market.distance", "MPAID","Tr")
dummy.match<- rbind(dummy.match.mpa,dummy.match.control)

# Seascape
Y <- dummy.match$outcome
X <-dummy.match$HouseholdID
Tr <- dummy.match$Tr
Z <- subset(dummy.match, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.seascape <- Match(Y=Y,X=X,Tr=Tr,Z=Z)
 

# Kaimana MPA
kaimana <- subset(dummy.match, MPAID==3)
Y <- kaimana$outcome
X <-kaimana$HouseholdID
Tr <- kaimana$Tr
Z <- subset(kaimana, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.kaimana <- Match(Y=Y,X=X,Tr=Tr,Z=Z )


# Kofiau MPA
kofiau <- subset(dummy.match, MPAID==4)
Y <- kofiau$outcome
X <-kofiau$HouseholdID
Tr <- kofiau$Tr
Z <- subset(kofiau, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.kofiau <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
enrol.kofiau.sens <- psens(enrol.kofiau, Gamma =2, GammaInc = 0.1)

# Misool MPA
misool <- subset(dummy.match, MPAID==6)
Y <- misool$outcome
X <-misool$HouseholdID
Tr <- misool$Tr
Z <- subset(misool, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.misool <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# T. Cenderawasih MPA
tntc <- subset(dummy.match, MPAID==2)
Y <- tntc$outcome
X <-tntc$HouseholdID
Tr <- tntc$Tr
Z <- subset(tntc, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.tntc <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
enrol.tntc.sens <- psens(enrol.tntc, Gamma =2, GammaInc = 0.1)

# T. Mayalibit MPA
telma <- subset(dummy.match, MPAID==1)
Y <- telma$outcome
X <-telma$HouseholdID
Tr <- telma$Tr
Z <- subset(telma, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.telma <- Match(Y=Y,X=X,Tr=Tr,Z=Z )
enrol.telma.sens <- psens(enrol.telma, Gamma =2, GammaInc = 0.1)

# S. Dampier MPA
dampier <- subset(dummy.match, MPAID==5)
Y <- dampier$outcome
X <-dampier$HouseholdID
Tr <- dampier$Tr
Z <- subset(dampier, select=c("dom.eth", "occ.dependence", "market.distance"))
enrol.dampier <- Match(Y=Y,X=X,Tr=Tr,Z=Z )

# p values for MPA-level ATTs
options("scipen"=100,"digits"=4)
p.vals.dampier.2yr <- data.frame(c("MA","FS","MT","PA","SE"),
                             sapply(list(assets.dampier,hfs.dampier,tenure.dampier,
                                         attach.dampier,enrol.dampier),
                                    function(i){
                                      (1 - pnorm(abs(i$est/i$se))) * 2
                                      }))
colnames(p.vals.dampier.2yr) <- c("Indicator","p.val")
p.vals.dampier.2yr$Indicator <- factor(p.vals.dampier.2yr$Indicator,
                                   levels=c("PA","SE","MT","FS","MA"),
                                   ordered=T)


p.vals.mayalibit.2yr <- data.frame(c("MA","FS","MT","PA","SE"),
                               sapply(list(assets.telma,hfs.telma,tenure.telma,
                                           attach.telma,enrol.telma),
                                      function(i){
                                        (1 - pnorm(abs(i$est/i$se))) * 2
                                      }))
colnames(p.vals.mayalibit.2yr) <- c("Indicator","p.val")
p.vals.mayalibit.2yr$Indicator <- factor(p.vals.mayalibit.2yr$Indicator,
                                   levels=c("PA","SE","MT","FS","MA"),
                                   ordered=T)

p.vals.kofiau.2yr <- data.frame(c("MA","FS","MT","PA","SE"),
                            sapply(list(assets.kofiau,hfs.kofiau,tenure.kofiau,
                                        attach.kofiau,enrol.kofiau),
                                   function(i){
                                     (1 - pnorm(abs(i$est/i$se))) * 2
                                   }))
colnames(p.vals.kofiau.2yr) <- c("Indicator","p.val")
p.vals.kofiau.2yr$Indicator <- factor(p.vals.kofiau.2yr$Indicator,
                                   levels=c("PA","SE","MT","FS","MA"),
                                   ordered=T)

p.vals.tntc.2yr <- data.frame(c("MA","FS","MT","PA","SE"),
                          sapply(list(assets.tntc,hfs.tntc,tenure.tntc,
                                      attach.tntc,enrol.tntc),
                                 function(i){
                                   (1 - pnorm(abs(i$est/i$se))) * 2
                                 }))
colnames(p.vals.tntc.2yr) <- c("Indicator","p.val")
p.vals.tntc.2yr$Indicator <- factor(p.vals.tntc.2yr$Indicator,
                                   levels=c("PA","SE","MT","FS","MA"),
                                   ordered=T)

p.vals.misool.2yr <- data.frame(c("MA","FS","MT","PA","SE"),
                            sapply(list(assets.misool,hfs.misool,tenure.misool,
                                        attach.misool,enrol.misool),
                                   function(i){
                                     (1 - pnorm(abs(i$est/i$se))) * 2
                                   }))
colnames(p.vals.misool.2yr) <- c("Indicator","p.val")
p.vals.misool.2yr$Indicator <- factor(p.vals.misool.2yr$Indicator,
                                   levels=c("PA","SE","MT","FS","MA"),
                                   ordered=T)

p.vals.kaimana.2yr <- data.frame(c("MA","FS","MT","PA","SE"),
                             sapply(list(assets.kaimana,hfs.kaimana,tenure.kaimana,
                                         attach.kaimana,enrol.kaimana),
                                    function(i){
                                      (1 - pnorm(abs(i$est/i$se))) * 2
                                    }))
colnames(p.vals.kaimana.2yr) <- c("Indicator","p.val")
p.vals.kaimana.2yr$Indicator <- factor(p.vals.kaimana.2yr$Indicator,
                                   levels=c("PA","SE","MT","FS","MA"),
                                   ordered=T)

p.vals.bhs <- data.frame(c("MA","FS","MT","PA","SE"),
                         sapply(list(assets.seascape,hfs.seascape,tenure.seascape,
                                     attach.seascape,enrol.seascape),
                                function(i){
                                  (1-pnorm(abs(i$est/i$se))) * 2
                                }))
colnames(p.vals.bhs) <- c("Indicator","p.val")
p.vals.bhs$Indicator <- factor(p.vals.bhs$Indicator,
                               levels=c("PA","SE","MT","FS","MA"),
                               ordered=T)
# 
# # ================================================================================================
# #
# # SECTION 10: Compute standardized mean differences (SMD) and smd.ci
# #
# # ================================================================================================
# # 10.1 Household food security
# hfs.smd <- smd(Group.1 =hfoodsec.ATT$MPA.outcome, Group.2=hfoodsec.ATT$control.outcome)
# n.1 <- nrow(hfoodsec.ATT)
# n.2 <-nrow(hfoodsec.ATT)
# hfs.ci <- ci.smd(n.1=n.1, n.2=n.2, conf.level=0.95, smd=hfs.smd)
# 
# # 10.2 Household marine tenure
# tenure.smd <- smd(Group.1 =tenure.ATT$MPA.outcome, Group.2=tenure.ATT$control.outcome)
# n.1 <- nrow(tenure.ATT)
# n.2 <-nrow(tenure.ATT)
# tenure.ci <- ci.smd(n.1=n.1, n.2=n.2, conf.level=0.95, smd=tenure.smd)
# 
# # 10.3 Household material assets
# assets.smd <- smd(Group.1 =assets.ATT$MPA.outcome, Group.2=assets.ATT$control.outcome)
# n.1 <- nrow(assets.ATT)
# n.2 <-nrow(assets.ATT)
# assets.ci <- ci.smd(n.1=n.1, n.2=n.2, conf.level=0.95, smd=assets.smd)
# 
# # 10.4 Place attachment
# attach.smd <- smd(Group.1 =attach.ATT$MPA.outcome, Group.2=attach.ATT$control.outcome)
# n.1 <- nrow(attach.ATT)
# n.2 <-nrow(attach.ATT)
# attach.ci <- ci.smd(n.1=n.1, n.2=n.2, conf.level=0.95, smd=attach.smd)
# 
# # 10.5 Enrollment
# enrol.smd <- smd(Group.1 =enrol.ATT$MPA.outcome, Group.2=enrol.ATT$control.outcome)
# n.1 <- nrow(enrol.ATT)
# n.2 <-nrow(enrol.ATT)
# enrol.ci <- ci.smd(n.1=n.1, n.2=n.2, conf.level=0.95, smd=enrol.smd)
# 
# # Create output table
# Outcome <-c("hfoodsec","assets","tenure","place.attach", "enrol")
# CI <-rbind(hfs.ci, assets.ci, tenure.ci, attach.ci, enrol.ci)
# std.diff.seascape <-cbind(Outcome,CI)
# write.table(std.diff.seascape, "5_SOCIAL_IMPACTS2YR/outputs/std_diff_seascape.txt", sep=",",row.names=FALSE)
# rm(Outcome,SMD, CI,hfs.smd,assets.smd,tenure.smd,attach.smd,enrol.smd,hfs.ci, assets.ci, tenure.ci, attach.ci, enrol.ci)
# 
# # MPA level standardized mean differences
# 
# MPAID <- subset(covariates, select=c("HouseholdID", "MPAID"))
# hfoodsec.ATT <- left_join(hfoodsec.ATT, MPAID, by="HouseholdID")
# hfs.SMD.MPA <- smd_ci_group(hfoodsec.ATT,conf=0.95)
# 
# # hfoodsec.ATT <- group_by(hfoodsec.ATT,MPAID)
# # hfs.SMD.MPA <- summarise(hfoodsec.ATT,
# #                          hfs.smd=smd(Group.1=MPA.outcome, Group.2=control.outcome))
# # assets.ATT <- left_join(assets.ATT, MPAID, by="HouseholdID")
# # assets.ATT <- group_by(assets.ATT,MPAID)
# # assets.SMD.MPA <- summarise(assets.ATT,
# #                          assets.smd=smd(Group.1=MPA.outcome, Group.2=control.outcome))
# # tenure.ATT <- left_join(tenure.ATT, MPAID, by="HouseholdID")
# # tenure.ATT <- group_by(tenure.ATT,MPAID)
# # tenure.SMD.MPA <- summarise(tenure.ATT,
# #                          tenure.smd=smd(Group.1=MPA.outcome, Group.2=control.outcome))
# # enrol.ATT <- left_join(enrol.ATT, MPAID, by="HouseholdID")
# # enrol.ATT <- group_by(enrol.ATT,MPAID)
# # enrol.SMD.MPA <- summarise(enrol.ATT,
# #                          enrol.smd=smd(Group.1=MPA.outcome, Group.2=control.outcome))
# # attach.ATT <- left_join(attach.ATT, MPAID, by="HouseholdID")
# # attach.ATT <- group_by(attach.ATT,MPAID)
# # attach.SMD.MPA <- summarise(attach.ATT,
# #                          attach.smd=smd(Group.1=MPA.outcome, Group.2=control.outcome))
# 
# assets.ATT <- left_join(assets.ATT, MPAID, by= "HouseholdID")
# assets.SMD.MPA <- smd_ci_group(assets.ATT,conf=0.95)
# 
# tenure.ATT <- left_join(tenure.ATT, MPAID, by= "HouseholdID")
# tenure.SMD.MPA <- smd_ci_group(tenure.ATT,conf=0.95)
# 
# enrol.ATT <- left_join(enrol.ATT, MPAID, by= "HouseholdID")
# enrol.SMD.MPA <- smd_ci_group(enrol.ATT,conf=0.95)
# 
# attach.ATT <- left_join(attach.ATT, MPAID, by= "HouseholdID")
# attach.SMD.MPA <- smd_ci_group(attach.ATT,conf=0.95)
# 
# # ================================================================================================
# #
# # SECTION 8: Typology of impacts
# #
# # ================================================================================================
# 
# ## Typology
#                                                                                                       
# 
# save.xlsx("typology_seascape.xlsx", hfs.typology,assets.typology,tenure.typology,attach.typology,enrol.typology)
# 
# 
# 
# 
# ## Typology by MPA
# hfs.typology <-ddply(hfoodsec.ATT, .(MPAID), summarise, MPA.mean=mean(MPA.outcome),control.mean=mean(control.outcome))
# assets.typology <-ddply(assets.ATT, .(MPAID), summarise, MPA.mean=mean(MPA.outcome),control.mean=mean(control.outcome))
# tenure.typology <-ddply(tenure.ATT, .(MPAID), summarise, MPA.mean=mean(MPA.outcome),control.mean=mean(control.outcome))
# attach.typology <-ddply(attach.ATT, .(MPAID), summarise, MPA.mean=mean(MPA.outcome),control.mean=mean(control.outcome))
# enrol.typology <-ddply(enrol.ATT, .(MPAID), summarise, MPA.mean=mean(MPA.outcome),control.mean=mean(control.outcome))
# save.xlsx("typology_by_MPA.xlsx", hfs.typology,assets.typology,tenure.typology,attach.typology,enrol.typology)
# 
# 
# # ================================================================================================
# #
# # SECTION 8: Sub-group treatment effects 
# #
# # ================================================================================================
# library(lme4)
# library(multcomp)
# hfoodsec.subgroup <- left_join(hfoodsec.ATT,covariates, by="HouseholdID")
# hfoodsec.subgroup$fisher <-as.factor(hfoodsec.subgroup$fisher)
# hfoodsec.subgroup$individual.gender <-as.factor(hfoodsec.subgroup$individual.gender)
# hfoodsec.subgroup$quintile <-as.factor(hfoodsec.subgroup$quintile)
# 
# hfoodsec.subgroup.ATT <- lmer(ATT~(1|MPAID.x)+(1|SettlementID)+ fisher + individual.gender + quintile,data=hfoodsec.subgroup)
# summary(glht(hfoodsec.subgroup.ATT,linfct=mcp(fisher="Tukey")))
# summary(glht(hfoodsec.subgroup.ATT,linfct=mcp(individual.gender="Tukey")))
# summary(glht(hfoodsec.subgroup.ATT,linfct=mcp(quintile="Tukey")))
# 
# plot(fitted(hfoodsec.subgroup.ATT), residuals(hfoodsec.subgroup.ATT), xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, lty = 2)
# lines(smooth.spline(fitted(hfoodsec.subgroup.ATT), residuals(hfoodsec.subgroup.ATT)))
# 
# 
# tenure.subgroup <- left_join(tenure.ATT,covariates, by="HouseholdID")
# tenure.subgroup$fisher <-as.factor(tenure.subgroup$fisher)
# tenure.subgroup$individual.gender <-as.factor(tenure.subgroup$individual.gender)
# tenure.subgroup$quintile <-as.factor(tenure.subgroup$quintile)
# 
# tenure.subgroup.ATT <- lmer(ATT~(1|MPAID.x)+(1|SettlementID)+ fisher + individual.gender + quintile,data=tenure.subgroup)
# summary(glht(tenure.subgroup.ATT,linfct=mcp(fisher="Tukey")))
# summary(glht(tenure.subgroup.ATT,linfct=mcp(individual.gender="Tukey")))
# summary(glht(tenure.subgroup.ATT,linfct=mcp(quintile="Tukey")))
# 
# plot(fitted(tenure.subgroup.ATT), residuals(tenure.subgroup.ATT), xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, lty = 2)
# lines(smooth.spline(fitted(tenure.subgroup.ATT), residuals(tenure.subgroup.ATT)))
# 
# assets.subgroup <- left_join(assets.ATT,covariates, by="HouseholdID")
# assets.subgroup$fisher <-as.factor(assets.subgroup$fisher)
# assets.subgroup$individual.gender <-as.factor(assets.subgroup$individual.gender)
# assets.subgroup$quintile <-as.factor(assets.subgroup$quintile)
# 
# assets.subgroup.ATT <- lmer(ATT~(1|MPAID.x)+(1|SettlementID)+ fisher + individual.gender + quintile,data=assets.subgroup)
# summary(glht(assets.subgroup.ATT,linfct=mcp(fisher="Tukey")))
# summary(glht(assets.subgroup.ATT,linfct=mcp(individual.gender="Tukey")))
# summary(glht(assets.subgroup.ATT,linfct=mcp(quintile="Tukey")))
# 
# plot(fitted(assets.subgroup.ATT), residuals(assets.subgroup.ATT), xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, lty = 2)
# lines(smooth.spline(fitted(assets.subgroup.ATT), residuals(assets.subgroup.ATT)))
# 
# enrol.subgroup <- left_join(enrol.ATT,covariates, by="HouseholdID")
# enrol.subgroup$fisher <-as.factor(enrol.subgroup$fisher)
# enrol.subgroup$individual.gender <-as.factor(enrol.subgroup$individual.gender)
# enrol.subgroup$quintile <-as.factor(enrol.subgroup$quintile)
# 
# enrol.subgroup.ATT <- lmer(ATT~(1|MPAID.x)+(1|SettlementID)+ fisher + individual.gender + quintile,data=enrol.subgroup)
# summary(glht(enrol.subgroup.ATT,linfct=mcp(fisher="Tukey")))
# summary(glht(enrol.subgroup.ATT,linfct=mcp(individual.gender="Tukey")))
# summary(glht(enrol.subgroup.ATT,linfct=mcp(quintile="Tukey")))
# 
# plot(fitted(enrol.subgroup.ATT), residuals(enrol.subgroup.ATT), xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, lty = 2)
# lines(smooth.spline(fitted(enrol.subgroup.ATT), residuals(enrol.subgroup.ATT)))
# 
# attach.subgroup <- left_join(attach.ATT,covariates, by="HouseholdID")
# attach.subgroup$fisher <-as.factor(attach.subgroup$fisher)
# attach.subgroup$individual.gender <-as.factor(attach.subgroup$individual.gender)
# attach.subgroup$quintile <-as.factor(attach.subgroup$quintile)
# 
# attach.subgroup.ATT <- lmer(ATT~(1|MPAID.x)+(1|SettlementID)+ fisher + individual.gender + quintile,data=attach.subgroup)
# summary(glht(attach.subgroup.ATT,linfct=mcp(fisher="Tukey")))
# summary(glht(attach.subgroup.ATT,linfct=mcp(individual.gender="Tukey")))
# summary(glht(attach.subgroup.ATT,linfct=mcp(quintile="Tukey")))
# 
# plot(fitted(attach.subgroup.ATT), residuals(attach.subgroup.ATT), xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, lty = 2)
# lines(smooth.spline(fitted(attach.subgroup.ATT), residuals(attach.subgroup.ATT)))
# 
# save.xlsx("ATT_by_MPA_2016_1027", hfoodsec.ATT,assets.ATT,tenure.ATT,enrol.ATT,attach.ATT)
# 


# ---- Remove unneeded data frames from environment, to reduce clutter ----
rm(assets.outcome,bias.adjust,Children.HH,covariates,dampier,dummy.match,
   dummy.match.control,dummy.match.mpa,household.assets,i.ps,int.xsection.covariate,
   kaimana,kofiau,master.MPA.t2,match.covariate,misool,MPA.int.xsection.pairs,MPA.rpt.xsection.pairs,
   MPA.time.pairs,n.child,nonbaseline.pairs.t2,rpt.xsection.covariate,telma,time.match.covariate,tntc,
   wealth.quintiles,X.ps,Z,assets.dampier,assets.dampier.sens,assets.kaimana,assets.kaimana.sens,
   assets.kofiau,assets.misool,assets.misool.sens,assets.seascape,assets.seascape.sens,
   assets.telma,assets.telma.sens,assets.tntc,attach.dampier,attach.kaimana,attach.kofiau,
   attach.misool,attach.seascape,attach.seascape.sens,attach.telma,attach.telma.sens,attach.tntc,
   attach.tntc.sens,caliper10,caliper7,dummy.balance,enrol.dampier,enrol.kaimana,enrol.kofiau,
   enrol.kofiau.sens,enrol.misool,enrol.seascape,enrol.telma,enrol.telma.sens,enrol.tntc,enrol.tntc.sens,
   hfs.dampier,hfs.dampier.sens,hfs.kaimana,hfs.kaimana.sens,hfs.kofiau,hfs.kofiau.sens,hfs.misool,
   hfs.misool.sens,hfs.seascape,hfs.seascape.sens,hfs.telma,hfs.telma.sens,hfs.tntc,im10,
   imb1,ips,ipsm1,m2,mb2,tenure.dampier,tenure.dampier.sens,tenure.kaimana,tenure.kaimana.sens,
   tenure.kofiau,tenure.misool,tenure.misool.sens,tenure.seascape,tenure.seascape.sens,tenure.telma,
   tenure.telma.sens,tenure.tntc,tenure.tntc.sens,Tr,X,xm7,xmb7,xps,xpsm1,Y)
