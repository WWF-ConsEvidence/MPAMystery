# Fuzzy string matching to link MPA social monitoring records
# author: Louise Glew, louise.glew@gmail.com; Kelly Claborn (EMAIL ADDRESS); Eric Spurgeon
# created: November 2017
# modified: --

# -----
# inputs: 
# HH.data - data on MPA outcomes, plus identifying variables
# DE.data - individual level demographic information
# district.data - treatment identifier for each settlement, plus other district level information

# -----
# outputs:
# std.diff.seascape.txt --Standardized mean differences for ATTs aggregated to Seascape level
# smd_by_MPA.xlsx --Standard mean differences for ATTs by MPA
# typology_by_MPA.xlsx -- mean outcomes for MPA and controls for each outcome, by MPA

# -----
# Code sections
#  1) Call libraries and import raw data
#  2) Normalize and clean data for fuzzy matching
#  3) Fuzzy matching -- t0 household to t2 household records
#  4) Fuzzy matching -- t0 household to t4 household records

# ================================================================================================
#
# SECTION 1: Call libraries, functions and import raw data
#
# ================================================================================================
# 1.1 Call in libraries
# 1.2 Create functions
# 1.3 Import raw data

# -----
# 1.1 Call in libraries
pacman::p_load(stringdist,tm,statar,fuzzyjoin,readxl,WriteXLS,reshape2,dplyr)

# -----

# 1.2 Create and source functions
source('~/function_matching_pass2.R')
source('~/function_matching_pass1.R')

# Function to remove all white space in string variables
trim <- function(x) gsub("^\\s+|\\s+$","",x)

# Function to clean string variables (lower case, remove punctuation)
str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
    trim()
}

# -----
# 1.3 Import raw data
HH.data <- read_excel("~Extract_Master_2017_117/HH_tbl_WELLBEING.xlsx") #redirect to final file destination
DE.data <- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/HH_tbl_DEMOGRAPHIC.xlsx") #redirect to final file destination
SE.data<- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/HH_tbl_SETTLEMENT.xlsx")
ethnic.lkp<- read_excel("Dropbox/DATA_ANALYSIS/Extract_Master_2017_117/master_ethnic_lookup_2017_117.xlsx")


# ================================================================================================
#
# SECTION 2: Pre-process and clean data for fuzzy matching
#
# ================================================================================================

# 2.1 Add settlement ID to demographic table
Settlement <- subset(HH.data, select=c("HouseholdID","SettlementID"))
treatment <- subset(SE.data, select=c("SettlementID", "MPAID","Treatment"))
Settlement <-left_join(Settlement,treatment, by=("SettlementID"))
DE.data <- left_join (DE.data,Settlement, by="HouseholdID")

# 2.1 Remove settlements with no post-baseline data and re-code Kaimana settlements
HH.data <- HH.data[HH.data$SettlementID!=84 &
                   HH.data$SettlementID!=96 &
                   HH.data$SettlementID!=97 &
                   HH.data$SettlementID!=98 &
                   HH.data$SettlementID!=99 &
                   HH.data$SettlementID!=100 &
                   HH.data$SettlementID!=101,]

DE.data <- DE.data[!is.na(DE.data$SettlementID) &
                       DE.data$SettlementID!=84 &
                       DE.data$SettlementID!=96 &
                       DE.data$SettlementID!=97 &
                       DE.data$SettlementID!=98 &
                       DE.data$SettlementID!=99 &
                       DE.data$SettlementID!=100 &
                       DE.data$SettlementID!=101,]

SE.data <- SE.data[!is.na(SE.data$SettlementID) &
                             SE.data$SettlementID!=84 &
                             SE.data$SettlementID!=96 &
                             SE.data$SettlementID!=97 &
                             SE.data$SettlementID!=98 &
                             SE.data$SettlementID!=99 &
                             SE.data$SettlementID!=100 &
                             SE.data$SettlementID!=101,]
SE.data$SettlementName <- as.character(SE.data$SettlementName)


SE.data$Treatment <- ifelse(SE.data$SettlementID==83 | SE.data$SettlementID==91 | SE.data$SettlementID==92,
                                "0",SE.data$Treatment)


# 2.1 Assign monitoring timestep (code by KC)

MonitoringYear <- group_by(HH.data,MPAID)
MonitoringYear <- summarise(MonitoringYear,
                            Baseline=min(InterviewYear),
                            TwoYear=as.integer(min(InterviewYear)+2),
                            FourYear=as.integer(min(InterviewYear)+4),
                            SevenYear=as.integer(min(InterviewYear)+7))
MonitoringYear <- left_join(HH.data[,c("HouseholdID","MPAID")],
                            MonitoringYear,
                            by="MPAID")

HH.data$MonitoringYear <- factor(mapply(a=HH.data$HouseholdID,
                                       b=HH.data$InterviewYear,
                                       function(a,b){
                                         ifelse(b==MonitoringYear$Baseline[MonitoringYear$HouseholdID==a],"t0",
                                                ifelse(b==MonitoringYear$TwoYear[MonitoringYear$HouseholdID==a],"t2",
                                                       ifelse(b==MonitoringYear$FourYear[MonitoringYear$HouseholdID==a],"t4",
                                                              ifelse(b==MonitoringYear$SevenYear[MonitoringYear$HouseholdID==a],"t7",NA))))
                                       }),
                                levels=c("t0","t2","t4"),
                                ordered=T)

# 2.1 Subset to Bird's Head Seascape monitoring data only
HH.data <- subset(HH.data, MPAID<7)
DE.data<-left_join(subset(HH.data, select=c("HouseholdID")),DE.data, by="HouseholdID")

# 2.2 Remove refusal households
HH.data <- HH.data[HH.data$HouseholdID!=188 &
                     HH.data$HouseholdID!=193 &
                     HH.data$HouseholdID!=196 &
                     HH.data$HouseholdID!=1058 &
                     HH.data$HouseholdID!=1295 &
                     HH.data$HouseholdID!=1321 &
                     HH.data$HouseholdID!=1347,]

# 2.1 Extract relevant columns from dataset
HH.fuzzy.vars <- subset (HH.data, select=c("HouseholdID","MPAID","SettlementID","Respondent","YearsResident","PaternalEthnicity","Religion","MonitoringYear"))
DE.fuzzy.vars <- subset (DE.data, select=c("DemographicID","HouseholdID","IndividualName", "RelationHHH","IndividualAge"))

# 2.2 Subset demographic data to household heads only
DE.fuzzy.vars <- subset (DE.fuzzy.vars, RelationHHH ==0)

# 2.3 Create binned variables for age and residency
is.na(DE.fuzzy.vars$IndividualAge) <- DE.fuzzy.vars$IndividualAge >= 990
DE.fuzzy.vars$IndividualAge[is.na(DE.fuzzy.vars$IndividualAge)]<-990
age.bin<-c(0,20,30,40,50,60,70,990)
ind.age<-.bincode(DE.fuzzy.vars$IndividualAge,age.bin,TRUE,TRUE)
DE.fuzzy.vars <- cbind(DE.fuzzy.vars, ind.age)
rm(ind.age, age.bin)

is.na(HH.data$YearsResident)<-HH.data$YearsResident>=990
HH.data$YearsResident[is.na(HH.data$YearsResident)]<-990
resident.bin<-c(0,10,20,30,40,50,60,990)
resident<-.bincode(HH.data$YearsResident,resident.bin,TRUE,TRUE)
HH.fuzzy.vars <-cbind(HH.fuzzy.vars,resident)
rm(resident.bin,resident)

# 2.4 Clean string variables
HH.fuzzy.vars$PaternalEthnicity <-str_clean(HH.fuzzy.vars$PaternalEthnicity)
DE.fuzzy.vars$IndividualName <-str_clean(DE.fuzzy.vars$IndividualName)

# 2.5 Update with ethnic codes
HH.fuzzy.vars <- left_join(HH.fuzzy.vars,ethnic.lkp, by=c("PaternalEthnicity"="std.eth.str"))

# 2.6 Combine data into single fuzzy matching master
fuzzy.vars <-left_join(HH.fuzzy.vars,DE.fuzzy.vars, by="HouseholdID")
fuzzy.vars <- subset(fuzzy.vars, select=c("HouseholdID","MPAID","SettlementID", "MonitoringYear","resident","ind.age","eth.iso","IndividualName"))
rm(HH.fuzzy.vars,DE.fuzzy.vars)

# Set row names to Household ID
#row.names(fuzzy.vars)<-fuzzy.vars$HouseholdID

# 2.7 Split fuzzy matching mater into time-steps
fuzzy.vars.bl <- subset(fuzzy.vars, MonitoringYear=="t0")
fuzzy.vars.t2 <- subset(fuzzy.vars,MonitoringYear=="t2")
fuzzy.vars.t4 <- subset(fuzzy.vars,MonitoringYear=="t4")

# ================================================================================================
#
# ---- SECTION 3: Fuzzy matching -- t0.t2, pass 1 (household heads only) ----
#
# ================================================================================================

# 3.1 Pass 1: Individual Name Only

t0.t2.pairs <-
  (stringdist_join(fuzzy.vars.bl,fuzzy.vars.t2, by="IndividualName",mode="left",ignore_case=TRUE,method='osa', distance_col="dist")) %>%
  group_by(HouseholdID.x) %>%
  top_n(1, -dist)

# 3.2  Run function to check if (a) household head names match; and then (b) if other individuals at to become household heads at t2.
t0.t2.pairs.matching <- matching_pass1(t0.t2.pairs,DE.data)
matched.households.pass1 <- as.data.frame(t0.t2.pairs.matching[[1]])  # Posssible matches
# adult.demos.pass1.matching <- as.data.frame(t0.t2.pairs.matching[[2]]) # Further demographic information to x-check potential matches

# 3.3. Create master list of matched households
t0.t2.matched <-subset(matched.households.pass1, Match=="Yes")#automatic matches where Match=YES.
t0.t2.matched <-subset(t0.t2.matched, select=c("HouseholdID.t0", "HouseholdID.t2"))

## 3.4 Extract possible matches for verficiation
t0.t2.possibles <-subset(matched.households.pass1, NumMatches>0 & Match=="No")  # possible matches to cross check, where NumMatches>0 AND Match=No
verified <-c("Yes","No","No","No","Yes","No","Yes","No","No","No","Yes","No","No","Yes","Yes","Yes","Yes","No") # manually verified against database
t0.t2.possibles$Match<-verified
rm(verified)

# 3.5 Update t0.t2.matched with manually verified matches from possibles list
t0.t2.possibles <-subset(t0.t2.possibles,select=c("HouseholdID.t0", "HouseholdID.t2","Match"))
update.matched <-subset(t0.t2.possibles, Match =="Yes")
update.matched$Match<-NULL
t0.t2.matched <- rbind(t0.t2.matched, update.matched)
rm(t0.t2.possibles,update.matched, t0.t2.pairs, matched.households.pass1)

# ================================================================================================
#
# ---- SECTION 4: Fuzzy matching -- t0.t2, pass 2 (all individuals only) ----
#
# ================================================================================================

# 4.1 Generate new input data frame (IndividualName, Binned Age > 18, Settlement, MPA, Monitoring Year)
fuzzy.vars.pass2 <- subset(DE.data, select=c("DemographicID", "HouseholdID", "IndividualName","IndividualAge","IndividualGender", "SettlementID"), IndividualAge>16)
x <-subset(HH.data, select=c("HouseholdID", "MonitoringYear","MPAID"))
fuzzy.vars.pass2 <-left_join(fuzzy.vars.pass2,x, by="HouseholdID")
               
# 4.2 Remove all previously matched records
exclude.pass2 <- subset(t0.t2.matched, select=c("HouseholdID.t0","HouseholdID.t2"))
exclude.pass2 <-melt(exclude.pass2)  
fuzzy.vars.pass2 <- anti_join(fuzzy.vars.pass2,exclude.pass2, by=c("HouseholdID"="value"))

# 4.3 Clean individual name
fuzzy.vars.pass2$IndividualName <- str_clean(fuzzy.vars.pass2$IndividualName)

# 4.4 Split by monitoring year
fuzzy.p2.bl <- subset(fuzzy.vars.pass2, MonitoringYear=="t0")
fuzzy.p2.t2 <- subset(fuzzy.vars.pass2,MonitoringYear=="t2")

# 4.5 Pass 2 string distance match (all individuals)
t0.t2.pairs.p2 <-(stringdist_join(fuzzy.p2.bl,fuzzy.p2.t2, by="IndividualName",mode="left",ignore_case=TRUE,method='osa', distance_col="dist")) %>%
group_by(DemographicID.x) %>%
top_n(1, -dist)

# 4.6 Apply decision rules to pass 2 match
matched.t0.t2.households.p2 <- matching_pass2(t0.t2.pairs.p2)
matched.t0.t2.households.p2 <- as.data.frame(matched.t0.t2.households.p2[[1]])

# 4.7 Extract automatic matches from pass 2 match
# Manual verification of function output indicates accept all 'yes', "Yes.HH.a", "Yes.HH.b"
t0.t2.matched.pass2 <- subset(matched.t0.t2.households.p2, select=c("HouseholdID.t0", "MatchedHouseholdID.t2"),(Match=="Yes"|Match=="Yes.HH.a"|Match=="Yes.HH.b"))
names(t0.t2.matched.pass2)<- c("HouseholdID.t0","HouseholdID.t2")                       

# 4.8 Manually verify and upload additional pass 2 matches
# Check all matches from matched.t0.t2.households.p2, which have "Manually check" in Match column
write.table(matched.t0.t2.households.p2,"pass2_t0t2_manual_verification.txt", sep=",")
verified.pass2 <- read_excel("Social_impacts_2017_1230/manually_verified_to_t2_pass2_matches.xlsx")

# 4.9 Combine all t0.t2.matches
t0.t2.matched <- rbind(t0.t2.matched, t0.t2.matched.pass2, verified.pass2)
t0.t2.matched <- t0.t2.matched[order(t0.t2.matched$HouseholdID.t0),]
t0.t2.matched$match.id <-seq(1, length(t0.t2.matched$HouseholdID.t0),by=1)

rm(verified.pass2,t0.t2.matched.pass2,matched.t0.t2.p2,t0.t2.pairs.p2, fuzzy.p2.bl, fuzzy.p2.t2, all.households.pass2, exclude.pass2)

# ================================================================================================
#
# ---- SECTION 5: Fuzzy matching -- t0.t2, pass 2 (all individuals only) ----
#
# ================================================================================================

# 5.1 Pass 1: Individual Name Only
t0.t4.pairs <-
  (stringdist_join(fuzzy.vars.bl,fuzzy.vars.t4, by="IndividualName",mode="left",ignore_case=TRUE,method='osa', distance_col="dist")) %>%
  group_by(HouseholdID.x) %>%
  top_n(1, -dist)

# 5.2  Run function to check if (a) household head names match; and then (b) if other individuals at to become household heads at t2.
t0.t4.pairs.matching <- matching_pass1(t0.t4.pairs,DE.data)
matched.households.pass1 <- as.data.frame(t0.t4.pairs.matching[[1]])  # Posssible matches
# adult.demos.pass1.matching <- as.data.frame(t0.t2.pairs.matching[[2]]) # Further demographic information to x-check potential matches

# 5.3 Create master list of matched households
t0.t4.matched <-subset(matched.households.pass1, Match=="Yes")#automatic matches where Match=YES.
t0.t4.matched <-subset(t0.t4.matched, select=c("HouseholdID.t0", "HouseholdID.t2"))

## 5.4 Extract possible matches for verficiation
t0.t4.possibles <-subset(matched.households.pass1, NumMatches>0 & Match=="No")  # possible matches to cross check, where NumMatches>0 AND Match=No
verified <-c("Yes","Yes","Yes","Yes","No","Yes","No","No","Yes","No","Yes","Yes","Yes","Yes","No","Yes","No","Yes","No","No","No","Yes","No","Yes") # manually verified against database
t0.t4.possibles$Match<-verified
rm(verified)

# 5.5 Update t0.t2.matched with manually verified matches from possibles list
t0.t4.possibles <-subset(t0.t4.possibles,select=c("HouseholdID.t0", "HouseholdID.t2","Match"),(Match =="Yes"))
t0.t4.possibles$Match <- NULL
t0.t4.matched <- rbind(t0.t4.matched, t0.t4.possibles)
names(t0.t4.matched) <-c("HouseholdID.t0","HouseholdID.t4")
rm(t0.t4.possibles)

# ================================================================================================
#
# ---- SECTION 6: Fuzzy matching -- t0.t4, pass 2 (all individuals only) ----
#
# ================================================================================================

# 6.2 Remove all previously matched records
exclude.pass2 <-melt(t0.t4.matched)  
fuzzy.vars.pass2 <- anti_join(fuzzy.vars.pass2,exclude.pass2, by=c("HouseholdID"="value"))

# 6.4 Split by monitoring year
fuzzy.p2.bl <- subset(fuzzy.vars.pass2, MonitoringYear=="t0")
fuzzy.p2.t4 <- subset(fuzzy.vars.pass2,MonitoringYear=="t4")

# 6.5 Pass 2 string distance match (all individuals)
t0.t4.pairs.p2 <-(stringdist_join(fuzzy.p2.bl,fuzzy.p2.t4, by="IndividualName",mode="left",ignore_case=TRUE,method='osa', distance_col="dist")) %>%
  group_by(DemographicID.x) %>%
  top_n(1, -dist)

# 6.6 Apply decision rules to pass 2 match
matched.t0.t4.households.p2 <- matching_pass2(t0.t4.pairs.p2)
matched.t0.t4.households.p2 <- as.data.frame(matched.t0.t4.households.p2[[1]])

# 6.7 Extract automatic matches from pass 2 match
# Manual verification of function output indicates accept all 'yes', "Yes.HH.a", "Yes.HH.b"
t0.t4.matched.pass2 <- subset(matched.t0.t4.households.p2, select=c("HouseholdID.t0", "MatchedHouseholdID.t2"),(Match=="Yes"|Match=="Yes.HH.a"|Match=="Yes.HH.b"))
names(t0.t4.matched.pass2)<- c("HouseholdID.t0","HouseholdID.t4")                       

# 6.8 Manually verify and upload additional pass 2 matches
# Check all matches from matched.t0.t4.households.p2, which have "Manually check" in Match column
write.table(matched.t0.t4.households.p2,"pass2_t0t4_manual_verification.txt", sep=",")
verified.pass2 <- read_excel("Social_impacts_2017_1230/manually_verified_t0t4_pass2_matches.xlsx")

# 6.9 Combine all t0.t4.matches
t0.t4.matched <- rbind(t0.t4.matched, t0.t4.matched.pass2, verified.pass2)
t0.t4.matched <- t0.t4.matched[order(t0.t4.matched$HouseholdID.t0),]
t0.t4.matched$match.id <-seq(1, length(t0.t4.matched$HouseholdID.t0),by=1)

rm(verified.pass2,t0.t4.matched.pass2,matched.t0.t4.p2,t0.t4.pairs.p2, fuzzy.p2.bl, fuzzy.p2.t4, exclude.pass2)

# ================================================================================================
#
# ---- SECTION 7: Summary statistics on longitudinal matches ----
#
# ================================================================================================

# 7.1  Households surveyed at t0, t2 and t4
t0.t2.t4.households <- inner_join(t0.t2.matched,t0.t4.matched, by=("HouseholdID.t0"))
nrow(t0.t2.t4.households)

#7.2  Longitudinal matches by MPA

#t0.t2
t0.t2.MPA <- left_join(t0.t2.matched,Settlement, by=c("HouseholdID.t0" = "HouseholdID"))
matches.by.MPA.t2 <-left_join(t0.t2.MPA %>% group_by(MPAID)%>% summarise(n=n()), sample.by.MPA <- Settlement %>% group_by(MPAID) %>%summarise(n=n()), by=("MPAID"))
matches.by.MPA.t2 <- mutate(matches.by.MPA.t2, proportion=(n.x/n.y))
names(matches.by.MPA.t2) <- c("MPA", "longitudinal.sample", "sample", "proportion")

#t0.t4
t0.t4.MPA <- left_join(t0.t4.matched,Settlement, by=c("HouseholdID.t0" = "HouseholdID"))
matches.by.MPA.t4 <-left_join(t0.t4.MPA %>% group_by(MPAID)%>% summarise(n=n()), sample.by.MPA <- Settlement %>% group_by(MPAID) %>%summarise(n=n()), by=("MPAID"))
matches.by.MPA.t4 <- mutate(matches.by.MPA.t4, proportion=(n.x/n.y))
names(matches.by.MPA.t4) <- c("MPA", "longitudinal.sample", "sample", "proportion")

#t0.t2.t4
t0.t2.t4.MPA <- left_join(t0.t2.t4.households,Settlement, by=c("HouseholdID.t0" = "HouseholdID"))
matches.by.MPA.t2.t4 <-left_join(t0.t2.t4.MPA %>% group_by(MPAID)%>% summarise(n=n()), sample.by.MPA <- Settlement %>% group_by(MPAID) %>%summarise(n=n()), by=("MPAID"))
matches.by.MPA.t2.t4 <- mutate(matches.by.MPA.t2.t4, proportion=(n.x/n.y))
names(matches.by.MPA.t4) <- c("MPA", "longitudinal.sample", "sample", "proportion")