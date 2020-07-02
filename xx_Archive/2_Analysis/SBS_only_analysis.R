pacman::p_load(plyr,ggplot2,reshape2,reldist,grid,gridExtra,varhandle,xlsx,
               RODBC,Matching,optmatch,tidyr,RItools,Hmisc,MBESS,rbounds,Kendall, tm, NLP, dplyr, dbplyr, RPostgreSQL)



setwd("SBS/Data/SBS_only_analysis")


# ---- 1.1 Import data ----

Organization.w.SBS.only.vars <- read.csv("Organization_for_SBS_only_analysis.csv", header = T, sep = ",")
HHData.w.SBS.only.vars <- read.csv("HHData_for_SBS_only_analysis.csv", header = T, sep = ",") # differs from "HHData" object in SBS_New_MPA_Mystery script has additional varible(s): "PaternalEthnicityClean" and "NumLocalThreatClean"
IndDemos.w.SBS.only.vars <- read.csv("IndDemos_for_SBS_only_analysis.csv", header = T, sep = ",") # differs from "IndDemos" object in SBS_New_MPA_Mystery script has additional varible(s): "NumLocalThreatClean"
# import "Settlements.csv" (object name = "Settlements") if not already in Global Environment


# FGD data files
SBS.FGD.MPA <- read.csv("FGD_MPA_BHS_SBS_2017_1109.csv",header=T)
SBS.FGD.Users <- read.csv("FGD_USERS_BHS_SBS_2017_1109.csv",header=T)

# KII data files
SBS.KII.sppRules <- read.csv("KII_sppRules_BHS_SBS_2017_1109.csv",header=T)
SBS.KII.habRules <- read.csv("KII_habRules_BHS_SBS_2017_1109.csv",header=T)

# Look-up data files for Ethnicity and Education

Ethnicity.lkp <- read.csv("master_ethnic_lookup_2017_117.csv")
Education.lkp <- read.csv("education_lkp.csv")


# ---- 1.2 Import functions ----

# Function to remove all white space in string variables

trim <- function(x) gsub("^\\s+|\\s+$","",x)

# Function to clean string variables (lower case, remove punctuation)

str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% 
    removePunctuation(preserve_intra_word_dashes = TRUE) %>% 
    stripWhitespace() %>% 
    trim()
}


# ---- 2 HOUSEHOLD SURVEY VARIABLES ----


# ---- 2.1 Define monitoring year for each MPA ----

MonitoringYear <- group_by(HHData.w.SBS.only.vars,MPAID)
MonitoringYear <- summarise(MonitoringYear,
                            Baseline=min(InterviewYear),
                            TwoYear=as.integer(min(InterviewYear)+2),
                            ThreeYear=as.integer(min(InterviewYear)+3),
                            SixYear=as.integer(min(InterviewYear)+6))
MonitoringYear <- left_join(HHData.w.SBS.only.vars[,c("HouseholdID","MPAID")],
                            MonitoringYear,
                            by="MPAID")

HHData.w.SBS.only.vars$MonitoringYear <- factor(mapply(a=HHData.w.SBS.only.vars$HouseholdID,
                                       b=HHData.w.SBS.only.vars$InterviewYear,
                                       function(a,b){
                                         ifelse(b==MonitoringYear$Baseline[MonitoringYear$HouseholdID==a],"Baseline",
                                                ifelse(b==MonitoringYear$TwoYear[MonitoringYear$HouseholdID==a],"2 Year Post",
                                                       ifelse(b==MonitoringYear$ThreeYear[MonitoringYear$HouseholdID==a],"3 Year Post",
                                                              ifelse(b==MonitoringYear$SixYear[MonitoringYear$HouseholdID==a],"6 Year Post",NA))))
                                       }),
                                levels=c("Baseline","2 Year Post", "3 Year Post", "4 Year Post"),
                                ordered=T)

# ---- 2.2 Create new dataframes for household analysis ----

HHDataSBS <-
  HHData.w.SBS.only.vars %>%
  dplyr::select(HouseholdID, MPAID, SettlementID, PaternalEthnicityClean, EconStatusTrendClean, NumLocalThreatClean, MarineGroup, SocialConflict, MonitoringYear, PrimaryLivelihoodClean) %>%
  left_join(., Settlements,by=c("SettlementID","MPAID"))

# Remove any NAs in HHID column
HHDataSBS <- HHDataSBS[!is.na(HHDataSBS$HouseholdID), ]

DemosSBS <-
  IndDemos.w.SBS.only.vars %>%
  dplyr::select(HouseholdID, MPAID, IndividualEducationClean, IndividualAgeClean, IndividualUnwellClean, RelationHHH) %>%
  left_join(., HHData.w.SBS.only.vars[,c("HouseholdID", "MPAID", "MonitoringYear", "SettlementID")]) %>%
  left_join(., Settlements)

# Remove any NAs in HHID column
DemosSBS <- DemosSBS[!is.na(DemosSBS$HouseholdID), ]

OrgSBS <- 
  Organization.w.SBS.only.vars %>% 
  left_join(., HHData.w.SBS.only.vars[,c("HouseholdID", "MPAID", "SettlementID", "MonitoringYear")]) %>% 
  left_join(., Settlements)

# Remove any NAs in HHID column
OrgSBS <- OrgSBS[!is.na(OrgSBS$HouseholdID), ]


# Join OrgSBS to HHDataSVS dataframes

HHDataSBS <-
  HHDataSBS %>%
  left_join(., OrgSBS) %>%
  dplyr::select(.,"HouseholdID", "MPAID", "SettlementID","SettlementName", "Treatment", "PaternalEthnicityClean", 
         "EconStatusTrendClean", "NumLocalThreatClean", "MarineGroup", 
         "MonitoringYear", "MarineMeetingClean", "SocialConflict", "MarineContributionClean")


# ---- 2.3 Prepare Ethnicity and Education look-up files for column matching ----


# Prepare Ethnicity look-up file for column matching
Ethnicity.lkp <- unique(Ethnicity.lkp)
Ethnicity.lkp <- rename(Ethnicity.lkp, EthnicityISO =  eth.iso, EthnicityName = std.eth.str)


# Prepare Education look-up file for column matching
Education.lkp <- unique(Education.lkp)
Education.lkp <- rename(Education.lkp, IndividualEdLevel =  ed.level)


# ---- 2.4 Synthesize HH results ----

# All SBS_specific HH variables except education and illness BY SETTLEMENT

Synth.HHDataSBS.BySett.All <- 
  HHDataSBS %>% 
  mutate(PaternalEthnicity_StrClean = str_clean(PaternalEthnicityClean)) %>% 
  left_join(., Ethnicity.lkp, by=c("PaternalEthnicity_StrClean"="EthnicityName")) %>% 
  filter(SocialConflict<=5) %>%
  group_by(SettlementID, MPAID, SettlementName, Treatment, MonitoringYear) %>%
  summarise(Num.EthnicGroups=length(unique(EthnicityISO[!is.na(EthnicityISO)])),
            Majority.Ethnic1=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 1),"No Data"),
            Majority.Ethnic2=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 2),"No Data"),
            Majority.Ethnic3=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 3),"No Data"),
            Majority.Ethnic4=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 4),"No Data"),
            Majority.Ethnic5=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 5),"No Data"),
            Percent.MajorityEthnic1=(length(EthnicityISO[EthnicityISO==Majority.Ethnic1 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.MajorityEthnic2=(length(EthnicityISO[EthnicityISO==Majority.Ethnic2 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.MajorityEthnic3=(length(EthnicityISO[EthnicityISO==Majority.Ethnic3 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.MajorityEthnic4=(length(EthnicityISO[EthnicityISO==Majority.Ethnic4 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.OtherEthnic=(length(EthnicityISO[EthnicityISO!=Majority.Ethnic1 &
                                                  EthnicityISO!=Majority.Ethnic2 &
                                                  EthnicityISO!=Majority.Ethnic3 &
                                                  EthnicityISO!=Majority.Ethnic4 &
                                                  !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Econ.Status.Much.Worse=(length(EconStatusTrendClean[EconStatusTrendClean==1 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Slighly.Worse=(length(EconStatusTrendClean[EconStatusTrendClean==2 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Neutral=(length(EconStatusTrendClean[EconStatusTrendClean==3 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Slightly.Better=(length(EconStatusTrendClean[EconStatusTrendClean==4 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Much.Better=(length(EconStatusTrendClean[EconStatusTrendClean==5 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Threat.Mean=mean(NumLocalThreatClean, na.rm = T),
            Threat.None=(length(NumLocalThreatClean[NumLocalThreatClean==0 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.One=(length(NumLocalThreatClean[NumLocalThreatClean==1 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Two=(length(NumLocalThreatClean[NumLocalThreatClean==2 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Three=(length(NumLocalThreatClean[NumLocalThreatClean==3 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Four=(length(NumLocalThreatClean[NumLocalThreatClean==4 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Minimum.Five =(length(NumLocalThreatClean[NumLocalThreatClean>=5 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Member.No=(length(MarineGroup[MarineGroup==0 &
                                            !is.na(MarineGroup)])/
                         length(MarineGroup[!is.na(MarineGroup)]))*100,
            Member.Yes=(length(MarineGroup[MarineGroup==1 &
                                             !is.na(MarineGroup)])/
                          length(MarineGroup[!is.na(MarineGroup)]))*100,
            Num.Member.Yes=length(MarineGroup[MarineGroup==1 & !is.na(MarineGroup)]),
            Num.Member.No=length(MarineGroup[MarineGroup==0 & !is.na(MarineGroup)]),
            Meeting.No=(length(MarineMeetingClean[MarineMeetingClean==0 &
                                                    !is.na(MarineMeetingClean)])/
                          length(MarineMeetingClean[!is.na(MarineMeetingClean)]))*100,
            Meeting.Yes=(length(MarineMeetingClean[MarineMeetingClean==1 &
                                                     !is.na(MarineMeetingClean)])/
                           length(MarineMeetingClean[!is.na(MarineMeetingClean)]))*100,
            Num.Meeting.Yes=length(MarineMeetingClean[MarineMeetingClean==1 & !is.na(MarineMeetingClean)]),
            Num.Meeting.No=length(MarineMeetingClean[MarineMeetingClean==0 & !is.na(MarineMeetingClean)]),
            Contribution=mean(MarineContributionClean, na.rm = T),
            Num.Member.Yes.Meeting.Yes = length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]),
            Num.Member.Yes.Meeting.No = length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]),
            Prop.Member.Yes.Meeting.Yes = (length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]) /length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)])) 
            / (length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)]) 
               + length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)]))* 100,
            Prop.Member.Yes.Meeting.No = (length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]) /length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)])) 
            / (length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)]) 
               + length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)]))*100,
            Percent.GreatlyIncreased.SocConflict=(length(SocialConflict[(SocialConflict==1) &
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Same.SocConflict=(length(SocialConflict[(SocialConflict==3) &
                                                              !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.GreatlyDecreased.SocConflict=(length(SocialConflict[SocialConflict==5 &
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100)


# All SBS_specific HH variables (including Organization-related variables) except education and illness BY MPA

Synth.HHDataSBS.ByMPA.All <- 
  HHDataSBS %>% 
  mutate(PaternalEthnicity_StrClean = str_clean(PaternalEthnicityClean)) %>% 
  left_join(., Ethnicity.lkp, by=c("PaternalEthnicity_StrClean"="EthnicityName")) %>% 
  filter(SocialConflict<=5) %>%
  group_by(MPAID, Treatment, MonitoringYear) %>%
  summarise(Num.EthnicGroups=length(unique(EthnicityISO[!is.na(EthnicityISO)])),
            Majority.Ethnic1=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 1),"No Data"),
            Majority.Ethnic2=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 2),"No Data"),
            Majority.Ethnic3=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 3),"No Data"),
            Majority.Ethnic4=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 4),"No Data"),
            Majority.Ethnic5=ifelse(tail(sort(table(EthnicityISO[!is.na(EthnicityISO)])), 1)!=0,
                                    tail(names(sort(table(EthnicityISO[!is.na(EthnicityISO)]))), 5),"No Data"),
            Percent.MajorityEthnic1=(length(EthnicityISO[EthnicityISO==Majority.Ethnic1 &
                                                           !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.MajorityEthnic2=(length(EthnicityISO[EthnicityISO==Majority.Ethnic2 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.MajorityEthnic3=(length(EthnicityISO[EthnicityISO==Majority.Ethnic3 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.MajorityEthnic4=(length(EthnicityISO[EthnicityISO==Majority.Ethnic4 &
                                                      !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Percent.OtherEthnic=(length(EthnicityISO[EthnicityISO!=Majority.Ethnic1 &
                                                  EthnicityISO!=Majority.Ethnic2 &
                                                  EthnicityISO!=Majority.Ethnic3 &
                                                  EthnicityISO!=Majority.Ethnic4 &
                                                  !is.na(EthnicityISO)])/length(EthnicityISO[!is.na(EthnicityISO)]))*100,
            Econ.Status.Much.Worse=(length(EconStatusTrendClean[EconStatusTrendClean==1 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Slighly.Worse=(length(EconStatusTrendClean[EconStatusTrendClean==2 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Neutral=(length(EconStatusTrendClean[EconStatusTrendClean==3 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Slightly.Better=(length(EconStatusTrendClean[EconStatusTrendClean==4 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Econ.Status.Much.Better=(length(EconStatusTrendClean[EconStatusTrendClean==5 & !is.na(EconStatusTrendClean)])/length(EconStatusTrendClean[!is.na(EconStatusTrendClean)]))*100,
            Threat.Mean=mean(NumLocalThreatClean, na.rm = T),
            Threat.None=(length(NumLocalThreatClean[NumLocalThreatClean==0 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.One=(length(NumLocalThreatClean[NumLocalThreatClean==1 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Two=(length(NumLocalThreatClean[NumLocalThreatClean==2 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Three=(length(NumLocalThreatClean[NumLocalThreatClean==3 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Four=(length(NumLocalThreatClean[NumLocalThreatClean==4 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Threat.Minimum.Five =(length(NumLocalThreatClean[NumLocalThreatClean>=5 & !is.na(NumLocalThreatClean)])/length(NumLocalThreatClean[!is.na(NumLocalThreatClean)]))*100,
            Member.No=(length(MarineGroup[MarineGroup==0 &
                                            !is.na(MarineGroup)])/
                         length(MarineGroup[!is.na(MarineGroup)]))*100,
            Member.Yes=(length(MarineGroup[MarineGroup==1 &
                                             !is.na(MarineGroup)])/
                          length(MarineGroup[!is.na(MarineGroup)]))*100,
            Num.Member.Yes=length(MarineGroup[MarineGroup==1 & !is.na(MarineGroup)]),
            Num.Member.No=length(MarineGroup[MarineGroup==0 & !is.na(MarineGroup)]),
            Member.No=(length(MarineGroup[MarineGroup==0 &
                                            !is.na(MarineGroup)])/
                         length(MarineGroup[!is.na(MarineGroup)]))*100,
            Member.Yes=(length(MarineGroup[MarineGroup==1 &
                                             !is.na(MarineGroup)])/
                          length(MarineGroup[!is.na(MarineGroup)]))*100,
            Num.Member.Yes=length(MarineGroup[MarineGroup==1 & !is.na(MarineGroup)]),
            Num.Member.No=length(MarineGroup[MarineGroup==0 & !is.na(MarineGroup)]),
            Meeting.No=(length(MarineMeetingClean[MarineMeetingClean==0 &
                                                    !is.na(MarineMeetingClean)])/
                          length(MarineMeetingClean[!is.na(MarineMeetingClean)]))*100,
            Meeting.Yes=(length(MarineMeetingClean[MarineMeetingClean==1 &
                                                     !is.na(MarineMeetingClean)])/
                           length(MarineMeetingClean[!is.na(MarineMeetingClean)]))*100,
            Num.Meeting.Yes=length(MarineMeetingClean[MarineMeetingClean==1 & !is.na(MarineMeetingClean)]),
            Num.Meeting.No=length(MarineMeetingClean[MarineMeetingClean==0 & !is.na(MarineMeetingClean)]),
            Contribution=mean(MarineContributionClean, na.rm = T),
            Num.Member.Yes.Meeting.Yes = length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]),
            Num.Member.Yes.Meeting.No = length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]),
            Prop.Member.Yes.Meeting.Yes = (length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]) /length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)])) 
            / (length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)]) 
               + length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)])) * 100,
            Prop.Member.Yes.Meeting.No = (length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)]) /length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)])) 
            / (length(HouseholdID[MarineMeetingClean==1 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)]) 
               + length(HouseholdID[MarineMeetingClean==0 & !is.na(MarineMeetingClean) & MarineGroup==1 & !is.na(MarineGroup)])/length(HouseholdID[!is.na(MarineMeetingClean) & !is.na(MarineGroup)])) * 100,
            Percent.GreatlyIncreased.SocConflict=(length(SocialConflict[(SocialConflict==1) &
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Increased.SocConflict=(length(SocialConflict[(SocialConflict==2) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Same.SocConflict=(length(SocialConflict[(SocialConflict==3) &
                                                              !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.Decreased.SocConflict=(length(SocialConflict[(SocialConflict==4) &
                                                                   !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100,
            Percent.GreatlyDecreased.SocConflict=(length(SocialConflict[SocialConflict==5 &
                                                                          !is.na(SocialConflict)])/length(SocialConflict[!is.na(SocialConflict)]))*100)


# Adult Education BY SETTLEMENT


AdultEducation.BySett.All <-
  DemosSBS %>%
  mutate(IndividualEducation_StrClean = str_clean(IndividualEducationClean)) %>%
  left_join(., Education.lkp, by=c("IndividualEducation_StrClean" = "IndividualEducation"))  %>%
  filter(., IndividualAgeClean>=18, IndividualEdLevel<=5) %>%
  group_by(SettlementID, MPAID, SettlementName, Treatment, MonitoringYear) %>%
  summarise(AdultEducNone=(length(IndividualEdLevel[IndividualEdLevel==0 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            AdultEducPre=(length(IndividualEdLevel[IndividualEdLevel==1 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            AdultEducPrim=(length(IndividualEdLevel[IndividualEdLevel==2 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            AdultEducMid=(length(IndividualEdLevel[IndividualEdLevel==3 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            AdultEducSec=(length(IndividualEdLevel[IndividualEdLevel==4 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            AdultEducHigher=(length(IndividualEdLevel[IndividualEdLevel==5 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100)

# Check row sums
rowSums(AdultEducation.BySett.All[, 6:11])


# Adult Education BY MPA
 
 AdultEducation.ByMPA.All <- 
   DemosSBS %>%
   mutate(IndividualEducation_StrClean = str_clean(IndividualEducationClean)) %>%
   left_join(., Education.lkp, by=c("IndividualEducation_StrClean" = "IndividualEducation")) %>%
   filter(., IndividualAgeClean>=18, IndividualEdLevel<=5) %>%
   group_by(MPAID, Treatment, MonitoringYear) %>%
   summarise(AdultEducNone=(length(IndividualEdLevel[IndividualEdLevel==0 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
             AdultEducPre=(length(IndividualEdLevel[IndividualEdLevel==1 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
             AdultEducPrim=(length(IndividualEdLevel[IndividualEdLevel==2 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
             AdultEducMid=(length(IndividualEdLevel[IndividualEdLevel==3 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
             AdultEducSec=(length(IndividualEdLevel[IndividualEdLevel==4 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
             AdultEducHigher=(length(IndividualEdLevel[IndividualEdLevel==5 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100)
 
# Check row sums
rowSums(AdultEducation.ByMPA.All[, 4:9])

 
 # Household Head Education BY SETTLEMENT
 
HHHEducation.BySett.All <- 
  DemosSBS %>%
  mutate(IndividualEducation_StrClean = str_clean(IndividualEducationClean)) %>%
  left_join(., Education.lkp, by=c("IndividualEducation_StrClean" = "IndividualEducation")) %>%
  filter(., IndividualAgeClean>=18 & RelationHHH==0) %>%
  filter(., IndividualEdLevel<=5)  %>%
  group_by(SettlementID, MPAID, SettlementName, Treatment, MonitoringYear) %>%
  summarise(HHHEducNone=(length(IndividualEdLevel[IndividualEdLevel==0 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducPre=(length(IndividualEdLevel[IndividualEdLevel==1 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducPrim=(length(IndividualEdLevel[IndividualEdLevel==2 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducMid=(length(IndividualEdLevel[IndividualEdLevel==3 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducSec=(length(IndividualEdLevel[IndividualEdLevel==4 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducHigher=(length(IndividualEdLevel[IndividualEdLevel==5 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100)

# Check row sums
rowSums(HHHEducation.BySett.All[, 6:11])


# Household Head Education BY MPA

HHHEducation.ByMPA.All <- 
  DemosSBS %>%
  mutate(IndividualEducation_StrClean = str_clean(IndividualEducationClean)) %>%
  left_join(., Education.lkp, by=c("IndividualEducation_StrClean" = "IndividualEducation")) %>%
  filter(., IndividualAgeClean>=18 & RelationHHH==0, IndividualEdLevel<=5) %>%
  group_by(MPAID, Treatment, MonitoringYear) %>%
  summarise(HHHEducNone=(length(IndividualEdLevel[IndividualEdLevel==0 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducPre=(length(IndividualEdLevel[IndividualEdLevel==1 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducPrim=(length(IndividualEdLevel[IndividualEdLevel==2 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducMid=(length(IndividualEdLevel[IndividualEdLevel==3 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducSec=(length(IndividualEdLevel[IndividualEdLevel==4 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100,
            HHHEducHigher=(length(IndividualEdLevel[IndividualEdLevel==5 & !is.na(IndividualEdLevel)])/length(IndividualEdLevel[!is.na(IndividualEdLevel)]))*100)

# Check row sums
rowSums(HHHEducation.ByMPA.All[,4:9])

# Illness BY SETTLEMENT

Illness.BySett.All <- 
  DemosSBS %>%
  group_by(SettlementID, MPAID, SettlementName, Treatment, MonitoringYear) %>%
  summarise(Ill = length(IndividualUnwellClean[IndividualUnwellClean==1 & !is.na(IndividualUnwellClean)]),
            Not.Ill = length(IndividualUnwellClean[IndividualUnwellClean==0 & !is.na(IndividualUnwellClean)]),
            Percent.Ill = (length(IndividualUnwellClean[IndividualUnwellClean==1 & !is.na(IndividualUnwellClean)])/length(IndividualUnwellClean[!is.na(IndividualUnwellClean)]))*100,
            Percent.Not.Ill = (length(IndividualUnwellClean[IndividualUnwellClean==0 & !is.na(IndividualUnwellClean)])/length(IndividualUnwellClean[!is.na(IndividualUnwellClean)]))*100)
            

# Check row sums
rowSums(Illness.BySett.All[,8:9])

# Illness BY MPA

Illness.ByMPA.All <- 
  DemosSBS %>%
  group_by(MPAID, Treatment, MonitoringYear) %>%
  summarise(Ill = length(IndividualUnwellClean[IndividualUnwellClean==1 & !is.na(IndividualUnwellClean)]),
            Not.Ill = length(IndividualUnwellClean[IndividualUnwellClean==0 & !is.na(IndividualUnwellClean)]),
            Percent.Ill = (length(IndividualUnwellClean[IndividualUnwellClean==1 & !is.na(IndividualUnwellClean)])/length(IndividualUnwellClean[!is.na(IndividualUnwellClean)]))*100,
            Percent.Not.Ill = (length(IndividualUnwellClean[IndividualUnwellClean==0 & !is.na(IndividualUnwellClean)])/length(IndividualUnwellClean[!is.na(IndividualUnwellClean)]))*100)

# Check row sums
rowSums(Illness.ByMPA.All[,6:7])

# Combine HH synthesized results for Education and Illness BY SETTLEMENT

Synth.DemosSBS.BySett.All <-
  left_join(AdultEducation.BySett.All, HHHEducation.BySett.All, by = c("SettlementID", "MPAID", "SettlementName", "Treatment", "MonitoringYear")) %>%
  left_join(., Illness.BySett.All, by = c("SettlementID", "MPAID", "SettlementName", "Treatment", "MonitoringYear"))

# Combine HH synthesized results for Education and Illness BY MPA

Synth.DemosSBS.ByMPA.All <-
  left_join(AdultEducation.ByMPA.All, HHHEducation.ByMPA.All, by = c("MPAID", "Treatment", "MonitoringYear")) %>%
  left_join(., Illness.ByMPA.All, by = c("MPAID", "Treatment", "MonitoringYear"))

# ----3 FOCUS GROUP DISCUSSION VARIABLES  ----

# ---- 3.1 Prepare FGD data for analysis  ----

SBS.FGD.Users <- rename(SBS.FGD.Users, MPAID = SiteCode, SettlementID = SettlementCode)


SBS.FGD.Users <- SBS.FGD.Users[SBS.FGD.Users$MPAID>14 &
                                 grepl("99",SBS.FGD.Users$UserNameE)==F,c("MPAID","SettlementID",
                                                                          "UserCode","UserNameE",
                                                                          "ParticipateEstablish","ParticipateBoundaries",
                                                                          "ParticipateAdmin","ParticipateRules", "YEAR")]


SBS.FGD.Users$ParticipateEstablish <- as.integer(levels(SBS.FGD.Users$ParticipateEstablish)) [SBS.FGD.Users$ParticipateEstablish]
SBS.FGD.Users$ParticipateRules <- as.integer(levels(SBS.FGD.Users$ParticipateRules)) [SBS.FGD.Users$ParticipateRules]

# Create clean FGD dataframe

SBS.FGD.Users.Clean <- SBS.FGD.Users %>%
  mutate(ParticipateEstablish = ifelse(ParticipateEstablish > 989, NA, ParticipateEstablish),
         ParticipateBoundaries = ifelse(ParticipateBoundaries > 989, NA, ParticipateBoundaries),
         ParticipateAdmin = ifelse(ParticipateAdmin > 989, NA, ParticipateAdmin),
         ParticipateRules = ifelse(ParticipateRules > 989, NA, ParticipateRules))




SBS.FGD.Users.Clean <- SBS.FGD.Users.Clean %>%
  left_join(Settlements, ., by=c("MPAID", "SettlementID"))

SBS.FGD.Users.Clean$MonitoringYear <- factor("Baseline", levels=c("Baseline","2 Year Post", "3 Year Post", "4 Year Post"),
                                             ordered=T)
#---- 3.2 Synthesize FGD results ----

# BY SETTLEMENT

SBS.FGD.PropUsers.BySett.All <-
  SBS.FGD.Users.Clean %>%
  group_by(SettlementID, SettlementName, MPAID, Treatment, MonitoringYear) %>%
  summarise(ParticipateEstablish=(length(UserNameE[ParticipateEstablish==1 &
                                                     !is.na(ParticipateEstablish)])/
                                    length(UserNameE[!is.na(ParticipateEstablish)]))*100,
            ParticipateBnd=(length(UserNameE[ParticipateBoundaries==1 &
                                               !is.na(ParticipateBoundaries)])/
                              length(UserNameE[!is.na(ParticipateBoundaries)]))*100,
            ParticipateOrg=(length(UserNameE[ParticipateAdmin==1 &
                                               !is.na(ParticipateAdmin)])/
                              length(UserNameE[!is.na(ParticipateAdmin)]))*100,
            ParticipateRules=(length(UserNameE[ParticipateRules==1 &
                                                 !is.na(ParticipateRules)])/
                                length(UserNameE[!is.na(ParticipateRules)]))*100)


# BY MPA

SBS.FGD.PropUsers.ByMPA.All <-
  SBS.FGD.Users.Clean %>%
  group_by(MPAID, Treatment, MonitoringYear) %>%
  summarise(ParticipateEstablish=(length(UserNameE[ParticipateEstablish==1 &
                                                     !is.na(ParticipateEstablish)])/
                                    length(UserNameE[!is.na(ParticipateEstablish)]))*100,
            ParticipateBnd=(length(UserNameE[ParticipateBoundaries==1 &
                                               !is.na(ParticipateBoundaries)])/
                              length(UserNameE[!is.na(ParticipateBoundaries)]))*100,
            ParticipateOrg=(length(UserNameE[ParticipateAdmin==1 &
                                               !is.na(ParticipateAdmin)])/
                              length(UserNameE[!is.na(ParticipateAdmin)]))*100,
            ParticipateRules=(length(UserNameE[ParticipateRules==1 &
                                                 !is.na(ParticipateRules)])/
                                length(UserNameE[!is.na(ParticipateRules)]))*100)

# ---- 4 KEY INFORMANT INTERVIEW VARIABLES ----

# ---- 4.1  Prepare Species Rules (KII) data for analysis ----


SBS.KII.sppRules <- rename(SBS.KII.sppRules, MPAID = SiteCode, SettlementID = SettlementCode)

SBS.KII.sppRules <- rbind.data.frame(SBS.KII.sppRules[SBS.KII.sppRules$MPAID>14,
                                                      c("MPAID", "SettlementID","SpeciesNameL","SppRule")])

SBS.KII.sppRules$SpeciesNameL <- ifelse(grepl("99",SBS.KII.sppRules$SpeciesNameL),NA,SBS.KII.sppRules$SpeciesNameL)
SBS.KII.sppRules$SppRule <- ifelse(grepl("99",SBS.KII.sppRules$SppRule),NA,SBS.KII.sppRules$SppRule)




SBS.KII.sppRules <- SBS.KII.sppRules %>%
  left_join(., HHData.w.SBS.only.vars[,c("HouseholdID", "MPAID", "MonitoringYear", "SettlementID")],by=c("MPAID", "SettlementID")) %>%
  left_join(Settlements, ., by=c("MPAID", "SettlementID"))

SBS.KII.sppRules$MonitoringYear <- factor("Baseline", levels=c("Baseline","2 Year Post", "3 Year Post", "4 Year Post"),
                                          ordered=T)
# ---- 4.2 Synthesize Species Rules (KII) results ----

# BY SETTLEMENT

SBS.KII.sppRules.BySett.All <-
  SBS.KII.sppRules %>%
  group_by(SettlementID, SettlementName, MPAID, Treatment, MonitoringYear) %>%
  summarise(PropRuleSpp=length(SppRule[SppRule==1 & !is.na(SppRule)])/length(SpeciesNameL[!is.na(SpeciesNameL)])*100)

# BY MPA

SBS.KII.sppRules.ByMPA.All <-
  SBS.KII.sppRules %>%
  group_by(MPAID, MonitoringYear, Treatment) %>%
  summarise(PropRuleSpp=length(SppRule[SppRule==1 & !is.na(SppRule)])/length(SpeciesNameL[!is.na(SpeciesNameL)])*100)


# Prepare Habitat Rules data for analysis

SBS.KII.habRules <- rename(SBS.KII.habRules, MPAID = SiteCode, SettlementID = SettlementCode)


SBS.KII.habRules <- rbind.data.frame(SBS.KII.habRules[SBS.KII.habRules$MPAID>14,
                                                      c("MPAID", "SettlementID","HabitatNameL","HabRule")])


SBS.KII.habRules$HabitatNameL <- ifelse(grepl("99",SBS.KII.habRules$HabitatNameL),NA,SBS.KII.habRules$HabitatNameL)
SBS.KII.habRules$HabRule <- ifelse(grepl("99",SBS.KII.habRules$HabRule),NA,SBS.KII.habRules$HabRule)


SBS.KII.habRules <- SBS.KII.habRules %>%
  left_join(., HHData.w.SBS.only.vars[,c("HouseholdID", "MPAID", "MonitoringYear", "SettlementID")],by=c("MPAID", "SettlementID")) %>%
  left_join(Settlements, ., by=c("MPAID", "SettlementID"))

SBS.KII.habRules$MonitoringYear <- factor("Baseline", levels=c("Baseline","2 Year Post", "3 Year Post", "4 Year Post"),
                                          ordered=T)
# Synthesize Habitat Rules (KII) results

# BY SETTLEMENT

SBS.KII.habRules.BySett.All <-
  SBS.KII.habRules %>%
  group_by(SettlementID, SettlementName, MPAID, Treatment, MonitoringYear) %>%
  summarise(PropRuleHab=length(HabRule[HabRule==1 & !is.na(HabRule)])/length(HabitatNameL[!is.na(HabitatNameL)])*100)

# BY MPA

SBS.KII.habRules.ByMPA.All <-
  SBS.KII.habRules %>%
  group_by(MPAID, MonitoringYear, Treatment, MonitoringYear) %>%
  summarise(PropRuleHab=length(HabRule[HabRule==1 & !is.na(HabRule)])/length(HabitatNameL[!is.na(HabitatNameL)])*100)

# ---- 4.3 Combine Species and Habitat Rules and synthesis Mean Rules results KII) ----

# BY SETTLEMENT

SBS.MeanRules.BySett.All <-
  full_join(SBS.KII.sppRules.BySett.All,SBS.KII.habRules.BySett.All, by = c("SettlementID", "SettlementName", "MPAID", "Treatment", "MonitoringYear")) %>%
  melt(id.vars=c("SettlementID", "Treatment"),measure.vars=c("PropRuleSpp","PropRuleHab")) %>%
  group_by(SettlementID, Treatment) %>%
  summarise(Mean.PropRule=mean(value, na.rm=TRUE))

# BY MPA

SBS.MeanRules.ByMPA.All <-
  full_join(SBS.KII.sppRules.ByMPA.All,SBS.KII.habRules.ByMPA.All,by = c("MPAID", "MonitoringYear", "Treatment")) %>%
  melt(id.vars=c("MPAID", "Treatment"),measure.vars=c("PropRuleSpp","PropRuleHab")) %>%
  group_by(MPAID, Treatment) %>%
  summarise(Mean.PropRule=mean(value, na.rm=TRUE))

# ---- 4.4 Create single dataframe for Rules results (KII) ----


SBS.Rules.BySett.All <- 
  full_join(SBS.MeanRules.BySett.All,SBS.KII.sppRules.BySett.All) %>%
  full_join(.,SBS.KII.habRules.BySett.All)


SBS.Rules.ByMPA.All <- 
  full_join(SBS.MeanRules.ByMPA.All,SBS.KII.sppRules.ByMPA.All) %>%
  full_join(.,SBS.KII.habRules.ByMPA.All)


# ---- 5 COMBINED ALL RESULTS (HH, FGD, and KII) ----

# BY SETTLEMENT

Techreport.SBS.BySett.All <-
  left_join(Synth.HHDataSBS.BySett.All, Synth.DemosSBS.BySett.All) %>%
  left_join(., SBS.Rules.BySett.All) %>%
  left_join(., SBS.FGD.PropUsers.BySett.All)

# BY SETTLEMENT subset to TREATMENT SITES ONLY

Techreport.SBS.BySett <-
  Techreport.SBS.BySett.All %>%
  filter(Treatment==1)

# BY SETTLEMENT subset to CONTROL SITES ONLY

Techreport.SBS.Control <-
  Techreport.SBS.BySett.All %>%
  filter(Treatment==0)

#BY MPA

Techreport.SBS.ByMPA.All <-
  left_join(Synth.HHDataSBS.ByMPA.All, Synth.DemosSBS.ByMPA.All) %>%
  left_join(., SBS.Rules.ByMPA.All) %>%
  left_join(., SBS.FGD.PropUsers.ByMPA.All)

# BY MPA subset to TREATMENT SITES ONLY

Techreport.SBS.ByMPA <-
  Techreport.SBS.ByMPA.All %>%
  filter(Treatment==1)

# BY MPA subset to CONTROL SITES ONLY
  
  Techreport.SBS.ByMPA.Control <-
  Techreport.SBS.ByMPA.All %>%
  filter(Treatment==0)
  

#---- 6 PREPARE ALL RESULTS FOR PLOTTING (HH, FGD, and KII) ----

# ---- 6.1 Subset to MPA of choice and create combined dataset with all results in PLOT FORMAT ----

Koon.level.SBSPropData.status.Control <- 
  rbind.data.frame(data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Techreport.SBS.ByMPA.Control[Techreport.SBS.ByMPA.Control$MPAID==18,c("Num.EthnicGroups", 
                                                                                                    "Majority.Ethnic1", "Majority.Ethnic2", "Majority.Ethnic3", "Majority.Ethnic4", 
                                                                                                    "Majority.Ethnic5", "Percent.MajorityEthnic1", "Percent.MajorityEthnic2", 
                                                                                                    "Percent.MajorityEthnic3", "Percent.MajorityEthnic4", "Percent.OtherEthnic", 
                                                                                                    "Econ.Status.Much.Worse", "Econ.Status.Slighly.Worse", "Econ.Status.Neutral", 
                                                                                                    "Econ.Status.Slightly.Better", "Econ.Status.Much.Better", "Threat.Mean", 
                                                                                                    "Threat.None", "Threat.One", "Threat.Two", "Threat.Three", "Threat.Four", 
                                                                                                    "Threat.Minimum.Five", "Member.No", "Member.Yes", "Num.Member.Yes", 
                                                                                                    "Num.Member.No", "Meeting.No", "Meeting.Yes", "Num.Meeting.Yes", 
                                                                                                    "Num.Meeting.No", "Contribution", "Num.Member.Yes.Meeting.Yes", 
                                                                                                    "Num.Member.Yes.Meeting.No", "Prop.Member.Yes.Meeting.Yes", "Prop.Member.Yes.Meeting.No", 
                                                                                                    "AdultEducNone", "AdultEducPre", "AdultEducPrim", "AdultEducMid", 
                                                                                                    "AdultEducSec", "AdultEducHigher", "HHHEducNone", "HHHEducPre", 
                                                                                                    "HHHEducPrim", "HHHEducMid", "HHHEducSec", "HHHEducHigher", "Ill", 
                                                                                                    "Not.Ill", "Percent.Ill", "Percent.Not.Ill", "Mean.PropRule", 
                                                                                                    "PropRuleSpp", "PropRuleHab", "ParticipateEstablish", "ParticipateBnd", 
                                                                                                    "ParticipateOrg", "ParticipateRules", "Percent.GreatlyIncreased.SocConflict", 
                                                                                                    "Percent.Increased.SocConflict", "Percent.Same.SocConflict", "Percent.Decreased.SocConflict", "Percent.GreatlyDecreased.SocConflict")]),
                   data.frame(MonitoringYear="Baseline",
                              SettlementID=0,
                              SettlementName="Koon MPA",
                              Techreport.SBS.ByMPA[Techreport.SBS.ByMPA$MPAID==18,c("Num.EthnicGroups", 
                                                                                    "Majority.Ethnic1", "Majority.Ethnic2", "Majority.Ethnic3", "Majority.Ethnic4", 
                                                                                    "Majority.Ethnic5", "Percent.MajorityEthnic1", "Percent.MajorityEthnic2", 
                                                                                    "Percent.MajorityEthnic3", "Percent.MajorityEthnic4", "Percent.OtherEthnic", 
                                                                                    "Econ.Status.Much.Worse", "Econ.Status.Slighly.Worse", "Econ.Status.Neutral", 
                                                                                    "Econ.Status.Slightly.Better", "Econ.Status.Much.Better", "Threat.Mean", 
                                                                                    "Threat.None", "Threat.One", "Threat.Two", "Threat.Three", "Threat.Four", 
                                                                                    "Threat.Minimum.Five", "Member.No", "Member.Yes", "Num.Member.Yes", 
                                                                                    "Num.Member.No", "Meeting.No", "Meeting.Yes", "Num.Meeting.Yes", 
                                                                                    "Num.Meeting.No", "Contribution", "Num.Member.Yes.Meeting.Yes", 
                                                                                    "Num.Member.Yes.Meeting.No", "Prop.Member.Yes.Meeting.Yes", "Prop.Member.Yes.Meeting.No", 
                                                                                    "AdultEducNone", "AdultEducPre", "AdultEducPrim", "AdultEducMid", 
                                                                                    "AdultEducSec", "AdultEducHigher", "HHHEducNone", "HHHEducPre", 
                                                                                    "HHHEducPrim", "HHHEducMid", "HHHEducSec", "HHHEducHigher", "Ill", 
                                                                                    "Not.Ill", "Percent.Ill", "Percent.Not.Ill", "Mean.PropRule", 
                                                                                    "PropRuleSpp", "PropRuleHab", "ParticipateEstablish", "ParticipateBnd", 
                                                                                    "ParticipateOrg", "ParticipateRules", "Percent.GreatlyIncreased.SocConflict", "Percent.Increased.SocConflict", 
                                                                                    "Percent.Same.SocConflict", "Percent.Decreased.SocConflict", "Percent.GreatlyDecreased.SocConflict")]))
      

null.row.PropData <- 
  matrix(rep(NA,67),ncol=67,dimnames=list(NULL,colnames(Koon.level.SBSPropData.status.Control)))


Koon.SBSPropData.Techreport.status.TREATMENT <- 
  Techreport.SBS.BySett[Techreport.SBS.BySett$MPAID==18,c("SettlementID","SettlementName", 
                                                          "Num.EthnicGroups", "Majority.Ethnic1", "Majority.Ethnic2", "Majority.Ethnic3", 
                                                          "Majority.Ethnic4", "Majority.Ethnic5", "Percent.MajorityEthnic1", 
                                                          "Percent.MajorityEthnic2", "Percent.MajorityEthnic3", "Percent.MajorityEthnic4", 
                                                          "Percent.OtherEthnic", "Econ.Status.Much.Worse", "Econ.Status.Slighly.Worse", 
                                                          "Econ.Status.Neutral", "Econ.Status.Slightly.Better", "Econ.Status.Much.Better", 
                                                          "Threat.Mean", "Threat.None", "Threat.One", "Threat.Two", "Threat.Three", 
                                                          "Threat.Four", "Threat.Minimum.Five", "Member.No", "Member.Yes", 
                                                          "Num.Member.Yes", "Num.Member.No", "Meeting.No", "Meeting.Yes", 
                                                          "Num.Meeting.Yes", "Num.Meeting.No", "Contribution", "Num.Member.Yes.Meeting.Yes", 
                                                          "Num.Member.Yes.Meeting.No", "Prop.Member.Yes.Meeting.Yes", "Prop.Member.Yes.Meeting.No", 
                                                          "AdultEducNone", "AdultEducPre", "AdultEducPrim", "AdultEducMid", 
                                                          "AdultEducSec", "AdultEducHigher", "HHHEducNone", "HHHEducPre", 
                                                          "HHHEducPrim", "HHHEducMid", "HHHEducSec", "HHHEducHigher", "Ill", 
                                                          "Not.Ill", "Percent.Ill", "Percent.Not.Ill", "Mean.PropRule", 
                                                          "PropRuleSpp", "PropRuleHab", "ParticipateEstablish", "ParticipateBnd", 
                                                          "ParticipateOrg", "ParticipateRules", "Percent.GreatlyIncreased.SocConflict", "Percent.Increased.SocConflict", "Percent.Same.SocConflict", 
                                                          "Percent.Decreased.SocConflict", "Percent.GreatlyDecreased.SocConflict")]

Koon.SBSPropData.Techreport.status.TREATMENT <- 
  Koon.SBSPropData.Techreport.status.TREATMENT[rev(order(Koon.SBSPropData.Techreport.status.TREATMENT$SettlementName)),]

Koon.SBSPropData.Techreport.status.PLOTFORMAT <- 
  rbind.data.frame(Koon.level.SBSPropData.status.Control[c("SettlementID", "SettlementName", "Num.EthnicGroups", 
                                                   "Majority.Ethnic1", "Majority.Ethnic2", "Majority.Ethnic3", "Majority.Ethnic4", 
                                                   "Majority.Ethnic5", "Percent.MajorityEthnic1", "Percent.MajorityEthnic2", 
                                                   "Percent.MajorityEthnic3", "Percent.MajorityEthnic4", "Percent.OtherEthnic", 
                                                   "Econ.Status.Much.Worse", "Econ.Status.Slighly.Worse", "Econ.Status.Neutral", 
                                                   "Econ.Status.Slightly.Better", "Econ.Status.Much.Better", "Threat.Mean", 
                                                   "Threat.None", "Threat.One", "Threat.Two", "Threat.Three", "Threat.Four", 
                                                   "Threat.Minimum.Five", "Member.No", "Member.Yes", "Num.Member.Yes", 
                                                   "Num.Member.No", "Meeting.No", "Meeting.Yes", "Num.Meeting.Yes", 
                                                   "Num.Meeting.No", "Contribution", "Num.Member.Yes.Meeting.Yes", 
                                                   "Num.Member.Yes.Meeting.No", "Prop.Member.Yes.Meeting.Yes", "Prop.Member.Yes.Meeting.No", 
                                                   "AdultEducNone", "AdultEducPre", "AdultEducPrim", "AdultEducMid", 
                                                   "AdultEducSec", "AdultEducHigher", "HHHEducNone", "HHHEducPre", 
                                                   "HHHEducPrim", "HHHEducMid", "HHHEducSec", "HHHEducHigher", "Ill", 
                                                   "Not.Ill", "Percent.Ill", "Percent.Not.Ill", "Mean.PropRule", 
                                                   "PropRuleSpp", "PropRuleHab", "ParticipateEstablish", "ParticipateBnd","ParticipateOrg", "ParticipateRules",
                                                   "Percent.GreatlyIncreased.SocConflict", "Percent.Increased.SocConflict", "Percent.Same.SocConflict", 
                                                   "Percent.Decreased.SocConflict", "Percent.GreatlyDecreased.SocConflict"
                                                   )],
                   null.row.PropData[c("SettlementID", "SettlementName", "Num.EthnicGroups", 
                                       "Majority.Ethnic1", "Majority.Ethnic2", "Majority.Ethnic3", "Majority.Ethnic4", 
                                       "Majority.Ethnic5", "Percent.MajorityEthnic1", "Percent.MajorityEthnic2", 
                                       "Percent.MajorityEthnic3", "Percent.MajorityEthnic4", "Percent.OtherEthnic", 
                                       "Econ.Status.Much.Worse", "Econ.Status.Slighly.Worse", "Econ.Status.Neutral", 
                                       "Econ.Status.Slightly.Better", "Econ.Status.Much.Better", "Threat.Mean", 
                                       "Threat.None", "Threat.One", "Threat.Two", "Threat.Three", "Threat.Four", 
                                       "Threat.Minimum.Five", "Member.No", "Member.Yes", "Num.Member.Yes", 
                                       "Num.Member.No", "Meeting.No", "Meeting.Yes", "Num.Meeting.Yes", 
                                       "Num.Meeting.No", "Contribution", "Num.Member.Yes.Meeting.Yes", 
                                       "Num.Member.Yes.Meeting.No", "Prop.Member.Yes.Meeting.Yes", "Prop.Member.Yes.Meeting.No", 
                                       "AdultEducNone", "AdultEducPre", "AdultEducPrim", "AdultEducMid", 
                                       "AdultEducSec", "AdultEducHigher", "HHHEducNone", "HHHEducPre", 
                                       "HHHEducPrim", "HHHEducMid", "HHHEducSec", "HHHEducHigher", "Ill", 
                                       "Not.Ill", "Percent.Ill", "Percent.Not.Ill", "Mean.PropRule", 
                                       "PropRuleSpp", "PropRuleHab", "ParticipateEstablish", "ParticipateBnd","ParticipateOrg", "ParticipateRules",
                                       "Percent.GreatlyIncreased.SocConflict", "Percent.Increased.SocConflict", "Percent.Same.SocConflict", 
                                       "Percent.Decreased.SocConflict", "Percent.GreatlyDecreased.SocConflict")],
                   Koon.SBSPropData.Techreport.status.TREATMENT)

# - make SettlementName an ordered factor for plotting
Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName),"",
         as.character(Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName))

Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName <-
  factor(Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName,
         levels=unique(Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Koon.SBSPropData.Techreport.status.PLOTFORMAT$Dummy <- 
  ifelse(Koon.SBSPropData.Techreport.status.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")

# View(Koon.SBSPropData.Techreport.status.PLOTFORMAT)





