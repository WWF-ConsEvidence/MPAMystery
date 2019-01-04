# ---
# code: Process 'Middle 15' social outcomes for MPA monitoring
# author: Kelly Claborn, Louise Glew, louise.glew@gmail.com
# created: February 2018
# modified: March 2018

# -----
# inputs: 

# KC_HHData_ForMPAMystery -- in BHS Master Database in Access
#   -this table can be created by running the query saved in the 
#    'KC_CodingSubsettingAnalysis' Group, named 'Q_HHData_ForMPAMystery'.
#   -this table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculated into the food security, material assets, place attachment, marine
#    tenure, and school enrollment indexes -- along with other livelihood and household 
#    demographic variables.
# KC_IndDemos_ForMPAMystery -- in BHS Master Database in Access
#   -the IndoDemos table can be created by running the query saved in the 
#    'KC_CodingSubsettingAnalysis' Group, named 'Q_IndividualDemos_ForMPAMystery'.
#   -the IndDemos table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculate the school enrollment rate, days unwell, and head of household gender. 
# HH_tbl_SETTLEMENT -- in BHS Master Database in Access
#   -the Settlement table provides the names and treatment vs. non-treatment status for 
#    each settlementID.

# -----
# outputs:
# Middle15 data frame

# -----
# Code sections
#  1) Call libraries and import raw data
#  2) Data wrangling
#  3) Compute indices
#  4) Combine into Middle 15 dataframe

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Call in libraries, import raw data and source functions ----
#         1.1 libraries
#         1.2 Source functions
#         1.3 Import raw data
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Call libraries ----
pacman::p_load(dplyr, readxl, tidyr, dummies)


# ---- 1.2 Source functions----
source('2_Functions/1_Aggregate_wrangle/Function_str_clean.R')
source('2_Functions/2_Analysis/Function_outcome_ATT_method1.R')


# ----1.3 Import raw data----

# !!! Select ONE option to import data - ONLY CHOOSE ONE !!!

# OPTION 1: Use ODBC connection to access database and import tables
MPAMysteryDB <- odbcConnect("Unified.Social.MPAMystery")

source('2_Social/SourcedScripts/SQLqueries_AccessODBC.R')


# # OPTION 2: Set working directory and import flat data files (.csv files)
# setwd("~/Dropbox/BHS")
# HHData <- read.csv('BHS_HHData.csv',header=T,sep=',')
# 
# IndDemos <- read.csv('BHS_HHDemos.csv',header=T,sep=',')
# IndDemos <- left_join(IndDemos,HHData[,c("HouseholdID","SettlementID")],by="HouseholdID")
# 
# Settlements <- read.csv('BHS_HH_tbl_SETTLEMENT.csv',header=T,sep=',')
# Settlements <- Settlements[,c(1,3:5)]

#
education_lkp <- read.delim("2_Social/FlatDataFiles/BHS/education_lkp.txt")


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Data wrangling ---
#         2.1 Remove settlement without post-baseline data, recode Kaimana controls
#         2.2 Define monitoring year for each MPA
#         2.3 Subset variables from HHData for Middle Fifteen calculations & descriptive statistics
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Remove settlements without post-baseline data, re-code Kaimana control settlements ----

HHData <- HHData[HHData$SettlementID!=84 &
                   HHData$SettlementID!=96 &
                   HHData$SettlementID!=97 &
                   HHData$SettlementID!=98 &
                   HHData$SettlementID!=99 &
                   HHData$SettlementID!=100 &
                   HHData$SettlementID!=101,]

IndDemos <- IndDemos[!is.na(IndDemos$SettlementID) &
                       IndDemos$SettlementID!=84 &
                       IndDemos$SettlementID!=96 &
                       IndDemos$SettlementID!=97 &
                       IndDemos$SettlementID!=98 &
                       IndDemos$SettlementID!=99 &
                       IndDemos$SettlementID!=100 &
                       IndDemos$SettlementID!=101,]

Settlements <- Settlements[!is.na(Settlements$SettlementID) &
                             Settlements$SettlementID!=84 &
                             Settlements$SettlementID!=96 &
                             Settlements$SettlementID!=97 &
                             Settlements$SettlementID!=98 &
                             Settlements$SettlementID!=99 &
                             Settlements$SettlementID!=100 &
                             Settlements$SettlementID!=101,]
Settlements$SettlementName <- as.character(Settlements$SettlementName)


Settlements$Treatment <- ifelse(Settlements$SettlementID==83 | Settlements$SettlementID==91 | Settlements$SettlementID==92,
                                "0",Settlements$Treatment)



# ---- 2.2 Define monitoring year for each MPA ----

MonitoringYear <- group_by(HHData,MPAID)
MonitoringYear <- summarise(MonitoringYear,
                            Baseline=min(InterviewYear),
                            TwoYear=as.integer(min(InterviewYear)+2),
                            FourYear=as.integer(min(InterviewYear)+4),
                            SevenYear=as.integer(min(InterviewYear)+7))
MonitoringYear <- left_join(HHData[,c("HouseholdID","MPAID")],
                            MonitoringYear,
                            by="MPAID")

HHData$MonitoringYear <- factor(mapply(a=HHData$HouseholdID,
                                       b=HHData$InterviewYear,
                                       function(a,b){
                                         ifelse(b==MonitoringYear$Baseline[MonitoringYear$HouseholdID==a],"Baseline",
                                                ifelse(b==MonitoringYear$TwoYear[MonitoringYear$HouseholdID==a],"2 Year Post",
                                                       ifelse(b==MonitoringYear$FourYear[MonitoringYear$HouseholdID==a],"4 Year Post",
                                                              ifelse(b==MonitoringYear$SevenYear[MonitoringYear$HouseholdID==a],"7 Year Post",NA))))
                                       }),
                                levels=c("Baseline","2 Year Post","4 Year Post"),
                                ordered=T)

# ---- 2.3 Subset variables from HHData for Middle Fifteen calculations & descriptive statistics ----

FS <- HHData[,c(1:4,11:17, 68:78)]
MA <- HHData[,c(1,29:40)]
PA <- HHData[,c(1,47:53)]
MT <- HHData[,c(1,59:64)]
ET <- HHData[,c("HouseholdID","EconomicStatusTrend")] 
MP <- HHData[,c("HouseholdID","MarineGroup")]
OP <- HHData[,c("HouseholdID","OtherGroup")]
SC <- HHData[,c("HouseholdID", "SocialConflict")]
HHLivelihood <- HHData[,c(1:3,79:90)]
HHDemos <- HHData[,c(1:3,91:94,99)]
HeadOfHH <- IndDemos[IndDemos$RelationHHH==0 &
                       !is.na(IndDemos$RelationHHH),1:4]

HHDataIdentifiers <- HHData %>%
  dplyr::select(HouseholdID, MPAID, MonitoringYear)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: "Middle Fifteen" Indexes ----
#         3.1 Economic well-being
#         3.2 Culture
#         3.3 Empowerment
#         3.4 Health
#         3.5 Education
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Economic well-being ----

  #-- 3.1.1 Material Assets

MA <- MA %>%
  mutate(MAIndex = ifelse(MA$RemoveMA=="No",
        rowSums(MA[,3:13],
        na.rm=TRUE),
        NA)) %>%
  dplyr::select(HouseholdID,MAIndex)

  #-- 3.1.2 Occupational dependence on marine fisheries

OD <- HHLivelihood %>%
  mutate(ODIndex = ifelse(HHLivelihood$PrimaryLivelihood==3,3,
                         (ifelse((HHLivelihood$SecondaryLivelihood==3 |HHLivelihood$TertiaryLivelihood==3),
                                 2,1)))) %>%
  dplyr::select(HouseholdID, ODIndex)         

  #-- 3.1.3 Economic status trend

ET <- ET %>%
  mutate(EconomicStatusTrend = ifelse(EconomicStatusTrend>=990,NA,EconomicStatusTrend),
         EconTrend = recode(EconomicStatusTrend, "1"="0","2"="0","3"="1","4"="2","5"="2", "NA"="NA"),
         EconDecline = ifelse(EconTrend==0,1,0),
         EconStable = ifelse(EconTrend==1,1,0),
         EconIncrease =ifelse(EconTrend==2,1,0)) %>%
  dplyr::select(HouseholdID, EconDecline, EconStable, EconIncrease)
         
# ---- 3.2 Culture ----

  #-- 3.2.1 Place attachment
PA <- PA %>%
  mutate(PAIndex = ifelse(PA$RemovePA=="No",
                     round(rowMeans(PA[,3:8],
                                    na.rm=TRUE),2),
                     NA))%>%
  dplyr::select(HouseholdID, PAIndex)

  #-- 3.2.2 Social conflict
SC <- SC %>%
 mutate(SocialConflict = ifelse(SocialConflict>=990,NA,SocialConflict),
      SocialConflict = as.numeric(recode(SocialConflict, "0"="0", "1" ="0","2"="0", "3"="1", "4"="2", "5"="2")),
      ConDecrease = ifelse(SocialConflict==2,1,0),
      ConStable = ifelse(SocialConflict==1,1,0),
      ConIncrease = ifelse(SocialConflict==0,1,0)) %>%
 dplyr::select(HouseholdID, SocialConflict, ConDecrease, ConStable, ConIncrease)

# ---- 3.3 Empowerment ----

  #-- 3.3.1 Marine tenure
MT <- MT %>%
  mutate(MTIndex =ifelse(MT$RemoveMT=="No",
                      rowSums(MT[,3:7],na.rm=TRUE),
                              NA), 
         Man.Excl.Trans =ifelse(RemoveMT=="No", 
                            ifelse((MT$RightsManageClean==1 |MT$RightsExcludeClean==1 |MT$RightsTransferClean==1),1,0),
                                NA),
         Acc.Harv = ifelse(RemoveMT=="No",
                        ifelse((MT$RightsAccessClean==1 |MT$RightsHarvestClean==1),1,0), 
                              NA)) %>%
  dplyr::select(HouseholdID, MTIndex, Acc.Harv,Man.Excl.Trans)

  #--3.3.2 Participation in community organizations 

MP <- MP %>%
  mutate(MarineGroup = ifelse(MarineGroup>1, NA, MarineGroup))
     
OP <- OP %>%
  mutate(OtherGroup = ifelse(OtherGroup>1, NA, OtherGroup))

# ---- 3.4 Health ----

  #-- 3.4.1 Food security (household and child)

FS <- FS %>%
  mutate(FSIndex = as.character(ifelse(RemoveFS=="No",
                                  rowSums(FS[,6:11],na.rm=TRUE),
                                      NA)),
         FSIndex = recode(FSIndex, '0'='0', '1'='2.04','2'='2.99','3'='3.77','4'='4.5','5'='5.38','6'='6.06'),
         FSIndex = 6.06-as.numeric(FSIndex),
         CFSIndex = ifelse(RemovecFS=="No",
                            rowSums(FS[15:22],na.rm=TRUE),
                            NA),
         CFSIndex.std = as.numeric(recode(CFSIndex, "0" ="0", "1"="2.9", "2"="4.2","3"="5.4","4"="6.3","5"="6.9","6"="7.5","7"="8.2","8"="8.7")),
         CFS.cat = ifelse(RemovecFS=="No",
                             ifelse(CFSIndex>=6.9,1,0),
                             NA),
         CFSIndex.inv = (8.7 - CFSIndex))%>%
  dplyr::select(HouseholdID, FSIndex,CFSIndex.std,CFS.cat,CFSIndex.inv)
       
 #-- 3.4.2 Household morbidity (% of household days over past four weeks affected by illness or injury)

MD <- 
  IndDemos%>%
  dplyr::select("HouseholdID","DaysUnwellClean") %>%
  replace(., is.na(.), 0) %>%
  group_by(HouseholdID) %>%
  summarise(DaysUnwell =sum(DaysUnwellClean)/(n()*28))  #number of individuals in household * number of days in four weeks


# ---- 3.5 Education ----


  #-- 3.5.1 School enrollment rate

Enrol <- 
  IndDemos %>%
  group_by(HouseholdID) %>%
  summarise(NumberChild=sum(ChildOrAdult,na.rm=T),
            EnrolledHH=sum(ChildEnrolled,na.rm=T),
            SERate=ifelse(NumberChild!=0 & !is.na(EnrolledHH),
                                   as.character(round((EnrolledHH/NumberChild)*100,2)),
                                   ifelse(NumberChild==0,
                                          "No School-Aged Children","No Data")))%>%
  dplyr::select(HouseholdID,SERate)

  #-- 3.5.2 Female enrollment
FemaleEnrol <- 
  IndDemos %>%
  group_by(HouseholdID,IndividualGenderClean) %>%
  filter(IndividualGenderClean==1)%>%
  summarise(NumberFemaleChild=sum(ChildOrAdult,na.rm=T),
            EnrolledFemaleHH=sum(ChildEnrolled,na.rm=T),
            FemaleSERate=ifelse(NumberFemaleChild!=0 & !is.na(EnrolledFemaleHH),
                                   as.character(round((EnrolledFemaleHH/NumberFemaleChild)*100,2)),
                                   ifelse(NumberFemaleChild==0,
                                          "No School-Aged Female Children","No Data")))%>%
  dplyr::select(HouseholdID,FemaleSERate)

  #-- 3.5.3 Male enrollment
MaleEnrol <- IndDemos %>%
  group_by(HouseholdID,IndividualGenderClean) %>%
  filter(IndividualGenderClean==0)%>%
  summarise(NumberMaleChild=sum(ChildOrAdult,na.rm=T),
            EnrolledMaleHH=sum(ChildEnrolled,na.rm=T),
            MaleSERate=ifelse(NumberMaleChild!=0 & !is.na(EnrolledMaleHH),
                                         as.character(round((EnrolledMaleHH/NumberMaleChild)*100,2)),
                                         ifelse(NumberMaleChild==0,
                                                "No School-Aged Male Children","No Data"))) %>%
  dplyr::select(HouseholdID,MaleSERate)

  #-- 3.5.4 Educational attainment of recent school-leavers (i.e., individuals who have left school during time of MPA estabishment)
 
education_lkp$IndividualEducation<-str_clean(education_lkp$IndividualEducation) #tidy education look up data
education_lkp<-unique(education_lkp) #remove any duplicate values from education look up data
 
                                       
Attain <-IndDemos %>%
    filter(IndividualAgeClean>18 & IndividualAgeClean<30 & RelationHHH == 0) %>%
    dplyr::select("HouseholdID", "IndividualEducationClean") %>%
    mutate(IndividualEducationClean = str_clean(IndividualEducationClean))

Attain <- left_join (Attain, education_lkp, by=c("IndividualEducationClean"="IndividualEducation"))
Attain$ed.level[Attain$ed.level>=990] <- NA 

# Create education attainment dummy variables
Attain <- cbind(Attain, dummy(Attain$ed.level, sep="."))  ## need to tuck this into dplyr call eventually, requires lazyeval
Attain <- Attain %>% 
  dplyr::select(HouseholdID, Attain.2, Attain.3, Attain.4, Attain.5)       
         
# -- 3.5.5 Environmental awareness 

#-- Local threat/action

    #HOLD UNTIL DC


    # HOLD UNTIL DC

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Create 'Middle 15' dataframe ----
#         4.1 Combine into dataframe
#         4.2 Rename NAs in education objects
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    

# ---- 4.1 Combine into dataframe ----

Middle15 <- 
  left_join(HHDataIdentifiers, MA, by="HouseholdID") %>%
  left_join(FS, by="HouseholdID") %>%
  left_join(PA, by="HouseholdID") %>%
  left_join(MT, by="HouseholdID") %>%
  left_join(MP, by="HouseholdID") %>%
  left_join(OP, by="HouseholdID") %>%
  left_join(OD, by="HouseholdID") %>%
  left_join(ET, by="HouseholdID") %>%
  left_join(SC, by="HouseholdID") %>%
  left_join(MD, by="HouseholdID") %>%
  left_join(Enrol, by="HouseholdID") %>% 
  left_join(FemaleEnrol, by="HouseholdID") %>%
  left_join(MaleEnrol, by="HouseholdID") %>%
  left_join(Attain, by="HouseholdID")


# ---- 4.2 Rename NAs in education objects ----
Middle15 <- Middle15 %>%
  mutate(SERate= as.numeric(ifelse(SERate=="No School-Aged Children",NA,SERate)),
         FemaleSERate= as.numeric(ifelse(FemaleSERate=="No School-Aged Female Children",NA,FemaleSERate)),
         MaleSERate= as.numeric(ifelse(MaleSERate=="No School-Aged Male Children",NA,MaleSERate)))


# ---- Remove all other objects ----

rm(MA,FS,MT,PA,OD,ET,SC,MD,MP,OP,Enrol,FemaleEnrol,MaleEnrol,Attain,HHDataIdentifiers,MonitoringYear,IndDemos)
