#
# code: Preprocess matching covariates function
# 
# author: Louise Glew, louise.glew@gmail.com
# date: May 2019
# modified: --
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files.R', local = T)
source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_household_indices.R')


# Creating yearsPost to create a continuous variable of time after baseline
HHData <- HHData %>%
 mutate(yearsPost = ifelse(MonitoringYear=="Baseline", 0,
                            as.integer(substr(MonitoringYear, 1, 1))))

#---- Import look up tables ----
ethnic.lkp <- import("x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx")
education.lkp <- import("x_Flat_data_files/1_Social/Inputs/education_lkp.xlsx")

# ---Create functions
# Function to remove all white space in string variables
trim <- function(x) gsub("^\\s+|\\s+$","",x)

# Function to clean string variables (lower case, remove punctuation)
str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
    trim()
}


#----Define function

# Age
age.bin<-c(0,20,30,40,50,60,70,990)

# duplicates created because of multiple HH heads in a household (in IndDemos)
HH.age <- IndDemos %>%
  filter(RelationHHH==0) %>%
  dplyr::select(HouseholdID,IndividualAge) %>%
  left_join(dplyr::select(HHData,HouseholdID,yearsPost),by="HouseholdID") %>%
  mutate(IndividualAge=.bincode(IndividualAge-yearsPost,age.bin,TRUE,TRUE)) %>% 
  dplyr::select(HouseholdID,IndividualAge)%>% 
  distinct(HouseholdID,.keep_all = T)

# Gender of Household Head (temp fix using distinct)
gender.HHH <- IndDemos %>% 
  filter(RelationHHH==0) %>% 
  dplyr::select("HouseholdID","IndividualGender") %>% 
  distinct(HouseholdID,.keep_all = T)

# Residency
resident.bin<-c(0,10,20,30,40,50,60,990)

HH.residency <- HHData %>%
  dplyr::select(HouseholdID,YrResident,MonitoringYear,yearsPost) %>%
  mutate(YearsResident=ifelse(MonitoringYear=="Baseline",.bincode(YrResident,resident.bin,TRUE,TRUE),
                              ifelse(YrResident>yearsPost,.bincode(YrResident-yearsPost,resident.bin,TRUE,TRUE),
                                     1))) %>% 
  dplyr::select(HouseholdID,YearsResident) %>% 
  na.omit()

# Dominant ethnicity
# some duplicates in ethnicity table (NAs), filtering out these here
ethnic.lkp1 <- ethnic.lkp %>% 
  distinct(std.eth.str,eth.iso,.keep_all = T) %>%
  filter(eth.iso!="NA")
# filter(!ethnic.id%in%c(2734,2813,5422,5425,5643)) # select out the specific five duplicates

HH.eth <- HHData %>% 
  dplyr::select(HouseholdID,PaternalEthnicity, MonitoringYear, SettlementID) %>% 
  mutate(PaternalEthnicity=str_clean(PaternalEthnicity)) %>% 
  left_join(ethnic.lkp1, by=c("PaternalEthnicity"="std.eth.str")) %>% 
  mutate(SettlYear=paste0(MonitoringYear,"_",SettlementID))

# this code gives you the top ethnicity for each settlement at each sampling period
max.eth <- HH.eth %>%
  group_by(SettlYear,eth.iso)%>%
  dplyr::summarise(freq.eth=n()) %>%
  top_n(1, freq.eth) 

HH.eth$dom.eth <- NA

# assign dominant ethnicity in a loop will assign a 0 if parentalEthinicity==NA
for (i in unique(HH.eth$SettlYear)){
  max.eth.dom<-  max.eth$eth.iso[max.eth$SettlYear==i]
  HH.eth$dom.eth[HH.eth$SettlYear==i] <- ifelse(HH.eth$eth.iso[HH.eth$SettlYear==i]%in%max.eth.dom,1,0)
}
HH.eth <- dplyr::select(HH.eth,HouseholdID,eth.iso,dom.eth)

# Education level of household head
# some duplicates in education table (NAs, perhaps white spaces), filtering out these here
#    dupl <- education.lkp$IndividualEducation[duplicated(education.lkp$IndividualEducation)]           
#    education.lkp[education.lkp$IndividualEducation%in%dupl,]
education.lkp1 <- education.lkp %>% 
  distinct(IndividualEducation,ed.level,.keep_all = T) %>%
  filter(ed.level!="NA")

# duplicates created because of multiple HH heads in a household (in IndDemos)
HH.ed <- IndDemos %>% 
  filter(RelationHHH==0) %>% 
  dplyr::select(HouseholdID,IndividualEducation) %>% 
  left_join(education.lkp1, by=c("IndividualEducation")) %>%
  dplyr::select(-IndividualEducation) %>%
  mutate(ed.level=ifelse(is.na(ed.level) | ed.level>=989, NA, as.numeric(ed.level))) %>%
  distinct(HouseholdID,.keep_all = T)

# dupl <- unique(IndDemos$HouseholdID[duplicated(IndDemos$HouseholdID) & IndDemos$RelationHHH==0])
# test <- HH.ed %>%
#   filter(HouseholdID%in%dupl ) %>%
#   arrange(HouseholdID)

# Children in Household
IndDemos$Child <- ifelse(IndDemos$IndividualAge<19,1,0) #  create new variable, child/adult
N.Child <- IndDemos%>%
  group_by(HouseholdID) %>% 
  summarise(n.child=sum(Child))

# Market distance
#create mean by settlement-year
market.mean.sett.yr <- HHData %>%
  group_by(SettlementID,MonitoringYear)%>%
  summarise (TimeMean.sett.yr=mean(TimeMarket, trim = 0.9,na.rm = T))

#create mean by settlement
market.mean.sett <- HHData %>%
  group_by(SettlementID)%>%
  summarise (TimeMean.sett=mean(TimeMarket, trim = 0.9,na.rm = T)) 

market.distance <- HHData %>% 
  dplyr::select(HouseholdID,TimeMarket,MonitoringYear,SettlementID) %>% 
  left_join(market.mean.sett.yr,by=c("SettlementID" = "SettlementID", "MonitoringYear"="MonitoringYear")) %>% 
  left_join(market.mean.sett,by=c("SettlementID" = "SettlementID")) %>% 
  mutate(TimeMarket=ifelse(is.na(TimeMarket),TimeMean.sett.yr,TimeMarket),
         TimeMarket=ifelse(is.na(TimeMarket),TimeMean.sett,TimeMarket)) %>% 
  dplyr::select(HouseholdID,TimeMarket)

head(market.distance)
# market.distance<-subset(HHData,select=c("HouseholdID","TimeMarket", "MonitoringYear","SettlementID"))
# market.distance$TimeMarket[market.distance$TimeMarket >=990] <- 990
# market.mean <-market.distance %>%
#   group_by(SettlementID,MonitoringYear)%>%
#   summarise (mean=mean(TimeMarket[TimeMarket!=990])) # subsequent rows handle blind codes, and missing data
# 
# market.mean$mean[is.na(market.mean$mean)]<- ave(market.mean$mean,
#                                  market.mean$SettlementID,
#                                  FUN=function(x)mean(x,na.rm = T))[is.na(market.mean$mean)]
# 
# impute.market <- filter(market.distance,TimeMarket==990)
# impute.market <-inner_join(subset(impute.market, select=c("HouseholdID","MonitoringYear", "SettlementID")),market.mean, by=c("MonitoringYear", "SettlementID"))
# colnames(impute.market) <-c("HouseholdID","MonitoringYear", "SettlementID", "TimeMarket")
# market.distance <-rbind((subset(market.distance, TimeMarket!=990)),impute.market)
# 
# rm(market.mean, impute.market)

# Compile match covariate
match.covariate <- HHData %>% 
  dplyr::select(HouseholdID, MPAID, SettlementID, MonitoringYear, yearsPost, Treatment) %>% 
  left_join(market.distance[,c("HouseholdID","TimeMarket")],by="HouseholdID") %>%
  left_join(N.Child,by="HouseholdID") %>%
  left_join(HH.ed,by="HouseholdID") %>%
  left_join(HH.eth,by="HouseholdID") %>%
  left_join(HH.residency,by="HouseholdID") %>%
  left_join(gender.HHH,by="HouseholdID") %>%
  left_join(HH.age,by="HouseholdID") 

#rm(market.distance,N.Child,HH.ed, HH.eth,HH.residency,gender.HHH, HH.age, market.mean.sett,market.mean.sett.yr,max.eth)

covariate.means <- 
  match.covariate %>%
  group_by(SettlementID, MPAID, MonitoringYear) %>%
  summarise(mean.age=mean(IndividualAge,na.rm=T),
            mean.year.res=mean(YearsResident,na.rm=T),
            mean.ed.level=mean(ed.level,na.rm=T),
            mean.ind.gender=mean(IndividualGender,na.rm=T),
            mean.time.market=mean(TimeMarket,na.rm=T)) %>%
  mutate(mean.time.market=ifelse(MPAID==1 & MonitoringYear=="Baseline",
                                 mean.time.market[MPAID==1 & MonitoringYear=="2 Year Post"],
                                 ifelse(MPAID==2 & MonitoringYear=="Baseline",
                                        mean.time.market[MPAID==2 & MonitoringYear=="2 Year Post"],
                                        mean.time.market)))

match.covariate <-
  left_join(match.covariate,covariate.means,by=c("SettlementID","MPAID","MonitoringYear")) %>%
  transmute(HouseholdID=HouseholdID,
            MPAID=MPAID,
            SettlementID=SettlementID,
            MonitoringYear=MonitoringYear,
            yearsPost=yearsPost,
            Treatment=Treatment,
            TimeMarket=ifelse(is.na(TimeMarket),
                              mean.time.market,
                              as.numeric(TimeMarket)),
            n.child=ifelse(is.na(n.child),
                           0,as.numeric(n.child)),
            ed.level=ifelse(is.na(ed.level) | ed.level>=989,
                            mean.ed.level,
                            as.numeric(ed.level)),
            dom.eth=dom.eth,
            YearsResident=ifelse(is.na(YearsResident),
                                 mean.year.res,
                                 as.numeric(YearsResident)),
            IndividualGender=ifelse(is.na(IndividualGender), 
                                    mean.ind.gender, 
                                    IndividualGender),
            IndividualAge=ifelse(is.na(IndividualAge),
                                 mean.age,
                                 as.numeric(IndividualAge)))
