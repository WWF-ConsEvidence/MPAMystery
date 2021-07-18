##----------------------------------------------------------------------------------------------------------------------------------##
##---MPA Social Impacts - BHS---##
##---Jan 2020---##
##---Duong Le---##
##---Research: MPA Social Impacts in BHS; Big Five impact indicators; aggregate and heterogeneous by time, MPA, and subgroup--------##


##--------------------------------------Analysis Steps------------------------------------------------------------------------------##
##---0. Sourcing and creating data frame [line 20]
##---1. [DiD + settlement baseline matching]---Perform matching & Construct new dataframe & produce before/after matching stats/plots [line 240]
##***Important: the 1to3_w dataframe is "DiD.data.SettleMatch"; the coarse matching dataframe is "DiD.data.coarseMatch"

##---2. DiD Model for aggregate impacts (Seasape-level) [line 490] ---> DiD.data.SettleMatch result
##---3. DiD Model for heterogeneous impact over time (t2 vs t4) [line 650] ---> DiD.data.SettleMatch result
##---4. DiD Model for heterogeneous impact across MPA (6 MPAs) [line 790] ---> DiD.data.SettleMatch result
##---5. DiD Model for heterogeneous impact across subgroups (4 subgroups) [line 990] ---> DiD.data.SettleMatch result
##----------------------------------------------------------------------------------------------------------------------------------##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---0. Sourcing and creating data frame 

source('2_Functions/2_Analysis/Function_process_covariates.R')

mpa.nam <- MPA.LKP %>% rename(MPAID = mpaid, MPAName = name)


pacman::p_load(lfe, cowplot, stargazer, broom, qvalue, psych, factoextra, ineq, sf, tidyverse)


resultPath <- "x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs"

# NOTE: change to same lat/long used in settlement to reef matching
soc.coord <- import('x_Flat_data_files/1_Social/Inputs/soc.coord.province.csv')

soc.coord <- soc.coord %>% mutate(lat = latitude, long = longitude) %>% filter(!is.na(lat)) %>% dplyr::select(-latitude, -longitude, -Treatment)

# --- DiD dataframe 
DiD.data <- match.covariate %>% 
  filter(!is.na(HouseholdID)) %>% 
  left_join(dplyr::select(HHData,DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, 
                   MAIndex:FSIndex, SERate, HouseholdID, InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam, by="MPAID") %>% 
  dplyr::select(HouseholdID:InterviewYear, MPAName) %>% 
  mutate(TreatFactor = as.factor(ifelse(Treatment==0, 0, MPAID)),
         Post = as.factor(ifelse(yearsPost==0,0 , 1)),
         Control = ifelse(Treatment==1, 0, 1), 
         
         yearsPostF = as.factor(yearsPost),
         MPAID = as.factor(MPAID),
         InterviewYear = as.factor(InterviewYear), 
         Fisher = ifelse(PrimaryLivelihood==3,1,0), 
         Male = IndividualGender, 
         PrimaryLivelihood.bin = as.factor(ifelse(PrimaryLivelihood==1,1,
                                                  ifelse(PrimaryLivelihood==2,2,
                                                         ifelse(PrimaryLivelihood==3|PrimaryLivelihood==4,3,
                                                                ifelse(PrimaryLivelihood==6|PrimaryLivelihood==7,4,5)))))) %>% 
  filter(!is.na(IndividualGender)) %>% 
  left_join(soc.coord, by="SettlementID")


summary(DiD.data)
# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other

# --- Filter to look at 6 BHS and 4 SBS MPAs (with t2 and/or t4 data) --- Jan 2020
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6, 15:18))

# --- two additional outcome variables --- Feb 2020
DiD.data <- DiD.data %>% 
  mutate(MTIndex_AccHarv = RightsAccess + RightsHarvest, 
         MTIndex_ManExcTrans= RightsManage + RightsExclude + RightsTransfer, 
         SocialConflict_increase = ifelse(is.na(SocialConflict), NA, 
                                          ifelse(SocialConflict==1|SocialConflict==2, 1, 0)))


# Prepare ethnic polarization index 
HH.eth <- HH.eth %>% 
  left_join(dplyr::select(DiD.data,HouseholdID, SettlementID, MPAID, yearsPost),by="HouseholdID")

eth.polarize <- HH.eth %>% 
  filter(yearsPost==0 & !is.na(HouseholdID)) %>% 
  group_by(SettlementID) %>% 
  mutate(num.HH.Settl=n()) %>% 
  group_by(SettlementID, eth.iso) %>% 
  summarize(num.HH.eth=n(), 
            num.HH.Settl=first(num.HH.Settl)) %>% 
  mutate(eth.pct = num.HH.eth/num.HH.Settl) %>% 
  group_by(SettlementID) %>% 
  summarise(eth.polarize=4*mean(eth.pct*eth.pct*(1-eth.pct)))

DiD.data <- DiD.data %>% 
  left_join(eth.polarize, by="SettlementID")

# Customary Governance Index (%HHs excercise rights to exclude/transfer/manage)  
customary.gov.data <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  dplyr::select(SettlementID, RightsManage, RightsTransfer, RightsExclude) %>% 
  mutate(customary.gov = ifelse(RightsManage%in%c(1) | RightsExclude%in%c(1) | RightsTransfer%in%c(1), 1,0)) %>% 
  group_by(SettlementID) %>% 
  summarise(num.HH.Settl=n(),
            num.HH.customary = sum(customary.gov)) %>% 
  mutate(customary.gov.pct = num.HH.customary/num.HH.Settl)


DiD.data <- DiD.data %>% 
  left_join(dplyr::select(customary.gov.data,customary.gov.pct,SettlementID), by="SettlementID")



# Indicator wealth.above in each seascape-year (factors)
MA.wealth.breaks <- DiD.data %>%
  group_by(yearsPost, MPAID) %>%
  summarise(wealth.median = median(MAIndex, na.rm = T))

DiD.data <- DiD.data %>% 
  left_join(MA.wealth.breaks, by = c("yearsPost","MPAID")) %>% 
  mutate(wealth.above=ifelse(MAIndex<=wealth.median,0,1))


## --- retaining the raw HHH.age (The IndividualAge is currently already categorized)
HH.age.raw <- IndDemos %>%
  filter(RelationHHH==0) %>%
  mutate(IndividualAge_raw = IndividualAge) %>% 
  dplyr::select(HouseholdID,IndividualAge_raw) %>%
  left_join(dplyr::select(DiD.data,HouseholdID,yearsPost),by="HouseholdID") %>%
  dplyr::select(HouseholdID,IndividualAge_raw) %>% 
  distinct(HouseholdID,.keep_all = T)

DiD.data <- DiD.data %>% 
  left_join(HH.age.raw,by="HouseholdID") 

## --- retaining the raw yrsResidence (The current yrResidence is already categorized)
YrResident.raw <- HHData %>%
  dplyr::select(HouseholdID, YrResident)

DiD.data <- DiD.data %>% 
  left_join(YrResident.raw, by="HouseholdID") 

## --- Modifying Asset Items to generate sub-Asset Groups (i.e. Household assets (discretionary & appliances), Productive Marine-based Assets (the boats), and land-based (vehicles)
DiD.data <- DiD.data %>% 
  mutate(Entertain = Entertain,
         PhoneCombined = PhoneCombined/2,
         Satellite = Satellite/3,
         TV = TV/4,
         Generator = Generator/5,
         BoatNoMotor = BoatNoMotor/6,
         BoatOutboard = BoatOutboard/7,
         BoatInboard = BoatInboard/8,
         Bicycle = Bicycle/9,
         Motorcycle = Motorcycle/10,
         CarTruck = CarTruck/11) %>% 
  mutate(Entertain_dum = ifelse(Entertain>0,1,0),
         PhoneCombined_dum = ifelse(PhoneCombined>0,1,0),
         Satellite_dum = ifelse(Satellite>0,1,0),
         TV_dum = ifelse(TV>0,1,0),
         Generator_dum = ifelse(Generator>0,1,0),
         BoatNoMotor_dum = ifelse(BoatNoMotor>0,1,0),
         BoatOutboard_dum = ifelse(BoatOutboard>0,1,0),
         BoatInboard_dum = ifelse(BoatInboard>0,1,0),
         Bicycle_dum = ifelse(Bicycle>0,1,0),
         Motorcycle_dum = ifelse(Motorcycle>0,1,0),
         CarTruck_dum = ifelse(CarTruck>0,1,0)) %>%
  mutate(Household_asset = Entertain + 2*PhoneCombined + 3*Satellite + 4*TV + 5*Generator,
         Boats_w1 = BoatNoMotor + 2*BoatOutboard + 3*BoatInboard,
         Boats_w2 = 6*BoatNoMotor + 7*BoatOutboard + 8*BoatInboard,
         Boats_motor_w1 = 1*BoatOutboard + 2*BoatInboard,
         Boats_motor_w2 = 7*BoatOutboard + 8*BoatInboard,
         Vehicles_w1 = Bicycle + 2*Motorcycle + 3*CarTruck,
         Vehicles_w2 = 9*Bicycle + 10*Motorcycle + 11*CarTruck) %>% 
  mutate(Boats_dum = ifelse(BoatNoMotor>0 | BoatOutboard >0 | BoatInboard >0,1,0),
         Boats_motor_dum = ifelse(BoatOutboard >0 | BoatInboard >0,1,0),
         Vehicles_dum = ifelse(Bicycle>0 | Motorcycle>0 | CarTruck>0,1,0))

# --- Adding Indicators for community Participation (marine and non-marine groups)
MarineGroup.Indicators <- Organization %>%
  group_by(HouseholdID) %>%
  summarise(NumMarineGroup=n(),
            Marine.Lead.count = length(HouseholdID[MarinePosition==2]),
            Marine.Meeting.Active = sum(MarineMeeting[MarineMeeting==1]),
            Marine.Days.Participate = sum(MarineDays[MarineDays<=989])) %>%
  left_join(DiD.data[,c("HouseholdID","MPAID")],.,by="HouseholdID") %>%
  mutate(NumMarineGroup=ifelse(is.na(NumMarineGroup),0,NumMarineGroup)) %>% 
  mutate(Marine.Lead.count=ifelse(is.na(Marine.Lead.count),0,Marine.Lead.count)) %>% 
  mutate(Marine.Days.Participate=ifelse(is.na(Marine.Days.Participate),0,Marine.Days.Participate)) %>% 
  mutate(Marine.Meeting.Active=ifelse(is.na(Marine.Days.Participate),0,
                                      ifelse(Marine.Days.Participate>0,1,0))) 

Non_MarineGroup.Indicators <- NMOrganization %>%
  group_by(HouseholdID) %>%
  summarise(NumOtherGroup=length(HouseholdID),
            OtherGroup.Lead.count = length(HouseholdID[OtherGroupPosition==2]),
            OtherGroup.Meeting.Active = sum(OtherGroupMeeting[OtherGroupMeeting==1]),
            OtherGroup.Days.Participate = sum(OtherGroupDays[OtherGroupDays<=989])) %>%
  left_join(DiD.data[,c("HouseholdID","MPAID")],.,by="HouseholdID") %>%
  mutate(NumOtherGroup=ifelse(is.na(NumOtherGroup),0,NumOtherGroup)) %>% 
  mutate(OtherGroup.Lead.count=ifelse(is.na(OtherGroup.Lead.count),0,OtherGroup.Lead.count)) %>% 
  mutate(OtherGroup.Days.Participate=ifelse(is.na(OtherGroup.Days.Participate),0,OtherGroup.Days.Participate)) %>% 
  mutate(OtherGroup.Meeting.Active=ifelse(is.na(OtherGroup.Meeting.Active),0,
                                          ifelse(OtherGroup.Meeting.Active>0,1,0))) 


DiD.data <-  DiD.data %>% 
  left_join(.,MarineGroup.Indicators[,c("HouseholdID", "NumMarineGroup", "Marine.Lead.count", "Marine.Meeting.Active", "Marine.Days.Participate")],by="HouseholdID") %>% 
  left_join(.,Non_MarineGroup.Indicators[,c("HouseholdID", "NumOtherGroup", "OtherGroup.Lead.count", "OtherGroup.Meeting.Active", "OtherGroup.Days.Participate")],by="HouseholdID") 

# --- summary(DiD.data)
DiD.data.summary <- DiD.data %>% 
  dplyr::select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(n=sum(!is.na(yearsPost)),
            yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  
DiD.data.summary

# calculate Z scores (standardized values for each of the Big Five and the sub_asset groups)
# DiD.data <- DiD.data %>% 
#   group_by(MPAID, MonitoringYear) %>% 
#   mutate_at(vars(MAIndex:SERate, Household_asset:Vehicles_w2, MTIndex_AccHarv, MTIndex_ManExcTrans, SocialConflict, SocialConflict_increase), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
#   ungroup()
# summary(DiD.data)
# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}



##---END---##
##---END---##
##---END---##



##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---1. [DiD + settlement baseline matching]---Perform matching & Construct new dataframe
##-------------------------------------------------------------------------------##

# DiD.data <- DiD.data %>% 
#   mutate(ed.no = ifelse(ed.level==0,1,0),
#          ed.primary = ifelse(ed.level<=1,1,0),
#          ed.high = ifelse(ed.level>=3,1,0),
#          ed.college = ifelse(ed.level>=4,1,0))

##---dataframe for matching settlements
DiD.data.matchingCov <- DiD.data %>%
  filter(yearsPost==0) %>% 
  dplyr::select(MPAID, MonitoringYear, SettlementID, InterviewYear, Treatment, yearsPost, Post, Fisher, TimeMarket, eth.polarize, customary.gov.pct, MTIndex, YrResident, EconStatusTrend, lat, long) %>% 
  group_by(SettlementID) %>%
  summarise(MPAID=first(MPAID),
            Treatment=first(Treatment),
            eth.polarize=first(eth.polarize),
            customary.gov.pct=first(customary.gov.pct),
            lat = first(lat),
            long = first(long),
            Fisher = mean(Fisher,na.rm=T), 
            #dom.MT = mean(dom.MT,na.rm=T), 
            #EconStatusTrend = mean(EconStatusTrend, na.rm=T),
            #dom.eth = mean(dom.eth,na.rm=T),
            TimeMarket = mean(TimeMarket,na.rm=T)) %>% 
  dplyr::select(SettlementID, Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, lat, long, MPAID) %>% 
  mutate(MPAID=as.character(MPAID),
         MPAID=as.integer(MPAID)) %>%
  filter(!is.na(lat)) # !!! QUICK FIX UNTIL LAT/LONG COORDS FOR ALL SOCIAL SETTLEMENTS

DiD.data.matchingCov.final <- DiD.data.matchingCov %>% 
  dplyr::select(TimeMarket, Fisher, eth.polarize, lat, long, MPAID)

## -- Mathcing settlements
Tr<-as.vector(DiD.data.matchingCov$Treatment)  

##-------------------------------------------------------------------------------------------##
### Run match algorithm (COVARIATE MATCHING (Malanobis) with calipers
## Always run with replace=TRUE, and ties=TRUE, vary M (# matches)
# m1<-Match(Y=NULL, Tr,X, M=1, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=TRUE)
# summary(m1)

# Exact matching on MPAID (i.e., control and treatment settlements have to come from the same MPA; that's why the last item is "1")
Xexact<-c(0,0,0,0,0,1)
m_mahanobis<-Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.final, M=3, exact=Xexact, replace=TRUE, ties=T)
summary(m_mahanobis)

# Compute match balance statistics (don't need stats on exact-matched variables)
m_balance_2<-Matching::MatchBalance(Tr~ TimeMarket + Fisher + eth.polarize  + lat + long,
                                    data=DiD.data.matchingCov, match.out=m_mahanobis, ks=TRUE, nboots=1000, digits=3)


# 
# ##-------------------------------------------------------------------------------------------##
# ##---pscore matching
# # calculate p scores
# pscore <- glm(Tr~ TimeMarket + Fisher +  eth.polarize + customary.gov.pct, data=DiD.data.matchingCov)$fitted.values
# psore_frame = cbind(pscore, DiD.data.matchingCov$MPAID)
# Xexact<-c(0,1)
# m_pscore<-Matching::Match(Y=NULL, Tr, X=psore_frame, M=1, exact=Xexact, replace=TRUE, ties=T)
# m_balance_2<-Matching::MatchBalance(Tr~  TimeMarket + Fisher + eth.polarize + customary.gov.pct ,data=DiD.data.matchingCov, match.out=m_pscore, ks=TRUE, nboots=1000, digits=3)
# ##-------------------------------------------------------------------------------------------##


##--Note: After the matching trials, best option is mahanobis, 1-to-3 match. Go with that for now.
##construct DiD.data.SettlMatch dataframe
i.treat<-m_mahanobis$index.treated
i.control<-m_mahanobis$index.control  

##-- mahalanobis distance weighting
DiD.data.matchingCov.mahDis <- DiD.data.matchingCov %>% 
  dplyr::select(TimeMarket, Fisher, eth.polarize, lat, long) 

mal.dist <- mahalanobis(DiD.data.matchingCov.mahDis,cov(DiD.data.matchingCov.mahDis) ,center=F)
head(mal.dist)
pair.treat.settl <-as.data.frame(cbind(DiD.data.matchingCov[i.treat,"SettlementID"],mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist=`mal.dist[i.treat]`,c.mal.dist=`mal.dist[i.control]`) %>%
  mutate(pair.dist=t.mal.dist-c.mal.dist,
         pair.id=1:nrow(.),
         inv.dist=1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt=inv.dist/sum(inv.dist)) %>% 
  dplyr::select(SettlementID,pair.id,t.mal.dist:dist.wt) %>% 
  ungroup()
head(pair.treat.settl)
summary(pair.treat.settl)
# treat.wt <- cbind(DiD.data.matchingCov[i.treat,"SettlementID"],dist.weight)
# pair.treat.settl <- DiD.data.matchingCov %>% 
#   filter(Treatment==1) %>% 
#   mutate(pair.id=SettlementID) %>% 
#   select(SettlementID, pair.id) %>% 
#   left_join(treat.wt,by="SettlementID")
# head(pair.treat.settl)

pair.control.settl <- cbind(DiD.data.matchingCov[i.control,"SettlementID"], pair.treat.settl) 
names(pair.control.settl)[1:2] <- c("ctrl.id", "treat.id")
head(pair.control.settl)  

settl.check <- pair.control.settl %>% 
  left_join(dplyr::select(Settlements,SettlementName,SettlementID), by=c("ctrl.id"="SettlementID")  ) %>%
  rename(ctrl.settlement=SettlementName) %>%
  left_join(dplyr::select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("ctrl.id"="SettlementID") ) %>% 
  rename(ctrl.TimeMarket=TimeMarket, ctrl.Fisher=Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(dplyr::select(Settlements,SettlementName,SettlementID), by=c("treat.id"="SettlementID") ) %>%
  rename(treat.settlement=SettlementName) %>% 
  left_join(dplyr::select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("treat.id"="SettlementID") ) %>% 
  rename(treat.TimeMarket=TimeMarket, treat.Fisher=Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  dplyr::select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket,ctrl.TimeMarket,treat.Fisher,ctrl.Fisher, 
         treat.eth.polarize, ctrl.eth.polarize, pair.dist,TimeMarket.dis:eth.polarize.dis)
head(settl.check)
#export(settl.check,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name_1to3_w_wLatLon_comp.xlsx")

DiD.data.SettlMatch <- data.frame()
DiD.data.SettlMatch <- rbind(pair.treat.settl, 
                             pair.control.settl %>% 
                               dplyr::select(-treat.id) %>% 
                               rename(SettlementID=ctrl.id)) %>% 
  arrange(pair.id,SettlementID) 
head(DiD.data.SettlMatch)

DiD.data.SettlMatch <- DiD.data.SettlMatch %>% left_join(DiD.data, by="SettlementID")
DiD.data.SettlMatch <- as.data.frame(DiD.data.SettlMatch)
# #export(DiD.data.SettlMatch,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/DiD_data_1to3_w_latLon.xlsx")
# #export(DiD.data.SettlMatch,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/DiD_data_1to3_w_latLon.csv")
# #Export to txt file
# library(foreign)
# #install.packages("writexl")
# library(writexl)
# write.table(DiD.data.SettlMatch, "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/DiD_data_1to3_w.txt", sep="\t")
# write_xlsx(DiD.data.SettlMatch, path = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/DiD_data_1to3_w", col_names = TRUE, format_headers = TRUE)



# test <- DiD.data.SettlMatch %>% 
#   filter(pair.id==38) %>% 
#   select(SettlementID, pair.id) 
# 
# unique(test$SettlementID)
# pair.control.settl %>% 
#   filter(pair.id==38)

# ##---Get settlement pair names
# pair_name <- pair.control.settl %>% 
#   left_join(select(SETTLEMENT,SettlementName,SettlementID), by="SettlementID" ) %>% 
#   rename(ctrl.settlement=SettlementName) %>% 
#   left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("pair.id"="SettlementID") ) %>% 
#   rename(treat.settlement=SettlementName) 
#  export(pair_name,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name_oneToOne.xlsx") 
#   
#   head(pair_name)
# #Export to txt file
# library(foreign)
# #install.packages("writexl")
# library(writexl)
# write.table(pair_name, "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name.txt", sep="\t")
# write_xlsx(pair_name, path = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name", col_names = TRUE, format_headers = TRUE)
# 





png(paste0(resultPath,"matching_qqplot_3.png"), bg = "white", width = 1200, height=700)
par(mfrow=c(1,3), cex=0.8)
qqplot(DiD.data.matchingCov$TimeMarket[i.treat],DiD.data.matchingCov$TimeMarket[i.control], xlab="Treat",ylab = "Control", main="Time to Market")
abline(0,1,col="red")
qqplot(DiD.data.matchingCov$Fisher[i.treat],DiD.data.matchingCov$Fisher[i.control], xlab="Treat",ylab = "Control", main="Pct Fisher")
abline(0,1,col="red")
qqplot(DiD.data.matchingCov$eth.polarize[i.treat],DiD.data.matchingCov$eth.polarize[i.control], xlab="Treat",ylab = "Control", main="Ethnic polarization")
abline(0,1,col="red")
dev.off()

##---BEGIN---##
# Export data frame to Excel for exploratory Stata analysis

# #Export to txt file
# library(foreign)
# #install.packages("writexl")
# library(writexl)
# write.table(DiD.data.SettlMatch, "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/matched_DID_BHS_data.txt", sep="\t")
# write_xlsx(DiD.data.SettlMatch, path = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/matched_DID_BHS_data", col_names = TRUE, format_headers = TRUE)


# ##--histogram plots of the matching covariate (baseline -- before matching)
DiD.data.density.before <- DiD.data.matchingCov %>% 
  mutate(Treat=as.factor(Treatment), 
         Group=ifelse(Treatment==1, " MPA", "Control"))

density.TimeMarket <- ggplot(DiD.data.density.before, aes(x = TimeMarket)) + labs(x="Time to Market", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() +
  theme(legend.position=c(0.85,0.85)) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black")) +
  theme(legend.title = element_text(colour="black", size=14, face="bold")) + theme(legend.text = element_text(colour="black", size=11))


density.eth.polarize <- ggplot(DiD.data.density.before, aes(x = eth.polarize)) + labs(x="Ethnic Polarization Index", y="Density") +  
  stat_density(aes(group = Treat, linetype = Group),position="identity", geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

density.Fisher <- ggplot(DiD.data.density.before, aes(x = Fisher)) + labs(x="Fishery Livelihood (%)", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

# density.customary <- ggplot(DiD.data.density.before, aes(x = customary.gov.pct)) + labs(x="Customary Governance (%)", y="Density") +
#   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() + theme(legend.position="none")

plot_grid(density.TimeMarket,density.Fisher,density.eth.polarize,ncol=2)
ggsave(paste0(resultPath,"Settlement_matching/plots/histograms_matchingCovs_before.jpg"),width = 11, height = 9)

# ##--histogram plots of the matching covariate (baseline -- after matching)
DiD.data.density.after <- DiD.data.SettlMatch %>% 
  filter(yearsPost==0) %>% 
  mutate(Treat=as.factor(Treatment), 
         Group=ifelse(Treatment==1, " MPA", "Control"))

density.TimeMarket <- ggplot(DiD.data.density.after, aes(x = TimeMarket)) + labs(x="Time to Market", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() +
  theme(legend.position=c(0.85,0.85)) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black")) +
  theme(legend.title = element_text(colour="black", size=14, face="bold")) + theme(legend.text = element_text(colour="black", size=11))


density.eth.polarize <- ggplot(DiD.data.density.after, aes(x = eth.polarize)) + labs(x="Ethnic Polarization Index", y="Density") +  
  stat_density(aes(group = Treat, linetype = Group),position="identity", geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

density.Fisher <- ggplot(DiD.data.density.after, aes(x = Fisher)) + labs(x="Fishery Livelihood (%)", y="Density") +
  stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")

# density.customary <- ggplot(DiD.data.density.after, aes(x = customary.gov.pct)) + labs(x="Customary Governance (%)", y="Density") +
#   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() + theme(legend.position="none")

plot_grid(density.TimeMarket,density.Fisher,density.eth.polarize,ncol=2)
ggsave(paste0(resultPath,"Settlement_matching/plots/histograms_matchingCovs_after.jpg"),width = 11, height = 9)

##---END---##
##---END---##
##---END---##

##--Important: Now changing "DiD.data" into "DiD.data" dataframe "DiD.data.coarseMatch", and renaming "DiD.data.SettlMatch" "DiD.data" for Sections 6.1 to 6.4
DiD.data.coarseMatch <- DiD.data
DiD.data.coarseMatch <- as.data.frame(DiD.data.coarseMatch)

##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---2. DiD Model to generate aggregate impacts (Seasape-level) 
##---Spec 1-3 are matched DiD (one-to-three matching); Spec 4-6 are coarse DID for [SI]
##-------------------------------------------------------------------------------##
varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
#varNames <- c("FSIndex_z")

#varNames <- c("MTIndex_AccHarv", "MTIndex_ManExcTrans", "SocialConflict", "SocialConflict_increase")
##DiD Regression model (presenting 6 alternative models)
model.out <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data.SettlMatch[,i]
  w <- DiD.data.SettlMatch[,"dist.wt"]
  ## 1. Specification 1 (main spec):  (a) + (b) + (c) + (two-way clustering s.e.)
  regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID  + pair.id + InterviewYear  | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch,exactDOF = TRUE, weights = w)
  summary(regValue)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:Post1", "Post1")) %>% 
    mutate(term=gsub("Treatment:Post1","Impact",term),
           term=gsub("Post1","Control_trend",term),
           Response=i)
  
  ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
  regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                              | SettlementID  + pair.id + InterviewYear  | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch,exactDOF = TRUE, weights = w)
  summary(regValue.treatTrend)
  
  reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
    filter(term%in%c("Post1")) %>% 
    mutate(term=gsub("Post1","Treatment_trend",term),
           Response=i)
  
  model.out <- rbind(model.out, reg.broom, reg.broom.treatTrend)
}


##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
model.out1 <- model.out %>% 
  mutate(domain=ifelse(Response=="FSIndex_z"," Health (Food Security)",
                       ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) #this line break the labels into 2 lines whenever it finds the symbol "(" in the string


##Export 
export(model.out1,  "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/BHS_impact_output_1to3_w_new.csv")

##PLOTS
pd <- position_dodge(width=.3) # move them .05 to the left and right


Big5.plot <- ggplot(filter(model.out1, term=="Impact"), aes(x=domain,y=estimate)) + 
  geom_line( position = pd) + coord_flip() +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=5, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimates", title="MPA Aggregate Impacts across Social Wellbeing Domains")  


ggsave(paste0(resultPath,"Settlement_matching/plots/1-big5-seascape-z-1to3_w.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---3. DiD Model for heterogeneous impact over time (t2 vs t4)
##-------------------------------------------------------------------------------##
varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
#varNames <- c("MTIndex_AccHarv", "MTIndex_ManExcTrans", "SocialConflict", "SocialConflict_increase")

##DiD Regression model
model.out <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data.SettlMatch[,i]
  w <- DiD.data.SettlMatch[,"dist.wt"]
  
  ## DiD model (main spec)
  regValue <- felm(Y  ~  Treatment + yearsPostF + Treatment:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + pair.id | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch,exactDOF = TRUE, weights=w)
  summary(regValue)
  
  reg.broom <- tidy(regValue) %>% 
    filter(term%in%c("Treatment:yearsPostF2","yearsPostF2", "Treatment:yearsPostF4","yearsPostF4", "Treatment:yearsPostF7","yearsPostF7")) %>% 
    mutate(Response=i, 
           term=gsub("Treatment:yearsPostF2","Impact_2",term),
           term=gsub("yearsPostF2","Control_2",term),
           term=gsub("Treatment:yearsPostF4","Impact_4",term),
           term=gsub("yearsPostF4","Control_4",term),
           term=gsub("Treatment:yearsPostF7","Impact_7",term),
           term=gsub("yearsPostF7","Control_7",term),)
  
  ## rerun DiD model (main spec) fto obtain Treatment trend
  regValue.treatTrend <- felm(Y  ~  Control + yearsPostF + Control:yearsPostF + 
                                n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                              | SettlementID + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch,exactDOF = TRUE, weights=w)
  summary(regValue.treatTrend)
  
  reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
    filter(term%in%c("yearsPostF2", "yearsPostF4", "yearsPostF7")) %>% 
    mutate(Response=i, 
           term=gsub("yearsPostF2","Treatment_2",term),
           term=gsub("yearsPostF4","Treatment_4",term),
           term=gsub("yearsPostF7","Treatment_7",term),)
  
  model.out <- rbind(model.out, reg.broom, reg.broom.treatTrend)
}


model.out.time <- model.out %>% 
  mutate(domain=ifelse(Response=="FSIndex_z"," Health (Food Security)",
                       ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) #this line break the labels into 2 lines whenever it finds the symbol "(" in the string

##Export 
export(model.out.time,  "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/BHS_impact_output_time_1to3_w_new.csv")


##PLOTS
pd <- position_dodge(width=.3) # move them .05 to the left and righ

#####################Plots
FS.plot <- ggplot(filter(model.out.time,Response=="FSIndex_z", term%in%c("Impact")), aes(x=time_post,y=estimate, color=term), group=1) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.time,Response=="MTIndex_z",  term%in%c("Impact")),aes(x=time_post,y=estimate, color=term), group=1) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.time,Response=="MAIndex_z",  term%in%c("Impact")),aes(x=time_post,y=estimate, color=term), group=1) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.time,Response=="PAIndex_z",  term%in%c("Impact")),aes(x=time_post,y=estimate, color=term), group=1) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.time,Response=="SERate_z", term%in%c("Impact")),aes(x=time_post,y=estimate, color=term), group=1) + 
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(MA.plot,FS.plot,MT.plot,PA.plot,SE.plot,ncol=3)
ggsave(paste0(resultPath,"/Paper 0-MPA Impact BHS/Settlement_matching/plots/2-big5-seascape-time-z-1to3_w_split.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##



##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---4. DiD Model for heterogeneous impact across Settlements (6 MPAs)
##-------------------------------------------------------------------------------##

###generating short MPA names
mpa.nam <- mpa.nam %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"  Telma",
                                ifelse(MPAID==2,"  TNTC",
                                       ifelse(MPAID==3," Kaimana",
                                              ifelse(MPAID==4," Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==15,"Selat Pantar",
                                                                          ifelse(MPAID==16,"Flores Timur","")))))))))

varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
#varNames <- c("MTIndex_AccHarv", "MTIndex_ManExcTrans", "SocialConflict", "SocialConflict_increase")

##DiD Regressions: Hetegeneous impact by MPA (and time)
model.out.mpalevel <- data.frame()

for (i in varNames) {
  for (mpaid in 1:6) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data.SettlMatch %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    w <- DiD.data.mpalevel[,"dist.wt"]
    
    regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + pair.id | 0 | SettlementID + pair.id, data=DiD.data.mpalevel,exactDOF = TRUE, weights=w)
    
    reg.broom <- tidy(regValue) %>% 
      filter(term%in%c("Treatment:Post1", "Post1")) %>% 
      mutate(term=gsub("Treatment:Post1","Impact",term),
             term=gsub("Post1","Control_trend",term),
             Response=i, MPAID=mpaid)
    
    ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
    regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
                                  n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                                | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.mpalevel,exactDOF = TRUE, weights = w)
    summary(regValue.treatTrend)
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      filter(term%in%c("Post1")) %>% 
      mutate(term=gsub("Post1","Treatment_trend",term),
             Response=i, MPAID=mpaid) 
    
    model.out.mpalevel <- rbind(model.out.mpalevel, reg.broom, reg.broom.treatTrend)
  }
}


##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
model.out.mpalevel1 <- model.out.mpalevel %>% 
  mutate(domain=ifelse(Response=="FSIndex_z"," Health (Food Security)",
                       ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) 

##Export 
export(model.out.mpalevel1,  "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/BHS_impact_output_MPA_1to3_w_new.csv")


##-------------------------------------------------------------------##
##Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel1, term=="Impact", Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))
#ggsave(paste0(resultPath,"Settlement_matching/plots/3-FS-mpa-z.jpg"),width = 12, height = 6)


MT.plot_z <- ggplot(filter(model.out.mpalevel1,term=="Impact",Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel1,term=="Impact",Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel1,term=="Impact",Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel1,term=="Impact",Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.0, size=2, color="black", position = pd ) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0.0, size=0.5, color="black", position = pd ) +
  geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))


##combine PLOTS
plot_grid(MA.plot_z,FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,ncol=3)
ggsave(paste0(resultPath,"Settlement_matching/plots/3-big5-mpa-z-1to3-w.jpg"),width = 12, height = 6)

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##


# ##-------------------------------------------------------------------------------##
# ##---5. DiD Model for heterogeneous impact across subgroups (5 subgroups)
# ##-------------------------------------------------------------------------------##
# model.out.subgroup <- data.frame()
# varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
# for (i in varNames) {
#   
#   ##----------------Gender------------------------------##
#   for (genderID in 0:1) {
#     DiD.data.gender <- DiD.data.SettlMatch %>%  filter(Male==genderID)
#     Y <- DiD.data.gender[,i]
#     w <- DiD.data.gender[,"dist.wt"]
#     
#     
#     regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
#                        n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                      | SettlementID + pair.id | 0 | SettlementID + pair.id,  data=DiD.data.gender,exactDOF = TRUE, weights=w)
#     
#     
#     reg.broom <- tidy(regValue) %>% 
#       filter(term%in%c("Treatment:Post1", "Post1")) %>% 
#       mutate(term=gsub("Treatment:Post1","Impact",term),
#              term=gsub("Post1","Control_trend",term),
#              Response=i, subgroup="Gender", subgroup_id=genderID)
#     
#     ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
#     regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
#                                   n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                                 | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.gender,exactDOF = TRUE, weights = w)
#     summary(regValue.treatTrend)
#     
#     reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
#       filter(term%in%c("Post1")) %>% 
#       mutate(term=gsub("Post1","Treatment_trend",term),
#              Response=i, subgroup="Gender", subgroup_id=genderID)
#     
#     model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
#   }
#   
#   ##----------------Ethnicity (dominant/non-dominant)------------------------------##
#   for (dom.ethID in 0:1) {
#     DiD.data.dom.eth <- DiD.data.SettlMatch %>% filter(dom.eth==dom.ethID)
#     Y <- DiD.data.dom.eth[,i]
#     w <- DiD.data.dom.eth[,"dist.wt"]
#     
#     regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
#                        n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                      | SettlementID + pair.id | 0 | SettlementID + pair.id, data=DiD.data.dom.eth,exactDOF = TRUE, weights=w)
#     
#     reg.broom <- tidy(regValue) %>% 
#       filter(term%in%c("Treatment:Post1", "Post1")) %>% 
#       mutate(term=gsub("Treatment:Post1","Impact",term),
#              term=gsub("Post1","Control_trend",term),
#              Response=i, subgroup="Ethnicity", subgroup_id=dom.ethID)
#     
#     ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
#     regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
#                                   n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                                 | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.dom.eth,exactDOF = TRUE, weights = w)
#     summary(regValue.treatTrend)
#     
#     reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
#       filter(term%in%c("Post1")) %>% 
#       mutate(term=gsub("Post1","Treatment_trend",term),
#              Response=i, subgroup="Ethnicity", subgroup_id=dom.ethID)
#     
#     model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
#   } 
#   
#   ##----------------Economic Wealth (below/above median)------------------------------##
#   for (wealthID in 0:1) {
#     DiD.data.wealth <- DiD.data.SettlMatch %>% filter(wealth.above==wealthID)
#     Y <- DiD.data.wealth[,i]
#     w <- DiD.data.wealth[,"dist.wt"]
#     
#     regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
#                        n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                      | SettlementID + pair.id | 0 | SettlementID + pair.id, data=DiD.data.wealth,exactDOF = TRUE, weights=w)
#     
#     reg.broom <- tidy(regValue) %>% 
#       filter(term%in%c("Treatment:Post1", "Post1")) %>% 
#       mutate(term=gsub("Treatment:Post1","Impact",term),
#              term=gsub("Post1","Control_trend",term),
#              Response=i, subgroup="Economic Wealth", subgroup_id=wealthID)
#     
#     ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
#     regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
#                                   n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                                 | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.wealth,exactDOF = TRUE, weights = w)
#     summary(regValue.treatTrend)
#     
#     reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
#       filter(term%in%c("Post1")) %>% 
#       mutate(term=gsub("Post1","Treatment_trend",term),
#              Response=i, subgroup="Economic Wealth", subgroup_id=wealthID)
#     
#     model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
#   }
#   
#   
#   
#   ##----------------Occupation (fisher/non-fisher)------------------------------##
#   
#   ##---DiD Regressions: by Occupation (non-fisher only)
#   DiD.data.fisher0 <- DiD.data.SettlMatch %>% filter(Fisher==0)
#   Y <- DiD.data.fisher0[,i]
#   w <- DiD.data.fisher0[,"dist.wt"]
#   
#   regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
#                      n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                    | SettlementID + pair.id | 0 | SettlementID + pair.id, data=DiD.data.fisher0,exactDOF = TRUE, weights=w)
#   
#   reg.broom <- tidy(regValue) %>% 
#     filter(term%in%c("Treatment:Post1", "Post1")) %>% 
#     mutate(term=gsub("Treatment:Post1","Impact",term),
#            term=gsub("Post1","Control_trend",term),
#            Response=i, subgroup="Fishing Livelihood", subgroup_id=0)
#   
#   ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
#   regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
#                                 n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
#                               | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.fisher0,exactDOF = TRUE, weights = w)
#   summary(regValue.treatTrend)
#   
#   reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
#     filter(term%in%c("Post1")) %>% 
#     mutate(term=gsub("Post1","Treatment_trend",term),
#            Response=i, subgroup="Fishing Livelihood", subgroup_id=0)
#   
#   model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
#   
#   
#   ##---DiD Regressions: by Occupation (fisher only)
#   DiD.data.fisher1 <- DiD.data.SettlMatch %>% filter(Fisher==1)
#   Y <- DiD.data.fisher1[,i]
#   w <- DiD.data.fisher1[,"dist.wt"]
#   
#   regValue <- felm(Y  ~   Treatment + Post + Treatment:Post +
#                      n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
#                    | SettlementID + pair.id | 0 | SettlementID + pair.id, data=DiD.data.fisher1,exactDOF = TRUE, weights=w)
#   
#   reg.broom <- tidy(regValue) %>% 
#     filter(term%in%c("Treatment:Post1", "Post1")) %>% 
#     mutate(term=gsub("Treatment:Post1","Impact",term),
#            term=gsub("Post1","Control_trend",term),
#            Response=i, subgroup="Fishing Livelihood", subgroup_id=1)
#   
#   ## Rerun with Control (instead of Treatment) and Post to get "Treatment trend" estimates
#   regValue.treatTrend <- felm(Y  ~  Control + Post + Control:Post + 
#                                 n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge 
#                               | SettlementID  + pair.id  | 0 | SettlementID + pair.id, data=DiD.data.fisher1,exactDOF = TRUE, weights = w)
#   summary(regValue.treatTrend)
#   
#   reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
#     filter(term%in%c("Post1")) %>% 
#     mutate(term=gsub("Post1","Treatment_trend",term),
#            Response=i, subgroup="Fishing Livelihood", subgroup_id=1)
#   
#   model.out.subgroup <- rbind(model.out.subgroup, reg.broom, reg.broom.treatTrend)
# }
# 
# ##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
# model.out.subgroup1 <- model.out.subgroup %>%
#   mutate(domain=ifelse(Response=="FSIndex_z"," Health (Food Security)",
#                        ifelse(Response=="MAIndex_z","Economic Wellbeing (Material Assets)",
#                               ifelse(Response=="MTIndex_z"," Empowerment (Marine Tenure)",
#                                      ifelse(Response=="PAIndex_z"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
#          domain=gsub(" \\(", "\n \\(", domain)) 
# 
# ##Export 
# export(model.out.subgroup1,  "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/BHS_impact_output_subGroup_1to3_w_new.csv")
# 
# 
# 
# 
# ##----------------------------------------------------------------##
# ##PLOT: "regular" index
# pd <- position_dodge(width=.5) # move them .05 to the left and right
# 
# 
# FS.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",Response=="FSIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Food Security")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# MT.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",Response=="MTIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Marine Tenure")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# PA.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",Response=="PAIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Place Attachment")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# 
# MA.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",Response=="MAIndex_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="Material Assets")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# SE.plot_z <- ggplot(filter(model.out.subgroup1,term=="Impact",Response=="SERate_z"),aes(x=subgroup, y=estimate, color=as.factor(subgroup_id)),group=2) +
#   geom_line( position = pd) +
#   theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error, color=as.factor(subgroup_id)), width=0.0, size=1, position = pd ) +
#   geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=as.factor(subgroup_id)), width=0.0, size=0, position = pd ) +
#   geom_point(stat="identity", position =pd, fill='white', size=3, shape=21)+ theme_bw() + theme(legend.position="none") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(x="",y="", title="School Enrollment")  +
#   scale_colour_manual(values = c("red", "blue"))
# 
# ##combine PLOTS
# plot_grid(MA.plot_z,FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,ncol=3)
# ggsave(paste0(resultPath,"Settlement_matching/plots/4-big5-subgroup-z-1to3-w.jpg"),width = 12, height = 6)
# 
# ##---END---##
# ##---END---##
# ##---END---##
# 
# 
