##----------------------------------------------------------------------------------------------------------------------------------##
##---MPA Social Impacts - BHS---##
##---Jan 2020---##
##---Duong Le---##
##---Research: MPA Social Impacts in BHS; Big Five impact indicators; aggregate and heterogeneous by time, MPA, and subgroup--------##


##--------------------------------------Analysis Steps------------------------------------------------------------------------------##
##---0. Sourcing and creating data frame [line 20]
##---1. Sensitivity Check: DiD aggregate impact (stargazer tables)
##----------------------------------------------------------------------------------------------------------------------------------##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---0. Sourcing and creating data frame 

setwd("D:/Dropbox/MPA_research/MPAMystery/")
source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
pacman::p_load(lfe,cowplot,broom,qvalue,psych,factoextra,ineq, stargazer, tidyverse)

resultPath <- "D:/Dropbox/MPA_research/"
soc.coord<- import("x_Flat_data_files/1_Social/Inputs/soc.coord.xlsx")


# --- DiD dataframe 
DiD.data <- match.covariate %>% 
  filter(!is.na(HouseholdID)) %>% 
  left_join(select(HHData,DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, MAIndex:FSIndex ,SERate ,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         Post = as.factor(ifelse(yearsPost==0,0,1)), 
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear), 
         Fisher=ifelse(PrimaryLivelihood==3,1,0), 
         Male = IndividualGender, 
         PrimaryLivelihood.bin = as.factor(ifelse(PrimaryLivelihood==1,1,
                                                  ifelse(PrimaryLivelihood==2,2,
                                                         ifelse(PrimaryLivelihood==3|PrimaryLivelihood==4,3,
                                                                ifelse(PrimaryLivelihood==6|PrimaryLivelihood==7,4,5)))))) %>% 
  filter(!is.na(IndividualGender)) %>% 
  left_join(soc.coord,by="SettlementID")


summary(DiD.data)
# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other

# --- Filter to look at 6 BHS and 4 SBS MPAs (with t2 and/or t4 data) --- Jan 2020
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6))


# Prepare ethnic polarization index 
HH.eth <- HH.eth %>% 
  left_join(select(DiD.data,HouseholdID, SettlementID, MPAID, yearsPost),by="HouseholdID")

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
  select(SettlementID, RightsManage, RightsTransfer, RightsExclude) %>% 
  mutate(customary.gov = ifelse(RightsManage%in%c(1) | RightsExclude%in%c(1) | RightsTransfer%in%c(1), 1,0)) %>% 
  group_by(SettlementID) %>% 
  summarise(num.HH.Settl=n(),
            num.HH.customary = sum(customary.gov)) %>% 
  mutate(customary.gov.pct = num.HH.customary/num.HH.Settl)


DiD.data <- DiD.data %>% 
  left_join(select(customary.gov.data,customary.gov.pct,SettlementID), by="SettlementID")



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
  select(HouseholdID,IndividualAge_raw) %>%
  left_join(select(DiD.data,HouseholdID,yearsPost),by="HouseholdID") %>%
  select(HouseholdID,IndividualAge_raw) %>% 
  distinct(HouseholdID,.keep_all = T)

DiD.data <- DiD.data %>% 
  left_join(HH.age.raw,by="HouseholdID") 

## --- retaining the raw yrsResidence (The current yrResidence is already categorized)
YrResident.raw <- HHData %>%
  select(HouseholdID, YrResident)

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
  select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  


# calculate Z scores (standardized values for each of the Big Five and the sub_asset groups)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, Household_asset:Vehicles_w2), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()
summary(DiD.data)
# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}



##---END---##
##---END---##
##---END---##



##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---1. Sensitivity Check: DiD aggregate impact (stargazer tables)
##-------------------------------------------------------------------------------##

# DiD.data <- DiD.data %>% 
#   mutate(ed.no = ifelse(ed.level==0,1,0),
#          ed.primary = ifelse(ed.level<=1,1,0),
#          ed.high = ifelse(ed.level>=3,1,0),
#          ed.college = ifelse(ed.level>=4,1,0))

##---dataframe for matching settlements
DiD.data.matchingCov <- DiD.data %>%
  filter(yearsPost==0) %>% 
  select(MPAID, MonitoringYear, SettlementID, InterviewYear, Treatment, yearsPost, Post, Fisher, TimeMarket, eth.polarize, customary.gov.pct, MTIndex, YrResident, EconStatusTrend, lat, long) %>% 
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
  select(SettlementID,Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, lat, long, MPAID) %>% 
  mutate(MPAID=as.integer(MPAID))

DiD.data.matchingCov.final <- DiD.data.matchingCov %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long, MPAID) 

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
  select(TimeMarket, Fisher, eth.polarize, lat, long) 

mal.dist <- mahalanobis(DiD.data.matchingCov.mahDis,cov(DiD.data.matchingCov.mahDis) ,center=F)
head(mal.dist)
pair.treat.settl <-as.data.frame(cbind(DiD.data.matchingCov[i.treat,"SettlementID"],mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist=`mal.dist[i.treat]`,c.mal.dist=`mal.dist[i.control]`) %>%
  mutate(pair.dist=t.mal.dist-c.mal.dist,
         pair.id=1:nrow(.),
         inv.dist=1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt=inv.dist/sum(inv.dist)) %>% 
  select(SettlementID,pair.id,t.mal.dist:dist.wt) %>% 
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
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("ctrl.id"="SettlementID")  ) %>%
  rename(ctrl.settlement=SettlementName) %>%
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("ctrl.id"="SettlementID") ) %>% 
  rename(ctrl.TimeMarket=TimeMarket, ctrl.Fisher=Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("treat.id"="SettlementID") ) %>%
  rename(treat.settlement=SettlementName) %>% 
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("treat.id"="SettlementID") ) %>% 
  rename(treat.TimeMarket=TimeMarket, treat.Fisher=Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket,ctrl.TimeMarket,treat.Fisher,ctrl.Fisher, 
         treat.eth.polarize, ctrl.eth.polarize, pair.dist,TimeMarket.dis:eth.polarize.dis)
head(settl.check)
#export(settl.check,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name_1to3_w_wLatLon_comp.xlsx")

DiD.data.SettlMatch <- data.frame()
DiD.data.SettlMatch <- rbind(pair.treat.settl, 
                             pair.control.settl %>% 
                               select(-treat.id) %>% 
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

# ##--QQ (Quantile-Quantile) plots to check matching quality
# png(paste0(resultPath,"matching_qqplot_3.png"), bg = "white", width = 1200, height=700)
# par(mfrow=c(1,3), cex=0.8)
# qqplot(DiD.data.matchingCov$TimeMarket[i.treat],DiD.data.matchingCov$TimeMarket[i.control], xlab="Treat",ylab = "Control", main="Time to Market")
# abline(0,1,col="red")
# qqplot(DiD.data.matchingCov$Fisher[i.treat],DiD.data.matchingCov$Fisher[i.control], xlab="Treat",ylab = "Control", main="Pct Fisher")
# abline(0,1,col="red")
# qqplot(DiD.data.matchingCov$eth.polarize[i.treat],DiD.data.matchingCov$eth.polarize[i.control], xlab="Treat",ylab = "Control", main="Ethnic polarization")
# abline(0,1,col="red")
# dev.off()
# 
# 
# # ##--histogram plots of the matching covariate (baseline -- before matching)
# DiD.data.density.before <- DiD.data.matchingCov %>% 
#   mutate(Treat=as.factor(Treatment), 
#          Group=ifelse(Treatment==1, " MPA", "Control"))
# 
# density.TimeMarket <- ggplot(DiD.data.density.before, aes(x = TimeMarket)) + labs(x="Time to Market", y="Density") +
#   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() +
#   theme(legend.position=c(0.85,0.85)) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black")) +
#   theme(legend.title = element_text(colour="black", size=14, face="bold")) + theme(legend.text = element_text(colour="black", size=11))
# 
# 
# density.eth.polarize <- ggplot(DiD.data.density.before, aes(x = eth.polarize)) + labs(x="Ethnic Polarization Index", y="Density") +  
#   stat_density(aes(group = Treat, linetype = Group),position="identity", geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")
# 
# density.Fisher <- ggplot(DiD.data.density.before, aes(x = Fisher)) + labs(x="Fishery Livelihood (%)", y="Density") +
#   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")
# 
# # density.customary <- ggplot(DiD.data.density.before, aes(x = customary.gov.pct)) + labs(x="Customary Governance (%)", y="Density") +
# #   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() + theme(legend.position="none")
# 
# plot_grid(density.TimeMarket,density.Fisher,density.eth.polarize,ncol=2)
# ggsave(paste0(resultPath,"Settlement_matching/plots/histograms_matchingCovs_before.jpg"),width = 11, height = 9)
# 
# # ##--histogram plots of the matching covariate (baseline -- after matching)
# DiD.data.density.after <- DiD.data.SettlMatch %>% 
#   filter(yearsPost==0) %>% 
#   mutate(Treat=as.factor(Treatment), 
#          Group=ifelse(Treatment==1, " MPA", "Control"))
# 
# density.TimeMarket <- ggplot(DiD.data.density.after, aes(x = TimeMarket)) + labs(x="Time to Market", y="Density") +
#   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() +
#   theme(legend.position=c(0.85,0.85)) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid",colour ="black")) +
#   theme(legend.title = element_text(colour="black", size=14, face="bold")) + theme(legend.text = element_text(colour="black", size=11))
# 
# 
# density.eth.polarize <- ggplot(DiD.data.density.after, aes(x = eth.polarize)) + labs(x="Ethnic Polarization Index", y="Density") +  
#   stat_density(aes(group = Treat, linetype = Group),position="identity", geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")
# 
# density.Fisher <- ggplot(DiD.data.density.after, aes(x = Fisher)) + labs(x="Fishery Livelihood (%)", y="Density") +
#   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.2, color="black") + theme_classic() + theme(legend.position="none")
# 
# # density.customary <- ggplot(DiD.data.density.after, aes(x = customary.gov.pct)) + labs(x="Customary Governance (%)", y="Density") +
# #   stat_density(aes(group = Treat, linetype = Group),position="identity",geom="line", size=1.3, color="black") + theme_classic() + theme(legend.position="none")
# 
# plot_grid(density.TimeMarket,density.Fisher,density.eth.polarize,ncol=2)
# ggsave(paste0(resultPath,"Settlement_matching/plots/histograms_matchingCovs_after.jpg"),width = 11, height = 9)

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##-------------------------------------------------------------------------------##
##---2. Sensitivity Check: DiD Model for Aggregate Impacts
##-------------------------------------------------------------------------------##

##---2.1 Generate data frame for 1to1 and 1to2 matching 

##-------------------------------------------1to1 matching (dataframe: "DiD.SettlMatch_1)
Xexact<-c(0,0,0,0,0,1)
m_mahanobis_1<-Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.final, M=1, exact=Xexact, replace=TRUE, ties=T)
summary(m_mahanobis_1)

##construct DiD.data.SettlMatch_1 dataframe
i.treat<-m_mahanobis_1$index.treated
i.control<-m_mahanobis_1$index.control  

##-- mahalanobis distance weighting
DiD.data.matchingCov.mahDis <- DiD.data.matchingCov %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long) 

mal.dist <- mahalanobis(DiD.data.matchingCov.mahDis,cov(DiD.data.matchingCov.mahDis) ,center=F)
head(mal.dist)
pair.treat.settl <-as.data.frame(cbind(DiD.data.matchingCov[i.treat,"SettlementID"],mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist=`mal.dist[i.treat]`,c.mal.dist=`mal.dist[i.control]`) %>%
  mutate(pair.dist=t.mal.dist-c.mal.dist,
         pair.id=1:nrow(.),
         inv.dist=1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt=inv.dist/sum(inv.dist)) %>% 
  select(SettlementID,pair.id,t.mal.dist:dist.wt) %>% 
  ungroup()
head(pair.treat.settl)
summary(pair.treat.settl)

pair.control.settl <- cbind(DiD.data.matchingCov[i.control,"SettlementID"], pair.treat.settl) 
names(pair.control.settl)[1:2] <- c("ctrl.id", "treat.id")
head(pair.control.settl)  

settl.check <- pair.control.settl %>% 
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("ctrl.id"="SettlementID")  ) %>%
  rename(ctrl.settlement=SettlementName) %>%
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("ctrl.id"="SettlementID") ) %>% 
  rename(ctrl.TimeMarket=TimeMarket, ctrl.Fisher=Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("treat.id"="SettlementID") ) %>%
  rename(treat.settlement=SettlementName) %>% 
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("treat.id"="SettlementID") ) %>% 
  rename(treat.TimeMarket=TimeMarket, treat.Fisher=Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket,ctrl.TimeMarket,treat.Fisher,ctrl.Fisher, 
         treat.eth.polarize, ctrl.eth.polarize, pair.dist,TimeMarket.dis:eth.polarize.dis)
head(settl.check)

DiD.data.SettlMatch_1 <- data.frame()
DiD.data.SettlMatch_1 <- rbind(pair.treat.settl, 
                             pair.control.settl %>% 
                               select(-treat.id) %>% 
                               rename(SettlementID=ctrl.id)) %>% 
  arrange(pair.id,SettlementID) 
head(DiD.data.SettlMatch_1)

DiD.data.SettlMatch_1 <- DiD.data.SettlMatch_1 %>% left_join(DiD.data, by="SettlementID")
DiD.data.SettlMatch_1 <- as.data.frame(DiD.data.SettlMatch_1)


##-------------------------------------------1to2 matching (dataframe: "DiD.SettlMatch_2)
Xexact<-c(0,0,0,0,0,1)
m_mahanobis_2<-Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.final, M=2, exact=Xexact, replace=TRUE, ties=T)
summary(m_mahanobis_2)

##construct DiD.data.SettlMatch_1 dataframe
i.treat<-m_mahanobis_2$index.treated
i.control<-m_mahanobis_2$index.control  

##-- mahalanobis distance weighting
DiD.data.matchingCov.mahDis <- DiD.data.matchingCov %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long) 

mal.dist <- mahalanobis(DiD.data.matchingCov.mahDis,cov(DiD.data.matchingCov.mahDis) ,center=F)
head(mal.dist)
pair.treat.settl <-as.data.frame(cbind(DiD.data.matchingCov[i.treat,"SettlementID"],mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist=`mal.dist[i.treat]`,c.mal.dist=`mal.dist[i.control]`) %>%
  mutate(pair.dist=t.mal.dist-c.mal.dist,
         pair.id=1:nrow(.),
         inv.dist=1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt=inv.dist/sum(inv.dist)) %>% 
  select(SettlementID,pair.id,t.mal.dist:dist.wt) %>% 
  ungroup()
head(pair.treat.settl)
summary(pair.treat.settl)

pair.control.settl <- cbind(DiD.data.matchingCov[i.control,"SettlementID"], pair.treat.settl) 
names(pair.control.settl)[1:2] <- c("ctrl.id", "treat.id")
head(pair.control.settl)  

settl.check <- pair.control.settl %>% 
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("ctrl.id"="SettlementID")  ) %>%
  rename(ctrl.settlement=SettlementName) %>%
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("ctrl.id"="SettlementID") ) %>% 
  rename(ctrl.TimeMarket=TimeMarket, ctrl.Fisher=Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("treat.id"="SettlementID") ) %>%
  rename(treat.settlement=SettlementName) %>% 
  left_join(select(DiD.data.matchingCov,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("treat.id"="SettlementID") ) %>% 
  rename(treat.TimeMarket=TimeMarket, treat.Fisher=Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket,ctrl.TimeMarket,treat.Fisher,ctrl.Fisher, 
         treat.eth.polarize, ctrl.eth.polarize, pair.dist,TimeMarket.dis:eth.polarize.dis)
head(settl.check)

DiD.data.SettlMatch_2 <- data.frame()
DiD.data.SettlMatch_2 <- rbind(pair.treat.settl, 
                               pair.control.settl %>% 
                                 select(-treat.id) %>% 
                                 rename(SettlementID=ctrl.id)) %>% 
  arrange(pair.id,SettlementID) 
head(DiD.data.SettlMatch_2)

DiD.data.SettlMatch_2 <- DiD.data.SettlMatch_2 %>% left_join(DiD.data, by="SettlementID")
DiD.data.SettlMatch_2 <- as.data.frame(DiD.data.SettlMatch_2)



##---create data frame droping 3 troublesome control units (keep 1-to-3 matching)---##

##---dataframe for matching settlements
DiD.data.matchingCov.dropC <- DiD.data %>%
  filter(yearsPost==0) %>% 
  select(MPAID, MonitoringYear, SettlementID, InterviewYear, Treatment, yearsPost, Post, Fisher, TimeMarket, eth.polarize, customary.gov.pct, MTIndex, YrResident, EconStatusTrend, lat, long) %>% 
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
  select(SettlementID,Treatment, TimeMarket, Fisher, eth.polarize, customary.gov.pct, lat, long, MPAID) %>% 
  mutate(MPAID=as.integer(MPAID))

##---removing 3 troublesome control units
DiD.data.matchingCov.dropC <- DiD.data.matchingCov.dropC %>% 
  filter(SettlementID != 59 & SettlementID != 61 & SettlementID != 68)


DiD.data.matchingCov.dropC.final <- DiD.data.matchingCov.dropC %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long, MPAID) 

## -- Mathcing settlements
Tr<-as.vector(DiD.data.matchingCov.dropC$Treatment)  

##-------------------------------------------------------------------------------------------##
# Exact matching on MPAID (i.e., control and treatment settlements have to come from the same MPA; that's why the last item is "1")
Xexact<-c(0,0,0,0,0,1)
m_mahanobis<-Matching::Match(Y=NULL, Tr, X=DiD.data.matchingCov.dropC.final, M=3, exact=Xexact, replace=TRUE, ties=T)
summary(m_mahanobis)

# Compute match balance statistics (don't need stats on exact-matched variables)
m_balance_2<-Matching::MatchBalance(Tr~ TimeMarket + Fisher + eth.polarize  + lat + long,
                                    data=DiD.data.matchingCov.dropC, match.out=m_mahanobis, ks=TRUE, nboots=1000, digits=3)


##--Note: After the matching trials, best option is mahanobis, 1-to-3 match. Go with that for now.
##construct DiD.data.SettlMatch dataframe
i.treat<-m_mahanobis$index.treated
i.control<-m_mahanobis$index.control  

##-- mahalanobis distance weighting
DiD.data.matchingCov.dropC.mahDis <- DiD.data.matchingCov.dropC %>% 
  select(TimeMarket, Fisher, eth.polarize, lat, long) 

mal.dist <- mahalanobis(DiD.data.matchingCov.dropC.mahDis,cov(DiD.data.matchingCov.dropC.mahDis) ,center=F)
head(mal.dist)
pair.treat.settl <-as.data.frame(cbind(DiD.data.matchingCov.dropC[i.treat,"SettlementID"],mal.dist[i.treat],mal.dist[i.control]))  %>%
  rename(t.mal.dist=`mal.dist[i.treat]`,c.mal.dist=`mal.dist[i.control]`) %>%
  mutate(pair.dist=t.mal.dist-c.mal.dist,
         pair.id=1:nrow(.),
         inv.dist=1/abs(pair.dist)) %>%
  group_by(SettlementID) %>% 
  mutate(dist.wt=inv.dist/sum(inv.dist)) %>% 
  select(SettlementID,pair.id,t.mal.dist:dist.wt) %>% 
  ungroup()
head(pair.treat.settl)
summary(pair.treat.settl)

pair.control.settl <- cbind(DiD.data.matchingCov.dropC[i.control,"SettlementID"], pair.treat.settl) 
names(pair.control.settl)[1:2] <- c("ctrl.id", "treat.id")
head(pair.control.settl)  

settl.check <- pair.control.settl %>% 
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("ctrl.id"="SettlementID")  ) %>%
  rename(ctrl.settlement=SettlementName) %>%
  left_join(select(DiD.data.matchingCov.dropC,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("ctrl.id"="SettlementID") ) %>% 
  rename(ctrl.TimeMarket=TimeMarket, ctrl.Fisher=Fisher, ctrl.eth.polarize = eth.polarize) %>% 
  
  left_join(select(SETTLEMENT,SettlementName,SettlementID), by=c("treat.id"="SettlementID") ) %>%
  rename(treat.settlement=SettlementName) %>% 
  left_join(select(DiD.data.matchingCov.dropC,TimeMarket, Fisher, eth.polarize, lat, long,SettlementID), by=c("treat.id"="SettlementID") ) %>% 
  rename(treat.TimeMarket=TimeMarket, treat.Fisher=Fisher, treat.eth.polarize = eth.polarize) %>% 
  
  mutate(TimeMarket.dis = treat.TimeMarket-ctrl.TimeMarket, 
         Fisher.dis = treat.Fisher-ctrl.Fisher, 
         eth.polarize.dis = treat.eth.polarize-ctrl.eth.polarize) %>% 
  select(treat.settlement, ctrl.settlement, pair.id,t.mal.dist,c.mal.dist, treat.TimeMarket,ctrl.TimeMarket,treat.Fisher,ctrl.Fisher, 
         treat.eth.polarize, ctrl.eth.polarize, pair.dist,TimeMarket.dis:eth.polarize.dis)
head(settl.check)
#export(settl.check,"D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/pair_name_1to3_w_wLatLon_comp.xlsx")

DiD.data.SettlMatch.dropC <- data.frame()
DiD.data.SettlMatch.dropC <- rbind(pair.treat.settl, 
                             pair.control.settl %>% 
                               select(-treat.id) %>% 
                               rename(SettlementID=ctrl.id)) %>% 
  arrange(pair.id,SettlementID) 
head(DiD.data.SettlMatch.dropC)

DiD.data.SettlMatch.dropC <- DiD.data.SettlMatch.dropC %>% left_join(DiD.data, by="SettlementID")
DiD.data.SettlMatch.dropC <- as.data.frame(DiD.data.SettlMatch.dropC)



##--------Renaming DiD.data to DiD.data.coarseMatch
##--Important: Now changing "DiD.data" into "DiD.data" dataframe "DiD.data.coarseMatch", and renaming "DiD.data.SettlMatch" "DiD.data" for Sections 6.1 to 6.4
DiD.data.coarseMatch <- DiD.data
DiD.data.coarseMatch <- as.data.frame(DiD.data.coarseMatch)

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##-- 2.2 Sensitivity Check: Stargazer 
varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

##---DiD Regression model (presenting 6 alternative models)
##---Spec 1-5: 1to3 matching DiD; varying DiD specs
##---Spec 6-8: Replicate of main spec (Spec 1); varying matching (1to1, 1to2, and coarse)
regValue.list <-list()
master.reg.list <- list()
for (i in varNames) {
  print(i)
  
    ##----------------------------------------------------------------------------##
    ## Specification   (base model)
    Y <- DiD.data.SettlMatch[,i]
    w <- DiD.data.SettlMatch[,"dist.wt"]
    ## Specification 1 (base/full model): 1-to-3 matching (with weight) and full set of DID elements (FEs and HH covs)
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post +
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID  + pair.id + InterviewYear  | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch,exactDOF = TRUE, weights = w)
    regValue.1 <- regValue

    ##----------------------------------------------------------------------------##
    ## Specification 2 (base and relaxing pair.id clusters)
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID  + pair.id + InterviewYear  | 0 | SettlementID, data=DiD.data.SettlMatch,exactDOF = TRUE, weights = w)
    regValue.2<- regValue
   
    ##----------------------------------------------------------------------------##
    ## Specification 3 (base and relaxing Interview Year FEs)
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID  + pair.id  | 0 | SettlementID, data=DiD.data.SettlMatch,exactDOF = TRUE, weights = w)
    regValue.3<- regValue
  
    ##----------------------------------------------------------------------------##
    ## Specification 4 (base and not controling HH covariates)
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                           PrimaryLivelihood.bin
                     | SettlementID  + pair.id + InterviewYear | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch,exactDOF = TRUE, weights = w)
    regValue.4 <- regValue
    
    ##----------------------------------------------------------------------------##
    ## Specification 5 (base and drop 3 troublesome control units (drop and redo matching))
    Y <- DiD.data.SettlMatch.dropC[,i]
    w <- DiD.data.SettlMatch.dropC[,"dist.wt"]
    ## Specification 1 (base/full model): 1-to-3 matching (with weight) and full set of DID elements (FEs and HH covs)
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID  + pair.id + InterviewYear  | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch.dropC,exactDOF = TRUE, weights = w)
    regValue.5 <- regValue
    
    ##----------------------------------------------------------------------------##
    ## Specification 6: 1 to 2
    Y <- DiD.data.SettlMatch_2[,i]
    w <- DiD.data.SettlMatch_2[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID  + pair.id + InterviewYear  | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch_2,exactDOF = TRUE, weights = w)
    regValue.6<- regValue
  
    ##----------------------------------------------------------------------------##
    ## Specification 7: 1 to 1
    Y <- DiD.data.SettlMatch_1[,i]
    w <- DiD.data.SettlMatch_1[,"dist.wt"]
    
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID  + pair.id + InterviewYear | 0 | SettlementID + pair.id, data=DiD.data.SettlMatch_1,exactDOF = TRUE, weights = w)
    regValue.7 <- regValue
    
    ##----------------------------------------------------------------------------##
    ## Specification 8  (coarse matching; note: no more pair.id FE and cluster; also no "weight")
    Y <- DiD.data.coarseMatch[,i]
    regValue <- felm(Y  ~  Treatment + Post + Treatment:Post + 
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + InterviewYear  | 0 | SettlementID, data=DiD.data.coarseMatch,exactDOF = TRUE)
    regValue.8 <- regValue
    

  ##
  master.reg.list[[i]] <- list(regValue.1,regValue.2,regValue.3,regValue.4,regValue.5,regValue.6,regValue.7,regValue.8)
}

names(master.reg.list$FSIndex_z) <- paste0("Spec ",seq(1:8))
names(master.reg.list$MAIndex_z) <- paste0("Spec ",seq(1:8))
names(master.reg.list$MTIndex_z) <- paste0("Spec ",seq(1:8))
names(master.reg.list$PAIndex_z) <- paste0("Spec ",seq(1:8))
names(master.reg.list$SERate_z) <- paste0("Spec ",seq(1:8))

# summary(master.reg.list$FSIndex_z[[1]])
# summary(master.reg.list$FSIndex_z$Spec1)

##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-- Output Sensitivity result tables for BigFive_z
##save texts -- with FEs
stargazer(master.reg.list$FSIndex_z[c("Spec 1","Spec 2","Spec 3","Spec 4","Spec 5","Spec 6","Spec 7","Spec 8")],
          out = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/Settlement_matching/tables/DiD-seascape-z-Sensitivity_FS.txt", type = "text", 
          df = FALSE, notes.append = FALSE, star.cutoffs = NA, omit.table.layout = "n", 
          keep = c("Treatment:Post1"), covariate.labels=c("Treatment X Post"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(master.reg.list$FSIndex_z), dep.var.labels="Health (Food Security Index)",
          add.lines = list(c("Settlement Matching",rep("1-to-3",5), "1-to-2", "1-to-1", "none"),
                           c("Pair Cluster (s.e.)","Yes","No", "Yes", rep("Yes",5)),
                           c("Interview Year FEs","Yes","Yes","No","Yes",rep("Yes",4)),
                           c("Household Covariates","Yes","Yes","Yes","No",rep("Yes",4)),
                           c("Control units",rep("All",4), "Drop-3", rep("All",3)),
                           c("Settlement Cluster (s.e.)",rep("Yes",8)),
                           c("Settlement, Pair FEs",rep("Yes",8))))


stargazer(master.reg.list$PAIndex_z[c("Spec 1","Spec 2","Spec 3","Spec 4","Spec 5","Spec 6","Spec 7","Spec 8")],
          out = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/Settlement_matching/tables/DiD-seascape-z-Sensitivity_PA.txt", type = "text", 
          df = FALSE, notes.append = FALSE, star.cutoffs = NA, omit.table.layout = "n", 
          keep = c("Treatment:Post1"), covariate.labels=c("Treatment X Post"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(master.reg.list$FSIndex_z), dep.var.labels="Culture (Place Attachment Index)",
          add.lines = list(c("Settlement Matching",rep("1-to-3",5), "1-to-2", "1-to-1", "none"),
                           c("Pair Cluster (s.e.)","Yes","No", "Yes", rep("Yes",5)),
                           c("Interview Year FEs","Yes","Yes","No","Yes",rep("Yes",4)),
                           c("Household Covariates","Yes","Yes","Yes","No",rep("Yes",4)),
                           c("Control units",rep("All",4), "Drop-3", rep("All",3)),
                           c("Settlement Cluster (s.e.)",rep("Yes",8)),
                           c("Settlement, Pair FEs",rep("Yes",8))))

stargazer(master.reg.list$MTIndex_z[c("Spec 1","Spec 2","Spec 3","Spec 4","Spec 5","Spec 6","Spec 7","Spec 8")],
          out = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/Settlement_matching/tables/DiD-seascape-z-Sensitivity_MT.txt", type = "text", 
          df = FALSE, notes.append = FALSE, star.cutoffs = NA, omit.table.layout = "n", 
          keep = c("Treatment:Post1"), covariate.labels=c("Treatment X Post"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(master.reg.list$FSIndex_z), dep.var.labels="Political Empowerment (Marine Tenure Index)",
          add.lines = list(c("Settlement Matching",rep("1-to-3",5), "1-to-2", "1-to-1", "none"),
                           c("Pair Cluster (s.e.)","Yes","No", "Yes", rep("Yes",5)),
                           c("Interview Year FEs","Yes","Yes","No","Yes",rep("Yes",4)),
                           c("Household Covariates","Yes","Yes","Yes","No",rep("Yes",4)),
                           c("Control units",rep("All",4), "Drop-3", rep("All",3)),
                           c("Settlement Cluster (s.e.)",rep("Yes",8)),
                           c("Settlement, Pair FEs",rep("Yes",8))))

stargazer(master.reg.list$MAIndex_z[c("Spec 1","Spec 2","Spec 3","Spec 4","Spec 5","Spec 6","Spec 7","Spec 8")],
          out = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/Settlement_matching/tables/DiD-seascape-z-Sensitivity_MA.txt", type = "text", 
          df = FALSE, notes.append = FALSE, star.cutoffs = NA, omit.table.layout = "n", 
          keep = c("Treatment:Post1"), covariate.labels=c("Treatment X Post"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(master.reg.list$FSIndex_z), dep.var.labels="Economic Wealth (Material Assets Index)",
          add.lines = list(c("Settlement Matching",rep("1-to-3",5), "1-to-2", "1-to-1", "none"),
                           c("Pair Cluster (s.e.)","Yes","No", "Yes", rep("Yes",5)),
                           c("Interview Year FEs","Yes","Yes","No","Yes",rep("Yes",4)),
                           c("Household Covariates","Yes","Yes","Yes","No",rep("Yes",4)),
                           c("Control units",rep("All",4), "Drop-3", rep("All",3)),
                           c("Settlement Cluster (s.e.)",rep("Yes",8)),
                           c("Settlement, Pair FEs",rep("Yes",8))))

stargazer(master.reg.list$SERate_z[c("Spec 1","Spec 2","Spec 3","Spec 4","Spec 5","Spec 6","Spec 7","Spec 8")],
          out = "D:/Dropbox/MPA_research/Paper 0-MPA Impact BHS/Settlement_matching/tables/DiD-seascape-z-Sensitivity_SE.txt", type = "text", 
          df = FALSE, notes.append = FALSE, star.cutoffs = NA, omit.table.layout = "n", 
          keep = c("Treatment:Post1"), covariate.labels=c("Treatment X Post"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(master.reg.list$FSIndex_z), dep.var.labels="Education (School Enrolment Rate)",
          add.lines = list(c("Settlement Matching",rep("1-to-3",5), "1-to-2", "1-to-1", "none"),
                           c("Pair Cluster (s.e.)","Yes","No", "Yes", rep("Yes",5)),
                           c("Interview Year FEs","Yes","Yes","No","Yes",rep("Yes",4)),
                           c("Household Covariates","Yes","Yes","Yes","No",rep("Yes",4)),
                           c("Control units",rep("All",4), "Drop-3", rep("All",3)),
                           c("Settlement Cluster (s.e.)",rep("Yes",8)),
                           c("Settlement, Pair FEs",rep("Yes",8))))



##---END---##
##---END---##
##---END---##
