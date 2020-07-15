##---MPA Social Impacts and Equity---##
##---September 2019---##
##---Duong Le---##
##---Data: BHS, with provision to incorporate SBS---##

##---0. Sourcing and creating data frame for status/trend and impact
##---1a. Conducting Principle Component Analysis (PCA) for Asset weights: 11 asset items
##---1b. Conducting Principle Component Analysis (PCA) for Asset weights: 9 asset items (combine boats)
##---2. Generating subGroup indicators
##---3. Generating household demographic indicators
##---4. Generate new ethnic dominance indicator (by marine tenureship)
##---5. Calculate GINI for baseline MA 
##---6. Export data frame to Excel for exploratory Stata analysis

##---7. Evidence for baseline inequality in asset wealth (MAIndex_pca)

##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---0. Sourcing and creating data frame for status/trend and impact
##---0. Sourcing and creating data frame for status/trend and impact

setwd("D:/Dropbox/MPA_research/MPAMystery/")

source('2_Functions/2_Analysis/Function_process_covariates.R')
source("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/my_summary_plot_functions.R")

mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
pacman::p_load(lfe,cowplot,stargazer,broom,qvalue,psych,factoextra,ineq, tidyverse)
# library(lfe) #regressions
# library(cowplot)
# library(Matrix)
# library(stargazer)
# library(broom)
# library(tidyverse)
# library(qvalue)
# library(psych) #summary statistics by groups
# library(factoextra)

# --- DiD specification 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, MAIndex:FSIndex ,SERate ,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear)) %>% 
  filter(!is.na(IndividualGender))

# --- Filter to look at 6 BHS  --- Nov 2019
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6))

## --- retaining the raw HHH.age (The IndividualAge is currently already categorized)
HH.age.raw <- IndDemos %>%
  filter(RelationHHH==0) %>%
  mutate(IndividualAge_raw = IndividualAge) %>% 
  select(HouseholdID,IndividualAge_raw) %>%
  left_join(select(DiD.data,HouseholdID,yearsPost),by="HouseholdID") %>%
  select(HouseholdID,IndividualAge_raw)%>% 
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
         TV = ifelse(MPAID==17,0,TV), #note: no TV in Kei t3, so remove all TV from MAIndex for both Kei t0 and t3
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
         CarTruck_dum = ifelse(CarTruck>0,1,0)) 
# mutate(Household_asset = Entertain + 2*PhoneCombined + 3*Satellite + 4*TV + 5*Generator,
#        Boats_w1 = BoatNoMotor + 2*BoatOutboard + 3*BoatInboard,
#        Boats_w2 = 6*BoatNoMotor + 7*BoatOutboard + 8*BoatInboard,
#        Boats_motor_w1 = 1*BoatOutboard + 2*BoatInboard,
#        Boats_motor_w2 = 7*BoatOutboard + 8*BoatInboard,
#        Vehicles_w1 = Bicycle + 2*Motorcycle + 3*CarTruck,
#        Vehicles_w2 = 9*Bicycle + 10*Motorcycle + 11*CarTruck) %>% 
# mutate(Boats_dum = ifelse(BoatNoMotor>0 | BoatOutboard >0 | BoatInboard >0,1,0),
#        Boats_motor_dum = ifelse(BoatOutboard >0 | BoatInboard >0,1,0),
#        Vehicles_dum = ifelse(Bicycle>0 | Motorcycle>0 | CarTruck>0,1,0))

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

summary(DiD.data)

# --- summary(DiD.data)
DiD.data.summary <- DiD.data %>% 
  select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  


# calculate Z scores (standardized values for each of the Big Five)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, CarTruck:Generator), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}

##---END---##
##---END---##
##---END---##

# 
# 
# ##---BEGIN---##
# ##---BEGIN---##
# ##---BEGIN---##
# ##---1a. Conducting Principle Component Analysis (PCA) for Asset weights: 11 asset items
# ##---1a. Conducting Principle Component Analysis (PCA) for Asset weights: 11 asset items
# 
# asset.frame <- DiD.data %>% 
#   subset(RemoveMA=="No") %>% 
#   select(HouseholdID:Treatment, CarTruck:Generator) %>% 
#   na.omit()
#   #omiting all NAs in assets to compute PCA weights (losing 13 records with NA)
# 
# # calculate Z scores (standardized values for each of the Big Five)
# asset.frame <- asset.frame %>% 
#   group_by(MonitoringYear) %>% 
#   mutate_at(vars(CarTruck:Generator), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
#   ungroup()
# summary(asset.frame)
# 
# 
# #Perform PCA to get eigenvalues (i.e. loading factors) for each asset's weights
# asset.pca <- prcomp(asset.frame[18:28], scale = FALSE)
# asset.pca.1 <- prcomp(asset.frame[7:17], scale = TRUE)
# 
# #Get eigenvalues, or variance percentage, for each principle components (i.e. how well each of PCs contribute in explaning the total variance in our asset data set)
# asset.eig.val <- get_eigenvalue(asset.pca)
# asset.eig.val
# fviz_eig(asset.pca) #a scree plot to illustrate the above
# 
# 
# #Get the contribution of each asset items in each PCs 
# #Importantly, literature use the contribution factors associated with the first component (i.e. Dimension 1) 
# #to construct the asset weights in computing composite asset-based index
# res.var <- get_pca_var(asset.pca)
# res.var$coord          # Coordinates
# res.var$contrib        # Contributions to the PCs
# res.var$cos2           # Quality of representation 
# 
# #Produce 2-dimensional PCA plots (i.e. looking at the first 2 PCs) to visualize the correlation of each assets in contributing to the first 2 PCs
# fviz_pca_var(asset.pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# #Conclusion: non-motorized boat has very weak contribution to explaining principle wealth indicator (orthognal to the other assets (not correlated)) -> combine all boats 
# ##---END---##
# ##---END---##
# ##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---1b. Conducting Principle Component Analysis (PCA) for Asset weights: 9 asset items (combine boats)
##---1b. Conducting Principle Component Analysis (PCA) for Asset weights: 9 asset items (combine boats)

asset.frame <- DiD.data %>% 
  subset(RemoveMA=="No") %>% 
  mutate(Boat=BoatNoMotor+BoatOutboard+BoatInboard) %>% 
  select(HouseholdID:Treatment, CarTruck:Motorcycle, PhoneCombined, TV, Entertain, Satellite, Generator, Boat) %>% 
  na.omit()
summary(asset.frame)
#Omiting all NAs in assets to compute PCA weights (losing 13 records with NA)

#calculate Z scores (standardized values for each of the Big Five)
asset.frame <- asset.frame %>% 
  group_by(MonitoringYear) %>% 
  mutate_at(vars(CarTruck:Boat), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()
summary(asset.frame)


#Perform PCA to get eigenvalues (i.e. loading factors) for each asset's weights
asset.pca <- prcomp(select(asset.frame,CarTruck_z:Boat_z), scale = FALSE)

#Get eigenvalues, or variance percentage, for each principle components (i.e. how well each of PCs contribute in explaning the total variance in our asset data set)
asset.eig.val <- get_eigenvalue(asset.pca)
asset.eig.val
fviz_eig(asset.pca) #a scree plot to illustrate the above
asset.eig.val.keep <- asset.eig.val %>% 
  subset(eigenvalue>=1) %>% 
  mutate(pc.contrib = variance.percent/sum(variance.percent)) #keep only PCs with eigenvalue >=1 and find % of explained variance that each PC contributes
asset.eig.val.keep <- as.data.frame(asset.eig.val.keep)

#Get the contribution of each asset items in each PCs 
#Importantly, literature use (1) the contribution factors associated with the first component (i.e. Dimension 1) 
#to construct the asset weights in computing composite asset-based index, or (2) all Dimensions with eigenvalue >=1
asset.weight.frame <- get_pca_var(asset.pca)$contrib
asset.weight.frame <- asset.weight.frame[ ,1:nrow(asset.eig.val.keep)] 
asset.weight.frame <- as.data.frame(t(asset.weight.frame))
asset.weight.frame <- cbind(asset.weight.frame,asset.eig.val.keep) 

#res.var$coord          # Coordinates
#res.var$contrib     # Contributions to the PCs
#res.var$cos2           # Quality of representation 

#Produce 2-dimensional PCA plots (i.e. looking at the first 2 PCs) to visualize the correlation of each assets in contributing to the first 2 PCs
fviz_pca_var(asset.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Extract weights (scoring factors) using the variance contribution of each assets to the First PC
asset.weight.pc1 <- asset.weight.frame[1,] %>% 
  select(CarTruck_z:Boat_z) 
asset.weight.pc1

varName<-c("CarTruck_z","Bicycle_z","Motorcycle_z","PhoneCombined_z","TV_z","Entertain_z","Satellite_z","Generator_z","Boat_z")

CarTruck_z_w <- asset.weight.pc1$CarTruck_z
Bicycle_z_w <- asset.weight.pc1$Bicycle_z
Motorcycle_z_w <- asset.weight.pc1$Motorcycle_z
PhoneCombined_z_w <- asset.weight.pc1$PhoneCombined_z
TV_z_w <- asset.weight.pc1$TV_z
Entertain_z_w <- asset.weight.pc1$Entertain_z
Satellite_z_w <- asset.weight.pc1$Satellite_z
Generator_z_w <- asset.weight.pc1$Generator_z
Boat_z_w <- asset.weight.pc1$Boat_z

#----------##
DiD.data <- DiD.data %>% 
  mutate(Boat=BoatNoMotor+BoatOutboard+BoatInboard) %>% 
  mutate(MAIndex_pca = CarTruck*CarTruck_z_w + Bicycle*Bicycle_z_w + Motorcycle*Motorcycle_z_w + PhoneCombined*PhoneCombined_z_w +TV*TV_z_w + Entertain*Entertain_z_w + Satellite*Satellite_z_w + Generator*Generator_z_w + Boat*Boat_z_w) 


##---1b. Additional: Conducting Principle Component Analysis (PCA) for Asset weights: 6 asset items (not including boats)
##---1b. Additional: Conducting Principle Component Analysis (PCA) for Asset weights: 6 asset items (not including boats)

# asset.frame <- DiD.data %>% 
#   subset(RemoveMA=="No") %>% 
#   mutate(Boat=BoatNoMotor+BoatOutboard+BoatInboard) %>% 
#   select(HouseholdID:Treatment, CarTruck:Motorcycle, PhoneCombined, TV, Entertain, Satellite, Generator) %>% 
#   na.omit()
# summary(asset.frame)
# #Omiting all NAs in assets to compute PCA weights (losing 13 records with NA)
# 
# #calculate Z scores (standardized values)
# asset.frame <- asset.frame %>%
#   group_by(MonitoringYear) %>%
#   mutate_at(vars(CarTruck:Generator), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>%
#   ungroup()
# summary(asset.frame)


#Perform PCA to get eigenvalues (i.e. loading factors) for each asset's weights
asset.frame.1 <- asset.frame %>%
  select(CarTruck_z:Generator_z)

asset.pca.1 <- prcomp(select(asset.frame.1,CarTruck_z:Generator_z), scale = FALSE)

#Get eigenvalues, or variance percentage, for each principle components (i.e. how well each of PCs contribute in explaning the total variance in our asset data set)
asset.eig.val.1 <- get_eigenvalue(asset.pca.1)
asset.eig.val.1
fviz_eig(asset.pca.1) #a scree plot to illustrate the above
asset.eig.val.keep.1 <- asset.eig.val.1 %>% 
  subset(eigenvalue>=1) %>% 
  mutate(pc.contrib.1 = variance.percent/sum(variance.percent)) #keep only PCs with eigenvalue >=1 and find % of explained variance that each PC contributes
asset.eig.val.keep.1 <- as.data.frame(asset.eig.val.keep.1)

#Get the contribution of each asset items in each PCs 
#Importantly, literature use (1) the contribution factors associated with the first component (i.e. Dimension 1) 
#to construct the asset weights in computing composite asset-based index, or (2) all Dimensions with eigenvalue >=1
asset.weight.frame.1 <- get_pca_var(asset.pca.1)$contrib
asset.weight.frame.1 <- asset.weight.frame.1[ ,1:nrow(asset.eig.val.keep.1)] 
asset.weight.frame.1 <- as.data.frame(t(asset.weight.frame.1))
asset.weight.frame.1 <- cbind(asset.weight.frame.1, asset.eig.val.keep.1) 

#res.var$coord          # Coordinates
#res.var$contrib     # Contributions to the PCs
#res.var$cos2           # Quality of representation 

#Produce 2-dimensional PCA plots (i.e. looking at the first 2 PCs) to visualize the correlation of each assets in contributing to the first 2 PCs
fviz_pca_var(asset.pca.1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Extract weights (scoring factors) using the variance contribution of each assets to the First PC
asset.weight.pc1.1 <- asset.weight.frame.1[1,] %>% 
  select(CarTruck_z:Generator_z) 
asset.weight.pc1.1

varName<-c("CarTruck_z","Bicycle_z","Motorcycle_z","PhoneCombined_z","TV_z","Entertain_z","Satellite_z","Generator_z")

CarTruck_z_w1 <- asset.weight.pc1.1$CarTruck_z
Bicycle_z_w1 <- asset.weight.pc1.1$Bicycle_z
Motorcycle_z_w1 <- asset.weight.pc1.1$Motorcycle_z
PhoneCombined_z_w1<- asset.weight.pc1.1$PhoneCombined_z
TV_z_w1 <- asset.weight.pc1.1$TV_z
Entertain_z_w1 <- asset.weight.pc1.1$Entertain_z
Satellite_z_w1 <- asset.weight.pc1.1$Satellite_z
Generator_z_w1 <- asset.weight.pc1.1$Generator_z

#----------##
#Reconstructing the new MA_index using the above PCA-derived weights (note: there's no boat in this asset basket, hence MAIndex_pca.1)
DiD.data <- DiD.data %>% 
  mutate(MAIndex_pca.1 = CarTruck*CarTruck_z_w1 + Bicycle*Bicycle_z_w1 + Motorcycle*Motorcycle_z_w1 + PhoneCombined*PhoneCombined_z_w1 +TV*TV_z_w1 + Entertain*Entertain_z_w1 + Satellite*Satellite_z_w1 + Generator*Generator_z_w1) 

##---END---##
##---END---##
##---END---##



##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---2. Generating subGroup indicators
##---2. Generating subGroup indicators

###Indicator for Fisher, Male, ethnic dominant (dummies)
DiD.data <- DiD.data %>% 
  mutate(Male=ifelse(IndividualGender==1,1,0),
         ethDom=dom.eth,
         Fisher=ifelse(PrimaryLivelihood==1,1,0),
         Fisher=ifelse(is.na(Fisher),0,Fisher))

# Indicator Material Asset Quintiles in each seascape-year (factors)
MA.quint.breaks <- DiD.data %>%
  group_by(yearsPost) %>%
  summarise(quint1 = quantile(MAIndex_pca, probs = c(.2), na.rm = T),
            quint2 = quantile(MAIndex_pca, probs = c(.4), na.rm = T),
            quint3 = quantile(MAIndex_pca, probs = c(.6), na.rm = T),
            quint4 = quantile(MAIndex_pca, probs = c(.8), na.rm = T))

##Note: the wealthQuint factor is reverse; so that richest quintile==1; poorest==5
## So that in the later regressions the richest quintile will be the base/reference category
DiD.data <- DiD.data %>% 
  left_join(MA.quint.breaks, by = "yearsPost") %>% 
  mutate(wealthQuint=ifelse(MAIndex_pca<=quint1,5,
                            ifelse(MAIndex_pca>quint1 & MAIndex_pca<=quint2,4,
                                   ifelse(MAIndex_pca>quint2 & MAIndex_pca<=quint3,3,
                                          ifelse(MAIndex_pca>quint3 & MAIndex_pca<=quint4,2,1))))) %>% 
  mutate(wealthQuint=as.factor(wealthQuint))

# varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

#spot check
DiD.data %>% 
  group_by(wealthQuint) %>% 
  summarise(min.x=min(MAIndex_pca),max.x=max(MAIndex_pca))
##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---3. Generating household demographic indicators
##---3. Generating household demographic indicators


#---- Import ethnic and education look up tables ----
ethnic.lkp<- import("x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx")
education.lkp <- import("x_Flat_data_files/1_Social/Inputs/education_lkp_BHS.xlsx")

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


# Education level of HH members
# some duplicates in education table (NAs, perhaps white spaces), filtering out these here
education.lkp1 <- education.lkp %>% 
  distinct(IndividualEducation,ed.level,.keep_all = T) %>%
  filter(ed.level!="NA")

IndDemos <- IndDemos %>% 
  left_join(education.lkp1, by=c("IndividualEducation")) %>% 
  ## Household-demographic factors: Household composition
  mutate(All.dum=1, 
         Male.dum = ifelse(IndividualGender==1,1,0),
         Female.dum = ifelse(IndividualGender==0,1,0),
         
         All.U18.dum = ifelse(IndividualAge<18,1,0),
         Male.U18.dum = ifelse(IndividualAge<18 & IndividualGender==1,1,0),
         Female.U18.dum = ifelse(IndividualAge<18 & IndividualGender==0,1,0),
         
         All.baby.dum = ifelse(IndividualAge<6,1,0),
         Male.baby.dum = ifelse(IndividualAge<6 & IndividualGender==1,1,0),
         Female.baby.dum = ifelse(IndividualAge<6 & IndividualGender==0,1,0),
         
         All.schoolAge.dum = ifelse(IndividualAge>=6 & IndividualAge<18,1,0),
         Male.schoolAge.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualGender==1,1,0),
         Female.schoolAge.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualGender==0,1,0),
         
         All.schoolAge.enrolled.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualEnrolled==1,1,0),
         Male.schoolAge.enrolled.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualEnrolled==1 & IndividualGender==1,1,0),
         Female.schoolAge.enrolled.dum = ifelse(IndividualAge>=6 & IndividualAge<18 & IndividualEnrolled==1 & IndividualGender==0,1,0),
         
         All.workingAge.dum = ifelse(IndividualAge>=18 & IndividualAge<=65,1,0),
         Male.workingAge.dum = ifelse(IndividualAge>=18 & IndividualAge<=65 & IndividualGender==1,1,0),
         Female.workingAge.dum = ifelse(IndividualAge>=18 & IndividualAge<=65 & IndividualGender==0,1,0),
         
         All.elder.dum = ifelse(IndividualAge>65,1,0),
         Male.elder.dum = ifelse(IndividualAge>65,1,0),
         Female.elder.dum = ifelse(IndividualAge>65,1,0)) %>% 
  
  ## Household-demographic factors: family relationship 
  mutate(is.HHH = ifelse(RelationHHH==0,1,0),
         is.HHH.spouse = ifelse(RelationHHH==1,1,0),
         
         is.HHH.child = ifelse(RelationHHH==2 | RelationHHH==6 | RelationHHH==11,1,0),
         is.HHH.baby = ifelse((RelationHHH==2 | RelationHHH==6 | RelationHHH==11) & IndividualAge<6,1,0),
         is.HHH.fosterChild = ifelse(RelationHHH==11,1,0),
         
         is.HHH.parent = ifelse(RelationHHH==3 | RelationHHH==5,1,0),
         is.HHH.grandChild = ifelse(RelationHHH==4,1,0),
         is.HHH.relative = ifelse(RelationHHH==7 | RelationHHH==8 | RelationHHH==9 | RelationHHH==10 | RelationHHH==12,1,0),
         is.HHH.notRelated = ifelse(RelationHHH==13,1,0)) %>% 
  
  
  ## Household-demographic factors: marriage structure (husband-wife relationship)
  mutate(HHH.gender = ifelse(RelationHHH == 0, IndividualGender, NA),
         HHH.ed.level = ifelse(RelationHHH == 0, ed.level, NA),
         HHH.age = ifelse(RelationHHH == 0, IndividualAge, NA),
         
         spouse.gender = ifelse(RelationHHH == 1, IndividualGender, NA),
         spouse.ed.level = ifelse(RelationHHH == 1, ed.level, NA),
         spouse.age = ifelse(RelationHHH == 1, IndividualAge, NA)) 


## Get HH.Demos and HH.Mariage, before combining them for the household-demographic indicator dataframe 
HH.Demos <- data.frame()

HH.Demos <- IndDemos %>% 
  select(HouseholdID, All.dum:Female.elder.dum, is.HHH:is.HHH.notRelated, HHH.gender:spouse.age) %>% 
  group_by(HouseholdID) %>% 
  summarise(HH.Total.count = sum(All.dum),
            HH.Male.count = sum(Male.dum),
            HH.Female.count = sum(Female.dum),
            
            HH.U18.count = sum(All.U18.dum),
            HH.Male.U18.count = sum(Male.U18.dum),
            HH.Female.U18.count = sum(Female.U18.dum),
            
            HH.baby.count = sum(All.baby.dum),
            HH.Male.baby.count = sum(Male.baby.dum),
            HH.Female.baby.count = sum(Female.baby.dum),
            
            HH.schoolAge.count = sum(All.schoolAge.dum),
            HH.Male.schoolAge.count = sum(Male.schoolAge.dum),
            HH.Female.schoolAge.count = sum(Female.schoolAge.dum),
            
            HH.schoolAge.enrol.count = sum(All.schoolAge.enrolled.dum),
            HH.Male.schoolAge.enrol.count = sum(Male.schoolAge.enrolled.dum),
            HH.Female.schoolAge.enrol.count = sum(Female.schoolAge.enrolled.dum),
            
            HH.workingAge.count = sum(All.workingAge.dum),
            HH.Male.workingAge.count = sum(Male.workingAge.dum),
            HH.Female.workingAge.count = sum(Female.workingAge.dum),
            
            HH.elder.count = sum(All.elder.dum),
            HH.Male.elder.count = sum(Male.elder.dum),
            HH.Female.elder.count = sum(Female.elder.dum),
            
            HHH.count = sum(is.HHH),
            HHH.spouse.count = sum(is.HHH.spouse),
            HHH.child.count = sum(is.HHH.child),
            HHH.baby.count = sum(is.HHH.baby),
            HHH.childFoster.count = sum(is.HHH.fosterChild),
            HHH.parent.count = sum(is.HHH.parent),
            HHH.grandChild.count = sum(is.HHH.grandChild),
            HHH.relative.count = sum(is.HHH.relative),
            HHH.notRelated.count = sum(is.HHH.notRelated),
            
            HHH.gender = sum(HHH.gender,na.rm=TRUE), 
            HHH.age = sum(HHH.age,na.rm=TRUE),
            HHH.ed.level = sum(HHH.ed.level,na.rm=TRUE),
            
            spouse.gender = sum(spouse.gender,na.rm=TRUE), 
            spouse.age = sum(spouse.age,na.rm=TRUE),
            spouse.ed.level = sum(spouse.ed.level,na.rm=TRUE),
            
            
            ## Generate important indicators re: household's demographic and compostion
            HHdemo.single.liveAlone = ifelse(HH.Total.count==1,1,0),
            HHdemo.single.withChildren = ifelse(HHH.age<=65 & HHH.spouse.count==0 & HHH.child.count>0,1,0),
            HHdemo.single.withElder = ifelse(HHH.age<=65 & HHH.spouse.count==0 & HH.elder.count>0,1,0),
            
            HHdemo.married.withChildren = ifelse(HHH.age<=65 & HHH.spouse.count==1 & HHH.child.count>0,1,0),
            HHdemo.married.withBaby = ifelse(HHH.age<=65 & HHH.spouse.count==1 & HH.baby.count>0,1,0),
            HHdemo.married.withChildMale = ifelse(HHH.age<=65 & HHH.spouse.count==1 & HHH.child.count>0 & HH.baby.count==0 & HH.Male.schoolAge.count>HH.Female.schoolAge.count,1,0),
            HHdemo.married.multiGen = ifelse((HHH.child.count>0 & HHH.parent.count>0) | HHH.grandChild.count>0,1,0),
            
            #Generate indicators re: marriage characteristics
            HHdemo.Male.headed = ifelse(HHH.gender==1,1,0),
            HHdemo.Husband.Older = ifelse((HHH.age>spouse.age) & HHH.gender==1,1,0),
            HHdemo.Husband.HigherEd = ifelse((HHH.ed.level>spouse.ed.level) & HHH.gender==1,1,0))

#merge HH.Demos into the main DiD.data
DiD.data <- DiD.data %>% 
  left_join(HH.Demos, by = "HouseholdID") 
summary(DiD.data)


count.MPA.IDs <- DiD.data %>% 
  group_by(MPAID, Treatment) %>%
  summarise(MPA=length(MPAID))

##---END---##
##---END---##
##---END---##




##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---4. Generate new ethnic dominance indicator (by marine tenureship)

# some duplicates in ethnicity table (NAs), filtering out these here
# ethnic.lkp1 <- ethnic.lkp %>% 
#   distinct(std.eth.str,eth.iso,.keep_all = T) %>%
#   filter(eth.iso!="NA")
# filter(!ethnic.id%in%c(2734,2813,5422,5425,5643)) # select out the specific five duplicates

HH.eth.new <- HHData %>% 
  select(HouseholdID,PaternalEthnicity, MTIndex, YrResident, MonitoringYear, SettlementID) %>% 
  mutate(PaternalEthnicity=str_clean(PaternalEthnicity)) %>% 
  left_join(ethnic.lkp1, by=c("PaternalEthnicity"="std.eth.str")) %>% 
  mutate(SettlYear=paste0(MonitoringYear,"_",SettlementID))


# this code gives you the ethnicity associated with (1) highest MT and (2) MT>=4, for each settlement at each sampling period
max.eth.highest.Tenure <- HH.eth.new %>%
  group_by(SettlYear,eth.iso) %>%
  summarise(MT.mean=mean(MTIndex), MT.median=median(MTIndex)) %>% 
  top_n(1, MT.mean) 


HH.eth.new$dom.eth <- NA
# assign dominant ethnicity in a loop will assign a 0 if parentalEthinicity==NA
for (i in unique(HH.eth.new$SettlYear)){
  max.eth.dom <-  max.eth.highest.Tenure$eth.iso[max.eth.highest.Tenure$SettlYear==i]
  HH.eth.new$dom.eth[HH.eth.new$SettlYear==i] <- ifelse(HH.eth.new$eth.iso[HH.eth.new$SettlYear==i]%in%max.eth.dom,1,0)
}

HH.eth.new <- HH.eth.new %>% 
  mutate(ethDom.highest.MT=dom.eth) %>% 
  select(HouseholdID, ethDom.highest.MT) 


#merge HH.eth.new into the main DiD.data
DiD.data <- DiD.data %>% 
  left_join(HH.eth.new, by = "HouseholdID") 
summary(DiD.data)


# calculate p scores
pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + ethDom.highest.MT + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  
##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---5. Calculate GINI for baseline MA for each settlement at each sampling period

#install.packages("ineq")
library(ineq)

MAIndex_GINI_Settl <- DiD.data %>% 
  group_by(SettlementID, yearsPost) %>% 
  summarise(MAIndex_GINI_Settl = ineq(MAIndex_pca,type="Gini")) 

DiD.data <- DiD.data %>% 
  left_join(MAIndex_GINI_Settl, by=c("SettlementID", "yearsPost"))


#Plot the Lorenz curve 
plot(Lc(DiD.data$MAIndex_pca),col="darkred",lwd=2)

hist(DiD.data$MAIndex_GINI_Settl)

##---covariate for settlement's baseline average MAIndex_pca
MAIndex_pca.base.Settl <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  select(SettlementID, MAIndex_pca, MAIndex_pca.1) %>% 
  group_by(SettlementID) %>% 
  summarise(MAIndex_pca.base = mean(MAIndex_pca, na.rm = TRUE),
            MAIndex_pca.1.base=mean(MAIndex_pca.1,na.rm = TRUE))

DiD.data <- DiD.data %>% 
  left_join(MAIndex_pca.base.Settl, by="SettlementID")
##---END---##
##---END---##
##---END---##




##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---6. Export data frame to Excel for exploratory Stata analysis
# 
# #Export to txt file
# library(foreign)
# #install.packages("writexl")
# library(writexl)
# write.table(DiD.data, "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/MPA_HouseholdDemo_data.txt", sep="\t")
# write_xlsx(DiD.data, path = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/MPA_HouseholdDemo_data", col_names = TRUE, format_headers = TRUE)
# #source("2_Functions/my_summary_plot_functions.R")

DiD.data <- DiD.data %>% 
  mutate(yearsPost.recode=ifelse(yearsPost==0,"Baseline",
                                 ifelse(yearsPost%in%2:3,"First Postline",
                                        "Second Postline")))
# table1.dat <- DiD.data %>% 
#   group_by(yearsPost.recode) %>% 
#   summarise(MAIndex_pca.mean=mean2(MAIndex_pca), MAIndex_pca.sd=sd2(MAIndex_pca),
#             MTIndex.mean=mean2(MTIndex), MTIndex.sd=sd2(MTIndex),
#             FSIndex.mean=mean2(FSIndex), FSIndex.sd=sd2(FSIndex)) %>% 
#   gather(var,val,MAIndex_pca.mean:FSIndex.sd) %>% 
#   separate(var,c("variable","parameter"), "\\.") %>% 
#   unite(var,yearsPost.recode, parameter) %>% 
#   spread(var,val) 
# 
# head(table1.dat)

DiD.data <- DiD.data %>% 
  mutate(ed.no = ifelse(ed.level==0,1,0),
         ed.primary = ifelse(ed.level<=1,1,0),
         ed.high = ifelse(ed.level>=3,1,0),
         ed.college = ifelse(ed.level>=4,1,0))


## -------------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------------- ##
# Descriptive Statistics
# Descriptive Statistics
# Descriptive Statistics
# Descriptive statistics for cars with automatic transmission
stargazer(subset(DiD.data[c("MAIndex_pca","MAIndex_pca.1","Fisher", "Female", "Ethnic", "Indigenous",
                            "TimeMarket","n.child","ed.level", 
                            "ed.no", "ed.primary", "ed.high", "ed.college",
                            "FSIndex","MTIndex","PAIndex")], DiD.data$yearsPost==0),
          title="Descriptive statistics: Baseline", type = "text", digits=2, out="D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/summary Stat/baseline-all.txt")





##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---7. Evidence for baseline inequality in asset wealth (MAIndex_pca)
DiD.data.sumStat <- DiD.data %>% 
  select(MAIndex_pca, MAIndex_pca.1, HouseholdID:IndividualAge, IndividualAge_raw, EconStatusTrend,MAIndex:SERate, MAIndex_z: Generator_z, Male:wealthQuint, ethDom.highest.MT, HH.Total.count,HH.workingAge.count, HH.U18.count,HHH.age, HHH.ed.level, MAIndex_GINI_Settl, yearsPost.recode)

describe(DiD.data.sumStat)
describeBy(DiD.data.sumStat, DiD.data.sumStat$yearsPost.recode) #summary stat by yearsPost 

#Define 5 subgroups
DiD.data <- DiD.data %>% 
  mutate(Fisher_3 = ifelse(PrimaryLivelihood==1 | SecondaryLivelihood==1 | TertiaryLivelihood==1,1,0))

DiD.data.median.baseline <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  group_by(MPAID) %>% 
  summarise(YrResident.median = median(YrResident, na.rm=TRUE), 
            Age.median = median(HHH.age,na.rm=TRUE))



DiD.data.baseline <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  left_join(DiD.data.median.baseline, by="MPAID") %>% 
  mutate(Gender = ifelse(Male==1,"Male","Female"),
         Fisher = as.factor(Fisher),
         Fisher_3 = as.factor(Fisher_3), 
         Livelihood = ifelse(Fisher==1,"Fisher","Other"), 
         Ethnicity = ifelse(ethDom.highest.MT==1,"Dominant", " Non-dominant"),
         Residence = ifelse(YrResident<=YrResident.median,"Below MPA Median", "Above MPA Median"),
         Age_Group = ifelse(IndividualAge_raw<=Age.median,"Young", "Old")) 



# table1.dat <- DiD.data.baseline %>% 
#   group_by(Male) %>% 
#   summarise(MAIndex_pca.mean=mean2(MAIndex_pca), MAIndex_pca.sd=sd2(MAIndex_pca),
#             MTIndex.mean=mean2(MTIndex), MTIndex.sd=sd2(MTIndex),
#             FSIndex.mean=mean2(FSIndex), FSIndex.sd=sd2(FSIndex)) %>% 
#   gather(var,val,MAIndex_pca.mean:FSIndex.sd) %>% 
#   separate(var,c("variable","parameter"), "\\.") %>% 
#   unite(var,Male, parameter) %>% 
#   spread(var,val) 
# head(table1.dat)


#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable to be summarized
# groupnames : vector of column names to be used as grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#--- Step 1: Baseline Social Inequality -- Point plots
#--- Step 1: Baseline Social Inequality -- Point plots
#--- Step 1: Baseline Social Inequality -- Point plots
#--- Step 1: Baseline Social Inequality -- Point plots

pd <- position_dodge(.05) # move them .05 to the left and right
##----Gender
Gender.baseline <- DiD.data.baseline %>% 
  mutate(Male = factor(Male)) %>% 
  group_by(Male) %>% 
  summarise(mean= mean(MAIndex_pca, na.rm=TRUE), 
            sd = sd(MAIndex_pca, na.rm=TRUE), 
            median = median(MAIndex_pca, na.rm=TRUE),
            n=length(MAIndex_pca), 
            se = sd/sqrt(n)) %>% 
  mutate(Gender=ifelse(Male==0,"Female","Male"))

test.val <- round(t.test(DiD.data.baseline$MAIndex_pca~DiD.data.baseline$Gender)$statistic, digits = 3)

Gender.point.plot <- ggplot(Gender.baseline,aes(x=Gender,y=mean, label=paste0("n= ",n))) + 
  geom_point(stat="identity", position = pd, fill='black', size=4) + 
  geom_point(aes(x=Gender,y=median), fill='red', shape=24, size=4) + 
  geom_errorbar(aes(ymin=mean-1.645*se, ymax=mean+1.645*se), width=0.5, position = pd) +
  geom_text(aes(y = mean+1.645*se + 0.5),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + theme(legend.position="none") +
  labs(x="",y="Asset Wealth (PCA)", title="Gender (Male vs. Female) ", subtitle=paste0("Baseline means difference t-test = ",test.val, " (median=triangle)"))  
Gender.point.plot
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Gender_baseIneq",".jpg"))


##----Livelihood
Livelihood.baseline <- DiD.data.baseline %>% 
  group_by(Livelihood) %>% 
  summarise(mean= mean(MAIndex_pca, na.rm=TRUE), 
            sd = sd(MAIndex_pca, na.rm=TRUE), 
            median = median(MAIndex_pca, na.rm=TRUE),
            n=length(MAIndex_pca), 
            se = sd/sqrt(n)) 

test.val <- round(t.test(DiD.data.baseline$MAIndex_pca~DiD.data.baseline$Livelihood)$statistic, digits = 3)

Livelihood.point.plot <- ggplot(Livelihood.baseline,aes(x=Livelihood,y=mean, label=paste0("n= ",n))) + 
  geom_point(stat="identity", position = pd, fill='black', size=4) + 
  geom_point(aes(x=Livelihood,y=median), fill='red', shape=24, size=4) + 
  geom_errorbar(aes(ymin=mean-1.645*se, ymax=mean+1.645*se), width=0.5, position = pd) +
  geom_text(aes(y = mean+1.645*se + 0.5),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + theme(legend.position="none") +
  labs(x="",y="Asset Wealth (PCA)", title="Livelihood (Fisher vs. Other)", subtitle=paste0("Baseline means difference t-test = ",test.val, " (median=triangle)"))
Livelihood.point.plot
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Livelihood_baseIneq",".jpg"))



##----Ethnicity
Ethnicity.baseline <- DiD.data.baseline %>% 
  group_by(Ethnicity) %>% 
  summarise(mean= mean(MAIndex_pca, na.rm=TRUE), 
            sd = sd(MAIndex_pca, na.rm=TRUE), 
            median = median(MAIndex_pca, na.rm=TRUE),
            n=length(MAIndex_pca), 
            se = sd/sqrt(n)) 

test.val <- round(t.test(DiD.data.baseline$MAIndex_pca~DiD.data.baseline$Ethnicity)$statistic, digits = 3)


Ethnicity.point.plot <- ggplot(Ethnicity.baseline,aes(x=Ethnicity,y=mean, label=paste0("n= ",n))) + 
  geom_point(stat="identity", position = pd, fill='black', size=4) + 
  geom_point(aes(x=Ethnicity,y=median), fill='red', shape=24, size=4) + 
  geom_errorbar(aes(ymin=mean-1.645*se, ymax=mean+1.645*se), width=0.5, position = pd) +
  geom_text(aes(y = mean+1.645*se + 0.5),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + theme(legend.position="none") +
  labs(x="",y="Asset Wealth (PCA)", title="Ethnicity (Dominant vs. non-Dominant)", subtitle=paste0("Baseline means difference t-test = ",test.val, " (median=triangle)"))
Ethnicity.point.plot
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Ethnicity_baseIneq",".jpg"))



##----Residence
Residence.baseline <- DiD.data.baseline %>% 
  filter(!is.na(Residence)) %>% 
  group_by(Residence) %>% 
  summarise(mean= mean(MAIndex_pca, na.rm=TRUE), 
            sd = sd(MAIndex_pca, na.rm=TRUE), 
            median = median(MAIndex_pca, na.rm=TRUE),
            n=length(MAIndex_pca), 
            se = sd/sqrt(n)) 

test.val <- round(t.test(DiD.data.baseline$MAIndex_pca~DiD.data.baseline$Residence)$statistic, digits = 3)

Residence.point.plot <- ggplot(Residence.baseline,aes(x=Residence,y=mean, label=paste0("n= ",n))) + 
  geom_point(stat="identity", position = pd, fill='black', size=4) + 
  geom_point(aes(x=Residence,y=median), fill='red', shape=24, size=4) + 
  geom_errorbar(aes(ymin=mean-1.645*se, ymax=mean+1.645*se), width=0.5, position = pd) +
  geom_text(aes(y = mean+1.645*se + 0.5),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + theme(legend.position="none") +
  labs(x="",y="Asset Wealth (PCA)", title="Indigenousness (Below vs. Above MPA residence years' median)", subtitle=paste0("Baseline means difference t-test = ",test.val, " (median=triangle)"))
Residence.point.plot
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Residence_baseIneq",".jpg"))


##----Age_Group
Age_Group.baseline <- DiD.data.baseline %>% 
  group_by(Age_Group) %>% 
  filter(!(Age_Group=="NA")) %>% 
  summarise(mean= mean(MAIndex_pca, na.rm=TRUE), 
            sd = sd(MAIndex_pca, na.rm=TRUE), 
            median = median(MAIndex_pca, na.rm=TRUE),
            n=length(MAIndex_pca), 
            se = sd/sqrt(n)) 

test.val <- round(t.test(DiD.data.baseline$MAIndex_pca~DiD.data.baseline$Age_Group)$statistic, digits = 3)

Age_Group.point.plot <- ggplot(Age_Group.baseline,aes(x=Age_Group,y=mean, label=paste0("n= ",n))) + 
  geom_point(stat="identity", position = pd, fill='black', size=4) + 
  geom_point(aes(x=Age_Group,y=median), fill='red', shape=24, size=4) + 
  geom_errorbar(aes(ymin=mean-1.645*se, ymax=mean+1.645*se), width=0.5, position = pd) +
  geom_text(aes(y = mean+1.645*se + 0.5),position = position_dodge(0.9),vjust = 0, size=4) +
  geom_line( position = pd) + theme_bw() + theme(legend.position="none") +
  labs(x="",y="Asset Wealth (PCA)", title="Age (Old vs. Young)",subtitle=paste0("Baseline means difference t-test = ",test.val, " (median=triangle)")) 
Age_Group.point.plot
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Age_Group_baseIneq",".jpg"))


library(cowplot)
# -- Combine 
plot_grid(Gender.point.plot, Livelihood.point.plot, Ethnicity.point.plot, Residence.point.plot, ncol=2)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_1",".pdf"), width = 12, height = 9)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_1",".png"), width = 12, height = 9)

plot_grid(Gender.point.plot, Livelihood.point.plot, Ethnicity.point.plot, Residence.point.plot, Age_Group.point.plot, ncol=2)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_2",".pdf"), width = 12, height = 12)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_2",".png"), width = 12, height = 12)




#--- Step 1: Baseline Social Inequality -- Vilolin plots (Appendix)
#--- Step 1: Baseline Social Inequality -- Vilolin plots (Appendix)
#--- Step 1: Baseline Social Inequality -- Vilolin plots (Appendix)
#--- Step 1: Baseline Social Inequality -- Vilolin plots (Appendix)

# --Gender  
Gender.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Gender)),aes(x=Gender,y=MAIndex_pca))+
  geom_violin(trim=T) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
  #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
  labs(x="",y="Asset Wealth (PCA)", title="Gender (Male vs. Female)", subtitle=paste0("Baseline distributions"))  +
  ylim(0,400)
Gender.violin
#ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Gender_baseIneq_violin",".jpg"))


# --Livelihood  
Livelihood.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Livelihood)),aes(x=Livelihood,y=MAIndex_pca))+
  geom_violin(trim=T) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
  #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
  labs(x="",y="Asset Wealth (PCA)", title="Livelihood (Fishers vs. other)", subtitle=paste0("Baseline distributions"))  +
  ylim(0,400)
Livelihood.violin
#ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Gender_baseIneq_violin",".jpg"))


# --Ethnicity  
Ethnicity.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Ethnicity)),aes(x=Ethnicity,y=MAIndex_pca))+
  geom_violin(trim=T) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
  #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
  labs(x="",y="Asset Wealth (PCA)", title="Ethnicity (non-Dominant vs. Dominant)", subtitle=paste0("Baseline distributions"))  +
  ylim(0,400)
Ethnicity.violin
#ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Gender_baseIneq_violin",".jpg"))

# --Residence  
Residence.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Residence)),aes(x=Residence,y=MAIndex_pca))+
  geom_violin(trim=T) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
  #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
  labs(x="",y="Asset Wealth (PCA)", title="Residence (Below vs. Above MPA medians)", subtitle=paste0("Baseline distributions"))  +
  ylim(0,400)
Residence.violin
#ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Gender_baseIneq_violin",".jpg"))

# --Gender  
Age_Group.violin <-  ggplot(filter(DiD.data.baseline,!is.na(Age_Group)),aes(x=Age_Group,y=MAIndex_pca))+
  geom_violin(trim=T) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +  theme_classic() +
  #stat_summary(fun.y=mean, geom="point", size=4, color="black") +
  labs(x="",y="Asset Wealth (PCA)", title="Age_Group (Young vs. Old)", subtitle=paste0("Baseline distributions"))  +
  ylim(0,400)
Age_Group.violin
#ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Gender_baseIneq_violin",".jpg"))


library(cowplot)
# -- Combine 
plot_grid(Gender.violin, Livelihood.violin, Ethnicity.violin, Residence.violin, ncol=2)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_1_violin",".pdf"), width = 12, height = 9)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_1_violin",".png"), width = 12, height = 9)

plot_grid(Gender.violin, Livelihood.violin, Ethnicity.violin, Residence.violin, Age_Group.violin, ncol=2)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_2_violin",".pdf"), width = 12, height = 12)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Combine_baseIneq_2_violin",".png"), width = 12, height = 12)



#--- Step 2: Reduced-form impact plots (seperate MPA impacts for each subgroup)
#--- Step 2: Reduced-form impact plots (seperate MPA impacts for each subgroup)
#--- Step 2: Reduced-form impact plots (seperate MPA impacts for each subgroup)
#--- Step 2: Reduced-form impact plots (seperate MPA impacts for each subgroup)
summary(DiD.data)
describe(DiD.data)

## -- redefining dummy indicators for the disadvantage class (in preparation for the triple-dif DiD regs)
DiD.data.median <- DiD.data %>% 
  filter(MPAID%in%c(1:6,15:18)) %>% 
  group_by(MPAID, yearsPost) %>% 
  summarise(YrResident.median = median(YrResident, na.rm=TRUE), 
            Age.median = median(HHH.age, na.rm=TRUE),
            MAIndex_GINI_Settl.med = median(MAIndex_GINI_Settl, na.rm=TRUE))

DiD.data<- DiD.data %>% 
  left_join(DiD.data.median, by=c("MPAID", "yearsPost")) %>% 
  mutate(Fisher = as.double(Fisher),
         Female = ifelse(Male==1,0,1),
         Ethnic = ifelse(ethDom.highest.MT==1,1,0),
         Indigenous = ifelse(YrResident<=YrResident.median,1,0)) 


##------##
## Generating Participation indicators for subgroups 
DiD.data <- DiD.data %>% 
  mutate(NumTotalGroup = NumMarineGroup + NumOtherGroup,
         NumTotalGroup.Female = ifelse(Female==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.Fisher = ifelse(Fisher==1, NumMarineGroup + NumOtherGroup, NA),
         NumTotalGroup.Ethnic = ifelse(Ethnic==1, NumMarineGroup + NumOtherGroup, NA), 
         NumTotalGroup.Indigenous = ifelse(Indigenous==1, NumMarineGroup + NumOtherGroup, NA),
         Parti.Female.dum = ifelse(Female==1 & NumTotalGroup>0, 1, 
                                   ifelse(Female==1 & NumTotalGroup==0, 0, NA)),
         Parti.Fisher.dum = ifelse(Fisher==1 & NumTotalGroup>0, 1,
                                   ifelse(Fisher==1 & NumTotalGroup==0, 0, NA)),
         Parti.Ethnic.dum = ifelse(Ethnic==1 & NumTotalGroup>0, 1,
                                   ifelse(Ethnic==1 & NumTotalGroup==0, 0, NA)),
         Parti.Indigenous.dum = ifelse(Indigenous==1 & NumTotalGroup>0, 1,
                                       ifelse(Indigenous==1 & NumTotalGroup==0, 0, NA)))



##-----##


## important: get the intensive (# of groups) and extensive (whether 0/1) measure of subgroup participations 
DiD.data.settl <- DiD.data %>% 
  select(MPAID, SettlementID, yearsPost, MAIndex_GINI_Settl, TimeMarket, NumTotalGroup, NumTotalGroup.Ethnic, NumTotalGroup.Fisher, NumTotalGroup.Female, NumTotalGroup.Indigenous,
         Parti.Female.dum, Parti.Ethnic.dum, Parti.Fisher.dum, Parti.Indigenous.dum) %>% 
  group_by(SettlementID, yearsPost) %>% 
  summarise(MPAID = first(MPAID),
            MAIndex_GINI_Settl.m = mean(MAIndex_GINI_Settl), 
            TimeMarket_settl.m = mean(TimeMarket),
            Parti_settl.m = mean(NumTotalGroup),
            Parti_settl.Female.m = mean(NumTotalGroup.Female, na.rm = TRUE),
            Parti_settl.Ethnic.m = mean(NumTotalGroup.Ethnic, na.rm = TRUE),
            Parti_settl.Fisher.m = mean(NumTotalGroup.Fisher, na.rm = TRUE),
            Parti_settl.Indigenous.m = mean(NumTotalGroup.Indigenous, na.rm = TRUE),
            Parti_settl.Female.pct = mean(Parti.Female.dum, na.rm = TRUE),
            Parti_settl.Ethnic.pct = mean(Parti.Ethnic.dum, na.rm = TRUE),
            Parti_settl.Fisher.pct = mean(Parti.Fisher.dum, na.rm = TRUE),
            Parti_settl.Indigenous.pct = mean(Parti.Indigenous.dum, na.rm = TRUE))

## -- Replace all participation measures with NaN as 0 (NaN is because that settlement doesnt have any Female --> level of female participation can be thought of as 0)
DiD.data.settl <- DiD.data.settl %>% 
  mutate(Parti_settl.Female.m = ifelse(is.nan(Parti_settl.Female.m), 0, Parti_settl.Female.m), 
         Parti_settl.Female.pct = ifelse(is.nan(Parti_settl.Female.pct), 0, Parti_settl.Female.pct), 
         Parti_settl.Fisher.m = ifelse(is.nan(Parti_settl.Fisher.m), 0, Parti_settl.Fisher.m), 
         Parti_settl.Fisher.pct = ifelse(is.nan(Parti_settl.Fisher.pct), 0, Parti_settl.Fisher.pct), 
         Parti_settl.Ethnic.m = ifelse(is.nan(Parti_settl.Ethnic.m), 0, Parti_settl.Ethnic.m), 
         Parti_settl.Ethnic.pct = ifelse(is.nan(Parti_settl.Ethnic.pct), 0, Parti_settl.Ethnic.pct), 
         Parti_settl.Indigenous.m = ifelse(is.nan(Parti_settl.Indigenous.m), 0, Parti_settl.Indigenous.m), 
         Parti_settl.Indigenous.pct = ifelse(is.nan(Parti_settl.Indigenous.pct), 0, Parti_settl.Indigenous.pct))

## -- now get mean/median value for each MPA (i.e. to split settlements by participation values later on)
DiD.data.MPA <- DiD.data.settl %>% 
  group_by(MPAID, yearsPost) %>% 
  summarise(MAIndex_GINI_MPA.m = mean(MAIndex_GINI_Settl.m), 
            TimeMarket_MPA.m = mean(TimeMarket_settl.m),
            Parti_MPA.m = mean(Parti_settl.m),
            Parti_MPA.Female.m = mean(Parti_settl.Female.m),
            Parti_MPA.Ethnic.m = mean(Parti_settl.Ethnic.m),
            Parti_MPA.Fisher.m = mean(Parti_settl.Fisher.m),
            Parti_MPA.Indigenous.m = mean(Parti_settl.Indigenous.m),
            Parti_MPA.Female.pct = mean(Parti_settl.Female.pct),
            Parti_MPA.Ethnic.pct = mean(Parti_settl.Ethnic.pct),
            Parti_MPA.Fisher.pct = mean(Parti_settl.Fisher.pct),
            Parti_MPA.Indigenous.pct = mean(Parti_settl.Indigenous.pct))

DiD.data <- DiD.data %>% 
  left_join(DiD.data.MPA, by=c("MPAID", "yearsPost"))

DiD.data <- DiD.data %>% 
  left_join(DiD.data.settl,by=c("SettlementID", "yearsPost"))

DiD.data <- DiD.data %>% 
  mutate(MPAID = MPAID.x) %>% 
  select(-c("MPAID.x", "MPAID.y"))

## -- now do the spliting by moderating factors--paying particular focus on participation (Settlement Above or Below )
DiD.data <- DiD.data %>% 
  mutate(mod.GINI.above = ifelse(MAIndex_GINI_Settl.m>=MAIndex_GINI_MPA.m,1,0),
         mod.TimeMarket.above = ifelse(TimeMarket_settl.m>=TimeMarket_MPA.m,1,0),
         mod.Parti.above = ifelse(Parti_settl.m>=Parti_MPA.m,1,0),
         mod.Parti.Female.m.above = ifelse(Parti_settl.Female.m>=Parti_MPA.Female.m,1,0),
         mod.Parti.Fisher.m.above = ifelse(Parti_settl.Fisher.m>=Parti_MPA.Fisher.m,1,0),
         mod.Parti.Ethnic.m.above = ifelse(Parti_settl.Ethnic.m>=Parti_MPA.Ethnic.m,1,0),
         mod.Parti.Indigenous.m.above = ifelse(Parti_settl.Indigenous.m>=Parti_MPA.Indigenous.m,1,0),
         mod.Parti.Female.pct.above = ifelse(Parti_settl.Female.pct>=Parti_MPA.Female.pct,1,0),
         mod.Parti.Fisher.pct.above = ifelse(Parti_settl.Fisher.pct>=Parti_MPA.Fisher.pct,1,0),
         mod.Parti.Ethnic.pct.above = ifelse(Parti_settl.Ethnic.pct>=Parti_MPA.Ethnic.pct,1,0),
         mod.Parti.Indigenous.pct.above = ifelse(Parti_settl.Indigenous.pct>=Parti_MPA.Indigenous.pct,1,0))


DiD.data<- DiD.data %>% 
  mutate(Post = ifelse(yearsPost==0,0,1),
         Gender = ifelse(Male==1,"Male","Female"),
         Fisher = as.factor(Fisher),
         Fisher_3 = as.factor(Fisher_3), 
         Livelihood = ifelse(Fisher==1,"Fisher","Other"), 
         Ethnicity = ifelse(ethDom.highest.MT==1,"Dominant", " Non-dominant"),
         Residence = ifelse(YrResident<=YrResident.median,"Below MPA Median", "Above MPA Median"),
         Age_Group = ifelse(IndividualAge_raw<=Age.median,"Young", "Old"))


##-------Correlation Matrix for the 4 subGroups ---##
##-------Correlation Matrix for the 4 subGroups ---##
#install.packages("Hmisc")
#install.packages("corrplot")

corr.subGroups.baseline.data <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  select(Female, Fisher, Ethnic, Indigenous)

corr.subGroups.baseline.data.BHS <- DiD.data %>% 
  filter(yearsPost==0, MPAID%in%c(1:6)) %>% 
  select(Female, Fisher, Ethnic, Indigenous)

corr.subGroups.baseline.data.SBS <- DiD.data %>% 
  filter(yearsPost==0, MPAID%in%c(15:18)) %>% 
  select(Female, Fisher, Ethnic, Indigenous)


library("Hmisc")
correlation.subGroups <- rcorr(as.matrix(corr.subGroups.baseline.data))
correlation.subGroups

corr.subGroups.baseline.BHS <- rcorr(as.matrix(corr.subGroups.baseline.data.BHS))
corr.subGroups.baseline.BHS

corr.subGroups.baseline.SBS <- rcorr(as.matrix(corr.subGroups.baseline.data.SBS))
corr.subGroups.baseline.SBS

library(corrplot)
corrplot(correlation.subGroups$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 0)


## --Step 1: Producing regression tables (showing all 4 subgroup results)
## --Step 1: Producing regression tables (showing all 4 subgroup results)
## --Step 1: Producing regression tables (showing all 4 subgroup results)
## -- Triple-Dif aggregate impacts
### -- Triple-Dif aggregate impacts
### -- Triple-Dif aggregate impacts
### -- Triple-Dif aggregate impacts
varNames <- c("MAIndex_pca", "MAIndex_pca.1")

## -- Variable: MAIndex_pca
regValue.gender <- felm(MAIndex_pca  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw 
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)

regValue.Fisher <- felm(MAIndex_pca  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)

regValue.ethnic <- felm(MAIndex_pca  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)

regValue.indigenous <- felm(MAIndex_pca  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                              Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                            | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)


stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups.html", type = "html",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          #order = c(5, 6, 1, 2, 3, 4),
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))



stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups.txt", type = "text",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          #order = c(5, 6, 1, 2, 3, 4),
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))




## -- Variable: MAIndex_pca.1
regValue.gender <- felm(MAIndex_pca.1  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw 
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)

regValue.Fisher <- felm(MAIndex_pca.1  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)

regValue.ethnic <- felm(MAIndex_pca.1  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)

regValue.indigenous <- felm(MAIndex_pca.1  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                              Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                            | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)


stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups-noBoat.html", type = "html",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA) [not including Boat]",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))



stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups-noBoat.txt", type = "text",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA) [not including Boat]",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))




## -------------------------------------------------------------------------------------------------- ##
## -- BHS results only
## -- BHS results only
## -- BHS results only

DiD.data.BHS <- DiD.data %>% 
  filter(MPAID%in%(1:6))

## -- Variable: MAIndex_pca
regValue.gender <- felm(MAIndex_pca  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw 
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS, exactDOF = TRUE)

regValue.Fisher <- felm(MAIndex_pca  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS, exactDOF = TRUE)

regValue.ethnic <- felm(MAIndex_pca  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS, exactDOF = TRUE)

regValue.indigenous <- felm(MAIndex_pca  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS, exactDOF = TRUE)


stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups-BHS.html", type = "html",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes", 4)), c("MPA x Year FE",rep("Yes", 4))))



stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups-BHS.txt", type = "text",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))


## BHS; separately for t2 and t4
## BHS; separately for t2 and t4
## BHS; separately for t2 and t4

regValue.gender <- felm(MAIndex_pca  ~ Treatment + yearsPostF + Treatment:yearsPostF + Female:Treatment + Female:yearsPostF + Female:Treatment:yearsPostF +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS,exactDOF = TRUE)

regValue.Fisher <- felm(MAIndex_pca  ~ Treatment + yearsPostF + Treatment:yearsPostF + Fisher:Treatment + Fisher:yearsPostF + Fisher:Treatment:yearsPostF +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS,exactDOF = TRUE)

regValue.ethnic <- felm(MAIndex_pca  ~ Treatment + yearsPostF  + Treatment:yearsPostF + Ethnic:Treatment + Ethnic:yearsPostF + Ethnic:Treatment:yearsPostF +
                          Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                        | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS,exactDOF = TRUE)

regValue.indigenous <- felm(MAIndex_pca  ~ Treatment + yearsPostF + Treatment:yearsPostF + Indigenous:Treatment + Indigenous:yearsPostF + Indigenous:Treatment:yearsPostF +
                              Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw 
                            | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.BHS,exactDOF = TRUE)


stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups-BHS-t2t4.html", type = "html",
          # keep = c("Treatment:yearsPostF2",	"Treatment:yearsPostF4", "Treatment:yearsPostF2:Female","Treatment:yearsPostF4:Female", 
          #          "Treatment:yearsPostF2:Fisher","Treatment:yearsPostF4:Fisher", 
          #          "Treatment:yearsPostF2:ethnic","Treatment:yearsPostF4:ethnic", 
          #          "Treatment:yearsPostF2:indigenous","Treatment:yearsPostF4:indigenous", 
          #          "n.child","ed.level","IndividualAge_raw", "IndividualGender"), 
          covariate.labels=c("Treatment", "Year 2", "Year 4", "Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),         
          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))


stargazer(regValue.gender, regValue.Fisher, regValue.ethnic, regValue.indigenous,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Triple-Dif-subGroups-BHS-t2t4.txt", type = "text",
          #keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:non_ethDom","Treatment:Post:indigenous", "n.child","ed.level","IndividualAge_raw", "IndividualGender"), 
          covariate.labels=c("Treatment", "Year 2", "Year 4", "Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),         
          title="Differential Impacts by Social Group (Triple-difference DiD)",
          align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",4)), c("MPA x Year FE",rep("Yes",4))))

## --------------------------------------------------------------------------------------------------------- ##
## --Step 2: Producing impact plots 
## --Step 2: Producing impact plots 
## --Step 2: Producing impact plots 

varNames <- c("MAIndex_pca", "MAIndex_pca.1", "FSIndex", "PAIndex", "MTIndex")

## -- Gender
model.out.gender<- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[[i]]
  
  regValue <- felm(Y ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                     Female + Fisher + Ethnic + Indigenous + 
                     n.child + ed.level + YrResident + IndividualAge_raw   
                   | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  
  ###Get covariance value between Treatment:Post and Female:Treatment:Post (need this for calculating S.E. of Female impact (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue)
  vcov <- vcov.matrix["Treatment:Post","Treatment:Post:Female"]
  
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i,vcov_alpha2_3=vcov)
  model.out.gender <- rbind(model.out.gender,reg.broom)
}


## -- Fisher
model.out.Fisher<- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[[i]]
  regValue <- felm(Y  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                     Female + Fisher + Ethnic + Indigenous + 
                     n.child + ed.level + YrResident + IndividualAge_raw   
                   | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ###Get covariance value between Treatment:Post and Female:Treatment:Post (need this for calculating S.E. of Female impact (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue)
  vcov <- vcov.matrix["Treatment:Post","Treatment:Post:Fisher1"]
  
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i,vcov_alpha2_3=vcov)
  model.out.Fisher <- rbind(model.out.Fisher,reg.broom)
}

## -- Ethnicity
model.out.ethnic<- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[[i]]
  regValue <- felm(Y  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                     Female + Fisher + Ethnic + Indigenous + 
                     n.child + ed.level + YrResident + IndividualAge_raw   
                   | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ###Get covariance value between Treatment:Post and Female:Treatment:Post (need this for calculating S.E. of Female impact (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue)
  vcov <- vcov.matrix["Treatment:Post","Treatment:Post:Ethnic"]
  
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i,vcov_alpha2_3=vcov)
  model.out.ethnic <- rbind(model.out.ethnic,reg.broom)
}

## -- indigenous 
model.out.indigenous<- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[[i]]
  regValue <- felm(Y  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                     Female + Fisher + Ethnic + Indigenous + 
                     n.child + ed.level + YrResident + IndividualAge_raw   
                   | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  ###Get covariance value between Treatment:Post and Female:Treatment:Post (need this for calculating S.E. of Female impact (alpha2 + alpha3))
  vcov.matrix<-vcov(regValue)
  vcov <- vcov.matrix["Treatment:Post","Treatment:Post:Indigenous"]
  
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i,vcov_alpha2_3=vcov)
  model.out.indigenous <- rbind(model.out.indigenous,reg.broom)
}


## --- Draw impact plots --- ##
## --- Draw impact plots --- ##
## --- Draw impact plots --- ##

##---Gender---##
model.out.gender1 <- model.out.gender %>% 
  filter(term%in%c("Treatment:Post", "Treatment:Post:Female")) %>%  
  mutate(term=gsub("Treatment:Post:Female","Female_Differential",term))  %>% 
  mutate(term=gsub("Treatment:Post","Male_Impact",term))

## spead dataframe to compute female impact
model.out.gender1.esti.spread <-model.out.gender1 %>% 
  select(Response,term,estimate,vcov_alpha2_3) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = Male_Impact + Female_Differential) %>% 
  select(Response, estimate) %>% 
  mutate(term="Female_Impact",statistic=0,p.value=0,vcov_alpha2_3=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out.gender1.se.spread<-model.out.gender1 %>% 
  select(Response,term,std.error,vcov_alpha2_3) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(Male_Impact^2 + Female_Differential^2 + 2*vcov_alpha2_3))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out.gender1.esti.spread<-cbind(model.out.gender1.esti.spread, model.out.gender1.se.spread) 

model.out.gender1<-rbind(model.out.gender1, model.out.gender1.esti.spread)

model.out.gender1 <- model.out.gender1 %>% 
  mutate(Group=ifelse(term=="Male_Impact"," Male Impact",
                      ifelse(term=="Female_Impact","  Female Impact","Gender Differential"))) 



##---Fisher---##
model.out.Fisher1 <- model.out.Fisher %>% 
  filter(term%in%c("Treatment:Post", "Treatment:Post:Fisher1")) %>%  
  mutate(term=gsub("Treatment:Post:Fisher1","Fisher_Differential",term))  %>% 
  mutate(term=gsub("Treatment:Post","nonFisher_Impact",term))

## spead dataframe to compute female impact
model.out.Fisher1.esti.spread <-model.out.Fisher1 %>% 
  select(Response,term,estimate,vcov_alpha2_3) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = nonFisher_Impact + Fisher_Differential) %>% 
  select(Response, estimate) %>% 
  mutate(term="Fisher_Impact",statistic=0,p.value=0,vcov_alpha2_3=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out.Fisher1.se.spread<-model.out.Fisher1 %>% 
  select(Response,term,std.error,vcov_alpha2_3) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(nonFisher_Impact^2 + Fisher_Differential^2 + 2*vcov_alpha2_3))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out.Fisher1.esti.spread<-cbind(model.out.Fisher1.esti.spread, model.out.Fisher1.se.spread) 

model.out.Fisher1<-rbind(model.out.Fisher1, model.out.Fisher1.esti.spread)

model.out.Fisher1 <- model.out.Fisher1 %>% 
  mutate(Group=ifelse(term=="nonFisher_Impact"," nonFisher Impact",
                      ifelse(term=="Fisher_Impact"," Fisher Impact","Livelihood Differential"))) 



##---Ethnic---##
model.out.ethnic1 <- model.out.ethnic %>% 
  filter(term%in%c("Treatment:Post", "Treatment:Post:Ethnic")) %>%  
  mutate(term=gsub("Treatment:Post:Ethnic","Ethnic_Differential",term))  %>% 
  mutate(term=gsub("Treatment:Post","dominant_Ethnic_Impact",term))

## spead dataframe to compute female impact
model.out.ethnic1.esti.spread <-model.out.ethnic1 %>% 
  select(Response,term,estimate,vcov_alpha2_3) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = dominant_Ethnic_Impact + Ethnic_Differential) %>% 
  select(Response, estimate) %>% 
  mutate(term="nonDominant_Ethnic_Impact",statistic=0,p.value=0,vcov_alpha2_3=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out.ethnic1.se.spread<-model.out.ethnic1 %>% 
  select(Response,term,std.error,vcov_alpha2_3) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(dominant_Ethnic_Impact^2 + Ethnic_Differential^2 + 2*vcov_alpha2_3))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out.ethnic1.esti.spread<-cbind(model.out.ethnic1.esti.spread, model.out.ethnic1.se.spread) 

model.out.ethnic1<-rbind(model.out.ethnic1, model.out.ethnic1.esti.spread)

model.out.ethnic1 <- model.out.ethnic1 %>% 
  mutate(Group=ifelse(term=="dominant_Ethnic_Impact","dominant Ethnic Impact",
                      ifelse(term=="nonDominant_Ethnic_Impact"," nonDominant Ethnic Impact","Ethnic Differential"))) 


##---Indigenous---##
model.out.indigenous1 <- model.out.indigenous %>% 
  filter(term%in%c("Treatment:Post", "Treatment:Post:Indigenous")) %>%  
  mutate(term=gsub("Treatment:Post:Indigenous","Indigenous_Differential",term))  %>% 
  mutate(term=gsub("Treatment:Post","Below_MPA_median",term))

## spead dataframe to compute female impact
model.out.indigenous1.esti.spread <-model.out.indigenous1 %>% 
  select(Response,term,estimate,vcov_alpha2_3) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = Below_MPA_median + Indigenous_Differential) %>% 
  select(Response, estimate) %>% 
  mutate(term="Above_MPA_median",statistic=0,p.value=0,vcov_alpha2_3=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out.indigenous1.se.spread<-model.out.indigenous1 %>% 
  select(Response,term,std.error,vcov_alpha2_3) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(Below_MPA_median^2 + Indigenous_Differential^2 + 2*vcov_alpha2_3))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out.indigenous1.esti.spread<-cbind(model.out.indigenous1.esti.spread, model.out.indigenous1.se.spread) 

model.out.indigenous1<-rbind(model.out.indigenous1, model.out.indigenous1.esti.spread)

model.out.indigenous1 <- model.out.indigenous1 %>% 
  mutate(Group=ifelse(term=="Above_MPA_median","Above MPA median",
                      ifelse(term=="Below_MPA_median","Below MPA median","Indigenousness Differential"))) 


##----------Entire Sample plots-------------------##
##----------Entire Sample plots-------------------##
##----------Entire Sample plots-------------------##
pd <- position_dodge() # move them .05 to the left and right


MAIndex_pca.Gender.plot <- ggplot(filter(model.out.gender1,Response=="MAIndex_pca"),aes(x=Group,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill="darkgray", width = 0.6)+ 
  theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Gender: Male vs. Female")  
ggsave("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Impact_plots/Gender-MAIndex_pca.png",width = 12, height = 6)


MAIndex_pca.Livelihood.plot <- ggplot(filter(model.out.Fisher1,Response=="MAIndex_pca"),aes(x=Group,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill="darkgray", width = 0.6)+ 
  theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.4, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Livelihood: Fisher vs. non-Fishers")  
ggsave("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Impact_plots/Fisher-MAIndex_pca.png",width = 12, height = 6)


MAIndex_pca.Ethnic.plot <- ggplot(filter(model.out.ethnic1,Response=="MAIndex_pca"),aes(x=Group,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill="darkgray", width = 0.6)+ 
  theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.4, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Ethnicity: Non-dominant vs. Dominant Ethnics")  
ggsave("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Impact_plots/Ethnic-MAIndex_pca.png",width = 12, height = 6)


MAIndex_pca.Indigenous.plot <- ggplot(filter(model.out.indigenous1,Response=="MAIndex_pca"),aes(x=Group,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill="darkgray", width = 0.6)+ 
  theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.4, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Indigenousness: Above vs. Below MPA's Median Length")  
ggsave("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Impact_plots/Indigenous-MAIndex_pca.png",width = 12, height = 6)

#--Combine
plot_grid(MAIndex_pca.Gender.plot, MAIndex_pca.Livelihood.plot, MAIndex_pca.Ethnic.plot, MAIndex_pca.Indigenous.plot, ncol=2)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Impact_plots/Combine-MAIndex_pca",".pdf"), width = 12, height = 9)
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/plots/Impact_plots/Combine-MAIndex_pca",".png"), width = 12, height = 9)


## -------------moderators
## -------------moderators
## -------------moderators
DiD.data.PartiAbove <- DiD.data %>% filter(mod.Parti.above == 1)
DiD.data.PartiBelow <- DiD.data %>% filter(mod.Parti.above == 0)

DiD.data.GINIAbove <- DiD.data %>% filter(mod.GINI.above == 1)
DiD.data.GINIBelow <- DiD.data %>% filter(mod.GINI.above == 0)

##-- intensive margin of participation by each social group (i.e., spliting by median #groups participated)
DiD.data.Female.m.PartiAbove <- DiD.data %>% filter(mod.Parti.Female.m.above == 1)
DiD.data.Female.m.PartiBelow <- DiD.data %>% filter(mod.Parti.Female.m.above == 0)

DiD.data.Fisher.m.PartiAbove <- DiD.data %>% filter(mod.Parti.Fisher.m.above == 1)
DiD.data.Fisher.m.PartiBelow <- DiD.data %>% filter(mod.Parti.Fisher.m.above == 0)

DiD.data.Ethnic.m.PartiAbove <- DiD.data %>% filter(mod.Parti.Ethnic.m.above == 1)
DiD.data.Ethnic.m.PartiBelow <- DiD.data %>% filter(mod.Parti.Ethnic.m.above == 0)

DiD.data.Indigenous.m.PartiAbove <- DiD.data %>% filter(mod.Parti.Indigenous.m.above == 1)
DiD.data.Indigenous.m.PartiBelow <- DiD.data %>% filter(mod.Parti.Indigenous.m.above == 0)

##-- extensive margin of participation by each social group (i.e., spliting by median of likelihood(%) that they participated)
DiD.data.Female.pct.PartiAbove <- DiD.data %>% filter(mod.Parti.Female.pct.above == 1)
DiD.data.Female.pct.PartiBelow <- DiD.data %>% filter(mod.Parti.Female.pct.above == 0)

DiD.data.Fisher.pct.PartiAbove <- DiD.data %>% filter(mod.Parti.Fisher.pct.above == 1)
DiD.data.Fisher.pct.PartiBelow <- DiD.data %>% filter(mod.Parti.Fisher.pct.above == 0)

DiD.data.Ethnic.pct.PartiAbove <- DiD.data %>% filter(mod.Parti.Ethnic.pct.above == 1)
DiD.data.Ethnic.pct.PartiBelow <- DiD.data %>% filter(mod.Parti.Ethnic.pct.above == 0)

DiD.data.Indigenous.pct.PartiAbove <- DiD.data %>% filter(mod.Parti.Indigenous.pct.above == 1)
DiD.data.Indigenous.pct.PartiBelow <- DiD.data %>% filter(mod.Parti.Indigenous.pct.above == 0)

## --mechanism: intensive participation of social groups
regValue.gender1 <- felm(MAIndex_pca  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Female.m.PartiAbove,exactDOF = TRUE)

regValue.Fisher1 <- felm(MAIndex_pca  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Fisher.m.PartiAbove,exactDOF = TRUE)

regValue.ethnic1 <- felm(MAIndex_pca  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Ethnic.m.PartiAbove,exactDOF = TRUE)

regValue.indigenous1 <- felm(MAIndex_pca  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                               Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                             | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Indigenous.m.PartiAbove,exactDOF = TRUE)
##-----------------------------------------##
regValue.gender2 <- felm(MAIndex_pca  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Female.m.PartiBelow,exactDOF = TRUE)

regValue.Fisher2 <- felm(MAIndex_pca  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Fisher.m.PartiBelow,exactDOF = TRUE)

regValue.ethnic2 <- felm(MAIndex_pca  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Ethnic.m.PartiBelow,exactDOF = TRUE)

regValue.indigenous2 <- felm(MAIndex_pca  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                               Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                             | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Indigenous.m.PartiBelow,exactDOF = TRUE)


stargazer(regValue.gender1, regValue.Fisher1, regValue.ethnic1, regValue.indigenous1,regValue.gender2, regValue.Fisher2, regValue.ethnic2, regValue.indigenous2,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Moderators/Triple-Dif-subGroups-PartiSpecific-m.html", type = "html",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),         
          title="Mechanism: Baseline Participation Intensity (by each group)", align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",8)), c("MPA x Year FE",rep("Yes",8)), c("Sample",rep("Above Median",4),rep("Below Median",4))))

stargazer(regValue.gender1, regValue.Fisher1, regValue.ethnic1, regValue.indigenous1,regValue.gender2, regValue.Fisher2, regValue.ethnic2, regValue.indigenous2,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Moderators/Triple-Dif-subGroups-PartiSpecific-m.txt", type = "text",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),         
          title="Mechanism: Baseline Participation Intensity (by each group)", align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",8)), c("MPA x Year FE",rep("Yes",8)), c("Sample",rep("Above Median",4),rep("Below Median",4))))



##-------------------------------------------------------------------------------------------------#


## -- all BHS and SBS -- mechanism: extensive participation of social groups
regValue.gender1 <- felm(FSIndex  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Female.pct.PartiAbove,exactDOF = TRUE)

regValue.Fisher1 <- felm(FSIndex  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Fisher.pct.PartiAbove,exactDOF = TRUE)

regValue.ethnic1 <- felm(FSIndex  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Ethnic.pct.PartiAbove,exactDOF = TRUE)

regValue.indigenous1 <- felm(FSIndex  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                               Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                             | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Indigenous.pct.PartiAbove,exactDOF = TRUE)
##-----------------------------------------##
regValue.gender2 <- felm(FSIndex  ~ Treatment + Post + Treatment:Post + Female:Treatment + Female:Post + Female:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw   
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Female.pct.PartiBelow,exactDOF = TRUE)

regValue.Fisher2 <- felm(FSIndex  ~ Treatment + Post + Fisher + Treatment:Post + Fisher:Treatment + Fisher:Post + Fisher:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Fisher.pct.PartiBelow,exactDOF = TRUE)

regValue.ethnic2 <- felm(FSIndex  ~ Treatment + Post + Ethnic + Treatment:Post + Ethnic:Treatment + Ethnic:Post + Ethnic:Treatment:Post +
                           Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                         | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Ethnic.pct.PartiBelow,exactDOF = TRUE)

regValue.indigenous2 <- felm(FSIndex  ~ Treatment + Post + Indigenous + Treatment:Post + Indigenous:Treatment + Indigenous:Post + Indigenous:Treatment:Post +
                               Female + Fisher + Ethnic + Indigenous + n.child + ed.level + YrResident + IndividualAge_raw  
                             | SettlementID + MPAID:InterviewYear | 0 | SettlementID, data=DiD.data.Indigenous.pct.PartiBelow,exactDOF = TRUE)


stargazer(regValue.gender1, regValue.Fisher1, regValue.ethnic1, regValue.indigenous1,regValue.gender2, regValue.Fisher2, regValue.ethnic2, regValue.indigenous2,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Moderators/Triple-Dif-subGroups-PartiSpecific-pct.html", type = "html",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),         
          title="Mechanism: Baseline Participation Likelihood (by each group)", align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",8)), c("MPA x Year FE",rep("Yes",8)), c("Sample",rep("Above Median",4),rep("Below Median",4))))

stargazer(regValue.gender1, regValue.Fisher1, regValue.ethnic1, regValue.indigenous1,regValue.gender2, regValue.Fisher2, regValue.ethnic2, regValue.indigenous2,
          out = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/JUST BHS/results/tables/Moderators/Triple-Dif-subGroups-PartiSpecific-pct.txt", type = "text",
          keep = c("Treatment:Post",	"Treatment:Post:Female","Treatment:Post:Fisher","Treatment:Post:Ethnic","Treatment:Post:Indigenous", 
                   "\\bFemale\\b", "\\bFisher1\\b", "\\bEthnic\\b", "\\bIndigenous\\b",  "n.child", "ed.level", "YrResident", "IndividualAge_raw"), 
          covariate.labels=c("Female", "Fisher", "Ethnic", "Indigenous",  "Number of Children", "Education Level", "Years of Residence", "Household Head's Age"),         
          title="Mechanism: Baseline Participation Likelihood (by each group)", align=TRUE, dep.var.labels="Material-Asset Wealth Index (PCA)",
          add.lines = list(c("Settlement FE",rep("Yes",8)), c("MPA x Year FE",rep("Yes",8)), c("Sample",rep("Above Median",4),rep("Below Median",4))))





























