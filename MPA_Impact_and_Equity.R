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


source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")

library(lfe) #regressions
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)
library(tidyverse)
library(qvalue)
library(psych) #summary statistics by groups
library(factoextra)

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
  summarise(NumMarineGroup=length(HouseholdID),
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

# --- Filter to look at 6 BHS and 2 SBS MPAs (with t2 and/or t4 data) --- Oct 2019
DiD.data <- DiD.data %>% 
  filter(MPAID==1|MPAID==2|MPAID==3|MPAID==4|MPAID==5|MPAID==6|MPAID==15|MPAID==16)

summary(DiD.data)
# calculate Z scores (standardized values for each of the Big Five)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, CarTruck:Generator), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}

# calculate p scores
pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  

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
asset.pca <- prcomp(asset.frame[16:24], scale = FALSE)

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
asset.weight.frame <- get_pca_var(asset.pca) 
asset.weight.frame <- asset.weight.frame$contrib 
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

#Reconstructing the new MA_index using the above PCA-derived weights
summary(DiD.data)
DiD.data <- DiD.data %>% 
  mutate(Boat=BoatNoMotor+BoatOutboard+BoatInboard) %>% 
  mutate(MAIndex_pca = CarTruck*CarTruck_z_w + Bicycle*Bicycle_z_w + Motorcycle*Motorcycle_z_w + Boat*Boat_z_w +PhoneCombined*PhoneCombined_z_w +TV*TV_z_w + Entertain*Entertain_z_w + Satellite*Satellite_z_w + Generator*Generator_z_w)
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
         Fisher=ifelse(PrimaryLivelihood==1,1,0)) 
DiD.data <- DiD.data %>% 
  mutate(Fisher=ifelse(is.na(Fisher),0,Fisher))

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

##---END---##
##---END---##
##---END---##




##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---6. Export data frame to Excel for exploratory Stata analysis

#Export to txt file
library(foreign)
#install.packages("writexl")
library(writexl)
write.table(DiD.data, "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/MPA_HouseholdDemo_data.txt", sep="\t")
write_xlsx(DiD.data, path = "D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/MPA_HouseholdDemo_data", col_names = TRUE, format_headers = TRUE)

##---END---##
##---END---##
##---END---##


##---BEGIN---##
##---BEGIN---##
##---BEGIN---##
##---7. Evidence for baseline inequality in asset wealth (MAIndex_pca)
describe(DiD.data)

#Define 5 subgroups
DiD.data <- DiD.data %>% 
  mutate(Fisher_3 = ifelse(PrimaryLivelihood==1 | SecondaryLivelihood==1 | TertiaryLivelihood==1,1,0))

DiD.data.baseline <- DiD.data %>% 
  filter(yearsPost==0) %>% 
  mutate(Male = as.factor(Male),
         Fisher = as.factor(Fisher),
         Fisher_3 = as.factor(Fisher_3))  
  


#show basic boxplot descriptive graphics
boxplot.Gender <-boxplot(MAIndex_pca~Male,data=DiD.data.baseline, main="Material Asset Ownership by Social Groups",
        xlab="Gender", ylab="Material Asset", names=c("Female", "Male")) 

#show basic boxplot descriptive graphics
boxplot.Fisher <- boxplot(MAIndex_pca~Fisher_3,data=DiD.data.baseline, main="Material Asset Ownership by Social Groups",
        xlab="Fishery Livelihood", ylab="Material Asset", names=c("non-Fisher", "Fisher")) 


vilolin.Gender <- ggplot(DiD.data.baseline, aes(x=Male, y=MAIndex_pca, fill=Male)) + 
  geom_violin(trim=TRUE) +  theme_classic() +
  geom_boxplot(width=0.05, fill="white") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) 

vilolin.Fisher <- ggplot(DiD.data.baseline, aes(x=as.factor(Fisher), y=MAIndex_pca, fill=Fisher)) + 
  geom_violin(trim=TRUE) +  theme_classic() +
  geom_boxplot(width=0.05, fill="white") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) 

comb.plot <-  plot_grid(vilolin.Gender,vilolin.Fisher ,ncol=2)

#install.packages("devtools")
# devtools::install_github("cardiomoon/webr")
# 
# meanDif.test = t.test(MAIndex_pca~Male, data=DiD.data.baseline, conf.level=0.99, alternative="greater", var.equal=TRUE)
# plot(meanDif.test)

########################Combine all boxplots
ggsave(paste0("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/stata/result/graph/baseline_inequality_boxplot/",mpa,"_BigFive.png"),width = 12, height = 6)


#show the raw data as well added to the boxplot
#stripchart(MAIndex_pca~Male,data=DiD.data.baseline,method="jitter",jitter=.0005,vertical=T,add=T)  
 
multi.hist(DiD.data$MAIndex_pca) 

#2-sample t-test 
with(DiD.data,t.test(MAIndex_pca~Male))


