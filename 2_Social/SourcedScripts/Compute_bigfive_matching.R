# 
# code: Compute 'BIG FIVE' human well-being outcomes
# 
# author: Louise Glew, louise.glew@gmail.com
# 
# 
# 
# 1.1 Call in R Source Code
source("2_Social/SourcedScripts/Function_export_xlsx.R")

## 1.2 IMPORT RAW DATA
HH.data <- read.delim("2_Social/FlatDataFiles/BHS/Seascape_HH_wellbeing_2015_0715.txt")
DE.data <-read.delim("2_Social/FlatDataFiles/BHS/Seascape_DE_data_2015_0716.txt") #fix path
district.data <-read.delim("2_Social/FlatDataFiles/BHS/District_2015_0715.txt")
settlement.data <-read.delim("2_Social/FlatDataFiles/BHS/Settlement_2015_0715.txt")
ethnic<- read.delim ("2_Social/FlatDataFiles/BHS/State_Seascape_2015_ethnicities_2015_0716.txt") 


##  JOIN DATASETS 
settlement.data <- subset (settlement.data, select=c("SettlementID","Treatment","MPAID"))
settlement.data$MPAID <- as.factor(settlement.data$MPAID)
settlement.data$MPA.network <-revalue(settlement.data$MPAID, c("1"="1", "2"="3","3"="2","4"="1","5"="1","6"="1", "7"="2","8"="2","9"="2"))
HH.data$MPAID<-NULL
HH.data <-left_join(HH.data, settlement.data, by="SettlementID")
DE.data <- left_join (DE.data, HH.data, by="HouseholdID")
identification <- subset (HH.data, select=c("HouseholdID","MPAID","MPA.network","SettlementID","Treatment", "InterviewYear"))
ethnic.data <-left_join(ethnic,identification, by="HouseholdID")

# 2.0 COMPUTE VARIABLES

# 2.1  Food Security
# Compute USDA Household Food Security Scale (Six Item Scale)
hfoodsec<-subset(HH.data, select=c("HouseholdID","FSDidNotLast", "FSBalancedDiet", "FSAdultSkip","FSEatLess","FSFreqAdultSkip","FSHungry", "MPAID","SettlementID","MPA.network"))
is.na( hfoodsec[, 2:7] ) <- hfoodsec[ ,2:7] >= 990
hfoodsec[is.na(hfoodsec)] <- 990

hfoodsec$FSDidNotLast<-as.factor(hfoodsec$FSDidNotLast) #  recode all FoodSecurity Data as factors
hfoodsec$FSBalancedDiet<-as.factor(hfoodsec$FSBalancedDiet)
hfoodsec$FSAdultSkip<-as.factor(hfoodsec$FSAdultSkip)
hfoodsec$FSFreqAdultSkip<-as.factor(hfoodsec$FSFreqAdultSkip)
hfoodsec$FSEatLess<-as.factor(hfoodsec$FSEatLess)
hfoodsec$FSHungry<-as.factor(hfoodsec$FSHungry)

hfoodsec$FSDidNotLast <- revalue(hfoodsec$FSDidNotLast, c("2"="1", "3"="0")) #  recode FSDidNotLast, FSBalancedDiet, FSAdultSkip to binary variables
hfoodsec$FSBalancedDiet <- revalue(hfoodsec$FSBalancedDiet, c("2"="1", "3"="0"))
hfoodsec$FSFreqAdultSkip <- revalue(hfoodsec$FSFreqAdultSkip, c("2"="1", "3"="0"))


hfoodsec$FSDidNotLast <- revalue(hfoodsec$FSDidNotLast, c("4"="990", "5"="990")) #  pull out junk values
hfoodsec$FSBalancedDiet <- revalue(hfoodsec$FSBalancedDiet, c("4"="990"))
hfoodsec$FSAdultSkip <- revalue(hfoodsec$FSAdultSkip, c("2"="990", "3"="990", "4"="990"))
hfoodsec$FSEatLess <- revalue(hfoodsec$FSEatLess, c("4"="990", "2"="990"))


hfoodsec$FSDidNotLast<-as.numeric(as.character(hfoodsec$FSDidNotLast)) #  List and Extract Households with insufficient data to compute score
hfoodsec$FSBalancedDiet<-as.numeric(as.character(hfoodsec$FSBalancedDiet))
hfoodsec$FSAdultSkip<-as.numeric(as.character(hfoodsec$FSAdultSkip))
hfoodsec$FSEatLess<-as.numeric(as.character(hfoodsec$FSEatLess))
hfoodsec$FSFreqAdultSkip<-as.numeric(as.character(hfoodsec$FSAdultSkip))
hfoodsec$FSHungry<-as.numeric(as.character(hfoodsec$FSHungry))
hfoodsec<-mutate(hfoodsec, error.sum = FSDidNotLast + FSBalancedDiet + FSAdultSkip + FSEatLess + FSFreqAdultSkip + FSHungry)
no.hfoodsec<-subset(hfoodsec, error.sum>3959)
hfoodsec<-subset(hfoodsec, error.sum<3960)

no.hfoodsec$HFS.Cat<-995 #  Create placeholder values for future scores
no.hfoodsec$HFS.Con<-995
no.hfoodsec<-subset(no.hfoodsec,select=c("HouseholdID","HFS.Cat","HFS.Con"))
hfoodsec$FSDidNotLast<-ifelse(hfoodsec$FSDidNotLast==990,ifelse((hfoodsec$FSBalancedDiet==1|hfoodsec$FSAdultSkip==1|hfoodsec$FSEatLess==1|hfoodsec$FSFreqAdultSkip==1|hfoodsec$FSHungry==1),1,0),hfoodsec$FSDidNotLast) # impute missing scores
hfoodsec$FSBalancedDiet<-ifelse(hfoodsec$FSBalancedDiet==990,ifelse((hfoodsec$FSDidNotLast==1 &(hfoodsec$FSAdultSkip==1|hfoodsec$FSEatLess==1|hfoodsec$FSFreqAdultSkip==1|hfoodsec$FSHungry==1)),1,0),hfoodsec$FSBalancedDiet)
hfoodsec$FSAdultSkip<-ifelse(hfoodsec$FSAdultSkip==990,ifelse(((hfoodsec$FSDidNotLast==1& hfoodsec$FSBalancedDiet==1) &(hfoodsec$FSEatLess==1|hfoodsec$FSFreqAdultSkip==1|hfoodsec$FSHungry==1)),1,0),hfoodsec$FSAdultSkip)
hfoodsec$FSEatLess<-ifelse(hfoodsec$FSEatLess==990,ifelse(((hfoodsec$FSDidNotLast==1& hfoodsec$FSBalancedDiet& hfoodsec$FSAdultSkip==1)&(hfoodsec$FSFreqAdultSkip==1|hfoodsec$FSHungry==1)),1,0),hfoodsec$FSEatLess)
hfoodsec$FSFreqAdultSkip<-ifelse(hfoodsec$FSFreqAdultSkip==990,ifelse(((hfoodsec$FSDidNotLast==1&hfoodsec$FSBalancedDiet & hfoodsec$FSAdultSkip==1 & hfoodsec$FSEatLess==1)&(hfoodsec$FSHungry==1)),1,0),hfoodsec$FSFreqAdultSkip)
hfoodsec$FSHungry<-ifelse(hfoodsec$FSHungry==990,ifelse((hfoodsec$FSDidNotLast==1& hfoodsec$FSBalancedDiet==1& hfoodsec$FSAdultSkip==1 & hfoodsec$FSEatLess==1 &hfoodsec$FSFreqAdultSkip==1),1,0),hfoodsec$FSHungry)


hfoodsec<-mutate(hfoodsec,hfoodsec=FSDidNotLast+FSBalancedDiet+FSAdultSkip+FSEatLess+FSFreqAdultSkip+FSHungry) #  Compute raw household food security scores
hfoodsec$hfoodsec<-as.factor(hfoodsec$hfoodsec)
hfoodsec$HFS.Cat <- revalue(hfoodsec$hfoodsec, c("0"="1", "1"="1","2"="2","3"="2","4"="2","5"="3","6"="3")) # convert to categorical score

hfoodsec$HFS.Con <- revalue(hfoodsec$hfoodsec, c("0"="0", "1"="2.04","2"="2.99","3"="3.77","4"="4.5","5"="5.38","6"="6.06")) #convert Rasch continuous score
hfoodsec<-subset(hfoodsec, select=c("HouseholdID","HFS.Cat","HFS.Con"))  # eliminate intermediate step rows
hfoodsec$HFS.Con<-as.numeric(as.character(hfoodsec$HFS.Con))
no.hfoodsec$HFS.Cat<-as.factor(no.hfoodsec$HFS.Cat) 
hfoodsec<-rbind(hfoodsec,no.hfoodsec)  # recombine households with HFS data and those with no data
hfoodsec<-hfoodsec[order(hfoodsec$HouseholdID),]  #reorder dataframe
hfoodsec <-left_join(hfoodsec,identification, by="HouseholdID")
is.na(hfoodsec$HFS.Cat)<-hfoodsec$HFS.Cat==995
hfoodsec<-na.omit(hfoodsec)
hfoodsec$seascape <-1
rm(no.hfoodsec)  # Remove working files

# 2.2  Place attachment

place.attach<-subset(HH.data, select=c("HouseholdID","PlaceHappy","PlaceFavourite","PlaceMiss","PlaceBest","PlaceFishHere","PlaceBeMyself"))
is.na(place.attach[,2:7])<-place.attach[,2:7]>=990 #clean all blind codes to a single value [990]
place.attach[is.na(place.attach)]<-990
place.attach<-mutate(place.attach, error.sum=PlaceHappy+PlaceFavourite+PlaceMiss+PlaceBest+PlaceFishHere+PlaceBeMyself) # eliminate households with insufficient data to compute score
no.place.attach<-subset(place.attach, error.sum>=5940)
no.place.attach$P.Attach<-NA
### Create placeholder to reinsert into final index
no.place.attach<-subset(no.place.attach, select=c("HouseholdID","P.Attach")) # create placeholder to insert into final index
place.attach<-subset(place.attach, error.sum<5940)
place.attach[place.attach==990] <- NA
place.attach$P.Attach<-rowMeans((place.attach[,2:7]), na.rm = TRUE) #compute place attachment index
place.attach.index<-subset(place.attach, select=c("HouseholdID","P.Attach")) #  recombine missing values
place.attach.index<-rbind(place.attach.index,no.place.attach)
place.attach.index<-place.attach.index[order(place.attach.index$HouseholdID),]
place.attach.index<-na.omit(place.attach.index) # turn off this line when computing treatment effects
rm(no.place.attach, place.attach) #remove intermediate objects 


# 2.3 Marine tenure index
marine.tenure<-subset(HH.data,select=c("HouseholdID","RightsAccess","RightsHarvest","RightsManage","RightsExclude","RightsTransfer", "SettlementID","MPAID","MPA.network"))
is.na(marine.tenure[,2:6])<-marine.tenure[,2:6]>1 #  Clean all blind codes to single value [990] 
marine.tenure[is.na(marine.tenure)]<-990
marine.tenure<-mutate(marine.tenure,error.sum=RightsAccess+RightsHarvest+RightsManage+RightsExclude+RightsTransfer) #  Extract households with no data for marine tenure items
no.marine.tenure<-subset(marine.tenure, error.sum>=4950)
no.marine.tenure$Rights<-NA # Create placeholder to reinsert into final index
no.marine.tenure<-subset(no.marine.tenure, select=c("HouseholdID","SettlementID","MPAID","MPA.network","Rights"))
marine.tenure[marine.tenure==990] <- 0 #  Compute household marine tenure index for households with sufficient data
marine.tenure<-mutate(marine.tenure,Rights=RightsAccess+RightsHarvest+RightsManage+RightsExclude+RightsTransfer)
marine.tenure.index<-subset(marine.tenure,select=c("HouseholdID","SettlementID","MPAID","MPA.network","Rights"))
marine.tenure.index<-rbind(marine.tenure.index,no.marine.tenure) #  Recombine households with insufficient data with marine.tenure.index
marine.tenure.index<-marine.tenure.index[order(marine.tenure.index$HouseholdID),]
marine.tenure.index <- na.omit(marine.tenure.index)
marine.tenure.index$seascape <- 1
rm(marine.tenure,no.marine.tenure)

# 2.4 Household material assets 
assets<-HH.data[,grep('^Asset',names(HH.data))]
HouseholdID<-HH.data$HouseholdID
assets<-cbind(HouseholdID,assets)
is.na(assets[,2:20])<-assets[,2:20]>990 #  Clean missing values to a single value
assets[is.na(assets)]<-990

#  Eliminate disaggregated columns.
assets<-subset(assets, select=c("HouseholdID","AssetCarTruck","AssetBicycle", "AssetMotorcycle","AssetBoatNoMotor", "AssetBoatOutboard","AssetBoatInboard", "AssetPhoneCombined","AssetEntertain", "AssetSatellite","AssetGenerator", "AssetTV"))
#  Eliminate households with insufficient information to compute household material assets index
assets<-mutate(assets, Missing.Sum=AssetCarTruck+AssetBicycle+AssetMotorcycle+AssetBoatNoMotor+AssetBoatOutboard+AssetBoatInboard+AssetPhoneCombined+AssetEntertain+AssetSatellite+AssetGenerator+AssetTV)
#no.asset.data<-subset(assets,Missing.Sum>=(as.numeric(length(names(assets))*990)))
#no.asset.data$asset.index<-995
#no.asset.index<-subset(no.asset.data, select=c("HouseholdID","asset.index")) # Note:  no.asset.data may be length =0, as few households report blind codes for all Asset items.
asset.rank<-c(11,9,10,6,7,8,4,1,3,5,2) #  asset.rank = value rank of item in basket of goods (high values = higher prices)
#assets<-subset(assets,Missing.Sum<(as.numeric(length(names(assets))*990))) #  compute assets index
#asset.items<-subset(assets,select=c("AssetCarTruck","AssetBicycle", "AssetMotorcycle","AssetBoatNoMotor", "AssetBoatOutboard","AssetBoatInboard", "AssetPhoneCombined","AssetEntertain", "AssetSatellite","AssetGenerator", "AssetTV"))
is.na(assets[,2:12])<-assets[,2:12]>=990
asset.items<-assets
asset.items[is.na(asset.items)]<-0
asset.items<-mutate(asset.items,CarTruck.weight=AssetCarTruck*asset.rank[1])
asset.items<-mutate(asset.items,Bicycle.weight=AssetBicycle*asset.rank[2])
asset.items<-mutate(asset.items,Motorcycle.weight=AssetMotorcycle*asset.rank[3])
asset.items<-mutate(asset.items,BoatNoMotor.weight=AssetBoatNoMotor*asset.rank[4])
asset.items<-mutate(asset.items,BoatOutboard.weight=AssetBoatOutboard*asset.rank[5])
asset.items<-mutate(asset.items,BoatInboard.weight=AssetBoatInboard*asset.rank[6])
asset.items<-mutate(asset.items,PhoneCombined.weight=AssetPhoneCombined*asset.rank[7])
asset.items<-mutate(asset.items,Entertain.weight=AssetEntertain*asset.rank[8])
asset.items<-mutate(asset.items,Satellite.weight=AssetSatellite*asset.rank[9])
asset.items<-mutate(asset.items,Generator.weight=AssetGenerator*asset.rank[10])
asset.items<-mutate(asset.items,TV.weight=AssetTV*asset.rank[11])
asset.items<-mutate(asset.items, asset.index=CarTruck.weight+Bicycle.weight+Motorcycle.weight+BoatNoMotor.weight+BoatOutboard.weight+BoatInboard.weight+PhoneCombined.weight+Entertain.weight+Satellite.weight+Generator.weight + TV.weight)
#household.assets<-rbind(household.assets, no.asset.index)
identifying.information<-data.frame(subset(HH.data, select=c("HouseholdID","MPAID","SettlementID","MPA.network")))
household.assets <- left_join(asset.items,identifying.information)
household.assets <-subset(household.assets, select=c("HouseholdID","MPAID","SettlementID","MPA.network", "asset.index"))
household.assets$seascape <- 1

# Child enrollment

##computes percentage of school age children in school for surveyed households.
HH.enrol <- subset (DE.data, select=c("DemographicID","HouseholdID","IndividualAge", "IndividualGender","IndividualEnrolled", "MPAID","SettlementID","MPA.network")) #  extract data
# Handle missing values
# For IndividualEnrollment, "998"
is.na(HH.enrol[,5])<-HH.enrol[,5]>=2 #convert any coding errors to NA
is.na(HH.enrol[, 4]) <- HH.enrol[, 4] >= 3
is.na(HH.enrol[,3]) <- HH.enrol[, 3] >=130
HH.enrol<-na.omit(HH.enrol)
HH.enrol$IndividualEnrolled<-as.factor(HH.enrol$IndividualEnrolled)
HH.enrol$IndividualEnrolled <- revalue(HH.enrol$IndividualEnrolled, c("998"="0","989"="0","993"="0", "994"="0", "995"="0", "996"="0","997"="0","999"="0")) # convert not applicables to zero
HH.enrol$IndividualEnrolled <-as.numeric(as.character(HH.enrol$IndividualEnrolled))

# Identify school age children in households
HH.enrol <- HH.enrol[complete.cases(HH.enrol[,2:3]),] #  omit individuals whose age is unknown
HH.enrol$Child <-ifelse((HH.enrol$IndividualAge<19)&(HH.enrol$IndividualAge>4),1,0) #  identify school age children
Children.HH <- ddply(HH.enrol, .(HouseholdID), summarise, N.Child=sum(Child))
No.Child.HH <- Children.HH[Children.HH$N.Child==0,]
HH.enrol<- HH.enrol[HH.enrol$Child==1,] #select only children from dataset
N.Children<-subset(HH.enrol,select=c("HouseholdID","Child")) 
Enrolled.Child <- subset(HH.enrol, select=c ("HouseholdID","IndividualEnrolled")) 
N.Children <- ddply(N.Children, .(HouseholdID), summarise, N.Child = sum(Child)) #count number of children in household
Enrolled.Child <- ddply(Enrolled.Child, . (HouseholdID), summarise, Child.Enrol =sum(IndividualEnrolled)) #  count number of children enrolled
Enrolled <- left_join (Enrolled.Child,N.Children, by="HouseholdID") #  rejoin the enrollment and child count data
Enrolled <- mutate (Enrolled,Percent.Enrolled=((Child.Enrol/N.Child)*100)) #  create percentage 
## Recombine data to households with no children
Enrolled <- subset(Enrolled, select=c("HouseholdID","Percent.Enrolled")) #select relevant fields
No.Child.HH$Percent.Enrolled <- 0 #create dummy for Percent.Enrolled for households without children
No.Child.HH$N.Child <- NULL #remove intermediate fields
Enrol<-rbind(Enrolled,No.Child.HH) #recombine households with and without children 
Enrol <-Enrol[order(Enrol$HouseholdID),] #re-order dataset
identifying.information<-data.frame(subset(HH.data, select=c("HouseholdID","MPAID","SettlementID","MPA.network")))
Enrol <-left_join(Enrol, identifying.information, by="HouseholdID")
Enrol$seascape <- 1
rm(No.Child.HH,Enrolled.Child,N.Children,Enrolled, HH.enrol)

#Clean up original data objects
rm(DE.data,district.data, ethnic, ethnic.data, HH.data, identification, settlement.data, identifying.information, save.xlsx,asset.items,assets,asset.rank)
