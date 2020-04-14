# 
# code: Compute 'Food security' human well-being outcomes. This code simply retains the original FS indicators
#for future use
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
hfoodsec$HFS.Con<-as.numeric(as.character(hfoodsec$HFS.Con))
no.hfoodsec$HFS.Cat<-as.factor(no.hfoodsec$HFS.Cat) 
hfoodsec<-select(hfoodsec,-c(MPAID, SettlementID, MPA.network))  # eliminate intermediate step rows
#hfoodsec<-rbind(hfoodsec,no.hfoodsec)  # recombine households with HFS data and those with no data
hfoodsec<-hfoodsec[order(hfoodsec$HouseholdID),]  #reorder dataframe
hfoodsec <-left_join(hfoodsec,identification, by="HouseholdID")
is.na(hfoodsec$HFS.Cat)<-hfoodsec$HFS.Cat==995
hfoodsec<-na.omit(hfoodsec)
hfoodsec$seascape <-1
rm(no.hfoodsec)  # Remove working files
head(hfoodsec)



