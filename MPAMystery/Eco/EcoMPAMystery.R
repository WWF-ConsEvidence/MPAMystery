# ----
# code:  Calculating the ecological impacts of marine protected areas
# author: Kelly Claborn, clabornkelly@gmail.com
# created: February 2017
# modified: 
# ----
# inputs:
# KC_Fish_ForEcoMPAMystery.txt -- exported from RHM Master Database in Access
#   -this table (and thus, text file) can be created by running the query (as "Make Table")
#    saved in the 'KC_Queries_Tables' Group, named 'KC_Q_Fish'.
#   -this table is the cleaned data from all MPAs and years, ready to be analyzed
#    and calculated into density, biomass, and other related variables -- by site, 
#    subzone, MPA, and/or year.
# KC_PIT_ForEcoMPAMystery.txt -- exported from RHM Master Database in Access
#   -this table (and thus, text file) can be created by running the query (as "Make Table")
#    saved in the 'KC_Queries_Tables' Group, named 'KC_Q_PIT'.
#   -this table is the data from all MPAs and years, ready to be analyzed
#    and calculated into percent coral cover, and other related variables -- by site, 
#    subzone, MPA, and/or year.
# ----
# Code sections
#  1) Import data & libraries, define functions
#  2) Clean data, define universal variables, subset data
#  3) Basic analysis, all data
#  
# ----
############################################################
#
# SECTION 1: Import data & libraries, define functions
#
############################################################
# ----

# 1.1 Call libraries and set working directory
library(plyr)
library(dplyr)
library(ggplot2)
library(varhandle)
library(grid)
library(gridExtra)
library(gtable)
#library(xlsx)
library(RPostgreSQL) # for Mac
library(RODBC)

#EcoMPAMysteryDB <- odbcConnect("EcoMPAMysteryDB")
EcoMPAMysteryDB <- odbcConnect("mpamystery") #Megan


# setwd('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPAMystery Team Folder/')
# setwd('C:/Users/kclaborn05/Dropbox (MPAMystery)/MPAMystery Team Folder/')

# ----- FOR MEGAN
# Site_Level_MetaData <- sqlFetch(EcoMPAMysteryDB,'Site_Level_MetaData')
# Site_Level_MetaData <- Site_Level_MetaData[order(Site_Level_MetaData$MPA_Name,
#                                                  Site_Level_MetaData$Site_Name,
#                                                  Site_Level_MetaData$Year),]
# 
# setwd('C:/Users/claborn-intern/Dropbox (MPAMystery)/MPAMystery Team Folder/MEGAN_BARNES')
# write.xlsx(Site_Level_MetaData,'SiteLevel_MetaData.xlsx',row.names=F)
# write.csv(Fish,'RawFishData.csv',row.names=F)
# write.csv(Benthic,'RawPITData.csv',row.names=F)

# 1.2 Import data
Fish <- sqlFetch(EcoMPAMysteryDB,'KC_Fish_ForEcoMPAMystery')
Benthic <- sqlFetch(EcoMPAMysteryDB,'KC_PIT_ForEcoMPAMystery')
Site <- sqlFetch(EcoMPAMysteryDB,'Site')

#Megan import flat files
Fish <- read.csv('../../MPA ECOLOGICAL DATABASE/R Analysis/ExportedData_FromAccess/KC_Fish_ForEcoMPAMystery.txt')
Benthic <- read.csv(EcoMPAMysteryDB,'../../MPA ECOLOGICAL DATABASE/R Analysis/ExportedData_FromAccess/KC_PIT_ForEcoMPAMystery.txt')
Site <- read.csv('../Site_Characteristics.txt')

# 1.3 Define functions for cleaning and manipulating data
# --- Substitution function for multiple patterns
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

# --- Capitalize all letters
cleancase <- function(x) {
  mgsub(c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"),
        c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"),
        x)
}

# --- Remove extra spaces & characters
trim <- function(x) mgsub(c("^\\s+|\\s+$","'","?"),rep("",3), x)

# --- Benthic categorization functions
benthic.categories <- c("coral","soft.coral","cca","bleached","rubble","oth.algae")

coral <- function(x) {
  result <- x
  for(i in 1:length(x)) {
    result[i] <- ifelse(x[i]=="AB" | x[i]=="AC" | x[i]=="ACB" |
                          x[i]=="ACD" | x[i]=="ACE" | x[i]=="ACS" |
                          x[i]=="ACT" | x[i]=="CB" | x[i]=="CCF" |
                          x[i]=="CCM" | x[i]=="CE" | x[i]=="CEC" | 
                          x[i]=="CF" | x[i]=="CHE" | x[i]=="CHL" | 
                          x[i]=="CM" | x[i]=="CME" | x[i]=="CMR" | 
                          x[i]=="CS" | x[i]=="CSM" | x[i]=="CTU" |
                          x[i]=="HC" | x[i]=="LC" | x[i]=="P" | 
                          x[i]=="PBR" | x[i]=="PMS",1,0)
  }
  result.sum <- sum(as.numeric(result))
  result.sum
}

soft.coral <- function(x) {
  result <- x
  for(i in 1:length(x)) {
    result[i] <- ifelse(x[i]=="SC" | x[i]=="SCB" | x[i]=="XN",1,0)
  }
  result.sum <- sum(as.numeric(result))
  result.sum
}

cca <- function(x) {
  result <- x
  for(i in 1:length(x)) {
    result[i] <- ifelse(x[i]=="CA" | x[i]=="CCA",1,0)
  }
  result.sum <- sum(as.numeric(result))
  result.sum
}

bleached <- function(x) {
  result <- x
  for(i in 1:length(x)) {
    result[i] <- ifelse(x[i]=="B" | x[i]=="BC",1,0)
  }
  result.sum <- sum(as.numeric(result))
  result.sum
}

rubble <- function(x) {
  result <- x
  for(i in 1:length(x)) {
    result[i] <- ifelse(x[i]=="R" | x[i]=="RB",1,0)
  }
  result.sum <- sum(as.numeric(result))
  result.sum
}

oth.algae <- function(x) {
  result <- x
  for(i in 1:length(x)) {
    result[i] <- ifelse(x[i]=="A" | x[i]=="AA" | x[i]=="DCA" |
                          x[i]=="HA" | x[i]=="M" | x[i]=="MA" |
                          x[i]=="TA",1,0)
  }
  result.sum <- sum(as.numeric(result))
  result.sum
}

# ----
############################################################
#
# SECTION 2: Clean data, define universal columns, subset data
#
############################################################
# ----

# 2.1 Clean data
# --- Fill empty "Size_cm" fields with mean fish family size per MPA
FishFam_SizeMeans <- 
  Fish %>%
  group_by(MPA_ID,Fish_Family) %>%
  summarise(Mean_Size_cm=mean(Size_cm,na.rm=T))

Fish$Size_cm <- mapply(a=Fish$Size_cm,
                       b=Fish$MPA_ID,
                       c=Fish$Fish_Family,
                       function(a,b,c){
                         ifelse(is.na(a),
                                FishFam_SizeMeans$Mean_Size_cm[FishFam_SizeMeans$MPA_ID==b &
                                                                 FishFam_SizeMeans$Fish_Family==c],
                                a)
                       })
# *** Should transect width be calculated here, after filling in empty Size_cm observations?
# *** Or, keep it in Access?

# --- Clean benthic data, using trim and cleancase functions (defined above)
Benthic[,9:108] <- mapply(a=seq(9,108,by=1),
                      function(a) {
                        b <- trim(Benthic[,a])
                        cleancase(b)
                      }
)
Benthic[,9:108] <- lapply(Benthic[,9:108],as.factor)

### Check to make sure all levels (with typos) are accounted for in benthic coding functions
    BenthicLevels <- mapply(a=seq(9,108,by=1),
                            function(a) {
                              levels(Benthic[,a])
                              })
    BenthicLevels <- unlist(BenthicLevels)
    BenthicLevels <- as.data.frame(unique(BenthicLevels))

# 2.2 Add universal columns to Fish and Benthic data frames

# --- Transect area
Fish$Transect_Area_ha <- (Fish$Transect_Width*Fish$Transect_Length)/10000

# --- Abundance per hectare
Fish$Abundance_ha <- Fish$Abundance_n/Fish$Transect_Area_ha

# --- Biomass per hectare
Fish$Biomass_kg_ha <- ((Fish$Abundance_n*Fish$Biomass_Constant_a*
                          (Fish$Size_cm^Fish$Biomass_Constant_b))/1000)/Fish$Transect_Area_ha

# 2.3 Subsets of data
# --- Fish data without Long Swim transects, 10m (+/- 2m) depth
Fish_noLS <- Fish[Fish$Transect_No!="LS" &
                    Fish$Transect_No!="All" &
                    Fish$Depth>=8 &
                    Fish$Depth<=12,]

# --- Benthic data at 10m (+/- 2m) depth
Benthic_Depth10m <- Benthic[Benthic$Depth>=8 &
                              Benthic$Depth<=12,]

# --- Current year for each site
Current_Year_Fish <- 
  Fish %>%
  group_by(Site_ID) %>%
  summarise(Current_Year_Fish=max(Year))

Current_Year_Benthic <-
  Benthic %>%
  group_by(Site_ID) %>%
  summarise(Current_Year_Benthic=max(Year))

# --- Transect-level logistical data
Transect_info <- aggregate(Fish[,1:7],by=list(Fish$Transect_ID),FUN=unique)
colnames(Transect_info) <- c("Transect_ID",colnames(Transect_info)[2:8])
Transect_info <- left_join(Transect_info,Current_Year_Fish,by="Site_ID")
Transect_info <- left_join(Transect_info,Current_Year_Benthic,by="Site_ID")

# --- Site-level logistical data
Site_info <- 
  Transect_info %>%
  group_by(Site_ID,MPA_ID) %>%
  summarise(NumberYearsMonitored=sum(length(unique(Sampling_Event_ID))))

# --- MPA-level logistical data
MPA_info <- 
  Site_info %>%
  group_by(MPA_ID) %>%
  summarise(MaxYearsMonitored=max(NumberYearsMonitored))

# --- Logistical data
Logistic_info <- left_join(Transect_info,Site_info,by=c("Site_ID","MPA_ID"))
Logistic_info <- left_join(Logistic_info,MPA_info,by="MPA_ID")
  
# ----
############################################################
#
# SECTION 3: Basic analysis, all data
#
############################################################
# ----

# 3.1 Calculate fish family abundance (#/ha) and biomass (kg/ha), per transect

FishData_ha_WithLS <- 
  Fish %>% 
  group_by(Transect_ID,Fish_Family) %>%
  summarise(Abundance_t=sum(Abundance_ha),
            Biomass_t=sum(Biomass_kg_ha))

FishData_ha_NoLS <- 
  Fish_noLS %>% 
  group_by(Transect_ID,Fish_Family) %>%
  summarise(Abundance_t=sum(Abundance_ha),
            Biomass_t=sum(Biomass_kg_ha))


FishData_ha_WithLS <- left_join(FishData_ha_WithLS,Logistic_info,by="Transect_ID")

FishData_ha_NoLS <- left_join(FishData_ha_NoLS,Logistic_info,by="Transect_ID")
FishData_ha_NoLS <- left_join(FishData_ha_NoLS,Site[,c(2,3,13,14)],by="Site_ID")

# 3.2 Calculate benthic data, per transect

for(i in benthic.categories) { 
  Benthic[,i] <- apply(Benthic[,9:108],1,i)
}

for(i in benthic.categories) { 
  Benthic_Depth10m[,i] <- apply(Benthic_Depth10m[,9:108],1,i)
}
  
Benthic_AllDepths_PerTransect <- Benthic[,c(1:8,109:114)]
Benthic_10m_PerTransect <- Benthic_Depth10m[,c(1:8,109:114)]

Benthic_10m_PerTransect <- left_join(Benthic_10m_PerTransect,Site[,c(2,3,13,14)],by="Site_ID")


# ----
############################################################
#
# SECTION 4: Fish analyses, broken down by location, year, and other attributes
#
############################################################
# ----

# 4.1 Fish Data, By Site (5 transects per site)

### ADD SD for variables


# PRETTY SURE ALL OF THESE BIOMASS ANALYSES WORK!
# --- Means for all years
FishData_NoLS_BySite <-
  FishData_ha_NoLS %>%
  group_by(Site_ID,Year,Fish_Family) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Type_of_Zone=unique(Type_of_Zone),
            Abundance_s=mean(Abundance_t,na.rm=T),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass_s=mean(Biomass_t,na.rm=T),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# --- Mean for only current year
FishData_NoLS_BySite_CurrentYr <-
  FishData_ha_NoLS[FishData_ha_NoLS$Year==FishData_ha_NoLS$Current_Year_Fish,] %>% 
  group_by(Site_ID,Fish_Family) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# 4.2 Fish Data, By MPA

# --- Means for all years
FishData_NoLS_ByMPA <-
  FishData_ha_NoLS[FishData_ha_NoLS$NumberYearsMonitored==FishData_ha_NoLS$MaxYearsMonitored,] %>%
  group_by(MPA_ID,Year,Fish_Family) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# --- Mean for only current year
FishData_NoLS_ByMPA_CurrentYr <-
  FishData_ha_NoLS[FishData_ha_NoLS$Year==FishData_ha_NoLS$Current_Year_Fish,] %>% 
  group_by(MPA_ID,Fish_Family) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# 4.3 Fish Data, By Zone Type

# --- Mean for current year, all MPAs combined
FishData_NoLS_ByZone_CurrentYr <-
  FishData_ha_NoLS[FishData_ha_NoLS$Year==FishData_ha_NoLS$Current_Year_Fish,] %>%
  group_by(Type_of_Zone,Fish_Family) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# --- Means for all years, broken down by MPA
FishData_NoLS_ByZone_ByMPA <-
  Fish_noLS %>%
  group_by(MPA_Name,Type_of_Zone,Year,Fish_Family) %>%
  summarise(Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

FishData_NoLS_ByZone_ByMPA$Type_of_Zone <- factor(FishData_NoLS_ByZone_ByMPA$Type_of_Zone,
                                                  levels=c("NTZ","Use","Control"),
                                                  ordered=T)

FishData_NoLS_ByZone_ByMPA$Year <- as.ordered(FishData_NoLS_ByZone_ByMPA$Year)
FishData_NoLS_ByZone_ByMPA <- FishData_NoLS_ByZone_ByMPA[!is.na(FishData_NoLS_ByZone_ByMPA$MPA_Name),]

# --- Means for current year, broken down by MPA
FishData_NoLS_ByZone_ByMPA_CurrentYr <-
  FishData_ha_NoLS[FishData_ha_NoLS$Year==FishData_ha_NoLS$Current_Year_Fish,] %>%
  group_by(Type_of_Zone,MPA_ID,Fish_Family) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# --- Means for all years, broken down by Site
FishData_NoLS_ByZone_BySite <-
  FishData_ha_NoLS %>%
  group_by(Site_ID,Type_of_Zone,Year,Fish_Family) %>%
  summarise(MPA=unique(MPA_Name),
            Site_Name=unique(Site_Name),
            Latitude=unique(Lat),
            Longitude=unique(Lon),
            Abundance=round(mean(Abundance_t,na.rm=T),3),
            Abundance_SE=round(sd(Abundance_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3),
            Biomass=round(mean(Biomass_t,na.rm=T),3),
            Biomass_SE=round(sd(Biomass_t,na.rm=T)/sqrt(length(unique(Transect_ID))),3))

# ----
############################################################
#
# SECTION 5: Benthic analyses, broken down by location, year, and other attributes
#
############################################################
# ----

# 5.1 Benthic Data, By Site (3 transects per site)

# THESE DO NOT WORK :(

# --- Categorical proportions for all years, combined
BenthicData_10m_BySite <- 
  Benthic_10m_PerTransect %>%
  group_by(Site_ID) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Percent.Coral=round(sum(coral)/length(unique(c(Site_ID,Type_of_Zone,Year))*100)*100,2),
            Percent.SoftCoral=round(sum(soft.coral)/length(unique(c(Site_ID,Type_of_Zone,Year))*100)*100,2),
            Percent.CCA=round(sum(cca)/length(unique(c(Site_ID,Type_of_Zone,Year))*100)*100,2),
            Percent.Bleached=round(sum(bleached)/length(unique(c(Site_ID,Type_of_Zone,Year))*100)*100,2),
            Percent.Rubble=round(sum(rubble)/length(unique(c(Site_ID,Type_of_Zone,Year))*100)*100,2),
            Percent.OthAlgae=round(sum(oth.algae)/length(unique(c(Site_ID,Type_of_Zone,Year))*100)*100,2))

# --- Categorical proportions for all years, separated
BenthicData_10m_BySite_ByYear <- 
  Benthic_10m_PerTransect %>%
  group_by(Site_ID,Year) %>%
  summarise(MPA_Name=unique(MPA_Name),
            Percent.Coral=round((sum(coral)/(sum(coral)+sum(soft.coral)+sum(cca)+
                                               sum(bleached)+sum(rubble)+sum(oth.algae)))*100,2),
            Percent.SoftCoral=round((sum(soft.coral)/(sum(coral)+sum(soft.coral)+sum(cca)+
                                                        sum(bleached)+sum(rubble)+sum(oth.algae)))*100,2),
            Percent.CCA=round((sum(cca)/(sum(coral)+sum(soft.coral)+sum(cca)+
                                           sum(bleached)+sum(rubble)+sum(oth.algae)))*100,2),
            Percent.Bleached=round((sum(bleached)/(sum(coral)+sum(soft.coral)+sum(cca)+
                                                     sum(bleached)+sum(rubble)+sum(oth.algae)))*100,2),
            Percent.Rubble=round((sum(rubble)/(sum(coral)+sum(soft.coral)+sum(cca)+
                                                 sum(bleached)+sum(rubble)+sum(oth.algae)))*100,2),
            Percent.OthAlgae=round((sum(oth.algae)/(sum(coral)+sum(soft.coral)+sum(cca)+
                                                      sum(bleached)+sum(rubble)+sum(oth.algae)))*100,2))

# --- Categorical proportions for only current year

# THIS WORKS!!!

# --- Categorical proportions for all years, by zone, by site
BenthicData_10m_ByZone_BySite <- 
  Benthic_10m_PerTransect %>%
  group_by(MPA_Name,Site_ID,Site_Name,`Type of Zone`,Year) %>%
  summarise(Latitude=unique(Lat),
            Longitude=unique(Lon),
            Percent.Coral=round((sum(coral)/(length(coral)*100))*100,3),
            SE.Coral=sd(coral)/sqrt(length(coral)),
            Percent.SoftCoral=round((sum(soft.coral)/(length(soft.coral)*100))*100,3),
            SE.SoftCoral=sd(soft.coral)/sqrt(length(soft.coral)),
            Percent.CCA=round((sum(cca)/(length(cca)*100))*100,3),
            SE.CCA=sd(cca)/sqrt(length(cca)),
            Percent.Bleached=round((sum(bleached)/(length(bleached)*100))*100,3),
            SE.Bleached=sd(bleached)/sqrt(length(bleached)),
            Percent.Rubble=round((sum(rubble)/(length(rubble)*100))*100,3),
            SE.Rubble=sd(rubble)/sqrt(length(rubble)),
            Percent.OthAlgae=round((sum(oth.algae)/(length(oth.algae)*100))*100,3),
            SE.OthAlgae=sd(oth.algae)/sqrt(length(oth.algae)))

# Useless -- ignore!
Kofiau.BenthicData <- BenthicData_10m_ByZone_BySite[BenthicData_10m_ByZone_BySite$MPA_Name=="Kofiau dan Pulau Boo Marine Protected Area",
                                                    c(1:3,5:7,4,8:19)]
Kofiau.FishData <- FishData_NoLS_ByZone_BySite[FishData_NoLS_ByZone_BySite$MPA=="Kofiau dan Pulau Boo Marine Protected Area" &
                                                 (FishData_NoLS_ByZone_BySite$Fish_Family=="Acanthuridae" |
                                                    FishData_NoLS_ByZone_BySite$Fish_Family=="Scaridae" |
                                                    FishData_NoLS_ByZone_BySite$Fish_Family=="Siganidae" |
                                                    FishData_NoLS_ByZone_BySite$Fish_Family=="Scarini" |
                                                    FishData_NoLS_ByZone_BySite$Fish_Family=="Haemulidae" |
                                                    FishData_NoLS_ByZone_BySite$Fish_Family=="Lutjanidae" |
                                                    FishData_NoLS_ByZone_BySite$Fish_Family=="Serranidae"),
                                               c(5,1,6,3,7,8,2,4,9:12)]

# ----
############################################################
#
# SECTION 6: Fish Data Plots
#
############################################################
# ----

fill.cols.abundance <- c("NTZ"=alpha("#1B448B",0.85),"Use"=alpha("#6B6B6B",0.85),
                       "Control"=alpha("#1B448B",0.5))
err.cols.abundance <- c("NTZ"=alpha("#1B448B",0.85),"Use"=alpha("#6B6B6B",0.85),
                      "Control"=alpha("#1B448B",0.5))
abundance.labs <- labs(x="Management Zone",y="Abundance (ind/ha)",title="LUTJANIDAE (2015)")

# 6.1 Mean biomass of fish families, by zone type and year (enter MPA_ID of choice)
FishAbundance.lutjanidae <- ggplot(data=FishData_NoLS_ByZone_ByMPA
                                                      [FishData_NoLS_ByZone_ByMPA$MPA_ID==26 &
                                                        FishData_NoLS_ByZone_ByMPA$Fish_Family=="Lutjanidae" &
                                                        FishData_NoLS_ByZone_ByMPA$Year=="2015",],
                                    aes(Type_of_Zone,
                                        Abundance)) +
  geom_bar(aes(fill=Type_of_Zone),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_errorbar(aes(ymin=Abundance-Abundance_SE,
                    ymax=Abundance+Abundance_SE,
                    colour=Type_of_Zone),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  scale_x_discrete(labels=c("No Take","Use","Control")) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1250)) +
  scale_fill_manual(values=fill.cols.abundance) +
  scale_colour_manual(values=err.cols.abundance) +
  plot.theme + plot.guides.MPAimpact.summ + abundance.labs
  

