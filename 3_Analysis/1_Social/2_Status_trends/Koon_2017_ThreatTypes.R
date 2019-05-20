# ---- 1.1 load libraries ----
pacman::p_load(dplyr)

# ---- 1.2 Set working directory ----
setwd("C:/Users/denardo/Dropbox (Personal)/MPA_R_Scripts_for_Kelly")

# ---- 1.3 import data ----
Koon.Threat.Types <- read.csv("C:/Users/denardo/Dropbox (Personal)/MPA_R_Scripts_for_Kelly/SBS/Data/SBS_only_Analysis/Koon_MPA_2017_Threat_Types_Categorized.csv", header = T, sep = ",")

# ---- 1.4 Synthesize threat type results BY MPA ----

Koon.Threat.Types.ByMPA.All <- 
  Koon.Threat.Types %>% 
  group_by(MPAID, Treatment) %>% 
  summarise(Pollution=(length(ThreatType[ThreatType=="Pollution" & 
                                           !is.na(ThreatType)])/
                         length(ThreatType[!is.na(ThreatType)]))*100,
            DestructiveFishing=(length(ThreatType[ThreatType=="Destructive fishing practices" & 
                                                    !is.na(ThreatType)])/
                                  length(ThreatType[!is.na(ThreatType)]))*100,
            IllegalFishing=(length(ThreatType[ThreatType=="Illegal fishing" & 
                                                !is.na(ThreatType)])/
                              length(ThreatType[!is.na(ThreatType)]))*100,
            ClimateChange=(length(ThreatType[ThreatType=="Climate change" & 
                                               !is.na(ThreatType)])/
                             length(ThreatType[!is.na(ThreatType)]))*100,
            HabitatLoss=(length(ThreatType[ThreatType=="Habitat loss" & 
                                             !is.na(ThreatType)])/
                           length(ThreatType[!is.na(ThreatType)]))*100,
            NaturalProcesses=(length(ThreatType[ThreatType=="Natural processes" & 
                                                  !is.na(ThreatType)])/
                                length(ThreatType[!is.na(ThreatType)]))*100,
            OtherMarineUses=(length(ThreatType[ThreatType=="Other marine resource uses" & 
                                                 !is.na(ThreatType)])/
                               length(ThreatType[!is.na(ThreatType)]))*100,
            Other=(length(ThreatType[ThreatType=="Other" & 
                                       !is.na(ThreatType)])/
                     length(ThreatType[!is.na(ThreatType)]))*100)

# create MPA TREATMENT only dataframe
Koon.Threat.Types.ByMPA <- 
  Koon.Threat.Types.ByMPA.All %>%
  filter(Treatment==1)

# create MPA CONTROL only dataframe
Koon.Threat.Types.ByMPA.Control <- 
  Koon.Threat.Types.ByMPA.All %>%
  filter(Treatment==0)

# check row sums
rowSums(Koon.Threat.Types.ByMPA.All[,3:10])


# check threat levels
levels(Koon.Threat.Types$ThreatType)

# ---- 1.5 Synthesize threat type results BY SETTLEMENT ----

Koon.Threat.Types.BySett.All <- 
  Koon.Threat.Types %>% 
  group_by(SettlementID, SettlementName, Treatment, MPAID) %>% 
  summarise(Pollution=(length(ThreatType[ThreatType=="Pollution" & 
                                           !is.na(ThreatType)])/
                         length(ThreatType[!is.na(ThreatType)]))*100,
            DestructiveFishing=(length(ThreatType[ThreatType=="Destructive fishing practices" & 
                                                    !is.na(ThreatType)])/
                                  length(ThreatType[!is.na(ThreatType)]))*100,
            IllegalFishing=(length(ThreatType[ThreatType=="Illegal fishing" & 
                                                !is.na(ThreatType)])/
                              length(ThreatType[!is.na(ThreatType)]))*100,
            ClimateChange=(length(ThreatType[ThreatType=="Climate change" & 
                                               !is.na(ThreatType)])/
                             length(ThreatType[!is.na(ThreatType)]))*100,
            HabitatLoss=(length(ThreatType[ThreatType=="Habitat loss" & 
                                             !is.na(ThreatType)])/
                           length(ThreatType[!is.na(ThreatType)]))*100,
            NaturalProcesses=(length(ThreatType[ThreatType=="Natural processes" & 
                                                  !is.na(ThreatType)])/
                                length(ThreatType[!is.na(ThreatType)]))*100,
            OtherMarineUses=(length(ThreatType[ThreatType=="Other marine resource uses" & 
                                                 !is.na(ThreatType)])/
                               length(ThreatType[!is.na(ThreatType)]))*100,
            Other=(length(ThreatType[ThreatType=="Other" & 
                                       !is.na(ThreatType)])/
                     length(ThreatType[!is.na(ThreatType)]))*100)

# create SETTLEMENT TREATMENT only dataframe
Koon.Threat.Types.BySett <- 
  Koon.Threat.Types.BySett.All %>%
  filter(Treatment==1)

# create SETTLEMENT CONTROL only dataframe
Koon.Threat.Types.BySett.Control <- 
  Koon.Threat.Types.BySett.All %>%
  filter(Treatment==0)

# check row sums
rowSums(Koon.Threat.Types.BySett[,5:12])


# ---- 1.6 Prepare dataframe for plotting ----

Koon.level.SBS.ThreatTypes <- 
  rbind.data.frame(data.frame(SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Koon.Threat.Types.ByMPA.Control[Koon.Threat.Types.ByMPA.Control$MPAID==18,c("Pollution", "DestructiveFishing", "IllegalFishing", 
                                                                                                                "ClimateChange", "HabitatLoss", "NaturalProcesses", "OtherMarineUses", 
                                                                                                                "Other")]),
                   data.frame(SettlementID=0,
                              SettlementName="Koon MPA",
                              Koon.Threat.Types.ByMPA[Koon.Threat.Types.ByMPA$MPAID==18,c("Pollution", "DestructiveFishing", "IllegalFishing", 
                                                                                                "ClimateChange", "HabitatLoss", "NaturalProcesses", "OtherMarineUses", 
                                                                                                "Other")]))
null.row.ThreatTypes <- 
  matrix(rep(NA,10),ncol=10,dimnames=list(NULL,colnames(Koon.level.SBS.ThreatTypes)))


Koon.SBS.ThreatTypes.BySett <- 
  Koon.Threat.Types.BySett[Koon.Threat.Types.BySett$MPAID==18,c("SettlementID", "SettlementName","Pollution", "DestructiveFishing", "IllegalFishing", 
                                                                      "ClimateChange", "HabitatLoss", "NaturalProcesses", "OtherMarineUses", 
                                                                      "Other")]

Koon.SBS.ThreatTypes.BySett <- 
  Koon.SBS.ThreatTypes.BySett[rev(order(Koon.SBS.ThreatTypes.BySett$SettlementName)),]


Koon.Threat.Types.PLOTFORMAT <- 
  rbind.data.frame(Koon.level.SBS.ThreatTypes[c("SettlementID", "SettlementName","Pollution", "DestructiveFishing", "IllegalFishing", 
                                                   "ClimateChange", "HabitatLoss", "NaturalProcesses", "OtherMarineUses", 
                                                   "Other")],
                   null.row.ThreatTypes[c("SettlementID", "SettlementName","Pollution", "DestructiveFishing", "IllegalFishing", 
                                          "ClimateChange", "HabitatLoss", "NaturalProcesses", "OtherMarineUses", 
                                          "Other")],
                   Koon.SBS.ThreatTypes.BySett)

# check row sums
rowSums(Koon.Threat.Types.PLOTFORMAT [,3:10])

# - make SettlementName an ordered factor for plotting
Koon.Threat.Types.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Koon.Threat.Types.PLOTFORMAT$SettlementName),"",
         as.character(Koon.Threat.Types.PLOTFORMAT$SettlementName))

Koon.Threat.Types.PLOTFORMAT$SettlementName <-
  factor(Koon.Threat.Types.PLOTFORMAT$SettlementName,
         levels=unique(Koon.Threat.Types.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Koon.Threat.Types.PLOTFORMAT$Dummy <- 
  ifelse(Koon.Threat.Types.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")

#check row sums
rowSums(Koon.Threat.Types.PLOTFORMAT [,3:10])



