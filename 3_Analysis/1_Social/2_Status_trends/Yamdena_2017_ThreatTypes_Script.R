
pacman::p_load(plyr,ggplot2,reshape2,reldist,grid,gridExtra,varhandle,xlsx,
               RODBC,Matching,optmatch,tidyr,RItools,Hmisc,MBESS,rbounds,Kendall, tm, NLP, dplyr, dbplyr, RPostgreSQL)

Yamdena.Threat.Types <- read.csv("C:/Users/denardo/Dropbox (Personal)/MPA_R_Scripts_for_Kelly/SBS/Data/SBS_only_analysis/Yamdena_2017_Threat_Types_Categorized.csv", header = T, sep = ",")

# count(Yamdena.Threat.Types, ThreatType)


Yamdena.Threat.Types.ByMPA.All <- 
  Yamdena.Threat.Types %>% 
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
            HabitalLoss=(length(ThreatType[ThreatType=="Habitat loss" & 
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

Yamdena.Threat.Types.ByMPA <- 
  Yamdena.Threat.Types.ByMPA.All %>%
  filter(Treatment==1)

Yamdena.Threat.Types.ByMPA.Control <- 
  Yamdena.Threat.Types.ByMPA.All %>%
  filter(Treatment==0)

            
# rowSums(Yamdena.Threat.Types.ByMPA.All[,3:10])
# 
# 
# levels(Yamdena.Threat.Types$ThreatType)

Yamdena.Threat.Types.BySett.All <- 
  Yamdena.Threat.Types %>% 
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
            HabitalLoss=(length(ThreatType[ThreatType=="Habitat loss" & 
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

Yamdena.Threat.Types.BySett <- 
  Yamdena.Threat.Types.BySett.All %>%
  filter(Treatment==1)

Yamdena.Threat.Types.BySett.Control <- 
  Yamdena.Threat.Types.BySett.All %>%
  filter(Treatment==0)

# rowSums(Yamdena.Threat.Types.BySett[,5:14])


Yamdena.level.SBS.ThreatTypes <- 
  rbind.data.frame(data.frame(SettlementID=0,
                              SettlementName="Control\nSettlements",
                              Yamdena.Threat.Types.ByMPA.Control[Yamdena.Threat.Types.ByMPA.Control$MPAID==19,c("Pollution", "DestructiveFishing", "IllegalFishing", 
                                                                                          "ClimateChange", "HabitalLoss", "NaturalProcesses", "OtherMarineUses", 
                                                                                          "Other")]),
                   data.frame(SettlementID=0,
                              SettlementName="Yamdena MPA",
                              Yamdena.Threat.Types.ByMPA[Yamdena.Threat.Types.ByMPA$MPAID==19,c("Pollution", "DestructiveFishing", "IllegalFishing", 
                                                                                    "ClimateChange", "HabitalLoss", "NaturalProcesses", "OtherMarineUses", 
                                                                                    "Other")]))
null.row.ThreatTypes <- 
  matrix(rep(NA,10),ncol=10,dimnames=list(NULL,colnames(Yamdena.level.SBS.ThreatTypes)))

# ---- 2.1 Status dataset for Yamdena, proportional data ----

Yamdena.SBS.ThreatTypes.BySett <- 
  Yamdena.Threat.Types.BySett[Yamdena.Threat.Types.BySett$MPAID==19,c("SettlementID", "SettlementName","Pollution", "DestructiveFishing", "IllegalFishing", 
                                                                "ClimateChange", "HabitalLoss", "NaturalProcesses", "OtherMarineUses", 
                                                                 "Other")]

Yamdena.SBS.ThreatTypes.BySett <- 
  Yamdena.SBS.ThreatTypes.BySett[rev(order(Yamdena.SBS.ThreatTypes.BySett$SettlementName)),]


Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT <- 
  rbind.data.frame(Yamdena.level.SBS.ThreatTypes[c("SettlementID", "SettlementName","Pollution", "DestructiveFishing", "IllegalFishing", 
                                                   "ClimateChange", "HabitalLoss", "NaturalProcesses", "OtherMarineUses", 
                                                   "Other")],
                   null.row.ThreatTypes[c("SettlementID", "SettlementName","Pollution", "DestructiveFishing", "IllegalFishing", 
                                       "ClimateChange", "HabitalLoss", "NaturalProcesses", "OtherMarineUses", 
                                       "Other")],
                   Yamdena.SBS.ThreatTypes.BySett)

# rowSums(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT [,3:12])

# - make SettlementName an ordered factor for plotting
Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName <-
  ifelse(is.na(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName),"",
         as.character(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName))

Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName <-
  factor(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName,
         levels=unique(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName),
         ordered=T)

# - add row for plot fill colour formatting
Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$Dummy <- 
  ifelse(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT$SettlementName=="","Dummy","NotDummy")

rowSums(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT [,3:10])


write.csv(Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT, "Yamdena.SBSPropData.ThreatTypes.PLOTFORMAT.csv")

