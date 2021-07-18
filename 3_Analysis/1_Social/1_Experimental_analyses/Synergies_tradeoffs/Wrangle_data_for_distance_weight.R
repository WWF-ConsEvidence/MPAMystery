# 
# code: create the input table for gravity weighting reef sites to settlements
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2020
# modified: 
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

pacman::p_load(rio, dplyr)

source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files.R')


# ---- 1.1 Import flat files ----

# Settlement level coordinates
sett.coords <- import('x_Flat_data_files/1_Social/Inputs/soc.coord.province.csv') %>%
  left_join(Settlements[,c("SettlementID","MPAID")], by="SettlementID") %>%
  mutate(MPAID = ifelse(SettlementID %in% c(113,82,81,83,84),7,
                        ifelse(SettlementID %in% c(114,115,93,94,92),8,
                               ifelse(SettlementID %in% c(85:90,95,91),9,MPAID))))


# Reef level coordinates
reef.coords <- import('x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/eco.sites.included.synergies.csv') %>%
  mutate(MPAID = ifelse(grepl("Mayalibit",MPA_Name),1,
                        ifelse(grepl("Cenderawasih",MPA_Name),2,
                               ifelse(grepl("Kofiau",MPA_Name),4,
                                      ifelse(grepl("Dampier",MPA_Name),5,
                                             ifelse(grepl("Misool",MPA_Name),6,
                                                    ifelse(grepl("Buruway",MPA_Name),7,
                                                           ifelse(grepl("Kaimana",MPA_Name),9,
                                                                  ifelse(grepl("Selat Pantar",MPA_Name),15,
                                                                         ifelse(grepl("Flores Timur",MPA_Name),16,
                                                                                ifelse(grepl("Kei Kecil",MPA_Name),17,
                                                                                       ifelse(grepl("Koon",MPA_Name),18,NA))))))))))))


# Settlement impacts
sett.impacts <- import('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/settlevel_impacts_1-3match_20201218.csv') %>%
  filter(term=="Impact" & !is.na(estimate) & Response=="FSIndex_z" & (time=="t2" | time=="t3") & MPAID!=8)
  # filter list of impacts so that each settlement is only listed once (instead of for each variable, year, etc.)
  # also, remove Teluk Etna MPA (MPAID==8) since there are no eco sites in Etna


# Eco impacts
eco.impacts <- import('x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/eco.impacts.20210409.csv')


# ---- 1.2 Identify reef sites with eco impacts associated ----

sites <- 
  left_join(eco.impacts[,c("Site_ID", "MPA_Name")], 
            reef.coords[,c("Site_ID","latitude","longitude","MPAID","t0","t1")], 
            by = "Site_ID") %>%
  rename("SiteID" = "Site_ID") %>%
  .[order(.$MPAID),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: WRANGLE INTO PROPER FORMAT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Identify number of sites and settlements per MPA

nsites.byMPA <-
  sites %>%
  group_by(MPAID) %>%
  summarise(nsites = length(SiteID))

nsetts.byMPA <-
  sett.impacts %>%
  group_by(MPAID) %>%
  summarise(nsetts = length(SettlementID))

n.byMPA <-
  left_join(nsites.byMPA, nsetts.byMPA, by="MPAID") %>%
  mutate(nsites.cumsum = cumsum(nsites))
  

# ---- 2.2 Identify settlements included in weighting analysis ----

setts.forweighting <- 
  left_join(sett.impacts[,c("SettlementID","MPAID","lat","long")], nsites.byMPA, by = "MPAID") %>%
  .[order(.$MPAID),]

duplicate.settrows <- 
  setts.forweighting[rep(1:nrow(setts.forweighting), setts.forweighting$nsites),] %>%
  dplyr::select(-MPAID, -nsites) %>%
  rename("lat.y" = "lat",
         "long.y" = "long")


# ---- 2.3 Remove duplicate eco site rows ----

duplicate.ecorows <- 
  rbind.data.frame(sites[rep(1:n.byMPA$nsites.cumsum[1],n.byMPA$nsetts[1]),],
                   sites[rep((n.byMPA$nsites.cumsum[1]+1):n.byMPA$nsites.cumsum[2],n.byMPA$nsetts[2]),],
                   sites[rep((n.byMPA$nsites.cumsum[2]+1):n.byMPA$nsites.cumsum[3],n.byMPA$nsetts[3]),],
                   sites[rep((n.byMPA$nsites.cumsum[3]+1):n.byMPA$nsites.cumsum[4],n.byMPA$nsetts[4]),],
                   sites[rep((n.byMPA$nsites.cumsum[4]+1):n.byMPA$nsites.cumsum[5],n.byMPA$nsetts[5]),],
                   sites[rep((n.byMPA$nsites.cumsum[5]+1):n.byMPA$nsites.cumsum[6],n.byMPA$nsetts[6]),],
                   sites[rep((n.byMPA$nsites.cumsum[6]+1):n.byMPA$nsites.cumsum[7],n.byMPA$nsetts[7]),],
                   sites[rep((n.byMPA$nsites.cumsum[7]+1):n.byMPA$nsites.cumsum[8],n.byMPA$nsetts[8]),],
                   sites[rep((n.byMPA$nsites.cumsum[8]+1):n.byMPA$nsites.cumsum[9],n.byMPA$nsetts[9]),],
                   sites[rep((n.byMPA$nsites.cumsum[9]+1):n.byMPA$nsites.cumsum[10],n.byMPA$nsetts[10]),],
                   sites[rep((n.byMPA$nsites.cumsum[10]+1):n.byMPA$nsites.cumsum[11],n.byMPA$nsetts[11]),]) %>%
  dplyr::select(-MPA_Name, -t0, -t1) %>%
  rename("lat.x" = "latitude",
         "long.x" = "longitude")


# ---- 2.4 Create data.for.weighting data frame that will feed directly into the distance weighting script ----

data.for.weighting <-
  cbind.data.frame(duplicate.ecorows, duplicate.settrows)


# export(data.for.weighting, 'x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/data.for.weighting.KC.csv')
