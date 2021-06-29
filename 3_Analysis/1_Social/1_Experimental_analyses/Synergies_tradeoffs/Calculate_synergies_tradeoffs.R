# 
# code: calculate settlement level synergies and tradeoffs across BHS and SBS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2020
# modified: February 2021
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- Load libraries ----

pacman::p_load(rio, car, lfe, corrplot, grid, gridExtra, ggplot2, ordinal, lme4, lmerTest, dplyr)

# define function for scaling (but not centering around mean) -- used for eco and social impacts
scale2 <- function(x, na.rm = TRUE) (x) / sd(x, na.rm = TRUE)
stdz <- function(x, na.rm = TRUE) (x - mean(x, na.rm = TRUE)) / (2*sd(x, na.rm = TRUE))

# ---- 1.1 Import flat files ----

# import eco impacts
eco.impacts <- import('x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/eco.impacts.20210409.csv') %>%
  rename("SiteID" = "Site_ID",
         "hard_coral_impact" = "coral.adj.impact",
         "macroalgae_cover_impact" = "algae.adj.impact",
         "total_biomass_impact" = "total.biomass.adj.impact",
         "herb_biomass_impact" = "herb.biomass.adj.impact",
         "key_biomass_impact" = "key.biomass.adj.impact") %>%
  dplyr::select(-V1) %>%
  mutate(MPAID = ifelse(grepl("Mayalibit", MPA_Name), 1, 
                        ifelse(grepl("Cenderawasih", MPA_Name), 2,
                               ifelse(grepl("Kofiau", MPA_Name), 4, 
                                      ifelse(grepl("Dampier", MPA_Name), 5, 
                                             ifelse(grepl("Misool", MPA_Name), 6,
                                                    ifelse(grepl("Buruway", MPA_Name), 7,
                                                           ifelse(grepl("Kaimana Kota", MPA_Name), 9, 
                                                                  ifelse(grepl("Selat Pantar", MPA_Name), 15,
                                                                         ifelse(grepl("Flores Timur", MPA_Name), 16, 
                                                                                ifelse(grepl("Kei Kecil", MPA_Name), 17, 
                                                                                       ifelse(grepl("Koon", MPA_Name), 18, NA))))))))))),
         
         Zoned = ifelse(MPAID%in%c(1, 2, 4, 5, 6, 15, 17), 1, 0),
         Type_of_Zone = ifelse(Zoned==0, "Use", Type_of_Zone))

# **plot macroalgae impacts against grazer impacts and if we see a tightly
#   linear relationship (negative), then it makes more sense.  

# import settlement impacts
sett.impacts <- import('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/settlevel_impacts_1-3match_20210510.csv') %>%
  filter(MPAID!=8 & !is.na(estimate)) 

# import distances from sites to settlements (for calculating weighted eco impacts)
site.dists <- import('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/Gravity_weights_allsites_persett_20210409.csv') %>%
  left_join(Settlements[,c("SettlementID","SettlementName","MPAID")], by="SettlementID") %>%
  mutate(MPAID = ifelse(SettlementID %in% c(113,82,81,83,84),7,
                        ifelse(SettlementID %in% c(114,115,93,94,92),8,
                               ifelse(SettlementID %in% c(85:90,95,91),9,MPAID))))



# ---- 1.2 Play with buffer for matching reef sites to settlements ----
check <- site.dists %>% filter(Distance_m<=50000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))


# -- 50km is a good buffer that maintains many reef sites, but also is arguably a day's worth of travel for fishing (which is possible in some MPAs)
site.dists.50kmbuffer <-
  site.dists %>% filter(Distance_m<=50000) %>%
  group_by(SettlementID, MPAID) %>%
  mutate(total_dist = sum(1/(Distance_m^2))) %>%
  ungroup() %>%
  mutate(Relative_Weight_new = (1/(Distance_m^2))/total_dist)


# ---- 1.3 Find minimum and maximum distances of reef sites for 50km buffer match ----

check.dists.50kmbuffer <-
  site.dists.50kmbuffer %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm=T)/1000)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CALCULATE SYNTHETIC ECO IMPACTS & COMPILE DATA----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Calculate eco impacts per settlement & per MPA ----

eco.impacts.cols <- c("total_biomass_impact", "herb_biomass_impact", "key_biomass_impact", 
                      "hard_coral_impact", "macroalgae_cover_impact")

# synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
ecoimpacts.bysett <- 
  left_join(site.dists, eco.impacts, by="SiteID") %>%
  mutate(total_biomass_impact_w = Relative_Weight * total_biomass_impact,
         herb_biomass_impact_w = Relative_Weight * herb_biomass_impact,
         key_biomass_impact_w = Relative_Weight * key_biomass_impact,
         hard_coral_impact_w = Relative_Weight * hard_coral_impact,
         macroalgae_cover_impact_w = Relative_Weight * macroalgae_cover_impact) %>%
  dplyr::group_by(SettlementID) %>%
  dplyr::summarise(total_biomass_impact = sum(total_biomass_impact_w),
                   herb_biomass_impact = sum(herb_biomass_impact_w),
                   key_biomass_impact = sum(key_biomass_impact_w),
                   hard_coral_impact = sum(hard_coral_impact_w),
                   macroalgae_cover_impact = sum(macroalgae_cover_impact_w),
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))


# MPA level impacts
ecoimpacts.byMPA <-
  left_join(site.dists, eco.impacts, by="SiteID") %>%
  group_by(MPA_Name) %>%
  summarise(total_biomass_impact = mean(total_biomass_impact, na.rm=T),
            herb_biomass_impact = mean(herb_biomass_impact, na.rm=T),
            key_biomass_impact = mean(key_biomass_impact, na.rm=T),
            hard_coral_impact = mean(hard_coral_impact, na.rm=T),
            macroalgae_cover_impact = mean(macroalgae_cover_impact, na.rm=T),
            PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2)) %>%
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
  
  

# ---- 2.2 Compile social impact and covariate data, per settlement ----

FGD.user.data <-
  import('x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/FGD_Users_20210106.csv') %>%
  .[,1:21] %>%
  filter(SiteCode%in%c(1:18)) %>%
  group_by(SettlementCode) %>%
  mutate(Earliest = min(YEAR, na.rm=T),
         ParticipateEstablish = ifelse(ParticipateEstablish>1 | ParticipateEstablish=="-", NA, as.numeric(ParticipateEstablish)),
         ParticipateBoundaries = ifelse(ParticipateBoundaries>1, NA, ParticipateBoundaries),
         ParticipateAdmin = ifelse(ParticipateAdmin>1, NA, ParticipateAdmin),
         ParticipateRules = ifelse(ParticipateRules>1 | ParticipateRules=="-", NA, as.numeric(ParticipateRules))) %>%
  ungroup() %>%
  filter(YEAR==Earliest) %>%
  # mutate(across(c("ParticipateEstablish", "ParticipateBoundaries", "ParticipateAdmin", "ParticipateRules"), scale2)) %>%
  mutate(Part.DecisionMaking = rowMeans(.[,c("ParticipateEstablish", "ParticipateBoundaries", 
                                             "ParticipateAdmin", "ParticipateRules")],
                                        na.rm = T)) %>%
  group_by(SettlementCode, SiteCode, YEAR) %>%
  summarise(ParticipateDecisionMaking = mean(Part.DecisionMaking, na.rm = T),
            Establish = mean(ParticipateEstablish),
            PropRules = length(ParticipateRules[ParticipateRules==1 & !is.na(ParticipateRules)])/length(ParticipateRules[!is.na(ParticipateRules)]),
            NUsersAnswerRules = length(ParticipateRules[!is.na(ParticipateRules)]),
            NUsers = length(SettlementCode)) %>%
  ungroup() %>%
  rename("SettlementID" = "SettlementCode")




KII.mpa.data <-
  import('x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/KII_MPA_20210106.csv') %>%
  .[,1:30] %>%
  group_by(SettlementCode) %>%
  mutate(YEAR = ifelse(`INTERVIEW YEAR`==2106, 2016, `INTERVIEW YEAR`),
         Earliest = min(YEAR, na.rm=T),
         DRuleEco = ifelse(DRuleEco>5, NA, DRuleEco),
         DRuleSoc = ifelse(DRuleSoc>5 | DRuleSoc<1, NA, DRuleSoc)) %>%
  ungroup() %>%
  filter(YEAR==Earliest) %>%
  mutate(Rule.Congruence = rowMeans(.[,c("DRuleEco", "DRuleSoc")], na.rm = T)) %>%
  group_by(SettlementCode, SiteCode, YEAR) %>%
  summarise(RuleCongruence = mean(Rule.Congruence, na.rm = T),
            NKII = length(SettlementCode)) %>%
  ungroup() %>%
  rename("SettlementID" = "SettlementCode")
  
summary(KII.mpa.data$RuleCongruence)
hist(KII.mpa.data$RuleCongruence, breaks = 10)


settlevel.covariates <- 
  HHData %>%
  mutate(PrimaryFish = ifelse(PrimaryLivelihood==3, 1, ifelse(is.na(PrimaryLivelihood), NA, 0))) %>%
  mutate(across(c("FreqEatFish", "FreqSaleFish", "PercentIncFish", "PrimaryFish", "FreqFish"), stdz)) %>%
  mutate(MarineReliance = rowMeans(dplyr::select(.,c("FreqEatFish", "FreqSaleFish", "PercentIncFish",
                                              "PrimaryFish", "FreqFish")), na.rm = T),
         MgmtRightsIndex = rowSums(dplyr::select(.,"RightsManage", "RightsTransfer", "RightsExclude"), na.rm=T),
         HarvestRightsIndex = rowSums(dplyr::select(.,"RightsAccess", "RightsHarvest"), na.rm=T)) %>%
  group_by(SettlementID) %>%
  mutate(MostRecent = max(InterviewYear),
         Baseline = min(InterviewYear)) %>%
  ungroup() %>%
  group_by(SettlementID, InterviewYear, MostRecent, Baseline) %>%
  summarise(TimeMarket=mean(TimeMarket,na.rm=T),
            MTManage=mean(RightsManage,na.rm=T),
            MTHarvest=mean(RightsHarvest,na.rm=T),
            MTAccess=mean(RightsAccess,na.rm=T),
            MTTransfer=mean(RightsTransfer,na.rm=T),
            MTExclude=mean(RightsExclude,na.rm=T),
            MgmtRights = length(SettlementID[MgmtRightsIndex>0 & !is.na(MgmtRightsIndex)])/length(SettlementID[!is.na(MgmtRightsIndex)]),
            HarvestRights = length(SettlementID[HarvestRightsIndex>0 & !is.na(HarvestRightsIndex)])/length(SettlementID[!is.na(HarvestRightsIndex)]),
            MarineReliance = mean(MarineReliance, na.rm = T),
            FSMean = mean(FSIndex, na.rm = T),
            MAMean = mean(MAIndex, na.rm = T)) %>%
  ungroup() %>%
  group_by(SettlementID) %>% # choose which monitoring year of data to pull for each covariate (e.g., baseline or most recent)
  summarise(TimeMarket = TimeMarket[InterviewYear==MostRecent],
            MgmtRights = MgmtRights[InterviewYear==Baseline],
            MarineReliance = MarineReliance[InterviewYear==Baseline],
            FSMean = FSMean[InterviewYear==Baseline],
            MAMean_trend = MAMean[InterviewYear==MostRecent] - MAMean[InterviewYear==Baseline],
            MAMean = MAMean[InterviewYear==Baseline]) %>%
  ungroup() %>%
  mutate(TimeMarket_z = stdz(TimeMarket),
         MgmtRights_z = stdz(MgmtRights),
         MarineReliance_z = stdz(MarineReliance),
         FSIndex_t0_z = stdz(FSMean),
         MAIndex_trend_z = stdz(MAMean_trend))

summary(settlevel.covariates$MAIndex_trend_z)
hist(settlevel.covariates$MarineReliance_z, breaks = 20)


settimpacts.reshape <-
  sett.impacts %>% mutate(most_recent = ifelse(MPAID%in%c(1:9), "t4",
                                               ifelse(MPAID%in%c(15:17), "t3",
                                                      ifelse(MPAID==18, "t2", NA))),
                          longest_impact = ifelse(time==most_recent, "longest", "not_longest"),
                          time = ifelse(time=="t2" | time=="t3", "t2.3", time)) %>%
  tidyr::pivot_wider(., id_cols = c(SettlementID, SettlementName, MPAID), 
                     names_from = c(Response, longest_impact), values_from = estimate)


sett.impacts.cols <- c("FSIndex_not_longest", "FSIndex_longest", "MAIndex_not_longest", "MAIndex_longest",
                       "MTIndex_not_longest", "MTIndex_longest", "PAIndex_not_longest", "PAIndex_longest",
                       "SERate_not_longest", "SERate_longest")


settimpacts.reshape <-
  settimpacts.reshape %>%
  mutate(across(all_of(sett.impacts.cols), scale2))


# ---- 2.5 Compile impacts data frame with 50km buffer ----

# synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
ecoimpacts.bysett.50kmbuffer <- 
  left_join(site.dists.50kmbuffer, eco.impacts, by = "SiteID") %>%
  mutate(total_biomass_impact_w = Relative_Weight * total_biomass_impact,
         herb_biomass_impact_w = Relative_Weight * herb_biomass_impact,
         key_biomass_impact_w = Relative_Weight * key_biomass_impact,
         hard_coral_impact_w = Relative_Weight * hard_coral_impact,
         macroalgae_cover_impact_w = Relative_Weight * macroalgae_cover_impact) %>%
  dplyr::group_by(SettlementID) %>%
  dplyr::summarise(total_biomass_impact = sum(total_biomass_impact_w),
                   herb_biomass_impact = sum(herb_biomass_impact_w),
                   key_biomass_impact = sum(key_biomass_impact_w),
                   hard_coral_impact = sum(hard_coral_impact_w),
                   macroalgae_cover_impact = sum(macroalgae_cover_impact_w),
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))

impacts.bysett.50kmbuffer <-
  left_join(settimpacts.reshape, ecoimpacts.bysett.50kmbuffer, by="SettlementID") %>%
  left_join(settlevel.covariates, by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(check.dists.50kmbuffer, by = c("SettlementID", "MPAID")) %>%
  left_join(MPA.name[,c("MPAID","MPAName")], by = "MPAID") %>%
  mutate(change_status = ifelse(MPAID%in%c(1, 4, 5, 6, 15, 17), 1, 0),
         soc_yrpost_zone = ifelse(MPAID%in%c(4, 6, 17), 3, #Kofiau, Misool, and Kei all have social data that goes 3 years past zoning  
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have social data post zonation
                                         ifelse(MPAID%in%c(1, 15), 2, #Mayalibit and Alor have social data 2 years post zonation
                                                ifelse(MPAID==5, 4, #Dampier has social data 4 years post zonation
                                                       ifelse(MPAID==2, 12, NA))))), #TNTC has social data 12 years post zonation
         eco_yrpost_zone = ifelse(MPAID%in%c(1, 4, 5), 4, #Mayalibit, Kofiau, and Dampier all have eco data 4 years post zonation
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have eco data post zonation
                                         ifelse(MPAID%in%c(15, 17), 2, #Alor and Kei have eco data 2 years post zonation
                                                ifelse(MPAID==6, 3, #Misool has eco data 3 years post zonation
                                                       ifelse(MPAID==2, 14, NA))))), #TNTC has eco data 14 years post zonation
         soc_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline social data that pre-dates establishment (with Kei Kecil having baseline data from the same year as establishment)
                                       ifelse(MPAID%in%c(1, 7, 9), 4, #Mayalibit, Buruway, and Triton have baseline social data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6), 5, #Kofiau and Misool have baseline social data from 5 years post establishment
                                                     ifelse(MPAID==5, 6, #Dampier has baseline social data from 6 years post establishment
                                                            ifelse(MPAID==2, 8, NA))))), #TNTC has baseline social data from 8 years post establishment
         eco_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline eco data that pre-dates establishment
                                       ifelse(MPAID%in%c(5, 7), 4, #Dampier and Buruway have baseline eco data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6, 9), 5, #Kofiau, Misool, and Triton all have baseline eco data from 5 years post establishment
                                                     ifelse(MPAID==1, 6, #Mayalibit has basline eco data from 6 years post establishment
                                                            ifelse(MPAID==2, 9, NA))))), #TNTC has baseline eco data from 9 years post establishment
         MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_herbbio_cat = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_trdoff = ifelse((FSIndex_longest>0 & herb_biomass_impact<0) |
                                        (FSIndex_longest<0 & herb_biomass_impact>0), 1, 0),
         MarineReliance_cat = factor(ifelse(MarineReliance>=0, "high", "low")),
         MPA_MarineReliance_cat = factor(paste0(MPAID, MarineReliance_cat, sep = "_")))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: DESCRIPTIVE STATISTICS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Impact info at MPA level  (number of eco sites, settlements, summary stats of impacts) ----

# -- Herb biomass impact data, MPA level (with impacts being summarised from site level to MPA level)
eco.site.info <-
  eco.impacts %>%
  mutate(herb_biomass_impact = scale2(herb_biomass_impact)) %>%
  group_by(MPAID) %>%
  summarise(numsites_perMPA = length(SiteID),
            mean_herb_biomass_impact = mean(herb_biomass_impact, na.rm = T),
            median_herb_biomass_impact = median(herb_biomass_impact, na.rm = T),
            min_herb_biomass_impact = min(herb_biomass_impact, na.rm = T),
            max_herb_biomass_impact = max(herb_biomass_impact, na.rm = T),
            sd_herb_biomass_impact = sd(herb_biomass_impact, na.rm = T))

# -- Food security impact data, MPA level(with impacts being summarised from settlement level to MPA level)
soc.sett.info <-
  sett.impacts %>%
  mutate(most_recent = ifelse(MPAID%in%c(1:9), "t4",
                              ifelse(MPAID%in%c(15:17), "t3",
                                     ifelse(MPAID==18, "t2", NA)))) %>%
  filter(Response=="FSIndex" & time==most_recent) %>%
  group_by(MPAID) %>%
  summarise(numsetts_perMPA = length(SettlementID),
            mean_FS_impact = mean(estimate, na.rm = T),
            median_FS_impact = median(estimate, na.rm = T),
            min_FS_impact = min(estimate, na.rm = T),
            max_FS_impact = max(estimate, na.rm = T),
            sd_FS_impact = sd(estimate, na.rm = T))

# -- Joined MPA level impact data
MPA.level.info <-
  left_join(eco.site.info, soc.sett.info, by = "MPAID") %>%
  left_join(MPA.name[,c("MPAID", "MPAName")], by = "MPAID") %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))



# ---- 3.2 Settlement to site match info (how many sites per sett, avg distance, etc.) ----

Sett.level.match.info <- 
  left_join(check, check.dists.50kmbuffer, by = c("SettlementID", "MPAID"))

Site.level.match.info <-
  site.dists.50kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

MPA.level.info <-
  Site.level.match.info %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID)) %>%
  left_join(MPA.level.info, ., by = "MPAID")


# ---- 3.3 Proportion of synergies vs. tradeoffs per MPA ----

impacts.50kmbuffer.byMPA <- 
  impacts.bysett.50kmbuffer %>%
  group_by(MPAID) %>%
  summarise(mean_dist = mean(mean_dist_km, na.rm = T),
            sd_dist = sd(sd_dist_km, na.rm = T),
            prop_negeco_impact = length(SettlementID[herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsoc_impact = length(SettlementID[FSIndex_longest<0 & !is.na(FSIndex_longest)])/length(SettlementID[!is.na(FSIndex_longest)]),
            prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

impacts.50kmbuffer.all <-
  impacts.bysett.50kmbuffer %>%
  summarise(prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: REGRESSION MODELS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 4.1 Correlations: covariates for all potential models ----

corrmatrix.potentialcovariates <- cor(impacts.bysett.50kmbuffer[,c("TimeMarket_z", "MarineReliance_z", "MgmtRights_z", "ParticipateDecisionMaking", "RuleCongruence", "FSIndex_t0_z", "MAIndex_trend_z", 
                                                                   "mean_dist_km", "PropNTZ", "change_status", "soc_yrpost_zone", "eco_yrpost_zone",
                                                                   "soc_yrpost_establish", "eco_yrpost_establish")],
                                      use = "complete.obs")
corrplot.potentialcovariates <- corrplot(corrmatrix.potentialcovariates)


corrmatrix.chosencovariates <- cor(impacts.bysett.50kmbuffer[,c("TimeMarket_z", "MarineReliance_z", "MgmtRights_z", "FSIndex_t0_z", "MAIndex_trend_z",
                                                                "mean_dist_km", "soc_yrpost_zone","eco_yrpost_zone", "soc_yrpost_establish", "eco_yrpost_establish")],
                                   use = "complete.obs")
corrplot.chosencovariates <- corrplot(corrmatrix.chosencovariates)


# ---- 4.2 Model 1: Binomial logistic regression - Food security & herbivore fish biomass ----
# ---      Eco sites within 50km buffer (and same MPA) included in weighted match to settlement

# Chosen covariates
model.1a <- glm(foodsec_herbbio_cat ~ TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + 
                  MAIndex_trend_z + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish,
                data = impacts.bysett.50kmbuffer, family = "binomial")


summary(model.1a)
log.odds.1a <- model.1a$linear.predictors

# All potential covariates of interest (includes many NAs, which end up being dropped)
model.1b <- glm(foodsec_herbbio_cat ~ TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
                  RuleCongruence + PropNTZ + FSIndex_t0 + mean_dist_km + change_status,
                data = impacts.bysett.50kmbuffer, family = "binomial")

summary(model.1b)
log.odds.1b <- model.1b$linear.predictors


# ---- 4.3 Model 2: Linear mixed effects models for each outcome - Food security & herbivore fish biomass ----

# -- Food security
fit.FS.lmer <- lmer(FSIndex_longest ~ 
                    TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                  + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish + (1 | MPAID), 
                  data = impacts.bysett.50kmbuffer %>% select(FSIndex_longest, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                              MAIndex_trend_z, mean_dist_km, soc_yrpost_zone, soc_yrpost_establish, MPAID) %>% na.omit(),
                  REML = F)


fit.FS.lmer.summ <- summary(fit.FS.lmer)


# -- Herbivore fish biomass
fit.biomass.lmer <- lmer(herb_biomass_impact ~ 
                      TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                    + mean_dist_km + eco_yrpost_zone + eco_yrpost_establish + (1 | MPAID), 
                    data = impacts.bysett.50kmbuffer %>% select(herb_biomass_impact, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                MAIndex_trend_z, mean_dist_km, eco_yrpost_zone, eco_yrpost_establish, MPAID) %>% na.omit(),
                    REML = F)


fit.biomass.lmer.summ <- summary(fit.biomass.lmer)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: EXPERIMENTAL ANALYSES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# MANOVA and univariate ANOVA looking at MPAID and Marine Reliance categorizations
fit.MPAID <- manova(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest", "key_biomass_impact")])~
                      as.matrix(impacts.bysett.50kmbuffer[,c("MPA_MarineReliance_cat")]))
fit.MPAID.summ <- summary(fit.MPAID, test="Wilks")


fit.MPAID.soc <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest")])~
                          as.matrix(impacts.bysett.50kmbuffer[,c("MPA_MarineReliance_cat")]))
fit.MPAID.soc.summ <- summary(fit.MPAID.soc, test="Wilks")


fit.MPAID.eco <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("key_biomass_impact")])~
                          as.matrix(impacts.bysett.50kmbuffer[,c("MPA_MarineReliance_cat")]))
fit.MPAID.eco.summ <- summary(fit.MPAID.eco, test="Wilks")


fit.MarineReliance <- manova(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest", "key_biomass_impact")])~
                               as.matrix(impacts.bysett.50kmbuffer[,c("MarineReliance_cat")]))
fit.MarineReliance.summ <- summary(fit.MarineReliance, test="Wilks")


fit.MarineReliance.soc <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest")])~
                       as.matrix(impacts.bysett.50kmbuffer[,c("MarineReliance_cat")]))
fit.MarineReliance.soc.summ <- summary(fit.MarineReliance.soc, test="Wilks")


fit.MarineReliance.eco <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("key_biomass_impact")])~
                       as.matrix(impacts.bysett.50kmbuffer[,c("MarineReliance_cat")]))
fit.MarineReliance.eco.summ <- summary(fit.MarineReliance.eco, test="Wilks")


# MULTIVARIATE MULTIPLE LINEAR REGRESSION
pacman::p_load(car)

fit.FS.biomass.lm.1 <- lm(cbind(FSIndex_longest, key_biomass_impact) ~ 
                     TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
                     RuleCongruence + PropNTZ + FSIndex_t0 + mean_dist_km,
                data = impacts.bysett.50kmbuffer)
fit.FS.biomass.lm.summ.1 <- summary(fit.FS.biomass.lm.1)

fit.FS.biomass.lm.2 <- lm(cbind(FSIndex_z_longest, key_biomass_impact) ~ FSIndex_t0,
                          data = impacts.bysett.50kmbuffer)
fit.FS.biomass.lm.summ.2 <- summary(fit.FS.biomass.lm.2)

Anova(fit.FS.biomass.lm.2)

fit.eco.lm <- lm(macroalgae_cover_impact ~ 
                   TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
                   RuleCongruence + PropNTZ + FSIndex_t0 + mean_dist_km,
                 data = impacts.bysett.50kmbuffer)
fit.eco.lm.summ <- summary(fit.eco.lm)

fit.multi.lm <- lm(cbind(FSIndex_z_longest, key_biomass_impact) ~ 
                     TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
                     RuleCongruence + PropNTZ + FSIndex_t0 + mean_dist_km,
                   data = impacts.bysett.50kmbuffer)


# PCA
pacman::p_load(factoextra)

FS.keybio.pca <- 
  prcomp(impacts.bysett.50kmbuffer[,c("FSIndex_longest", "herb_biomass_impact")] %>% 
           filter(!is.na(herb_biomass_impact) & !is.na(FSIndex_longest)), 
         scale = T)

fviz_eig(FS.keybio.pca)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: SENSITIVITY TESTING ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 6.1 20km buffer - full analysis using 20km ----

# -- Define site.dists with buffer
site.dists.20kmbuffer <-
  site.dists %>% filter(Distance_m<=20000) %>%
  group_by(SettlementID, MPAID) %>%
  mutate(total_dist = sum(1/(Distance_m^2))) %>%
  ungroup() %>%
  mutate(Relative_Weight_new = (1/(Distance_m^2))/total_dist)


# -- Find minimum and maximum distances of reef sites for 20km buffer match
check.20km <- site.dists %>% filter(Distance_m<=20000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))

check.dists.20kmbuffer <-
  site.dists.20kmbuffer %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm=T)/1000)


# -- Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
ecoimpacts.bysett.20kmbuffer <- 
  left_join(site.dists.20kmbuffer, eco.impacts, by = "SiteID") %>%
  mutate(total_biomass_impact_w = Relative_Weight * total_biomass_impact,
         herb_biomass_impact_w = Relative_Weight * herb_biomass_impact,
         key_biomass_impact_w = Relative_Weight * key_biomass_impact,
         hard_coral_impact_w = Relative_Weight * hard_coral_impact,
         macroalgae_cover_impact_w = Relative_Weight * macroalgae_cover_impact) %>%
  dplyr::group_by(SettlementID) %>%
  dplyr::summarise(total_biomass_impact = sum(total_biomass_impact_w),
                   herb_biomass_impact = sum(herb_biomass_impact_w),
                   key_biomass_impact = sum(key_biomass_impact_w),
                   hard_coral_impact = sum(hard_coral_impact_w),
                   macroalgae_cover_impact = sum(macroalgae_cover_impact_w),
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))

impacts.bysett.20kmbuffer <-
  left_join(settimpacts.reshape, ecoimpacts.bysett.20kmbuffer, by="SettlementID") %>%
  left_join(settlevel.covariates, by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(check.dists.20kmbuffer, by = c("SettlementID", "MPAID")) %>%
  left_join(MPA.name[,c("MPAID","MPAName")], by = "MPAID") %>%
  mutate(change_status = ifelse(MPAID%in%c(1, 4, 5, 6, 15, 17), 1, 0),
         soc_yrpost_zone = ifelse(MPAID%in%c(4, 6, 17), 3, #Kofiau, Misool, and Kei all have social data that goes 3 years past zoning  
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have social data post zonation
                                         ifelse(MPAID%in%c(1, 15), 2, #Mayalibit and Alor have social data 2 years post zonation
                                                ifelse(MPAID==5, 4, #Dampier has social data 4 years post zonation
                                                       ifelse(MPAID==2, 12, NA))))), #TNTC has social data 12 years post zonation
         eco_yrpost_zone = ifelse(MPAID%in%c(1, 4, 5), 4, #Mayalibit, Kofiau, and Dampier all have eco data 4 years post zonation
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have eco data post zonation
                                         ifelse(MPAID%in%c(15, 17), 2, #Alor and Kei have eco data 2 years post zonation
                                                ifelse(MPAID==6, 3, #Misool has eco data 3 years post zonation
                                                       ifelse(MPAID==2, 14, NA))))), #TNTC has eco data 14 years post zonation
         soc_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline social data that pre-dates establishment (with Kei Kecil having baseline data from the same year as establishment)
                                       ifelse(MPAID%in%c(1, 7, 9), 4, #Mayalibit, Buruway, and Triton have baseline social data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6), 5, #Kofiau and Misool have baseline social data from 5 years post establishment
                                                     ifelse(MPAID==5, 6, #Dampier has baseline social data from 6 years post establishment
                                                            ifelse(MPAID==2, 8, NA))))), #TNTC has baseline social data from 8 years post establishment
         eco_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline eco data that pre-dates establishment
                                       ifelse(MPAID%in%c(5, 7), 4, #Dampier and Buruway have baseline eco data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6, 9), 5, #Kofiau, Misool, and Triton all have baseline eco data from 5 years post establishment
                                                     ifelse(MPAID==1, 6, #Mayalibit has basline eco data from 6 years post establishment
                                                            ifelse(MPAID==2, 9, NA))))), #TNTC has baseline eco data from 9 years post establishment
         MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_herbbio_cat = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_trdoff = ifelse((FSIndex_longest>0 & herb_biomass_impact<0) |
                                           (FSIndex_longest<0 & herb_biomass_impact>0), 1, 0),
         MarineReliance_cat = factor(ifelse(MarineReliance>=0, "high", "low")),
         MPA_MarineReliance_cat = factor(paste0(MPAID, MarineReliance_cat, sep = "_")))


# -- Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.)
Sett.level.match.info.20km <- 
  left_join(check.20km, check.dists.20kmbuffer, by = c("SettlementID", "MPAID"))

Site.level.match.info.20km <-
  site.dists.20kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

MPA.level.info.20km <-
  Site.level.match.info.20km %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID))


# -- Proportion of synergies vs. tradeoffs per MPA
impacts.20kmbuffer.byMPA <- 
  impacts.bysett.20kmbuffer %>%
  group_by(MPAID) %>%
  summarise(mean_dist = mean(mean_dist_km, na.rm = T),
            sd_dist = sd(sd_dist_km, na.rm = T),
            prop_negeco_impact = length(SettlementID[herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsoc_impact = length(SettlementID[FSIndex_longest<0 & !is.na(FSIndex_longest)])/length(SettlementID[!is.na(FSIndex_longest)]),
            prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

impacts.20kmbuffer.all <-
  impacts.bysett.20kmbuffer %>%
  summarise(prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

# -- Regression models

# Model 1: Binomial logistic regression - Food security & herbivore fish biomass ----
# ---      Eco sites within 25km buffer (and same MPA) included in weighted match to settlement

# Chosen covariates
model.1a.20km <- glm(foodsec_herbbio_cat ~ TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + 
                       MAIndex_trend_z + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish,
                     data = impacts.bysett.20kmbuffer, family = "binomial")


summary(model.1a.20km)


# Model 2: Linear mixed effects models for each outcome - Food security & herbivore fish biomass ----
# -- Food security
fit.FS.20km.lmer <- lmer(FSIndex_longest ~ 
                      TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                    + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish + (1 | MPAID), 
                    data = impacts.bysett.20kmbuffer %>% select(FSIndex_longest, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                MAIndex_trend_z, mean_dist_km, soc_yrpost_zone, soc_yrpost_establish, MPAID) %>% na.omit(),
                    REML = F)


fit.FS.20km.lmer.summ <- summary(fit.FS.20km.lmer)


# -- Herbivore fish biomass
fit.biomass.20km.lmer <- lmer(herb_biomass_impact ~ 
                           TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                         + mean_dist_km + eco_yrpost_zone + eco_yrpost_establish + (1 | MPAID), 
                         data = impacts.bysett.20kmbuffer %>% select(herb_biomass_impact, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                     MAIndex_trend_z, mean_dist_km, eco_yrpost_zone, eco_yrpost_establish, MPAID) %>% na.omit(),
                         REML = F)


fit.biomass.20km.lmer.summ <- summary(fit.biomass.20km.lmer)



# ---- 6.2 25km buffer - full analysis using 25km ----

# -- Define site.dists with buffer
site.dists.25kmbuffer <-
  site.dists %>% filter(Distance_m<=25000) %>%
  group_by(SettlementID, MPAID) %>%
  mutate(total_dist = sum(1/(Distance_m^2))) %>%
  ungroup() %>%
  mutate(Relative_Weight_new = (1/(Distance_m^2))/total_dist)


# -- Find minimum and maximum distances of reef sites for 25km buffer match
check.25km <- site.dists %>% filter(Distance_m<=25000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))

check.dists.25kmbuffer <-
  site.dists.25kmbuffer %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm=T)/1000)


# -- Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
ecoimpacts.bysett.25kmbuffer <- 
  left_join(site.dists.25kmbuffer, eco.impacts, by = "SiteID") %>%
  mutate(total_biomass_impact_w = Relative_Weight * total_biomass_impact,
         herb_biomass_impact_w = Relative_Weight * herb_biomass_impact,
         key_biomass_impact_w = Relative_Weight * key_biomass_impact,
         hard_coral_impact_w = Relative_Weight * hard_coral_impact,
         macroalgae_cover_impact_w = Relative_Weight * macroalgae_cover_impact) %>%
  dplyr::group_by(SettlementID) %>%
  dplyr::summarise(total_biomass_impact = sum(total_biomass_impact_w),
                   herb_biomass_impact = sum(herb_biomass_impact_w),
                   key_biomass_impact = sum(key_biomass_impact_w),
                   hard_coral_impact = sum(hard_coral_impact_w),
                   macroalgae_cover_impact = sum(macroalgae_cover_impact_w),
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))

impacts.bysett.25kmbuffer <-
  left_join(settimpacts.reshape, ecoimpacts.bysett.25kmbuffer, by="SettlementID") %>%
  left_join(settlevel.covariates, by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(check.dists.25kmbuffer, by = c("SettlementID", "MPAID")) %>%
  left_join(MPA.name[,c("MPAID","MPAName")], by = "MPAID") %>%
  mutate(change_status = ifelse(MPAID%in%c(1, 4, 5, 6, 15, 17), 1, 0),
         soc_yrpost_zone = ifelse(MPAID%in%c(4, 6, 17), 3, #Kofiau, Misool, and Kei all have social data that goes 3 years past zoning  
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have social data post zonation
                                         ifelse(MPAID%in%c(1, 15), 2, #Mayalibit and Alor have social data 2 years post zonation
                                                ifelse(MPAID==5, 4, #Dampier has social data 4 years post zonation
                                                       ifelse(MPAID==2, 12, NA))))), #TNTC has social data 12 years post zonation
         eco_yrpost_zone = ifelse(MPAID%in%c(1, 4, 5), 4, #Mayalibit, Kofiau, and Dampier all have eco data 4 years post zonation
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have eco data post zonation
                                         ifelse(MPAID%in%c(15, 17), 2, #Alor and Kei have eco data 2 years post zonation
                                                ifelse(MPAID==6, 3, #Misool has eco data 3 years post zonation
                                                       ifelse(MPAID==2, 14, NA))))), #TNTC has eco data 14 years post zonation
         soc_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline social data that pre-dates establishment (with Kei Kecil having baseline data from the same year as establishment)
                                       ifelse(MPAID%in%c(1, 7, 9), 4, #Mayalibit, Buruway, and Triton have baseline social data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6), 5, #Kofiau and Misool have baseline social data from 5 years post establishment
                                                     ifelse(MPAID==5, 6, #Dampier has baseline social data from 6 years post establishment
                                                            ifelse(MPAID==2, 8, NA))))), #TNTC has baseline social data from 8 years post establishment
         eco_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline eco data that pre-dates establishment
                                       ifelse(MPAID%in%c(5, 7), 4, #Dampier and Buruway have baseline eco data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6, 9), 5, #Kofiau, Misool, and Triton all have baseline eco data from 5 years post establishment
                                                     ifelse(MPAID==1, 6, #Mayalibit has basline eco data from 6 years post establishment
                                                            ifelse(MPAID==2, 9, NA))))), #TNTC has baseline eco data from 9 years post establishment
         MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_herbbio_cat = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_trdoff = ifelse((FSIndex_longest>0 & herb_biomass_impact<0) |
                                           (FSIndex_longest<0 & herb_biomass_impact>0), 1, 0),
         MarineReliance_cat = factor(ifelse(MarineReliance>=0, "high", "low")),
         MPA_MarineReliance_cat = factor(paste0(MPAID, MarineReliance_cat, sep = "_")))


# -- Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.)
Sett.level.match.info.25km <- 
  left_join(check.25km, check.dists.25kmbuffer, by = c("SettlementID", "MPAID"))

Site.level.match.info.25km <-
  site.dists.25kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

MPA.level.info.25km <-
  Site.level.match.info.25km %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID))


# -- Proportion of synergies vs. tradeoffs per MPA
impacts.25kmbuffer.byMPA <- 
  impacts.bysett.25kmbuffer %>%
  group_by(MPAID) %>%
  summarise(mean_dist = mean(mean_dist_km, na.rm = T),
            sd_dist = sd(sd_dist_km, na.rm = T),
            prop_negeco_impact = length(SettlementID[herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsoc_impact = length(SettlementID[FSIndex_longest<0 & !is.na(FSIndex_longest)])/length(SettlementID[!is.na(FSIndex_longest)]),
            prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

impacts.25kmbuffer.all <-
  impacts.bysett.25kmbuffer %>%
  summarise(prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

# -- Regression models

# Model 1: Binomial logistic regression - Food security & herbivore fish biomass ----
# ---      Eco sites within 25km buffer (and same MPA) included in weighted match to settlement

# Chosen covariates
model.1a.25km <- glm(foodsec_herbbio_cat ~ TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + 
                  MAIndex_trend_z + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish,
                data = impacts.bysett.25kmbuffer, family = "binomial")


summary(model.1a.25km)


# Model 2: Linear mixed effects models for each outcome - Food security & herbivore fish biomass ----
# -- Food security
fit.FS.25km.lmer <- lmer(FSIndex_longest ~ 
                           TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                         + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish + (1 | MPAID), 
                         data = impacts.bysett.25kmbuffer %>% select(FSIndex_longest, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                     MAIndex_trend_z, mean_dist_km, soc_yrpost_zone, soc_yrpost_establish, MPAID) %>% na.omit(),
                         REML = F)


fit.FS.25km.lmer.summ <- summary(fit.FS.25km.lmer)


# -- Herbivore fish biomass
fit.biomass.25km.lmer <- lmer(herb_biomass_impact ~ 
                                TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                              + mean_dist_km + eco_yrpost_zone + eco_yrpost_establish + (1 | MPAID), 
                              data = impacts.bysett.25kmbuffer %>% select(herb_biomass_impact, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                          MAIndex_trend_z, mean_dist_km, eco_yrpost_zone, eco_yrpost_establish, MPAID) %>% na.omit(),
                              REML = F)


fit.biomass.25km.lmer.summ <- summary(fit.biomass.25km.lmer)


# ---- 6.3 30km buffer - full analysis using 30km ----

# -- Define site.dists with buffer
site.dists.30kmbuffer <-
  site.dists %>% filter(Distance_m<=30000) %>%
  group_by(SettlementID, MPAID) %>%
  mutate(total_dist = sum(1/(Distance_m^2))) %>%
  ungroup() %>%
  mutate(Relative_Weight_new = (1/(Distance_m^2))/total_dist)


# -- Find minimum and maximum distances of reef sites for 30km buffer match
check.30km <- site.dists %>% filter(Distance_m<=30000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))

check.dists.30kmbuffer <-
  site.dists.30kmbuffer %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm=T)/1000)


# -- Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
ecoimpacts.bysett.30kmbuffer <- 
  left_join(site.dists.30kmbuffer, eco.impacts, by = "SiteID") %>%
  mutate(total_biomass_impact_w = Relative_Weight * total_biomass_impact,
         herb_biomass_impact_w = Relative_Weight * herb_biomass_impact,
         key_biomass_impact_w = Relative_Weight * key_biomass_impact,
         hard_coral_impact_w = Relative_Weight * hard_coral_impact,
         macroalgae_cover_impact_w = Relative_Weight * macroalgae_cover_impact) %>%
  dplyr::group_by(SettlementID) %>%
  dplyr::summarise(total_biomass_impact = sum(total_biomass_impact_w),
                   herb_biomass_impact = sum(herb_biomass_impact_w),
                   key_biomass_impact = sum(key_biomass_impact_w),
                   hard_coral_impact = sum(hard_coral_impact_w),
                   macroalgae_cover_impact = sum(macroalgae_cover_impact_w),
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))

impacts.bysett.30kmbuffer <-
  left_join(settimpacts.reshape, ecoimpacts.bysett.30kmbuffer, by="SettlementID") %>%
  left_join(settlevel.covariates, by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(check.dists.30kmbuffer, by = c("SettlementID", "MPAID")) %>%
  left_join(MPA.name[,c("MPAID","MPAName")], by = "MPAID") %>%
  mutate(change_status = ifelse(MPAID%in%c(1, 4, 5, 6, 15, 17), 1, 0),
         soc_yrpost_zone = ifelse(MPAID%in%c(4, 6, 17), 3, #Kofiau, Misool, and Kei all have social data that goes 3 years past zoning  
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have social data post zonation
                                         ifelse(MPAID%in%c(1, 15), 2, #Mayalibit and Alor have social data 2 years post zonation
                                                ifelse(MPAID==5, 4, #Dampier has social data 4 years post zonation
                                                       ifelse(MPAID==2, 12, NA))))), #TNTC has social data 12 years post zonation
         eco_yrpost_zone = ifelse(MPAID%in%c(1, 4, 5), 4, #Mayalibit, Kofiau, and Dampier all have eco data 4 years post zonation
                                  ifelse(MPAID%in%c(7, 9, 16, 18), 0, #Buruway, Triton, Flotim, and Koon all do not have eco data post zonation
                                         ifelse(MPAID%in%c(15, 17), 2, #Alor and Kei have eco data 2 years post zonation
                                                ifelse(MPAID==6, 3, #Misool has eco data 3 years post zonation
                                                       ifelse(MPAID==2, 14, NA))))), #TNTC has eco data 14 years post zonation
         soc_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline social data that pre-dates establishment (with Kei Kecil having baseline data from the same year as establishment)
                                       ifelse(MPAID%in%c(1, 7, 9), 4, #Mayalibit, Buruway, and Triton have baseline social data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6), 5, #Kofiau and Misool have baseline social data from 5 years post establishment
                                                     ifelse(MPAID==5, 6, #Dampier has baseline social data from 6 years post establishment
                                                            ifelse(MPAID==2, 8, NA))))), #TNTC has baseline social data from 8 years post establishment
         eco_yrpost_establish = ifelse(MPAID%in%c(15, 16, 17, 18), 0, #Alor, Flotim, Kei, and Koon all have baseline eco data that pre-dates establishment
                                       ifelse(MPAID%in%c(5, 7), 4, #Dampier and Buruway have baseline eco data from 4 years post establishment
                                              ifelse(MPAID%in%c(4, 6, 9), 5, #Kofiau, Misool, and Triton all have baseline eco data from 5 years post establishment
                                                     ifelse(MPAID==1, 6, #Mayalibit has basline eco data from 6 years post establishment
                                                            ifelse(MPAID==2, 9, NA))))), #TNTC has baseline eco data from 9 years post establishment
         MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_herbbio_cat = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_trdoff = ifelse((FSIndex_longest>0 & herb_biomass_impact<0) |
                                           (FSIndex_longest<0 & herb_biomass_impact>0), 1, 0),
         MarineReliance_cat = factor(ifelse(MarineReliance>=0, "high", "low")),
         MPA_MarineReliance_cat = factor(paste0(MPAID, MarineReliance_cat, sep = "_")))


# -- Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.)
Sett.level.match.info.30km <- 
  left_join(check.30km, check.dists.30kmbuffer, by = c("SettlementID", "MPAID"))

Site.level.match.info.30km <-
  site.dists.30kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

MPA.level.info.30km <-
  Site.level.match.info.30km %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID))


# -- Proportion of synergies vs. tradeoffs per MPA
impacts.30kmbuffer.byMPA <- 
  impacts.bysett.30kmbuffer %>%
  group_by(MPAID) %>%
  summarise(mean_dist = mean(mean_dist_km, na.rm = T),
            sd_dist = sd(sd_dist_km, na.rm = T),
            prop_negeco_impact = length(SettlementID[herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsoc_impact = length(SettlementID[FSIndex_longest<0 & !is.na(FSIndex_longest)])/length(SettlementID[!is.na(FSIndex_longest)]),
            prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

impacts.30kmbuffer.all <-
  impacts.bysett.30kmbuffer %>%
  summarise(prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

# -- Regression models

# Model 1: Binomial logistic regression - Food security & herbivore fish biomass ----
# ---      Eco sites within 30km buffer (and same MPA) included in weighted match to settlement

# Chosen covariates
model.1a.30km <- glm(foodsec_herbbio_cat ~ TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + 
                       MAIndex_trend_z + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish,
                     data = impacts.bysett.30kmbuffer, family = "binomial")


summary(model.1a.30km)


# Model 2: Linear mixed effects models for each outcome - Food security & herbivore fish biomass ----
# -- Food security
fit.FS.30km.lmer <- lmer(FSIndex_longest ~ 
                           TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                         + mean_dist_km + soc_yrpost_zone + soc_yrpost_establish + (1 | MPAID), 
                         data = impacts.bysett.30kmbuffer %>% select(FSIndex_longest, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                     MAIndex_trend_z, mean_dist_km, soc_yrpost_zone, soc_yrpost_establish, MPAID) %>% na.omit(),
                         REML = F)


fit.FS.30km.lmer.summ <- summary(fit.FS.30km.lmer)


# -- Herbivore fish biomass
fit.biomass.30km.lmer <- lmer(herb_biomass_impact ~ 
                                TimeMarket_z + MarineReliance_z + MgmtRights_z + FSIndex_t0_z + MAIndex_trend_z 
                              + mean_dist_km + eco_yrpost_zone + eco_yrpost_establish + (1 | MPAID), 
                              data = impacts.bysett.30kmbuffer %>% select(herb_biomass_impact, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, 
                                                                          MAIndex_trend_z, mean_dist_km, eco_yrpost_zone, eco_yrpost_establish, MPAID) %>% na.omit(),
                              REML = F)


fit.biomass.30km.lmer.summ <- summary(fit.biomass.30km.lmer)



herbbio.v.foodsec.30kmbuffer.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  annotate("text", x = 2.3, y = 5.7, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  annotate("text", x = -2.3, y = -2, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  annotate("text", x = -2.3, y = 5.7, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  annotate("text", x = 2.3, y = -2, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       title = "Synergies & Tradeoffs: Food Security v. Herbivore Fish Biomass",
                       subtitle = "Settlement-level impacts")
