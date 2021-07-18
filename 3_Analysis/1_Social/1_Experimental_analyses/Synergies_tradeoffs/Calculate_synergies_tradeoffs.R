# 
# code: Calculate settlement level synergies and tradeoffs, BHS & SBS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2020
# modified: July 2021
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & IMPORT DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- Load libraries ----

pacman::p_load(rio, car, lfe, factoextra, corrplot, sjPlot, grid, gridExtra, ordinal, lme4, lmerTest, dplyr, ggplot2)


# ---- Source files ----

source('2_Functions/2_Analysis/Function_process_covariates.R')
source('3_Analysis/1_Social/1_Experimental_analyses/Synergies_tradeoffs/Calculate_poverty_index.R')


# ---- Define functions ----

# define function for scaling (but not centering around mean) -- used for eco and social impacts
scale2 <- function(x, na.rm = TRUE) (x) / (2 * sd(x, na.rm = TRUE))

# define function for standardizing covariates 
stdz <- function(x, na.rm = TRUE) (x - mean(x, na.rm = TRUE)) / (2*sd(x, na.rm = TRUE))

# define function to identify most recent file for social impacts and distance weighting outputs
last.file <- function(dir.nam, nam){
  import(paste0(dir.nam, last(sort(grep(nam, list.files(dir.nam), value=T, fixed=T))))) }


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
         Zoned = ifelse(MPAID%in%c(1, 2, 4, 5, 6, 15, 17), 1, 0), # this variable counts Zoned ==1 if zonation has occurred by the time of the most recent monitoring used in the analysis
         Type_of_Zone = ifelse(Zoned==0, "Use", Type_of_Zone)) # if the MPA has not yet been zoned by the time of the most recent monitoring, then all sites within that MPA are automatically categorized as "Use"


# import settlement impacts
sett.impacts <- last.file(dir.nam='x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/', nam='settlevel_impacts_1-3match') %>%
  filter(MPAID!=8 & !is.na(estimate)) 


# import distances from sites to settlements (for calculating weighted eco impacts)
site.dists <- last.file(dir.nam='x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/', nam='Distance_weights_allsites_persett') %>%
  left_join(Settlements[,c("SettlementID","SettlementName","MPAID")], by="SettlementID") %>%
  mutate(MPAID = ifelse(SettlementID %in% c(113,82,81,83,84),7,
                        ifelse(SettlementID %in% c(114,115,93,94,92),8,
                               ifelse(SettlementID %in% c(85:90,95,91),9,MPAID))))


# import years of monitoring data per MPA, and establishment history
MPA.years <- import('x_Flat_data_files/1_Social/Inputs/Synergies_tradeoffs/MPA.years.csv') %>%
  left_join(MPA.name, by = "MPAID") %>%
  mutate(MPAID = factor(MPAID, levels = c("1", "4", "5", "6", "7", "9", "2", "15", "16", "17", "18"), ordered = T),
         Details = ifelse(Details=="", NA, Details),
         DataType = factor(DataType, levels = c("establish.history", "social", "ecological"), ordered = T))



# ---- 1.2 Get descriptive info about 50 buffer for matching reef sites to settlements ----

check <- site.dists %>% filter(Distance_m<=50000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))


# -- 50km is a good buffer that maintains many reef sites, but also is arguably a half- to full-day's worth of travel for fishing (which is possible in some MPAs)
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
  summarise(lowest_dist_km = min(Distance_m, na.rm = T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm = T)],
            mean_dist_km = mean(Distance_m, na.rm = T)/1000,
            highest_dist_km = max(Distance_m, na.rm = T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm = T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm = T)/1000)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: COMPILE DATA & CALCULATE SYNTHETIC ECO IMPACTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Compile social impact and covariate data, per settlement ----

# FLAT FILE: FGD User data - currently, we do not use any of these data for the analysis 
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


# FLAT FILE: KII MPA data - currently, we do not use any of these data for the analysis 
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


# Settlement level covariates data frame, pulling in HHData frame, post-processing some new indices,
#  and subsetting to only include one year of data (either baseline or most recent or a trend through time, depending on the indicator)
settlevel.covariates <- 
  HHData %>% 
  left_join(DiD.data[,c("HouseholdID", "PovertyIndex_pca")], by = "HouseholdID") %>%
  mutate(PrimaryFish = ifelse(PrimaryLivelihood==3, 1, ifelse(is.na(PrimaryLivelihood), NA, 0))) %>%
  mutate(across(c("FreqEatFish", "FreqSaleFish", "PercentIncFish", "PrimaryFish", "FreqFish"), stdz)) %>%
  mutate(MarineReliance = rowMeans(dplyr::select(.,c("FreqEatFish", "FreqSaleFish", "PercentIncFish",
                                                     "PrimaryFish", "FreqFish")), na.rm = T),
         MgmtRightsIndex = rowSums(dplyr::select(.,"RightsManage", "RightsTransfer", "RightsExclude"), na.rm=T),
         HarvestRightsIndex = rowSums(dplyr::select(.,"RightsAccess", "RightsHarvest"), na.rm=T),
         NumLivelihoods = ifelse(!is.na(PrimaryLivelihood) & !is.na(SecondaryLivelihood) & !is.na(TertiaryLivelihood), 3,
                                 ifelse(!is.na(PrimaryLivelihood) & !is.na(SecondaryLivelihood) & is.na(TertiaryLivelihood), 2, 
                                        ifelse(!is.na(PrimaryLivelihood) & is.na(SecondaryLivelihood) & is.na(TertiaryLivelihood), 1, NA)))) %>%
  group_by(SettlementID) %>%
  mutate(MostRecent = max(InterviewYear),
         Baseline = min(InterviewYear)) %>%
  ungroup() %>%
  group_by(SettlementID, InterviewYear, MostRecent, Baseline) %>%
  summarise(TimeMarket = mean(TimeMarket,na.rm = T),
            MTManage = mean(RightsManage,na.rm = T),
            MTHarvest = mean(RightsHarvest,na.rm = T),
            MTAccess = mean(RightsAccess,na.rm = T),
            MTTransfer = mean(RightsTransfer,na.rm = T),
            MTExclude = mean(RightsExclude,na.rm = T),
            MgmtRights = length(SettlementID[MgmtRightsIndex>0 & !is.na(MgmtRightsIndex)])/length(SettlementID[!is.na(MgmtRightsIndex)]),
            HarvestRights = length(SettlementID[HarvestRightsIndex>0 & !is.na(HarvestRightsIndex)])/length(SettlementID[!is.na(HarvestRightsIndex)]),
            MarineReliance = mean(MarineReliance, na.rm = T),
            FSMean = mean(FSIndex, na.rm = T),
            MAMean = mean(MAIndex, na.rm = T),
            PovertyIndexMean = mean(PovertyIndex_pca, na.rm = T),
            LivelihoodDiversity = mean(NumLivelihoods, na.rm = T),
            PropFishers = length(SettlementID[!is.na(PrimaryLivelihood) & PrimaryLivelihood==3])/length(SettlementID[!is.na(PrimaryLivelihood)])) %>%
  ungroup() %>%
  group_by(SettlementID) %>% # choose which monitoring year of data to pull for each covariate (e.g., baseline, or most recent, or calculate a trend)
  summarise(TimeMarket = TimeMarket[InterviewYear==MostRecent],
            MgmtRights = MgmtRights[InterviewYear==Baseline],
            MarineReliance = MarineReliance[InterviewYear==Baseline],
            FSMean = FSMean[InterviewYear==Baseline],
            MAMean_trend = MAMean[InterviewYear==MostRecent] - MAMean[InterviewYear==Baseline],
            MAMean = MAMean[InterviewYear==Baseline],
            PovertyIndex_trend = PovertyIndexMean[InterviewYear==MostRecent] - PovertyIndexMean[InterviewYear==Baseline],
            PovertyIndexMean = PovertyIndexMean[InterviewYear==Baseline],
            LivelihoodDiversity_trend = LivelihoodDiversity[InterviewYear==MostRecent] - LivelihoodDiversity[InterviewYear==Baseline],
            LivelihoodDiversity = LivelihoodDiversity[InterviewYear==MostRecent],
            PropFishers_trend = PropFishers[InterviewYear==MostRecent] - PropFishers[InterviewYear==Baseline]) %>%
  ungroup() %>%
  mutate(TimeMarket_z = stdz(TimeMarket), # standardize the covariates to be used in following regression analyses, for cross-variable comparisons
         MgmtRights_z = stdz(MgmtRights),
         MarineReliance_z = stdz(MarineReliance),
         FSIndex_t0_z = stdz(FSMean),
         MAIndex_trend_z = stdz(MAMean_trend),
         PovertyIndex_trend_z = stdz(PovertyIndex_trend), # currently, this indicator is not used in analysis, but kept it here in case (there are NAs with this indicator)
         LivelihoodDiversity_trend_z = stdz(LivelihoodDiversity_trend), # currently, this indicator is not used in analysis, but kept it here in case (there are NAs with this indicator)
         LivelihoodDiversity_z = stdz(LivelihoodDiversity), # currently, this indicator is not used in analysis, but kept it here in case (there are NAs with this indicator)
         PropFishers_trend_z = stdz(PropFishers_trend)) # currently, this indicator is not used in analysis, but kept it here in case (there are NAs with this indicator)


# check out the summary statistics and histogram for standardized variables (can replace the variable name with whatever you'd like)
summary(settlevel.covariates$PropFishers_trend_z)
hist(settlevel.covariates$PropFishers_trend_z, breaks = 20)


# ---- 2.2 Reshape the settlement impact data to identify the most recent impacts for each settlement ----
#          (i.e., when there is more than one, like BHS MPAs that have t2 and t4)

settimpacts.reshape <-
  sett.impacts %>% mutate(most_recent = ifelse(MPAID%in%c(1:9), "t4",
                                               ifelse(MPAID%in%c(15:17), "t3",
                                                      ifelse(MPAID==18, "t2", NA))),
                          longest_impact = ifelse(time==most_recent, "longest", "not_longest"),
                          time = ifelse(time=="t2" | time=="t3", "t2.3", time)) %>%
  tidyr::pivot_wider(., id_cols = c(SettlementID, SettlementName, MPAID), 
                     names_from = c(Response, longest_impact), values_from = estimate)


# "longest" in the column name indicates that the impact is the most recent / measuring the longest amount of time that our data allows (i.e., a t4 impact instead of t2 for a BHS settlement that has both)
sett.impacts.cols <- c("FSIndex_not_longest", "FSIndex_longest", "MAIndex_not_longest", "MAIndex_longest",
                       "MTIndex_not_longest", "MTIndex_longest", "PAIndex_not_longest", "PAIndex_longest",
                       "SERate_not_longest", "SERate_longest")


# Scale the social impacts
settimpacts.reshape <-
  settimpacts.reshape %>%
  mutate(across(all_of(sett.impacts.cols), scale2),
         Region = ifelse(MPAID%in%c(1, 4, 5, 6), "Raja Ampat", # calculate a "region" variable
                         ifelse(MPAID%in%c(7, 9), "Kaimana",
                                ifelse(MPAID%in%c(15, 16), "Nusa Tenggara Timur",
                                       ifelse(MPAID%in%c(17, 18), "Maluku",
                                              ifelse(MPAID==2, "Teluk Cenderawasih", NA))))))


# ---- 2.3 Compile impacts data frame with 50km buffer ----


eco.impacts.cols <- c("total_biomass_impact", "herb_biomass_impact", "key_biomass_impact", 
                      "hard_coral_impact", "macroalgae_cover_impact")


# Calculate synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
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
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID),
                   PropNTZ_w = sum(Relative_Weight[Type_of_Zone=="NTZ"]) / length(SiteID)) %>% # this calculates a weighted proportion of sites that are NTZ matched to the settlement 
  mutate(PropNTZ_w_z = stdz(PropNTZ_w), # standardize the weighted proportion of NTZ zones matched to a settlement
         across(all_of(eco.impacts.cols), scale2)) # scale all eco impacts

# Calculate master impacts by sett data frame for all regression analysis and subsequent plotting
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
         foodsec_herbbio_possyn = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_soctrdoff = ifelse(FSIndex_longest<0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_ecotrdoff = ifelse(FSIndex_longest>0 & herb_biomass_impact<0, 1, 0),
         foodsec_herbbio_negsyn = ifelse(FSIndex_longest<0 & herb_biomass_impact<0, 1, 0))
  

# Rename columns and only include variables included in regression models to be used as input data for models
impacts.formodel.50km <-
  impacts.bysett.50kmbuffer %>%
  select(FSIndex_longest, herb_biomass_impact, foodsec_herbbio_possyn, foodsec_herbbio_soctrdoff, foodsec_herbbio_ecotrdoff,
         foodsec_herbbio_negsyn, TimeMarket_z, MarineReliance_z, MgmtRights_z, FSIndex_t0_z, MAIndex_trend_z, 
         PropNTZ_w_z, mean_dist_km, soc_yrpost_zone, soc_yrpost_establish, eco_yrpost_zone, eco_yrpost_establish, MPAID) %>% 
  na.omit() %>%
  rename(FoodSecurityImpact = FSIndex_longest,
         HerbFishBiomassImpact = herb_biomass_impact,
         PositiveSynergy = foodsec_herbbio_possyn,
         SocialTradeoff = foodsec_herbbio_soctrdoff,
         EcologicalTradeoff = foodsec_herbbio_ecotrdoff,
         NegativeSynergy = foodsec_herbbio_negsyn,
         TimeMarket = TimeMarket_z,
         MarineReliance = MarineReliance_z, 
         MgmtRights = MgmtRights_z, 
         InitialFoodSecurity = FSIndex_t0_z, 
         WealthTrend = MAIndex_trend_z,
         ProximityNTZ = PropNTZ_w_z,
         MeanDistance = mean_dist_km,
         YearsPostZonation_SocMon = soc_yrpost_zone,
         YearsPostEstablish_SocMon = soc_yrpost_establish,
         YearsPostZonation_EcoMon = eco_yrpost_zone,
         YearsPostEstablish_EcoMon = eco_yrpost_establish,
         MPA = MPAID)



# ---- 2.4 Calculate eco impacts per settlement & per MPA, NO BUFFER ----
#          NOTE: these calculations are not used in the analysis currently, but could be used as senstivitiy tests 
#                (i.e., what if we included all sites, no buffer? Or, what if we didn't do distance weights at all and only calculated MPA-level eco impacts?)

# synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement)
ecoimpacts.bysett <- 
  left_join(site.dists, eco.impacts, by = c("SiteID", "MPAID")) %>%
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
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID),
                   PropNTZ_w = sum(Relative_Weight[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(PropNTZ_w_z = stdz(PropNTZ_w),
         across(all_of(eco.impacts.cols), scale2))


# MPA level impacts
ecoimpacts.byMPA <-
  left_join(site.dists, eco.impacts, by = c("SiteID", "MPAID")) %>%
  group_by(MPA_Name) %>%
  summarise(total_biomass_impact = mean(total_biomass_impact, na.rm=T),
            herb_biomass_impact = mean(herb_biomass_impact, na.rm=T),
            key_biomass_impact = mean(key_biomass_impact, na.rm=T),
            hard_coral_impact = mean(hard_coral_impact, na.rm=T),
            macroalgae_cover_impact = mean(macroalgae_cover_impact, na.rm=T),
            PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID),
            PropNTZ_w = sum(Relative_Weight[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(PropNTZ_w_z = stdz(PropNTZ_w),
         across(all_of(eco.impacts.cols), scale2)) %>%
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

# -- Joined MPA level descriptive impact data
MPA.level.info <-
  left_join(eco.site.info, soc.sett.info, by = "MPAID") %>%
  left_join(MPA.name[,c("MPAID", "MPAName")], by = "MPAID") %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))


# ---- 3.2 Settlement to site match info (how many sites per sett, avg distance, etc.) ----

Sett.level.match.info <- 
  left_join(check, check.dists.50kmbuffer, by = c("SettlementID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))

Site.level.match.info <-
  site.dists.50kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

# -- Joined to MPA level descriptive impact data
MPA.level.info <-
  Site.level.match.info %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID),
            sitesmatched_atleast1 = length(SiteID[nummatches>0])) %>%
  mutate(buffer = "50 km") %>%
  left_join(Sett.level.match.info %>% group_by(MPAID) %>% 
              summarise(numsetts_included = length(SettlementID)), by = "MPAID") %>%
  left_join(MPA.level.info, ., by = "MPAID")


# ---- 3.3 Proportion of synergies vs. tradeoffs per MPA, 50km buffer ----

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


# ---- 4.1 Correlations: covariates for all models ----

corrmatrix.chosencovariates <- cor(impacts.bysett.50kmbuffer[,c("TimeMarket_z", "MarineReliance_z", "MgmtRights_z", "FSIndex_t0_z", "MAIndex_trend_z",
                                                                "mean_dist_km", "soc_yrpost_zone","eco_yrpost_zone", "soc_yrpost_establish", "eco_yrpost_establish")],
                                   use = "complete.obs")
corrplot.chosencovariates <- corrplot(corrmatrix.chosencovariates)


# ---- 4.2 Binomial logistic regression - Food security & herbivore fish biomass ----
# ---      Eco sites within 50km buffer (and same MPA) included in weighted match to settlement

# --- Model 1: positive synergy as outcome variable
#     NOTE: this is currently the only binomial regression output we include in the analysis, given the framing of the second research question
model.1 <- glmer(PositiveSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
               + ProximityNTZ + MeanDistance + (1 | MPA),
                data = impacts.formodel.50km, family = "binomial",
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))


summary(model.1)


# --- Model 2: eco tradeoff as outcome variable
model.2 <- glmer(EcologicalTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                 + ProximityNTZ + MeanDistance + (1 | MPA),
                 data = impacts.formodel.50km, family = "binomial",
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))


summary(model.2)


# --- Model 3: soc tradeoff as outcome variable
model.3 <- glmer(SocialTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                 + ProximityNTZ + MeanDistance + (1 | MPA),
                 data = impacts.formodel.50km, family = "binomial",
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))

summary(model.3)
tab_model(model.2, model.3)


# --- Model 4: negative synergy as outcome variable
model.4 <- glmer(NegativeSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                 + ProximityNTZ + MeanDistance + (1 | MPA),
                 data = impacts.formodel.50km, family = "binomial",
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))


summary(model.4)
tab_model(model.1, model.4)


# ---- 4.3 Model 2: Linear mixed effects models for each outcome - Food security & herbivore fish biomass ----

# -- Food security
fit.FS.lmer <- lmer(FoodSecurityImpact ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                     + ProximityNTZ + MeanDistance + (1 | MPA),
                     data = impacts.formodel.50km, REML = F)


fit.FS.lmer.summ <- summary(fit.FS.lmer)
tab_model(fit.FS.lmer)


# -- Herbivore fish biomass
fit.biomass.lmer <- lmer(HerbFishBiomassImpact ~ 
                           TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                         + ProximityNTZ + MeanDistance + (1 | MPA), 
                         data = impacts.formodel.50km,
                         REML = F)


fit.biomass.lmer.summ <- summary(fit.biomass.lmer)
tab_model(fit.biomass.lmer)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: EXPERIMENTAL ANALYSES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# # MANOVA and univariate ANOVA looking at MPAID and Marine Reliance categorizations
# fit.MPAID <- manova(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest", "key_biomass_impact")])~
#                       as.matrix(impacts.bysett.50kmbuffer[,c("MPA_MarineReliance_cat")]))
# fit.MPAID.summ <- summary(fit.MPAID, test="Wilks")
# 
# 
# fit.MPAID.soc <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest")])~
#                           as.matrix(impacts.bysett.50kmbuffer[,c("MPA_MarineReliance_cat")]))
# fit.MPAID.soc.summ <- summary(fit.MPAID.soc, test="Wilks")
# 
# 
# fit.MPAID.eco <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("key_biomass_impact")])~
#                           as.matrix(impacts.bysett.50kmbuffer[,c("MPA_MarineReliance_cat")]))
# fit.MPAID.eco.summ <- summary(fit.MPAID.eco, test="Wilks")
# 
# 
# fit.MarineReliance <- manova(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest", "key_biomass_impact")])~
#                                as.matrix(impacts.bysett.50kmbuffer[,c("MarineReliance_cat")]))
# fit.MarineReliance.summ <- summary(fit.MarineReliance, test="Wilks")
# 
# 
# fit.MarineReliance.soc <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("FSIndex_longest")])~
#                        as.matrix(impacts.bysett.50kmbuffer[,c("MarineReliance_cat")]))
# fit.MarineReliance.soc.summ <- summary(fit.MarineReliance.soc, test="Wilks")
# 
# 
# fit.MarineReliance.eco <- aov(as.matrix(impacts.bysett.50kmbuffer[,c("key_biomass_impact")])~
#                        as.matrix(impacts.bysett.50kmbuffer[,c("MarineReliance_cat")]))
# fit.MarineReliance.eco.summ <- summary(fit.MarineReliance.eco, test="Wilks")
# 
# 
# # MULTIVARIATE MULTIPLE LINEAR REGRESSION
# pacman::p_load(car)
# 
# fit.FS.biomass.lm.1 <- lm(cbind(FSIndex_longest, key_biomass_impact) ~ 
#                      TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
#                      RuleCongruence + PropNTZ_w_z + FSIndex_t0 + mean_dist_km,
#                 data = impacts.bysett.50kmbuffer)
# fit.FS.biomass.lm.summ.1 <- summary(fit.FS.biomass.lm.1)
# 
# fit.FS.biomass.lm.2 <- lm(cbind(FSIndex_z_longest, key_biomass_impact) ~ FSIndex_t0,
#                           data = impacts.bysett.50kmbuffer)
# fit.FS.biomass.lm.summ.2 <- summary(fit.FS.biomass.lm.2)
# 
# Anova(fit.FS.biomass.lm.2)
# 
# fit.eco.lm <- lm(macroalgae_cover_impact ~ 
#                    TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
#                    RuleCongruence + PropNTZ_w_z + FSIndex_t0 + mean_dist_km,
#                  data = impacts.bysett.50kmbuffer)
# fit.eco.lm.summ <- summary(fit.eco.lm)
# 
# fit.multi.lm <- lm(cbind(FSIndex_z_longest, key_biomass_impact) ~ 
#                      TimeMarket + MarineReliance + MgmtRights + ParticipateDecisionMaking + 
#                      RuleCongruence + PropNTZ_w_z + FSIndex_t0 + mean_dist_km,
#                    data = impacts.bysett.50kmbuffer)
# 
# 
# # PCA
# pacman::p_load(factoextra)
# 
# FS.keybio.pca <- 
#   prcomp(impacts.bysett.50kmbuffer[,c("FSIndex_longest", "herb_biomass_impact")] %>% 
#            filter(!is.na(herb_biomass_impact) & !is.na(FSIndex_longest)), 
#          scale = T)
# 
# fviz_eig(FS.keybio.pca)
