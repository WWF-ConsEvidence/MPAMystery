# 
# code: Conduct sensitivity tests across buffer sizes, synergies and tradeoffs
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: June 2021
# modified: July 2021
# 
# 
# NOTE: to run this analysis, Calculate_synergies_tradeoffs.R & Plot_synergies_tradeoffs.R must be run first
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: 20KM BUFFER ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# FULL ANALYSIS USING 20KM BUFFER

# ---- 1.1 Define site.dists with buffer ----

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


# ---- 1.2 Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement) ----

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
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID),
                   PropNTZ_w = sum(Relative_Weight[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(PropNTZ_w_z = stdz(PropNTZ_w),
         across(all_of(eco.impacts.cols), scale2))


# ---- 1.3 Create data frame for model analysis ----

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
         foodsec_herbbio_possyn = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_soctrdoff = ifelse(FSIndex_longest<0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_ecotrdoff = ifelse(FSIndex_longest>0 & herb_biomass_impact<0, 1, 0),
         foodsec_herbbio_negsyn = ifelse(FSIndex_longest<0 & herb_biomass_impact<0, 1, 0))

# Rename columns and only include variables included in regression models to be used as input data for models
impacts.formodel.20km <-
  impacts.bysett.20kmbuffer %>%
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



# ---- 1.4 Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.) ----

Sett.level.match.info.20km <- 
  left_join(check.20km, check.dists.20kmbuffer, by = c("SettlementID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))

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
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID),
            sitesmatched_atleast1 = length(SiteID[nummatches>0])) %>%
  mutate(buffer = "20 km") %>%
  left_join(Sett.level.match.info.20km %>% group_by(MPAID) %>% 
              summarise(numsetts_included = length(SettlementID)), by = "MPAID")



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


# ---- 1.5 Regression models ----

# Binomial logistic regression - Food security & herbivore fish biomass
# ---      Eco sites within 20km buffer (and same MPA) included in weighted match to settlement

# --- Model 1: positive synergy as outcome variable
model.1.20km <- glmer(PositiveSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.20km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.1.20km)


# --- Model 2: eco tradeoff as outcome variable
model.2.20km <- glmer(EcologicalTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.20km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.2.20km)


# --- Model 3: soc tradeoff as outcome variable
model.3.20km <- glmer(SocialTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.20km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.3.20km)


# --- Model 4: negative synergy as outcome variable
model.4.20km <- glmer(NegativeSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.20km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.4.20km)


# --- Model 5: Linear mixed effects models for each outcome - Food security & herbivore fish biomass

# -- Food security
fit.FS.20km.lmer <- lmer(FoodSecurityImpact ~ 
                           TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                         + ProximityNTZ + MeanDistance + (1 | MPA), 
                         data = impacts.formodel.20km,
                         REML = F)


fit.FS.20km.lmer.summ <- summary(fit.FS.20km.lmer)


# -- Herbivore fish biomass
fit.biomass.20km.lmer <- lmer(HerbFishBiomassImpact ~ 
                                TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                              + ProximityNTZ + MeanDistance + (1 | MPA), 
                              data = impacts.formodel.20km,
                              REML = F)


fit.biomass.20km.lmer.summ <- summary(fit.biomass.20km.lmer)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: 30KM BUFFER ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# FULL ANALYSIS USING 30KM BUFFER


# ---- 2.1 Define site.dists with buffer ----

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


# ---- 2.2 Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement) ----

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
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID),
                   PropNTZ_w = sum(Relative_Weight[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(PropNTZ_w_z = stdz(PropNTZ_w),
         across(all_of(eco.impacts.cols), scale2))


# ---- 2.3 Create data frame for model analysis ----

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
         foodsec_herbbio_possyn = ifelse(FSIndex_longest>0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_soctrdoff = ifelse(FSIndex_longest<0 & herb_biomass_impact>0, 1, 0),
         foodsec_herbbio_ecotrdoff = ifelse(FSIndex_longest>0 & herb_biomass_impact<0, 1, 0),
         foodsec_herbbio_negsyn = ifelse(FSIndex_longest<0 & herb_biomass_impact<0, 1, 0))

# Rename columns and only include variables included in regression models to be used as input data for models
impacts.formodel.30km <-
  impacts.bysett.30kmbuffer %>%
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



# ---- 2.4 Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.) ----

Sett.level.match.info.30km <- 
  left_join(check.30km, check.dists.30kmbuffer, by = c("SettlementID", "MPAID")) %>% 
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))

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
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID),
            sitesmatched_atleast1 = length(SiteID[nummatches>0])) %>%
  mutate(buffer = "30 km") %>%
  left_join(Sett.level.match.info.30km %>% group_by(MPAID) %>% 
              summarise(numsetts_included = length(SettlementID)), by = "MPAID")



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


# ---- 2.5 Regression models ----

# Binomial logistic regression - Food security & herbivore fish biomass
# ---      Eco sites within 30km buffer (and same MPA) included in weighted match to settlement

# --- Model 1: positive synergy as outcome variable
model.1.30km <- glmer(PositiveSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.30km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.1.30km)


# --- Model 2: eco tradeoff as outcome variable
model.2.30km <- glmer(EcologicalTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.30km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.2.30km)


# --- Model 3: soc tradeoff as outcome variable
model.3.30km <- glmer(SocialTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.30km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))

summary(model.3.30km)


# --- Model 4: negative synergy as outcome variable
model.4.30km <- glmer(NegativeSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.30km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.4.30km)


# --- Model 5: Linear mixed effects models for each outcome - Food security & herbivore fish biomass

# -- Food security
fit.FS.30km.lmer <- lmer(FoodSecurityImpact ~ 
                           TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                         + ProximityNTZ + MeanDistance + (1 | MPA), 
                         data = impacts.formodel.30km,
                         REML = F)


fit.FS.30km.lmer.summ <- summary(fit.FS.30km.lmer)


# -- Herbivore fish biomass
fit.biomass.30km.lmer <- lmer(HerbFishBiomassImpact ~ 
                                TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                              + ProximityNTZ + MeanDistance + (1 | MPA), 
                              data = impacts.formodel.30km,
                              REML = F)


fit.biomass.30km.lmer.summ <- summary(fit.biomass.30km.lmer)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: 70KM BUFFER ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# FULL ANALYSIS USING 70KM BUFFER

# ---- 3.1 Define site.dists with buffer ----

site.dists.70kmbuffer <-
  site.dists %>% filter(Distance_m<=70000) %>%
  group_by(SettlementID, MPAID) %>%
  mutate(total_dist = sum(1/(Distance_m^2))) %>%
  ungroup() %>%
  mutate(Relative_Weight_new = (1/(Distance_m^2))/total_dist)


# -- Find minimum and maximum distances of reef sites for 70km buffer match
check.70km <- site.dists %>% filter(Distance_m<=70000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))

check.dists.70kmbuffer <-
  site.dists.70kmbuffer %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm=T)/1000)


# ---- 3.2 Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement) ----

ecoimpacts.bysett.70kmbuffer <- 
  left_join(site.dists.70kmbuffer, eco.impacts, by = "SiteID") %>%
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


# --- 3.3 Create data frame for model analysis ----

impacts.bysett.70kmbuffer <-
  left_join(settimpacts.reshape, ecoimpacts.bysett.70kmbuffer, by="SettlementID") %>%
  left_join(settlevel.covariates, by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(check.dists.70kmbuffer, by = c("SettlementID", "MPAID")) %>%
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
impacts.formodel.70km <-
  impacts.bysett.70kmbuffer %>%
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


# ---- 3.4 Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.) ----

Sett.level.match.info.70km <- 
  left_join(check.70km, check.dists.70kmbuffer, by = c("SettlementID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))

Site.level.match.info.70km <-
  site.dists.70kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

MPA.level.info.70km <-
  Site.level.match.info.70km %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID),
            sitesmatched_atleast1 = length(SiteID[nummatches>0])) %>%
  mutate(buffer = "70 km") %>%
  left_join(Sett.level.match.info.70km %>% group_by(MPAID) %>% 
              summarise(numsetts_included = length(SettlementID)), by = "MPAID")



# -- Proportion of synergies vs. tradeoffs per MPA
impacts.70kmbuffer.byMPA <- 
  impacts.bysett.70kmbuffer %>%
  group_by(MPAID) %>%
  summarise(mean_dist = mean(mean_dist_km, na.rm = T),
            sd_dist = sd(sd_dist_km, na.rm = T),
            prop_negeco_impact = length(SettlementID[herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsoc_impact = length(SettlementID[FSIndex_longest<0 & !is.na(FSIndex_longest)])/length(SettlementID[!is.na(FSIndex_longest)]),
            prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

impacts.70kmbuffer.all <-
  impacts.bysett.70kmbuffer %>%
  summarise(prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))


# ---- 3.5 Regression models ----

# Binomial logistic regression - Food security & herbivore fish biomass
# ---      Eco sites within 70km buffer (and same MPA) included in weighted match to settlement

# --- Model 1: positive synergy as outcome variable
model.1.70km <- glmer(PositiveSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.70km, family = "binomial")


summary(model.1.70km)


# --- Model 2: eco tradeoff as outcome variable
model.2.70km <- glmer(EcologicalTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.70km, family = "binomial")


summary(model.2.70km)


# --- Model 3: soc tradeoff as outcome variable
model.3.70km <- glmer(SocialTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.70km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.3.70km)


# --- Model 4: negative synergy as outcome variable
model.4.70km <- glmer(NegativeSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                      + ProximityNTZ + MeanDistance + (1 | MPA),
                      data = impacts.formodel.70km, family = "binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))


summary(model.4.70km)


# --- Model 5: Linear mixed effects models for each outcome - Food security & herbivore fish biomass

# -- Food security
fit.FS.70km.lmer <- lmer(FoodSecurityImpact ~ 
                           TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                         + ProximityNTZ + MeanDistance + (1 | MPA), 
                         data = impacts.formodel.70km,
                         REML = F)


fit.FS.70km.lmer.summ <- summary(fit.FS.70km.lmer)


# -- Herbivore fish biomass
fit.biomass.70km.lmer <- lmer(HerbFishBiomassImpact ~ 
                                TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                              + ProximityNTZ + MeanDistance + (1 | MPA), 
                              data = impacts.formodel.70km,
                              REML = F)


fit.biomass.70km.lmer.summ <- summary(fit.biomass.70km.lmer)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: 100KM BUFFER ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# FULL ANALYSIS USING 100KM BUFFER


# ---- 4.1 Define site.dists with buffer ----

site.dists.100kmbuffer <-
  site.dists %>% filter(Distance_m<=100000) %>%
  group_by(SettlementID, MPAID) %>%
  mutate(total_dist = sum(1/(Distance_m^2))) %>%
  ungroup() %>%
  mutate(Relative_Weight_new = (1/(Distance_m^2))/total_dist)


# -- Find minimum and maximum distances of reef sites for 100km buffer match
check.100km <- site.dists %>% filter(Distance_m<=100000) %>%
  group_by(SettlementID, MPAID) %>%
  summarise(numsites = length(SiteID))

check.dists.100kmbuffer <-
  site.dists.100kmbuffer %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km,
            sd_dist_km = sd(Distance_m, na.rm=T)/1000)


# ---- 4.2 Synthetic eco impacts, per settlement (using relative weight based on sq distance to settlement) ----

ecoimpacts.bysett.100kmbuffer <- 
  left_join(site.dists.100kmbuffer, eco.impacts, by = "SiteID") %>%
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


# ---- 4.3 Create data frame for model analysis ----

impacts.bysett.100kmbuffer <-
  left_join(settimpacts.reshape, ecoimpacts.bysett.100kmbuffer, by="SettlementID") %>%
  left_join(settlevel.covariates, by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(check.dists.100kmbuffer, by = c("SettlementID", "MPAID")) %>%
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
impacts.formodel.100km <-
  impacts.bysett.100kmbuffer %>%
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


# ---- 4.4 Descriptive stats - Settlement to site match info (how many sites per sett, avg distance, etc.) ----

Sett.level.match.info.100km <- 
  left_join(check.100km, check.dists.100kmbuffer, by = c("SettlementID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T))

Site.level.match.info.100km <-
  site.dists.100kmbuffer %>%
  group_by(SiteID, MPAID) %>%
  summarise(nummatches = length(SettlementID)) %>%
  left_join(eco.impacts[,c("SiteID", "herb_biomass_impact", "MPAID", "MPA_Name", "Type_of_Zone")], ., by = c("SiteID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T)) %>%
  left_join(MPA.level.info[,c("MPAID", "numsetts_perMPA")], by = "MPAID") %>%
  mutate(nummatches = ifelse(is.na(nummatches), 0, nummatches),
         propmatched = nummatches / numsetts_perMPA)

MPA.level.info.100km <-
  Site.level.match.info.100km %>%
  group_by(MPAID) %>%
  summarise(avg_propmatched = mean(propmatched, na.rm = T),
            propmatched_atleast1 = length(SiteID[nummatches>0])/length(SiteID),
            sitesmatched_atleast1 = length(SiteID[nummatches>0])) %>%
  mutate(buffer = "100 km") %>%
  left_join(Sett.level.match.info.100km %>% group_by(MPAID) %>% 
              summarise(numsetts_included = length(SettlementID)), by = "MPAID")


# -- Proportion of synergies vs. tradeoffs per MPA
impacts.100kmbuffer.byMPA <- 
  impacts.bysett.100kmbuffer %>%
  group_by(MPAID) %>%
  summarise(mean_dist = mean(mean_dist_km, na.rm = T),
            sd_dist = sd(sd_dist_km, na.rm = T),
            prop_negeco_impact = length(SettlementID[herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsoc_impact = length(SettlementID[FSIndex_longest<0 & !is.na(FSIndex_longest)])/length(SettlementID[!is.na(FSIndex_longest)]),
            prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))

impacts.100kmbuffer.all <-
  impacts.bysett.100kmbuffer %>%
  summarise(prop_possyn_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_negsyn_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_ecotrd_foodsec_herbbio = length(SettlementID[FSIndex_longest>0 & herb_biomass_impact<0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]),
            prop_soctrd_foodsec_herbbio = length(SettlementID[FSIndex_longest<0 & herb_biomass_impact>0 & !is.na(herb_biomass_impact)])/length(SettlementID[!is.na(herb_biomass_impact)]))


# ---- 4.5 Regression models ----

# Binomial logistic regression - Food security & herbivore fish biomass
# ---      Eco sites within 100km buffer (and same MPA) included in weighted match to settlement

# --- Model 1: positive synergy as outcome variable
model.1.100km <- glmer(PositiveSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                       + ProximityNTZ + MeanDistance + (1 | MPA),
                       data = impacts.formodel.100km, family = "binomial",
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))


summary(model.1.100km)


# --- Model 2: eco tradeoff as outcome variable
model.2.100km <- glmer(EcologicalTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                       + ProximityNTZ + MeanDistance + (1 | MPA),
                       data = impacts.formodel.100km, family = "binomial",
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))


summary(model.2.100km)


# --- Model 3: soc tradeoff as outcome variable
model.3.100km <- glmer(SocialTradeoff ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                       + ProximityNTZ + MeanDistance + (1 | MPA),
                       data = impacts.formodel.100km, family = "binomial",
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))


summary(model.3.100km)


# --- Model 4: negative synergy as outcome variable
model.4.100km <- glmer(NegativeSynergy ~ TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                       + ProximityNTZ + MeanDistance + (1 | MPA),
                       data = impacts.formodel.100km, family = "binomial",
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))


summary(model.4.100km)


# --- Model 5: Linear mixed effects models for each outcome - Food security & herbivore fish biomass

# -- Food security
fit.FS.100km.lmer <- lmer(FoodSecurityImpact ~ 
                            TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                          + ProximityNTZ + MeanDistance + (1 | MPA), 
                          data = impacts.formodel.100km,
                          REML = F)


fit.FS.100km.lmer.summ <- summary(fit.FS.100km.lmer)


# -- Herbivore fish biomass
fit.biomass.100km.lmer <- lmer(HerbFishBiomassImpact ~ 
                                 TimeMarket + MarineReliance + MgmtRights + MarineReliance*MgmtRights + InitialFoodSecurity + WealthTrend 
                               + ProximityNTZ + MeanDistance + (1 | MPA), 
                               data = impacts.formodel.100km,
                               REML = F)


fit.biomass.100km.lmer.summ <- summary(fit.biomass.100km.lmer)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: CROSSWALK BUFFERS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 5.1 Combine model outputs for all buffers into HTML-ready tables for easier viewing / exporting / comparing ----

tab_model(model.1.20km, model.1.30km, model.1, model.1.70km, model.1.100km,
          show.ci = FALSE)

tab_model(fit.FS.20km.lmer, fit.FS.30km.lmer, fit.FS.lmer, fit.FS.70km.lmer, fit.FS.100km.lmer,
          show.ci = FALSE)

tab_model(fit.biomass.20km.lmer, fit.biomass.30km.lmer, fit.biomass.lmer, fit.biomass.70km.lmer, fit.biomass.100km.lmer,
          show.ci = FALSE)


# ---- 5.2 Create data table comparing descriptive stats and buffer "strength of match", etc. for all buffers ----

crosswalk.buffers <-
  left_join(MPA.level.info %>% select(MPAID, buffer, sitesmatched_atleast1, propmatched_atleast1, numsetts_included, avg_propmatched), impacts.50kmbuffer.byMPA, by = "MPAID") %>%
  rbind.data.frame(left_join(MPA.level.info.20km %>% select(MPAID, buffer, sitesmatched_atleast1, propmatched_atleast1, numsetts_included, avg_propmatched), impacts.20kmbuffer.byMPA, by = "MPAID")) %>%
  rbind.data.frame(left_join(MPA.level.info.30km %>% select(MPAID, buffer, sitesmatched_atleast1, propmatched_atleast1, numsetts_included, avg_propmatched), impacts.30kmbuffer.byMPA, by = "MPAID")) %>%
  rbind.data.frame(left_join(MPA.level.info.70km %>% select(MPAID, buffer, sitesmatched_atleast1, propmatched_atleast1, numsetts_included, avg_propmatched), impacts.70kmbuffer.byMPA, by = "MPAID")) %>%
  rbind.data.frame(left_join(MPA.level.info.100km %>% select(MPAID, buffer, sitesmatched_atleast1, propmatched_atleast1, numsetts_included, avg_propmatched), impacts.100kmbuffer.byMPA, by = "MPAID")) %>%
  left_join(MPA.level.info %>% select(MPAID, MPAName), by = "MPAID")


# export(crosswalk.buffers, 'x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/buffer_sensitivity_crosswalk.csv')  


# ---- 5.3 Plot S&Ts for all buffers ----

herbbio.v.foodsec.20kmbuffer.plot <-
  ggplot(impacts.bysett.20kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  ggplot2::annotate("text", x = 2, y = 3, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = -2, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = 3, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 2, y = -2, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  scale_x_continuous(limits = c(-2.3, 2.3)) +
  scale_y_continuous(limits = c(-2.1, 3.1)) +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       subtitle = "20km buffer")

herbbio.v.foodsec.30kmbuffer.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  ggplot2::annotate("text", x = 2, y = 3, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = -2, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = 3, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 2, y = -2, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  scale_x_continuous(limits = c(-2.3, 2.3)) +
  scale_y_continuous(limits = c(-2.1, 3.1)) +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       subtitle = "30km buffer")

herbbio.v.foodsec.70kmbuffer.plot <-
  ggplot(impacts.bysett.70kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  ggplot2::annotate("text", x = 2, y = 3, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = -2, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = 3, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 2, y = -2, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  scale_x_continuous(limits = c(-2.3, 2.3)) +
  scale_y_continuous(limits = c(-2.1, 3.1)) +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       subtitle = "70km buffer")

herbbio.v.foodsec.100kmbuffer.plot <-
  ggplot(impacts.bysett.100kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  ggplot2::annotate("text", x = 2, y = 3, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = -2, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -2, y = 3, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 2, y = -2, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  scale_x_continuous(limits = c(-2.3, 2.3)) +
  scale_y_continuous(limits = c(-2.1, 3.1)) +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       subtitle = "100km buffer")


buffer.STs.arranged <- arrangeGrob(herbbio.v.foodsec.20kmbuffer.plot, herbbio.v.foodsec.30kmbuffer.plot,
                                    herbbio.v.foodsec.70kmbuffer.plot, herbbio.v.foodsec.100kmbuffer.plot,
                                    ncol = 4,
                                    top = textGrob("Synergies and tradeoffs, across buffers\n", 
                                                   gp = gpar(fontsize = 15, fontface = "bold", lineheight = 0.7)))

# Export
png(paste(FigureFileName, '/synergies_tradeoffs_acrossbuffers.png', sep = ""),
    units="in",height=5,width=15,res=400)
grid.newpage()
grid.draw(buffer.STs.arranged)
dev.off()
