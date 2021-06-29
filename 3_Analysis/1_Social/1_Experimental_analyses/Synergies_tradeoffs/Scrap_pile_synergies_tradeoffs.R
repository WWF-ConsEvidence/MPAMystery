# Synergies/tradeoffs scrap pile

# ---- 1.2 Only take the top 3 sites for each settlement and recalculate relative weights ----

site.dists.3sitematch <-
  site.dists %>%
  .[order(.$SettlementID,.$Distance_m),] %>%
  group_by(SettlementID) %>%
  summarise(site1 = SiteID[1],
            site2 = SiteID[2],
            site3 = SiteID[3],
            sum_dists = sum(1/(Distance_m^2)[1], 1/(Distance_m^2)[2], 1/(Distance_m^2)[3], na.rm=T)) %>%
  tidyr::pivot_longer(., cols = c(site1, site2, site3), names_to = "SiteMatch", values_to = "SiteID", values_drop_na = T) %>%
  left_join(site.dists, by = c("SettlementID","SiteID")) %>%
  mutate(Relative_Weight_3site = (1/(Distance_m^2))/sum_dists)

check.ecosite.3matches <-
  site.dists.3sitematch %>%
  group_by(SiteID) %>%
  summarise(num_setts = length(unique(SettlementID)))


# ---- 1.3 Only take the top 5 sites for each settlement and recalculate relative weights ----

site.dists.5sitematch <-
  site.dists %>%
  .[order(.$SettlementID,.$Distance_m),] %>%
  group_by(SettlementID) %>%
  summarise(site1 = SiteID[1],
            site2 = SiteID[2],
            site3 = SiteID[3],
            site4 = SiteID[4],
            site5 = SiteID[5],
            sum_dists = sum(1/(Distance_m^2)[1], 1/(Distance_m^2)[2], 1/(Distance_m^2)[3], 1/(Distance_m^2)[4], 1/(Distance_m^2)[5], na.rm=T)) %>%
  tidyr::pivot_longer(., cols = c(site1, site2, site3, site4, site5), names_to = "SiteMatch", values_to = "SiteID", values_drop_na = T) %>%
  left_join(site.dists, by = c("SettlementID","SiteID")) %>%
  mutate(Relative_Weight_5site = (1/(Distance_m^2))/sum_dists)

check.ecosite.5matches <-
  site.dists.5sitematch %>%
  group_by(SiteID) %>%
  summarise(num_setts = length(unique(SettlementID)))

check.dists.5sitematch <-
  site.dists.5sitematch %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)])


# Check dists for all site matches, 3 site, and 5 site
check.dists.allsitematch <-
  site.dists %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km)


check.dists.3sitematch <-
  site.dists.3sitematch %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km)


check.dists.5sitematch <-
  site.dists.5sitematch %>%
  group_by(SettlementID, MPAID) %>%
  summarise(lowest_dist_km = min(Distance_m, na.rm=T)/1000,
            lowest_dist_site = SiteID[Distance_m==min(Distance_m, na.rm=T)],
            mean_dist_km = mean(Distance_m, na.rm=T)/1000,
            highest_dist_km = max(Distance_m, na.rm=T)/1000,
            highest_dist_site = SiteID[Distance_m==max(Distance_m, na.rm=T)],
            range_sites = highest_dist_km - lowest_dist_km)



# ---- 2.3 Compile impacts data frames for plotting ----

impacts.bysett <-
  left_join(settimpacts.reshape, ecoimpacts.bysett, by="SettlementID") %>%
  left_join(settlevel.covariates %>% filter(InterviewYear==MostRecent), by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(initial.conditions, by = "SettlementID") %>%
  left_join(check.dists.allsitematch, by = c("SettlementID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_keybio_cat = ifelse(FSIndex_longest>0 & key_biomass_impact>0, 1, 0),
         foodsec_coral_cat = ifelse(FSIndex_longest>0 & hard_coral_impact>0, 1, 0),
         foodsec_algae_cat = ifelse(FSIndex_longest>0 & macroalgae_cover_impact>0, 1, 0),
         assets_keybio_cat = ifelse(MAIndex_longest>0 & key_biomass_impact>0, 1, 0),
         assets_coral_cat = ifelse(MAIndex_longest>0 & hard_coral_impact>0, 1, 0),
         assets_algae_cat = ifelse(MAIndex_longest>0 & macroalgae_cover_impact>0, 1, 0))

impacts.bysett.withMPAeco <-
  left_join(settimpacts.reshape, ecoimpacts.byMPA, by = "MPAID") %>%
  left_join(settlevel.covariates%>%filter(InterviewYear==MostRecent), by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(initial.conditions, by = "SettlementID") %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_keybio_cat = ifelse(FSIndex_longest>0 & key_biomass_impact>0, 1, 0),
         foodsec_coral_cat = ifelse(FSIndex_longest>0 & hard_coral_impact>0, 1, 0),
         foodsec_algae_cat = ifelse(FSIndex_longest>0 & macroalgae_cover_impact>0, 1, 0),
         assets_keybio_cat = ifelse(MAIndex_longest>0 & key_biomass_impact>0, 1, 0),
         assets_coral_cat = ifelse(MAIndex_longest>0 & hard_coral_impact>0, 1, 0),
         assets_algae_cat = ifelse(MAIndex_longest>0 & macroalgae_cover_impact>0, 1, 0))


# ---- 2.4 Test out calculations with only up to 5 site matches per settlement ----

ecoimpacts.bysett.5sites <- 
  left_join(site.dists.5sitematch, eco.impacts, by="SiteID") %>%
  mutate(total_biomass_impact_w = Relative_Weight_5site * total_biomass_impact,
         herb_biomass_impact_w = Relative_Weight_5site * herb_biomass_impact,
         key_biomass_impact_w = Relative_Weight_5site * key_biomass_impact,
         hard_coral_impact_w = Relative_Weight_5site * hard_coral_impact,
         macroalgae_cover_impact_w = Relative_Weight_5site * macroalgae_cover_impact) %>%
  dplyr::group_by(SettlementID) %>%
  dplyr::summarise(total_biomass_impact = sum(total_biomass_impact_w),
                   herb_biomass_impact = sum(herb_biomass_impact_w),
                   key_biomass_impact = sum(key_biomass_impact_w),
                   hard_coral_impact = sum(hard_coral_impact_w),
                   macroalgae_cover_impact = sum(macroalgae_cover_impact_w),
                   PropNTZ = length(SiteID[Type_of_Zone=="NTZ"]) / length(SiteID)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))

impacts.bysett.5sites <- 
  left_join(settimpacts.reshape, ecoimpacts.bysett.5sites, by = "SettlementID") %>%
  left_join(settlevel.covariates %>% filter(InterviewYear==MostRecent), by = "SettlementID") %>%
  left_join(FGD.user.data %>% dplyr::select(SettlementID, ParticipateDecisionMaking), by = "SettlementID") %>%
  left_join(KII.mpa.data %>% dplyr::select(SettlementID, RuleCongruence), by = "SettlementID") %>%
  left_join(initial.conditions, by = "SettlementID") %>%
  left_join(check.dists.5sitematch, by = c("SettlementID", "MPAID")) %>%
  mutate(MPAID = factor(MPAID, levels=c("1","2","4","5","6","7","9","15","16","17","18"), ordered = T),
         foodsec_keybio_cat = ifelse(FSIndex_longest>0 & key_biomass_impact>0, 1, 0),
         foodsec_coral_cat = ifelse(FSIndex_longest>0 & hard_coral_impact>0, 1, 0),
         foodsec_algae_cat = ifelse(FSIndex_longest>0 & macroalgae_cover_impact>0, 1, 0),
         assets_keybio_cat = ifelse(MAIndex_longest>0 & key_biomass_impact>0, 1, 0),
         assets_coral_cat = ifelse(MAIndex_longest>0 & hard_coral_impact>0, 1, 0),
         assets_algae_cat = ifelse(MAIndex_longest>0 & macroalgae_cover_impact>0, 1, 0)) %>%
  mutate(across(all_of(eco.impacts.cols), scale2))


# ---- PLOTS ----

coral.v.foodsec.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Hard coral cover impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

algae.v.foodsec.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Macroalgae cover impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

# food sec vs. eco variables, 5 sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.foodsec.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = FSIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Total fish biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

keybio.v.foodsec.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = FSIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Key fisheries biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

herbbio.v.foodsec.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Herbivore biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

coral.v.foodsec.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = FSIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Hard coral cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

algae.v.foodsec.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = FSIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Macroalgae cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")


# food sec vs. eco variables, all sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.foodsec.plot <-
  ggplot(impacts.bysett, aes(x = FSIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Total fish biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

keybio.v.foodsec.plot <-
  ggplot(impacts.bysett, aes(x = FSIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Key fisheries biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

herbbio.v.foodsec.plot <-
  ggplot(impacts.bysett, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Herbivore biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


coral.v.foodsec.plot <-
  ggplot(impacts.bysett, aes(x = FSIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Hard coral cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

algae.v.foodsec.plot <-
  ggplot(impacts.bysett, aes(x = FSIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Macroalgae cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


# food sec vs. eco variables, MPA level eco impacts (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.foodsec.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = FSIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Total fish biomass impacts",
                       subtitle = "MPA level eco impacts")

keybio.v.foodsec.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = FSIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Key fisheries biomass impacts",
                       subtitle = "MPA level eco impacts")

herbbio.v.foodsec.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Herbivore biomass impacts",
                       subtitle = "MPA level eco impacts")

coral.v.foodsec.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = FSIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Hard coral cover impacts",
                       subtitle = "MPA level eco impacts")

algae.v.foodsec.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = FSIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Food security impacts", y = "Macroalgae cover impacts",
                       subtitle = "MPA level eco impacts")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PLOT ECO IMPACTS V. MATERIAL ASSETS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# material assets vs. eco variables, sites within 50km buffer matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.assets.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Total fish biomass impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

keybio.v.assets.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Key fisheries biomass impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

herbbio.v.assets.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Herbivore biomass impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

coral.v.assets.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Hard coral cover impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

algae.v.assets.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Macroalgae cover impacts",
                       subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

# material assets vs. eco variables, 5 sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.assets.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MAIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Total fish biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

keybio.v.assets.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MAIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Key fisheries biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

herbbio.v.assets.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MAIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Herbivore biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

coral.v.assets.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MAIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Hard coral cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

algae.v.assets.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MAIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Macroalgae cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")


# material assets vs. eco variables, all sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.assets.plot <-
  ggplot(impacts.bysett, aes(x = MAIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Total fish biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

keybio.v.assets.plot <-
  ggplot(impacts.bysett, aes(x = MAIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Key fisheries biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

herbbio.v.assets.plot <-
  ggplot(impacts.bysett, aes(x = MAIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Herbivore biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


coral.v.assets.plot <-
  ggplot(impacts.bysett, aes(x = MAIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Hard coral cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

algae.v.assets.plot <-
  ggplot(impacts.bysett, aes(x = MAIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Macroalgae cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


# material assets vs. eco variables, MPA level eco impacts (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.assets.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MAIndex_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Total fish biomass impacts",
                       subtitle = "MPA level eco impacts")

keybio.v.assets.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MAIndex_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Key fisheries biomass impacts",
                       subtitle = "MPA level eco impacts")

herbbio.v.assets.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MAIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Herbivore biomass impacts",
                       subtitle = "MPA level eco impacts")

coral.v.assets.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MAIndex_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Hard coral cover impacts",
                       subtitle = "MPA level eco impacts")

algae.v.assets.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MAIndex_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Material assets impacts", y = "Macroalgae cover impacts",
                       subtitle = "MPA level eco impacts")

# ---- Export material assets v. eco impacts plots ----
totalbio.v.assets.all <- arrangeGrob(totalbio.v.assets.5sites.plot, totalbio.v.assets.plot, totalbio.v.assets.MPAeco.plot,
                                     top = textGrob("Total Fish Biomass vs. Material Assets", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/totalbio.v.assets.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(totalbio.v.assets.all)
dev.off()


keybio.v.assets.all <- arrangeGrob(keybio.v.assets.5sites.plot, keybio.v.assets.plot, keybio.v.assets.MPAeco.plot,
                                   top = textGrob("Key Fisheries Biomass vs. Material Assets", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/keybio.v.assets.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(keybio.v.assets.all)
dev.off()


herbbio.v.assets.all <- arrangeGrob(herbbio.v.assets.5sites.plot, herbbio.v.assets.plot, herbbio.v.assets.MPAeco.plot,
                                    top = textGrob("Herbivore Biomass vs. Material Assets", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/herbbio.v.assets.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(herbbio.v.assets.all)
dev.off()


coral.v.assets.all <- arrangeGrob(coral.v.assets.5sites.plot, coral.v.assets.plot, coral.v.assets.MPAeco.plot,
                                  top = textGrob("Hard Coral Cover vs. Material Assets", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/coral.v.assets.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(coral.v.assets.all)
dev.off()


algae.v.assets.all <- arrangeGrob(algae.v.assets.5sites.plot, algae.v.assets.plot, algae.v.assets.MPAeco.plot,
                                  top = textGrob("Macroalgae Cover vs. Material Assets", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/algae.v.assets.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(algae.v.assets.all)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: PLOT ECO IMPACTS V. SCHOOL ENROLLMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# school enrollment vs. eco variables, 5 sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.enroll.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = SERate_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Total fish biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

keybio.v.enroll.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = SERate_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Key fisheries biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

herbbio.v.enroll.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = SERate_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Herbivore biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

coral.v.enroll.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = SERate_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Hard coral cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

algae.v.enroll.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = SERate_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Macroalgae cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")


# school enrollment vs. eco variables, all sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.enroll.plot <-
  ggplot(impacts.bysett, aes(x = SERate_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Total fish biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

keybio.v.enroll.plot <-
  ggplot(impacts.bysett, aes(x = SERate_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Key fisheries biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

herbbio.v.enroll.plot <-
  ggplot(impacts.bysett, aes(x = SERate_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Herbivore biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


coral.v.enroll.plot <-
  ggplot(impacts.bysett, aes(x = SERate_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Hard coral cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

algae.v.enroll.plot <-
  ggplot(impacts.bysett, aes(x = SERate_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Macroalgae cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


# school enrollment vs. eco variables, MPA level eco impacts (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.enroll.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = SERate_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Total fish biomass impacts",
                       subtitle = "MPA level eco impacts")

keybio.v.enroll.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = SERate_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Key fisheries biomass impacts",
                       subtitle = "MPA level eco impacts")

herbbio.v.enroll.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = SERate_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Herbivore biomass impacts",
                       subtitle = "MPA level eco impacts")

coral.v.enroll.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = SERate_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Hard coral cover impacts",
                       subtitle = "MPA level eco impacts")

algae.v.enroll.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = SERate_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "School enrollment impacts", y = "Macroalgae cover impacts",
                       subtitle = "MPA level eco impacts")

# ---- Export school enrollment v. eco impacts plots ----
totalbio.v.enroll.all <- arrangeGrob(totalbio.v.enroll.5sites.plot, totalbio.v.enroll.plot, totalbio.v.enroll.MPAeco.plot,
                                     top = textGrob("Total Fish Biomass vs. School Enrollment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/totalbio.v.enroll.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(totalbio.v.enroll.all)
dev.off()


keybio.v.enroll.all <- arrangeGrob(keybio.v.enroll.5sites.plot, keybio.v.enroll.plot, keybio.v.enroll.MPAeco.plot,
                                   top = textGrob("Key Fisheries Biomass vs. School Enrollment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/keybio.v.enroll.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(keybio.v.enroll.all)
dev.off()


herbbio.v.enroll.all <- arrangeGrob(herbbio.v.enroll.5sites.plot, herbbio.v.enroll.plot, herbbio.v.enroll.MPAeco.plot,
                                    top = textGrob("Herbivore Biomass vs. School Enrollment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/herbbio.v.enroll.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(herbbio.v.enroll.all)
dev.off()


coral.v.enroll.all <- arrangeGrob(coral.v.enroll.5sites.plot, coral.v.enroll.plot, coral.v.enroll.MPAeco.plot,
                                  top = textGrob("Hard Coral Cover vs. School Enrollment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/coral.v.enroll.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(coral.v.enroll.all)
dev.off()


algae.v.enroll.all <- arrangeGrob(algae.v.enroll.5sites.plot, algae.v.enroll.plot, algae.v.enroll.MPAeco.plot,
                                  top = textGrob("Macroalgae Cover vs. School Enrollment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/algae.v.enroll.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(algae.v.enroll.all)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: PLOT ECO IMPACTS V. PLACE ATTACHMENT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# place attachment vs. eco variables, 5 sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.attach.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = PAIndex_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Total fish biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

keybio.v.attach.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = PAIndex_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Key fisheries biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

herbbio.v.attach.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = PAIndex_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Herbivore biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

coral.v.attach.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = PAIndex_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Hard coral cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

algae.v.attach.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = PAIndex_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Macroalgae cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")


# place attachment vs. eco variables, all sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.attach.plot <-
  ggplot(impacts.bysett, aes(x = PAIndex_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Total fish biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

keybio.v.attach.plot <-
  ggplot(impacts.bysett, aes(x = PAIndex_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Key fisheries biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

herbbio.v.attach.plot <-
  ggplot(impacts.bysett, aes(x = PAIndex_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Herbivore biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


coral.v.attach.plot <-
  ggplot(impacts.bysett, aes(x = PAIndex_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Hard coral cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

algae.v.attach.plot <-
  ggplot(impacts.bysett, aes(x = PAIndex_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Macroalgae cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


# place attachment vs. eco variables, MPA level eco impacts (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.attach.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = PAIndex_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Total fish biomass impacts",
                       subtitle = "MPA level eco impacts")

keybio.v.attach.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = PAIndex_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Key fisheries biomass impacts",
                       subtitle = "MPA level eco impacts")

herbbio.v.attach.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = PAIndex_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Herbivore biomass impacts",
                       subtitle = "MPA level eco impacts")

coral.v.attach.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = PAIndex_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Hard coral cover impacts",
                       subtitle = "MPA level eco impacts")

algae.v.attach.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = PAIndex_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Place attachment impacts", y = "Macroalgae cover impacts",
                       subtitle = "MPA level eco impacts")

# ---- Export place attachment v. eco impacts plots ----
totalbio.v.attach.all <- arrangeGrob(totalbio.v.attach.5sites.plot, totalbio.v.attach.plot, totalbio.v.attach.MPAeco.plot,
                                     top = textGrob("Total Fish Biomass vs. Place Attachment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/totalbio.v.attach.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(totalbio.v.attach.all)
dev.off()


keybio.v.attach.all <- arrangeGrob(keybio.v.attach.5sites.plot, keybio.v.attach.plot, keybio.v.attach.MPAeco.plot,
                                   top = textGrob("Key Fisheries Biomass vs. Place Attachment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/keybio.v.attach.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(keybio.v.attach.all)
dev.off()


herbbio.v.attach.all <- arrangeGrob(herbbio.v.attach.5sites.plot, herbbio.v.attach.plot, herbbio.v.attach.MPAeco.plot,
                                    top = textGrob("Herbivore Biomass vs. Place Attachment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/herbbio.v.attach.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(herbbio.v.attach.all)
dev.off()


coral.v.attach.all <- arrangeGrob(coral.v.attach.5sites.plot, coral.v.attach.plot, coral.v.attach.MPAeco.plot,
                                  top = textGrob("Hard Coral Cover vs. Place Attachment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/coral.v.attach.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(coral.v.attach.all)
dev.off()


algae.v.attach.all <- arrangeGrob(algae.v.attach.5sites.plot, algae.v.attach.plot, algae.v.attach.MPAeco.plot,
                                  top = textGrob("Macroalgae Cover vs. Place Attachment", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/algae.v.attach.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(algae.v.attach.all)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: PLOT ECO IMPACTS V. MARINE TENURE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# marine tenure vs. eco variables, 5 sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.tenure.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MTIndex_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Total fish biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

keybio.v.tenure.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MTIndex_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Key fisheries biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

herbbio.v.tenure.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MTIndex_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Herbivore biomass impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

coral.v.tenure.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MTIndex_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Hard coral cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")

algae.v.tenure.5sites.plot <-
  ggplot(impacts.bysett.5sites, aes(x = MTIndex_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Macroalgae cover impacts",
                       subtitle = "5 eco sites (within MPA) matched per settlement")


# marine tenure vs. eco variables, all sites matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.tenure.plot <-
  ggplot(impacts.bysett, aes(x = MTIndex_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Total fish biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

keybio.v.tenure.plot <-
  ggplot(impacts.bysett, aes(x = MTIndex_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Key fisheries biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

herbbio.v.tenure.plot <-
  ggplot(impacts.bysett, aes(x = MTIndex_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Herbivore biomass impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


coral.v.tenure.plot <-
  ggplot(impacts.bysett, aes(x = MTIndex_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Hard coral cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")

algae.v.tenure.plot <-
  ggplot(impacts.bysett, aes(x = MTIndex_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Macroalgae cover impacts",
                       subtitle = "All eco sites (within MPA) matched per settlement")


# marine tenure vs. eco variables, MPA level eco impacts (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)
totalbio.v.tenure.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MTIndex_z_longest, y = total_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Total fish biomass impacts",
                       subtitle = "MPA level eco impacts")

keybio.v.tenure.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MTIndex_z_longest, y = key_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Key fisheries biomass impacts",
                       subtitle = "MPA level eco impacts")

herbbio.v.tenure.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MTIndex_z_longest, y = herb_biomass_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Herbivore biomass impacts",
                       subtitle = "MPA level eco impacts")

coral.v.tenure.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MTIndex_z_longest, y = hard_coral_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Hard coral cover impacts",
                       subtitle = "MPA level eco impacts")

algae.v.tenure.MPAeco.plot <-
  ggplot(impacts.bysett.withMPAeco, aes(x = MTIndex_z_longest, y = macroalgae_cover_impact)) +
  geom_point(colour = "blue") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  plot.theme.st + labs(x = "Marine tenure impacts", y = "Macroalgae cover impacts",
                       subtitle = "MPA level eco impacts")

# ---- Export marine tenure v. eco impacts plots ----
totalbio.v.tenure.all <- arrangeGrob(totalbio.v.tenure.5sites.plot, totalbio.v.tenure.plot, totalbio.v.tenure.MPAeco.plot,
                                     top = textGrob("Total Fish Biomass vs. Marine Tenure", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/totalbio.v.tenure.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(totalbio.v.tenure.all)
dev.off()


keybio.v.tenure.all <- arrangeGrob(keybio.v.tenure.5sites.plot, keybio.v.tenure.plot, keybio.v.tenure.MPAeco.plot,
                                   top = textGrob("Key Fisheries Biomass vs. Marine Tenure", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/keybio.v.tenure.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(keybio.v.tenure.all)
dev.off()


herbbio.v.tenure.all <- arrangeGrob(herbbio.v.tenure.5sites.plot, herbbio.v.tenure.plot, herbbio.v.tenure.MPAeco.plot,
                                    top = textGrob("Herbivore Biomass vs. Marine Tenure", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/herbbio.v.tenure.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(herbbio.v.tenure.all)
dev.off()


coral.v.tenure.all <- arrangeGrob(coral.v.tenure.5sites.plot, coral.v.tenure.plot, coral.v.tenure.MPAeco.plot,
                                  top = textGrob("Hard Coral Cover vs. Marine Tenure", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/coral.v.tenure.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(coral.v.tenure.all)
dev.off()


algae.v.tenure.all <- arrangeGrob(algae.v.tenure.5sites.plot, algae.v.tenure.plot, algae.v.tenure.MPAeco.plot,
                                  top = textGrob("Macroalgae Cover vs. Marine Tenure", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/algae.v.tenure.png',
    units="in",height=6,width=18,res=400)
grid.newpage()
grid.draw(algae.v.tenure.all)
dev.off()
