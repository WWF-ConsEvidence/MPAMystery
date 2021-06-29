# 
# code: Plot synergies/tradeoffs
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2020
# modified: February 2021


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE DATA, LOAD PLOT THEMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

plot.theme.st <- theme(plot.title = element_text(size = 13, 
                                                 colour = "#303030", 
                                                 face = "bold"),
                       plot.subtitle = element_text(size = 12, 
                                                    colour = "#303030",
                                                    face = "italic"),
                       axis.ticks = element_blank(),
                       panel.background = element_rect(fill = "white",
                                                       colour = "#909090"),
                       panel.border = element_rect(fill = NA,
                                                   size = 0.25,
                                                   colour = "#C0C0C0"),
                       panel.grid.major.x = element_line(colour = "#C0C0C0",
                                                         size = 0.25,
                                                         linetype = 3),
                       panel.grid.major.y = element_blank(),
                       plot.margin = margin(t=5, r=20, b=5, l=5, unit="pt"),
                       axis.title = element_text(size = 12,
                                                 angle = 0,
                                                 face = "bold",
                                                 colour = "#303030"),
                       axis.text = element_text(size = 12,
                                                angle = 0,
                                                colour = "#303030",
                                                lineheight = 0.7),
                       legend.position = "right",
                       legend.justification = "right",
                       legend.box.spacing = unit(0.1, "cm"))

plot.theme.bympa <- theme(plot.title = element_text(size = 12, 
                                                    colour = "#303030",
                                                    face = "italic"),
                          plot.subtitle = element_text(size = 11, 
                                                       colour = "#303030"),
                          axis.ticks = element_blank(),
                          panel.background = element_rect(fill = "white",
                                                          colour = "#909090"),
                          panel.border = element_rect(fill = NA,
                                                      size = 0.25,
                                                      colour = "#C0C0C0"),
                          panel.grid.major.x = element_line(colour = "#C0C0C0",
                                                            size = 0.25,
                                                            linetype = 3),
                          panel.grid.major.y = element_blank(),
                          plot.margin = margin(t=5, r=20, b=5, l=5, unit="pt"),
                          axis.title = element_text(size = 12,
                                                    angle = 0,
                                                    face = "bold",
                                                    colour = "#303030"),
                          axis.text = element_text(size = 12,
                                                   angle = 0,
                                                   colour = "#303030",
                                                   lineheight = 0.7),
                          legend.position = "right",
                          legend.justification = "right",
                          legend.box.spacing = unit(0.1, "cm"))

plot.theme.boxplots <- theme(plot.title = element_text(size = 13, 
                                                       colour = "#303030",
                                                       face = "bold"),
                             plot.subtitle = element_text(size = 12, 
                                                          colour = "#303030",
                                                          face = "italic"),
                             axis.ticks = element_blank(),
                             panel.background = element_rect(fill = "white",
                                                             colour = "#909090"),
                             panel.border = element_rect(fill = NA,
                                                         size = 0.25,
                                                         colour = "#C0C0C0"),
                             panel.grid.major = element_line(colour = "#C0C0C0",
                                                               size = 0.25,
                                                               linetype = 3),
                             plot.margin = margin(t=5, r=20, b=5, l=5, unit="pt"),
                             axis.title = element_text(size = 12,
                                                       angle = 0,
                                                       face = "bold",
                                                       colour = "#303030"),
                             axis.text.x = element_text(size = 12,
                                                      angle = 315,
                                                      hjust = 0,
                                                      colour = "#303030",
                                                      lineheight = 0.7),
                             axis.text.y = element_text(size = 12,
                                                      angle = 0,
                                                      colour = "#303030",
                                                      lineheight = 0.7))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: PLOT - FOOD SECURITY V. FISH BIOMASS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Food sec vs. fish biomass, sites within 50km buffer matched (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon) ----

# totalbio.v.foodsec.50kmbuffer.plot <-
#   ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_longest, y = total_biomass_impact)) +
#   geom_point(colour = "blue") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   plot.theme.st + labs(x = "Food security impacts", y = "Total fish biomass impacts",
#                        subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")
# 
# keybio.v.foodsec.50kmbuffer.plot <-
#   ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_longest, y = key_biomass_impact)) +
#   geom_point(colour = "blue") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   plot.theme.st + labs(x = "Food security impacts", y = "Key fisheries biomass impacts",
#                        subtitle = "Eco sites within 50km buffer (within MPA) matched per settlement")

herbbio.v.foodsec.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
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


# ---- 2.2 Food sec vs. herb fish biomass, per MPA ----

herbbio.v.foodsec.50kmbuffer.MPAID1.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==1), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Mayalibit MPA, BHS",
                       subtitle = "(Social: 2010-2014; Eco: 2012-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID2.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==2), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Teluk Cenderawasih NP, BHS",
                       subtitle = "(Social: 2010-2014; Eco: 2011-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID4.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==4), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Kofiau dan Pulau Boo MPA, BHS",
                       subtitle = "(Social: 2011-2015; Eco: 2011-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID5.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==5), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Selat Dampier MPA, BHS",
                       subtitle = "(Social: 2012-2016; Eco: 2010-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID6.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==6), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "South East Misool MPA, BHS",
                       subtitle = "(Social: 2011-2015; Eco: 2011-2015)")

herbbio.v.foodsec.50kmbuffer.MPAID7.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==7), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) + 
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Buruway MPA, BHS",
                       subtitle = "(Social: 2012-2016; Eco: 2012-2015)")

herbbio.v.foodsec.50kmbuffer.MPAID9.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==9), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Triton (Kaimana Kota) MPA, BHS",
                       subtitle = "(Social: 2012-2016; Eco: 2013-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID15.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==15), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Selat Pantar MPA, SBS",
                       subtitle = "(Social: 2014-2017; Eco: 2014-2017)")

herbbio.v.foodsec.50kmbuffer.MPAID16.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==16), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Flores Timur MPA, SBS",
                       subtitle = "(Social: 2014-2017; Eco: 2014-2017)")

herbbio.v.foodsec.50kmbuffer.MPAID17.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==17), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       title = "Kei Kecil MPA, SBS",
                       subtitle = "(Social: 2016-2019; Eco: 2015-2018)")

herbbio.v.foodsec.50kmbuffer.MPAID18.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==18), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Koon MPA, SBS",
                       subtitle = "(Social: 2016-2018; Eco: 2016-2018)")



# ---- Export food security v. fish biomass plots ----

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210529/FS_herb_biomass_all.png',
    units="in",height=8,width=8,res=400)
grid.newpage()
grid.draw(herbbio.v.foodsec.50kmbuffer.plot)
dev.off()


# all MPA-specific synergies/tradeoffs
herbbio.v.foodsec.allMPA <- arrangeGrob(herbbio.v.foodsec.50kmbuffer.MPAID1.plot, herbbio.v.foodsec.50kmbuffer.MPAID2.plot, 
                                        herbbio.v.foodsec.50kmbuffer.MPAID4.plot, herbbio.v.foodsec.50kmbuffer.MPAID5.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID6.plot, herbbio.v.foodsec.50kmbuffer.MPAID7.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID9.plot, herbbio.v.foodsec.50kmbuffer.MPAID15.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID16.plot, herbbio.v.foodsec.50kmbuffer.MPAID17.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID18.plot,
                                        ncol = 3, nrow = 4,
                                        top = textGrob("Synergies & Tradeoffs per MPA: Food Security v. Herbivore Fish Biomass\n", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)))


png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210529/FS_herb_biomass_allMPA.png',
    units="in",height=12,width=10,res=400)
grid.newpage()
grid.draw(herbbio.v.foodsec.allMPA)
dev.off()

# keybio.v.foodsec.all <- arrangeGrob(keybio.v.foodsec.5sites.plot, keybio.v.foodsec.plot, keybio.v.foodsec.MPAeco.plot,
#                                     top = textGrob("Key Fisheries Biomass vs. Food Security", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)
# 
# png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/keybio.v.foodsec.png',
#     units="in",height=6,width=18,res=400)
# grid.newpage()
# grid.draw(keybio.v.foodsec.all)
# dev.off()
# 
# 
# herbbio.v.foodsec.all <- arrangeGrob(herbbio.v.foodsec.5sites.plot, herbbio.v.foodsec.plot, herbbio.v.foodsec.MPAeco.plot,
#                                      top = textGrob("Herbivore Biomass vs. Food Security", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)), ncol = 3)
# 
# png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210104_syntrd_1-3settmatch/herbbio.v.foodsec.png',
#     units="in",height=6,width=18,res=400)
# grid.newpage()
# grid.draw(herbbio.v.foodsec.all)
# dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PLOT - MPA IMPACT BOX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Food security, by MPA ----

FS.byMPA <-
  ggplot(impacts.bysett.50kmbuffer,
         aes(x = MPAID, y = FSIndex_longest)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  geom_vline(aes(xintercept = 7.5), colour = "#909090") +
  annotate("text", x = 7, y = -3, label = "Bird's Head\nSeascape", size = 3, colour = "#909090") +
  annotate("text", x = 8, y = -3, label = "Sunda Banda\nSeascape", size = 3, colour = "#909090") +
  geom_segment(aes(x = 6.6, y = -3, xend = 6.3, yend = -3),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_segment(aes(x = 8.45, y = -3, xend = 8.75, yend = -3),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_boxplot(fill = "#88CCEE", colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2) +
  scale_x_discrete(name = "",
                   labels = unique(impacts.bysett.50kmbuffer$MPAName[order(impacts.bysett.50kmbuffer$MPAID)])) +
  scale_y_continuous(name = "Scaled impact",
                     limits = c(-3.2, 13)) +
  plot.theme.boxplots + labs(title = "Household Food Security Impacts, by MPA",
                             subtitle = "Asterisk (*) represents the mean")

herb.biomass.bysett.byMPA <-
  ggplot(impacts.bysett.50kmbuffer, 
         aes(x = MPAID, y = herb_biomass_impact)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  geom_vline(aes(xintercept = 7.5), colour = "#909090") +
  annotate("text", x = 7, y = -3, label = "Bird's Head\nSeascape", size = 3, colour = "#909090") +
  annotate("text", x = 8, y = -3, label = "Sunda Banda\nSeascape", size = 3, colour = "#909090") +
  geom_segment(aes(x = 6.6, y = -3, xend = 6.3, yend = -3),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_segment(aes(x = 8.45, y = -3, xend = 8.75, yend = -3),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_boxplot(fill = "#88CCEE", colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2) +
  scale_x_discrete(name = "",
                   labels = unique(impacts.bysett.50kmbuffer$MPAName[order(impacts.bysett.50kmbuffer$MPAID)])) +
  scale_y_continuous(name = "Scaled impact",
                     limits = c(-3.2, 13)) +
  plot.theme.boxplots + labs(title = "Herbivore Fish Biomass Impacts, by MPA",
                             subtitle = "'Synthesized' eco impacts, per settlement; Asterisk (*) represents the mean")

herb.biomass.bysite.byMPA <-
  ggplot(eco.impacts %>% mutate(herb_biomass_impact = scale2(herb_biomass_impact),
                                MPAID = as.factor(MPAID)), 
         aes(x = MPAID, y = herb_biomass_impact)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  geom_vline(aes(xintercept = 7.5), colour = "#909090") +
  annotate("text", x = 7, y = -3, label = "Bird's Head\nSeascape", size = 3, colour = "#909090") +
  annotate("text", x = 8, y = -3, label = "Sunda Banda\nSeascape", size = 3, colour = "#909090") +
  geom_segment(aes(x = 6.6, y = -3, xend = 6.3, yend = -3),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_segment(aes(x = 8.45, y = -3, xend = 8.75, yend = -3),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_boxplot(fill = "#88CCEE", colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2) +
  scale_x_discrete(name = "",
                   labels = unique(impacts.bysett.50kmbuffer$MPAName[order(impacts.bysett.50kmbuffer$MPAID)])) +
  scale_y_continuous(name = "Scaled impact",
                     limits = c(-3.2, 13)) +
  plot.theme.boxplots + labs(title = "Herbivore Fish Biomass Impacts, by MPA",
                             subtitle = "Non-'synthesized' eco impacts, per reef site; Asterisk (*) represents the mean")


png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210529/FS_impacts_byMPA.png',
    units="in",height=8,width=12,res=400)
grid.newpage()
grid.draw(FS.byMPA)
dev.off()

png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210529/herb_biomass_impacts_byMPA.png',
    units="in",height=8,width=12,res=400)
grid.newpage()
grid.draw(herb.biomass.bysett.byMPA)
dev.off()


png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210529/herb_biomass_impacts_nonsynth_byMPA.png',
    units="in",height=8,width=12,res=400)
grid.newpage()
grid.draw(herb.biomass.bysite.byMPA)
dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: PLOT - FOOD SECURITY V. FISH BIOMASS, BY WEALTH ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


MAMean.quartiles <- summary(settlevel.covariates$MAMean)

wealth.synergies.data <- settlevel.covariates %>% 
  mutate(wealth.quartile = ifelse(MAMean<=MAMean.quartiles[2], "1", 
                                  ifelse(MAMean>MAMean.quartiles[2] & MAMean<=MAMean.quartiles[4], "2",
                                         ifelse(MAMean>MAMean.quartiles[4] & MAMean<=MAMean.quartiles[5], "3", 
                                                ifelse(MAMean>MAMean.quartiles[5], "4", NA))))) %>%
  select(wealth.quartile, SettlementID) %>%
  left_join(impacts.bysett.30kmbuffer, ., by = "SettlementID")


herbbio.v.foodsec.bywealth <-
  ggplot(wealth.synergies.data, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(aes(colour = wealth.quartile)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  annotate("text", x = 2.3, y = 5.7, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  annotate("text", x = -2.3, y = -2, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  annotate("text", x = -2.3, y = 5.7, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  annotate("text", x = 2.3, y = -2, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       title = "Synergies & Tradeoffs: Food Security v. Herbivore Fish Biomass",
                       subtitle = "Settlement-level impacts")


herbbio.v.foodsec.wealthquart1 <-
  ggplot(wealth.synergies.data %>% filter(wealth.quartile=="1"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = " \n ", y = " \n ", title = "1st Wealth Quartile (poorest)")

herbbio.v.foodsec.wealthquart2 <-
  ggplot(wealth.synergies.data %>% filter(wealth.quartile=="2"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = " \n ", y = " \n ", title = "2nd Wealth Quartile")

herbbio.v.foodsec.wealthquart3 <-
  ggplot(wealth.synergies.data %>% filter(wealth.quartile=="3"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                          title = "3rd Wealth Quartile")

herbbio.v.foodsec.wealthquart4 <-
  ggplot(wealth.synergies.data %>% filter(wealth.quartile=="4"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2, 5.7)) +
  plot.theme.bympa + labs(x = " \n ", y = " \n ", title = "4th Wealth Quartile (wealthiest)")

herbbio.v.foodsec.allwealth <- arrangeGrob(herbbio.v.foodsec.wealthquart1, herbbio.v.foodsec.wealthquart2, 
                                           herbbio.v.foodsec.wealthquart3, herbbio.v.foodsec.wealthquart4,
                                        ncol = 2, nrow = 2,
                                        top = textGrob("Synergies & Tradeoffs per Wealth Quartile:\nFood Security v. Herbivore Fish Biomass (30km buffer)\n", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)))


png('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/20210629/FS_herb_biomass_allwealth_30km.png',
    units="in",height=10,width=10,res=400)
grid.newpage()
grid.draw(herbbio.v.foodsec.allwealth)
dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: PLOT - COVARIATES AGAINST IMPACTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Plot covariates against food security impacts ----

# -- Time to Market
foodsec.v.timemarket.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = TimeMarket_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_x_continuous(limits = c(-1, 2.8)) +
  plot.theme.st + labs(x = "Time to Market\n(standardized)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security v. Time to Market",
                       subtitle = "Settlement-level impacts")

# -- Marine Reliance
foodsec.v.marinereliance.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = MarineReliance_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(x = "Marine Reliance Index\n(standardized)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security v. Marine Reliance",
                       subtitle = "Settlement-level impacts")

# -- Management Rights
foodsec.v.mgmtrights.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = MgmtRights_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_x_continuous(limits = c(-1, 2)) +
  plot.theme.st + labs(x = "Management Rights\n(standardized)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security v. Management Rights",
                       subtitle = "Settlement-level impacts")

# -- Baseline Food Security
foodsec.v.fst0.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = FSIndex_t0_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  plot.theme.st + labs(x = "Initial Food Security\n(standardized)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security Impact v. Initial Food Security",
                       subtitle = "Settlement-level impacts")

# -- Material Assets Trend
foodsec.v.matrend.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = MAIndex_trend_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(x = "Trend in Material Assets\n(standardized)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security Impact v. Material Assets Trend",
                       subtitle = "Settlement-level impacts")

# -- Mean Distance to Matched Reef Sites
foodsec.v.meandist.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = mean_dist_km, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  plot.theme.st + labs(x = "Mean Distance\n(in km)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security Impact v. Mean Distance to Matched Reef Sites",
                       subtitle = "Settlement-level impacts")

# -- Years since establishment (to social baseline)
foodsec.v.socyrestablish.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = soc_yrpost_establish, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  plot.theme.st + labs(x = "Years Since Establishment\n(to Baseline)", y = "Food Security\n(scaled impacts)",
                       title = "Food Security v. Years Since MPA Establishment",
                       subtitle = "Settlement-level impacts")


# ---- 5.2 Plot covariates against herbivore fish biomass impacts ----

# -- Time to Market
herbbio.v.timemarket.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = TimeMarket_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  scale_x_continuous(limits = c(-1, 2.8)) +
  plot.theme.st + labs(x = "Time to Market\n(standardized)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Time to Market",
                       subtitle = "Settlement-level impacts")

# -- Marine Reliance
herbbio.v.marinereliance.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = MarineReliance_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(x = "Marine Reliance Index\n(standardized)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Marine Reliance",
                       subtitle = "Settlement-level impacts")

# -- Management Rights
herbbio.v.mgmtrights.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = MgmtRights_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  scale_x_continuous(limits = c(-1, 2)) +
  plot.theme.st + labs(x = "Management Rights\n(standardized)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Management Rights",
                       subtitle = "Settlement-level impacts")

# -- Baseline Food Security
herbbio.v.fst0.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = FSIndex_t0_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  plot.theme.st + labs(x = "Initial Food Security\n(standardized)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Initial Food Security",
                       subtitle = "Settlement-level impacts")

# -- Material Assets Trend
herbbio.v.matrend.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = MAIndex_trend_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(x = "Trend in Material Assets\n(standardized)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Material Assets Trend",
                       subtitle = "Settlement-level impacts")

# -- Mean Distance to Matched Reef Sites
herbbio.v.meandist.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = mean_dist_km, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  plot.theme.st + labs(x = "Mean Distance\n(in km)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Mean Distance to Matched Reef Sites",
                       subtitle = "Settlement-level impacts")

# -- Years since establishment (to eco baseline)
herbbio.v.ecoyrestablish.plot <-
  ggplot(impacts.bysett.30kmbuffer, aes(x = eco_yrpost_establish, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-2.5, 5.7)) +
  plot.theme.st + labs(x = "Years Since Establishment\n(to Baseline)", y = "Biomass\n(scaled impacts)",
                       title = "Herbivore Biomass v. Years Since MPA Establishment",
                       subtitle = "Settlement-level impacts")
