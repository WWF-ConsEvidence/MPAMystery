# 
# code: Plots for synergies/tradeoffs analysis, BHS & SBS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2020
# modified: July 2021


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE DATA, LOAD PLOT THEMES, CREATE OUTPUT FOLDER ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Define figure output directory ----

dir.create(paste0("x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/", format(Sys.Date(), format = "%Y%m%d"), "_S&T_plots", sep = ""))
FigureFileName <- paste0("x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/", format(Sys.Date(), format = "%Y%m%d"), "_S&T_plots", sep = "")


# ---- 1.2 Plot themes ----

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

plot.theme.years <- theme(plot.title = element_text(size = 14, 
                                                 colour = "#303030", 
                                                 face = "bold"),
                       plot.subtitle = element_text(size = 11, 
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
                       axis.title = element_text(size = 10,
                                                 angle = 0,
                                                 face = "bold",
                                                 colour = "#303030"),
                       axis.text = element_text(size = 10,
                                                angle = 0,
                                                colour = "#303030",
                                                lineheight = 1),
                       legend.position = "right",
                       legend.justification = "right",
                       legend.box.spacing = unit(0.1, "cm"))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: PLOT - FOOD SECURITY V. FISH BIOMASS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Food sec vs. fish biomass, sites within 50km buffer matched ----
#          (t4 for BHS, t3 for Alor/Flotim/Kei, t2 for Koon)

herbbio.v.foodsec.50kmbuffer.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  ggplot2::annotate("text", x = 1.3, y = 3, label = "Positive\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -1.3, y = -1, label = "Negative\nSynergy", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = -1.3, y = 3, label = "Social\nTradeoff", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 1.3, y = -1, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
  plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       title = "Synergies & Tradeoffs: Food Security v. Herbivore Fish Biomass",
                       subtitle = "Settlement-level impacts")


# ---- 2.2 Food sec vs. herb fish biomass, per MPA ----

herbbio.v.foodsec.50kmbuffer.MPAID1.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==1), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Mayalibit MPA, BHS",
                       subtitle = "(Social: 2010-2014; Eco: 2012-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID2.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==2), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Teluk Cenderawasih NP, BHS",
                       subtitle = "(Social: 2010-2014; Eco: 2011-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID4.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==4), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Kofiau dan Pulau Boo MPA, BHS",
                       subtitle = "(Social: 2011-2015; Eco: 2011-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID5.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==5), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Selat Dampier MPA, BHS",
                       subtitle = "(Social: 2012-2016; Eco: 2010-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID6.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==6), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "South East Misool MPA, BHS",
                       subtitle = "(Social: 2011-2015; Eco: 2011-2015)")

herbbio.v.foodsec.50kmbuffer.MPAID7.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==7), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Buruway MPA, BHS",
                       subtitle = "(Social: 2012-2016; Eco: 2012-2015)")

herbbio.v.foodsec.50kmbuffer.MPAID9.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==9), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Triton (Kaimana Kota) MPA, BHS",
                       subtitle = "(Social: 2012-2016; Eco: 2013-2016)")

herbbio.v.foodsec.50kmbuffer.MPAID15.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==15), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Selat Pantar MPA, SBS",
                       subtitle = "(Social: 2014-2017; Eco: 2014-2017)")

herbbio.v.foodsec.50kmbuffer.MPAID16.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==16), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Flores Timur MPA, SBS",
                       subtitle = "(Social: 2014-2017; Eco: 2014-2017)")

herbbio.v.foodsec.50kmbuffer.MPAID17.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==17), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
                       title = "Kei Kecil MPA, SBS",
                       subtitle = "(Social: 2016-2019; Eco: 2015-2018)")

herbbio.v.foodsec.50kmbuffer.MPAID18.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(MPAID==18), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                       title = "Koon MPA, SBS",
                       subtitle = "(Social: 2016-2018; Eco: 2016-2018)")

# All MPA-specific synergies/tradeoffs, arranged
herbbio.v.foodsec.allMPA <- arrangeGrob(herbbio.v.foodsec.50kmbuffer.MPAID1.plot, herbbio.v.foodsec.50kmbuffer.MPAID2.plot, 
                                        herbbio.v.foodsec.50kmbuffer.MPAID4.plot, herbbio.v.foodsec.50kmbuffer.MPAID5.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID6.plot, herbbio.v.foodsec.50kmbuffer.MPAID7.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID9.plot, herbbio.v.foodsec.50kmbuffer.MPAID15.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID16.plot, herbbio.v.foodsec.50kmbuffer.MPAID17.plot,
                                        herbbio.v.foodsec.50kmbuffer.MPAID18.plot,
                                        ncol = 3, nrow = 4,
                                        top = textGrob("Synergies & Tradeoffs per MPA: Food Security v. Herbivore Fish Biomass\n", gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)))



# ---- 2.3 Food sec vs. herb fish biomass, per Region ----

herbbio.v.foodsec.50kmbuffer.RajaAmpat.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(Region=="Raja Ampat"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                          title = "Raja Ampat MPA Network, BHS",
                          subtitle = "Teluk Mayalibit; Selat Dampier;\nKofiau dan Pulau Boo; Misool Selatan Timur")

herbbio.v.foodsec.50kmbuffer.Kaimana.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(Region=="Kaimana"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                          title = "Kaimana MPA Network, BHS",
                          subtitle = "\nBuruway; Teluk Triton")

herbbio.v.foodsec.50kmbuffer.TNTC.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(Region=="Teluk Cenderawasih"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                          title = "Teluk Cenderawasih, BHS",
                          subtitle = "\nTeluk Cenderawasih National Park")

herbbio.v.foodsec.50kmbuffer.NTT.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(Region=="Nusa Tenggara Timur"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                          title = "Nusa Tenggara Timur Province, SBS",
                          subtitle = "\nSelat Pantar; Flores Timur")

herbbio.v.foodsec.50kmbuffer.Maluku.plot <-
  ggplot(impacts.bysett.50kmbuffer %>% filter(Region=="Maluku"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1, 3)) +
  plot.theme.bympa + labs(x = "", y = "",
                          title = "Maluku Province, SBS",
                          subtitle = "\nKei Kecil; Koon")


# All Region-specific synergies/tradeoffs, arranged
herbbio.v.foodsec.allRegion <- arrangeGrob(herbbio.v.foodsec.50kmbuffer.RajaAmpat.plot, herbbio.v.foodsec.50kmbuffer.Kaimana.plot, 
                                           herbbio.v.foodsec.50kmbuffer.TNTC.plot, herbbio.v.foodsec.50kmbuffer.NTT.plot,
                                           herbbio.v.foodsec.50kmbuffer.Maluku.plot, 
                                           ncol = 3, nrow = 2,
                                           top = textGrob("Synergies & Tradeoffs by Region: Food Security v. Herbivore Fish Biomass\n", 
                                                          gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1.5)))


# ---- 2.4 Export food security v. fish biomass plots ----

png(paste(FigureFileName, "/FS_herb_biomass_all.png", sep = ""),
    units="in",height=8,width=8,res=400)
grid.newpage()
grid.draw(herbbio.v.foodsec.50kmbuffer.plot)
dev.off()


png(paste(FigureFileName, "/FS_herb_biomass_allMPA.png", sep = ""),
    units="in",height=12,width=10,res=400)
grid.newpage()
grid.draw(herbbio.v.foodsec.allMPA)
dev.off()


png(paste(FigureFileName, '/FS_herb_biomass_allRegion.png', sep = ""),
    units="in",height=9,width=12,res=400)
grid.newpage()
grid.draw(herbbio.v.foodsec.allRegion)
dev.off()


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
  ggplot2::annotate("text", x = 7, y = -1.4, label = "Bird's Head\nSeascape", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 8, y = -1.4, label = "Sunda Banda\nSeascape", size = 3, colour = "#909090") +
  geom_segment(aes(x = 6.6, y = -1.4, xend = 6.3, yend = -1.4),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_segment(aes(x = 8.45, y = -1.4, xend = 8.75, yend = -1.4),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_boxplot(fill = "#88CCEE", colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2) +
  scale_x_discrete(name = "",
                   labels = unique(impacts.bysett.50kmbuffer$MPAName[order(impacts.bysett.50kmbuffer$MPAID)])) +
  scale_y_continuous(name = "Scaled impact",
                     limits = c(-1.5, 3),
                     breaks = c(-1, 0, 1, 2, 3)) +
  plot.theme.boxplots + labs(title = "Household Food Security Impacts, by MPA",
                             subtitle = "Impact, per settlement; Asterisk (*) represents the mean")


# ---- 3.2 Fish biomass, by MPA ----

herb.biomass.bysett.byMPA <-
  ggplot(impacts.bysett.50kmbuffer, 
         aes(x = MPAID, y = herb_biomass_impact)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  geom_vline(aes(xintercept = 7.5), colour = "#909090") +
  ggplot2::annotate("text", x = 7, y = -1.4, label = "Bird's Head\nSeascape", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 8, y = -1.4, label = "Sunda Banda\nSeascape", size = 3, colour = "#909090") +
  geom_segment(aes(x = 6.6, y = -1.4, xend = 6.3, yend = -1.4),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_segment(aes(x = 8.45, y = -1.4, xend = 8.75, yend = -1.4),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_boxplot(fill = "#88CCEE", colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2) +
  scale_x_discrete(name = "",
                   labels = unique(impacts.bysett.50kmbuffer$MPAName[order(impacts.bysett.50kmbuffer$MPAID)])) +
  scale_y_continuous(name = "Scaled impact",
                     limits = c(-1.5, 3),
                     breaks = c(-1, 0, 1, 2, 3)) +
  plot.theme.boxplots + labs(title = "Herbivore Fish Biomass Impacts, by MPA",
                             subtitle = "Weighted average impact, per settlement; Asterisk (*) represents the mean")

herb.biomass.bysite.byMPA <-
  ggplot(eco.impacts %>% mutate(herb_biomass_impact = scale2(herb_biomass_impact),
                                MPAID = as.factor(MPAID)), 
         aes(x = MPAID, y = herb_biomass_impact)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  geom_vline(aes(xintercept = 7.5), colour = "#909090") +
  ggplot2::annotate("text", x = 7, y = -1.4, label = "Bird's Head\nSeascape", size = 3, colour = "#909090") +
  ggplot2::annotate("text", x = 8, y = -1.4, label = "Sunda Banda\nSeascape", size = 3, colour = "#909090") +
  geom_segment(aes(x = 6.6, y = -1.4, xend = 6.3, yend = -1.4),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_segment(aes(x = 8.45, y = -1.4, xend = 8.75, yend = -1.4),
               arrow = arrow(length = unit(0.07, "in")),
               colour = "#909090") +
  geom_boxplot(fill = "#88CCEE", colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2) +
  scale_x_discrete(name = "",
                   labels = unique(impacts.bysett.50kmbuffer$MPAName[order(impacts.bysett.50kmbuffer$MPAID)])) +
  scale_y_continuous(name = "Scaled impact",
                     limits = c(-1.5, 6.5),
                     breaks = c(-1, 0, 1, 2, 3, 4, 5, 6)) +
  plot.theme.boxplots + labs(title = "Herbivore Fish Biomass Impacts, by MPA",
                             subtitle = "Non-'synthesized' eco impacts, per reef site; Asterisk (*) represents the mean")


# ---- 3.3 Export MPA impact boxplots ----

png(paste(FigureFileName, '/FS_impacts_byMPA.png', sep = ""),
    units="in",height=8,width=12,res=400)
grid.newpage()
grid.draw(FS.byMPA)
dev.off()

png(paste(FigureFileName, '/herb_biomass_impacts_byMPA.png', sep = ""),
    units="in",height=8,width=12,res=400)
grid.newpage()
grid.draw(herb.biomass.bysett.byMPA)
dev.off()


png(paste(FigureFileName, '/herb_biomass_impacts_nonsynth_byMPA.png', sep = ""),
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


# # ---- 4.1 Plot food sec v. biomass, by wealth (50 km buffer) ----
# 
# # --define quartiles by using material assets (MAMean) and Poverty Index (Duong's metric, using PCA analysis)
# MAMean.quartiles <- summary(settlevel.covariates$MAMean)
# PovertyIndexMean.quartiles <- summary(settlevel.covariates$PovertyIndexMean)
# 
# # -- define wealth synergies data, with both quartile options (MAMean, PovertIndexMean)
# wealth.synergies.data <- settlevel.covariates %>% 
#   mutate(MAMean.wealth.quartile = ifelse(MAMean<=MAMean.quartiles[2], "1", 
#                                   ifelse(MAMean>MAMean.quartiles[2] & MAMean<=MAMean.quartiles[4], "2",
#                                          ifelse(MAMean>MAMean.quartiles[4] & MAMean<=MAMean.quartiles[5], "3", 
#                                                 ifelse(MAMean>MAMean.quartiles[5], "4", NA)))),
#          PovertyIndex.wealth.quartile = ifelse(PovertyIndexMean<=PovertyIndexMean.quartiles[2], "1", 
#                                          ifelse(PovertyIndexMean>PovertyIndexMean.quartiles[2] & PovertyIndexMean<=PovertyIndexMean.quartiles[4], "2",
#                                                 ifelse(PovertyIndexMean>PovertyIndexMean.quartiles[4] & PovertyIndexMean<=PovertyIndexMean.quartiles[5], "3", 
#                                                        ifelse(PovertyIndexMean>PovertyIndexMean.quartiles[5], "4", NA))))) %>%
#   select(MAMean.wealth.quartile, PovertyIndex.wealth.quartile, SettlementID) %>%
#   left_join(impacts.bysett.50kmbuffer, ., by = "SettlementID")
# 
# 
# herbbio.v.foodsec.bywealth <-
#   ggplot(wealth.synergies.data, aes(x = FSIndex_longest, y = herb_biomass_impact)) +
#   geom_point(aes(colour = PovertyIndex.wealth.quartile)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   ggplot2::annotate("text", x = 1.3, y = 3, label = "Positive\nSynergy", size = 3, colour = "#909090") +
#   ggplot2::annotate("text", x = -1.3, y = -1, label = "Negative\nSynergy", size = 3, colour = "#909090") +
#   ggplot2::annotate("text", x = -1.3, y = 3, label = "Social\nTradeoff", size = 3, colour = "#909090") +
#   ggplot2::annotate("text", x = 1.3, y = -1, label = "Ecological\nTradeoff", size = 3, colour = "#909090") +
#   plot.theme.st + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
#                        title = "Synergies & Tradeoffs: Food Security v. Herbivore Fish Biomass",
#                        subtitle = "Settlement-level impacts")
# 
# 
# herbbio.v.foodsec.wealthquart1 <-
#   ggplot(wealth.synergies.data %>% filter(PovertyIndex.wealth.quartile=="1"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +  
#   scale_x_continuous(limits = c(-1.5, 1.5)) +
#   scale_y_continuous(limits = c(-1, 3)) +
#   plot.theme.bympa + labs(x = " \n ", y = " \n ", title = "1st Wealth Quartile (poorest)")
# 
# herbbio.v.foodsec.wealthquart2 <-
#   ggplot(wealth.synergies.data %>% filter(PovertyIndex.wealth.quartile=="2"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   scale_x_continuous(limits = c(-1.5, 1.5)) +
#   scale_y_continuous(limits = c(-1, 3)) +
#   plot.theme.bympa + labs(x = " \n ", y = " \n ", title = "2nd Wealth Quartile")
# 
# herbbio.v.foodsec.wealthquart3 <-
#   ggplot(wealth.synergies.data %>% filter(PovertyIndex.wealth.quartile=="3"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   scale_x_continuous(limits = c(-1.5, 1.5)) +
#   scale_y_continuous(limits = c(-1, 3)) +
#   plot.theme.bympa + labs(x = "Food Security\n(scaled impacts)", y = "Biomass\n(scaled impacts)",
#                           title = "3rd Wealth Quartile")
# 
# herbbio.v.foodsec.wealthquart4 <-
#   ggplot(wealth.synergies.data %>% filter(PovertyIndex.wealth.quartile=="4"), aes(x = FSIndex_longest, y = herb_biomass_impact)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   scale_x_continuous(limits = c(-1.5, 1.5)) +
#   scale_y_continuous(limits = c(-1, 3)) +
#   plot.theme.bympa + labs(x = " \n ", y = " \n ", title = "4th Wealth Quartile (wealthiest)")
# 
# herbbio.v.foodsec.allwealth <- arrangeGrob(herbbio.v.foodsec.wealthquart1, herbbio.v.foodsec.wealthquart2, 
#                                            herbbio.v.foodsec.wealthquart3, herbbio.v.foodsec.wealthquart4,
#                                         ncol = 2, nrow = 2,
#                                         top = textGrob("Synergies & Tradeoffs per Wealth Quartile:\nFood Security v. Herbivore Fish Biomass\n", 
#                                                        gp = gpar(fontsize = 14, fontface = "bold", lineheight = 1)))
# 
# 
# png(paste(FigureFileName, '/FS_herb_biomass_allwealth_50km.png', sep = ""),
#     units="in",height=10,width=10,res=400)
# grid.newpage()
# grid.draw(herbbio.v.foodsec.allwealth)
# dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: PLOT - COVARIATES AGAINST IMPACTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Plot covariates against food security impacts, 50km buffer ----

# -- Time to Market
foodsec.v.timemarket.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = TimeMarket_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_x_continuous(limits = c(-1, 2.8)) +
  plot.theme.st + labs(title = "\nTime to Market\n(standardized)", x = "", y = "Scaled impact")

# -- Marine Reliance
foodsec.v.marinereliance.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MarineReliance_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(title = "\nMarine Reliance Index\n(standardized)", x = "", y = "Scaled impact")

# -- Management Rights
foodsec.v.mgmtrights.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MgmtRights_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_x_continuous(limits = c(-1, 2)) +
  plot.theme.st + labs(title = "\nManagement Rights\n(standardized)", x = "", y = "Scaled impact")

# -- Baseline Food Security
foodsec.v.fst0.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_t0_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  plot.theme.st + labs(title = "\nInitial Food Security\n(standardized)", x = "", y = "Scaled impact")

# -- Material Assets Trend
foodsec.v.matrend.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_trend_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(title = "\nTrend in Material Assets\n(standardized)", x = "", y = "Scaled impact")

# -- Weighted proportion of matched reef sites within NTZ
foodsec.v.propntz.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = PropNTZ_w_z, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  plot.theme.st + labs(title = "\nProximity to No-Take Zones\n(standardized)", x = "", y = "Scaled impact")

# -- Mean Distance to Matched Reef Sites
foodsec.v.meandist.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = mean_dist_km, y = FSIndex_longest)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  plot.theme.st + labs(title = "\nMean Distance\n(in km)", x = "", y = "Scaled impact")

# # -- Years since establishment (to social baseline)
# foodsec.v.socyrestablish.plot <-
#   ggplot(impacts.bysett.50kmbuffer, aes(x = soc_yrpost_establish, y = FSIndex_longest)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +  
#   scale_y_continuous(limits = c(-1.5, 1.5)) +
#   plot.theme.st + labs(title = "\nYears Since Establishment\n(to baseline monitoring)", x = "", y = "Scaled impact")
# 
# # -- Years since zonation (to most recent monitoring)
# foodsec.v.socyrzone.plot <-
#   ggplot(impacts.bysett.50kmbuffer, aes(x = soc_yrpost_zone, y = FSIndex_longest)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +  
#   scale_y_continuous(limits = c(-2, 1.5)) +
#   plot.theme.st + labs(title = "\nYears Since Zonation\n(to most recent monitoring)", x = "", y = "Scaled impact")


# -- Combine all covariate scatterplots into single figure

foodsec.covariates.plot <- arrangeGrob(foodsec.v.timemarket.plot, foodsec.v.marinereliance.plot, 
                                       foodsec.v.mgmtrights.plot, foodsec.v.fst0.plot,
                                       foodsec.v.matrend.plot, foodsec.v.propntz.plot,
                                       foodsec.v.meandist.plot, 
                                       # foodsec.v.socyrestablish.plot, foodsec.v.socyrzone.plot,
                                       ncol = 4, 
                                       top = textGrob("Household Food Security Impacts v. Contextual Variables", 
                                                      gp = gpar(fontsize = 15, fontface = "bold", lineheight = 1.5)))


png(paste(FigureFileName, '/FS_covariates_scatterplots.png', sep = ""),
    units="in",height=9,width=12,res=400)
grid.newpage()
grid.draw(foodsec.covariates.plot)
dev.off()



# ---- 5.2 Plot covariates against herbivore fish biomass impacts ----

# -- Time to Market
herbbio.v.timemarket.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = TimeMarket_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  scale_x_continuous(limits = c(-1, 2.8)) +
  plot.theme.st + labs(title = "\nTime to Market\n(standardized)", x = "", y = "Scaled impact")

# -- Marine Reliance
herbbio.v.marinereliance.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MarineReliance_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(title = "\nMarine Reliance Index\n(standardized)", x = "", y = "Scaled impact")

# -- Management Rights
herbbio.v.mgmtrights.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MgmtRights_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  scale_x_continuous(limits = c(-1, 2)) +
  plot.theme.st + labs(title = "\nManagement Rights\n(standardized)", x = "", y = "Scaled impact")

# -- Baseline Food Security
herbbio.v.fst0.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = FSIndex_t0_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  plot.theme.st + labs(title = "\nInitial Food Security\n(standardized)", x = "", y = "Scaled impact")

# -- Material Assets Trend
herbbio.v.matrend.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = MAIndex_trend_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  plot.theme.st + labs(title = "\nTrend in Material Assets\n(standardized)", x = "", y = "Scaled impact")

# -- Weighted proportion of matched reef sites within NTZ
herbbio.v.propntz.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = PropNTZ_w_z, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  plot.theme.st + labs(title = "\nProximity to No-Take Zones\n(standardized)", x = "", y = "Scaled impact")

# -- Mean Distance to Matched Reef Sites
herbbio.v.meandist.plot <-
  ggplot(impacts.bysett.50kmbuffer, aes(x = mean_dist_km, y = herb_biomass_impact)) +
  geom_point(colour = "#5DA8CD") +
  geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +  
  scale_y_continuous(limits = c(-1.5, 3.2)) +
  plot.theme.st + labs(title = "\nMean Distance\n(in km)", x = "", y = "Scaled impact")

# # -- Years since establishment (to eco baseline)
# herbbio.v.ecoyrestablish.plot <-
#   ggplot(impacts.bysett.50kmbuffer, aes(x = eco_yrpost_establish, y = herb_biomass_impact)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +  
#   scale_y_continuous(limits = c(-1.5, 3.2)) +
#   plot.theme.st + labs(title = "\nYears Since Establishment\n(to baseline monitoring)", x = "", y = "Scaled impact")
# 
# # -- Years since zonation (to most recent monitoring)
# herbbio.v.ecoyrzone.plot <-
#   ggplot(impacts.bysett.50kmbuffer, aes(x = eco_yrpost_zone, y = FSIndex_longest)) +
#   geom_point(colour = "#5DA8CD") +
#   geom_smooth(colour = "#508EAD", fill = "#5DA8CD") +
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +  
#   scale_y_continuous(limits = c(-1.5, 3.2)) +
#   plot.theme.st + labs(title = "\nYears Since Zonation\n(to most recent monitoring)", x = "", y = "Scaled impact")


# -- Combine all covariate scatterplots into single figure

herbbio.covariates.plot <- arrangeGrob(herbbio.v.timemarket.plot, herbbio.v.marinereliance.plot, 
                                       herbbio.v.mgmtrights.plot, herbbio.v.fst0.plot,
                                       herbbio.v.matrend.plot, herbbio.v.propntz.plot,
                                       herbbio.v.meandist.plot, 
                                       # herbbio.v.ecoyrestablish.plot, herbbio.v.ecoyrzone.plot,
                                       ncol = 4,
                                       top = textGrob("Herbivore Fish Biomass Impacts v. Contextual Variables", 
                                                      gp = gpar(fontsize = 15, fontface = "bold", lineheight = 1.5)))


png(paste(FigureFileName, '/herb_biomass_covariates_scatterplots.png', sep = ""),
    units="in",height=9,width=12,res=400)
grid.newpage()
grid.draw(herbbio.covariates.plot)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: PLOT - YEARS OF ECO AND SOCIAL DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# Wrangle data for plot
MPA.years.reshape <- 
  MPA.years %>% filter(!is.na(Details)) %>%
  tidyr::pivot_wider(., id_cols = c(MPAID, MPAName, DataType), 
                     names_from = Details, values_from = Year) %>%
  mutate(MPAID = factor(MPAID, levels = c("1", "4", "5", "6", "7", "9", "2", "15", "16", "17", "18"), ordered = T),
         MPAName = ifelse(MPAName=="Teluk Triton MPA (Kaimana)", "Teluk Triton MPA", 
                          ifelse(MPAName=="Buruway MPA (Kaimana)", "Buruway MPA", 
                                 ifelse(grepl("Kofiau", MPAName), "Kofiau dan Pulau Boo MPA", MPAName))),
         Region = ifelse(MPAID%in%c(1, 4, 5, 6), "Raja Ampat", 
                         ifelse(MPAID%in%c(7, 9), "Kaimana",
                                ifelse(MPAID%in%c(15, 16), "Nusa Tenggara Timur",
                                       ifelse(MPAID%in%c(17, 18), "Maluku",
                                              ifelse(MPAID==2, "Teluk Cenderawasih", NA))))),
         DataType = factor(DataType, levels = c("establish.history", "social", "ecological"), ordered = T),
         MPAName.labels = paste0(MPAName, "\n", "(", Region, ")", sep = ""))


# Create plot
MPA.years.plot <- ggplot() +
  geom_rect(data = data.frame(xmin = c(1.5, 3.5, 5.5, 7.5, 9.5), xmax = c(2.5, 4.5, 6.5, 8.5, 10.5),
                              ymin = c(1997, 1997, 1997, 1997, 1997), ymax = c(2020.5, 2020.5, 2020.5, 2020.5, 2020.5)),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", alpha = 0.1) +
  geom_linerange(data = MPA.years.reshape, aes(x = MPAID, ymin = baseline, ymax = most.recent, 
                     group = DataType, colour = DataType),
                 position = position_dodge(width = 0.5),
                 size = 1.5,
                 alpha = 0.8) +
  geom_point(data = MPA.years, aes(x = MPAID, y = Year, group = DataType, colour = DataType),
             position = position_dodge(width = 0.5),
             size = 2.5) +
  geom_text(data = MPA.years %>% filter(MPAID==1 & EstablishmentDetails!=""), 
            aes(x = 0.5, y = Year, label = EstablishmentDetails),
            size = 3,
            colour = "#303030") +
  ggplot2::annotate("text", x = 8.75, y = 2012.6, label = "*", size = 6, colour = "#303030") +
  ggplot2::annotate("text", x = 6.75, y = 2002.5, label = "**", size = 6, colour = "#303030") +
  ggplot2::annotate("text", x = 4.75, y = 2018.6, label = "***", size = 6, colour = "#303030") +
  ggplot2::annotate("text", x = 5.75, y = 2018.6, label = "***", size = 6, colour = "#303030") +
  scale_colour_manual(name = "",
                      labels = c("MPA establishment history",
                                 "Social monitoring",
                                 "Ecological monitoring"),
                      values = c("#88CCEE", "#117733", "#332288")) +
  scale_x_discrete(name = "", 
                   expand = c(0.08, 0),
                   labels = unique(MPA.years.reshape$MPAName.labels[order(MPA.years.reshape$MPAID)])) +
  scale_y_continuous(name = "",
                     expand = c(0,0),
                     breaks = seq(2000, 2021, by = 3)) +
  coord_flip() + plot.theme.years + guides(colour = guide_legend(reverse = T,
                                                                 label.theme = element_text(size = 10))) +
  labs(title = "MPA Timeline",
       subtitle = "Ecological monitoring years, social monitoring years, establishment histories")


# Arrange plot with sub-text
MPA.years.arranged <- grid.arrange(MPA.years.plot, 
                                   bottom = grid.text(label = "Notes:    * Flores Timur MPA has not yet been established or zoned\n             ** Teluk Cenderawasih NP was re-zoned in 2010\n            *** Teluk Triton and Buruway MPA were originally part of a single MPA that was zoned around 2012", 
                                                      x = unit(142, "pt"),
                                                      just = "left",
                                                      gp = gpar(fontsize = 9, lineheight = 1, col = "#303030")),
                                   ncol = 1,
                                   padding = unit(5, "pt"), 
                                   vp = viewport(width = 1, height = 0.95))


# Export plot
png(paste(FigureFileName, '/MPA_data_timeline.png', sep = ""),
    units="in",height=8,width=12,res=400)
grid.newpage()
grid.draw(MPA.years.arranged)
dev.off()


