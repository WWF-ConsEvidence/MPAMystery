
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Source code and upload data from MPAMystery GitHub repo ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

### ADJUST WORKING DIRECTORY TO SOURCE LOCAL GITHUB REPO

source('3_Analysis/1_Social/4_Post-matching/Calculate_ATTs_BHS.R')

pacman::p_load(ggplot2, grid, gridExtra, gtable, lemon)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Define asterisks, universal data set, and plotting themes for impact plots ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# define function to identify how many asterisks to be displayed per impact
asterisks.impactplots.byMPA <- function(x) {
  p.val <- x$p.val
  mpa <- x$mpa
  year <- x$year
  
  result <- cbind.data.frame(mpa, year,
                             data.frame(asterisk=rep(NA,length(year))))
  
  for(i in 1:length(year)) {
    result[i,"asterisk"] <- ifelse(p.val[i] < 0.01, "***", 
                                   ifelse(p.val[i] < 0.05 & p.val[i] >= 0.01, "**",
                                          ifelse(p.val[i] < 0.1 & p.val[i] >= 0.05, "*", "")))
  }
  
  result
  
}


# put all data together for plots, including asterisk data
impactplotdata.byMPA <-
  asterisks.impactplots.byMPA(hfs.att.byMPA) %>%
  dplyr::rename(hfs = asterisk) %>%
  left_join(asterisks.impactplots.byMPA(asset.att.byMPA),by = c("year", "mpa")) %>%
  dplyr::rename(asset = asterisk) %>%
  left_join(asterisks.impactplots.byMPA(tenure.att.byMPA),by = c("year", "mpa")) %>%
  dplyr::rename(tenure = asterisk) %>%
  left_join(asterisks.impactplots.byMPA(enrol.att.byMPA),by = c("year", "mpa")) %>%
  dplyr::rename(enrol = asterisk) %>%
  left_join(asterisks.impactplots.byMPA(attach.att.byMPA),by = c("year", "mpa")) %>%
  dplyr::rename(attach = asterisk) %>%
  cbind.data.frame(hfs.est = hfs.att.byMPA$est, hfs.se = hfs.att.byMPA$se,
                   asset.est = asset.att.byMPA$est, asset.se = asset.att.byMPA$se,
                   tenure.est = tenure.att.byMPA$est, tenure.se = tenure.att.byMPA$se,
                   enrol.est = enrol.att.byMPA$est, enrol.se = enrol.att.byMPA$se,
                   attach.est = attach.att.byMPA$est, attach.se = attach.att.byMPA$se)
  

# define global plotting theme for plots
plot.theme.impactplots <- theme(axis.ticks=element_blank(),
                                panel.background=element_rect(fill="white",
                                                              colour="#909090"),
                                panel.border=element_rect(fill=NA,
                                                          size=0.25,
                                                          colour="#C0C0C0"),
                                panel.grid.major.x=element_line(colour="#C0C0C0",
                                                                size=0.25,
                                                                linetype=3),
                                panel.grid.major.y=element_blank(),
                                axis.title=element_text(size=rel(0.9),
                                                        angle=0,
                                                        face="bold",
                                                        colour="#303030"),
                                axis.text=element_text(size=rel(0.9),
                                                       angle=0,
                                                       colour="#303030"),
                                plot.title=element_text(hjust=0.5),
                                legend.position="top",
                                legend.justification="center",
                                legend.box.spacing=unit(0.1,"cm"))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Define each variable's impact plot, by MPA and read to .png ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# FOOD SECURITY
hfs.impactplot.byMPA <- 
  ggplot(impactplotdata.byMPA) +
  geom_bar(aes(x = mpa, y = hfs.est, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3,
           show.legend = F) +
  geom_errorbar(aes(x = mpa, ymin = hfs.est - hfs.se, ymax = hfs.est + hfs.se, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = mpa, group = year, y = ifelse(hfs.est > 0, 
                                                  hfs.est + hfs.se + 0.2*hfs.se,
                                                  hfs.est - hfs.se - 0.2*hfs.se), label = hfs),
            size = 3,
            position = position_dodge(width=0.45)) +
  scale_x_continuous(name = "MPA",
                     breaks = 1:6,
                     labels = c("Mayalibit", "TNTC", "Kaimana", "Kofiau", "Dampier", "Misool")) +
  scale_y_continuous(name = "") +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95)),
                    guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Household Food Security")


# MATERIAL ASSETS
asset.impactplot.byMPA <- 
  ggplot(impactplotdata.byMPA) +
  geom_bar(aes(x = mpa, y = asset.est, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3,
           show.legend = F) +
  geom_errorbar(aes(x = mpa, ymin = asset.est - asset.se, ymax = asset.est + asset.se, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = mpa, group = year, y = ifelse(asset.est > 0, 
                                                  asset.est + asset.se + 0.2*asset.se,
                                                  asset.est - asset.se - 0.2*asset.se), label = asset),
            size = 3,
            position = position_dodge(width = 0.45)) +
  scale_x_continuous(name = "",
                     breaks = 1:6,
                     labels = c("Mayalibit", "TNTC", "Kaimana", "Kofiau", "Dampier", "Misool")) +
  scale_y_continuous(name = "") +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95)),
                    guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Material Assets")


# MARINE TENURE
tenure.impactplot.byMPA <- 
  ggplot(impactplotdata.byMPA) +
  geom_bar(aes(x = mpa, y = tenure.est, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3,
           show.legend = F) +
  geom_errorbar(aes(x = mpa, ymin = tenure.est - tenure.se, ymax = tenure.est + tenure.se, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = mpa, group = year, y = ifelse(tenure.est > 0, 
                                                  tenure.est + tenure.se + 0.2*tenure.se,
                                                  tenure.est - tenure.se - 0.2*tenure.se), label = tenure),
            size = 3,
            position = position_dodge(width = 0.45)) +
  scale_x_continuous(name = "",
                     breaks = 1:6,
                     labels = c("Mayalibit", "TNTC", "Kaimana", "Kofiau", "Dampier", "Misool")) +
  scale_y_continuous(name = "") +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95)),
                    guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Marine Tenure")


# SCHOOL ENROLLMENT
enrol.impactplot.byMPA <- 
  ggplot(impactplotdata.byMPA) +
  geom_bar(aes(x = mpa, y = enrol.est, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3,
           show.legend = F) +
  geom_errorbar(aes(x = mpa, ymin = enrol.est - enrol.se, ymax = enrol.est + enrol.se, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = mpa, group = year, y = ifelse(enrol.est > 0, 
                                                  enrol.est + enrol.se + 0.2*enrol.se,
                                                  enrol.est - enrol.se - 0.2*enrol.se), label = enrol),
            size = 3,
            position = position_dodge(width = 0.45)) +
  scale_x_continuous(name = "MPA",
                     breaks = 1:6,
                     labels = c("Mayalibit", "TNTC", "Kaimana", "Kofiau", "Dampier", "Misool")) +
  scale_y_continuous(name = "Average Treatment Effect") +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95)),
                    guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "School Enrollment")


# PLACE ATTACHMENT
attach.impactplot.byMPA <- 
  ggplot(impactplotdata.byMPA) +
  geom_bar(aes(x = mpa, y = attach.est, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3,
           show.legend = F) +
  geom_errorbar(aes(x = mpa, ymin = attach.est - attach.se, ymax = attach.est + attach.se, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = mpa, group = year, y = ifelse(attach.est > 0, 
                                                  attach.est + attach.se + 0.2*attach.se,
                                                  attach.est - attach.se - 0.2*attach.se), label = attach),
            size = 3,
            position = position_dodge(width = 0.45)) +
  scale_x_continuous(name = "",
                     breaks = 1:6,
                     labels = c("Mayalibit", "TNTC", "Kaimana", "Kofiau", "Dampier", "Misool")) +
  scale_y_continuous(name = "Average Treatment Effect") +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95)),
                    guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Place Attachment")



# LEGEND, to add to grid.arrange of all plots
legendplot.impactplot.byMPA <- 
  ggplot(impactplotdata.byMPA) +
  geom_bar(aes(x = mpa, y = attach.est, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3) +
  geom_errorbar(aes(x = mpa, ymin = attach.est - attach.se, ymax = attach.est + attach.se, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.1,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = mpa, group = year, y = ifelse(attach.est > 0, 
                                                  attach.est + attach.se + 0.2*attach.se,
                                                  attach.est - attach.se - 0.2*attach.se), 
                label = attach),
            size = 3,
            position = position_dodge(width = 0.35)) +
  scale_x_continuous(name = "",
                     breaks = 1:6,
                     labels = c("Mayalibit", "TNTC", "Kaimana", "Kofiau", "Dampier", "Misool")) +
  scale_y_continuous(name = "Average Treatment Effect") +
  scale_fill_manual(name = "",
                    labels = c("2 Year\nImpacts", "4 Year\nImpacts"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  coord_flip() + plot.theme.impactplots + labs(title = "Place Attachment")


legend.impactplots <- g_legend(legendplot.impactplot.byMPA)


# grid.arrange all plots into one figure
impactplots.byMPA <- arrangeGrob(hfs.impactplot.byMPA, asset.impactplot.byMPA, tenure.impactplot.byMPA, 
                                  enrol.impactplot.byMPA, attach.impactplot.byMPA, legend.impactplots, 
                                  ncol = 3, padding = 30) # adjust number of columns if adding or removing number of plots shown in one image


# read to .png file
png('x_Flat_data_files/1_Social/Outputs/BHS.impactplots.byMPA.png',
    units="in",height=6,width=9.5,res=400)
grid.newpage()
grid.draw(impactplots.byMPA)
dev.off()




rm(hfs.t2.att.byMPA,hfs.t4.att.byMPA,
   attach.t2.att.byMPA,attach.t4.att.byMPA,
   tenure.t2.att.byMPA,tenure.t4.att.byMPA,
   enrol.t2.att.byMPA,enrol.t4.att.byMPA,
   attach.t2.att.byMPA,attach.t4.att.byMPA,
   legendplot.impactplot.byMPA,legend.impactplots)
