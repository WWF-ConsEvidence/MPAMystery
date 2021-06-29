# 
# code: take DiD.data.SettlMatch dataframe from DiD_settlement_level_social_impacts.R and calculate settlement level impacts
# --- t2, t3, and t4, across SBS and BHS


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION XX: Pre-process data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


###generating short MPA names
mpa.nam <- mpa.nam %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"Telma",
                                ifelse(MPAID==2,"TNTC",
                                       ifelse(MPAID==3,"Kaimana",
                                              ifelse(MPAID==4,"Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==15,"Alor",
                                                                          ifelse(MPAID==16,"Flotim",
                                                                                 ifelse(MPAID==17,"Kei",
                                                                                        ifelse(MPAID==18,"Koon","")))))))))),
         MPAName_short = factor(MPAName_short,
                                levels = c("Telma","TNTC","Kaimana","Kofiau","Dampier","Misool","Alor","Flotim","Kei","Koon"),
                                ordered = T))


# variable names for social impact DiD
varNames <- c("FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
varNames_noZ <- c("FSIndex", "MAIndex", "MTIndex", "PAIndex", "SERate")

# groups of matched settlements (1:3 matches of treatment to control settlements)
groups <- 
  pair.control.settl %>%
  group_by(treat.id) %>%
  summarise(group.id = unique(treat.id),
            pair1 = ctrl.id[1],
            pair2 = ctrl.id[2],
            pair3 = ctrl.id[3],
            pair.id1 = pair.id[1],
            pair.id2 = pair.id[2],
            pair.id3 = pair.id[3]) %>%
  melt(id.vars=c("group.id", "pair.id1", "pair.id2", "pair.id3"), value.name = "SettlementID") %>%
  dplyr::select(-variable) %>% filter(!is.na(SettlementID))

match.to.DiD.data <-
  DiD.data.SettlMatch[,c("SettlementID","pair.id")] %>%
  group_by(SettlementID, pair.id) %>%
  mutate(group.id = groups$group.id[which((groups$pair.id1==pair.id | 
                                             groups$pair.id2==pair.id | 
                                             groups$pair.id3==pair.id) & 
                                            groups$SettlementID==SettlementID)]) %>%
  ungroup()

DiD.data.SettlMatch <-
  cbind.data.frame(DiD.data.SettlMatch, match.to.DiD.data[,"group.id"]) %>%
  mutate(SettlementID = as.factor(SettlementID),
         TreatmentF = ifelse(Treatment==0, 0, group.id),
         pair.id = as.factor(pair.id),
         group.id = as.factor(group.id),
         TreatmentF = as.factor(TreatmentF))

group.list <- unique(groups$group.id)

summary(DiD.data.SettlMatch)

test <- unique(DiD.data.SettlMatch[,c("group.id", "SettlementID", "Treatment", "TreatmentF")])


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION XX: DiD Model for heterogeneous impact across settlements (BHS/SBS) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# make empty output dataframes for t2, t3, and t4 impacts
model.out.settlevel <- data.frame()


# ---- DiD Regressions: Heterogeneous impact by settlement, t2, t3, and t4 ----
# NOTE: t2 impacts exist for all BHS MPAs, as well as Koon MPA in SBS
# NOTE: t3 impacts exist for SBS MPAs only -- Alor, Flotim, Kei
# NOTE: t4 impacts exist only for BHS MPAs

for (i in varNames_noZ) {
  print(i)
  
  Y <- DiD.data.SettlMatch[,i]
  w <- DiD.data.SettlMatch[,"dist.wt"]
  
  regValue <- felm(Y  ~   TreatmentF + yearsPostF +  TreatmentF:yearsPostF + 
                     n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                   | SettlementID + pair.id | 0 | SettlementID + pair.id + group.id, data = DiD.data.SettlMatch, exactDOF = TRUE, weights=w)
  

  reg.broom <- tidy(regValue) %>% 
    mutate(keep = ifelse(grepl("TreatmentF", term) & grepl(":yearsPost", term), 1, 0),
           SettlementID = sub("TreatmentF", "", term),
           SettlementID = sub(":yearsPostF*.", "", SettlementID),
           time = paste("t", gsub(".*PostF", "\\1", term), sep = "")) %>%
    filter(keep==1) %>% 
    dplyr::select(-keep) %>%
    mutate(Response = i) %>%
    na.omit()
  

  model.out.settlevel <- rbind(model.out.settlevel, reg.broom)
}


##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
model.out.settlevel1 <- model.out.settlevel %>% 
  mutate(SettlementID = as.numeric(SettlementID),
         domain=ifelse(Response=="FSIndex"," Health (Food Security)",
                       ifelse(Response=="MAIndex","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) %>%
  left_join(Settlements[,c("SettlementID", "SettlementName", "MPAID")], by = "SettlementID") %>%
  left_join(soc.coord[,c("SettlementID", "lat", "long")], by = "SettlementID") %>%
  mutate(MPAID=ifelse(SettlementID %in% c(113,82,81,83,84), 7,
                      ifelse(SettlementID %in% c(114,115,93,94,92), 8,
                             ifelse(SettlementID %in% c(85:90,95,91), 9, MPAID))))

export(model.out.settlevel1, paste(resultPath, 'settlevel_impacts_1-3match_20210510.csv', sep="/"))


# OLD WAY
for (i in varNames_noZ) {
    print(i)
    
    Y <- DiD.data.SettlMatch[,i]
    w <- DiD.data.SettlMatch[,"dist.wt"]
    
    regValue <- felm(Y  ~   Treatment + yearsPostF + group.id + Treatment:yearsPostF + Treatment:group.id + 
                       yearsPostF:group.id + Treatment:yearsPostF:group.id +
                       n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                     | SettlementID + pair.id | 0 | SettlementID + pair.id + group.id, data = DiD.data.SettlMatch, exactDOF = TRUE, weights = w)
    
    reg.broom <- tidy(regValue) %>% 
      mutate(keep = ifelse((grepl("Treatment:", term) & grepl("yearsPost", term) & grepl("group.id", term)) |
                             (grepl("yearsPost", term) & grepl("group.id", term)) |
                             term%in%c("Treatment:yearsPostF2", "Treatment:yearsPostF3", "Treatment:yearsPostF4",
                                       "yearsPostF2", "yearsPostF3", "yearsPostF4") , 1, 0),
             SettlementID = sub(".*group.id", "", term),
             SettlementID = ifelse(term%in%c("Treatment:yearsPostF2", "Treatment:yearsPostF3", "Treatment:yearsPostF4",
                                             "yearsPostF2", "yearsPostF3", "yearsPostF4"), 1, SettlementID),
             time1 = paste("t", gsub(".*PostF", "\\1", term), sep = ""),
             time2 = paste("t", gsub(".*PostF(.+):group.*", "\\1", term), sep = ""),
             time = ifelse(nchar(time1)==2, time1, time2)) %>%
      filter(keep==1) %>% 
      dplyr::select(-c(keep, time1, time2)) %>%
      mutate(term = ifelse((grepl("Treatment:", term) & grepl("yearsPost", term) & grepl("group.id", term)) |
                             term%in%c("Treatment:yearsPostF2", "Treatment:yearsPostF3", "Treatment:yearsPostF4"),
                           "Impact", "Control")) %>%
      na.omit() %>%
      group_by(term, time) %>%
      mutate(estimate = ifelse(SettlementID!=1, estimate+estimate[SettlementID==1], estimate),
             Response = i) %>%
      filter(!(SettlementID==1 & time=="t3"))

    ## Rerun with Control (instead of Treatment) and yearsPostF to get "Treatment trend" estimates
    regValue.treatTrend <- felm(Y  ~   Control + yearsPostF + group.id + Control:yearsPostF + Control:group.id + 
                                  yearsPostF:group.id + Control:yearsPostF:group.id +
                                  n.child  + ed.level +  dom.eth + YearsResident + IndividualGender + IndividualAge + PrimaryLivelihood.bin
                                | SettlementID + pair.id | 0 | SettlementID + pair.id + group.id, data = DiD.data.SettlMatch, exactDOF = TRUE, weights = w)
    summary(regValue.treatTrend)
    
    reg.broom.treatTrend <- tidy(regValue.treatTrend) %>% 
      mutate(keep = ifelse((grepl("yearsPost", term) & grepl("group.id", term) & !grepl("Control:", term)) |
                             term%in%c("yearsPostF2", "yearsPostF3", "yearsPostF4") , 1, 0),
             SettlementID = sub(".*group.id", "", term),
             SettlementID = ifelse(term%in%c("yearsPostF2", "yearsPostF3", "yearsPostF4"), 1, SettlementID),
             time1 = paste("t", gsub(".*PostF", "\\1", term), sep = ""),
             time2 = paste("t", gsub(".*PostF(.+):group.*", "\\1", term), sep = ""),
             time = ifelse(nchar(time1)==2, time1, time2)) %>%
      filter(keep==1) %>% 
      dplyr::select(-c(keep, time1, time2)) %>%
      mutate(term = "Treatment") %>%
      na.omit() %>%
      group_by(term, time) %>%
      mutate(estimate = ifelse(SettlementID!=1, estimate+estimate[SettlementID==1], estimate),
             Response = i) %>%
      filter(!(SettlementID==1 & time=="t3"))
    
    model.out.settlevel <- rbind(model.out.settlevel, reg.broom, reg.broom.treatTrend)
}


##keeping only 2 relevant terms "Treatment:Post1" and "Post1" 
model.out.settlevel1 <- model.out.settlevel %>% 
  mutate(SettlementID = as.numeric(SettlementID),
         domain=ifelse(Response=="FSIndex"," Health (Food Security)",
                       ifelse(Response=="MAIndex","Economic Wellbeing (Material Assets)",
                              ifelse(Response=="MTIndex"," Empowerment (Marine Tenure)",
                                     ifelse(Response=="PAIndex"," Culture (Place Attachment)", "  Education (School Enrollment)")))),
         domain=gsub(" \\(", "\n \\(", domain)) %>%
  left_join(Settlements[,c("SettlementID", "SettlementName", "MPAID")], by = "SettlementID") %>%
  left_join(soc.coord[,c("SettlementID", "lat", "long")], by = "SettlementID") %>%
  mutate(MPAID=ifelse(SettlementID %in% c(113,82,81,83,84), 7,
                      ifelse(SettlementID %in% c(114,115,93,94,92), 8,
                             ifelse(SettlementID %in% c(85:90,95,91), 9, MPAID))))



export(model.out.settlevel1, paste(resultPath, 'settlevel_impacts_1-3match_MPAstandard_20210401.csv', sep="/"))

# check number of settlements from output per MPA, etc.

Num.setts.years <- HHData %>% group_by(MPAID,InterviewYear,MPAName) %>% summarise(num.setts = length(SettlementID))

Num.setts.impacts <- model.out.settlevel1 %>% filter(term=="Impact", Response=="FSIndex") %>% group_by(MPAID) %>% summarise(num.setts = length(SettlementID))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION XX: PLOT SETTLEMENT LEVEL IMPACTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# Define plot theme(s)
plot.theme <- theme(plot.title = element_text(size=16,
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
                    plot.subtitle = element_text(size=13,
                                                 face="italic",
                                                 colour="#303030",
                                                 lineheight = 1.2),
                    axis.ticks = element_blank(),
                    panel.background = element_rect(fill="white",
                                                    colour="#909090"),
                    panel.border = element_rect(fill=NA,
                                                size=0.25,
                                                colour="#303030"),
                    panel.grid = element_blank(),
                    plot.margin = margin(t=15,r=20,b=5,l=5,unit="pt"),
                    axis.title = element_text(size=12,
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
                    axis.text = element_text(size=12,
                                             angle=0,
                                             colour="#303030",
                                             lineheight=0.7),
                    legend.position = "right",
                    legend.justification = "right",
                    legend.box.spacing = unit(0.1,"cm"),
                    legend.margin = margin(l=10,unit="pt"))

legends <- guides(colour = guide_legend(title.theme = element_text(face="bold",
                                                                               size=12,
                                                                               angle=0,
                                                                               colour="#505050",
                                                                               lineheight=0.75),
                                                    label.theme = element_text(size=12,
                                                                               angle=0,
                                                                               colour="#505050",
                                                                               lineheight=1.25),
                                                    barheight = 10,
                                                    order = 1))
  
# prep data
model.out.settlevel.plotting <- 
  left_join(model.out.settlevel1, mpa.nam[,c("MPAID","MPAName_short")], by = "MPAID") %>%
  mutate(significant = ifelse(p.value<0.01,"<0.01",
                              ifelse(p.value>=0.01 & p.value<0.05, "<0.05", 
                                     ifelse(p.value>=0.05 & p.value<0.1, "<0.1", 
                                            ifelse(p.value>=0.1 & p.value<0.2, "<0.2", "None")))))

significant.colors <- c("<0.01" = "#16A085", "<0.05" = "#DFB306", "<0.1" = "#8E44AD", "<0.2" = "#B03A2E", "None" = "black")

# ---- Food security settlement level impacts ----
# POINTS
FS.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="FSIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Food Security", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
FS.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="FSIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.1,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Food Security", subtitle = "Settlement level impacts (t3/t4)")


# ---- Material assets settlement level impacts ----
MA.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Material Assets", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
MA.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2,2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Material Assets", subtitle = "Settlement level impacts (t3/t4)")


# ---- Marine tenure settlement level impacts ----
MT.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MTIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.1,2.2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Marine Tenure", subtitle = "Settlement level impacts (t3/t4)")


# SETT NAME LABELS
MT.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="MTIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.1,2.2)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Marine Tenure", subtitle = "Settlement level impacts (t3/t4)")


# ---- Place attachment settlement level impacts ----
PA.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="PAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.3,2.3)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Place Attachment", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
PA.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="PAIndex_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),
                     limits = c(-2.3,2.3)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "Place Attachment", subtitle = "Settlement level impacts (t3/t4)")


# ---- School enrollment settlement level impacts ----
SE.sett.impacts.byMPA <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="SERate_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_point(aes(colour = significant)) +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5),
                     limits = c(-2.6,2.6)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "School Enrollment", subtitle = "Settlement level impacts (t3/t4)")

# SETT NAME LABELS
SE.sett.impacts.byMPA.settnames <-
  ggplot(model.out.settlevel.plotting%>%filter(term=="Impact" & Response=="SERate_z" & !is.na(estimate)),
         aes(x = MPAName_short, y = estimate)) +
  geom_text(aes(label = SettlementName, colour = significant), size = 3.5, fontface = "bold") +
  geom_hline(aes(yintercept = 0), linetype = 3, size = 0.5, colour = "#303030") +
  scale_colour_manual(name = "Signficance",
                      values = significant.colors) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),
                     limits = c(-3,3.1)) +
  plot.theme + legends + 
  labs(x = "MPA", y = "Average treatment effect", title = "School Enrollment", subtitle = "Settlement level impacts (t3/t4)")


# ---- export ----


png(paste(resultPath,"FS.BHS.sett.t3-4.impacts.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(FS.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"MA.BHS.sett.t3-4.impacts.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(MA.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"MT.BHS.sett.t3-4.impacts.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(MT.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"PA.BHS.sett.t3-4.impacts.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(PA.sett.impacts.byMPA)
dev.off()

png(paste(resultPath,"SE.BHS.sett.t3-4.impacts.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(SE.sett.impacts.byMPA)
dev.off()


# SETT NAMES PLOTS
png(paste(resultPath,"20201216_1-3match_t3-4_wlabels/FS.sett.t3-4.impacts.wlabels.png",sep="/"),
    units="in",height=8,width=12,res=400)
plot(FS.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"20201216_1-3match_t3-4_wlabels/MA.sett.t3-4.impacts.wlabels.png",sep="/"),
    units="in",height=8,width=12,res=400)
plot(MA.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"20201216_1-3match_t3-4_wlabels/MT.sett.t3-4.impacts.wlabels.png",sep="/"),
    units="in",height=8,width=12,res=400)
plot(MT.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"20201216_1-3match_t3-4_wlabels/PA.sett.t3-4.impacts.wlabels.png",sep="/"),
    units="in",height=8,width=12,res=400)
plot(PA.sett.impacts.byMPA.settnames)
dev.off()

png(paste(resultPath,"20201216_1-3match_t3-4_wlabels/SE.sett.t3-4.impacts.wlabels.png",sep="/"),
    units="in",height=8,width=12,res=400)
plot(SE.sett.impacts.byMPA.settnames)
dev.off()

