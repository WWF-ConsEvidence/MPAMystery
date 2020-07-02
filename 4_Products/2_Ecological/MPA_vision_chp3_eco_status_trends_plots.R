# 
# code: MPA vision chapter 3, ecological indicator plotting

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT DATA, LIBRARIES, PLOT THEMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


pacman::p_load(rio,openxlsx,tidyr,dplyr,ggplot2)


# ---- import data ----

# - eco status indicators 
EcoStatus <-
  import('x_Flat_data_files/2_Ecological/Inputs/MPA_vision_chp3_eco_status.csv')

# - eco trends indicators
EcoTrends <-
  import('x_Flat_data_files/2_Ecological/Inputs/MPA_vision_chp3_eco_trends_20200417.xlsx',which='MPA.Trend.Data')

EcoTrends.3pt <-
  import('x_Flat_data_files/2_Ecological/Inputs/MPA_vision_chp3_eco_trends_20200417.xlsx',which='MPA.trends.3points')

# - for impact evaluation case study
Kei.eco.impacts <- 
  import('x_Flat_data_files/2_Ecological/Inputs/2019-07-14_DAB_Kei eco impacts.csv')


# ---- fix year group variable in eco status & trends data, define variable levels ----

EcoStatus <-
  EcoStatus %>%
  mutate(Province=factor(Province,levels=c("Aceh","West Sumatra","Riau Islands","Central Java","Bali",
                                           "South Sulawesi","Southeast Sulawesi","West Nusa Tenggara",
                                           "East Nusa Tenggara","Maluku","West Papua","Papua"),
                         ordered=T))
  
  
# EcoTrends <-
#   EcoTrends %>%
#   mutate(yeargroup=ifelse(grepl("Mar",group3),"3-5",
#                           ifelse(grepl("Jun",group3),"6-8",
#                                  ifelse(grepl("Sep",group3),"9-11",
#                                         ifelse(grepl("Dec",group3),"12-14",group3)))),
#          yeargroup=factor(yeargroup,levels=c("0-2","3-5","6-8","9-11","12-14","15-17",
#                                              "18-20","21-23","24-26","27-29","30-32","33-34"),
#                           ordered=T),
#          Province=factor(Province,levels=c("Aceh","West Sumatra","Riau Islands","Central Java","Bali",
#                                            "South Sulawesi","Southeast Sulawesi","West Nusa Tenggara",
#                                            "East Nusa Tenggara","Maluku","West Papua","Papua"),
#                          ordered=T))

EcoTrends.3pt <-
  EcoTrends.3pt %>%
  mutate(Province=factor(Province,levels=c("Aceh","Central Java","Southeast Sulawesi","East Nusa Tenggara",
                                           "Maluku","West Papua","West Papua & Papua"),
                                           ordered=T))
                                           
                                           
# ---- plot themes ----

plot.theme.impact <- theme(axis.ticks=element_blank(),
                      panel.background=element_rect(fill="white",
                                                    colour="#909090"),
                      panel.border=element_rect(fill=NA,
                                                size=0.25,
                                                colour="#C0C0C0"),
                      panel.grid.major.x=element_line(colour="#C0C0C0",
                                                      size=0.25,
                                                      linetype=3),
                      panel.grid.major.y=element_blank(),
                      plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                      plot.title=element_text(size=14,
                                              face="bold",
                                              colour="#303030",
                                              hjust=0.5),
                      plot.subtitle=element_text(size=13,
                                              face="bold.italic",
                                              colour="#303030",
                                              hjust=0.5),
                      axis.title=element_text(size=13,
                                              angle=0,
                                              face="bold",
                                              colour="#303030",
                                              hjust=0.5),
                      axis.text.y=element_text(size=11,
                                               angle=0,
                                               colour="#303030",
                                               lineheight=0.7),
                      axis.text.x=element_text(size=11,
                                               angle=0,
                                               colour="#303030",
                                               lineheight=0.9,
                                               hjust=1),
                      legend.position="top",
                      legend.justification="right",
                      legend.box.spacing=unit(0.1,"cm"))


plot.theme.trends <- theme(axis.ticks=element_blank(),
                      panel.background=element_rect(fill="white",
                                                    colour="#909090"),
                      panel.border=element_rect(fill=NA,
                                                size=0.25,
                                                colour="#C0C0C0"),
                      panel.grid.major.y=element_line(colour="#C0C0C0",
                                                      size=0.3,
                                                      linetype=3),
                      panel.grid.major.x=element_blank(),
                      plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                      axis.title=element_text(size=13,
                                              angle=0,
                                              face="bold",
                                              colour="#303030",
                                              hjust=0.5),
                      axis.text.y=element_text(size=11,
                                               angle=0,
                                               colour="#303030",
                                               lineheight=0.7),
                      axis.text.x=element_text(size=11,
                                               angle=55,
                                               colour="#303030",
                                               lineheight=0.9,
                                               hjust=1),
                      legend.position="top",
                      legend.justification="right",
                      legend.box.spacing=unit(0.1,"cm"))


# - file name for exporting figures
FigureFileName.chp3 <- 'x_Flat_data_files/2_Ecological/Outputs/MPA_Vision_Chp3/Figures_Analyzed_20200417'



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: IMPACT PLOT FOR CASE STUDY ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


Kei.keyfish.impact <- 
  ggplot(data=Kei.eco.impacts %>% filter(grepl("com",key) & !grepl("impact",key)) %>% mutate(key=factor(key,levels=c("MPA.rate.com","Control.rate.com"),ordered=T))) +
  geom_bar(aes(x=Type_of_Zone, 
               group=key, 
               y=mean,
               fill=key), 
           stat="identity", 
           position="dodge",
           width=0.6,
           colour="black") +  
  geom_errorbar(aes(x=Type_of_Zone,
                    group=key,
                    ymin=mean-se,
                    ymax=mean+se,
                    colour=key),
                stat="identity",
                position=position_dodge(width=0.6),
                width=0.1,
                size=0.7,
                show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_vline(aes(xintercept=1.5),
             linetype=2,
             colour="#303030",
             size=0.25) +
  geom_segment(aes(x=1.6,xend=1.6,y=mean[key=="MPA.rate.com" & Type_of_Zone=="Use"],
                   yend=mean[key=="Control.rate.com" & Type_of_Zone=="Use"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.65,y=mean[key=="MPA.rate.com" & Type_of_Zone=="Use"],
                   yend=mean[key=="MPA.rate.com" & Type_of_Zone=="Use"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.65,y=mean[key=="Control.rate.com" & Type_of_Zone=="Use"],
                   yend=mean[key=="Control.rate.com" & Type_of_Zone=="Use"]),
               lineend="square",
               size=1) +
  geom_point(data=data.frame(x=c(1.6,1.6,1.6,1.6,1.6),y=c(-57,-67,-77,-87,-97)),
             aes(x=x,y=y),
             shape=24,
             fill="black",
             size=2,
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.6,y=mean[key=="MPA.rate.com" & Type_of_Zone=="NTZ"],
                   yend=mean[key=="Control.rate.com" & Type_of_Zone=="NTZ"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.65,y=mean[key=="MPA.rate.com" & Type_of_Zone=="NTZ"],
                   yend=mean[key=="MPA.rate.com" & Type_of_Zone=="NTZ"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.65,y=mean[key=="Control.rate.com" & Type_of_Zone=="NTZ"],
                   yend=mean[key=="Control.rate.com" & Type_of_Zone=="NTZ"]),
               lineend="square",
               size=1) +
  geom_point(data=data.frame(x=c(0.6,0.6),y=c(-6,-12)),
             aes(x=x,y=y),
             shape=25,
             fill="black",
             size=2,
             show.legend=F) +
  geom_text(aes(x=0.6,
                y=-33,
                label="Negative\n'exacerbating'\nimpact"),
            size=4,
            fontface="italic") +
  geom_text(aes(x=1.62,
                y=-118,
                label="Positive\n'buffering'\nimpact"),
            size=4,
            fontface="italic") +
  scale_size_manual(values=0.75) + 
  scale_fill_manual(name="",
                    labels=c("MPA","Control"),
                    values=c("MPA.rate.com"=alpha("#1B448B",0.95),"Control.rate.com"=alpha("#9B9B9B",0.95))) +
  scale_colour_manual(values=c("MPA.rate.com"=alpha("#112850",0.7),"Control.rate.com"=alpha("#242424",0.7))) +
  plot.theme.impact + labs(x="Type of Zone",y="Change in Biomass\nsince Baseline (kg/yr)\n",title="Key Fisheries Species Impacts",subtitle="Kei Kecil MPA")


# - export impact plot
png(paste(FigureFileName.chp3,"Kei.keyfish.impacts.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Kei.keyfish.impact)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PROVINCIAL LEVEL ECO FIGURES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


Data_to_remove <- 
  EcoTrends %>%
  pivot_longer(cols=c("Herb_Biomass_N","Herb_Abundance_N","Key_Biomass_N","Key_Abundance_N",
                      "Humphead_Biomass_N","Humphead_Abundance_N","Bumphead_Biomass_N","Bumphead_Abundance_N",
                      "Coral_Cover_N"),names_to="Indicator",values_to="N",values_drop_na=T) %>%
  group_by(Indicator,Province) %>%
  summarise(median.n=median(N))
  

# ---- BIOMASS ----

# ---- Herbivore biomass trend data & plot ----
Herb.biomass.data <-
  left_join(EcoTrends %>% filter(!is.na(Herb_Biomass)),Data_to_remove %>% filter(Indicator=="Herb_Biomass_N"),by="Province") %>%
  filter(Herb_Biomass_N>3 & Herb_Biomass_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Herb_Biomass, Herb_Biomass_SE, Herb_Biomass_N, Herb.B_CI.l, Herb.B_CI.u)
  
  
Herb.biomass.yeargroup <-
  Herb.biomass.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Herb.biomass.data <-
  Herb.biomass.data %>%
  rbind.data.frame(data.frame(Province=c("East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku",
                                         "Southeast Sulawesi","Southeast Sulawesi",
                                         "West Papua"),
                              yeargroup=c("0-2","9-11","12-14",
                                          "0-2","6-8","9-11","12-14",
                                          "0-2","3-5",
                                          "12-14"),
                              as.data.frame(matrix(rep(NA,length(colnames(Herb.biomass.data[3:7]))),
                                                   ncol=length(colnames(Herb.biomass.data[3:7])),
                                                   nrow=10,
                                                   dimnames=list(NULL,colnames(Herb.biomass.data[3:7]))))))

  
Herb.biomass.plot <-
  ggplot(EcoTrends.3pt,
         aes(x=Province,y=Herb_Biomass)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Herb_Biomass-Herb_Biomass_SE,
                    ymax=Herb_Biomass+Herb_Biomass_SE,
                    group=yeargroup,alpha=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,950)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                       values=c(0.25, 0.4, 0.6, 0.8, 1)) + 
  plot.theme.trends + labs(y="Herbivore Biomass (kg/ha)\n")


# ---- Key Fisheries biomass trend data & plot ----
Key.biomass.data <-
  left_join(EcoTrends %>% filter(!is.na(Key_Biomass)),Data_to_remove %>% filter(Indicator=="Key_Biomass_N"),by="Province") %>%
  filter(Key_Biomass_N>3 & Key_Biomass_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Key_Biomass, Key_Biomass_SE, Key_Biomass_N, Key.B_CI.l, Key.B_CI.u)


Key.biomass.yeargroup <-
  Key.biomass.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Key.biomass.data <-
  Key.biomass.data %>%
  rbind.data.frame(data.frame(Province=c("East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku",
                                         "Southeast Sulawesi","Southeast Sulawesi",
                                         "West Papua"),
                              yeargroup=c("0-2","9-11","12-14",
                                          "0-2","6-8","9-11","12-14",
                                          "0-2","3-5",
                                          "12-14"),
                              as.data.frame(matrix(rep(NA,length(colnames(Key.biomass.data[3:7]))),
                                                   ncol=length(colnames(Key.biomass.data[3:7])),
                                                   nrow=10,
                                                   dimnames=list(NULL,colnames(Key.biomass.data[3:7]))))))


Key.biomass.plot <-
  ggplot(Key.biomass.data,
         aes(x=Province,y=Key_Biomass)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Key_Biomass-Key_Biomass_SE,
                    ymax=Key_Biomass+Key_Biomass_SE,
                    group=yeargroup,alpha=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,550)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.25, 0.4, 0.6, 0.8, 1)) + 
  plot.theme.trends + labs(y="Key Fisheries Species\nBiomass (kg/ha)\n")


# ---- Bumphead biomass trend data & plot ----
Bumphead.biomass.data <-
  left_join(EcoTrends %>% filter(!is.na(Bumphead_Biomass)),Data_to_remove %>% filter(Indicator=="Bumphead_Biomass_N"),by="Province") %>%
  filter(Bumphead_Biomass_N>3 & Bumphead_Biomass_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Bumphead_Biomass, Bumphead_Biomass_SE, Bumphead_Biomass_N, Bump.B_CI.l, Bump.B_CI.u)


Bumphead.biomass.yeargroup <-
  Bumphead.biomass.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Bumphead.biomass.data <-
  Bumphead.biomass.data %>%
  rbind.data.frame(data.frame(Province=c("East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku",
                                         "Southeast Sulawesi","Southeast Sulawesi",
                                         "West Papua"),
                              yeargroup=c("0-2","9-11","12-14",
                                          "0-2","6-8","9-11","12-14",
                                          "0-2","3-5",
                                          "12-14"),
                              as.data.frame(matrix(rep(NA,length(colnames(Bumphead.biomass.data[3:7]))),
                                                   ncol=length(colnames(Bumphead.biomass.data[3:7])),
                                                   nrow=10,
                                                   dimnames=list(NULL,colnames(Bumphead.biomass.data[3:7]))))))


Bumphead.biomass.plot <-
  ggplot(Bumphead.biomass.data,
         aes(x=Province,y=Bumphead_Biomass)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Bumphead_Biomass-Bumphead_Biomass_SE,
                    ymax=Bumphead_Biomass+Bumphead_Biomass_SE,
                    group=yeargroup,alpha=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  geom_rect(aes(xmin=1.7,
                xmax=1.9,
                ymin=0.3,
                ymax=14),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.8,y=7,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,150)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.25, 0.4, 0.6, 0.8, 1)) + 
  plot.theme.trends + labs(y="Bumphead Parrotfish\nBiomass (kg/ha)\n")


# ---- Humphead (Napoleon Wrasse) biomass trend data & plot ----
Humphead.biomass.data <-
  left_join(EcoTrends %>% filter(!is.na(Humphead_Biomass)),Data_to_remove %>% filter(Indicator=="Humphead_Biomass_N"),by="Province") %>%
  filter(Humphead_Biomass_N>3 & Humphead_Biomass_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Humphead_Biomass, Humphead_Biomass_SE, Humphead_Biomass_N, Hump.B_CI.l, Hump.B_CI.u)


Humphead.biomass.yeargroup <-
  Humphead.biomass.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Humphead.biomass.data <-
  Humphead.biomass.data %>%
  rbind.data.frame(data.frame(Province=c("East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku",
                                         "Southeast Sulawesi","Southeast Sulawesi",
                                         "West Papua"),
                              yeargroup=c("0-2","9-11","12-14",
                                          "0-2","6-8","9-11","12-14",
                                          "0-2","3-5",
                                          "12-14"),
                              as.data.frame(matrix(rep(NA,length(colnames(Humphead.biomass.data[3:7]))),
                                                   ncol=length(colnames(Humphead.biomass.data[3:7])),
                                                   nrow=10,
                                                   dimnames=list(NULL,colnames(Humphead.biomass.data[3:7]))))))


Humphead.biomass.plot <-
  ggplot(Humphead.biomass.data,
         aes(x=Province,y=Humphead_Biomass)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Humphead_Biomass-Humphead_Biomass_SE,
                    ymax=Humphead_Biomass+Humphead_Biomass_SE,
                    group=yeargroup,alpha=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  geom_rect(aes(xmin=1.7,
                xmax=1.9,
                ymin=0.05,
                ymax=2.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.8,y=1,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,22)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.25, 0.4, 0.6, 0.8, 1)) + 
  plot.theme.trends + labs(y="Humphead Napoleon Wrasse\nBiomass (kg/ha)\n")



# ---- ABUNDANCE ----

# ---- Herbivore abundance status plot ----
Herb.abundance.status <-
  ggplot(EcoStatus %>% filter(!is.na(Herb_Abundance)),
         aes(x=Province, y=Herb_Abundance)) +
  geom_bar(stat="identity",
           fill="#1B448BD9",
           colour="#505050") +
  geom_errorbar(aes(ymin=Herb_Abundance-Herb_Abundance_SE,
                    ymax=Herb_Abundance+Herb_Abundance_SE),
                stat="identity",
                colour="#0A1D4E99",
                width=0.2) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,4000)) +
  labs(y="Herbivore Abundance\n(individuals/ha)\n") +
  plot.theme.trends


# ---- Herbivore abundance trend data & plot ----
Herb.abundance.data <-
  left_join(EcoTrends %>% filter(!is.na(Herb_Abundance)),Data_to_remove %>% filter(Indicator=="Herb_Abundance_N"),by="Province") %>%
  filter(Herb_Abundance_N>3 & Herb_Abundance_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Herb_Abundance, Herb_Abundance_SE, Herb_Abundance_N, Herb.A_CI.l, Herb.A_CI.u)


Herb.abundance.yeargroup <-
  Herb.abundance.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Herb.abundance.data <-
  Herb.abundance.data %>%
  rbind.data.frame(data.frame(Province=c("Central Java",
                                         "East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku","Maluku",
                                         "South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi",
                                         "Southeast Sulawesi","Southeast Sulawesi","Southeast Sulawesi",
                                         "West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara",
                                         "West Papua","West Papua"),
                              yeargroup=c("0-2",
                                          "0-2","9-11","12-14","15-17",
                                          "0-2","6-8","9-11","12-14","15-17",
                                          "0-2","3-5","6-8","9-11","15-17",
                                          "0-2","3-5","15-17",
                                          "3-5","6-8","9-11","12-14","15-17",
                                          "12-14","15-17"),
                              as.data.frame(matrix(rep(NA,length(colnames(Herb.abundance.data[3:7]))),
                                                   ncol=length(colnames(Herb.abundance.data[3:7])),
                                                   nrow=25,
                                                   dimnames=list(NULL,colnames(Herb.abundance.data[3:7]))))))


Herb.abundance.plot <-
  ggplot(Herb.abundance.data %>% filter(Province!="Aceh"),
         aes(x=Province,y=Herb_Abundance)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Herb_Abundance-Herb_Abundance_SE,
                    ymax=Herb_Abundance+Herb_Abundance_SE,
                    group=yeargroup,
                    colour=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,4800)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.1, 0.3, 0.5, 0.7, 0.85, 1)) + 
  scale_colour_manual(values=c("#0A1D4E60","#0A1D4E60","#0A1D4E80",
                               "#0A1D4E80","#0A1D4E90","#0A1D4E99")) +
  plot.theme.trends + labs(y="Herbivore Abundance\n(individuals/ha)\n")


# ---- Key Fisheries abundance status plot ----
Key.abundance.status <-
  ggplot(EcoStatus %>% filter(!is.na(Key_Abundance)),
         aes(x=Province, y=Key_Abundance)) +
  geom_bar(stat="identity",
           fill="#1B448BD9",
           colour="#505050") +
  geom_errorbar(aes(ymin=Key_Abundance-Key_Abundance_SE,
                    ymax=Key_Abundance+Key_Abundance_SE),
                stat="identity",
                colour="#0A1D4E99",
                width=0.2) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,4000)) +
  labs(y="Key Fisheries Species\nAbundance (individuals/ha)\n") +
  plot.theme.trends


# ---- Key Fisheries abundance trend data & plot ----
Key.abundance.data <-
  left_join(EcoTrends %>% filter(!is.na(Key_Abundance)),Data_to_remove %>% filter(Indicator=="Key_Abundance_N"),by="Province") %>%
  filter(Key_Abundance_N>3 & Key_Abundance_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Key_Abundance, Key_Abundance_SE, Key_Abundance_N, Key.A_CI.l, Key.A_CI.u)


Key.abundance.yeargroup <-
  Key.abundance.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Key.abundance.data <-
  Key.abundance.data %>%
  rbind.data.frame(data.frame(Province=c("Bali","Bali","Bali","Bali","Bali",
                                         "Central Java",
                                         "East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku","Maluku",
                                         "Riau Islands","Riau Islands","Riau Islands","Riau Islands","Riau Islands",
                                         "South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi",
                                         "Southeast Sulawesi","Southeast Sulawesi","Southeast Sulawesi",
                                         "West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara",
                                         "West Papua","West Papua",
                                         "West Sumatra","West Sumatra","West Sumatra","West Sumatra","West Sumatra"),
                              yeargroup=c("0-2","3-5","9-11","12-14","15-17",
                                          "0-2",
                                          "0-2","9-11","12-14","15-17",
                                          "0-2","6-8","9-11","12-14","15-17",
                                          "0-2","3-5","9-11","12-14","15-17",
                                          "0-2","3-5","6-8","9-11","15-17",
                                          "0-2","3-5","15-17",
                                          "3-5","6-8","9-11","12-14","15-17",
                                          "12-14","15-17",
                                          "0-2","3-5","6-8","12-14","15-17"),
                              as.data.frame(matrix(rep(NA,length(colnames(Key.abundance.data[3:7]))),
                                                   ncol=length(colnames(Key.abundance.data[3:7])),
                                                   nrow=40,
                                                   dimnames=list(NULL,colnames(Key.abundance.data[3:7]))))))


Key.abundance.plot <-
  ggplot(Key.abundance.data %>% filter(Province!="Aceh"),
         aes(x=Province,y=Key_Abundance)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Key_Abundance-Key_Abundance_SE,
                    ymax=Key_Abundance+Key_Abundance_SE,
                    group=yeargroup,
                    colour=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=7.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=8.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=9.5,linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,3200)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.1, 0.3, 0.5, 0.7, 0.85, 1)) + 
  scale_colour_manual(values=c("#0A1D4E60","#0A1D4E60","#0A1D4E80",
                               "#0A1D4E80","#0A1D4E90","#0A1D4E99")) +
  plot.theme.trends + labs(y="Key Fisheries Species\nAbundance (individuals/ha)\n")


# ---- Bumphead abundance trend data & plot ----
Bumphead.abundance.data <-
  left_join(EcoTrends %>% filter(!is.na(Bumphead_Abundance)),Data_to_remove %>% filter(Indicator=="Bumphead_Abundance_N"),by="Province") %>%
  filter(Bumphead_Abundance_N>3 & Bumphead_Abundance_N>0.5*median.n & Bumphead_Abundance<100) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Bumphead_Abundance, Bumphead_Abundance_SE, Bumphead_Abundance_N, Bump.A_CI.l, Bump.A_CI.u)


Bumphead.abundance.yeargroup <-
  Bumphead.abundance.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Bumphead.abundance.data <-
  Bumphead.abundance.data %>%
  rbind.data.frame(data.frame(Province=c("Central Java",
                                         "East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku","Maluku",
                                         "South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi",
                                         "Southeast Sulawesi","Southeast Sulawesi","Southeast Sulawesi",
                                         "West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara",
                                         "West Papua","West Papua",
                                         "West Sumatra","West Sumatra","West Sumatra","West Sumatra","West Sumatra"),
                              yeargroup=c("0-2",
                                          "0-2","9-11","12-14","15-17",
                                          "0-2","6-8","9-11","12-14","15-17",
                                          "0-2","3-5","6-8","9-11","15-17",
                                          "0-2","3-5","15-17",
                                          "3-5","6-8","9-11","12-14","15-17",
                                          "12-14","15-17",
                                          "0-2","3-5","6-8","12-14","15-17"),
                              as.data.frame(matrix(rep(NA,length(colnames(Bumphead.abundance.data[3:7]))),
                                                   ncol=length(colnames(Bumphead.abundance.data[3:7])),
                                                   nrow=30,
                                                   dimnames=list(NULL,colnames(Bumphead.abundance.data[3:7]))))))


Bumphead.abundance.plot <-
  ggplot(Bumphead.abundance.data %>% filter(Province!="Aceh"),
         aes(x=Province,y=Bumphead_Abundance)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Bumphead_Abundance-Bumphead_Abundance_SE,
                    ymax=Bumphead_Abundance+Bumphead_Abundance_SE,
                    group=yeargroup,
                    colour=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=7.5,linetype=2,size=0.25,colour="#505050") +
  geom_rect(aes(xmin=0.99, #west sumatra
                xmax=1.15,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.07,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=1.67, #central java
                xmax=1.83,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.75,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=1.83,
                xmax=1.99,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.91,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=1.99,
                xmax=2.15,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=2.07,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=2.15,
                xmax=2.32,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=2.23,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=2.32,
                xmax=2.48,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=2.4,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=3.15, #south sulawesi
                xmax=3.32,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=3.23,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=4.51, #west nusa tenggara
                xmax=4.67,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=4.59,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=5.67, #east nusa tenggara
                xmax=5.83,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=5.75,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,10)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.1, 0.3, 0.5, 0.7, 0.85, 1)) +
  scale_colour_manual(values=c("#0A1D4E60","#0A1D4E60","#0A1D4E80",
                               "#0A1D4E80","#0A1D4E90","#0A1D4E99")) +
  plot.theme.trends + labs(y="Bumphead Parrotfish Species\nAbundance (individuals/ha)\n")


# ---- Humphead (Napoleon wrasse) abundance trend data & plot ----
Humphead.abundance.data <-
  left_join(EcoTrends %>% filter(!is.na(Humphead_Abundance)),Data_to_remove %>% filter(Indicator=="Humphead_Abundance_N"),by="Province") %>%
  filter(Humphead_Abundance_N>3 & Humphead_Abundance_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Humphead_Abundance, Humphead_Abundance_SE, Humphead_Abundance_N, Hump.A_CI.l, Hump.A_CI.u)


Humphead.abundance.yeargroup <-
  Humphead.abundance.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Humphead.abundance.data <-
  Humphead.abundance.data %>%
  rbind.data.frame(data.frame(Province=c("Central Java",
                                         "East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku","Maluku",
                                         "Riau Islands","Riau Islands","Riau Islands","Riau Islands","Riau Islands",
                                         "South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi",
                                         "Southeast Sulawesi","Southeast Sulawesi","Southeast Sulawesi",
                                         "West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara",
                                         "West Papua","West Papua",
                                         "West Sumatra","West Sumatra","West Sumatra","West Sumatra","West Sumatra"),
                              yeargroup=c("0-2",
                                          "0-2","9-11","12-14","15-17",
                                          "0-2","6-8","9-11","12-14","15-17",
                                          "0-2","3-5","9-11","12-14","15-17",
                                          "0-2","3-5","6-8","9-11","15-17",
                                          "0-2","3-5","15-17",
                                          "3-5","6-8","9-11","12-14","15-17",
                                          "12-14","15-17",
                                          "0-2","3-5","6-8","12-14","15-17"),
                              as.data.frame(matrix(rep(NA,length(colnames(Humphead.abundance.data[3:7]))),
                                                   ncol=length(colnames(Humphead.abundance.data[3:7])),
                                                   nrow=35,
                                                   dimnames=list(NULL,colnames(Humphead.abundance.data[3:7]))))))


Humphead.abundance.plot <-
  ggplot(Humphead.abundance.data %>% filter(Province!="Aceh"),
         aes(x=Province,y=Humphead_Abundance)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Humphead_Abundance-Humphead_Abundance_SE,
                    ymax=Humphead_Abundance+Humphead_Abundance_SE,
                    group=yeargroup,
                    colour=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=7.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=8.5,linetype=2,size=0.25,colour="#505050") +
  geom_rect(aes(xmin=0.99, #west sumatra
                xmax=1.15,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.07,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=1.83, #riau islands
                xmax=1.99,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=1.91,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=3.32, #central java
                xmax=3.48,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=3.4,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  geom_rect(aes(xmin=4.15, #south sulawesi
                xmax=4.32,
                ymin=0.05,
                ymax=1.2),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#C0C0C0",
            alpha=0.9) +
  geom_text(aes(x=4.23,y=0.55,label="Zero"),
            angle=90, 
            size=2.5,
            colour="#A0A0A0") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,10)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.1, 0.3, 0.5, 0.7, 0.85, 1)) + 
  scale_colour_manual(values=c("#0A1D4E60","#0A1D4E60","#0A1D4E80",
                               "#0A1D4E80","#0A1D4E90","#0A1D4E99")) +
  plot.theme.trends + labs(y="Humphead Napoleon Wrasse Species\nAbundance (individuals/ha)\n")


# ---- CORAL COVER ----

# ---- Coral cover status plot ----
Coral.cover.status <-
  ggplot(EcoStatus %>% filter(!is.na(Coral_Cover)),
         aes(x=Province, y=Coral_Cover)) +
  geom_bar(stat="identity",
           fill="#1B448BD9",
           colour="#505050") +
  geom_errorbar(aes(ymin=Coral_Cover-Coral_Cover_SE,
                    ymax=Coral_Cover+Coral_Cover_SE),
                stat="identity",
                colour="#0A1D4E99",
                width=0.2) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,100),
                     breaks=c(0,20,40,60,80,100)) +
  labs(y="Coral Cover (%)\n") +
  plot.theme.trends


# ---- Coral cover trend data & plot ----
Coral.cover.data <-
  left_join(EcoTrends %>% filter(!is.na(Coral_Cover)),Data_to_remove %>% filter(Indicator=="Coral_Cover_N"),by="Province") %>%
  filter(Coral_Cover_N>3 & Coral_Cover_N>0.5*median.n) %>% # filter data to only include province/yeargroup where there are over 3 sites, and more than 0.5 of the median number of sites across all yeargroups for that province
  select(Province, yeargroup, Coral_Cover, Coral_Cover_SE, Coral_Cover_N, Coral_CI.l, Coral_CI.u)


Coral.cover.yeargroup <-
  Coral.cover.data %>%
  group_by(yeargroup) %>%
  summarise(Province.peryeargroup=list(as.character(unique(Province))),
            n.province=length(unique(Province)))

Coral.cover.data <-
  Coral.cover.data %>%
  rbind.data.frame(data.frame(Province=c("Bali","Bali","Bali","Bali","Bali",
                                         "Central Java","Central Java",
                                         "East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara","East Nusa Tenggara",
                                         "Maluku","Maluku","Maluku","Maluku",
                                         "Papua","Papua","Papua","Papua","Papua",
                                         "Riau Islands","Riau Islands","Riau Islands","Riau Islands","Riau Islands",
                                         "South Sulawesi","South Sulawesi","South Sulawesi","South Sulawesi",
                                         "Southeast Sulawesi","Southeast Sulawesi","Southeast Sulawesi",
                                         "West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara","West Nusa Tenggara",
                                         "West Papua","West Papua",
                                         "West Sumatra","West Sumatra","West Sumatra","West Sumatra","West Sumatra"),
                              yeargroup=c("0-2","3-5","9-11","12-14","15-17",
                                          "0-2","12-14",
                                          "0-2","9-11","12-14","15-17",
                                          "0-2","6-8","12-14","15-17",
                                          "0-2","3-5","6-8","12-14","15-17",
                                          "0-2","3-5","9-11","12-14","15-17",
                                          "0-2","3-5","6-8","15-17",
                                          "0-2","3-5","15-17",
                                          "3-5","6-8","12-14","15-17",
                                          "12-14","15-17",
                                          "0-2","3-5","6-8","12-14","15-17"),
                              as.data.frame(matrix(rep(NA,length(colnames(Coral.cover.data[3:7]))),
                                                   ncol=length(colnames(Coral.cover.data[3:7])),
                                                   nrow=43,
                                                   dimnames=list(NULL,colnames(Coral.cover.data[3:7]))))))


Coral.cover.plot <-
  ggplot(Coral.cover.data %>% filter(Province!="Aceh"),
         aes(x=Province,y=Coral_Cover)) +
  geom_bar(aes(group=yeargroup,alpha=yeargroup),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Coral_Cover-Coral_Cover_SE,
                    ymax=Coral_Cover+Coral_Cover_SE,
                    group=yeargroup,
                    colour=yeargroup),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=7.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=8.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=9.5,linetype=2,size=0.25,colour="#505050") +
  geom_vline(xintercept=10.5,linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,100)) +
  scale_alpha_manual(name="Years since MPA Initiation",
                     values=c(0.1, 0.3, 0.5, 0.7, 0.85, 1)) +
  scale_colour_manual(values=c("#0A1D4E60","#0A1D4E60","#0A1D4E80",
                               "#0A1D4E80","#0A1D4E90","#0A1D4E99")) +
  plot.theme.trends + labs(y="Coral Cover (%)\n")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: EXPORT FIGURES & DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- Biomass ----

png(paste(FigureFileName.chp3,"Herb.biomass.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Herb.biomass.plot)
dev.off()

png(paste(FigureFileName.chp3,"Key.biomass.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Key.biomass.plot)
dev.off()

png(paste(FigureFileName.chp3,"Bumphead.biomass.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Bumphead.biomass.plot)
dev.off()

png(paste(FigureFileName.chp3,"Humphead.biomass.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Humphead.biomass.plot)
dev.off()


# ---- Abundance ----

png(paste(FigureFileName.chp3,"Herb.abundance.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Herb.abundance.plot)
dev.off()

png(paste(FigureFileName.chp3,"Key.abundance.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Key.abundance.plot)
dev.off()

png(paste(FigureFileName.chp3,"Bumphead.abundance.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Bumphead.abundance.plot)
dev.off()

png(paste(FigureFileName.chp3,"Humphead.abundance.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Humphead.abundance.plot)
dev.off()


# ---- Coral cover ----

png(paste(FigureFileName.chp3,"Coral.cover.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Coral.cover.plot)
dev.off()


# - export data

wb <- createWorkbook("Chp_3_eco_trends_plotting_data") 
addWorksheet(wb,"Herb_biomass")
addWorksheet(wb,"Key_biomass")
addWorksheet(wb,"Herb_abundance")
addWorksheet(wb,"Key_abundance")
addWorksheet(wb,"Bumphead_abundance")
addWorksheet(wb,"Humphead_abundance")
addWorksheet(wb,"Coral_cover")


writeData(wb,"Herb_biomass",Herb.biomass.data)
writeData(wb,"Key_biomass",Key.biomass.data)
writeData(wb,"Herb_abundance",Herb.abundance.data)
writeData(wb,"Key_abundance",Key.abundance.data)
writeData(wb,"Bumphead_abundance",Bumphead.abundance.data)
writeData(wb,"Humphead_abundance",Humphead.abundance.data)
writeData(wb,"Coral_cover",Coral.cover.data)

saveWorkbook(wb,'x_Flat_data_files/2_Ecological/Outputs/MPA_Vision_Chp3/Chp3_eco_trends_plotting_data.xlsx')
