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
  

TrendDataAvailability <- 
  EcoTrends %>%
  group_by(MPAName_short) %>%
  summarise(HerbBiomassData=ifelse(length(Herb_Biomass[which(is.na(Herb_Biomass))])==length(MPAName_short),"No","Yes"),
            HerbAbundanceData=ifelse(length(Herb_Abundance[which(is.na(Herb_Abundance))])==length(MPAName_short),"No","Yes"),
            KeyBiomassData=ifelse(length(Key_Biomass[which(is.na(Key_Biomass))])==length(MPAName_short),"No","Yes"),
            KeyAbundanceData=ifelse(length(Key_Abundance[which(is.na(Key_Abundance))])==length(MPAName_short),"No","Yes"),
            BumpAbundanceData=ifelse(length(Bumphead_Abundance[which(is.na(Bumphead_Abundance))])==length(MPAName_short),"No","Yes"),
            HumpAbundanceData=ifelse(length(Humphead_Abundance[which(is.na(Humphead_Abundance))])==length(MPAName_short),"No","Yes"))


EcoTrends <-
  EcoTrends %>% select(-c(Province,MPA,MPAName_short,Year)) %>%
  mutate_all(as.numeric) %>%
  cbind.data.frame(.,EcoTrends[,c("Province","MPA","MPAName_short","Year")]) %>%
  mutate(Province=factor(Province,levels=c("Aceh","Central Java","Southeast Sulawesi","East Nusa Tenggara",
                                           "Maluku","West Papua","West Papua & Papua"),
                         ordered=T),
         MPAName_short=factor(MPAName_short,levels=c("Pesisir Timur Pulau Weh","Karimun Jawa","Wakatobi",
                                                     "Selat Pantar","Koon","Raja Ampat","Teluk Cendrawasih"),
                              ordered=T),
         MPA_label=factor(paste(MPAName_short,"\n","(",Province,")",sep=""),
                          levels=c("Pesisir Timur Pulau Weh\n(Aceh)","Karimun Jawa\n(Central Java)","Wakatobi\n(Southeast Sulawesi)",
                                   "Selat Pantar\n(East Nusa Tenggara)","Koon\n(Maluku)","Raja Ampat\n(West Papua)",
                                   "Teluk Cendrawasih\n(West Papua & Papua)"),
                          ordered=T),
         Year=factor(Year,levels=c("2005","2006","2007","2008","2009","2010","2011",
                                   "2012","2013","2014","2015","2016","2017","2018"),
                     ordered=T)) %>%
  left_join(TrendDataAvailability,by="MPAName_short")


EcoTrends.3pt <-
  EcoTrends.3pt %>% select(-c(Province,MPA)) %>%
  mutate_all(as.numeric) %>%
  cbind.data.frame(.,EcoTrends.3pt[,c("Province","MPA")]) %>%
  rbind.data.frame(.,data.frame(Province=c("East Nusa Tenggara","Maluku"),
                                MPA=c("Suaka Alam Perairan Selat Pantar Selat Pantar dan laut sekitarnya","Koon"),
                                Since.Est=NA,
                                Year=NA,
                                RepeatYear=c(3,3),
                                data.frame(matrix(rep(NA,35),
                                                  ncol=35,
                                                  dimnames=list(NULL,colnames(EcoTrends.3pt[6:40])))))) %>%
  mutate(Province=factor(Province,levels=c("Aceh","Central Java","Southeast Sulawesi","East Nusa Tenggara",
                                           "Maluku","West Papua","West Papua & Papua"),
                         ordered=T),
         RepeatYear=factor(RepeatYear,levels=c("1","2","3"),ordered=T))
                                           
                                           
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
# ---- SECTION 3: PROVINCIAL/MPA LEVEL ECO FIGURES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

  

# ---- BIOMASS ----

# ---- Herbivore biomass trend plot ----
Herb.biomass.plot <-
  ggplot(EcoTrends %>% filter(HerbBiomassData=="Yes"),
         aes(x=MPA_label,y=Herb_Biomass)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Herb_Biomass-Herb_Biomass_SE,
                    ymax=Herb_Biomass+Herb_Biomass_SE,
                    group=Year,alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1200),
                     breaks=c(0,250,500,750,1000)) +
  geom_vline(xintercept=1.5,linetype=2,size=0.3,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.3,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.3,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.3,colour="#505050") +
  plot.theme.trends + labs(x="MPA",y="Herbivore Biomass (kg/ha)\n")


Herb.biomass.plot.3pt <-
  ggplot(EcoTrends.3pt %>% filter(Province!="Aceh" & Province!="Central Java"),
         aes(x=Province,y=Herb_Biomass)) +
  geom_bar(aes(group=RepeatYear,alpha=RepeatYear),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Herb_Biomass-Herb_Biomass_SE,
                    ymax=Herb_Biomass+Herb_Biomass_SE,
                    group=RepeatYear,alpha=RepeatYear),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1000)) +
  scale_alpha_manual(name="",
                     labels=c("1"="First Year of Data","","Most Recent Year"),
                       values=c(0.3, 0.6, 0.9)) + 
  plot.theme.trends + labs(y="Herbivore Biomass (kg/ha)\n")


# ---- Key Fisheries biomass trend plot ----

Key.biomass.plot <-
  ggplot(EcoTrends %>% filter(KeyBiomassData=="Yes"),
         aes(x=MPA_label,y=Key_Biomass)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Key_Biomass-Key_Biomass_SE,
                    ymax=Key_Biomass+Key_Biomass_SE,
                    group=Year,alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.3,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.3,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.3,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.3,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1200),
                     breaks=c(0,250,500,750,1000)) +
  plot.theme.trends + labs(x="MPA",y="Key Fisheries Species\nBiomass (kg/ha)\n")



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
                     limits=c(0,7000),
                     breaks=c(0,1500,3000,4500,6000)) +
  labs(y="Herbivore Abundance\n(individuals/ha)\n") +
  plot.theme.trends


# ---- Herbivore abundance trend plot ----
Herb.abundance.plot <-
  ggplot(EcoTrends,
         aes(x=MPA_label,y=Herb_Abundance)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Herb_Abundance-Herb_Abundance_SE,
                    ymax=Herb_Abundance+Herb_Abundance_SE,
                    group=Year,alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                width=0.25,
                colour="#0A1D4E80",
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.4,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,7000),
                     breaks=c(0,1500,3000,4500,6000)) +
  plot.theme.trends + labs(x="MPA",y="Herbivore Abundance\n(individuals/ha)\n")


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
                     limits=c(0,4100),
                     breaks=c(0,1000,2000,3000,4000)) +
  labs(y="Key Fisheries Species\nAbundance (individuals/ha)\n") +
  plot.theme.trends


# ---- Key Fisheries abundance trend plot ----

Key.abundance.plot <-
  ggplot(EcoTrends,
         aes(x=MPA_label,y=Key_Abundance)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Key_Abundance-Key_Abundance_SE,
                    ymax=Key_Abundance+Key_Abundance_SE,
                    group=Year,
                    alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                colour="#0A1D4E80",
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.4,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,4100),
                     breaks=c(0,1000,2000,3000,4000)) +
  plot.theme.trends + labs(x="MPA",y="Key Fisheries Species\nAbundance (individuals/ha)\n")


# ---- Bumphead abundance trend plot ----

Bumphead.abundance.plot <-
  ggplot(EcoTrends,
         aes(x=MPA_label,y=Bumphead_Abundance)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Bumphead_Abundance-Bumphead_Abundance_SE,
                    ymax=Bumphead_Abundance+Bumphead_Abundance_SE,
                    group=Year,
                    alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                colour="#0A1D4E80",
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.4,colour="#505050") +
  geom_rect(aes(xmin=0.614, #aceh
                xmax=0.678,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.643,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=0.678,
                xmax=0.742,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.707,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=0.806,
                xmax=0.87,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.835,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=0.934,
                xmax=0.998,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.963,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.062,
                xmax=1.126,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=1.091,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.55, #central java
                xmax=1.614,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=1.579,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.614,
                xmax=1.678,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=1.643,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.678,
                xmax=1.742,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=1.707,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.998,
                xmax=2.062,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=2.027,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=2.062,
                xmax=2.126,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=2.091,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=2.254,
                xmax=2.318,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=2.283,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=4.126, #NTT
                xmax=4.19,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=4.155,y=0.5,label="Zero"), 
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=7.382,
                xmax=7.446,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=7.411,y=0.5,label="Zero"), #West Papua & Papua
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,13)) +
  plot.theme.trends + labs(x="MPA", y="Bumphead Parrotfish Species\nAbundance (individuals/ha)\n")


# ---- Humphead (Napoleon wrasse) abundance trend plot ----

Humphead.abundance.plot <-
  ggplot(EcoTrends,
         aes(x=MPA_label,y=Humphead_Abundance)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Humphead_Abundance-Humphead_Abundance_SE,
                    ymax=Humphead_Abundance+Humphead_Abundance_SE,
                    group=Year,
                    alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                colour="#0A1D4E80",
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.4,colour="#505050") +
  geom_rect(aes(xmin=0.684, #Aceh
                xmax=0.746,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.711,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=0.807,
                xmax=0.871,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.836,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=0.934,
                xmax=0.998,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=0.963,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.062,
                xmax=1.126,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=1.091,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=1.55, #central java
                xmax=1.614,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=1.579,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=2.254,
                xmax=2.318,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=2.283,y=0.5,label="Zero"),
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  geom_rect(aes(xmin=4.318, #NTT
                xmax=4.382,
                ymin=0.05,
                ymax=1),
            linetype=3,
            size=0.3,
            fill="white",
            colour="#212121",
            alpha=0.9) +
  # geom_text(aes(x=4.347,y=0.5,label="Zero"), 
  #           angle=90,
  #           size=1.5,
  #           colour="#212121") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,13)) +
  plot.theme.trends + labs(x="MPA", y="Humphead Napoleon Wrasse Species\nAbundance (individuals/ha)\n")


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
                     limits=c(0,60),
                     breaks=c(0,10,20,30,40,50,60)) +
  labs(y="Coral Cover (%)\n") +
  plot.theme.trends


# ---- Coral cover trend plot ----

Coral.cover.plot <-
  ggplot(EcoTrends,
         aes(x=MPA_label,y=Coral_Cover)) +
  geom_bar(aes(group=Year,alpha=Year),
           stat="identity",
           position="dodge",
           fill="#1B448BD9",
           colour="#505050",
           size=0.25) +
  geom_errorbar(aes(ymin=Coral_Cover-Coral_Cover_SE,
                    ymax=Coral_Cover+Coral_Cover_SE,
                    group=Year,
                    alpha=Year),
                stat="identity",
                position=position_dodge(width=0.9),
                colour="#0A1D4E80",
                width=0.25,
                show.legend=F) +
  geom_vline(xintercept=1.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=2.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=3.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=4.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=5.5,linetype=2,size=0.4,colour="#505050") +
  geom_vline(xintercept=6.5,linetype=2,size=0.4,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,61),
                     breaks=c(0,10,20,30,40,50,60)) +
  plot.theme.trends + labs(x="MPA", y="Coral Cover (%)\n")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: EXPORT FIGURES & DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- Biomass ----

png(paste(FigureFileName.chp3,"Herb.biomass.trend.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Herb.biomass.plot)
dev.off()

png(paste(FigureFileName.chp3,"Key.biomass.trend.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Key.biomass.plot)
dev.off()


# ---- Abundance ----

png(paste(FigureFileName.chp3,"Herb.abundance.status.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Herb.abundance.status)
dev.off()

png(paste(FigureFileName.chp3,"Herb.abundance.trend.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Herb.abundance.plot)
dev.off()

png(paste(FigureFileName.chp3,"Key.abundance.status.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Key.abundance.status)
dev.off()

png(paste(FigureFileName.chp3,"Key.abundance.trend.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Key.abundance.plot)
dev.off()

png(paste(FigureFileName.chp3,"Bumphead.abundance.trend.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(Bumphead.abundance.plot)
dev.off()

png(paste(FigureFileName.chp3,"Humphead.abundance.trend.png",sep="/"),
    units="in",height=6,width=8,res=400)
plot(Humphead.abundance.plot)
dev.off()


# ---- Coral cover ----

png(paste(FigureFileName.chp3,"Coral.cover.status.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Coral.cover.status)
dev.off()

png(paste(FigureFileName.chp3,"Coral.cover.trend.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(Coral.cover.plot)
dev.off()


# # - export data
# 
# wb <- createWorkbook("Chp_3_eco_trends_plotting_data") 
# addWorksheet(wb,"Herb_biomass")
# addWorksheet(wb,"Key_biomass")
# addWorksheet(wb,"Herb_abundance")
# addWorksheet(wb,"Key_abundance")
# addWorksheet(wb,"Bumphead_abundance")
# addWorksheet(wb,"Humphead_abundance")
# addWorksheet(wb,"Coral_cover")
# 
# 
# writeData(wb,"Herb_biomass",Herb.biomass.data)
# writeData(wb,"Key_biomass",Key.biomass.data)
# writeData(wb,"Herb_abundance",Herb.abundance.data)
# writeData(wb,"Key_abundance",Key.abundance.data)
# writeData(wb,"Bumphead_abundance",Bumphead.abundance.data)
# writeData(wb,"Humphead_abundance",Humphead.abundance.data)
# writeData(wb,"Coral_cover",Coral.cover.data)
# 
# saveWorkbook(wb,'x_Flat_data_files/2_Ecological/Outputs/MPA_Vision_Chp3/Chp3_eco_trends_plotting_data.xlsx')
