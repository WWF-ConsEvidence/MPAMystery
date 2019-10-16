# 
# code:  MPA Comprehensive Technical Report Plots
# 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2017
# modified: Amari Bauer, June 2019
# modified: Kelly Claborm, October 2019
# 
# 
# ---- inputs ----
#  1) Dependencies: Function_define_asteriskplotting.R
#  2) Source Alor.TechReport.Datasets.2019.R 
#     - Dependencies: Alor.TechReport.SigTests.2019.R
#                     SBS.TechReport.Calculations.R
# 
# ---- code sections ----
#  1) DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES
#  2) AGE/GENDER PLOT 
#  3) STATUS PLOTS
#  4) TREND PLOTS
#  5) ANNEX PLOTS
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Define significance labels and (x,y) coordinates for plots ----

Statusplot.asterisks <- 
  define.statusplot.asterisks(Sett.level.ContData.status.PLOTFORMAT %>% 
                                select(SettlementName, FS.pval, MA.pval, PA.pval, MT.pval, 
                                       SE.pval, TimeMarket.pval, Unwell.pval))

Statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Sett.level.ContData.status.PLOTFORMAT,
                                 Statusplot.asterisks)  

# ---- 1.3 Define Alor-specific plot labels, with significance asterisks ----

Trendplot.monitoryear.labs <- 
  define.year.monitoryear.column(Sett.level.ContData.annex.PLOTFORMAT)

Trendplot.monitoryear.labs <- rep(paste(Trendplot.monitoryear.labs[1],"\n\n\n\n",Trendplot.monitoryear.labs[2]),2)

Annexplot.monitoryear.labs <- rev(define.year.monitoryear.column(Sett.level.ContData.annex.PLOTFORMAT))

Conttrendplot.ylabs <- 
  define.conttrendplot.ylabels.withasterisks(MPA.level.ContData.trend.PLOTFORMAT
                                             [MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear=="p.value",
                                               c("FSMean","MAMean","PAMean","MTMean",
                                                 "SEMean","TimeMarketMean","UnwellMean")])

Proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test)


Trendplot.labs <- list(FS=labs(y=as.character(Conttrendplot.ylabs["FSMean"]),x="Monitoring Year"),
                       MA=labs(y=as.character(Conttrendplot.ylabs["MAMean"]),x="Monitoring Year"),
                       PA=labs(y=as.character(Conttrendplot.ylabs["PAMean"]),x="Monitoring Year"),
                       SE=labs(y=as.character(Conttrendplot.ylabs["SEMean"]),x="Monitoring Year"),
                       Unwell=labs(y=as.character(Conttrendplot.ylabs["UnwellMean"]),
                                   x="Monitoring Year"),
                       Gender=labs(y="Gender (% head of household)",x="Monitoring Year"),
                       Religion=labs(y="Religion (% households)",x="Monitoring Year"),
                       PrimaryOcc=labs(y=as.character(Proptrendplot.ylabs["Primary occupation (% households)"]),x="Monitoring Year"),
                       FreqFish=labs(y=as.character(Proptrendplot.ylabs["Frequency of fishing (% households)"]),x="Monitoring Year"),
                       FreqSellFish=labs(y=as.character(Proptrendplot.ylabs["Frequency of selling at least some catch (% households)"]),x="Monitoring Year"),
                       IncFish=labs(y=as.character(Proptrendplot.ylabs["Income from fishing in past 6 months (% households)"]),x="Monitoring Year"),
                       FishTech=labs(y=as.character(Proptrendplot.ylabs["Fishing technique most often used in past 6 months (% households)"]),x="Monitoring Year"),
                       ChildFS=labs(y=as.character(Proptrendplot.ylabs["Child hunger (% households)"]),x="Monitoring Year"),
                       Protein=labs(y=as.character(Proptrendplot.ylabs["Dietary protein from fish in past 6 months (% households)"]),x="Monitoring Year"),
                       EconStatus=labs(y=as.character(Proptrendplot.ylabs["Change in economic status of fishing households (% households)"])),
                       NumLocalThreats=labs(y=as.character(Proptrendplot.ylabs["Number of identified local threats to marine environment (% households)"])),
                       SecondaryOcc=labs(y=as.character(Proptrendplot.ylabs["Secondary occupation (% households)"])),
                       OccDiverse=labs(y=as.character(Proptrendplot.ylabs["Occupational diversity (% households)"])))

Annexplot.settnames <- 
  define.annexplot.settname.labels(annex.sigvals)

Annexplot.settnames[3,] <- rep("",length(Annexplot.settnames[3,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Repeat One ----

Age.gender.RepeatOne <- 
  melt(AgeGender,id.vars="AgeCat",measure.vars=c("Female.RepeatOne","Male.RepeatOne")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Trendplot.monitoryear.labs[1]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=c("Female","Male"),
                    values=c("Female.RepeatOne"=alpha("#7FCDBB",0.95),
                             "Male.RepeatOne"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="")


# ---- 2.2 Baseline ----

Age.gender.Baseline <- 
  melt(AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Trendplot.monitoryear.labs[2]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=c("Female","Male"),
                    values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="Population distribution (% of individuals by gender)")


# ---- 2.3 Create legend ----

Age.gender.legend.plot <-
  melt(AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     name="",
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95)),
                    labels=c("Female","Male")) +
  coord_flip() + plot.guides.techreport + theme(legend.justification="right")

Age.gender.legend <- g_legend(Age.gender.legend.plot)


# ---- 2.4 Arrange grob ----

Age.gender.plot <- 
  grid.arrange(Age.gender.legend,
               arrangeGrob(
                 Age.gender.RepeatOne,
                 Age.gender.Baseline,ncol=1),nrow=2,heights=c(0.35,10))
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
FS.statusplot <- 
  rbind.data.frame(Sett.level.ContData.status.PLOTFORMAT[,c("SettlementName","FSMean","FSErr","SettLevel")],
                   data.frame(SettlementName="  ",
                              FSMean=NA,
                              FSErr=NA,
                              SettLevel="Dummy")) %>%
  ggplot(aes(x=SettlementName)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(y=FSMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=FS),
            label=Statusplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=FS.ref),
            label=Statusplot.asterisks$FS.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  geom_text(aes(x=length(SettlementName),y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(SettlementName),y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(SettlementName),y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["FS"] + theme(axis.ticks=element_blank(),
                                               panel.background=element_rect(fill="white",
                                                                             colour="#909090"),
                                               panel.border=element_rect(fill=NA,
                                                                         size=0.25,
                                                                         colour="#C0C0C0"),
                                               panel.grid.major.x=element_blank(),
                                               panel.grid.major.y=element_blank(),
                                               axis.title=element_text(size=10,
                                                                       angle=0,
                                                                       face="bold",
                                                                       colour="#303030"),
                                               axis.text=element_text(size=8,
                                                                      angle=0,
                                                                      colour="#303030"))


# - MATERIAL ASSETS
MA.statusplot <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                             aes(x=SettlementName)) +
  geom_bar(aes(y=MAMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") + 
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=MA),
            label=Statusplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=MA.ref),
            label=Statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Sett.level.ContData.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MA"] + plot.theme


# - PLACE ATTACHMENT
PA.statusplot <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                             aes(x=SettlementName)) +
  geom_bar(aes(y=PAMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=PA),
            label=Statusplot.asterisks$PA,
            nudge_x=-0.07,
            nudge_y=0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=PA.ref),
            label=Statusplot.asterisks$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["PA"] + plot.theme


# - MARINE TENURE
MT.statusplot <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                             aes(x=SettlementName)) +
  geom_bar(aes(y=MTMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=MT+(0.05*MT)),
            label=Statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=MT.ref),
            label=Statusplot.asterisks$MT.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
SE.statusplot <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                             aes(x=SettlementName)) +
  geom_bar(aes(y=SEMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=SE),
            label=Statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=SE.ref),
            label=Statusplot.asterisks$SE.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1.1)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["SE"] + plot.theme


# - TIME TO MARKET
Time.statusplot <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                               aes(x=SettlementName)) +
  geom_bar(aes(y=TimeMarketMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=TimeMarketMean-TimeMarketErr,
                    ymax=TimeMarketMean+TimeMarketErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=TimeMarket),
            label=Statusplot.asterisks$TimeMarket,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=TimeMarket.ref),
            label=Statusplot.asterisks$TimeMarket.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Sett.level.ContData.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Time"] + plot.theme


# - DAYS UNWELL
Unwell.statusplot <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                                 aes(x=SettlementName)) +
  geom_bar(aes(y=UnwellMean,
               fill=SettLevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
                    ymax=UnwellMean+UnwellErr,
                    colour=SettLevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,
                y=Unwell),
            label=Statusplot.asterisks$Unwell,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos,
            aes(x=SettlementName,y=Unwell.ref),
            label=Statusplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.status.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Sett.level.ContData.status.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.theme


# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Gender.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=SettlementName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + plot.theme + Statusplot.labs["Gender"] + plot.guides.techreport

# - RELIGION
Religion.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=SettlementName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + Statusplot.labs["Religion"] +
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           ncol=3,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

# - PRIMARY OCCUPATION
Primaryocc.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.PrimaryOcc.Other",  
                                               "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
                                               "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.HarvestForest", 
                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other", "Aquaculture", "Tourism", "Extraction of non-renewable marine resources",  "Other Wage Labor",
                             "Harvest Forest Products","Fishing", "Farming")) +
  coord_flip() + plot.theme + Statusplot.labs["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
Freqfish.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Statusplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Freqsellfish.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Statusplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Incfish.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Statusplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Fishtech.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + Statusplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Childfs.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=SettlementName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + Statusplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
Proteinfish.statusplot <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Statusplot.labs["FishProtein"] + plot.guides.techreport

# - CATEGORICAL FOOD SECURITY
FSCategorical.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.FoodInsecure.YesHunger", "Percent.FoodInsecure.NoHunger", "Percent.FoodSecure")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FSCategorical"]],
                    labels=c("Food insecure with hunger", "Food insecure without hunger","Food secure" )) +
  coord_flip() + plot.theme + Statusplot.labs["FSCategorical"] + plot.guides.techreport

# - ECONOMIC STATUS
EconStatus.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Econ.Status.Much.Better","Econ.Status.Slightly.Better",
                                               "Econ.Status.Neutral","Econ.Status.Slighly.Worse",
                                               "Econ.Status.Much.Worse")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["EconStatus"]],
                    labels=c("Much better","Slightly better","Neither better nor worse","Slightly worse","Much worse")) +
  coord_flip() + plot.theme + Statusplot.labs["EconStatus"] + plot.guides.techreport

# - MEMBER OF MARINE RESOURCE ORGANIZATION
MarineMember.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("MarineMember.No","MarineMember.Yes")) %>%
  ggplot(aes(x=SettlementName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["MarineMember"]],
                    labels=c("Non-member","Member")) +
  coord_flip() + plot.theme + Statusplot.labs["MarineMember"] + plot.guides.techreport

# - MEETING ATTENDANCE
MarineMeeting.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("MarineMeeting.No", "MarineMeeting.Yes")) %>%
  ggplot(aes(x=SettlementName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["MarineAttendance"]],
                    labels=c("Have not attended a meeting","Attended a meeting")) +
  coord_flip() + plot.theme + Statusplot.labs["MarineAttendance"] + plot.guides.techreport

# - MARINE RESOUCE CONFLICT
SocialConflict.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.GreatlyDecreased.SocConflict","Percent.Decreased.SocConflict",
                                               "Percent.Same.SocConflict","Percent.Increased.SocConflict",
                                               "Percent.GreatlyIncreased.SocConflict")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["SocialConflict"]],
                    labels=c("Greatly decreased","Decreased","Neither increased nor decreased","Increased","Greatly Increased")) +
  coord_flip() + plot.theme + Statusplot.labs["SocialConflict"] + plot.guides.techreport

# - NUMBER OF LOCAL THREATS
NumThreat.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Threat.Minimum.Five","Threat.Four", "Threat.Three",
                                               "Threat.Two","Threat.One","Threat.None")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["NumLocalThreats"]],
                    labels=c("More than four threats","Four threats","Three threats","Two threats","One threat", "No threats")) +
  coord_flip() + plot.theme + Statusplot.labs["NumLocalThreats"] + plot.guides.techreport

# - MARINE GROUP CONTRIBUTION
MarineContribution.statusplot <- 
  ggplot(data=Sett.level.PropData.status.PLOTFORMAT,
         aes(x=SettlementName)) +
  geom_bar(aes(y=MarineContribution,
               fill="NotDummy"),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.PropData.status.PLOTFORMAT$Contribution,na.rm=T) +
                                1.5* max(Sett.level.PropData.status.PLOTFORMAT$Contribution,na.rm=T)), labels = scales::comma) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MarineContribution"] + plot.theme


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TREND PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 4.1 Continuous data plots ----

# - FOOD SECURITY 
FS.trendplot <- 
  ggplot(MPA.level.ContData.trend.PLOTFORMAT[MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear!="p.value",],
         aes(x=SettlementName)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(y=FSMean,
               group=MonitoringYear,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    group=MonitoringYear,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_text(aes(x=length(unique(MonitoringYear))+0.46,y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(MonitoringYear))+0.46,y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(MonitoringYear))+0.46,y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_vline(aes(xintercept=1.5),size=0.25,colour="#505050") +
  geom_text(aes(x=1.6,y=max(FSMean)+1.5,label="Treatment",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=1.47,y=max(FSMean)+1.5,label="Control",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  coord_flip() + Trendplot.labs["FS"] + theme(axis.ticks=element_blank(),
                                                   panel.background=element_rect(fill="white",
                                                                                 colour="#909090"),
                                                   panel.border=element_rect(fill=NA,
                                                                             size=0.25,
                                                                             colour="#C0C0C0"),
                                                   panel.grid.major.x=element_blank(),
                                                   panel.grid.major.y=element_blank(),
                                                   axis.title=element_text(size=10,
                                                                           angle=0,
                                                                           face="bold",
                                                                           colour="#303030"),
                                                   axis.text=element_text(size=8,
                                                                          angle=0,
                                                                          colour="#303030"))

# - MATERIAL ASSETS
MA.trendplot <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT
         [!is.na(MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear),],
         aes(x=MonitoringYear)) +
  geom_bar(aes(y=MAMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.ContData.trend.PLOTFORMAT$MAMean,na.rm=T)+
                                max(MPA.level.ContData.trend.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(MPA.level.ContData.trend.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  coord_flip() + Trendplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
PA.trendplot <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT
         [!is.na(MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear),],
         aes(x=MonitoringYear)) +
  geom_bar(aes(y=PAMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  coord_flip() + Trendplot.labs["PA"] + plot.theme

# - MARINE TENURE
MT.trendplot <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT
         [!is.na(MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear),],
         aes(x=MonitoringYear)) +
  geom_bar(aes(y=MTMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  coord_flip() + Trendplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
SE.trendplot <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT
         [!is.na(MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear),],
         aes(x=MonitoringYear)) +
  geom_bar(aes(y=SEMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1)) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  coord_flip() + Trendplot.labs["SE"] + plot.theme

# - TIME TO MARKET
Time.trendplot <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT
         [!is.na(MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear),],
         aes(x=MonitoringYear)) +
  geom_bar(aes(y=TimeMarketMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=TimeMarketMean-TimeMarketErr,
                    ymax=TimeMarketMean+TimeMarketErr),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.ContData.trend.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(MPA.level.ContData.trend.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(MPA.level.ContData.trend.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  coord_flip() + Trendplot.labs["Time"] + plot.theme

# - DAYS UNWELL
Unwell.trendplot <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT
         [!is.na(MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear),],
         aes(x=MonitoringYear)) +
  geom_bar(aes(y=UnwellMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
                    ymax=UnwellMean+UnwellErr),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.ContData.trend.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(MPA.level.ContData.trend.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(MPA.level.ContData.trend.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  coord_flip() + Trendplot.labs["Unwell"] + plot.theme


# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Gender.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + Trendplot.labs["Gender"] + plot.theme + plot.guides.techreport

# - RELIGION
Religion.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=rev(MonitoringYear),
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + Trendplot.labs["Religion"] + 
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           ncol=3,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

# - PRIMARY OCCUPATION
Primaryocc.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Percent.PrimaryOcc.Other","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism","Percent.PrimaryOcc.Fish",
                                               "Percent.PrimaryOcc.HarvestForest","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + Trendplot.labs["PrimaryOcc"] + plot.guides.techreport 

# - FISHING FREQUENCY
Freqfish.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Trendplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Freqsellfish.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Trendplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Incfish.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Trendplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Fishtech.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + Trendplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Childfs.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=rev(MonitoringYear),
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + Trendplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
Proteinfish.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Trendplot.labs["Protein"] + plot.guides.techreport

# - ECONOMIC STATUS 
EconStatus.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Econ.Status.Much.Better","Econ.Status.Slightly.Better",
                                               "Econ.Status.Neutral","Econ.Status.Slighly.Worse",
                                               "Econ.Status.Much.Worse")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["EconStatus"]],
                    labels=c("Much better","Slightly better","Neither better nor worse","Slightly worse","Much worse")) +
  coord_flip() + plot.theme + Trendplot.labs["EconStatus"] + plot.guides.techreport

# - NUMBER OF LOCAL THREATS
NumThreat.trendplot <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Threat.Minimum.Five","Threat.Four", "Threat.Three",
                                               "Threat.Two","Threat.One","Threat.None")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["NumLocalThreats"]],
                    labels=c("More than four threats","Four threats","Three threats","Two threats","One threat", "No threats")) +
  coord_flip() + plot.theme + Trendplot.labs["Protein"] + plot.guides.techreport

# - SECONDARY OCCUPATION

# - OCCUPATIONAL DIVERSITY
OccDiverse.trendplot <-
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars=c("MonitoringYear"),measure.vars=c("Percent.MultipleOcc.Diverse","Percent.OneOcc.Diverse")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),size=0.25,colour="#505050") +
  geom_text(aes(x=3.25,y=.91,label="Treatment",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=2.95,y=.91,label="Control",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=trendplot.x.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["OccDiverse"]],
                    labels=c("Multiple Occupations","One Occupation")) +
  coord_flip() + plot.theme + labs(y="Occupational diversity (% households)", x="Monitoring Year",title="") + plot.guides.techreport 


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

FS.annexplot <- 
  rbind.data.frame(Sett.level.ContData.annex.PLOTFORMAT[,c("MonitoringYear","SettlementName","FSMean","FSErr","SettLevel")],
                   data.frame(MonitoringYear=NA,
                              SettlementName="  ",
                              FSMean=NA,
                              FSErr=NA,
                              SettLevel="Dummy")) %>%
  ggplot(aes(x=SettlementName)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(x=SettlementName,
               y=FSMean,
               alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(x=SettlementName,
                    ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(aes(x=length(unique(SettlementName)),y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(SettlementName)),y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(SettlementName)),y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=(Trendplot.monitoryear.labs),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(Annexplot.settnames[,"FS"]," "),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  coord_flip() + Statusplot.labs["FS"] + plot.guides.techreport + theme(axis.ticks=element_blank(),
                                                                        panel.background=element_rect(fill="white",
                                                                                                      colour="#909090"),
                                                                        panel.border=element_rect(fill=NA,
                                                                                                  size=0.25,
                                                                                                  colour="#C0C0C0"),
                                                                        panel.grid.major.x=element_blank(),
                                                                        panel.grid.major.y=element_blank(),
                                                                        axis.title=element_text(size=10,
                                                                                                angle=0,
                                                                                                face="bold",
                                                                                                colour="#303030"),
                                                                        axis.text=element_text(size=8,
                                                                                               angle=0,
                                                                                               colour="#303030"),
                                                                        legend.position="top",
                                                                        legend.justification="right",
                                                                        legend.box.spacing=unit(0.1,"cm"))


# ---- 5.2 Material assets -----

MA.annexplot <- 
  ggplot(data=Sett.level.ContData.annex.PLOTFORMAT,
         aes(x=SettlementName,
             y=MAMean)) +
  geom_bar(aes(alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.annex.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Sett.level.ContData.annex.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.annex.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["MA"] + plot.guides.techreport + plot.theme


# ---- 5.3 Place attachment -----

PA.annexplot <- 
  ggplot(data=Sett.level.ContData.annex.PLOTFORMAT,
         aes(x=SettlementName,
             y=PAMean)) +
  geom_bar(aes(alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2.55,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["PA"] + plot.guides.techreport + plot.theme


# ---- 5.4 Marine tenure -----

MT.annexplot <- 
  ggplot(data=Sett.level.ContData.annex.PLOTFORMAT,
         aes(x=SettlementName,
             y=MTMean)) +
  geom_bar(aes(alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["MT"] + plot.guides.techreport + plot.theme


# ---- 5.5 School enrollment -----

SE.annexplot <- 
  ggplot(data=Sett.level.ContData.annex.PLOTFORMAT,
         aes(x=SettlementName,
             y=SEMean)) +
  geom_bar(aes(alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + Statusplot.labs["SE"] + plot.guides.techreport + plot.theme


# ---- 5.6 Time to market -----

Time.annexplot <- 
  ggplot(data=Sett.level.ContData.annex.PLOTFORMAT,
         aes(x=SettlementName,
             y=TimeMarketMean)) +
  geom_bar(aes(alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(ymin=TimeMarketMean-TimeMarketErr,
                    ymax=TimeMarketMean+TimeMarketErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames[,"TimeMarket"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.annex.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Sett.level.ContData.annex.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.annex.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Time"] + plot.guides.techreport + plot.theme


# ---- 5.7 Days unwell -----

Unwell.annexplot <- 
  ggplot(data=Sett.level.ContData.annex.PLOTFORMAT,
         aes(x=SettlementName,
             y=UnwellMean)) +
  geom_bar(aes(alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
                    ymax=UnwellMean+UnwellErr,
                    colour=SettLevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.annex.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Sett.level.ContData.annex.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.annex.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.guides.techreport + plot.theme


