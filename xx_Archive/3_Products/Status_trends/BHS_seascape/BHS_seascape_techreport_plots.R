# 
# code:  BHS Technical Report Plots
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/BHS/Plots
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2017
# modified: 
# 
# 
# ---- inputs ----
#  1) Dependencies: Function_define_asteriskplotting.R
#  2) Source BHS.TechReport.Datasets.R 
#     - Dependencies: BHS.TechReport.SigTests.R
#                     BHS_MPA_Mystery.R
# 
# ---- code sections ----
#  1) DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES
#  2) AGE/GENDER PLOT 
#  3) STATUS PLOTS
#  4) TREND PLOTS
#  5) ANNEX PLOTS
#  6) WRITE TO .PNG
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Source data sets from "BHS.TechReport.Datasets.R" ----

source("4_Products/1_Social/1_status_trends_reports/BHS_seascape/BHS.Seascape.TechReport.Datasets.R")
source('2_Functions/3_Plotting/Function_define_asteriskplotting.R')

# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

BHS.statusplot.asterisks <- 
  define.seascape.statusplot.asterisks(BHS.ContData.Techreport.status.PLOTFORMAT[,c("MPAName","FS.pval",
                                                                                    "MA.pval","PA.pval",
                                                                                    "MT.pval","SE.pval",
                                                                                    "Time.pval","Unwell.pval")])
BHS.statusplot.sigpos <- 
  define.seascape.statusplot.asterisk.pos(BHS.ContData.Techreport.status.PLOTFORMAT,
                                          BHS.statusplot.asterisks)  


BHS.baselineplot.asterisks <- 
  define.seascape.baselineplot.asterisks(BHS.ContData.Techreport.baseline.PLOTFORMAT[,c("MPAName","FS.pval",
                                                                                    "MA.pval","PA.pval",
                                                                                    "MT.pval","SE.pval","Unwell.pval")])
BHS.baselineplot.sigpos <- 
  define.seascape.baselineplot.asterisk.pos(BHS.ContData.Techreport.baseline.PLOTFORMAT,
                                          BHS.baselineplot.asterisks)  


# ---- 1.3 Define BHS-specific plot labels, with significance asterisks ----

BHSlevel.statusplot.labs <- list(FS=labs(y="Mean household food security",x="MPA"),
                                 MA=labs(y="Mean household assets",x="MPA"),
                                 PA=labs(y="Mean place attachment",x="MPA"),
                                 MT=labs(y="Mean household marine tenure",x="MPA"),
                                 SE=labs(y="School enrollment (% children ages 5-18 years old)",x="MPA"),
                                 Time=labs(y="Mean travel time to closest market (hours)",x="MPA"),
                                 Unwell=labs(y="Mean time suffering from illness or injury in past 4 weeks (days)",
                                             x="MPA"),
                                 Ethnicity=labs(y="Number of unique ethnic groups",x="MPA"),
                                 Gender=labs(y="Gender (% head of household)",x="MPA"),
                                 Religion=labs(y="Religion (% head of household)",x="MPA"),
                                 PrimaryOcc=labs(y="Primary occupation (% households)",x="MPA"),
                                 FreqFish=labs(y="Frequency of fishing (% households)",x="MPA"),
                                 FreqSellFish=labs(y="Frequency of selling at least some catch (% households)",
                                                   x="MPA"),
                                 IncFish=labs(y="Income from fishing in past 6 months (% households)",
                                              x="MPA"),
                                 FishTech=labs(y="Fishing technique most often used in past 6 months (% households)",
                                               x="MPA"),
                                 ChildFS=labs(y="Child hunger (% households)",x="MPA"),
                                 Protein=labs(y="Dietary protein from fish in past 6 months (% households)",
                                              x="MPA"))

BHS.conttrendplot.ylabs <- 
  define.conttrendplot.ylabels.withasterisks(BHS.TrendContData.Techreport.PLOTFORMAT
                                             [is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),
                                               c("FSMean","MAMean","PAMean","MTMean",
                                                 "SEMean","TimeMarketMean","UnwellMean")])

BHS.proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test.BHS)


BHS.trendplot.labs <- list(FS=labs(y=as.character(BHS.conttrendplot.ylabs["FSMean"]),x="Monitoring Year"),
                            MA=labs(y=as.character(BHS.conttrendplot.ylabs["MAMean"]),x="Monitoring Year"),
                            PA=labs(y=as.character(BHS.conttrendplot.ylabs["PAMean"]),x="Monitoring Year"),
                            MT=labs(y=as.character(BHS.conttrendplot.ylabs["MTMean"]),x="Monitoring Year"),
                            SE=labs(y=as.character(BHS.conttrendplot.ylabs["SEMean"]),x="Monitoring Year"),
                            Time=labs(y=as.character(BHS.conttrendplot.ylabs["TimeMarketMean"]),
                                      x="Monitoring Year"),
                            Unwell=labs(y=as.character(BHS.conttrendplot.ylabs["UnwellMean"]),
                                        x="Monitoring Year"))

                            # Gender=labs(y="Gender (% head of household)",x="Monitoring Year"),
                            # Religion=labs(y="Religion (% households)",x="Monitoring Year"),
                            # PrimaryOcc=labs(y=as.character(BHS.proptrendplot.ylabs["PrimaryOcc"]),x="Monitoring Year"),
                            # FreqFish=labs(y=as.character(BHS.proptrendplot.ylabs["FreqFish"]),x="Monitoring Year"),
                            # FreqSellFish=labs(y=as.character(BHS.proptrendplot.ylabs["SellFish"]),x="Monitoring Year"),
                            # IncFish=labs(y=as.character(BHS.proptrendplot.ylabs["IncFish"]),x="Monitoring Year"),
                            # FishTech=labs(y=as.character(BHS.proptrendplot.ylabs["FishTech"]),x="Monitoring Year"),
                            # ChildFS=labs(y=as.character(BHS.proptrendplot.ylabs["ChildFS"]),x="Monitoring Year"),
                            # Protein=labs(y=as.character(BHS.proptrendplot.ylabs["Protein"]),x="Monitoring Year"))

BHS.annexplot.MPAnames <- 
  define.annexplot.MPAname.labels(annex.sigvals.BHS)

BHS.annexplot.MPAnames[2,] <- rep(" ",length(BHS.annexplot.MPAnames[2,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Baseline ----

BHS.age.gender.baseline <- 
  melt(BHS.AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label="Baseline"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="Population distribution (% of individuals by gender)")


# ---- 2.2 Two Year Post Baseline ----

BHS.age.gender.2yr <- 
  melt(BHS.AgeGender,id.vars="AgeCat",measure.vars=c("Female.2yr","Male.2yr")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label="2 Year Post"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     name="",
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(values=c("Female.2yr"=alpha("#7FCDBB",0.95),
                             "Male.2yr"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age")


# ---- 2.3 Four Year Post Baseline ----

BHS.age.gender.4yr <- 
  melt(BHS.AgeGender,id.vars="AgeCat",measure.vars=c("Female.4yr","Male.4yr")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label="4 Year Post"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     name="",
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    values=c("Female.4yr"=alpha("#7FCDBB",0.95),
                             "Male.4yr"=alpha("#253494",0.95)),
                    labels=c("Female","Male")) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age")


# ---- 2.4 Arrange grob ----

BHS.agegender.legend.plot <-
  melt(BHS.AgeGender,id.vars="AgeCat",measure.vars=c("Female.4yr","Male.4yr")) %>%
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
                    values=c("Female.4yr"=alpha("#7FCDBB",0.95),
                             "Male.4yr"=alpha("#253494",0.95)),
                    labels=c("Female","Male")) +
  coord_flip() + plot.guides.techreport + theme(legend.justification="right")

BHS.agegender.legend <- g_legend(BHS.agegender.legend.plot)


BHS.age.gender.plot <- 
  grid.arrange(BHS.agegender.legend,
               arrangeGrob(
                 BHS.age.gender.4yr,
                 BHS.age.gender.2yr,
                 BHS.age.gender.baseline,ncol=1),nrow=2,heights=c(0.35,10))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
BHS.fs.statusplot <- 
  rbind.data.frame(BHS.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(MPAID=NA,MPAName="  ",
                                    matrix(rep(NA,21),ncol=21,
                                           dimnames=list(NULL,
                                                         colnames(BHS.ContData.Techreport.status.PLOTFORMAT)[3:23])),
                                    MPALevel="Dummy")) %>%
  ggplot(aes(x=MPAName)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(y=FSMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=FS),
            label=BHS.statusplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=FS.ref),
            label=BHS.statusplot.asterisks$FS.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  geom_text(aes(x=length(MPAName),y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MPAName),y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MPAName),y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
BHS.ma.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                             aes(x=MPAName)) +
  geom_bar(aes(y=MAMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=MA),
            label=BHS.statusplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=MA.ref),
            label=BHS.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(BHS.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(BHS.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
BHS.pa.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                             aes(x=MPAName)) +
  geom_bar(aes(y=PAMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=PA),
            label=BHS.statusplot.asterisks$PA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=PA.ref),
            label=BHS.statusplot.asterisks$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["PA"] + plot.theme

# - MARINE TENURE
BHS.mt.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                             aes(x=MPAName)) +
  geom_bar(aes(y=MTMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=MT+(0.05*MT)),
            label=BHS.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=MT.ref),
            label=BHS.statusplot.asterisks$MT.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
BHS.se.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                             aes(x=MPAName)) +
  geom_bar(aes(y=SEMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=SE),
            label=BHS.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=SE.ref),
            label=BHS.statusplot.asterisks$SE.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1.1)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["SE"] + plot.theme

# - TIME TO MARKET
BHS.time.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                               aes(x=MPAName)) +
  geom_bar(aes(y=TimeMarketMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=TimeMarketMean-TimeMarketErr,
                    ymax=TimeMarketMean+TimeMarketErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=Time),
            label=BHS.statusplot.asterisks$Time,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=Time.ref),
            label=BHS.statusplot.asterisks$Time.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(BHS.ContData.Techreport.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(BHS.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["Time"] + plot.theme

# - DAYS UNWELL
BHS.unwell.statusplot <- ggplot(data=BHS.ContData.Techreport.status.PLOTFORMAT,
                                 aes(x=MPAName)) +
  geom_bar(aes(y=UnwellMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
                    ymax=UnwellMean+UnwellErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,
                y=Unwell),
            label=BHS.statusplot.asterisks$Unwell,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.statusplot.sigpos,
            aes(x=MPAName,y=Unwell.ref),
            label=BHS.statusplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(BHS.ContData.Techreport.status.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(BHS.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["Unwell"] + plot.theme

# - NUMBER UNIQUE ETHNIC GROUPS
BHS.ethnic.statusplot <- ggplot(data=BHS.PropData.Techreport.status.PLOTFORMAT,
                                 aes(x=MPAName)) +
  geom_bar(aes(y=Num.EthnicGroups,
               fill=Dummy),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.PropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T)+
                                0.03*max(BHS.PropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["Ethnicity"] + plot.theme

# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
BHS.gender.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=MPAName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["Gender"] + plot.guides.techreport

# - RELIGION
BHS.religion.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=MPAName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["Religion"] +
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
BHS.primaryocc.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Percent.PrimaryOcc.Other","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism","Percent.PrimaryOcc.Fish",
                                               "Percent.PrimaryOcc.HarvestForest","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=MPAName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
BHS.freqfish.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=MPAName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
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
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
BHS.freqsellfish.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=MPAName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
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
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
BHS.incfish.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=MPAName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
BHS.fishtech.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=MPAName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
BHS.childfs.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=MPAName,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
BHS.proteinfish.statusplot <- 
  melt(BHS.PropData.Techreport.status.PLOTFORMAT,
       id.vars="MPAName",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=MPAName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + BHSlevel.statusplot.labs["Protein"] + plot.guides.techreport

# # - SPREAD OF ETHNIC GROUPS
# BHS.ethnicspread.statusplot <-
#   left_join(cbind.data.frame(melt(BHS.PropData.Techreport.status.PLOTFORMAT,
#        id.vars="MPAName",measure.vars=c("Prop.Majority.Ethnic4","Prop.Majority.Ethnic3",
#                                                "Prop.Majority.Ethnic2","Prop.Majority.Ethnic1",
#                                                "Prop.OtherEthnic"),
#        variable.name="Prop.Ethnic",value.name="Prop"),RandomID=1:(length(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName)*5)),
#        cbind.data.frame(melt(BHS.PropData.Techreport.status.PLOTFORMAT,
#             id.vars="MPAName",measure.vars=c("Majority.Ethnic4","Majority.Ethnic3",
#                                                     "Majority.Ethnic2","Majority.Ethnic1"),
#        variable.name="Ethnic.Group",value.name="Ethnicity"),
#        RandomID=1:(length(BHS.PropData.Techreport.status.PLOTFORMAT$MPAName)*4))) %>%
#   ggplot(aes(x=MPAName,y=Prop,fill=Ethnicity)) +
#   geom_bar(stat="identity",
#            position="fill",
#            width=0.75,
#            size=0.15,
#            colour="#505050") +
#   geom_vline(aes(xintercept=2),
#              linetype=2,
#              size=0.35,
#              colour="#505050") +
#   scale_y_continuous(expand=c(0,0),
#                      labels=scales::percent_format()) +
#   # scale_fill_manual(name="",
#   #                   values=multianswer.fillcols.status[["Protein"]],
#   #                   labels=c("All","Most","About half","Some","None")) +
#   coord_flip() + plot.theme  + plot.guides.techreport



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TREND PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 4.1 Continuous data plots ----

# - FOOD SECURITY 
BHS.fs.trendplot <- 
  ggplot(BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),]) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(x=MonitoringYear,
               y=FSMean),
           fill=fillcols.trend,
           stat="identity",
           position="dodge",
           width=0.65) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    x=MonitoringYear),
                colour=errcols.trend,
                width=0.15,
                size=0.5,
                position=position_dodge(width=1)) +
  geom_text(aes(x=length(MonitoringYear)+0.46,y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MonitoringYear)+0.46,y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MonitoringYear)+0.46,y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
BHS.ma.trendplot <- 
  ggplot(data=BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(BHS.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(BHS.TrendContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(BHS.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
BHS.pa.trendplot <- 
  ggplot(data=BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["PA"] + plot.theme

# - MARINE TENURE
BHS.mt.trendplot <- 
  ggplot(data=BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
BHS.se.trendplot <- 
  ggplot(data=BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["SE"] + plot.theme

# - TIME TO MARKET
BHS.time.trendplot <- 
  ggplot(data=BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(BHS.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(BHS.TrendContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(BHS.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["Time"] + plot.theme

# - DAYS UNWELL
BHS.unwell.trendplot <- 
  ggplot(data=BHS.TrendContData.Techreport.PLOTFORMAT
         [!is.na(BHS.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(BHS.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(BHS.TrendContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(BHS.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  coord_flip() + BHS.trendplot.labs["Unwell"] + plot.theme


# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
BHS.gender.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + BHS.trendplot.labs["Gender"] + plot.theme + plot.guides.techreport

# - RELIGION
BHS.religion.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=MonitoringYear,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["Religion"] + 
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
BHS.primaryocc.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Percent.PrimaryOcc.Other","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism","Percent.PrimaryOcc.Fish",
                                               "Percent.PrimaryOcc.HarvestForest","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
BHS.freqfish.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
BHS.freqsellfish.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
BHS.incfish.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
BHS.fishtech.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
BHS.childfs.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=MonitoringYear,
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
BHS.proteinfish.trendplot <- 
  melt(BHS.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=c("Baseline", "2 Year Post", "4 Year Post")) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + BHS.trendplot.labs["Protein"] + plot.guides.techreport


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

BHS.fs.annexplot <- 
  rbind.data.frame(BHS.AnnexContData.Techreport.PLOTFORMAT,
                   cbind.data.frame(matrix(rep(NA,17),ncol=17,
                                           dimnames=list(NULL,
                                                         colnames(BHS.AnnexContData.Techreport.PLOTFORMAT[1:17]))),
                                    MPALevel="Dummy")) %>%
  ggplot() +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(x=MPAName,
               y=FSMean,
               alpha=MonitoringYear),
           stat="identity",
           position="dodge",
           fill=fillcols.trend,
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_errorbar(aes(x=MPAName,
                    ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(aes(x=length(unique(MPAName)),y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(MPAName)),y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(MPAName)),y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(BHS.annexplot.MPAnames[,"FS"]," "),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  coord_flip() + BHSlevel.statusplot.labs["FS"] + plot.guides.techreport + theme(axis.ticks=element_blank(),
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

BHS.ma.annexplot <- 
  ggplot(data=BHS.AnnexContData.Techreport.PLOTFORMAT,
         aes(x=MPAName,
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
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=BHS.annexplot.MPAnames[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(BHS.AnnexContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(BHS.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + BHSlevel.statusplot.labs["MA"] + plot.guides.techreport + plot.theme


# ---- 5.3 Place attachment -----

BHS.pa.annexplot <- 
  ggplot(data=BHS.AnnexContData.Techreport.PLOTFORMAT,
         aes(x=MPAName,
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
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=BHS.annexplot.MPAnames[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + BHSlevel.statusplot.labs["PA"] + plot.guides.techreport + plot.theme


# ---- 5.4 Marine tenure -----

BHS.mt.annexplot <- 
  ggplot(data=BHS.AnnexContData.Techreport.PLOTFORMAT,
         aes(x=MPAName,
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
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=BHS.annexplot.MPAnames[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + BHSlevel.statusplot.labs["MT"] + plot.guides.techreport + plot.theme


# ---- 5.5 School enrollment -----

BHS.se.annexplot <- 
  ggplot(data=BHS.AnnexContData.Techreport.PLOTFORMAT,
         aes(x=MPAName,
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
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=BHS.annexplot.MPAnames[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + BHSlevel.statusplot.labs["SE"] + plot.guides.techreport + plot.theme


# ---- 5.6 Time to market -----
BHS.time.annexplot <- 
  ggplot(data=BHS.AnnexContData.Techreport.PLOTFORMAT,
         aes(x=MPAName,
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
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=BHS.annexplot.MPAnames[,"Time"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(BHS.AnnexContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(BHS.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + BHSlevel.statusplot.labs["Time"] + plot.guides.techreport + plot.theme


# ---- 5.7 Days unwell -----

BHS.unwell.annexplot <- 
  ggplot(data=BHS.AnnexContData.Techreport.PLOTFORMAT,
         aes(x=MPAName,
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
                    colour=MPALevel,
                    alpha=MonitoringYear),
                width=0.25,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=c("Baseline", "2 Year Post", "4 Year Post"),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=BHS.annexplot.MPAnames[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(BHS.AnnexContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(BHS.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + BHSlevel.statusplot.labs["Unwell"] + plot.guides.techreport + plot.theme


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


dir.create(paste("x_Flat_data_files/1_Social/Outputs/BHS_level_Analysis/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("x_Flat_data_files/1_Social/Outputs/BHS_level_Analysis/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

# ---- 6.1 Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.fs.statusplot)
dev.off()

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.fs.trendplot)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.fs.annexplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.ma.statusplot)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.ma.trendplot)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.ma.annexplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.pa.statusplot)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.pa.trendplot)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.pa.annexplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.mt.statusplot)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.mt.trendplot)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.mt.annexplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.se.statusplot)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.se.trendplot)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.se.annexplot)
dev.off()


# ---- 6.6 Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.time.statusplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.time.trendplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.time.annexplot)
dev.off()


# ---- 6.7 Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.unwell.statusplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.unwell.trendplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(BHS.unwell.annexplot)
dev.off()


# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.gender.statusplot)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.gender.trendplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.religion.statusplot)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.religion.trendplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.primaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.primaryocc.trendplot)
dev.off()


# ---- 6.11 Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.freqfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.freqfish.trendplot)
dev.off()


# ---- 6.12 Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.freqsellfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.freqsellfish.trendplot)
dev.off()


# ---- 6.13 Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.incfish.statusplot)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.incfish.trendplot)
dev.off()


# ---- 6.14 Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.fishtech.statusplot)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.fishtech.trendplot)
dev.off()


# ---- 6.15 Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.childfs.statusplot)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.childfs.trendplot)
dev.off()


# ---- 6.16 Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.proteinfish.statusplot)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.proteinfish.trendplot)
dev.off()


# ---- 6.17 Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(BHS.age.gender.plot)
dev.off()


# ---- 6.18 Number ethnic groups ----

png(paste(FigureFileName,"Num.Ethnic.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.ethnic.statusplot)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ----BASELINE STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- Continuous data plots ----

# - FOOD SECURITY
BHS.fs.baselineplot <- 
  rbind.data.frame(BHS.ContData.Techreport.baseline.PLOTFORMAT,
                   cbind.data.frame(MPAID=NA,MPAName="  ",
                                    matrix(rep(NA,18),ncol=18,
                                           dimnames=list(NULL,
                                                         colnames(BHS.ContData.Techreport.baseline.PLOTFORMAT)[3:20])),
                                    MPALevel="Dummy")) %>%
  ggplot(aes(x=MPAName)) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(y=FSMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,
                y=FS),
            label=BHS.baselineplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,y=FS.ref),
            label=BHS.baselineplot.asterisks$FS.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  geom_text(aes(x=length(MPAName),y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MPAName),y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(MPAName),y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
BHS.ma.baselineplot <- ggplot(data=BHS.ContData.Techreport.baseline.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=MAMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,
                y=MA),
            label=BHS.baselineplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,y=MA.ref),
            label=BHS.baselineplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.ContData.Techreport.baseline.PLOTFORMAT$MAMean,na.rm=T)+
                                max(BHS.ContData.Techreport.baseline.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(BHS.ContData.Techreport.baseline.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
BHS.pa.baselineplot <- ggplot(data=BHS.ContData.Techreport.baseline.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=PAMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,
                y=PA),
            label=BHS.baselineplot.asterisks$PA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,y=PA.ref),
            label=BHS.baselineplot.asterisks$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["PA"] + plot.theme

# - MARINE TENURE
BHS.mt.baselineplot <- ggplot(data=BHS.ContData.Techreport.baseline.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=MTMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,
                y=MT+(0.05*MT)),
            label=BHS.baselineplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,y=MT.ref),
            label=BHS.baselineplot.asterisks$MT.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
BHS.se.baselineplot <- ggplot(data=BHS.ContData.Techreport.baseline.PLOTFORMAT,
                            aes(x=MPAName)) +
  geom_bar(aes(y=SEMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,
                y=SE),
            label=BHS.baselineplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,y=SE.ref),
            label=BHS.baselineplot.asterisks$SE.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1.1)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["SE"] + plot.theme

# - DAYS UNWELL
BHS.unwell.baselineplot <- ggplot(data=BHS.ContData.Techreport.baseline.PLOTFORMAT,
                                aes(x=MPAName)) +
  geom_bar(aes(y=UnwellMean,
               fill=MPALevel),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
                    ymax=UnwellMean+UnwellErr,
                    colour=MPALevel),
                width=0.25,
                size=0.5,
                show.legend=F) +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,
                y=Unwell),
            label=BHS.baselineplot.asterisks$Unwell,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=BHS.baselineplot.sigpos,
            aes(x=MPAName,y=Unwell.ref),
            label=BHS.baselineplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(BHS.ContData.Techreport.baseline.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(BHS.ContData.Techreport.baseline.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(BHS.ContData.Techreport.baseline.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + BHSlevel.statusplot.labs["Unwell"] + plot.theme



# --- READ TO PNG
png(paste(FigureFileName,"FS.baseline.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.fs.baselineplot)
dev.off()

png(paste(FigureFileName,"MA.baseline.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.ma.baselineplot)
dev.off()

png(paste(FigureFileName,"MT.baseline.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.mt.baselineplot)
dev.off()

png(paste(FigureFileName,"PA.baseline.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.pa.baselineplot)
dev.off()

png(paste(FigureFileName,"SE.baseline.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.se.baselineplot)
dev.off()

png(paste(FigureFileName,"Unwell.baseline.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(BHS.unwell.baselineplot)
dev.off()


# ---- Remove all plot objects from environment ----
rm(median.MPAs.BHS,BHS.statusplot.asterisks,BHS.statusplot.sigpos,
   BHS.conttrendplot.ylabs,BHS.proptrendplot.ylabs,
   BHS.trendplot.labs,BHS.annexplot.MPAnames,
   BHS.age.gender.baseline,BHS.age.gender.2yr,BHS.age.gender.4yr,
   BHS.agegender.legend.plot,BHS.agegender.legend,BHS.age.gender.plot,
   BHS.fs.statusplot,BHS.fs.trendplot,BHS.fs.annexplot,
   BHS.ma.statusplot,BHS.ma.trendplot,BHS.ma.annexplot,
   BHS.pa.statusplot,BHS.pa.trendplot,BHS.pa.annexplot,
   BHS.mt.statusplot,BHS.mt.trendplot,BHS.mt.annexplot,
   BHS.se.statusplot,BHS.se.trendplot,BHS.se.annexplot,
   BHS.time.statusplot,BHS.time.trendplot,BHS.time.annexplot,
   BHS.unwell.statusplot,BHS.unwell.trendplot,BHS.unwell.annexplot,
   BHS.gender.statusplot,BHS.gender.trendplot,BHS.religion.statusplot,
   BHS.religion.trendplot,BHS.primaryocc.statusplot,BHS.primaryocc.trendplot,
   BHS.freqfish.statusplot,BHS.freqsellfish.statusplot,BHS.freqsellfish.trendplot,
   BHS.incfish.statusplot,BHS.incfish.trendplot,BHS.fishtech.statusplot,
   BHS.fishtech.trendplot,BHS.childfs.statusplot,BHS.childfs.trendplot,
   BHS.proteinfish.statusplot,BHS.proteinfish.trendplot,BHS.ethnic.statusplot)

# ---- Remove all tech report datasets from environment ----
rm(BHS.AgeGender,
   BHS.ContData.Techreport.status.PLOTFORMAT,
   BHS.PropData.Techreport.status.PLOTFORMAT,
   BHS.TrendContData.Techreport.PLOTFORMAT,BHS.TrendPropData.Techreport.PLOTFORMAT,
   BHS.AnnexContData.Techreport.PLOTFORMAT,BHS.AnnexPropData.Techreport.PLOTFORMAT,
   sigvals.BHS,trend.sigvals.BHS,annex.sigvals.BHS,propdata.trend.test.BHS,
   dist.BHS.FS,dist.BHS.MA,dist.BHS.PA,dist.BHS.MT,dist.BHS.SE,dist.BHS.TimeMarket,dist.BHS.DaysUnwell)
