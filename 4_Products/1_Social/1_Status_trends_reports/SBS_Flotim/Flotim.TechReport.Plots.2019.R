# 
# code:  Flotim Technical Report Plots
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS/Plots
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2017
# modified: Amari Bauer, June 2019
# 
# 
# ---- inputs ----
#  1) Source Flotim.TR.Datasets.R 
#     - Dependencies: Flotim.TR.SigTest.R
#                     After_Calculate_BigFive.R
#                     Calculate_BigFive.R
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
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/SBS_TechReport_Calculations.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/Flotim.TechReport.SigTest.2019.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/Flotim.TechReport.Datasets.2019.R")


#DETERMINING HOW MANY PEOPLE ACTUALLY RESPONDED TO THE FISH QUESTIONS
HHData %>%
  filter(MPAID==16) %>%
  filter(Treatment==1) %>%
  group_by(SettlementName,InterviewYear) %>%
  summarise(total=length(HouseholdID),
            actualfishfreq=length(FreqFish[!is.na(FreqFish)]),
            actualfreqsale=length(FreqSaleFish[!is.na(FreqSaleFish)]),
            actualpercentinc=length(PercentIncFish[!is.na(PercentIncFish)]),
            actualmajfishtech=length(MajFishTechnique[!is.na(MajFishTechnique)]),
            actualproteinfish=length(PercentProteinFish[!is.na(PercentProteinFish)]),
            actualeatfish=length(FreqEatFish[!is.na(FreqEatFish)])) %>% View()


# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

library(gridExtra)

Flotim.statusplot.asterisks <- 
  define.statusplot.asterisks(Flotim.ContData.Techreport.status.PLOTFORMAT[,c("SettlementName","FS.pval",
                                                                              "MA.pval","MT.pval","PA.pval",
                                                                              "SE.pval", "TimeMarket.pval",
                                                                              "Unwell.pval")])
Flotim.statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Flotim.ContData.Techreport.status.PLOTFORMAT,
                                 Flotim.statusplot.asterisks)  


# ---- 1.3 Define Flotim-specific plot labels, with significance asterisks ----

Flotim.annexplot.monitoryear.labs <- rev(define.year.monitoryear.column(Flotim.AnnexContData.Techreport.PLOTFORMAT))
Flotim.trendplot.monitoryear.labs <- (define.year.monitoryear.column(Flotim.AnnexContData.Techreport.PLOTFORMAT))

Flotim.conttrendplot.ylabs <- 
  define.conttrendplot.ylabels.withasterisks(Flotim.TrendContData.Techreport.PLOTFORMAT
                                             [is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),
                                               c("FSMean","MAMean","PAMean","MTMean",
                                                 "SEMean","TimeMarketMean","UnwellMean")])

proportional.variables.plotlabs <-colnames(propdata.trend.test.Flotim)

Flotim.proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test.Flotim)


Flotim.trendplot.labs <- list(FS=labs(y=as.character(Flotim.conttrendplot.ylabs["FSMean"]),x="Monitoring Year"),
                              MA=labs(y=as.character(Flotim.conttrendplot.ylabs["MAMean"]),x="Monitoring Year"),
                              MT=labs(y=as.character(Flotim.conttrendplot.ylabs["MTMean"]),x="Monitoring Year"),
                              PA=labs(y=as.character(Flotim.conttrendplot.ylabs["PAMean"]),x="Monitoring Year"),
                              SE=labs(y=as.character(Flotim.conttrendplot.ylabs["SEMean"]),x="Monitoring Year"),
                              Market=labs(y=as.character(Flotim.conttrendplot.ylabs["TimeMarketMean"]),
                                          x="Monitoring Year"),
                              Unwell=labs(y=as.character(Flotim.conttrendplot.ylabs["UnwellMean"]),x="Monitoring Year"),
                              Gender=labs(y="Gender (% head of household)",x="Monitoring Year"),
                              Religion=labs(y="Religion (% households)",x="Monitoring Year"),
                              PrimaryOcc=labs(y=as.character(Flotim.proptrendplot.ylabs["Primary occupation (% households)"]),x="Monitoring Year"),
                              FreqFish=labs(y=as.character(Flotim.proptrendplot.ylabs["Frequency of fishing (% households)"]),x="Monitoring Year"),
                              FreqSellFish=labs(y=as.character(Flotim.proptrendplot.ylabs["Frequency of selling at least some catch (% households)"]),x="Monitoring Year"),
                              IncFish=labs(y=as.character(Flotim.proptrendplot.ylabs["Income from fishing in past 6 months (% households)"]),x="Monitoring Year"),
                              FishTech=labs(y=as.character(Flotim.proptrendplot.ylabs["Fishing technique most often used in past 6 months (% households)"]),x="Monitoring Year"),
                              ChildFS=labs(y=as.character(Flotim.proptrendplot.ylabs["Child hunger (% households)"]),x="Monitoring Year"),
                              Protein=labs(y=as.character(Flotim.proptrendplot.ylabs["Dietary protein from fish in past 6 months (% households)"]),x="Monitoring Year"))

Flotim.annexplot.settnames <- 
  define.annexplot.settname.labels(annex.sigvals.Flotim)

Flotim.annexplot.settnames[3,] <- rep("",length(Flotim.annexplot.settnames[3,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 3 Year ----
Flotim.age.gender.3Year <- 
  melt(Flotim.AgeGender,id.vars="AgeCat",measure.vars=c("Female.3Year","Male.3Year")) %>%
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
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=c("Female","Male"),
                    values=c("Female.3Year"=alpha("#7FCDBB",0.95),
                             "Male.3Year"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="2017 Population distribution (% of individuals by gender)")+
  theme(legend.position="none")
Flotim.age.gender.3Year

# ---- 2.2 Baseline ----
Flotim.age.gender.Baseline <- 
  melt(Flotim.AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
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
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=c("Female","Male"),
                    values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="2014 Population distribution (% of individuals by gender)")+
  theme(legend.position="none")

Flotim.age.gender.Baseline

Flotim.agegender.legend.plot <-
  melt(Flotim.AgeGender,id.vars="AgeCat",measure.vars=c("Female.3Year","Male.3Year")) %>%
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
                    values=c("Female.3Year"=alpha("#7FCDBB",0.95),
                             "Male.3Year"=alpha("#253494",0.95)),
                    labels=c("Female","Male")) +
  coord_flip() + plot.guides.techreport + theme(legend.justification="right")
Flotim.agegender.legend.plot

Flotim.agegender.legend <- g_legend(Flotim.agegender.legend.plot)

Flotim.age.gender.plot <- 
  grid.arrange(Flotim.agegender.legend,
               arrangeGrob(
                 Flotim.age.gender.3Year,
                 Flotim.age.gender.Baseline,ncol=1),nrow=2,heights=c(0.35,10))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
Flotim.fs.statusplot <- 
  rbind.data.frame(Flotim.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,22),ncol=22,
                                           dimnames=list(NULL,
                                                         colnames(Flotim.ContData.Techreport.status.PLOTFORMAT)[3:24])),
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=FS),
            label=Flotim.statusplot.asterisks$FS,
            nudge_x=-0.07,
            nudge_y=-0.1,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=FS.ref),
            label=Flotim.statusplot.asterisks$FS.ref,
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
Flotim.fs.statusplot

# - MATERIAL ASSETS
Flotim.ma.statusplot <- ggplot(data=Flotim.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=MA),
            label=Flotim.statusplot.asterisks$MA,
            nudge_x=-0.07,
            nudge_y=0.28,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=MA.ref),
            label=Flotim.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Flotim.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Flotim.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Flotim.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MA"] + plot.theme
Flotim.ma.statusplot

# - PLACE ATTACHMENT
Flotim.pa.statusplot <- ggplot(data=Flotim.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=PA),
            label=Flotim.statusplot.asterisks$PA,
            nudge_x=-0.07,
            nudge_y=0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=PA.ref),
            label=Flotim.statusplot.asterisks$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["PA"] + plot.theme
Flotim.pa.statusplot

# - MARINE TENURE
Flotim.mt.statusplot <- ggplot(data=Flotim.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=MT+(0.05*MT)),
            label=Flotim.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=MT.ref),
            label=Flotim.statusplot.asterisks$MT.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MT"] + plot.theme
Flotim.mt.statusplot

# - SCHOOL ENROLLMENT
Flotim.se.statusplot <- ggplot(data=Flotim.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=SE),
            label=Flotim.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=SE.ref),
            label=Flotim.statusplot.asterisks$SE.ref,
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
Flotim.se.statusplot

# - TIME TO MARKET
Flotim.time.statusplot <- ggplot(data=Flotim.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=Market),
            label=Flotim.statusplot.asterisks$Market,
            nudge_x=-0.07,
            nudge_y=0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=Market.ref),
            label=Flotim.statusplot.asterisks$Market.ref,
            size=rel(3),
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Flotim.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Flotim.ContData.Techreport.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Flotim.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Time"] + plot.theme
Flotim.time.statusplot

# - DAYS UNWELL
Flotim.unwell.statusplot <- ggplot(data=Flotim.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,
                y=Unwell),
            label=Flotim.statusplot.asterisks$Unwell,
            nudge_x=-0.07,
            nudge_y=-0.1,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Flotim.statusplot.sigpos,
            aes(x=SettlementName,y=Unwell.ref),
            label=Flotim.statusplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Flotim.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Flotim.ContData.Techreport.status.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Flotim.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.theme
Flotim.unwell.statusplot


# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Flotim.gender.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.gender.statusplot

# - RELIGION
Flotim.religion.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.religion.statusplot

# - PRIMARY OCCUPATION
Flotim.primaryocc.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.PrimaryOcc.Other", 
                                               "Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism",
                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.HarvestForest", "Percent.PrimaryOcc.Farm")) %>%
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
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products", "Farming")) +
  coord_flip() + plot.theme + Statusplot.labs["PrimaryOcc"] + plot.guides.techreport
Flotim.primaryocc.statusplot


# - FISHING FREQUENCY
Flotim.freqfish.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.freqfish.statusplot

# - SELL FISH FREQUENCY
Flotim.freqsellfish.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.freqsellfish.statusplot

# - INCOME FROM FISHING
Flotim.incfish.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.incfish.statusplot

# - FISHING TECHNIQUE
Flotim.fishtech.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.fishtech.statusplot

# - CHILDHOOD FOOD SECURITY
Flotim.childfs.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.childfs.statusplot

# - PROTEIN FROM FISH
Flotim.proteinfish.statusplot <- 
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.proteinfish.statusplot

# - CATEGORICAL FOOD SECURITY
Flotim.FSCategorical.statusplot <-
  melt(Flotim.PropData.Techreport.status.PLOTFORMAT,
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
Flotim.FSCategorical.statusplot

# ADULT EDUCATION
Flotim.AdultEduc.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("AdultEducHigher", "AdultEducSec", "AdultEducMid",
                                               "AdultEducPrim", "AdultEducPre", "AdultEducNone")) %>%
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
                    values=multianswer.fillcols.status[["AdultEducation"]],
                    labels=c("Further or higher education","High school education","Middle school education","Primary school education","Pre-school education", "No formal education")) +
  coord_flip() + plot.theme + Statusplot.labs["AdultEduc"] + plot.guides.techreport


# HOUSEHOLD HEAD EDUCATION
Flotim.HHHEduc.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("HHHEducHigher", "HHHEducSec", "HHHEducMid",
                                               "HHHEducPrim", "HHHEducPre", "HHHEducNone")) %>%
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
                    values=multianswer.fillcols.status[["HHHEducation"]],
                    labels=c("Further or higher education","High school education","Middle school education","Primary school education","Pre-school education", "No formal education")) +
  coord_flip() + plot.theme + Statusplot.labs["HHHEduc"] + plot.guides.techreport


# ECONOMIC STATUS
Flotim.econ.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
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
                    labels=c("Much better","Slightly better","Neither better or worse","Slightly worse","Much worse")) +
  coord_flip() + plot.theme + Statusplot.labs["EconStatus"] + plot.guides.techreport

# RULES
Flotim.rules.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("PropRuleHab", "PropRuleSpp")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PropRules"]],
                    labels=c("Important species","Important habitats")) +
  coord_flip() + plot.theme + Statusplot.labs["Rules"] + plot.guides.techreport

# PARTICIPATION IN DECISION-MAKING
Flotim.participation.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("ParticipateRules","ParticipateBnd","ParticipateOrg", "ParticipateEstablish")) %>%
  filter(., SettlementName!= "Control\nSettlements") %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Participate"]],
                    labels=c("Setting appropriation rules", "MPA boundary delineation", "Design of MPA management body", "Design of MPA-managing organization")) +
  coord_flip() + plot.theme + Statusplot.labs["Participation"] + plot.guides.techreport


# - MEMBER OF MARINE RESOURCE ORGANIZATION
Flotim.member.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Member.No","Member.Yes")) %>%
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
                    values=multianswer.fillcols.status[["Member"]],
                    labels=c("Non-member","Member")) +
  coord_flip() + plot.theme + Statusplot.labs["Member"] + plot.guides.techreport


# - MEETING ATTENDANCE
Flotim.meeting.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.Member.Yes.Meeting.No", "Prop.Member.Yes.Meeting.Yes")) %>%
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
                    values=multianswer.fillcols.status[["Attendance"]],
                    labels=c("Have not attended a meeting","Attended a meeting")) +
  coord_flip() + plot.theme + Statusplot.labs["Attendance"] + plot.guides.techreport

# - ILLNESS
Flotim.illness.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.Not.Ill", "Percent.Ill")) %>%
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
                    values=multianswer.fillcols.status[["Illness"]],
                    labels=c("Ill or injured ","Not Ill or injured")) +
  coord_flip() + plot.theme + Statusplot.labs["Ill"] + plot.guides.techreport

# MARINE RESOUCE CONFLICT
Flotim.conflict.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
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
                    labels=c("Greatly decreased","Decreased","Neither increased or decreased","Increased","Greatly Increased")) +
  coord_flip() + plot.theme + Statusplot.labs["Conflict"] + plot.guides.techreport

# NUMBER OF LOCAL THREATS
Flotim.NumThreat.statusplot <- 
  melt(Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
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
                    values=multianswer.fillcols.status[["NumThreats"]],
                    labels=c("More than five threats","Four threats","Three threats","Two threats","One threat", "No threats")) +
  coord_flip() + plot.theme + Statusplot.labs["NumLocalThreats"] + plot.guides.techreport


# - THREAT TYPES
Flotim.ThreatType.statusplot <- 
  melt(Flotim.Threat.Types.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Other", "OtherMarineUses", "NaturalProcesses", "HabitatLoss", 
                                               "ClimateChange", "IllegalFishing", "DestructiveFishing", "Pollution")) %>%
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
                    values=multianswer.fillcols.status[["ThreatType"]],
                    labels=c("Other", "Other marine resource uses", "Natural processes", "Habitat loss", 
                             "Climate change", "Illegal fishing", "Destructive fishing", "Pollution")) +
  coord_flip() + plot.theme + Statusplot.labs["ThreatTypes"] + plot.guides.techreport


# - Number of Ethnicities
Flotim.ethnicity.statusplot <- ggplot(data=Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
                                      aes(x=SettlementName)) +
  geom_bar(aes(y=Num.EthnicGroups,
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
                     limits=c(0,max(Flotim.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T) + 
                                0.03*max(Flotim.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Ethnicity"] + plot.theme


# - Contribution
Flotim.contribution.statusplot <- ggplot(data=Flotim.SBSPropData.Techreport.status.PLOTFORMAT,
                                         aes(x=SettlementName)) +
  geom_bar(aes(y=Contribution,
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
                     limits=c(0,max(Flotim.SBSPropData.Techreport.status.PLOTFORMAT$Contribution,na.rm=T) + 
                                1.5* max(Flotim.SBSPropData.Techreport.status.PLOTFORMAT$Contribution,na.rm=T)), labels = scales::comma) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Contribution"] + plot.theme

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TREND PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 4.1 Continuous data plots ----

# - FOOD SECURITY 
Flotim.fs.trendplot <- 
  ggplot(Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),]) +
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
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
Flotim.fs.trendplot

# - MATERIAL ASSETS
Flotim.ma.trendplot <- 
  ggplot(data=Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Flotim.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Flotim.TrendContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Flotim.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["MA"] + plot.theme
Flotim.ma.trendplot

# - PLACE ATTACHMENT
Flotim.pa.trendplot <- 
  ggplot(data=Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["PA"] + plot.theme
Flotim.pa.trendplot

# - MARINE TENURE
Flotim.mt.trendplot <- 
  ggplot(data=Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["MT"] + plot.theme
Flotim.mt.trendplot

# - SCHOOL ENROLLMENT
Flotim.se.trendplot <- 
  ggplot(data=Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["SE"] + plot.theme
Flotim.se.trendplot

# - TIME TO MARKET
Flotim.time.trendplot <- 
  ggplot(data=Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Flotim.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Flotim.TrendContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Flotim.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["Market"] + plot.theme
Flotim.time.trendplot

# - DAYS UNWELL
Flotim.unwell.trendplot <- 
  ggplot(data=Flotim.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Flotim.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Flotim.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Flotim.TrendContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Flotim.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  coord_flip() + Flotim.trendplot.labs["Unwell"] + plot.theme
Flotim.unwell.trendplot

# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Flotim.gender.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + Flotim.trendplot.labs["Gender"] + plot.theme + plot.guides.techreport
Flotim.gender.trendplot

# - RELIGION
Flotim.religion.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["Religion"] + 
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
Flotim.religion.trendplot

# - PRIMARY OCCUPATION
Flotim.primaryocc.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Percent.PrimaryOcc.Other","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism","Percent.PrimaryOcc.Fish",
                                               "Percent.PrimaryOcc.HarvestForest","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["PrimaryOcc"] + plot.guides.techreport 
Flotim.primaryocc.trendplot

#USED TO CHECK DISTRIBUTION OF SECONDARY OCCUPATIONS
Flotim.Secondaryocc.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Percent.SecondaryOcc.Other","Percent.SecondaryOcc.WageLabor",
                                               "Percent.SecondaryOcc.Tourism","Percent.SecondaryOcc.Fish",
                                               "Percent.SecondaryOcc.HarvestForest","Percent.SecondaryOcc.Farm")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["SecondaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + labs(y="Secondary occupation (% households)",x="Monitoring Year") + plot.guides.techreport 
Flotim.Secondaryocc.trendplot


# - FISHING FREQUENCY
Flotim.freqfish.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["FreqFish"] + plot.guides.techreport
Flotim.freqfish.trendplot

# - SELL FISH FREQUENCY
Flotim.freqsellfish.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["FreqSellFish"] + plot.guides.techreport
Flotim.freqsellfish.trendplot

# - INCOME FROM FISHING
Flotim.incfish.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["IncFish"] + plot.guides.techreport
Flotim.incfish.trendplot 

# - FISHING TECHNIQUE
Flotim.fishtech.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["FishTech"] + plot.guides.techreport
Flotim.fishtech.trendplot

# - CHILDHOOD FOOD SECURITY
Flotim.childfs.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=(MonitoringYear),
             y=value)) +
  geom_bar(aes(fill=variable),
           stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["ChildFS"] + plot.guides.techreport
Flotim.childfs.trendplot

# - PROTEIN FROM FISH
Flotim.proteinfish.trendplot <- 
  melt(Flotim.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Flotim.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Flotim.trendplot.labs["Protein"] + plot.guides.techreport
Flotim.proteinfish.trendplot

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

Flotim.fs.annexplot <- 
  rbind.data.frame(Flotim.AnnexContData.Techreport.PLOTFORMAT,
                   cbind.data.frame(MonitoringYear=NA,SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,14),ncol=14,
                                           dimnames=list(NULL,
                                                         colnames(Flotim.AnnexContData.Techreport.PLOTFORMAT)[4:17])),
                                    SettLevel="Dummy")) %>%
  ggplot() +
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
                     labels=(Flotim.annexplot.monitoryear.labs),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(Flotim.annexplot.settnames[,"FS"]," "),
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
Flotim.fs.annexplot

# ---- 5.2 Material assets -----

Flotim.ma.annexplot <- 
  ggplot(data=Flotim.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Flotim.annexplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Flotim.annexplot.settnames[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Flotim.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Flotim.AnnexContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Flotim.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["MA"] + plot.guides.techreport + plot.theme

Flotim.ma.annexplot

# ---- 5.3 Place attachment -----

Flotim.pa.annexplot <- 
  ggplot(data=Flotim.AnnexContData.Techreport.PLOTFORMAT,
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
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Flotim.annexplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Flotim.annexplot.settnames[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["PA"] + plot.guides.techreport + plot.theme
Flotim.pa.annexplot

# ---- 5.4 Marine tenure -----

Flotim.mt.annexplot <- 
  ggplot(data=Flotim.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Flotim.annexplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Flotim.annexplot.settnames[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["MT"] + plot.guides.techreport + plot.theme
Flotim.mt.annexplot


# ---- 5.5 School enrollment -----

Flotim.se.annexplot <- 
  ggplot(data=Flotim.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Flotim.annexplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Flotim.annexplot.settnames[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + Statusplot.labs["SE"] + plot.guides.techreport + plot.theme
Flotim.se.annexplot

# ---- 5.6 Time to market -----
Flotim.time.annexplot <- 
  ggplot(data=Flotim.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Flotim.annexplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Flotim.annexplot.settnames[,"TimeMarket"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Flotim.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Flotim.AnnexContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Flotim.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Time"] + plot.guides.techreport + plot.theme
Flotim.time.annexplot

# ---- 5.7 Days unwell -----

Flotim.unwell.annexplot <- 
  ggplot(data=Flotim.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Flotim.annexplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Flotim.annexplot.settnames[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Flotim.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Flotim.AnnexContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Flotim.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.guides.techreport + plot.theme
Flotim.unwell.annexplot


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
dir.create(paste("C:/Users/HP/Dropbox/Products/",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("C:/Users/HP/Dropbox/Products/",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.fs.trendplot)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.fs.annexplot)
dev.off()

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.fs.statusplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.ma.statusplot)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.ma.trendplot)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.ma.annexplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.pa.statusplot)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.pa.trendplot)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.pa.annexplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.mt.statusplot)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.mt.trendplot)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.mt.annexplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.se.statusplot)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.se.trendplot)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.se.annexplot)
dev.off()


# ---- 6.6 Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.time.statusplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.time.trendplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.time.annexplot)
dev.off()


# ---- 6.7 Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.unwell.statusplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.unwell.trendplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Flotim.unwell.annexplot)
dev.off()


# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.gender.statusplot)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.gender.trendplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.religion.statusplot)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.religion.trendplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.primaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.primaryocc.trendplot)
dev.off()

# ---- 6.15 Secondary occupation ----

png(paste(FigureFileName,"SecondaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.Secondaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"SecondaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.Secondaryocc.trendplot)
dev.off()


# ---- 6.11 Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.freqfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.freqfish.trendplot)
dev.off()


# ---- 6.12 Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.freqsellfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.freqsellfish.trendplot)
dev.off()


# ---- 6.13 Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.incfish.statusplot)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.incfish.trendplot)
dev.off()


# ---- 6.14 Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.fishtech.statusplot)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.fishtech.trendplot)
dev.off()


# ---- 6.15 Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.childfs.statusplot)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.childfs.trendplot)
dev.off()


# ---- 6.16 Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.proteinfish.statusplot)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.proteinfish.trendplot)
dev.off()

# ---- 6.17 Age/Gender ----
library(grid)
png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(Flotim.age.gender.plot)
dev.off()


# ---- 6.18 Number ethnic groups ----

png(paste(FigureFileName,"Num.Ethnic.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Flotim.ethnic.statusplot)
dev.off()

