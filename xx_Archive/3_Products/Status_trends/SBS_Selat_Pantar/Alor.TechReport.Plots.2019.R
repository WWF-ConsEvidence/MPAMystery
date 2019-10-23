# 
# code:  Alor Technical Report Plots
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
#  6) WRITE TO .PNG
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
source("C:/Users/HP/Dropbox/NotThisOne/Source_social_data_flat_files.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/SBS_TechReport_Calculations.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/Alor.TechReport.SigTest.2019.R")

source("C:/Users/bauer-intern/Dropbox/MPAMystery/MyWork/Alor.TechReport.Datasets.2019.R")


# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

Alor.statusplot.asterisks <- 
  define.statusplot.asterisks(Alor.ContData.Techreport.status.PLOTFORMAT[,c("SettlementName","FS.pval",
                                                                            "MA.pval","PA.pval",
                                                                            "SE.pval",
                                                                            "Unwell.pval")])
Alor.statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Alor.ContData.Techreport.status.PLOTFORMAT,
                                 Alor.statusplot.asterisks)  

# ---- 1.3 Define Alor-specific plot labels, with significance asterisks ----

Alor.trendplot.monitoryear.labs <- (define.year.monitoryear.column(Alor.AnnexContData.Techreport.PLOTFORMAT))
Alor.annexplot.monitoryear.labs <- rev(define.year.monitoryear.column(Alor.AnnexContData.Techreport.PLOTFORMAT))

Alor.conttrendplot.ylabs <- 
  define.conttrendplot.ylabels.withasterisks(Alor.TrendContData.Techreport.PLOTFORMAT
                                             [is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),
                                               c("FSMean","MAMean","PAMean",
                                                 "SEMean","UnwellMean")])

proportional.variables.plotlabs <-colnames(propdata.trend.test.Alor)
Alor.proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test.Alor)


Alor.trendplot.labs <- list(FS=labs(y=as.character(Alor.conttrendplot.ylabs["FSMean"]),x="Monitoring Year"),
                            MA=labs(y=as.character(Alor.conttrendplot.ylabs["MAMean"]),x="Monitoring Year"),
                            PA=labs(y=as.character(Alor.conttrendplot.ylabs["PAMean"]),x="Monitoring Year"),
                            SE=labs(y=as.character(Alor.conttrendplot.ylabs["SEMean"]),x="Monitoring Year"),
                            Unwell=labs(y=as.character(Alor.conttrendplot.ylabs["UnwellMean"]),
                                        x="Monitoring Year"),
                            Gender=labs(y="Gender (% head of household)",x="Monitoring Year"),
                            Religion=labs(y="Religion (% households)",x="Monitoring Year"),
                            PrimaryOcc=labs(y=as.character(Alor.proptrendplot.ylabs["Primary occupation (% households)"]),x="Monitoring Year"),
                            FreqFish=labs(y=as.character(Alor.proptrendplot.ylabs["Frequency of fishing (% households)"]),x="Monitoring Year"),
                            FreqSellFish=labs(y=as.character(Alor.proptrendplot.ylabs["Frequency of selling at least some catch (% households)"]),x="Monitoring Year"),
                            IncFish=labs(y=as.character(Alor.proptrendplot.ylabs["Income from fishing in past 6 months (% households)"]),x="Monitoring Year"),
                            FishTech=labs(y=as.character(Alor.proptrendplot.ylabs["Fishing technique most often used in past 6 months (% households)"]),x="Monitoring Year"),
                            ChildFS=labs(y=as.character(Alor.proptrendplot.ylabs["Child hunger (% households)"]),x="Monitoring Year"),
                            Protein=labs(y=as.character(Alor.proptrendplot.ylabs["Dietary protein from fish in past 6 months (% households)"]),x="Monitoring Year"))
Alor.annexplot.settnames <- 
  define.annexplot.settname.labels(annex.sigvals.Alor)

Alor.annexplot.settnames[3,] <- rep("",length(Alor.annexplot.settnames[3,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 3 Year ----

Alor.age.gender.3Year <- 
  melt(Alor.AgeGender,id.vars="AgeCat",measure.vars=c("Female.3Year","Male.3Year")) %>%
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
Alor.age.gender.3Year

# ---- 2.2 Baseline ----
Alor.age.gender.Baseline <- 
  melt(Alor.AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
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
Alor.age.gender.Baseline

Alor.agegender.legend.plot <-
  melt(Alor.AgeGender,id.vars="AgeCat",measure.vars=c("Female.3Year","Male.3Year")) %>%
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

Alor.agegender.legend <- g_legend(Alor.agegender.legend.plot)


Alor.age.gender.plot <- 
  grid.arrange(Alor.agegender.legend,
               arrangeGrob(
                 Alor.age.gender.3Year,
                 Alor.age.gender.Baseline,ncol=1),nrow=2,heights=c(0.35,10))
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
Alor.fs.statusplot <- 
  rbind.data.frame(Alor.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,17),ncol=17,
                                           dimnames=list(NULL,
                                                         colnames(Alor.ContData.Techreport.status.PLOTFORMAT)[3:19])),
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=FS),
            label=Alor.statusplot.asterisks$FS,
            nudge_x=-0.07,
            nudge_y=-.5,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=FS.ref),
            label=Alor.statusplot.asterisks$FS.ref,
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
Alor.fs.statusplot

# - MATERIAL ASSETS
Alor.ma.statusplot <- ggplot(data=Alor.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=MA),
            label=Alor.statusplot.asterisks$MA,
            nudge_x=-0.07,
            nudge_y=0.7,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=MA.ref),
            label=Alor.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Alor.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Alor.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MA"] + plot.theme
Alor.ma.statusplot

# - PLACE ATTACHMENT
Alor.pa.statusplot <- ggplot(data=Alor.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=PA),
            label=Alor.statusplot.asterisks$PA,
            nudge_x=-0.07,
            nudge_y=0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=PA.ref),
            label=Alor.statusplot.asterisks$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["PA"] + plot.theme
Alor.pa.statusplot

# - MARINE TENURE
Alor.mt.statusplot <- ggplot(data=Alor.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=MT+(0.05*MT)),
            label=Alor.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=MT.ref),
            label=Alor.statusplot.asterisks$MT.ref,
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
Alor.se.statusplot <- ggplot(data=Alor.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=SE),
            label=Alor.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=SE.ref),
            label=Alor.statusplot.asterisks$SE.ref,
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
Alor.se.statusplot

# - TIME TO MARKET
Alor.time.statusplot <- ggplot(data=Alor.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=Time),
            label=Alor.statusplot.asterisks$Time,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=Time.ref),
            label=Alor.statusplot.asterisks$Time.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Alor.ContData.Techreport.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Alor.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Time"] + plot.theme
Alor.time.statusplot

# - DAYS UNWELL
Alor.unwell.statusplot <- ggplot(data=Alor.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,
                y=Unwell),
            label=Alor.statusplot.asterisks$Unwell,
            nudge_x=-0.07,
            nudge_y=1,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Alor.statusplot.sigpos,
            aes(x=SettlementName,y=Unwell.ref),
            label=Alor.statusplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Alor.ContData.Techreport.status.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Alor.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.theme
Alor.unwell.statusplot


# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Alor.gender.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.gender.statusplot

# - RELIGION
Alor.religion.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.religion.statusplot

# - PRIMARY OCCUPATION
Alor.primaryocc.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.primaryocc.statusplot

# - FISHING FREQUENCY
Alor.freqfish.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.freqfish.statusplot

# - SELL FISH FREQUENCY
Alor.freqsellfish.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.freqsellfish.statusplot

# - INCOME FROM FISHING
Alor.incfish.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.incfish.statusplot

# - FISHING TECHNIQUE
Alor.fishtech.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.fishtech.statusplot

# - CHILDHOOD FOOD SECURITY
Alor.childfs.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.childfs.statusplot

# - PROTEIN FROM FISH
Alor.proteinfish.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.proteinfish.statusplot

# - CATEGORICAL FOOD SECURITY
Alor.FSCategorical.statusplot <-
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
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
Alor.FSCategorical.statusplot

# ADULT EDUCATION
Alor.AdultEduc.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.HHHEduc.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.econ.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.rules.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.participation.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.member.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.meeting.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.illness.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.conflict.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.NumThreat.statusplot <- 
  melt(Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
Alor.ThreatType.statusplot <- 
  melt(Alor.Threat.Types.PLOTFORMAT,
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
Alor.ethnicity.statusplot <- ggplot(data=Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
                     limits=c(0,max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T) + 
                                0.03*max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Ethnicity"] + plot.theme


# - Contribution
Alor.contribution.statusplot <- ggplot(data=Alor.SBSPropData.Techreport.status.PLOTFORMAT,
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
                     limits=c(0,max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Contribution,na.rm=T) + 
                                1.5* max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Contribution,na.rm=T)), labels = scales::comma) +
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
Alor.fs.trendplot <- 
  ggplot(Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),]) +
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
Alor.fs.trendplot

# - MATERIAL ASSETS
Alor.ma.trendplot <- 
  ggplot(data=Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Alor.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Alor.TrendContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Alor.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["MA"] + plot.theme
Alor.ma.trendplot

# - PLACE ATTACHMENT
Alor.pa.trendplot <- 
  ggplot(data=Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["PA"] + plot.theme
Alor.pa.trendplot

# - MARINE TENURE
Alor.mt.trendplot <- 
  ggplot(data=Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["MT"] + plot.theme
Alor.mt.trendplot

# - SCHOOL ENROLLMENT
Alor.se.trendplot <- 
  ggplot(data=Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["SE"] + plot.theme
Alor.se.trendplot

# - TIME TO MARKET
Alor.time.trendplot <- 
  ggplot(data=Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Alor.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Alor.TrendContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Alor.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["Time"] + plot.theme

# - DAYS UNWELL
Alor.unwell.trendplot <- 
  ggplot(data=Alor.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Alor.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Alor.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Alor.TrendContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Alor.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  coord_flip() + Alor.trendplot.labs["Unwell"] + plot.theme
Alor.unwell.trendplot

# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Alor.gender.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=rev(MonitoringYear),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + Alor.trendplot.labs["Gender"] + plot.theme + plot.guides.techreport
Alor.gender.trendplot

# - RELIGION
Alor.religion.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["Religion"] + 
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
Alor.religion.trendplot

# - PRIMARY OCCUPATION
Alor.primaryocc.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["PrimaryOcc"] + plot.guides.techreport 
Alor.primaryocc.trendplot

# - FISHING FREQUENCY
Alor.freqfish.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["FreqFish"] + plot.guides.techreport
Alor.freqfish.trendplot

# - SELL FISH FREQUENCY
Alor.freqsellfish.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["FreqSellFish"] + plot.guides.techreport
Alor.freqsellfish.trendplot

# - INCOME FROM FISHING
Alor.incfish.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["IncFish"] + plot.guides.techreport
Alor.incfish.trendplot 

# - FISHING TECHNIQUE
Alor.fishtech.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["FishTech"] + plot.guides.techreport
Alor.fishtech.trendplot

# - CHILDHOOD FOOD SECURITY
Alor.childfs.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["ChildFS"] + plot.guides.techreport
Alor.childfs.trendplot

# - PROTEIN FROM FISH
Alor.proteinfish.trendplot <- 
  melt(Alor.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Alor.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Alor.trendplot.labs["Protein"] + plot.guides.techreport
Alor.proteinfish.trendplot

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

Alor.fs.annexplot <- 
  rbind.data.frame(Alor.AnnexContData.Techreport.PLOTFORMAT,
                   cbind.data.frame(MonitoringYear=NA,SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,12),ncol=12,
                                           dimnames=list(NULL,
                                                         colnames(Alor.AnnexContData.Techreport.PLOTFORMAT)[4:15])),
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
                     labels=(Alor.trendplot.monitoryear.labs),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(Alor.annexplot.settnames[,"FS"]," "),
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
Alor.fs.annexplot

# ---- 5.2 Material assets -----

Alor.ma.annexplot <- 
  ggplot(data=Alor.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Alor.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Alor.annexplot.settnames[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Alor.AnnexContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Alor.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["MA"] + plot.guides.techreport + plot.theme

Alor.ma.annexplot

# ---- 5.3 Place attachment -----

Alor.pa.annexplot <- 
  ggplot(data=Alor.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Alor.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Alor.annexplot.settnames[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["PA"] + plot.guides.techreport + plot.theme
Alor.pa.annexplot

# ---- 5.4 Marine tenure -----

Alor.mt.annexplot <- 
  ggplot(data=Alor.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Alor.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Alor.annexplot.settnames[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["MT"] + plot.guides.techreport + plot.theme


# ---- 5.5 School enrollment -----

Alor.se.annexplot <- 
  ggplot(data=Alor.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Alor.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Alor.annexplot.settnames[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + Statusplot.labs["SE"] + plot.guides.techreport + plot.theme
Alor.se.annexplot

# ---- 5.6 Time to market -----
Alor.time.annexplot <- 
  ggplot(data=Alor.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Alor.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Alor.annexplot.settnames[,"Time"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Alor.AnnexContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Alor.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Time"] + plot.guides.techreport + plot.theme


# ---- 5.7 Days unwell -----

Alor.unwell.annexplot <- 
  ggplot(data=Alor.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Alor.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Alor.annexplot.settnames[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Alor.AnnexContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Alor.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.guides.techreport + plot.theme
Alor.unwell.annexplot


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

dir.create(paste("C:/Users/HP/Dropbox/AlorProducts",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("C:/Users/HP/Dropbox/AlorProducts",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.fs.trendplot)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.fs.annexplot)
dev.off()

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.fs.statusplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.ma.statusplot)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.ma.trendplot)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.ma.annexplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.pa.statusplot)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.pa.trendplot)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.pa.annexplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.mt.statusplot)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.mt.trendplot)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.mt.annexplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.se.statusplot)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.se.trendplot)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.se.annexplot)
dev.off()


# ---- 6.6 Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.time.statusplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.time.trendplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.time.annexplot)
dev.off()


# ---- 6.7 Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.unwell.statusplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.unwell.trendplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Alor.unwell.annexplot)
dev.off()


# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.gender.statusplot)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.gender.trendplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.religion.statusplot)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.religion.trendplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.primaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.primaryocc.trendplot)
dev.off()


# ---- 6.11 Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.freqfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.freqfish.trendplot)
dev.off()


# ---- 6.12 Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.freqsellfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.freqsellfish.trendplot)
dev.off()


# ---- 6.13 Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.incfish.statusplot)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.incfish.trendplot)
dev.off()


# ---- 6.14 Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.fishtech.statusplot)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.fishtech.trendplot)
dev.off()


# ---- 6.15 Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.childfs.statusplot)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.childfs.trendplot)
dev.off()


# ---- 6.16 Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.proteinfish.statusplot)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.proteinfish.trendplot)
dev.off()


# ---- 6.17 Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(Alor.age.gender.plot)
dev.off()

# ---- 6.18 Number ethnic groups ----

png(paste(FigureFileName,"Num.Ethnic.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Alor.ethnic.statusplot)
dev.off()


