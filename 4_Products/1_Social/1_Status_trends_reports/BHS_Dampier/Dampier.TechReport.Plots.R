# 
# code:  Dampier Technical Report Plots
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
#  2) Source Dampier.TechReport.Datasets.R 
#     - Dependencies: Dampier.TechReport.SigTests.R
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


# ---- 1.1 Source data sets from "Dampier.TechReport.Datasets.R" ----

source("2_Social/TechnicalReports/BHS/Datasets/Dampier.TechReport.Datasets.R")


# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

Damp.statusplot.asterisks <- 
  define.statusplot.asterisks(Dampier.ContData.Techreport.status.PLOTFORMAT[,c("SettlementName","FS.pval",
                                                                               "MA.pval","PA.pval",
                                                                               "MT.pval","SE.pval",
                                                                               "Time.pval","Unwell.pval")])
Damp.statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Dampier.ContData.Techreport.status.PLOTFORMAT,
                                 Damp.statusplot.asterisks)  


# ---- 1.3 Define Dampier-specific plot labels, with significance asterisks ----

Damp.trendplot.monitoryear.labs <- rev(define.year.monitoryear.column(Dampier.AnnexContData.Techreport.PLOTFORMAT))

Damp.conttrendplot.ylabs <- 
  define.conttrendplot.ylabels.withasterisks(Dampier.TrendContData.Techreport.PLOTFORMAT
                                             [is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),
                                               c("FSMean","MAMean","PAMean","MTMean",
                                                 "SEMean","TimeMarketMean","UnwellMean")])

Damp.proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test.Damp)


Damp.trendplot.labs <- list(FS=labs(y=as.character(Damp.conttrendplot.ylabs["FSMean"]),x="Monitoring Year"),
                            MA=labs(y=as.character(Damp.conttrendplot.ylabs["MAMean"]),x="Monitoring Year"),
                            PA=labs(y=as.character(Damp.conttrendplot.ylabs["PAMean"]),x="Monitoring Year"),
                            MT=labs(y=as.character(Damp.conttrendplot.ylabs["MTMean"]),x="Monitoring Year"),
                            SE=labs(y=as.character(Damp.conttrendplot.ylabs["SEMean"]),x="Monitoring Year"),
                            Time=labs(y=as.character(Damp.conttrendplot.ylabs["TimeMarketMean"]),
                                      x="Monitoring Year"),
                            Unwell=labs(y=as.character(Damp.conttrendplot.ylabs["UnwellMean"]),
                                        x="Monitoring Year"),
                            Gender=labs(y="Gender (% head of household)",x="Monitoring Year"),
                            Religion=labs(y="Religion (% households)",x="Monitoring Year"),
                            PrimaryOcc=labs(y=as.character(Damp.proptrendplot.ylabs["PrimaryOcc"]),x="Monitoring Year"),
                            FreqFish=labs(y=as.character(Damp.proptrendplot.ylabs["FreqFish"]),x="Monitoring Year"),
                            FreqSellFish=labs(y=as.character(Damp.proptrendplot.ylabs["SellFish"]),x="Monitoring Year"),
                            IncFish=labs(y=as.character(Damp.proptrendplot.ylabs["IncFish"]),x="Monitoring Year"),
                            FishTech=labs(y=as.character(Damp.proptrendplot.ylabs["FishTech"]),x="Monitoring Year"),
                            ChildFS=labs(y=as.character(Damp.proptrendplot.ylabs["ChildFS"]),x="Monitoring Year"),
                            Protein=labs(y=as.character(Damp.proptrendplot.ylabs["Protein"]),x="Monitoring Year"))

Damp.annexplot.settnames <- 
  define.annexplot.settname.labels(annex.sigvals.Damp)

Damp.annexplot.settnames[2,] <- rep(" ",length(Damp.annexplot.settnames[2,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Baseline ----

Damp.age.gender.baseline <- 
  melt(Dampier.AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Damp.trendplot.monitoryear.labs[1]),
             size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="Population distribution (% of individuals by gender)")


# ---- 2.2 Two Year Post Baseline ----

Damp.age.gender.2yr <- 
  melt(Dampier.AgeGender,id.vars="AgeCat",measure.vars=c("Female.2yr","Male.2yr")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Damp.trendplot.monitoryear.labs[2]),
             size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     name="",
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(values=c("Female.2yr"=alpha("#7FCDBB",0.95),
                             "Male.2yr"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age")


# ---- 2.3 Four Year Post Baseline ----

Damp.age.gender.4yr <- 
  melt(Dampier.AgeGender,id.vars="AgeCat",measure.vars=c("Female.4yr","Male.4yr")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Damp.trendplot.monitoryear.labs[3]),
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

Damp.agegender.legend.plot <-
  melt(Dampier.AgeGender,id.vars="AgeCat",measure.vars=c("Female.4yr","Male.4yr")) %>%
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

Damp.agegender.legend <- g_legend(Damp.agegender.legend.plot)


Damp.age.gender.plot <- 
  grid.arrange(Damp.agegender.legend,
    arrangeGrob(
    Damp.age.gender.4yr,
    Damp.age.gender.2yr,
    Damp.age.gender.baseline,ncol=1),nrow=2,heights=c(0.35,10))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
Damp.fs.statusplot <- 
  rbind.data.frame(Dampier.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,21),ncol=21,
                                           dimnames=list(NULL,
                                                         colnames(Dampier.ContData.Techreport.status.PLOTFORMAT)[3:23])),
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=FS),
            label=Damp.statusplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=FS.ref),
            label=Damp.statusplot.asterisks$FS.ref,
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
Damp.ma.statusplot <- ggplot(data=Dampier.ContData.Techreport.status.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=MA),
            label=Damp.statusplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=MA.ref),
            label=Damp.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Dampier.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Dampier.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Dampier.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
Damp.pa.statusplot <- ggplot(data=Dampier.ContData.Techreport.status.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=PA),
            label=Damp.statusplot.asterisks$PA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=PA.ref),
            label=Damp.statusplot.asterisks$PA.ref,
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
Damp.mt.statusplot <- ggplot(data=Dampier.ContData.Techreport.status.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=MT+(0.05*MT)),
            label=Damp.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=MT.ref),
            label=Damp.statusplot.asterisks$MT.ref,
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
Damp.se.statusplot <- ggplot(data=Dampier.ContData.Techreport.status.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=SE),
            label=Damp.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=SE.ref),
            label=Damp.statusplot.asterisks$SE.ref,
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
Damp.time.statusplot <- ggplot(data=Dampier.ContData.Techreport.status.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=Time),
            label=Damp.statusplot.asterisks$Time,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=Time.ref),
            label=Damp.statusplot.asterisks$Time.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Dampier.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                      max(Dampier.ContData.Techreport.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Dampier.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Time"] + plot.theme

# - DAYS UNWELL
Damp.unwell.statusplot <- ggplot(data=Dampier.ContData.Techreport.status.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,
                y=Unwell),
            label=Damp.statusplot.asterisks$Unwell,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Damp.statusplot.sigpos,
            aes(x=SettlementName,y=Unwell.ref),
            label=Damp.statusplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Dampier.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Dampier.ContData.Techreport.status.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Dampier.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.theme

# - NUMBER UNIQUE ETHNIC GROUPS
Damp.ethnic.statusplot <- ggplot(data=Dampier.PropData.Techreport.status.PLOTFORMAT,
                                 aes(x=SettlementName)) +
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
                     limits=c(0,max(Dampier.PropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T)+
                                0.03*max(Dampier.PropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  coord_flip() + Statusplot.labs["Ethnicity"] + plot.theme

# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Damp.gender.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=SettlementName,
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
  coord_flip() + plot.theme + Statusplot.labs["Gender"] + plot.guides.techreport

# - RELIGION
Damp.religion.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=SettlementName,
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
Damp.primaryocc.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.PrimaryOcc.Other","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism","Percent.PrimaryOcc.Fish",
                                               "Percent.PrimaryOcc.HarvestForest","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
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
  coord_flip() + plot.theme + Statusplot.labs["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
Damp.freqfish.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
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
  coord_flip() + plot.theme + Statusplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Damp.freqsellfish.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
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
  coord_flip() + plot.theme + Statusplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Damp.incfish.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
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
  coord_flip() + plot.theme + Statusplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Damp.fishtech.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
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
  coord_flip() + plot.theme + Statusplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Damp.childfs.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=SettlementName,
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
  coord_flip() + plot.theme + Statusplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
Damp.proteinfish.statusplot <- 
  melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
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
  coord_flip() + plot.theme + Statusplot.labs["Protein"] + plot.guides.techreport

# # - SPREAD OF ETHNIC GROUPS
# Damp.ethnicspread.statusplot <-
#   left_join(cbind.data.frame(melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
#        id.vars="SettlementName",measure.vars=c("Prop.Majority.Ethnic4","Prop.Majority.Ethnic3",
#                                                "Prop.Majority.Ethnic2","Prop.Majority.Ethnic1",
#                                                "Prop.OtherEthnic"),
#        variable.name="Prop.Ethnic",value.name="Prop"),RandomID=1:(length(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName)*5)),
#        cbind.data.frame(melt(Dampier.PropData.Techreport.status.PLOTFORMAT,
#             id.vars="SettlementName",measure.vars=c("Majority.Ethnic4","Majority.Ethnic3",
#                                                     "Majority.Ethnic2","Majority.Ethnic1"),
#        variable.name="Ethnic.Group",value.name="Ethnicity"),
#        RandomID=1:(length(Dampier.PropData.Techreport.status.PLOTFORMAT$SettlementName)*4))) %>%
#   ggplot(aes(x=SettlementName,y=Prop,fill=Ethnicity)) +
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
Damp.fs.trendplot <- 
ggplot(Dampier.TrendContData.Techreport.PLOTFORMAT
       [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),]) +
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
Damp.ma.trendplot <- 
  ggplot(data=Dampier.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Dampier.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Dampier.TrendContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Dampier.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
Damp.pa.trendplot <- 
  ggplot(data=Dampier.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["PA"] + plot.theme

# - MARINE TENURE
Damp.mt.trendplot <- 
  ggplot(data=Dampier.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
Damp.se.trendplot <- 
  ggplot(data=Dampier.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["SE"] + plot.theme

# - TIME TO MARKET
Damp.time.trendplot <- 
  ggplot(data=Dampier.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Dampier.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Dampier.TrendContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Dampier.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["Time"] + plot.theme

# - DAYS UNWELL
Damp.unwell.trendplot <- 
  ggplot(data=Dampier.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Dampier.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Dampier.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Dampier.TrendContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Dampier.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  coord_flip() + Damp.trendplot.labs["Unwell"] + plot.theme


# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Damp.gender.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + Damp.trendplot.labs["Gender"] + plot.theme + plot.guides.techreport

# - RELIGION
Damp.religion.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["Religion"] + 
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
Damp.primaryocc.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
Damp.freqfish.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Damp.freqsellfish.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Damp.incfish.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Damp.fishtech.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Damp.childfs.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
Damp.proteinfish.trendplot <- 
  melt(Dampier.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Damp.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Damp.trendplot.labs["Protein"] + plot.guides.techreport


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

Damp.fs.annexplot <- 
  rbind.data.frame(Dampier.AnnexContData.Techreport.PLOTFORMAT,
                   cbind.data.frame(matrix(rep(NA,17),ncol=17,
                                           dimnames=list(NULL,
                                                         colnames(Dampier.AnnexContData.Techreport.PLOTFORMAT[1:17]))),
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
  geom_vline(aes(xintercept=2),
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
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(Damp.annexplot.settnames[,"FS"]," "),
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

Damp.ma.annexplot <- 
  ggplot(data=Dampier.AnnexContData.Techreport.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Damp.annexplot.settnames[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Dampier.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Dampier.AnnexContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Dampier.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["MA"] + plot.guides.techreport + plot.theme


# ---- 5.3 Place attachment -----

Damp.pa.annexplot <- 
  ggplot(data=Dampier.AnnexContData.Techreport.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Damp.annexplot.settnames[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["PA"] + plot.guides.techreport + plot.theme


# ---- 5.4 Marine tenure -----

Damp.mt.annexplot <- 
  ggplot(data=Dampier.AnnexContData.Techreport.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Damp.annexplot.settnames[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["MT"] + plot.guides.techreport + plot.theme


# ---- 5.5 School enrollment -----

Damp.se.annexplot <- 
  ggplot(data=Dampier.AnnexContData.Techreport.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Damp.annexplot.settnames[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + Statusplot.labs["SE"] + plot.guides.techreport + plot.theme


# ---- 5.6 Time to market -----
Damp.time.annexplot <- 
  ggplot(data=Dampier.AnnexContData.Techreport.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Damp.annexplot.settnames[,"Time"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Dampier.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Dampier.AnnexContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Dampier.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Time"] + plot.guides.techreport + plot.theme


# ---- 5.7 Days unwell -----

Damp.unwell.annexplot <- 
  ggplot(data=Dampier.AnnexContData.Techreport.PLOTFORMAT,
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
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_alpha_manual(name="",
                     values=c(0.3,0.6,1),
                     labels=Damp.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Damp.annexplot.settnames[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Dampier.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Dampier.AnnexContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Dampier.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.guides.techreport + plot.theme


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


dir.create(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Dampier/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Dampier/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

# ---- 6.1 Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.fs.statusplot)
dev.off()

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.fs.trendplot)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.fs.annexplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.ma.statusplot)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.ma.trendplot)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.ma.annexplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.pa.statusplot)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.pa.trendplot)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.pa.annexplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.mt.statusplot)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.mt.trendplot)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.mt.annexplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.se.statusplot)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.se.trendplot)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.se.annexplot)
dev.off()


# ---- 6.6 Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.time.statusplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.time.trendplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.time.annexplot)
dev.off()


# ---- 6.7 Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.unwell.statusplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.unwell.trendplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.unwell.annexplot)
dev.off()


# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.gender.statusplot)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.gender.trendplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.religion.statusplot)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.religion.trendplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.primaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.primaryocc.trendplot)
dev.off()


# ---- 6.11 Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.freqfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.freqfish.trendplot)
dev.off()


# ---- 6.12 Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.freqsellfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.freqsellfish.trendplot)
dev.off()


# ---- 6.13 Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.incfish.statusplot)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.incfish.trendplot)
dev.off()


# ---- 6.14 Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.fishtech.statusplot)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.fishtech.trendplot)
dev.off()


# ---- 6.15 Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.childfs.statusplot)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.childfs.trendplot)
dev.off()


# ---- 6.16 Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.proteinfish.statusplot)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.proteinfish.trendplot)
dev.off()


# ---- 6.17 Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(Damp.age.gender.plot)
dev.off()


# ---- 6.18 Number ethnic groups ----

png(paste(FigureFileName,"Num.Ethnic.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Damp.ethnic.statusplot)
dev.off()




# ---- Remove all plot objects from environment ----
rm(median.setts.Damp,Damp.statusplot.asterisks,Damp.statusplot.sigpos,
   Damp.trendplot.monitoryear.labs,Damp.conttrendplot.ylabs,Damp.proptrendplot.ylabs,
   Damp.trendplot.labs,Damp.annexplot.settnames,
   Damp.age.gender.baseline,Damp.age.gender.2yr,Damp.age.gender.4yr,
   Damp.agegender.legend.plot,Damp.agegender.legend,Damp.age.gender.plot,
   Damp.fs.statusplot,Damp.fs.trendplot,Damp.fs.annexplot,
   Damp.ma.statusplot,Damp.ma.trendplot,Damp.ma.annexplot,
   Damp.pa.statusplot,Damp.pa.trendplot,Damp.pa.annexplot,
   Damp.mt.statusplot,Damp.mt.trendplot,Damp.mt.annexplot,
   Damp.se.statusplot,Damp.se.trendplot,Damp.se.annexplot,
   Damp.time.statusplot,Damp.time.trendplot,Damp.time.annexplot,
   Damp.unwell.statusplot,Damp.unwell.trendplot,Damp.unwell.annexplot,
   Damp.gender.statusplot,Damp.gender.trendplot,Damp.religion.statusplot,
   Damp.religion.trendplot,Damp.primaryocc.statusplot,Damp.primaryocc.trendplot,
   Damp.freqfish.statusplot,Damp.freqsellfish.statusplot,Damp.freqsellfish.trendplot,
   Damp.incfish.statusplot,Damp.incfish.trendplot,Damp.fishtech.statusplot,
   Damp.fishtech.trendplot,Damp.childfs.statusplot,Damp.childfs.trendplot,
   Damp.proteinfish.statusplot,Damp.proteinfish.trendplot,Damp.ethnic.statusplot)

# ---- Remove all tech report datasets from environment ----
rm(Dampier.AgeGender,
   Dampier.ContData.Techreport.status.PLOTFORMAT,
   Dampier.PropData.Techreport.status.PLOTFORMAT,
   Dampier.TrendContData.Techreport.PLOTFORMAT,Dampier.TrendPropData.Techreport.PLOTFORMAT,
   Dampier.AnnexContData.Techreport.PLOTFORMAT,Dampier.AnnexPropData.Techreport.PLOTFORMAT,
   sigvals.Damp,trend.sigvals.Damp,annex.sigvals.Damp,propdata.trend.test.Damp,
   dist.Damp.FS,dist.Damp.MA,dist.Damp.PA,dist.Damp.MT,dist.Damp.SE,dist.Damp.TimeMarket,dist.Damp.DaysUnwell)
