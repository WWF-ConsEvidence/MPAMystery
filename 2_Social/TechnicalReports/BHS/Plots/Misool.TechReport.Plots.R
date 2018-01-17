# 
# code:  Misool Technical Report Plots
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
#  2) Source Misool.TechReport.Datasets.R 
#     - Dependencies: Misool.TechReport.SigTests.R
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


# ---- 1.1 Source data sets from "Misool.TechReport.Datasets.R" ----

source("2_Social/TechnicalReports/BHS/Datasets/Misool.TechReport.Datasets.R")


# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

Mis.statusplot.asterisks <- 
  define.statusplot.asterisks(Misool.ContData.Techreport.status.PLOTFORMAT[,c("SettlementName","FS.pval",
                                                                               "MA.pval","PA.pval",
                                                                               "MT.pval","SE.pval",
                                                                               "Time.pval","Unwell.pval")])
Mis.statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Misool.ContData.Techreport.status.PLOTFORMAT,
                                 Mis.statusplot.asterisks)  


# ---- 1.3 Define Misool-specific plot labels, with significance asterisks ----

Mis.trendplot.monitoryear.labs <- rev(define.year.monitoryear.column(Misool.AnnexContData.Techreport.PLOTFORMAT))

Mis.conttrendplot.ylabs <- 
  define.conttrendplot.ylabels.withasterisks(Misool.TrendContData.Techreport.PLOTFORMAT
                                             [is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),
                                               c("FSMean","MAMean","PAMean","MTMean",
                                                 "SEMean","TimeMarketMean","UnwellMean")])

Mis.proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test.Mis)


Mis.trendplot.labs <- list(FS=labs(y=as.character(Mis.conttrendplot.ylabs["FSMean"]),x="Monitoring Year"),
                            MA=labs(y=as.character(Mis.conttrendplot.ylabs["MAMean"]),x="Monitoring Year"),
                            PA=labs(y=as.character(Mis.conttrendplot.ylabs["PAMean"]),x="Monitoring Year"),
                            MT=labs(y=as.character(Mis.conttrendplot.ylabs["MTMean"]),x="Monitoring Year"),
                            SE=labs(y=as.character(Mis.conttrendplot.ylabs["SEMean"]),x="Monitoring Year"),
                            Time=labs(y=as.character(Mis.conttrendplot.ylabs["TimeMarketMean"]),
                                      x="Monitoring Year"),
                            Unwell=labs(y=as.character(Mis.conttrendplot.ylabs["UnwellMean"]),
                                        x="Monitoring Year"),
                            Gender=labs(y="Gender (% head of household)",x="Monitoring Year"),
                            Religion=labs(y="Religion (% households)",x="Monitoring Year"),
                            PrimaryOcc=labs(y=as.character(Mis.proptrendplot.ylabs["PrimaryOcc"]),x="Monitoring Year"),
                            FreqFish=labs(y=as.character(Mis.proptrendplot.ylabs["FreqFish"]),x="Monitoring Year"),
                            FreqSellFish=labs(y=as.character(Mis.proptrendplot.ylabs["SellFish"]),x="Monitoring Year"),
                            IncFish=labs(y=as.character(Mis.proptrendplot.ylabs["IncFish"]),x="Monitoring Year"),
                            FishTech=labs(y=as.character(Mis.proptrendplot.ylabs["FishTech"]),x="Monitoring Year"),
                            ChildFS=labs(y=as.character(Mis.proptrendplot.ylabs["ChildFS"]),x="Monitoring Year"),
                            Protein=labs(y=as.character(Mis.proptrendplot.ylabs["Protein"]),x="Monitoring Year"))

Mis.annexplot.settnames <- 
  define.annexplot.settname.labels(annex.sigvals.Mis)

Mis.annexplot.settnames[2,] <- rep(" ",length(Mis.annexplot.settnames[2,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Baseline ----

Mis.age.gender.baseline <- 
  melt(Misool.AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Mis.trendplot.monitoryear.labs[1]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="Population distribution (% of individuals by gender)")


# ---- 2.2 Two Year Post Baseline ----

Mis.age.gender.2yr <- 
  melt(Misool.AgeGender,id.vars="AgeCat",measure.vars=c("Female.2yr","Male.2yr")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Mis.trendplot.monitoryear.labs[2]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     name="",
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(values=c("Female.2yr"=alpha("#7FCDBB",0.95),
                             "Male.2yr"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age")


# ---- 2.3 Four Year Post Baseline ----

Mis.age.gender.4yr <- 
  melt(Misool.AgeGender,id.vars="AgeCat",measure.vars=c("Female.4yr","Male.4yr")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-8,label=Mis.trendplot.monitoryear.labs[3]),
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

Mis.agegender.legend.plot <-
  melt(Misool.AgeGender,id.vars="AgeCat",measure.vars=c("Female.4yr","Male.4yr")) %>%
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

Mis.agegender.legend <- g_legend(Mis.agegender.legend.plot)


Mis.age.gender.plot <- 
  grid.arrange(Mis.agegender.legend,
               arrangeGrob(
                 Mis.age.gender.4yr,
                 Mis.age.gender.2yr,
                 Mis.age.gender.baseline,ncol=1),nrow=2,heights=c(0.35,10))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
Mis.fs.statusplot <- 
  rbind.data.frame(Misool.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,21),ncol=21,
                                           dimnames=list(NULL,
                                                         colnames(Misool.ContData.Techreport.status.PLOTFORMAT)[3:23])),
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=FS),
            label=Mis.statusplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=FS.ref),
            label=Mis.statusplot.asterisks$FS.ref,
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
Mis.ma.statusplot <- ggplot(data=Misool.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=MA),
            label=Mis.statusplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=MA.ref),
            label=Mis.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Misool.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Misool.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Misool.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
Mis.pa.statusplot <- ggplot(data=Misool.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=PA),
            label=Mis.statusplot.asterisks$PA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=PA.ref),
            label=Mis.statusplot.asterisks$PA.ref,
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
Mis.mt.statusplot <- ggplot(data=Misool.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=MT+(0.05*MT)),
            label=Mis.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=MT.ref),
            label=Mis.statusplot.asterisks$MT.ref,
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
Mis.se.statusplot <- ggplot(data=Misool.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=SE),
            label=Mis.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=SE.ref),
            label=Mis.statusplot.asterisks$SE.ref,
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
Mis.time.statusplot <- ggplot(data=Misool.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=Time),
            label=Mis.statusplot.asterisks$Time,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=Time.ref),
            label=Mis.statusplot.asterisks$Time.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Misool.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Misool.ContData.Techreport.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Misool.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Time"] + plot.theme

# - DAYS UNWELL
Mis.unwell.statusplot <- ggplot(data=Misool.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,
                y=Unwell),
            label=Mis.statusplot.asterisks$Unwell,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Mis.statusplot.sigpos,
            aes(x=SettlementName,y=Unwell.ref),
            label=Mis.statusplot.asterisks$Unwell.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Misool.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Misool.ContData.Techreport.status.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Misool.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.theme

# - NUMBER UNIQUE ETHNIC GROUPS
Mis.ethnic.statusplot <- ggplot(data=Misool.PropData.Techreport.status.PLOTFORMAT,
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
                     limits=c(0,max(Misool.PropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T)+
                                0.03*max(Misool.PropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  coord_flip() + Statusplot.labs["Ethnicity"] + plot.theme


# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Mis.gender.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.religion.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.primaryocc.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.freqfish.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.freqsellfish.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.incfish.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.fishtech.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.childfs.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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
Mis.proteinfish.statusplot <- 
  melt(Misool.PropData.Techreport.status.PLOTFORMAT,
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TREND PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 4.1 Continuous data plots ----

# - FOOD SECURITY 
Mis.fs.trendplot <- 
  ggplot(Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),]) +
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["FS"] + theme(axis.ticks=element_blank(),
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
Mis.ma.trendplot <- 
  ggplot(data=Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Misool.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Misool.TrendContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Misool.TrendContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
Mis.pa.trendplot <- 
  ggplot(data=Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["PA"] + plot.theme

# - MARINE TENURE
Mis.mt.trendplot <- 
  ggplot(data=Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["MT"] + plot.theme

# - SCHOOL ENROLLMENT
Mis.se.trendplot <- 
  ggplot(data=Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["SE"] + plot.theme

# - TIME TO MARKET
Mis.time.trendplot <- 
  ggplot(data=Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Misool.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Misool.TrendContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Misool.TrendContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["Time"] + plot.theme

# - DAYS UNWELL
Mis.unwell.trendplot <- 
  ggplot(data=Misool.TrendContData.Techreport.PLOTFORMAT
         [!is.na(Misool.TrendContData.Techreport.PLOTFORMAT$MonitoringYear),],
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
                     limits=c(0,max(Misool.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Misool.TrendContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Misool.TrendContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  coord_flip() + Mis.trendplot.labs["Unwell"] + plot.theme


# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Mis.gender.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
       id.vars="MonitoringYear",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=MonitoringYear,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.65,
           size=0.15,
           colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=c("Female","Male")) +
  coord_flip() + Mis.trendplot.labs["Gender"] + plot.theme + plot.guides.techreport

# - RELIGION
Mis.religion.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=c("Other","Muslim","Christian")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["Religion"] + 
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
Mis.primaryocc.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=c("Other","Other Wage Labor","Tourism",
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
Mis.freqfish.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Mis.freqsellfish.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=c("More than a few times per week","A few times per week",
                             "A few times per month","A few times per six months",
                             "Once every six months")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Mis.incfish.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Mis.fishtech.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=c("Mobile line","Stationary line",
                             "Mobile net","Stationary net","Fishing by hand")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Mis.childfs.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=c("Evidence of child hunger","No evidence of child hunger")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["ChildFS"] + plot.guides.techreport

# - PROTEIN FROM FISH
Mis.proteinfish.trendplot <- 
  melt(Misool.TrendPropData.Techreport.PLOTFORMAT,
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
  scale_x_discrete(labels=Mis.trendplot.monitoryear.labs) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=c("All","Most","About half","Some","None")) +
  coord_flip() + plot.theme + Mis.trendplot.labs["Protein"] + plot.guides.techreport


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

Mis.fs.annexplot <- 
  rbind.data.frame(Misool.AnnexContData.Techreport.PLOTFORMAT,
                   cbind.data.frame(matrix(rep(NA,17),ncol=17,
                                           dimnames=list(NULL,
                                                         colnames(Misool.AnnexContData.Techreport.PLOTFORMAT[1:17]))),
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(Mis.annexplot.settnames[,"FS"]," "),
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

Mis.ma.annexplot <- 
  ggplot(data=Misool.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Mis.annexplot.settnames[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Misool.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Misool.AnnexContData.Techreport.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Misool.AnnexContData.Techreport.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["MA"] + plot.guides.techreport + plot.theme


# ---- 5.3 Place attachment -----

Mis.pa.annexplot <- 
  ggplot(data=Misool.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Mis.annexplot.settnames[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["PA"] + plot.guides.techreport + plot.theme


# ---- 5.4 Marine tenure -----

Mis.mt.annexplot <- 
  ggplot(data=Misool.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Mis.annexplot.settnames[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs["MT"] + plot.guides.techreport + plot.theme


# ---- 5.5 School enrollment -----

Mis.se.annexplot <- 
  ggplot(data=Misool.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Mis.annexplot.settnames[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + Statusplot.labs["SE"] + plot.guides.techreport + plot.theme


# ---- 5.6 Time to market -----
Mis.time.annexplot <- 
  ggplot(data=Misool.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Mis.annexplot.settnames[,"Time"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Misool.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Misool.AnnexContData.Techreport.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Misool.AnnexContData.Techreport.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Time"] + plot.guides.techreport + plot.theme


# ---- 5.7 Days unwell -----

Mis.unwell.annexplot <- 
  ggplot(data=Misool.AnnexContData.Techreport.PLOTFORMAT,
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
                     labels=Mis.trendplot.monitoryear.labs,
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Mis.annexplot.settnames[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Misool.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Misool.AnnexContData.Techreport.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Misool.AnnexContData.Techreport.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + Statusplot.labs["Unwell"] + plot.guides.techreport + plot.theme


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


dir.create(paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Misool/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("2_Social/FlatDataFiles/BHS/TechReportOutput/Misool/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

# ---- 6.1 Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.fs.statusplot)
dev.off()

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.fs.trendplot)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.fs.annexplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.ma.statusplot)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.ma.trendplot)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.ma.annexplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.pa.statusplot)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.pa.trendplot)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.pa.annexplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.mt.statusplot)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.mt.trendplot)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.mt.annexplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.se.statusplot)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.se.trendplot)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.se.annexplot)
dev.off()


# ---- 6.6 Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.time.statusplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.time.trendplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.time.annexplot)
dev.off()


# ---- 6.7 Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.unwell.statusplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.unwell.trendplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Mis.unwell.annexplot)
dev.off()


# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.gender.statusplot)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.gender.trendplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.religion.statusplot)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.religion.trendplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.primaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.primaryocc.trendplot)
dev.off()


# ---- 6.11 Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.freqfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.freqfish.trendplot)
dev.off()


# ---- 6.12 Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.freqsellfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.freqsellfish.trendplot)
dev.off()


# ---- 6.13 Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.incfish.statusplot)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.incfish.trendplot)
dev.off()


# ---- 6.14 Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.fishtech.statusplot)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.fishtech.trendplot)
dev.off()


# ---- 6.15 Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.childfs.statusplot)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.childfs.trendplot)
dev.off()

# ---- 6.16 Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.proteinfish.statusplot)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.proteinfish.trendplot)
dev.off()


# ---- 6.17 Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(Mis.age.gender.plot)
dev.off()


# ---- 6.18 Number ethnic groups ----

png(paste(FigureFileName,"Num.Ethnic.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Mis.ethnic.statusplot)
dev.off()



# ---- Remove all plot objects from environment ----
rm(median.setts.Mis,Mis.statusplot.asterisks,Mis.statusplot.sigpos,
   Mis.trendplot.monitoryear.labs,Mis.conttrendplot.ylabs,Mis.proptrendplot.ylabs,
   Mis.trendplot.labs,Mis.annexplot.settnames,
   Mis.age.gender.baseline,Mis.age.gender.2yr,Mis.age.gender.4yr,
   Mis.agegender.legend.plot,Mis.agegender.legend,Mis.age.gender.plot,
   Mis.fs.statusplot,Mis.fs.trendplot,Mis.fs.annexplot,
   Mis.ma.statusplot,Mis.ma.trendplot,Mis.ma.annexplot,
   Mis.pa.statusplot,Mis.pa.trendplot,Mis.pa.annexplot,
   Mis.mt.statusplot,Mis.mt.trendplot,Mis.mt.annexplot,
   Mis.se.statusplot,Mis.se.trendplot,Mis.se.annexplot,
   Mis.time.statusplot,Mis.time.trendplot,Mis.time.annexplot,
   Mis.unwell.statusplot,Mis.unwell.trendplot,Mis.unwell.annexplot,
   Mis.gender.statusplot,Mis.gender.trendplot,Mis.religion.statusplot,
   Mis.religion.trendplot,Mis.primaryocc.statusplot,Mis.primaryocc.trendplot,
   Mis.freqfish.statusplot,Mis.freqsellfish.statusplot,Mis.freqsellfish.trendplot,
   Mis.incfish.statusplot,Mis.incfish.trendplot,Mis.fishtech.statusplot,
   Mis.fishtech.trendplot,Mis.childfs.statusplot,Mis.childfs.trendplot,
   Mis.proteinfish.statusplot,Mis.proteinfish.trendplot,Mis.ethnic.statusplot)

# ---- Remove all tech report datasets from environment ----
rm(Misool.AgeGender,
   Misool.ContData.Techreport.status.PLOTFORMAT,
   Misool.PropData.Techreport.status.PLOTFORMAT,
   Misool.TrendContData.Techreport.PLOTFORMAT,Misool.TrendPropData.Techreport.PLOTFORMAT,
   Misool.AnnexContData.Techreport.PLOTFORMAT,Misool.AnnexPropData.Techreport.PLOTFORMAT,
   sigvals.Mis,trend.sigvals.Mis,annex.sigvals.Mis,propdata.trend.test.Mis,
   dist.Mis.FS,dist.Mis.MA,dist.Mis.PA,dist.Mis.MT,dist.Mis.SE,dist.Mis.TimeMarket,dist.Mis.DaysUnwell)
