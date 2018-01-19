# 
# code:  Alor Technical Report Plots
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/TechnicalReports/SBS/Plots
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2017
# modified: 
# 
# 
# ---- inputs ----
#  1) Dependencies: Function_define_asteriskplotting.R
#  2) Source Alor.TechReport.Datasets.R 
#     - Dependencies: Alor.TechReport.SigTests.R
#                     SBS_MPA_Mystery.R
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


# ---- 1.1 Source data sets from "Alor.TechReport.Datasets.R" ----

source("2_Social/TechnicalReports/SBS/Datasets/Alor.TechReport.Datasets.R")


# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

Alor.statusplot.asterisks <- 
  define.statusplot.asterisks(Alor.ContData.Techreport.status.PLOTFORMAT[,c("SettlementName","FS.pval",
                                                                               "MA.pval","PA.pval",
                                                                               "MT.pval","SE.pval",
                                                                               "Time.pval","Unwell.pval")])
Alor.statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Alor.ContData.Techreport.status.PLOTFORMAT,
                                 Alor.statusplot.asterisks)  



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Baseline ----

Alor.age.gender.baseline <- 
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
                             "Male.Baseline"=alpha("#253494",0.95))) +
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="Population distribution (% of individuals by gender)")


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
                                    matrix(rep(NA,21),ncol=21,
                                           dimnames=list(NULL,
                                                         colnames(Alor.ContData.Techreport.status.PLOTFORMAT)[3:23])),
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

# - PRIMARY OCCUPATION
Alor.primaryocc.statusplot <- 
  melt(Alor.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.PrimaryOcc.Other","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.Tourism","Percent.PrimaryOcc.Fish",
                                               "Percent.PrimaryOcc.HarvestForest","Percent.PrimaryOcc.Farm")) %>%
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
                             "Fishing","Harvest Forest Products","Farming")) +
  coord_flip() + plot.theme + Statusplot.labs["PrimaryOcc"] + plot.guides.techreport

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
  coord_flip() + plot.theme + Statusplot.labs["Protein"] + plot.guides.techreport


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

dir.create(paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Alor/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Alor/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

# ---- 6.1 Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.fs.statusplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.ma.statusplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.pa.statusplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.mt.statusplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.se.statusplot)
dev.off()


# ---- 6.6 Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.time.statusplot)
dev.off()


# ---- 6.7 Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.unwell.statusplot)
dev.off()


# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.gender.statusplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.religion.statusplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.primaryocc.statusplot)
dev.off()


# ---- 6.11 Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.freqfish.statusplot)
dev.off()


# ---- 6.12 Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.freqsellfish.statusplot)
dev.off()


# ---- 6.13 Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.incfish.statusplot)
dev.off()


# ---- 6.14 Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.fishtech.statusplot)
dev.off()


# ---- 6.15 Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.childfs.statusplot)
dev.off()


# ---- 6.16 Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Alor.proteinfish.statusplot)
dev.off()


# ---- 6.17 Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=6,width=6,res=400)
grid.newpage()
grid.draw(Alor.age.gender.baseline)
dev.off()





# ---- Remove all plot objects from environment ----
rm(median.setts.Alor,Alor.statusplot.asterisks,Alor.statusplot.sigpos,
   Alor.trendplot.monitoryear.labs,Alor.conttrendplot.ylabs,Alor.proptrendplot.ylabs,
   Alor.trendplot.labs,Alor.annexplot.settnames,
   Alor.age.gender.baseline,Alor.age.gender.2yr,Alor.age.gender.4yr,
   Alor.agegender.legend.plot,Alor.agegender.legend,Alor.age.gender.plot,
   Alor.fs.statusplot,Alor.fs.trendplot,Alor.fs.annexplot,
   Alor.ma.statusplot,Alor.ma.trendplot,Alor.ma.annexplot,
   Alor.pa.statusplot,Alor.pa.trendplot,Alor.pa.annexplot,
   Alor.mt.statusplot,Alor.mt.trendplot,Alor.mt.annexplot,
   Alor.se.statusplot,Alor.se.trendplot,Alor.se.annexplot,
   Alor.time.statusplot,Alor.time.trendplot,Alor.time.annexplot,
   Alor.unwell.statusplot,Alor.unwell.trendplot,Alor.unwell.annexplot,
   Alor.gender.statusplot,Alor.gender.trendplot,Alor.religion.statusplot,
   Alor.religion.trendplot,Alor.primaryocc.statusplot,Alor.primaryocc.trendplot,
   Alor.freqfish.statusplot,Alor.freqsellfish.statusplot,Alor.freqsellfish.trendplot,
   Alor.incfish.statusplot,Alor.incfish.trendplot,Alor.fishtech.statusplot,
   Alor.fishtech.trendplot,Alor.childfs.statusplot,Alor.childfs.trendplot,
   Alor.proteinfish.statusplot,Alor.proteinfish.trendplot,Alor.ethnic.statusplot)

# ---- Remove all tech report datasets from environment ----
rm(Alor.AgeGender,
   Alor.ContData.Techreport.status.PLOTFORMAT,
   Alor.PropData.Techreport.status.PLOTFORMAT,
   Alor.TrendContData.Techreport.PLOTFORMAT,Alor.TrendPropData.Techreport.PLOTFORMAT,
   Alor.AnnexContData.Techreport.PLOTFORMAT,Alor.AnnexPropData.Techreport.PLOTFORMAT,
   sigvals.Alor,trend.sigvals.Alor,annex.sigvals.Alor,propdata.trend.test.Alor,
   dist.Alor.FS,dist.Alor.MA,dist.Alor.PA,dist.Alor.MT,dist.Alor.SE,dist.Alor.TimeMarket,dist.Alor.DaysUnwell)
