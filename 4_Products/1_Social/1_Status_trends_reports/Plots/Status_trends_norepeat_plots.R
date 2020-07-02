# 
# code:  Status Plots, for data with no repeat
# 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2017
# modified: October 2019
# 
# 
# ---- inputs ----
#  1) Dependencies: Function_define_asteriskplotting.R
#  2) Source Status_trends_onerepeat_plots.R 
# 
# ---- code sections ----
#  1) DEFINE MPA-SPECIFIC PLOTTING DATA FRAMES
#  2) AGE/GENDER PLOT 
#  3) STATUS PLOTS
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 



# ---- 2.1 Baseline ----

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
               arrangeGrob(Age.gender.Baseline,ncol=1),nrow=2,heights=c(0.35,10))


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
                   if(length(Sett.level.ContData.status.PLOTFORMAT$SettlementName)<25) {
                             data.frame(SettlementName="  ",
                              FSMean=NA,
                              FSErr=NA,
                              SettLevel="Dummy")
                     } else {
                       data.frame(SettlementName=c("  ","  "),
                                  FSMean=NA,
                                  FSErr=NA,
                                  SettLevel=c("Dummy","Dummy"))
                     }) %>%
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
  geom_text(aes(x=length(SettlementName),
                y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(SettlementName),
                y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(SettlementName),
                y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0,0.05,0)) +
  scale_x_discrete(expand=c(0.02,0.02,0.08,0.08)) +
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
  scale_y_continuous(expand=c(0,0,0.05,0),
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
  scale_y_continuous(expand=c(0,0,0.05,0),
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
  scale_y_continuous(expand=c(0,0,0.05,0),
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
  scale_y_continuous(expand=c(0,0,0.05,0),
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
  scale_y_continuous(expand=c(0,0,0.05,0),
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
  scale_y_continuous(expand=c(0,0,0.05,0),
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
  coord_flip() + plot.theme + Statusplot.labs["Gender"] +
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           nrow=1,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

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
                    labels=c("Other", "Aquaculture", "Tourism", "Extracting non-renewable marine resources",  "Other Wage Labor",
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
  coord_flip() + plot.theme + Statusplot.labs["ChildFS"] +
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           nrow=1,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

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
  coord_flip() + plot.theme + Statusplot.labs["FSCategorical"] + 
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           ncol=2,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

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
  coord_flip() + plot.theme + Statusplot.labs["MarineMember"] + 
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           nrow=1,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

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
  coord_flip() + plot.theme + Statusplot.labs["MarineAttendance"] + 
  guides(fill=guide_legend(label.vjust=0.5,
                           label.theme=element_text(size=rel(9),
                                                    angle=0,
                                                    colour="#505050",
                                                    lineheight=0.75),
                           direction="horizontal",
                           nrow=1,
                           title.position="left",
                           label.position="right",
                           keywidth=unit(0.75,"cm"),
                           keyheight=unit(0.5,"cm"),
                           reverse=T))

# - MARINE RESOUCE CONFLICT
SocialConflict.statusplot <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.GreatlyIncreased.SocConflict","Percent.Increased.SocConflict",
                                               "Percent.Same.SocConflict","Percent.Decreased.SocConflict",
                                               "Percent.GreatlyDecreased.SocConflict")) %>%
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
                    labels=c("Greatly increased","Increased","Neither increased nor decreased","Decreased","Greatly decreased")) +
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
                     limits=c(0,max(Sett.level.PropData.status.PLOTFORMAT$MarineContribution,na.rm=T) +
                                1.5* max(Sett.level.PropData.status.PLOTFORMAT$MarineContribution,na.rm=T)), 
                     labels = scales::comma) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MarineContribution"] + plot.theme

