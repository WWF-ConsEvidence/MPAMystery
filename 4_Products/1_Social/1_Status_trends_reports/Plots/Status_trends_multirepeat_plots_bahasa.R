# 
# code:  Status & Trend Plots, for data with multiple repeats - BAHASA
# 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2017
# modified: July 2020
# 
# 
# ---- inputs ----
#  1) Dependencies: Function_define_asteriskplotting.R; Function_plotthemes.R
#  2) Source Status_trends_onerepeat_datasets.R 
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

Statusplot.asterisks.bahasa <- 
  define.statusplot.asterisks.bahasa(Sett.level.ContData.status.PLOTFORMAT %>% 
                                select(SettlementName.bahasa, FS.pval, MA.pval, PA.pval, MT.pval, 
                                       SE.pval, TimeMarket.pval, Unwell.pval))

Statusplot.sigpos.bahasa <- 
  define.statusplot.asterisk.pos.bahasa(Sett.level.ContData.status.PLOTFORMAT,
                                 Statusplot.asterisks.bahasa)  

# ---- 1.3 Define MPA-specific plot labels, with significance asterisks ----

Monitoryear.labs.bahasa <- 
  define.year.monitoryear.column.bahasa(MPA.Level.Means)


Conttrendplot.ylabs.bahasa <- 
  define.conttrendplot.ylabels.withasterisks.bahasa(MPA.level.ContData.trend.PLOTFORMAT
                                             [MPA.level.ContData.trend.PLOTFORMAT$MonitoringYear=="p.value",
                                               c("FSMean","MAMean","PAMean","MTMean",
                                                 "SEMean","TimeMarketMean","UnwellMean")])

Proptrendplot.ylabs.bahasa <- 
  define.proptrendplot.ylabels.withasterisks.bahasa(propdata.trend.test)


Trendplot.labs.bahasa <- list(FS=labs(y=as.character(Conttrendplot.ylabs.bahasa["FSMean"]),x=sett.names.bahasa[["MonYear"]]),
                       MA=labs(y=as.character(Conttrendplot.ylabs.bahasa["MAMean"]),x=sett.names.bahasa[["MonYear"]]),
                       PA=labs(y=as.character(Conttrendplot.ylabs.bahasa["PAMean"]),x=sett.names.bahasa[["MonYear"]]),
                       MT=labs(y=as.character(Conttrendplot.ylabs.bahasa["MTMean"]),x=sett.names.bahasa[["MonYear"]]),
                       SE=labs(y=as.character(Conttrendplot.ylabs.bahasa["SEMean"]),x=sett.names.bahasa[["MonYear"]]),
                       Time=labs(y=as.character(Conttrendplot.ylabs.bahasa["TimeMarketMean"]),x=sett.names.bahasa[["MonYear"]]),
                       Unwell=labs(y=as.character(Conttrendplot.ylabs.bahasa["UnwellMean"]),
                                   x=sett.names.bahasa[["MonYear"]]),
                       Gender=labs(y="Jenis kelamin (% kepala rumah tangga)",x=sett.names.bahasa[["MonYear"]]),
                       Religion=labs(y="Agama (% kepala rumah tangga)",x=sett.names.bahasa[["MonYear"]]),
                       PrimaryOcc=labs(y=as.character(Proptrendplot.ylabs.bahasa["PrimaryOcc"]),x=sett.names.bahasa[["MonYear"]]),
                       FreqFish=labs(y=as.character(Proptrendplot.ylabs.bahasa["FreqFish"]),x=sett.names.bahasa[["MonYear"]]),
                       FreqSellFish=labs(y=as.character(Proptrendplot.ylabs.bahasa["SellFish"]),x=sett.names.bahasa[["MonYear"]]),
                       IncFish=labs(y=as.character(Proptrendplot.ylabs.bahasa["IncFish"]),x=sett.names.bahasa[["MonYear"]]),
                       FishTech=labs(y=as.character(Proptrendplot.ylabs.bahasa["FishTech"]),x=sett.names.bahasa[["MonYear"]]),
                       ChildFS=labs(y=as.character(Proptrendplot.ylabs.bahasa["child"]),x=sett.names.bahasa[["MonYear"]]),
                       Protein=labs(y=as.character(Proptrendplot.ylabs.bahasa["Protein"]),x=sett.names.bahasa[["MonYear"]]),
                       EconStatus=labs(y=as.character(Proptrendplot.ylabs.bahasa["EconStatus"]),x=sett.names.bahasa[["MonYear"]]),
                       NumLocalThreats=labs(y=as.character(Proptrendplot.ylabs.bahasa["NumLocalThreats"]),x=sett.names.bahasa[["MonYear"]]),
                       SecondaryOcc=labs(y=as.character(Proptrendplot.ylabs.bahasa["SecondaryOcc"]),x=sett.names.bahasa[["MonYear"]]),
                       OccDiverse=labs(y=as.character(Proptrendplot.ylabs.bahasa["OccDiverse"]),x=sett.names.bahasa[["MonYear"]]))

Annexplot.settnames.bahasa <- 
  define.annexplot.settname.labels(annex.sigvals.bahasa)

Annexplot.settnames.bahasa[3,] <- rep("",length(Annexplot.settnames.bahasa[3,]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Baseline ----

Age.gender.Baseline.bahasa <- 
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
  geom_text(aes(x=19,y=-7,label=Monitoryear.labs.bahasa$Label[1]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=legend.labs.bahasa[["AgeGender"]],
                    values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
                             "Male.Baseline"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + Statusplot.labs.bahasa[["AgeGender"]]


# ---- 2.2 Repeat One ----

Age.gender.RepeatOne.bahasa <- 
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
  geom_text(aes(x=19,y=-7,label=Monitoryear.labs.bahasa$Label[2]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=legend.labs.bahasa[["AgeGender"]],
                    values=c("Female.RepeatOne"=alpha("#7FCDBB",0.95),
                             "Male.RepeatOne"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + 
  if(num.years==4) { Statusplot.labs.bahasa[["AgeGender"]] 
    } else { Statusplot.labs.bahasa[["AgeGenderUpperPlot"]] }


# ---- 2.3 Repeat Two ----

Age.gender.RepeatTwo.bahasa <- 
  if(num.years>2) {
    melt(AgeGender,id.vars="AgeCat",measure.vars=c("Female.RepeatTwo","Male.RepeatTwo")) %>%
      ggplot() +
      geom_bar(aes(x=AgeCat,
                   y=value,
                   fill=variable),
               stat="identity",
               width=0.75,
               colour="#505050",
               size=0.15,
               show.legend=F) +
      geom_text(aes(x=19,y=-7,label=Monitoryear.labs.bahasa$Label[3]),
                size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
      scale_y_continuous(expand=c(0,0),
                         limits=c(-10,10),
                         labels=abs(seq(-10,10,5))) +
      scale_fill_manual(name="",
                        labels=legend.labs.bahasa[["AgeGender"]],
                        values=c("Female.RepeatTwo"=alpha("#7FCDBB",0.95),
                                 "Male.RepeatTwo"=alpha("#253494",0.95)))+ 
      coord_flip() + age.gender.plot.theme + plot.guides.techreport + Statusplot.labs.bahasa[["AgeGenderUpperPlot"]]
  } else { NA }


# ---- 2.4 Repeat Three ----

Age.gender.RepeatThree.bahasa <- 
  if(num.years>3) {
  melt(AgeGender,id.vars="AgeCat",measure.vars=c("Female.RepeatThree","Male.RepeatThree")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-7,label=Monitoryear.labs.bahasa$Label[4]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=legend.labs.bahasa[["AgeGender"]],
                    values=c("Female.RepeatThree"=alpha("#7FCDBB",0.95),
                             "Male.RepeatThree"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + Statusplot.labs.bahasa[["AgeGenderUpperPlot"]]
  } else { NA }


# ---- 2.5 Repeat Four ----

Age.gender.RepeatFour.bahasa <- 
  if(num.years>4) {
  melt(AgeGender,id.vars="AgeCat",measure.vars=c("Female.RepeatFour","Male.RepeatFour")) %>%
  ggplot() +
  geom_bar(aes(x=AgeCat,
               y=value,
               fill=variable),
           stat="identity",
           width=0.75,
           colour="#505050",
           size=0.15,
           show.legend=F) +
  geom_text(aes(x=19,y=-7,label=Monitoryear.labs.bahasa$Label[5]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10),
                     labels=abs(seq(-10,10,5))) +
  scale_fill_manual(name="",
                    labels=legend.labs.bahasa[["AgeGender"]],
                    values=c("Female.RepeatFour"=alpha("#7FCDBB",0.95),
                             "Male.RepeatFour"=alpha("#253494",0.95)))+ 
  coord_flip() + age.gender.plot.theme + plot.guides.techreport + Statusplot.labs.bahasa[["AgeGenderUpperPlot"]]
  } else { NA }
    
    
# ---- 2.6 Create legend ----

Age.gender.legend.plot.bahasa <-
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
                    labels=legend.labs.bahasa[["AgeGender"]]) +
  coord_flip() + plot.guides.techreport + theme(legend.justification="right")

Age.gender.legend.bahasa <- g_legend(Age.gender.legend.plot.bahasa)


# ---- 2.4 Arrange grob ----

Age.gender.plot.bahasa <- 
  if(num.years==2) {
    grid.arrange(Age.gender.legend.bahasa,
                 arrangeGrob(
                   Age.gender.RepeatOne.bahasa,
                   Age.gender.Baseline.bahasa,ncol=1),nrow=2,heights=c(0.35,10))
  } else { if(num.years==3) {
    grid.arrange(Age.gender.legend.bahasa,
                 arrangeGrob(
                   Age.gender.RepeatTwo.bahasa,
                   Age.gender.RepeatOne.bahasa,
                   Age.gender.Baseline.bahasa,ncol=1),nrow=2,heights=c(0.35,10))
  } else { if(num.years==4) {
    grid.arrange(Age.gender.legend.bahasa,
                 arrangeGrob(
                   Age.gender.RepeatThree.bahasa,
                   Age.gender.Repeat.Two.bahasa,
                   Age.gender.RepeatOne.bahasa,
                   Age.gender.Baseline.bahasa,ncol=2),nrow=2,heights=c(0.35,10))
  } else { if(num.years==5) {
    grid.arrange(Age.gender.legend.bahasa,
                 arrangeGrob(
                   Age.gender.RepeatFour.bahasa,
                   Age.gender.RepeatThree.bahasa,
                   Age.gender.Repeat.Two.bahasa,
                   Age.gender.RepeatOne.bahasa,
                   Age.gender.Baseline.bahasa,ncol=2),nrow=2,heights=c(0.35,10))
  } else { NA }}}}



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
FS.statusplot.bahasa <- 
  rbind.data.frame(Sett.level.ContData.status.PLOTFORMAT[,c("SettlementName.bahasa","FSMean","FSErr","SettLevel")],
                   data.frame(SettlementName.bahasa="  ",
                              FSMean=NA,
                              FSErr=NA,
                              SettLevel="Dummy")) %>%
  ggplot(aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=FS),
            label=Statusplot.asterisks.bahasa$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=FS.ref),
            label=Statusplot.asterisks.bahasa$FS.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  geom_text(aes(x=length(SettlementName.bahasa),y=(0.5*(6.06-4.02))+4.02,label=legend.labs.bahasa[["FoodSecure"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(SettlementName.bahasa),y=(0.5*(4.02-1.56))+1.56,label=legend.labs.bahasa[["NoHunger"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(SettlementName.bahasa),y=0.5*1.56,label=legend.labs.bahasa[["YesHunger"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs.bahasa["FS"] + theme(axis.ticks=element_blank(),
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
MA.statusplot.bahasa <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                        aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=MA),
            label=Statusplot.asterisks.bahasa$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=MA.ref),
            label=Statusplot.asterisks.bahasa$MA.ref,
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
  coord_flip() + Statusplot.labs.bahasa["MA"] + plot.theme


# - PLACE ATTACHMENT
PA.statusplot.bahasa <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                        aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=PA),
            label=Statusplot.asterisks.bahasa$PA,
            nudge_x=-0.07,
            nudge_y=0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=PA.ref),
            label=Statusplot.asterisks.bahasa$PA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs.bahasa["PA"] + plot.theme


# - MARINE TENURE
MT.statusplot.bahasa <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                        aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=MT+(0.05*MT)),
            label=Statusplot.asterisks.bahasa$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=MT.ref),
            label=Statusplot.asterisks.bahasa$MT.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs.bahasa["MT"] + plot.theme

# - SCHOOL ENROLLMENT
SE.statusplot.bahasa <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                        aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=SE),
            label=Statusplot.asterisks.bahasa$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=SE.ref),
            label=Statusplot.asterisks.bahasa$SE.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1.1)) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs.bahasa["SE"] + plot.theme


# - TIME TO MARKET
Time.statusplot.bahasa <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                          aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=TimeMarket),
            label=Statusplot.asterisks.bahasa$TimeMarket,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=TimeMarket.ref),
            label=Statusplot.asterisks.bahasa$TimeMarket.ref,
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
  coord_flip() + Statusplot.labs.bahasa["Time"] + plot.theme


# - DAYS UNWELL
Unwell.statusplot.bahasa <- ggplot(data=Sett.level.ContData.status.PLOTFORMAT,
                            aes(x=SettlementName.bahasa)) +
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
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,
                y=Unwell),
            label=Statusplot.asterisks.bahasa$Unwell,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Statusplot.sigpos.bahasa,
            aes(x=SettlementName.bahasa,y=Unwell.ref),
            label=Statusplot.asterisks.bahasa$Unwell.ref,
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
  coord_flip() + Statusplot.labs.bahasa["Unwell"] + plot.theme


# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Gender.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=SettlementName.bahasa,
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
                    labels=legend.labs.bahasa[["AgeGender"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["Gender"] +
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
Religion.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=SettlementName.bahasa,
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
                    labels=legend.labs.bahasa[["Religion"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["Religion"] +
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
Primaryocc.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Percent.PrimaryOcc.Other",  
                                               "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
                                               "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
                                               "Percent.PrimaryOcc.HarvestForest", 
                                               "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["PrimaryOcc"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["PrimaryOcc"] + plot.guides.techreport

# - FISHING FREQUENCY
Freqfish.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                               "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                               "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["FreqFish"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Freqsellfish.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                               "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                               "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["FreqSellFish"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Incfish.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                               "Prop.IncFish.Half","Prop.IncFish.Some",
                                               "Prop.IncFish.None")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["IncFish"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Fishtech.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                               "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                               "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["FishTech"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Childfs.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=SettlementName.bahasa,
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
                    labels=legend.labs.bahasa[["ChildFS"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["ChildFS"] +
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
Proteinfish.statusplot.bahasa <- 
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                               "ProteinFish.Half","ProteinFish.Some",
                                               "ProteinFish.None")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["Protein"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["FishProtein"] + plot.guides.techreport

# - CATEGORICAL FOOD SECURITY
FSCategorical.statusplot.bahasa <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Percent.FoodInsecure.YesHunger", "Percent.FoodInsecure.NoHunger", "Percent.FoodSecure")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["FSCategorical"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["FSCategorical"] + 
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
EconStatus.statusplot.bahasa <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Econ.Status.Much.Better","Econ.Status.Slightly.Better",
                                               "Econ.Status.Neutral","Econ.Status.Slighly.Worse",
                                               "Econ.Status.Much.Worse")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["EconStatus"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["EconStatus"] + plot.guides.techreport

# - MEMBER OF MARINE RESOURCE ORGANIZATION
MarineMember.statusplot.bahasa <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("MarineMember.No","MarineMember.Yes")) %>%
  ggplot(aes(x=SettlementName.bahasa,
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
                    labels=legend.labs.bahasa[["MarineMember"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["MarineMember"] + 
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
MarineMeeting.statusplot.bahasa <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("MarineMeeting.No", "MarineMeeting.Yes")) %>%
  ggplot(aes(x=SettlementName.bahasa,
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
                    labels=legend.labs.bahasa[["MarineAttendance"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["MarineAttendance"] + 
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
SocialConflict.statusplot.bahasa <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Percent.GreatlyIncreased.SocConflict","Percent.Increased.SocConflict",
                                               "Percent.Same.SocConflict","Percent.Decreased.SocConflict",
                                               "Percent.GreatlyDecreased.SocConflict")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["SocialConflict"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["SocialConflict"] + plot.guides.techreport

# - NUMBER OF LOCAL THREATS
NumThreat.statusplot.bahasa <-
  melt(Sett.level.PropData.status.PLOTFORMAT,
       id.vars="SettlementName.bahasa",measure.vars=c("Threat.Minimum.Five","Threat.Four", "Threat.Three",
                                               "Threat.Two","Threat.One","Threat.None")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["NumLocalThreats"]]) +
  coord_flip() + plot.theme + Statusplot.labs.bahasa["NumLocalThreats"] + plot.guides.techreport

# - MARINE GROUP CONTRIBUTION
MarineContribution.statusplot.bahasa <- 
  ggplot(data=Sett.level.PropData.status.PLOTFORMAT,
         aes(x=SettlementName.bahasa)) +
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
  coord_flip() + Statusplot.labs.bahasa["MarineContribution"] + plot.theme


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TREND PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 4.1 Continuous data plots ----

# - FOOD SECURITY 
FS.trendplot.bahasa <- 
  ggplot(rbind.data.frame(MPA.level.ContData.trend.PLOTFORMAT %>% 
                            mutate(FSMean=ifelse(MonitoringYear=="p.value",NA,FSMean)) %>% 
                            select(MonitoringYear, Treatment, FSMean, FSErr, order),
                          data.frame(MonitoringYear="", Treatment=NA, FSMean=NA, FSErr=NA,
                                     order=max(MPA.level.ContData.trend.PLOTFORMAT$order)+1)),
         aes(x=factor(order))) +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_bar(aes(y=FSMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=FSMean-FSErr,
                    ymax=FSMean+FSErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_text(aes(x=length(unique(MonitoringYear))+0.46,y=(0.5*(6.06-4.02))+4.02,label=legend.labs.bahasa[["FoodSecure"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(MonitoringYear))+0.46,y=(0.5*(4.02-1.56))+1.56,label=legend.labs.bahasa[["NoHunger"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(MonitoringYear))+0.46,y=0.5*1.56,label=legend.labs.bahasa[["YesHunger"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=5.56,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=5.56,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_x_discrete(labels=c(MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)],"")) +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  coord_flip() + Trendplot.labs.bahasa["FS"] + theme(axis.ticks=element_blank(),
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
MA.trendplot.bahasa <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT %>% 
           mutate(MAMean=ifelse(MonitoringYear=="p.value",NA,MAMean)),
         aes(x=factor(order))) +
  geom_bar(aes(y=MAMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=MAMean-MAErr,
                    ymax=MAMean+MAErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=max(MAMean,na.rm=T),label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=max(MAMean,na.rm=T),label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.ContData.trend.PLOTFORMAT$MAMean,na.rm=T)+
                                max(MPA.level.ContData.trend.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(MPA.level.ContData.trend.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_x_discrete(labels=MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)]) +
  coord_flip() + Trendplot.labs.bahasa["MA"] + plot.theme

# - PLACE ATTACHMENT
PA.trendplot.bahasa <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT %>% 
           mutate(PAMean=ifelse(MonitoringYear=="p.value",NA,PAMean)),
         aes(x=factor(order))) +
  geom_bar(aes(y=PAMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=PAMean-PAErr,
                    ymax=PAMean+PAErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=4.6,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=4.6,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)]) +
  coord_flip() + Trendplot.labs.bahasa["PA"] + plot.theme

# - MARINE TENURE
MT.trendplot.bahasa <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT %>% 
           mutate(MTMean=ifelse(MonitoringYear=="p.value",NA,MTMean)),
         aes(x=factor(order))) +
  geom_bar(aes(y=MTMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=MTMean-MTErr,
                    ymax=MTMean+MTErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=4.6,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=4.6,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +  
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)]) +
  coord_flip() + Trendplot.labs.bahasa["MT"] + plot.theme

# - SCHOOL ENROLLMENT
SE.trendplot.bahasa <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT %>% 
           mutate(SEMean=ifelse(MonitoringYear=="p.value",NA,SEMean)),
         aes(x=factor(order))) +
  geom_bar(aes(y=SEMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=SEMean-SEErr,
                    ymax=SEMean+SEErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=0.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=0.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format(),
                     limits=c(0,1)) +
  scale_x_discrete(labels=MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)]) +
  coord_flip() + Trendplot.labs.bahasa["SE"] + plot.theme

# - TIME TO MARKET
Time.trendplot.bahasa <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT %>% 
           mutate(TimeMarketMean=ifelse(MonitoringYear=="p.value",NA,TimeMarketMean)),
         aes(x=factor(order))) +
  geom_bar(aes(y=TimeMarketMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=TimeMarketMean-TimeMarketErr,
                    ymax=TimeMarketMean+TimeMarketErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=max(TimeMarketMean)+max(TimeMarketErr)-0.06*max(TimeMarketMean),label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=max(TimeMarketMean)+max(TimeMarketErr)-0.06*max(TimeMarketMean),label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.ContData.trend.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(MPA.level.ContData.trend.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(MPA.level.ContData.trend.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  scale_x_discrete(labels=MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)]) +
  coord_flip() + Trendplot.labs.bahasa["Time"] + plot.theme

# - DAYS UNWELL
Unwell.trendplot.bahasa <- 
  ggplot(data=MPA.level.ContData.trend.PLOTFORMAT %>% 
           mutate(UnwellMean=ifelse(MonitoringYear=="p.value",NA,UnwellMean)),
         aes(x=factor(order))) +
  geom_bar(aes(y=UnwellMean,
               fill=Treatment),
           stat="identity",
           position=position_dodge(width=0.75),
           width=0.65,
           show.legend=F) +
  geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
                    ymax=UnwellMean+UnwellErr,
                    colour=Treatment),
                width=0.15,
                size=0.5,
                position=position_dodge(width=0.75),
                show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=max(UnwellMean)+max(UnwellErr)-0.06*max(UnwellMean),label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=max(UnwellMean)+max(UnwellErr)-0.06*max(UnwellMean),label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_fill_manual(values=fillcols.cont.trend) +
  scale_colour_manual(values=errcols.cont.trend) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.ContData.trend.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(MPA.level.ContData.trend.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(MPA.level.ContData.trend.PLOTFORMAT$UnwellMean,na.rm=T))) +
  scale_x_discrete(labels=MPA.level.ContData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.ContData.trend.PLOTFORMAT$order)]) +
  coord_flip() + Trendplot.labs.bahasa["Unwell"] + plot.theme


# ---- 4.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Gender.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("HHH.female","HHH.male")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Gender"]],
                    labels=legend.labs.bahasa[["AgeGender"]]) +
  coord_flip() + Trendplot.labs.bahasa["Gender"] + plot.theme + 
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
Religion.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Percent.Rel.Other","Percent.Rel.Muslim","Percent.Rel.Christian")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Religion"]],
                    labels=legend.labs.bahasa[["Religion"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["Religion"] + 
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
Primaryocc.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Percent.PrimaryOcc.Other",  
                                      "Percent.PrimaryOcc.Aquaculture","Percent.PrimaryOcc.Tourism",
                                      "Percent.PrimaryOcc.Extraction","Percent.PrimaryOcc.WageLabor",
                                      "Percent.PrimaryOcc.HarvestForest", 
                                      "Percent.PrimaryOcc.Fish","Percent.PrimaryOcc.Farm")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PrimaryOcc"]],
                    labels=legend.labs.bahasa[["PrimaryOcc"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["PrimaryOcc"] + plot.guides.techreport 

# - FISHING FREQUENCY
Freqfish.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Prop.Fish.MoreFewTimesWk","Prop.Fish.FewTimesPerWk",
                                      "Prop.Fish.FewTimesPerMo","Prop.Fish.FewTimesPer6Mo",
                                      "Prop.Fish.AlmostNever")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqFish"]],
                    labels=legend.labs.bahasa[["FreqFish"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["FreqFish"] + plot.guides.techreport

# - SELL FISH FREQUENCY
Freqsellfish.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Prop.SellFish.MoreFewTimesWk","Prop.SellFish.FewTimesPerWk",
                                      "Prop.SellFish.FewTimesPerMo","Prop.SellFish.FewTimesPer6Mo",
                                      "Prop.SellFish.AlmostNever")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FreqSellFish"]],
                    labels=legend.labs.bahasa[["FreqSellFish"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["FreqSellFish"] + plot.guides.techreport

# - INCOME FROM FISHING
Incfish.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Prop.IncFish.All","Prop.IncFish.Most",
                                      "Prop.IncFish.Half","Prop.IncFish.Some",
                                      "Prop.IncFish.None")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["IncFish"]],
                    labels=legend.labs.bahasa[["IncFish"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["IncFish"] + plot.guides.techreport

# - FISHING TECHNIQUE
Fishtech.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Prop.FishTech.MobileLine","Prop.FishTech.StatLine",
                                      "Prop.FishTech.MobileNet","Prop.FishTech.StatNet",
                                      "Prop.FishTech.ByHand")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["FishTech"]],
                    labels=legend.labs.bahasa[["FishTech"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["FishTech"] + plot.guides.techreport

# - CHILDHOOD FOOD SECURITY
Childfs.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Child.FS.yes","Child.FS.no")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ChildFS"]],
                    labels=legend.labs.bahasa[["ChildFS"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["ChildFS"] + 
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
Proteinfish.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("ProteinFish.All","ProteinFish.Most",
                                      "ProteinFish.Half","ProteinFish.Some",
                                      "ProteinFish.None")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Protein"]],
                    labels=legend.labs.bahasa[["Protein"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["Protein"] + plot.guides.techreport

# - ECONOMIC STATUS 
EconStatus.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Econ.Status.Much.Better","Econ.Status.Slightly.Better",
                                      "Econ.Status.Neutral","Econ.Status.Slighly.Worse",
                                      "Econ.Status.Much.Worse")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["EconStatus"]],
                    labels=legend.labs.bahasa[["EconStatus"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["EconStatus"] + plot.guides.techreport

# - NUMBER OF LOCAL THREATS
NumThreat.trendplot.bahasa <- 
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars="order",measure.vars=c("Threat.Minimum.Five","Threat.Four", "Threat.Three",
                                      "Threat.Two","Threat.One","Threat.None")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["NumLocalThreats"]],
                    labels=legend.labs.bahasa[["NumLocalThreats"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["NumLocalThreats"] + plot.guides.techreport

# - SECONDARY OCCUPATION
Secondaryocc.trendplot.bahasa <-
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars=c("order"),measure.vars=c("Percent.SecondaryOcc.Other",  
                                         "Percent.SecondaryOcc.Aquaculture","Percent.SecondaryOcc.Tourism",
                                         "Percent.SecondaryOcc.Extraction","Percent.SecondaryOcc.WageLabor",
                                         "Percent.SecondaryOcc.HarvestForest", 
                                         "Percent.SecondaryOcc.Fish","Percent.SecondaryOcc.Farm")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["SecondaryOcc"]],
                    labels=legend.labs.bahasa[["SecondaryOcc"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["SecondaryOcc"] + plot.guides.techreport 

# - OCCUPATIONAL DIVERSITY
OccDiverse.trendplot.bahasa <-
  melt(MPA.level.PropData.trend.PLOTFORMAT,
       id.vars=c("order"),measure.vars=c("Percent.MultipleOcc.Diverse","Percent.OneOcc.Diverse")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=.91,label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=.91,label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["OccDiverse"]],
                    labels=legend.labs.bahasa[["OccDiverse"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa["OccDiverse"] + 
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

# - MARINE GROUP CONTRIBUTION
MarineContribution.trendplot.bahasa <- 
  ggplot(data=MPA.level.PropData.trend.PLOTFORMAT%>%mutate(Treatment=ifelse(grepl("MPA",MPAName),"MPA",ifelse(grepl("Control",MPAName),"Control",NA)))) +
  geom_bar(aes(x=as.factor(order),
               y=MarineContribution,
               group=MonitoringYear,
               fill=Treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_vline(aes(xintercept=num.years+1),size=0.25,colour="#505050") +
  geom_text(aes(x=num.years+1.25,y=max(MarineContribution,na.rm=T)+0.06*max(MarineContribution,na.rm=T),label=sett.names.bahasa[["Treatment"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=num.years+0.95,y=max(MarineContribution,na.rm=T)+0.06*max(MarineContribution,na.rm=T),label=sett.names.bahasa[["Control"]],fontface=2),
            size=rel(2.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(MPA.level.PropData.trend.PLOTFORMAT$MarineContribution,na.rm=T) +
                                0.5*max(MPA.level.PropData.trend.PLOTFORMAT$MarineContribution,na.rm=T)), 
                     labels = scales::comma) +
  scale_x_discrete(labels=MPA.level.PropData.trend.PLOTFORMAT$Label.bahasa[order(MPA.level.PropData.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(values=fillcols.cont.trend) +
  coord_flip() + Statusplot.labs.bahasa["MarineContribution"] + plot.theme


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: ANNEX PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Food security -----

FS.annexplot.bahasa <- 
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
  geom_text(aes(x=length(unique(SettlementName)),y=(0.5*(6.06-4.02))+4.02,label=legend.labs.bahasa[["FoodSecure"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(SettlementName)),y=(0.5*(4.02-1.56))+1.56,label=legend.labs.bahasa[["NoHunger"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=length(unique(SettlementName)),y=0.5*1.56,label=legend.labs.bahasa[["YesHunger"]]),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_alpha_manual(name="",
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=c(Annexplot.settnames.bahasa[,"FS"]," "),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  coord_flip() + Statusplot.labs.bahasa["FS"] + plot.guides.techreport + theme(axis.ticks=element_blank(),
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

MA.annexplot.bahasa <- 
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
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames.bahasa[,"MA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.annex.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Sett.level.ContData.annex.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.annex.PLOTFORMAT$MAMean,na.rm=T))) +
  coord_flip() + Statusplot.labs.bahasa["MA"] + plot.guides.techreport + plot.theme


# ---- 5.3 Place attachment -----

PA.annexplot.bahasa <- 
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
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames.bahasa[,"PA"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs.bahasa["PA"] + plot.guides.techreport + plot.theme


# ---- 5.4 Marine tenure -----

MT.annexplot.bahasa <- 
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
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames.bahasa[,"MT"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  coord_flip() + Statusplot.labs.bahasa["MT"] + plot.guides.techreport + plot.theme


# ---- 5.5 School enrollment -----

SE.annexplot.bahasa <- 
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
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames.bahasa[,"SE"]) +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  coord_flip() + Statusplot.labs.bahasa["SE"] + plot.guides.techreport + plot.theme


# ---- 5.6 Time to market -----

Time.annexplot.bahasa <- 
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
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames.bahasa[,"TimeMarket"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.annex.PLOTFORMAT$TimeMarketMean,na.rm=T)+
                                max(Sett.level.ContData.annex.PLOTFORMAT$TimeMarketErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.annex.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
  coord_flip() + Statusplot.labs.bahasa["Time"] + plot.guides.techreport + plot.theme


# ---- 5.7 Days unwell -----

Unwell.annexplot.bahasa <- 
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
                     values=annex.alpha[[paste("numyear",num.years,sep="")]],
                     labels=(Monitoryear.labs.bahasa$Label),
                     na.translate=FALSE) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  scale_x_discrete(labels=Annexplot.settnames.bahasa[,"Unwell"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Sett.level.ContData.annex.PLOTFORMAT$UnwellMean,na.rm=T)+
                                max(Sett.level.ContData.annex.PLOTFORMAT$UnwellErr,na.rm=T)+
                                0.03*max(Sett.level.ContData.annex.PLOTFORMAT$UnwellMean,na.rm=T))) +
  coord_flip() + Statusplot.labs.bahasa["Unwell"] + plot.guides.techreport + plot.theme


