# 
# code:  Koon Technical Report Plots
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
#  2) Source Koon_2018_TechReport_Datasets.R 
#     - Dependencies: Koon_2018_TechReport_SigTests.R
#                     Koon_2018_calculate_indicators.R
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

# 
# # ---- 1.1 Source or run data sets ----
# 

define.statusplot.asterisks <- function(x) {
  result <- x
  reference <- x
  for(a in colnames(x[2:6])){
    for(i in 1:length(x$SettlementName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
      reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
    }
  }
  colnames(result) <- c("SettlementName","FS","MA","PA","MT","SE")
  colnames(reference) <- c("SettlementName","FS.ref","MA.ref","PA.ref","MT.ref","SE.ref")
  result <- left_join(result,reference,by="SettlementName")
  result
}


define.statusplot.asterisk.pos <- function(x,asterisks) {
  result <- asterisks[,1:6]
  ref <- asterisks[,c(1,7:11)]
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,2:6] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks[,2:6],
                         d=c(1:5),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,2:6] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks[,7:11],
                      d=c(1:5),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="SettlementName")
  result
}

# ---- 1.2 Define significance labels and (x,y) coordinates for plots ----

Koon.statusplot.asterisks <- 
  define.statusplot.asterisks(Koon.ContData.Techreport.status.PLOTFORMAT[,c("SettlementName","FS.pval",
                                                                               "MA.pval","PA.pval",
                                                                               "MT.pval","SE.pval")])
Koon.statusplot.sigpos <- 
  define.statusplot.asterisk.pos(Koon.ContData.Techreport.status.PLOTFORMAT,
                                 Koon.statusplot.asterisks)  



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: AGE/GENDER PLOT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Baseline ----

# Koon.age.gender.baseline <- 
#   melt(Koon.AgeGender,id.vars="AgeCat",measure.vars=c("Female.Baseline","Male.Baseline")) %>%
#   ggplot() +
#   geom_bar(aes(x=AgeCat,
#                y=value,
#                fill=variable),
#            stat="identity",
#            width=0.75,
#            colour="#505050",
#            size=0.15) +
#   scale_y_continuous(expand=c(0,0),
#                      limits=c(-10,10),
#                      labels=abs(seq(-10,10,5))) +
#   scale_fill_manual(name="",
#                     labels=c("Female","Male"),
#                     values=c("Female.Baseline"=alpha("#7FCDBB",0.95),
#                              "Male.Baseline"=alpha("#253494",0.95))) +
#   coord_flip() + age.gender.plot.theme + plot.guides.techreport + labs(x="Age",y="Population distribution (% of individuals by gender)")
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATUS PLOTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Continuous data plots ----

# - FOOD SECURITY
Koon.fs.statusplot <- 
  rbind.data.frame(Koon.ContData.Techreport.status.PLOTFORMAT,
                   cbind.data.frame(SettlementID=NA,SettlementName="  ",
                                    matrix(rep(NA,15),ncol=15,
                                           dimnames=list(NULL,
                                                         colnames(Koon.ContData.Techreport.status.PLOTFORMAT)[3:17])),
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
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,
                y=FS),
            label=Koon.statusplot.asterisks$FS,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,y=FS.ref),
            label=Koon.statusplot.asterisks$FS.ref,
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
Koon.ma.statusplot <- ggplot(data=Koon.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,
                y=MA),
            label=Koon.statusplot.asterisks$MA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,y=MA.ref),
            label=Koon.statusplot.asterisks$MA.ref,
            size=rel(3),
            nudge_x=0.02,
            fontface="bold.italic",
            colour=errcols.status["NotDummy"]) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Koon.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T)+
                                max(Koon.ContData.Techreport.status.PLOTFORMAT$MAErr,na.rm=T)+
                                0.03*max(Koon.ContData.Techreport.status.PLOTFORMAT$MAMean,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["MA"] + plot.theme

# - PLACE ATTACHMENT
Koon.pa.statusplot <- ggplot(data=Koon.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,
                y=PA),
            label=Koon.statusplot.asterisks$PA,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,y=PA.ref),
            label=Koon.statusplot.asterisks$PA.ref,
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
Koon.mt.statusplot <- ggplot(data=Koon.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,
                y=MT+(0.05*MT)),
            label=Koon.statusplot.asterisks$MT,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,y=MT.ref),
            label=Koon.statusplot.asterisks$MT.ref,
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
Koon.se.statusplot <- ggplot(data=Koon.ContData.Techreport.status.PLOTFORMAT,
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
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,
                y=SE),
            label=Koon.statusplot.asterisks$SE,
            nudge_x=-0.07,
            size=rel(4),
            colour=errcols.status["NotDummy"]) +
  geom_text(data=Koon.statusplot.sigpos,
            aes(x=SettlementName,y=SE.ref),
            label=Koon.statusplot.asterisks$SE.ref,
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
# 
# # - TIME TO MARKET
# Koon.time.statusplot <- ggplot(data=Koon.ContData.Techreport.status.PLOTFORMAT,
#                              aes(x=SettlementName)) +
#   geom_bar(aes(y=TimeMarketMean,
#                fill=SettLevel),
#            stat="identity",
#            position="dodge",
#            width=0.75,
#            show.legend=F) +
#   geom_errorbar(aes(ymin=TimeMarketMean-TimeMarketErr,
#                     ymax=TimeMarketMean+TimeMarketErr,
#                     colour=SettLevel),
#                 width=0.25,
#                 size=0.5,
#                 show.legend=F) +
#   geom_vline(aes(xintercept=3),
#              linetype=2,
#              size=0.35,
#              colour="#505050") +
#   geom_text(data=Koon.statusplot.sigpos,
#             aes(x=SettlementName,
#                 y=Time),
#             label=Koon.statusplot.asterisks$Time,
#             nudge_x=-0.07,
#             size=rel(4),
#             colour=errcols.status["NotDummy"]) +
#   geom_text(data=Koon.statusplot.sigpos,
#             aes(x=SettlementName,y=Time.ref),
#             label=Koon.statusplot.asterisks$Time.ref,
#             size=rel(3),
#             nudge_x=0.02,
#             fontface="bold.italic",
#             colour=errcols.status["NotDummy"]) +
#   scale_y_continuous(expand=c(0,0),
#                      limits=c(0,max(Koon.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T)+
#                                       max(Koon.ContData.Techreport.status.PLOTFORMAT$TimeMarketErr,na.rm=T)+
#                                 0.03*max(Koon.ContData.Techreport.status.PLOTFORMAT$TimeMarketMean,na.rm=T))) +
#   scale_fill_manual(values=fillcols.status) +
#   scale_colour_manual(values=errcols.status) +
#   coord_flip() + Statusplot.labs["Time"] + plot.theme
# 
# # - DAYS UNWELL
# Koon.unwell.statusplot <- ggplot(data=Koon.ContData.Techreport.status.PLOTFORMAT,
#                              aes(x=SettlementName)) +
#   geom_bar(aes(y=UnwellMean,
#                fill=SettLevel),
#            stat="identity",
#            position="dodge",
#            width=0.75,
#            show.legend=F) +
#   geom_errorbar(aes(ymin=UnwellMean-UnwellErr,
#                     ymax=UnwellMean+UnwellErr,
#                     colour=SettLevel),
#                 width=0.25,
#                 size=0.5,
#                 show.legend=F) +
#   geom_vline(aes(xintercept=3),
#              linetype=2,
#              size=0.35,
#              colour="#505050") +
#   geom_text(data=Koon.statusplot.sigpos,
#             aes(x=SettlementName,
#                 y=Unwell),
#             label=Koon.statusplot.asterisks$Unwell,
#             nudge_x=-0.07,
#             size=rel(4),
#             colour=errcols.status["NotDummy"]) +
#   geom_text(data=Koon.statusplot.sigpos,
#             aes(x=SettlementName,y=Unwell.ref),
#             label=Koon.statusplot.asterisks$Unwell.ref,
#             size=rel(3),
#             nudge_x=0.02,
#             fontface="bold.italic",
#             colour=errcols.status["NotDummy"]) +
#   scale_y_continuous(expand=c(0,0),
#                      limits=c(0,max(Koon.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T)+
#                                 max(Koon.ContData.Techreport.status.PLOTFORMAT$UnwellErr,na.rm=T)+
#                                 0.03*max(Koon.ContData.Techreport.status.PLOTFORMAT$UnwellMean,na.rm=T))) +
#   scale_fill_manual(values=fillcols.status) +
#   scale_colour_manual(values=errcols.status) +
#   coord_flip() + Statusplot.labs["Unwell"] + plot.theme
# 


# ---- 3.2 Proportional data plots ----

# - GENDER OF HEAD OF HOUSEHOLD
Koon.gender.statusplot <- 
  melt(Koon.PropData.Techreport.status.PLOTFORMAT,
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
Koon.religion.statusplot <- 
  melt(Koon.PropData.Techreport.status.PLOTFORMAT,
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
Koon.primaryocc.statusplot <- 
  melt(Koon.PropData.Techreport.status.PLOTFORMAT,
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


# - SECONDARY OCCUPATION
Koon.secondaryocc.statusplot <- 
  melt(Koon.PropData.Techreport.status.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Percent.SecondaryOcc.Other",  
                                               "Percent.SecondaryOcc.Aquaculture","Percent.SecondaryOcc.Tourism",
                                               "Percent.SecondaryOcc.Extraction","Percent.SecondaryOcc.WageLabor",
                                               "Percent.SecondaryOcc.HarvestForest", 
                                               "Percent.SecondaryOcc.Fish","Percent.SecondaryOcc.Farm")) %>%
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
                    values=multianswer.fillcols.status[["SecondaryOcc"]],
                    labels=c("Other", "Aquaculture", "Tourism", "Extraction of non-renewable marine resources",  "Other Wage Labor",
                             "Harvest Forest Products","Fishing", "Farming")) +
  coord_flip() + plot.theme + labs(x="Settlement",y="Secondary occupatioin (% households)") + plot.guides.techreport


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

dir.create(paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Koon/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("2_Social/FlatDataFiles/SBS/TechReportOutput/Koon/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

# ---- 6.1 Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.fs.statusplot)
dev.off()


# ---- 6.2 Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.ma.statusplot)
dev.off()


# ---- 6.3 Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.pa.statusplot)
dev.off()


# ---- 6.4 Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.mt.statusplot)
dev.off()


# ---- 6.5 School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.se.statusplot)
dev.off()

# 
# # ---- 6.6 Time to market ----
# 
# png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
#     units="in",height=6,width=9,res=400)
# plot(Koon.time.statusplot)
# dev.off()
# 
# 
# # ---- 6.7 Days unwell ----
# 
# png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
#     units="in",height=6,width=9,res=400)
# plot(Koon.unwell.statusplot)
# dev.off()
# 

# ---- 6.8 Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.gender.statusplot)
dev.off()


# ---- 6.9 Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.religion.statusplot)
dev.off()


# ---- 6.10 Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.primaryocc.statusplot)
dev.off()


# ---- 6.11 Secondary occupation ----

png(paste(FigureFileName,"SecondaryOcc.status.png",sep="/"),
    units="in",height=6,width=9,res=400)
plot(Koon.secondaryocc.statusplot)
dev.off()

# ---- Remove all plot objects from environment ----
# rm(median.setts.Koon,Koon.statusplot.asterisks,Koon.statusplot.sigpos,
#    Koon.trendplot.monitoryear.labs,Koon.conttrendplot.ylabs,Koon.proptrendplot.ylabs,
#    Koon.trendplot.labs,Koon.annexplot.settnames,
#    Koon.age.gender.baseline,Koon.age.gender.2yr,Koon.age.gender.4yr,
#    Koon.agegender.legend.plot,Koon.agegender.legend,Koon.age.gender.plot,
#    Koon.fs.statusplot,Koon.fs.trendplot,Koon.fs.annexplot,
#    Koon.ma.statusplot,Koon.ma.trendplot,Koon.ma.annexplot,
#    Koon.pa.statusplot,Koon.pa.trendplot,Koon.pa.annexplot,
#    Koon.mt.statusplot,Koon.mt.trendplot,Koon.mt.annexplot,
#    Koon.se.statusplot,Koon.se.trendplot,Koon.se.annexplot,
#    Koon.time.statusplot,Koon.time.trendplot,Koon.time.annexplot,
#    Koon.unwell.statusplot,Koon.unwell.trendplot,Koon.unwell.annexplot,
#    Koon.gender.statusplot,Koon.gender.trendplot,Koon.religion.statusplot,
#    Koon.religion.trendplot,Koon.primaryocc.statusplot,Koon.primaryocc.trendplot,
#    Koon.freqfish.statusplot,Koon.freqsellfish.statusplot,Koon.freqsellfish.trendplot,
#    Koon.incfish.statusplot,Koon.incfish.trendplot,Koon.fishtech.statusplot,
#    Koon.fishtech.trendplot,Koon.childfs.statusplot,Koon.childfs.trendplot,
#    Koon.proteinfish.statusplot,Koon.proteinfish.trendplot,Koon.ethnic.statusplot, Koon.FSCategorical.statusplot,
#    Koon.AdultEduc.statusplot, Koon.econ.statusplot, Koon.econ.statusplot, Koon.rules.statusplot,
#    Koon.participation.statusplot, Koon.member.statusplot, Koon.meeting.statusplot, Koon.illness.statusplot,
#    Koon.conflict.statusplot, Koon.NumThreat.statusplot, Koon.ThreatType.statusplot, Koon.ethnicity.statusplot, 
#    Koon.contribution.statusplot)
# 
# # ---- Remove all tech report datasets from environment ----
# rm(Koon.AgeGender,
#    Koon.ContData.Techreport.status.PLOTFORMAT,
#    Koon.PropData.Techreport.status.PLOTFORMAT,
#    Koon.TrendContData.Techreport.PLOTFORMAT,Koon.TrendPropData.Techreport.PLOTFORMAT,
#    Koon.AnnexContData.Techreport.PLOTFORMAT,Koon.AnnexPropData.Techreport.PLOTFORMAT,
#    sigvals.Koon,trend.sigvals.Koon,annex.sigvals.Koon,propdata.trend.test.Koon,
#    dist.Koon.FS,dist.Koon.MA,dist.Koon.PA,dist.Koon.MT,dist.Koon.SE,dist.Koon.TimeMarket,dist.Koon.DaysUnwell)
