# ---- code: significance tests and plotting for BHS SOTS 2019 ----

# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2019
# modified:
# 
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE PLOT THEMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


source('2_Functions/3_Plotting/Function_plotthemes.R')


plot.theme.1 <- theme(axis.ticks=element_blank(),
                      panel.background=element_rect(fill="white",
                                                    colour="#909090"),
                      panel.border=element_rect(fill=NA,
                                                size=0.25,
                                                colour="#C0C0C0"),
                      panel.grid.major.x=element_line(colour="#C0C0C0",
                                                      size=0.25,
                                                      linetype=3),
                      panel.grid.major.y=element_blank(),
                      plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                      axis.title=element_text(size=rel(0.9),
                                              angle=0,
                                              face="bold",
                                              colour="#303030"),
                      axis.text.y=element_text(size=rel(0.9),
                                             angle=0,
                                             colour="#303030",
                                             lineheight=0.7),
                      axis.text.x=element_text(size=rel(0.9),
                                               angle=55,
                                               colour="#303030",
                                               lineheight=0.9,
                                               hjust=1),
                      legend.position="top",
                      legend.justification="right",
                      legend.box.spacing=unit(0.1,"cm"))

plot.guide <- guides(alpha=guide_legend(ncol=2))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: STATISTICAL ANALYSIS FOR BIG FIVE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

mpa.names <- unique(SOTS.HHData$MPAName)


# Mann-Whitney u test, comparing values from most recent monitoring year per MPA to the BHS-wide values from most recent monitoring year 
status.non.parametric.test.byMPA <-
  data.frame(mapply(b=c("FSIndex","MAIndex","MTIndex","PAIndex"),
                    function(b){
                      results <- 
                        list(cbind.data.frame(MPAName=mpa.names,
                                              t(data.frame(mapply(i=mpa.names,
                                               function(i){
                                                 var <- 
                                                   rbind.data.frame(data.frame(SOTS.HHData[SOTS.HHData$MonitoringYear=="7 Year Post",],
                                                                             Seascape.v.MPA=1),
                                                                  data.frame(SOTS.HHData[SOTS.HHData$MonitoringYear=="7 Year Post" &
                                                                                           SOTS.HHData$MPAName==i,],
                                                                             Seascape.v.MPA=0)) %>% .[,b]
                                                 
                                                 test <- 
                                                   wilcox.test(var~Seascape.v.MPA,
                                                               data=rbind.data.frame(data.frame(SOTS.HHData[SOTS.HHData$MonitoringYear=="7 Year Post",],
                                                                                                Seascape.v.MPA=1),
                                                                                     data.frame(SOTS.HHData[SOTS.HHData$MonitoringYear=="7 Year Post" &
                                                                                                              SOTS.HHData$MPAName==i,],
                                                                                                Seascape.v.MPA=0)),
                                                               exact=F)
                                               }))["p.value",])))
                    })) %>%
  rename(MPAName=FSIndex.MPAName, FS.status.pval = FSIndex.p.value, MA.status.pval = MAIndex.p.value, MT.status.pval = MTIndex.p.value, PA.status.pval = PAIndex.p.value) %>%
  select(MPAName,FS.status.pval,MA.status.pval,MT.status.pval,PA.status.pval) %>%
  tibble::remove_rownames() %>%
  rbind.data.frame(.,
                   data.frame(MPAName="Bird's Head\nSeascape",
                              FS.status.pval="reference",
                              MA.status.pval="reference",
                              MT.status.pval="reference",
                              PA.status.pval="reference"))


data.frame(mapply(b=c("SERate"),
                  function(b){
                    results <- 
                      list(cbind.data.frame(MPAName=mpa.names,
                                            t(data.frame(mapply(i=mpa.names,
                                                                function(i){
                                                                  var <- 
                                                                    rbind.data.frame(data.frame(WWF.HHData[WWF.HHData$MonitoringYear=="4 Year Post",],
                                                                                                Seascape.v.MPA=1),
                                                                                     data.frame(WWF.HHData[WWF.HHData$MonitoringYear=="4 Year Post" &
                                                                                                             WWF.HHData$MPAName==i,],
                                                                                                Seascape.v.MPA=0)) %>% .[,b]
                                                                  
                                                                  test <- 
                                                                    wilcox.test(var~Seascape.v.MPA,
                                                                                data=rbind.data.frame(data.frame(WWF.HHData[WWF.HHData$MonitoringYear=="4 Year Post",],
                                                                                                                 Seascape.v.MPA=1),
                                                                                                      data.frame(WWF.HHData[WWF.HHData$MonitoringYear=="4 Year Post" &
                                                                                                                              WWF.HHData$MPAName==i,],
                                                                                                                 Seascape.v.MPA=0)),
                                                                                exact=F)
                                                                }))["p.value",])))
                  }))


# Mann-Kendall monotonic trend test, looking for monotonic trends across all monitoring years, per MPA
trend.non.parametric.test.byMPA  <- 
  cbind.data.frame(MPAName=mpa.names,
                   mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex"),
                          function(a){
                            t(data.frame(mapply(i=as.character(mpa.names),
                                                function(i){
                                                  MannKendall(c(SOTS.HHData[SOTS.HHData$MPAName==i &
                                                                              SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[1],a],
                                                                SOTS.HHData[SOTS.HHData$MPAName==i &
                                                                              SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[2],a], 
                                                                SOTS.HHData[SOTS.HHData$MPAName==i &
                                                                              SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[3],a],
                                                                SOTS.HHData[SOTS.HHData$MPAName==i &
                                                                              SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[4],a]))
                                                }))["sl",])})) %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex) 

trend.non.parametric.test.bySeascape <-
  data.frame(mapply(i=SOTS.HHData[,c("FSIndex","MAIndex","MTIndex","PAIndex")],
                    function(i){
                      MannKendall(c(i[SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[1]],
                                    i[SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[2]],
                                    i[SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[3]],
                                    i[SOTS.HHData$MonitoringYear==unique(SOTS.HHData$MonitoringYear)[4]]))
                    }))["sl",] %>%
  rename(FS.trend.pval = FSIndex, MA.trend.pval = MAIndex, MT.trend.pval = MTIndex, PA.trend.pval = PAIndex) %>%
  mutate(MPAName="Bird's Head\nSeascape")

trend.pvals <-
  rbind.data.frame(trend.non.parametric.test.byMPA,
                   trend.non.parametric.test.bySeascape) %>%
  mutate(MPAName=factor(MPAName,levels=c("Teluk Cenderawasih\nNational Park","Buruway MPA","Teluk Etna MPA","Teluk Triton MPA",
                                         "Kofiau dan Pulau\nBoo MPA","Misool Selatan\nTimur MPA","Selat Dampier MPA","Teluk Mayalibit MPA",
                                         "Bird's Head\nSeascape"),
                        ordered=T))

trend.SERate <- cbind.data.frame(MPAName=mpa.names,
                                 t(data.frame(mapply(i=as.character(mpa.names),
                                                     function(i){
                                                       MannKendall(c(WWF.HHData[WWF.HHData$MPAName==i &
                                                                                  WWF.HHData$MonitoringYear==unique(WWF.HHData$MonitoringYear)[1],"SERate"],
                                                                     WWF.HHData[WWF.HHData$MPAName==i &
                                                                                  WWF.HHData$MonitoringYear==unique(WWF.HHData$MonitoringYear)[2],"SERate"],
                                                                     WWF.HHData[WWF.HHData$MPAName==i &
                                                                                  WWF.HHData$MonitoringYear==unique(WWF.HHData$MonitoringYear)[3],"SERate"]))
                                                     }))["sl",])) %>%
  rename(pval=sl) %>%
  tibble::remove_rownames()

trend.SERate.bySeascape <-
  data.frame(MannKendall(c(WWF.HHData[WWF.HHData$MonitoringYear==unique(WWF.HHData$MonitoringYear)[1],"SERate"],
                           WWF.HHData[WWF.HHData$MonitoringYear==unique(WWF.HHData$MonitoringYear)[2],"SERate"],
                           WWF.HHData[WWF.HHData$MonitoringYear==unique(WWF.HHData$MonitoringYear)[3],"SERate"]))["sl"]) %>%
  rename(pval = sl) %>%
  mutate(MPAName="Bird's Head\nSeascape")

trend.SERate <-
  rbind.data.frame(trend.SERate,
                   trend.SERate.bySeascape) %>%
  mutate(MPAName=factor(MPAName,levels=c("Teluk Cenderawasih\nNational Park","Buruway MPA","Teluk Etna MPA","Teluk Triton MPA",
                                         "Kofiau dan Pulau\nBoo MPA","Misool Selatan\nTimur MPA","Selat Dampier MPA","Teluk Mayalibit MPA",
                                         "Bird's Head\nSeascape"),
                        ordered=T))

# Add asterisks to MPA name

Plotting.SOTS.HHData <-
  Plotting.SOTS.HHData %>%
  left_join(trend.pvals,by="MPAName") %>%
  mutate(Asterisk.FS=ifelse(FS.trend.pval<0.01,"***",
                            ifelse(FS.trend.pval<0.05 & FS.trend.pval>=0.01,"**",
                                   ifelse(FS.trend.pval<0.1 & FS.trend.pval>=0.05,"*",""))),
         Asterisk.MA=ifelse(MA.trend.pval<0.01,"***",
                            ifelse(MA.trend.pval<0.05 & MA.trend.pval>=0.01,"**",
                                   ifelse(MA.trend.pval<0.1 & MA.trend.pval>=0.05,"*",""))),
         Asterisk.MT=ifelse(MT.trend.pval<0.01,"***",
                            ifelse(MT.trend.pval<0.05 & MT.trend.pval>=0.01,"**",
                                   ifelse(MT.trend.pval<0.1 & MT.trend.pval>=0.05,"*",""))),
         Asterisk.PA=ifelse(PA.trend.pval<0.01,"***",
                            ifelse(PA.trend.pval<0.05 & PA.trend.pval>=0.01,"**",
                                   ifelse(PA.trend.pval<0.1 & PA.trend.pval>=0.05,"*",""))),
         MPAName.FS=ifelse(Asterisk.FS=="",as.character(MPAName),paste(MPAName,Asterisk.FS,sep=" ")),
         MPAName.MA=ifelse(Asterisk.MA=="",as.character(MPAName),paste(MPAName,Asterisk.MA,sep=" ")),
         MPAName.MT=ifelse(Asterisk.MT=="",as.character(MPAName),paste(MPAName,Asterisk.MT,sep=" ")),
         MPAName.PA=ifelse(Asterisk.PA=="",as.character(MPAName),paste(MPAName,Asterisk.PA,sep=" ")))


Plotting.SOTS.SERate <-
  WWF.HHData.SEMean %>%
  left_join(trend.SERate,by="MPAName") %>%
  mutate(Asterisk=ifelse(pval<0.01,"***",
                            ifelse(pval<0.05 & pval>=0.01,"**",
                                   ifelse(pval<0.1 & pval>=0.05,"*",""))),
         MPAName.SE=ifelse(Asterisk=="",as.character(MPAName),paste(MPAName,Asterisk,sep=" ")))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PLOTTING FOR BIG FIVE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# --- Material assets

MA <- ggplot(Plotting.SOTS.HHData, aes(x=MPAName,y=MAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,ymax=MAMean+MAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=8.5),linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,30),
                     breaks=seq(0,30,by=5)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","Two years post-baseline","Four years post-baseline","Seven years post-baseline"),
                     values=c("Baseline"=0.2,"2 Year Post"=0.5,"4 Year Post"=0.8,"7 Year Post"=1)) +
  scale_x_discrete(labels=unique(Plotting.SOTS.HHData$MPAName.MA)) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household material assets")


# --- Food security

FS <- 
  rbind.data.frame(Plotting.SOTS.HHData[,c("FSMean","FSErr","MonitoringYear","MPAName","MPAName.FS")],
                   data.frame(FSMean=rep(0,8),
                              FSErr=NA,
                              MonitoringYear=rep(c("Baseline","2 Year Post","4 Year Post","7 Year Post"),2),
                              MPAName=c(rep("",4),rep(" ",4)),
                              MPAName.FS=c(rep("",4),rep(" ",4)))) %>%
  ggplot(aes(x=MPAName,y=FSMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=FSMean-FSErr,ymax=FSMean+FSErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=8.5),linetype=2,size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=1.56),size=0.25,colour="#505050") +
  geom_hline(aes(yintercept=4.02),size=0.25,colour="#505050") +
  geom_text(aes(x=10.7,y=(0.5*(6.06-4.02))+4.02,label="Food secure"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=10.7,y=(0.5*(4.02-1.56))+1.56,label="Food insecure\nwithout hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  geom_text(aes(x=10.7,y=0.5*1.56,label="Food insecure\nwith hunger"),
            size=rel(2.5),lineheight=0.8,fontface="bold.italic",colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.06)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","Two years post-baseline","Four years post-baseline","Seven years post-baseline"),
                     values=c("Baseline"=0.2,"2 Year Post"=0.5,"4 Year Post"=0.8,"7 Year Post"=1)) +
  scale_x_discrete(labels=c(unique(Plotting.SOTS.HHData$MPAName.FS)," "," ")) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household food security")


# --- Marine tenure

MT <- ggplot(Plotting.SOTS.HHData, aes(x=MPAName,y=MTMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,ymax=MTMean+MTErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=8.5),linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5),
                     breaks=seq(0,5,by=1)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","Two years post-baseline","Four years post-baseline","Seven years post-baseline"),
                     values=c("Baseline"=0.2,"2 Year Post"=0.5,"4 Year Post"=0.8,"7 Year Post"=1)) +
  scale_x_discrete(labels=unique(Plotting.SOTS.HHData$MPAName.MT)) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Household marine tenure")


# --- Place attachment

PA <- ggplot(Plotting.SOTS.HHData, aes(x=MPAName,y=PAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=PAMean-PAErr,ymax=PAMean+PAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=8.5),linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5.2),
                     breaks=seq(0,5,by=1)) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","Two years post-baseline","Four years post-baseline","Seven years post-baseline"),
                     values=c("Baseline"=0.2,"2 Year Post"=0.5,"4 Year Post"=0.8,"7 Year Post"=1)) +
  scale_x_discrete(labels=unique(Plotting.SOTS.HHData$MPAName.PA)) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="Place attachment")


# --- School enrollment (t0, t2, t4)

SE <- ggplot(Plotting.SOTS.SERate, aes(x=MPAName,y=SEMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=SEMean-SEErr,ymax=SEMean+SEErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  geom_vline(aes(xintercept=8.5),linetype=2,size=0.25,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     breaks=seq(0,1,by=0.2),
                     labels=scales::percent_format()) +
  scale_alpha_manual(name="",
                     labels=c("Baseline","Two years post-baseline","Four years post-baseline","Seven years post-baseline"),
                     values=c("Baseline"=0.2,"2 Year Post"=0.5,"4 Year Post"=0.8,"7 Year Post"=1)) +
  scale_x_discrete(labels=unique(Plotting.SOTS.SERate$MPAName.SE)) +
  plot.theme.1 + plot.guide + labs(title="",x="",y="School enrollment rate")

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: READ TO PNG, EXPORT DATA TABLES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

FigureFileName.SOTS <- 'x_Flat_data_files/1_Social/Outputs/BHS_level_Analysis/SOTS_2019/'


# ---- Material assets ----

png(paste(FigureFileName.SOTS,"SOTS.MA.UPDATE.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(MA)
dev.off()


# ---- Food security ----

png(paste(FigureFileName.SOTS,"SOTS.FS.UPDATE.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(FS)
dev.off()


# ---- Marine tenure ----

png(paste(FigureFileName.SOTS,"SOTS.MT.UPDATE.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(MT)
dev.off()


# ---- Place attachment ----

png(paste(FigureFileName.SOTS,"SOTS.PA.UPDATE.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(PA)
dev.off()


# ---- School enrollment ----

png(paste(FigureFileName.SOTS,"SOTS.SE.png",sep="/"),
    units="in",height=5,width=8,res=400)
plot(SE)
dev.off()



# ---- Export data table w sig vals ----

export.table <-
  left_join(Plotting.SOTS.HHData,status.non.parametric.test.byMPA,by="MPAName")

export(export.table,paste(FigureFileName.SOTS,"SOTS.2019.output.data.xlsx"))
