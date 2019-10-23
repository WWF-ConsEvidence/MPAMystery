# need to source: HHData, hfs.outcome.t2, hfs.outcome.t4, asset.outcome.t2, asset.outcome.t4, through all Bigfive

pacman::p_load(ggplot2,grid,gridExtra)


fillcols.trend.t2t4 <- c("MPA.trend"=alpha("#2C7FB8",0.95),"Control.trend"=alpha("#303030",0.3))

plot.theme.matchedtrends <- theme(axis.ticks=element_blank(),
                                panel.background=element_rect(fill="white",
                                                              colour="#909090"),
                                panel.border=element_rect(fill=NA,
                                                          size=0.25,
                                                          colour="#C0C0C0"),
                                panel.grid.major.x=element_line(colour="#C0C0C0",
                                                                size=0.25,
                                                                linetype=3),
                                panel.grid.major.y=element_blank(),
                                axis.title=element_text(size=rel(0.9),
                                                        angle=0,
                                                        face="bold",
                                                        colour="#303030"),
                                axis.text=element_text(size=rel(0.9),
                                                       angle=0,
                                                       colour="#303030"),
                                plot.title=element_text(hjust=0.5),
                                legend.position="top",
                                legend.justification="center",
                                legend.box.spacing=unit(0.1,"cm"))


# ---- Define plotting data sets, for all BigFive matched data and outcomes tables ----

hfs.matched.trends <- 
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(hfs.outcome.t2$tr1tx)),
                                    left_join(hfs.outcome.t2,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))),
                   cbind.data.frame(year=rep("t4",length(hfs.outcome.t4$tr1tx)),
                                    left_join(hfs.outcome.t4,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))))  %>%
  group_by(year,MPAID) %>%
  summarise(MPA.trend=mean(MPA.outcome,na.rm=T),
            Control.trend=mean(Control.outcome,na.rm=T))


asset.matched.trends <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(asset.outcome.t2$tr1tx)),
                                    left_join(asset.outcome.t2,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))),
                   cbind.data.frame(year=rep("t4",length(asset.outcome.t4$tr1tx)),
                                    left_join(asset.outcome.t4,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))))  %>%
  group_by(year,MPAID) %>%
  summarise(MPA.trend=mean(MPA.outcome,na.rm=T),
            Control.trend=mean(Control.outcome,na.rm=T))


tenure.matched.trends <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(tenure.outcome.t2$tr1tx)),
                                    left_join(tenure.outcome.t2,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))),
                   cbind.data.frame(year=rep("t4",length(tenure.outcome.t4$tr1tx)),
                                    left_join(tenure.outcome.t4,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID")))) %>%
  group_by(year,MPAID) %>%
  summarise(MPA.trend=mean(MPA.outcome,na.rm=T),
            Control.trend=mean(Control.outcome,na.rm=T))
  

attach.matched.trends <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(attach.outcome.t2$tr1tx)),
                                    left_join(attach.outcome.t2,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))),
                   cbind.data.frame(year=rep("t4",length(attach.outcome.t4$tr1tx)),
                                    left_join(attach.outcome.t4,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID")))) %>%
  group_by(year,MPAID) %>%
  summarise(MPA.trend=mean(MPA.outcome,na.rm=T),
            Control.trend=mean(Control.outcome,na.rm=T))
  

enrol.matched.trends <-
  rbind.data.frame(cbind.data.frame(year=rep("t2",length(enrol.outcome.t2$tr1tx)),
                                    left_join(enrol.outcome.t2,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID"))),
                   cbind.data.frame(year=rep("t4",length(enrol.outcome.t4$tr1tx)),
                                    left_join(enrol.outcome.t4,HHData[,c("HouseholdID","MPAID")],by=c("tr1tx"="HouseholdID")))) %>%
  group_by(year,MPAID) %>%
  summarise(MPA.trend=mean(MPA.outcome,na.rm=T),
            Control.trend=mean(Control.outcome,na.rm=T))


# ---- FOOD SECURITY TRENDS, BY MPA ----

Telma.hfs.trends <- 
  hfs.matched.trends%>%
  filter(MPAID==1) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends + guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Telma Food Security")


TNTC.hfs.trends <- 
  hfs.matched.trends%>%
  filter(MPAID==2) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="TNTC Food Security")

  
Kaimana.hfs.trends <-
 hfs.matched.trends%>%
  filter(MPAID==3) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                     values=fillcols.trend.t2t4,
                     labels=c("Control", "MPA"),
                     na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kaimana Food Security")


Kofiau.hfs.trends <- 
  hfs.matched.trends%>%
  filter(MPAID==4) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kofiau Food Security")


Dampier.hfs.trends <- 
  hfs.matched.trends%>%
  filter(MPAID==5) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Dampier Food Security")


Misool.hfs.trends <- 
  hfs.matched.trends%>%
  filter(MPAID==6) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Misool Food Security")


# grid.arrange all plots into one figure
hfs.trends.byMPA <- arrangeGrob(Telma.hfs.trends, TNTC.hfs.trends, Kaimana.hfs.trends, 
                                Kofiau.hfs.trends, Dampier.hfs.trends, Misool.hfs.trends, 
                                 ncol = 3, padding = 30)


# ---- MATERIAL ASSETS TRENDS, BY MPA ----

Telma.asset.trends <- 
  asset.matched.trends%>%
  filter(MPAID==1) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Telma Material Assets")


TNTC.asset.trends <- 
  asset.matched.trends%>%
  filter(MPAID==2) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="TNTC Material Assets")


Kaimana.asset.trends <-
  asset.matched.trends%>%
  filter(MPAID==3) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kaimana Material Assets")


Kofiau.asset.trends <- 
  asset.matched.trends%>%
  filter(MPAID==4) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kofiau Material Assets")


Dampier.asset.trends <- 
  asset.matched.trends%>%
  filter(MPAID==5) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Dampier Material Assets")


Misool.asset.trends <- 
  asset.matched.trends%>%
  filter(MPAID==6) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-10,10)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Misool Material Assets")



# grid.arrange all plots into one figure
asset.trends.byMPA <- arrangeGrob(Telma.asset.trends, TNTC.asset.trends, Kaimana.asset.trends, 
                                Kofiau.asset.trends, Dampier.asset.trends, Misool.asset.trends, 
                                ncol = 3, padding = 30)

# ---- MARINE TENURE TRENDS, BY MPA ----

Telma.tenure.trends <- 
  tenure.matched.trends%>%
  filter(MPAID==1) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Telma Marine Tenure")


TNTC.tenure.trends <- 
  tenure.matched.trends%>%
  filter(MPAID==2) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="TNTC Marine Tenure")


Kaimana.tenure.trends <-
  tenure.matched.trends%>%
  filter(MPAID==3) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kaimana Marine Tenure")


Kofiau.tenure.trends <- 
  tenure.matched.trends%>%
  filter(MPAID==4) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kofiau Marine Tenure")


Dampier.tenure.trends <- 
  tenure.matched.trends%>%
  filter(MPAID==5) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Dampier Marine Tenure")


Misool.tenure.trends <- 
  tenure.matched.trends%>%
  filter(MPAID==6) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Misool Marine Tenure")


# grid.arrange all plots into one figure
tenure.trends.byMPA <- arrangeGrob(Telma.tenure.trends, TNTC.tenure.trends, Kaimana.tenure.trends, 
                                Kofiau.tenure.trends, Dampier.tenure.trends, Misool.tenure.trends, 
                                ncol = 3, padding = 30)

# ---- PLACE ATTACHMENT TRENDS, BY MPA ----

Telma.attach.trends <- 
  attach.matched.trends%>%
  filter(MPAID==1) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Telma Place Attachment")


TNTC.attach.trends <- 
  attach.matched.trends%>%
  filter(MPAID==2) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="TNTC Place Attachment")


Kaimana.attach.trends <-
  attach.matched.trends%>%
  filter(MPAID==3) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kaimana Place Attachment")


Kofiau.attach.trends <- 
  attach.matched.trends%>%
  filter(MPAID==4) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kofiau Place Attachment")


Dampier.attach.trends <- 
  attach.matched.trends%>%
  filter(MPAID==5) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Dampier Place Attachment")


Misool.attach.trends <- 
  attach.matched.trends%>%
  filter(MPAID==6) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-1,1)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Misool Place Attachment")


# grid.arrange all plots into one figure
attach.trends.byMPA <- arrangeGrob(Telma.attach.trends, TNTC.attach.trends, Kaimana.attach.trends, 
                                Kofiau.attach.trends, Dampier.attach.trends, Misool.attach.trends, 
                                ncol = 3, padding = 30)


# ---- SCHOOL ENROLLMENT TRENDS, BY MPA ----

Telma.enrol.trends <- 
  enrol.matched.trends%>%
  filter(MPAID==1) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.3,0.3)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Telma School Enrollment")


TNTC.enrol.trends <- 
  enrol.matched.trends%>%
  filter(MPAID==2) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.3,0.3)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="TNTC School Enrollment")


Kaimana.enrol.trends <-
  enrol.matched.trends%>%
  filter(MPAID==3) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.3,0.3)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kaimana School Enrollment")


Kofiau.enrol.trends <- 
  enrol.matched.trends%>%
  filter(MPAID==4) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.3,0.3)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Kofiau School Enrollment")


Dampier.enrol.trends <- 
  enrol.matched.trends%>%
  filter(MPAID==5) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.3,0.3)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Dampier School Enrollment")


Misool.enrol.trends <- 
  enrol.matched.trends%>%
  filter(MPAID==6) %>%
  melt(id.vars="year",measure.vars=c("MPA.trend","Control.trend"),variable.name="treatment",value.name="trend") %>%
  mutate(treatment=factor(treatment,levels=c("Control.trend","MPA.trend"),ordered=T)) %>%
  ggplot() +
  geom_bar(aes(x=year,
               y=trend,
               group=treatment,
               fill=treatment),
           stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#909090") +
  geom_hline(aes(yintercept=0),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_fill_manual(name="",
                    values=fillcols.trend.t2t4,
                    labels=c("Control", "MPA"),
                    na.translate=FALSE) +
  scale_x_discrete(labels=c("Two year","Four year"),
                   na.value=" ") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.3,0.3)) +
  coord_flip() + plot.theme.matchedtrends+ guides(fill=guide_legend(reverse=T)) +
  labs(x="Monitoring Year", y="Change since Baseline", title="Misool School Enrollment")


# grid.arrange all plots into one figure
enrol.trends.byMPA <- arrangeGrob(Telma.enrol.trends, TNTC.enrol.trends, Kaimana.enrol.trends, 
                                Kofiau.enrol.trends, Dampier.enrol.trends, Misool.enrol.trends, 
                                ncol = 3, padding = 30)


# ---- WRITE TO .PNG ----

dir.create(paste("x_Flat_data_files/1_Social/Outputs/BHS_level_Analysis/Matched_trend_figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("x_Flat_data_files/1_Social/Outputs/BHS_level_Analysis/Matched_trend_figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")

# -- Food security

png(paste(FigureFileName,"hfs.trends.byMPA.png",sep="/"),
    units="in",height=7,width=11,res=400)
plot(hfs.trends.byMPA)
dev.off()

# -- Material assets

png(paste(FigureFileName,"asset.trends.byMPA.png",sep="/"),
    units="in",height=7,width=11,res=400)
plot(asset.trends.byMPA)
dev.off()

# -- Marine tenure

png(paste(FigureFileName,"tenure.trends.byMPA.png",sep="/"),
    units="in",height=7,width=11,res=400)
plot(tenure.trends.byMPA)
dev.off()

# -- Place attachment

png(paste(FigureFileName,"attach.trends.byMPA.png",sep="/"),
    units="in",height=7,width=11,res=400)
plot(attach.trends.byMPA)
dev.off()

# -- School enrollment

png(paste(FigureFileName,"enrol.trends.byMPA.png",sep="/"),
    units="in",height=7,width=11,res=400)
plot(enrol.trends.byMPA)
dev.off()
