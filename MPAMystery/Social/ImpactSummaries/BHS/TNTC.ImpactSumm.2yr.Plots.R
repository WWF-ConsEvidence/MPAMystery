# ----
# code:  Cenderawasih 2Yr Impact Summary Plots
# git branch: MPAMystery --> Social --> ImpactSummaries --> Cenderawasih
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: February 2017
# 
# ----
# code sections:
#  1) Source Cenderawasih 2Yr Impact Summary Statistical Code
#  2) Status and Trend Plots, Big Five
#  3) Impact Plots, Big Five
#  4) Standardized, "Snapshot" Plot, Big Five
# 
# ----
############################################################
#
# SECTION 1: Source Cenderawasih 2Yr Impact Summary Statistical Code
#
############################################################
# ----

source('ImpactSummaries/Cenderawasih/TNTC.ImpactSumm.2yr.SigTests.R')

# ----
############################################################
#
# SECTION 2: Status and Trend Plots, Big Five
#
############################################################
# ----

# 2.1 Food Security
TNTC.MPAimpact.summ.fs.st <- ggplot(data=TNTC.BigFive.MPAimpact.summ.st,
                                    aes(Year,
                                        FS)) +
  geom_hline(aes(yintercept=4.02),
             show.legend=F,
             colour="black",
             size=0.5,
             alpha=0.4) +
  geom_hline(aes(yintercept=1.56),
             show.legend=F,
             colour="black",
             size=0.5,
             alpha=0.4) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_errorbar(aes(ymin=FS-FSErr,
                    ymax=FS+FSErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  annotate("rect",xmin=1.7,xmax=2.3,ymin=1.13,ymax=1.5,alpha=0.6,fill="white",colour="#FFFFFF") +
  annotate("text",x=2.3,y=5.85,label="Food secure",alpha=0.4,fontface="italic",size=3) +
  annotate("text",x=2,y=3.8,label="Food insecure without hunger",alpha=0.4,fontface="italic",size=3) +
  annotate("text",x=2.06,y=1.35,label="Food insecure with hunger",alpha=0.4,fontface="italic",size=3) +
  scale_x_discrete(labels=c(as.character(TNTC.sig.labs["FS","xaxis.st.baselabs"]),
                            as.character(TNTC.sig.labs["FS","xaxis.st.2yrlabs"]))) +
  scale_y_continuous(limits=c(0,6.06),
                     expand=c(0,0)) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  fs.st.plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.fs.labs.st

# 2.2 Material Assets
TNTC.MPAimpact.summ.ma.st <- ggplot(data=TNTC.BigFive.MPAimpact.summ.st,
                                    aes(Year,
                                        MA)) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  scale_x_discrete(labels=c(as.character(TNTC.sig.labs["MA","xaxis.st.baselabs"]),
                            as.character(TNTC.sig.labs["MA","xaxis.st.2yrlabs"]))) +
  scale_y_continuous(expand=c(0,0)) +
  geom_errorbar(aes(ymin=MA-MAErr,
                    ymax=MA+MAErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.ma.labs.st

# 2.3 Place Attachment
TNTC.MPAimpact.summ.pa.st <- ggplot(data=TNTC.BigFive.MPAimpact.summ.st,
                                    aes(Year,
                                        PA)) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_errorbar(aes(ymin=PA-PAErr,
                    ymax=PA+PAErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  scale_x_discrete(labels=c(as.character(TNTC.sig.labs["PA","xaxis.st.baselabs"]),
                            as.character(TNTC.sig.labs["PA","xaxis.st.2yrlabs"]))) +
  scale_y_continuous(limits=c(0,5),
                     expand=c(0,0)) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.pa.labs.st

# 2.4 Marine Tenure
TNTC.MPAimpact.summ.mt.st <- ggplot(data=TNTC.BigFive.MPAimpact.summ.st,
                                    aes(Year,
                                        MT)) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_errorbar(aes(ymin=MT-MTErr,
                    ymax=MT+MTErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  scale_y_continuous(limits=c(0,5),
                     expand=c(0,0)) +
  scale_x_discrete(labels=c(as.character(TNTC.sig.labs["MT","xaxis.st.baselabs"]),
                            as.character(TNTC.sig.labs["MT","xaxis.st.2yrlabs"]))) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.mt.labs.st

# 2.5 School Enrollment
TNTC.MPAimpact.summ.se.st <- ggplot(data=TNTC.BigFive.MPAimpact.summ.st,
                                    aes(Year,
                                        SE)) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  scale_x_discrete(labels=c(as.character(TNTC.sig.labs["SE","xaxis.st.baselabs"]),
                            as.character(TNTC.sig.labs["SE","xaxis.st.2yrlabs"]))) +
  scale_y_continuous(labels=scales::percent_format(),
                     expand=c(0,0),
                     limits=c(0,1)) +
  geom_errorbar(aes(ymin=SE-SEErr,
                    ymax=SE+SEErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.se.labs.st

# ----
############################################################
#
# SECTION 3: Impact Plots, Big Five
#
############################################################
# ----

# 3.1 Food Security
TNTC.MPAimpact.summ.fs.i <- ggplot(data=TNTC.2yr.impacts,
                                   aes(x=Treatment,y=FS)) +
  geom_bar(aes(fill=Treatment,width=0.5),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=FS[Treatment=="Control"],yend=FS[Treatment=="Control"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=FS[Treatment=="MPA"],yend=FS[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=FS[Treatment=="Control"],yend=FS[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_point(data=data.frame(x=rep(0.6,2),
                             FS=c(0.305,0.335)),
             aes(x=x,y=FS,shape=""),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_errorbar(aes(ymin=FS-FSErr,
                    ymax=FS+FSErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position="dodge",
                show.legend=F) +
  annotate("text",y=TNTC.sig.pos["FS",],x=0.65,
           label=TNTC.sig.labs["FS","impact.labs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=ifelse(TNTC.2yr.impacts$FS[TNTC.2yr.impacts$Treatment=="MPA"]>
                                     TNTC.2yr.impacts$FS[TNTC.2yr.impacts$Treatment=="Control"],24,25),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.fs.labs.i + plot.guides.MPAimpact.summ

# 3.2 Material Assets
TNTC.MPAimpact.summ.ma.i <- ggplot(data=TNTC.2yr.impacts,
                                   aes(x=Treatment,y=MA)) +
  geom_bar(aes(fill=Treatment,width=0.5),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=MA[Treatment=="Control"],yend=MA[Treatment=="Control"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=MA[Treatment=="MPA"],yend=MA[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=MA[Treatment=="Control"],yend=MA[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_point(data=data.frame(x=rep(0.6,2),
                             MA=c(4.55,4.05)),
             aes(x=x,y=MA,shape=""),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_errorbar(aes(ymin=MA-MAErr,
                    ymax=MA+MAErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position="dodge",
                show.legend=F) +
  annotate("text",y=TNTC.sig.pos["MA",],x=0.65,
           label=TNTC.sig.labs["MA","impact.labs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=ifelse(TNTC.2yr.impacts$MA[TNTC.2yr.impacts$Treatment=="MPA"]>
                                     TNTC.2yr.impacts$MA[TNTC.2yr.impacts$Treatment=="Control"],24,25),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.ma.labs.i + plot.guides.MPAimpact.summ

# 3.3 Place Attachment
TNTC.MPAimpact.summ.pa.i <- ggplot(data=TNTC.2yr.impacts,
                                   aes(x=Treatment,y=PA)) +
  geom_bar(aes(fill=Treatment,width=0.5),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=PA[Treatment=="Control"],yend=PA[Treatment=="Control"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=PA[Treatment=="MPA"],yend=PA[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=PA[Treatment=="Control"],yend=PA[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_point(data=TNTC.impact.arrows,
             aes(x=x,y=PA,shape=""),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=TNTC.sig.pos["PA",]-0.01,x=0.65,
           label=TNTC.sig.labs["PA","impact.labs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  geom_errorbar(aes(ymin=PA-PAErr,
                    ymax=PA+PAErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position="dodge",
                show.legend=F) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=ifelse(TNTC.2yr.impacts$PA[TNTC.2yr.impacts$Treatment=="MPA"]>
                                     TNTC.2yr.impacts$PA[TNTC.2yr.impacts$Treatment=="Control"],24,25),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.pa.labs.i + plot.guides.MPAimpact.summ

# 3.4 Marine Tenure
TNTC.MPAimpact.summ.mt.i <- ggplot(data=TNTC.2yr.impacts,
                                   aes(x=Treatment,y=MT)) +
  geom_bar(aes(fill=Treatment,width=0.5),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=MT[Treatment=="Control"],yend=MT[Treatment=="Control"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=MT[Treatment=="MPA"],yend=MT[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=MT[Treatment=="Control"],yend=MT[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_point(data=TNTC.impact.arrows,
             aes(x=x,y=MT,shape=""),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_errorbar(aes(ymin=MT-MTErr,
                    ymax=MT+MTErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position="dodge",
                show.legend=F) +
  annotate("text",y=TNTC.sig.pos["MT",],x=0.65,
           label=TNTC.sig.labs["MT","impact.labs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=ifelse(TNTC.2yr.impacts$MT[TNTC.2yr.impacts$Treatment=="MPA"]>
                                     TNTC.2yr.impacts$MT[TNTC.2yr.impacts$Treatment=="Control"],24,25),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.mt.labs.i + plot.guides.MPAimpact.summ

# 3.5 School Enrollment
TNTC.MPAimpact.summ.se.i <- ggplot(data=TNTC.2yr.impacts,
                                   aes(x=Treatment,y=SE)) +
  geom_bar(aes(fill=Treatment,width=0.5),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=SE[Treatment=="Control"],yend=SE[Treatment=="Control"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=SE[Treatment=="MPA"],yend=SE[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=SE[Treatment=="Control"],yend=SE[Treatment=="MPA"]),
               lineend="square",
               size=1) +
  geom_point(data=TNTC.impact.arrows,
             aes(x=x,y=SE,shape=""),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_errorbar(aes(ymin=SE-SEErr,
                    ymax=SE+SEErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position="dodge",
                show.legend=F) +
  annotate("text",y=TNTC.sig.pos["SE",],x=0.65,
           label=TNTC.sig.labs["SE","impact.labs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=ifelse(TNTC.2yr.impacts$SE[TNTC.2yr.impacts$Treatment=="MPA"]>
                                     TNTC.2yr.impacts$SE[TNTC.2yr.impacts$Treatment=="Control"],24,25),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.se.labs.i + plot.guides.MPAimpact.summ

# ----
############################################################
#
# SECTION 4: Standardized, "Snapshot" Plot, Big Five
#
############################################################
# ----

# 4.1 Snapshot plot, all Big Five standardized impacts on one plot
snapshot.TNTC.MPAimpact.summ <- ggplot(data=TNTC.2yr.std.impacts,
                                       aes(x=Domain,
                                           y=Std.impact)) +
  geom_bar(data=seascape.impact.2yr,
           aes(x=Domain,
               y=Std.impact,
               linetype=""),
           fill="#505050",
           alpha=0.4,
           stat="identity",
           position="dodge",
           width=0.25,
           colour=alpha("black",0.6)) +
  geom_bar(aes(fill=impact.direction),
           stat="identity",
           position="dodge",
           width=1,
           colour=alpha("black",0.4)) +
  geom_errorbar(aes(x=Domain,
                    ymin=Std.impact-Std.se,
                    ymax=Std.impact+Std.se,
                    colour=impact.direction),
                width=0.05,
                size=0.25,
                show.legend=F) +
  geom_hline(aes(yintercept=0),
             linetype="solid",
             size=1,
             colour="#505050",
             show.legend=F) +
  scale_x_discrete(labels=TNTC.sig.labs[,"snapshot.labs"]) +
  scale_y_continuous(limits=c(-1,1)) +
  scale_fill_manual(values=c(alpha("#65B65E",0.7),alpha("#1B448B",0.7)),
                    name="Direction of\nImpact",
                    labels=c("Positive","Negative")) +
  scale_colour_manual(values=c(alpha("#2B5027",0.7),alpha("#072258",0.7))) +
  scale_linetype_manual(values=1,
                        name="Bird's Head\nSeascape\nMean",
                        guide="legend") +
  labs(x="Human Well-Being Domain\n ",y="\n MPA Impact",
       title="Social Impacts of Teluk Cenderawasih MPA") +
  coord_flip() + snapshot.plot.theme.MPAimpact.summ + 
  snapshot.plot.guide.MPAimpact.summ 