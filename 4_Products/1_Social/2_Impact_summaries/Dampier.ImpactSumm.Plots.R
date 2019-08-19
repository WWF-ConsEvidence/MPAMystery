# 
# code:  Dampier Impact Summary Plots - 2 and 4 year impacts
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/ImpactSummaries/BHS
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: January 2018
# 
# ---- inputs ----
#  1) Source Dampier.ImpactSumm.SigTests.R 
#     - Dependencies: BHS_MPA_Mystery.R
#                     Matching_2yr_impacts.R
#                     BHS_impact_data.R
#                     Function_summarise_bigfive_impacts.R
#                     
# 
# ---- code sections ----
#  1) Source Dampier Impact Summary Statistical Code
#  2) Status and Trend Plots, Big Five
#  3) Impact Plots, Big Five
#  4) Standardized, "Snapshot" Plot, Big Five
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Source Dampier Impact Summary Statistical Code ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

  
  
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Status and Trend Plots, Big Five ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Food Security ----

Damp.MPAimpact.summ.fs.st <- ggplot(data=Damp.status.trend,
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
  annotate("rect",xmin=2.7,xmax=3.3,ymin=0.05,ymax=0.35,alpha=0.6,fill="white",colour="#FFFFFF") +
  annotate("rect",xmin=2.6,xmax=3.3,ymin=1.61,ymax=1.91,alpha=0.6,fill="white",colour="#FFFFFF") +
  annotate("text",x=3.3,y=4.24,label="Food secure",alpha=0.4,fontface="italic",size=3) +
  annotate("text",x=3.1,y=1.76,label="Food insecure without hunger",alpha=0.4,fontface="italic",size=3) +
  annotate("text",x=3.1,y=0.2,label="Food insecure with hunger",alpha=0.4,fontface="italic",size=3) +
  scale_x_discrete(labels=c(as.character(Damp.sig.labs["FS","xaxis.st.baselabs"]),
                            as.character(Damp.sig.labs["FS","xaxis.st.2yrlabs"]),
                            as.character(Damp.sig.labs["FS","xaxis.st.4yrlabs"]))) +
  scale_y_continuous(limits=c(0,6.06),
                     expand=c(0,0)) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  fs.st.plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.fs.labs.st


# ---- 2.2 Material Assets ----

Damp.MPAimpact.summ.ma.st <- ggplot(data=Damp.status.trend,
                                    aes(Year,
                                        MA)) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  scale_x_discrete(labels=c(as.character(Damp.sig.labs["MA","xaxis.st.baselabs"]),
                            as.character(Damp.sig.labs["MA","xaxis.st.2yrlabs"]),
                            as.character(Damp.sig.labs["MA","xaxis.st.4yrlabs"]))) +
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


# ---- 2.3 Place Attachment ----

Damp.MPAimpact.summ.pa.st <- ggplot(data=Damp.status.trend,
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
  scale_x_discrete(labels=c(as.character(Damp.sig.labs["PA","xaxis.st.baselabs"]),
                            as.character(Damp.sig.labs["PA","xaxis.st.2yrlabs"]),
                            as.character(Damp.sig.labs["PA","xaxis.st.4yrlabs"]))) +
  scale_y_continuous(limits=c(0,5),
                     expand=c(0,0)) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.pa.labs.st


# ---- 2.4 Marine Tenure ----

Damp.MPAimpact.summ.mt.st <- ggplot(data=Damp.status.trend,
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
  scale_x_discrete(labels=c(as.character(Damp.sig.labs["MT","xaxis.st.baselabs"]),
                            as.character(Damp.sig.labs["MT","xaxis.st.2yrlabs"]),
                            as.character(Damp.sig.labs["MT","xaxis.st.4yrlabs"]))) +
  scale_fill_manual(values=fill.cols.MPAimpact.summ) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  plot.theme.MPAimpact.summ + plot.guides.MPAimpact.summ + plot.mt.labs.st


# ---- 2.5 School Enrollment ----

Damp.MPAimpact.summ.se.st <- ggplot(data=Damp.status.trend,
                                    aes(Year,
                                        SE)) +
  geom_bar(aes(fill=Treatment),
           width=0.5,
           stat="identity",
           position="dodge",
           show.legend=F) +
  scale_x_discrete(labels=c(as.character(Damp.sig.labs["SE","xaxis.st.baselabs"]),
                            as.character(Damp.sig.labs["SE","xaxis.st.2yrlabs"]),
                            as.character(Damp.sig.labs["SE","xaxis.st.4yrlabs"]))) +
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


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Impact Plots, Big Five ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


pacman::p_load(rio,ggplot2,tidyr,dplyr)

source('2_Functions/3_Plotting/Function_plotthemes.R')



macp.flotim.impacts <- 
  import('x_Flat_data_files/1_Social/Outputs/impact_analysis/Flores Timur/macp_plots_output.csv') %>%
  filter(MPAID==16 & !(grepl("_z",Response))) %>%
  mutate(p.val=2*pnorm(-abs(z.score)),
         sig.labs=ifelse(p.val<=0.01,"*\n*\n*",
                         ifelse(p.val<=0.05 & p.val>0.01, "*\n*",
                                ifelse(p.val<=0.1 & p.val>0.05, "*", ""))),
         term=factor(term,ordered=T,levels=c("Impact","Treatment_Trend","Control_Trend"))) 


impact.arrows <- data.frame(mapply(i=t(macp.flotim.impacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                                            j=t(macp.flotim.impacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                                            function(i,j){
                                              if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                                if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                                  if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                      if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                        if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                            }),
                                     x=rep(0.3,4)) %>%
  rename(FS=X1,MA=X2,MT=X3,PA=X4,SE=X5)


plotrange <- mapply(imin=t(macp.flotim.impacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                         imax=t(macp.flotim.impacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                         jmin=t(macp.flotim.impacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                         jmax=t(macp.flotim.impacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                         function(imin,imax,jmin,jmax){
                           max <- ifelse((imax>0 & jmax<=0) | (imax>0 & jmax>0 & imax>jmax),imax,
                                             ifelse((imax<=0 & jmax>0) | (imax>0 & jmax>0 & imax<jmax),jmax,0))
                           min <- ifelse((imin<0 & jmin>0) | (imin<0 & jmin<0 & imin<jmin),imin,
                                             ifelse((imin>0 & jmin<0) | (imin<0 & jmin<0 & imin>jmin),jmin,0))
                           abs(max)+abs(min)
                         })

sig.pos <- data.frame(TwoYr=mapply(i=t(macp.flotim.impacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                                        j=t(macp.flotim.impacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                                        k=t(macp.flotim.impacts%>%filter(term=="Impact")%>%select(sig.labs)),
                                        range=plotrange,
                                        function(i,j,k,range){
                                          if(k=="*\n*\n*" & i<j) {i-0.03*range} else
                                            if(k=="*\n*\n*" & i>j) {i+0.03*range} else
                                              if(k=="*\n*" & i<j) {i-0.02*range} else
                                                if(k=="*\n*" & i>j) {i+0.02*range} else
                                                  if(k=="*" & i<j) {i-0.01*range} else
                                                    if(k=="*" & i>j) {i+0.01*range} else {0}
                                        }))

rownames(sig.pos) <- c("FS","MA","MT","PA","SE")



fill.cols.MPAimpacts <- c("Treatment_Trend"=alpha("#2C7FB8",0.95),"Control_Trend"=alpha("#6B6B6B",0.4))



# ---- 3.1 Food Security ----

MPAimpact.summ.fs.i <- ggplot(data=macp.flotim.impacts[!grepl("Impact",macp.flotim.impacts$term) & 
                                                              macp.flotim.impacts$Response=="FSIndex",],
                                   aes(x=term,y=estimate)) +
  geom_bar(aes(fill=term),
           stat="identity",
           position="dodge",
           width=0.9,
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.3,xend=0.4,y=estimate[term=="Control_Trend"],
                   yend=estimate[term=="Control_Trend"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.3,xend=0.4,y=estimate[term=="Treatment_Trend"],
                   yend=estimate[term=="Treatment_Trend"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.3,xend=0.3,y=estimate[term=="Control_Trend"],
                   yend=estimate[term=="Treatment_Trend"]),
               lineend="square",
               size=1) +
  geom_point(data=impact.arrows,
             aes(x=x,y=FS,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["FS","TwoYr"],x=0.35,
           label=macp.flotim.impacts%>%filter(term=="Impact" & Response=="FSIndex")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="FSIndex")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="FSIndex")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.MPAimpact.summ + plot.fs.labs.i + plot.guides.MPAimpact.summ


# ---- 3.2 Material Assets ----

Damp.MPAimpact.summ.ma.i <- ggplot(data=Damp.impacts,
                                   aes(x=Year,y=MA)) +
  geom_bar(aes(fill=Treatment,width=0.5,group=Treatment),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=MA[Treatment=="Control" & Year=="2 Year"],
                   yend=MA[Treatment=="Control"& Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=MA[Treatment=="MPA" & Year=="2 Year"],
                   yend=MA[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=MA[Treatment=="Control" & Year=="2 Year"],
                   yend=MA[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=MA[Treatment=="Control" & Year=="4 Year"],
                   yend=MA[Treatment=="Control"& Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=MA[Treatment=="MPA" & Year=="4 Year"],
                   yend=MA[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.6,y=MA[Treatment=="Control" & Year=="4 Year"],
                   yend=MA[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_errorbar(aes(ymin=MA-MAErr,
                    ymax=MA+MAErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  geom_point(data=Damp.impact.arrows.2yr,
             aes(x=x,y=MA,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_point(data=Damp.impact.arrows.4yr,
             aes(x=x,y=MA,shape="4yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=Damp.sig.pos["MA","TwoYr"],x=0.65,
           label=Damp.sig.labs["MA","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  annotate("text",y=Damp.sig.pos["MA","FourYr"],x=1.65,
           label=Damp.sig.labs["MA","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=c("2yr"=ifelse(Damp.impacts$MA[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="2 Year"]>
                                             Damp.impacts$MA[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="2 Year"],24,25),
                              "4yr"=ifelse(Damp.impacts$MA[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="4 Year"]>
                                             Damp.impacts$MA[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="4 Year"],24,25)),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.ma.labs.i + plot.guides.MPAimpact.summ


# ---- 3.3 Place Attachment ----

Damp.MPAimpact.summ.pa.i <- ggplot(data=Damp.impacts,
                                   aes(x=Year,y=PA)) +
  geom_bar(aes(fill=Treatment,width=0.5,group=Treatment),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=PA[Treatment=="Control" & Year=="2 Year"],
                   yend=PA[Treatment=="Control"& Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=PA[Treatment=="MPA" & Year=="2 Year"],
                   yend=PA[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=PA[Treatment=="Control" & Year=="2 Year"],
                   yend=PA[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=PA[Treatment=="Control" & Year=="4 Year"],
                   yend=PA[Treatment=="Control"& Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=PA[Treatment=="MPA" & Year=="4 Year"],
                   yend=PA[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.6,y=PA[Treatment=="Control" & Year=="4 Year"],
                   yend=PA[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_errorbar(aes(ymin=PA-PAErr,
                    ymax=PA+PAErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  geom_point(data=Damp.impact.arrows.2yr,
             aes(x=x,y=PA,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_point(data=Damp.impact.arrows.4yr,
             aes(x=x,y=PA,shape="4yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=Damp.sig.pos["PA","TwoYr"],x=0.65,
           label=Damp.sig.labs["PA","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  annotate("text",y=Damp.sig.pos["PA","FourYr"],x=1.65,
           label=Damp.sig.labs["PA","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=c("2yr"=ifelse(Damp.impacts$PA[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="2 Year"]>
                                             Damp.impacts$PA[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="2 Year"],24,25),
                              "4yr"=ifelse(Damp.impacts$PA[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="4 Year"]>
                                             Damp.impacts$PA[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="4 Year"],24,25)),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.pa.labs.i + plot.guides.MPAimpact.summ

# ---- 3.4 Marine Tenure ----

Damp.MPAimpact.summ.mt.i <- ggplot(data=Damp.impacts,
                                   aes(x=Year,y=MT)) +
  geom_bar(aes(fill=Treatment,width=0.5,group=Treatment),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=MT[Treatment=="Control" & Year=="2 Year"],
                   yend=MT[Treatment=="Control"& Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=MT[Treatment=="MPA" & Year=="2 Year"],
                   yend=MT[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=MT[Treatment=="Control" & Year=="2 Year"],
                   yend=MT[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=MT[Treatment=="Control" & Year=="4 Year"],
                   yend=MT[Treatment=="Control"& Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=MT[Treatment=="MPA" & Year=="4 Year"],
                   yend=MT[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.6,y=MT[Treatment=="Control" & Year=="4 Year"],
                   yend=MT[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_errorbar(aes(ymin=MT-MTErr,
                    ymax=MT+MTErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  geom_point(data=Damp.impact.arrows.2yr,
             aes(x=x,y=MT,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_point(data=Damp.impact.arrows.4yr,
             aes(x=x,y=MT,shape="4yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=Damp.sig.pos["MT","TwoYr"],x=0.65,
           label=Damp.sig.labs["MT","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  annotate("text",y=Damp.sig.pos["MT","FourYr"],x=1.65,
           label=Damp.sig.labs["MT","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=c("2yr"=ifelse(Damp.impacts$MT[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="2 Year"]>
                                             Damp.impacts$MT[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="2 Year"],24,25),
                              "4yr"=ifelse(Damp.impacts$MT[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="4 Year"]>
                                             Damp.impacts$MT[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="4 Year"],24,25)),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.mt.labs.i + plot.guides.MPAimpact.summ

# ---- 3.5 School Enrollment ----

Damp.MPAimpact.summ.se.i <- ggplot(data=Damp.impacts,
                                   aes(x=Year,y=SE)) +
  geom_bar(aes(fill=Treatment,width=0.5,group=Treatment),
           stat="identity",
           position="dodge",
           show.legend=F) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030",
             show.legend=F) +
  geom_segment(aes(x=0.6,xend=0.7,y=SE[Treatment=="Control" & Year=="2 Year"],
                   yend=SE[Treatment=="Control"& Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.7,y=SE[Treatment=="MPA" & Year=="2 Year"],
                   yend=SE[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=0.6,xend=0.6,y=SE[Treatment=="Control" & Year=="2 Year"],
                   yend=SE[Treatment=="MPA" & Year=="2 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=SE[Treatment=="Control" & Year=="4 Year"],
                   yend=SE[Treatment=="Control"& Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.7,y=SE[Treatment=="MPA" & Year=="4 Year"],
                   yend=SE[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_segment(aes(x=1.6,xend=1.6,y=SE[Treatment=="Control" & Year=="4 Year"],
                   yend=SE[Treatment=="MPA" & Year=="4 Year"]),
               lineend="square",
               size=1) +
  geom_errorbar(aes(ymin=SE-SEErr,
                    ymax=SE+SEErr,
                    colour=Treatment),
                width=0.055,
                size=0.65,
                position=position_dodge(width=0.5),
                show.legend=F) +
  geom_point(data=Damp.impact.arrows.2yr,
             aes(x=x,y=SE,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  geom_point(data=Damp.impact.arrows.4yr,
             aes(x=x,y=SE,shape="4yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=Damp.sig.pos["SE","TwoYr"],x=0.65,
           label=Damp.sig.labs["SE","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  annotate("text",y=Damp.sig.pos["SE","FourYr"],x=1.65,
           label=Damp.sig.labs["SE","impact.2yrlabs"],
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpact.summ) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_colour_manual(values=err.cols.MPAimpact.summ) +
  scale_shape_manual(values=c("2yr"=ifelse(Damp.impacts$SE[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="2 Year"]>
                                             Damp.impacts$SE[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="2 Year"],24,25),
                              "4yr"=ifelse(Damp.impacts$SE[Damp.impacts$Treatment=="MPA" & Damp.impacts$Year=="4 Year"]>
                                             Damp.impacts$SE[Damp.impacts$Treatment=="Control" & Damp.impacts$Year=="4 Year"],24,25)),
                     labels="Direction of\nImpact (+/-)") +
  plot.theme.MPAimpact.summ + plot.se.labs.i + plot.guides.MPAimpact.summ


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Standardized, "Snapshot" Plot, Big Five ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 4.1 Snapshot plot, all Big Five standardized impacts on one plot ----

# - 2 year impacts
snapshot.Damp.MPAimpact.summ.2yr <- ggplot(data=Damp.2yr.std.impacts,
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
           colour=alpha("black",0.6),
           show.legend=F) +
  geom_bar(aes(fill=impact.direction),
           stat="identity",
           position="dodge",
           width=1,
           colour=alpha("black",0.4),
           show.legend=F) +
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
  scale_x_discrete(labels=Damp.sig.labs[,"snapshot.2yrlabs"]) +
  scale_y_continuous(limits=c(-0.75,0.75),
                     breaks=c(seq(-0.5,0.5,by=0.25))) +
  scale_fill_manual(values=c(alpha("#65B65E",0.7),alpha("#1B448B",0.7)),
                    name="Direction of\nImpact",
                    labels=c("Positive","Negative")) +
  scale_colour_manual(values=c(alpha("#2B5027",0.7),alpha("#072258",0.7))) +
  scale_linetype_manual(values=1,
                        name="Bird's Head\nSeascape\nMean",
                        guide="legend") +
  labs(x="",y="\n MPA Impact",title="Two Year Impacts") +
  coord_flip() + snapshot.plot.theme.MPAimpact.summ + 
  snapshot.plot.guide.MPAimpact.summ 

# - 4 year impacts
snapshot.Damp.MPAimpact.summ.4yr <- ggplot(data=Damp.4yr.std.impacts,
                                           aes(x=Domain,
                                               y=Std.impact)) +
  geom_bar(data=seascape.impact.4yr,
           aes(x=Domain,
               y=Std.impact,
               linetype=""),
           fill="#505050",
           alpha=0.4,
           stat="identity",
           position="dodge",
           width=0.25,
           colour=alpha("black",0.6),
           show.legend=F) +
  geom_bar(aes(fill=impact.direction),
           stat="identity",
           position="dodge",
           width=1,
           colour=alpha("black",0.4),
           show.legend=F) +
  geom_errorbar(aes(x=Domain,
                    ymin=Lower.CI,
                    ymax=Upper.CI,
                    colour=impact.direction),
                width=0.05,
                size=0.25,
                show.legend=F) +
  geom_hline(aes(yintercept=0),
             linetype="solid",
             size=1,
             colour="#505050",
             show.legend=F) +
  scale_x_discrete(labels=Damp.sig.labs[,"snapshot.4yrlabs"]) +
  scale_y_continuous(limits=c(-0.75,0.75),
                     breaks=c(seq(-0.5,0.5,by=0.25))) +
  scale_fill_manual(values=c(alpha("#65B65E",0.7),alpha("#1B448B",0.7)),
                    name="Direction of\nImpact",
                    labels=c("Positive","Negative")) +
  scale_colour_manual(values=c(alpha("#2B5027",0.7),alpha("#072258",0.7))) +
  scale_linetype_manual(values=1,
                        name="Bird's Head\nSeascape\nMean",
                        guide="legend") +
  labs(x="",y="\n MPA Impact",title="Four Year Impacts") +
  coord_flip() + snapshot.plot.theme.MPAimpact.summ + 
  snapshot.plot.guide.MPAimpact.summ 


# ---- 4.2 Arrange grob ----

Damp.snapshot.legend.plot <-
  ggplot(data=Damp.4yr.std.impacts,
         aes(x=Domain,
             y=Std.impact)) +
  geom_bar(data=seascape.impact.4yr,
           aes(x=Domain,
               y=Std.impact,
               linetype=""),
           stat="identity") +
  geom_bar(aes(fill=impact.direction),
           stat="identity") +
  scale_fill_manual(values=c(alpha("#65B65E",0.7),alpha("#1B448B",0.7)),
                    name="Direction of\nImpact",
                    labels=c("Positive","Negative")) +
  scale_colour_manual(values=c(alpha("#2B5027",0.7),alpha("#072258",0.7))) +
  scale_linetype_manual(values=1,
                        name="Bird's Head\nSeascape\nMean",
                        guide="legend") +
  coord_flip() + snapshot.plot.theme.MPAimpact.summ + 
  snapshot.plot.guide.MPAimpact.summ 

Damp.snapshot.legend <- g_legend(Damp.snapshot.legend.plot)

snapshot.Damp.MPAimpact.bothyr <-
  grid.arrange(arrangeGrob(snapshot.Damp.MPAimpact.summ.2yr,
                           snapshot.Damp.MPAimpact.summ.4yr,
                           ncol=2),
               Damp.snapshot.legend,
               ncol=2,widths=c(3,0.5))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


dir.create(paste("2_Social/FlatDataFiles/BHS/ImpactSummOutput/Dampier/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("2_Social/FlatDataFiles/BHS/ImpactSummOutput/Dampier/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")


# ---- 5.1 FOOD SECURITY ----

png(paste(FigureFileName,"FS.status.trend.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.fs.st)
dev.off()

png(paste(FigureFileName,"FS.impact.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.fs.i)
dev.off()


# ---- 5.2 MATERIAL ASSETS ----

png(paste(FigureFileName,"MA.status.trend.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.ma.st)
dev.off()

png(paste(FigureFileName,"MA.impact.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.ma.i)
dev.off()


# ---- 5.3 MARINE TENURE ----

png(paste(FigureFileName,"MT.status.trend.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.mt.st)
dev.off()

png(paste(FigureFileName,"MT.impact.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.mt.i)
dev.off()


# ---- 5.4 PLACE ATTACHMENT ----

png(paste(FigureFileName,"PA.status.trend.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.pa.st)
dev.off()

png(paste(FigureFileName,"PA.impact.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.pa.i)
dev.off()


# ---- 5.5 SCHOOL ENROLLMENT ----

png(paste(FigureFileName,"SE.status.trend.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.se.st)
dev.off()

png(paste(FigureFileName,"SE.impact.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Damp.MPAimpact.summ.se.i)
dev.off()


# ---- 5.6 SNAPSHOT PLOT ----

png(paste(FigureFileName,"standardized.impacts.png",sep="/"),
    units="in",height=5,width=12,res=400)
grid.newpage()
grid.draw(snapshot.Damp.MPAimpact.bothyr)
dev.off()
