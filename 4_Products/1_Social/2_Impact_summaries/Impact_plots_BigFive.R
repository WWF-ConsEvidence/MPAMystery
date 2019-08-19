# 
# code:  Impact Summary Plots for Big Five indicators - 2 year impacts
# 
# github: WWF-ConsEvidence/MPAMystery/4_Products/1_Social/ImpactSummaries
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: August 2019
# 
# ---- inputs ----
#  1) Source Function_plotthemes.R
#  2) MPA-level impact outputs from DiD regression analysis
#                     
# 
# ---- code sections ----
#  1) Source plot themes, call in data
#  2) Impact Plots, Big Five
#  3) Standardized, "Snapshot" Plot, Big Five
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Source plot themes, call in data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


pacman::p_load(rio,ggplot2,tidyr,dplyr)

source('2_Functions/3_Plotting/Function_plotthemes.R')


# ---- 1.2 Call in MPA-specific impact outputs from DiD regression ----

macp.flotim.impacts <- 
  import('x_Flat_data_files/1_Social/Outputs/impact_analysis/Flores Timur/macp_plots_output.csv') %>%
  filter(MPAID==16 & !(grepl("_z",Response))) %>%
  mutate(p.val=2*pnorm(-abs(z.score)),
         sig.labs=ifelse(p.val<=0.01,"*\n*\n*",
                         ifelse(p.val<=0.05 & p.val>0.01, "*\n*",
                                ifelse(p.val<=0.1 & p.val>0.05, "*", ""))),
         term=factor(term,ordered=T,levels=c("Impact","Treatment_Trend","Control_Trend"))) 

macp.flotim.impacts.subclass <- 
  import('x_Flat_data_files/1_Social/Outputs/impact_analysis/Flores Timur/macp_plots_output_Asset_subClasses.csv') %>%
  subset(Response=="Household_asset" | Response=="BoatNoMotor" | Response=="Boats_motorized" | Response=="Vehicles") %>%
  mutate(p.val=2*pnorm(-abs(z.score)),
         sig.labs=ifelse(p.val<=0.01,"*\n*\n*",
                         ifelse(p.val<=0.05 & p.val>0.01, "*\n*",
                                ifelse(p.val<=0.1 & p.val>0.05, "*", ""))),
         term=factor(term,ordered=T,levels=c("Impact","Treatment_Trend","Control_Trend"))) 

macp.flotim.allimpacts <- 
  rbind.data.frame(macp.flotim.impacts,macp.flotim.impacts.subclass)

# ---- 1.3 Calculate plot-specific variables for Big Five & Assets sub-classes ----

impact.arrows <- data.frame(mapply(i=t(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                                            j=t(macp.flotim.allimpacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                                            function(i,j){
                                              if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                                if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                                  if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                      if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                        if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                            }),
                                     x=rep(0.3,4)) %>%
  rename(FS=X1,MA=X2,MT=X3,PA=X4,SE=X5,HHAsset=X6,BoatNoMotor=X7,BoatMotor=X8,Vehicle=X9)


plotrange <- mapply(imin=t(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                         imax=t(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                         jmin=t(macp.flotim.allimpacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                         jmax=t(macp.flotim.allimpacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                         function(imin,imax,jmin,jmax){
                           max <- ifelse((imax>0 & jmax<=0) | (imax>0 & jmax>0 & imax>jmax),imax,
                                             ifelse((imax<=0 & jmax>0) | (imax>0 & jmax>0 & imax<jmax),jmax,0))
                           min <- ifelse((imin<0 & jmin>0) | (imin<0 & jmin<0 & imin<jmin),imin,
                                             ifelse((imin>0 & jmin<0) | (imin<0 & jmin<0 & imin>jmin),jmin,0))
                           abs(max)+abs(min)
                         })

sig.pos <- data.frame(TwoYr=mapply(i=t(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend")%>%select(estimate)),
                                        j=t(macp.flotim.allimpacts%>%filter(term=="Control_Trend")%>%select(estimate)),
                                        k=t(macp.flotim.allimpacts%>%filter(term=="Impact")%>%select(sig.labs)),
                                        range=plotrange+plotrange*0.6,
                                        function(i,j,k,range){
                                          if(k=="*\n*\n*" & i<j) {i-0.03*range} else
                                            if(k=="*\n*\n*" & i>j) {i+0.03*range} else
                                              if(k=="*\n*" & i<j) {i-0.02*range} else
                                                if(k=="*\n*" & i>j) {i+0.02*range} else
                                                  if(k=="*" & i<j) {i-0.01*range} else
                                                    if(k=="*" & i>j) {i+0.01*range} else {0}
                                        }))

rownames(sig.pos) <- c("FS","MA","MT","PA","SE","HHAsset","BoatNoMotor","BoatMotor","Vehicle")



fill.cols.MPAimpacts <- c("Treatment_Trend"=alpha("#2C7FB8",0.95),"Control_Trend"=alpha("#9B9B9B",0.95))


# -- EXAMPLE IMPACT PLOT FOR INTERPRETATION GUIDE --


MPAimpact.summ.example.i <- ggplot(data=data.frame(Response=rep("Indicator",2),
                                                   term=factor(c("Treatment_Trend","Control_Trend"),ordered=T, levels=c("Treatment_Trend","Control_Trend")),
                                                   estimate=c(1,-0.8)),
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
  geom_point(data=data.frame(x=rep(0.3,4),Indicator=c(0.7,0.3,-0.1,-0.5)),
             aes(x=x,y=Indicator,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=1.1,x=0.35,
           label="*\n*\n*",
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpacts,
                    name="Group") +
  scale_x_discrete(labels=impact.x.labs) +
  scale_y_continuous(limits=c(-0.9,1.2)) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=24,
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + labs(x="",y="Change in Indicator Value since Baseline",title="") + plot.guides.MPAimpact.summ


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Impact Plots, Big Five ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Food Security ----

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
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpacts,
                    name="Group") +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="FSIndex")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="FSIndex")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.fs.labs.i + plot.guides.MPAimpact.summ


# ---- 2.2 Material Assets ----

MPAimpact.summ.ma.i <- ggplot(data=macp.flotim.impacts[!grepl("Impact",macp.flotim.impacts$term) & 
                                                              macp.flotim.impacts$Response=="MAIndex",],
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
             aes(x=x,y=MA,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["MA","TwoYr"],x=0.35,
           label=macp.flotim.impacts%>%filter(term=="Impact" & Response=="MAIndex")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="MAIndex")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="MAIndex")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.ma.labs.i + plot.guides.MPAimpact.summ


# -- Material assets sub-classes --
# - Household items

MPAimpact.summ.ma.hh.i <- ggplot(data=macp.flotim.allimpacts[!grepl("Impact",macp.flotim.allimpacts$term) & 
                                                               macp.flotim.allimpacts$Response=="Household_asset",],
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
             aes(x=x,y=HHAsset,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["HHAsset","TwoYr"],x=0.35,
           label=macp.flotim.allimpacts%>%filter(term=="Impact" & Response=="Household_asset")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend" & Response=="Household_asset")%>%select(estimate)>
                                             macp.flotim.allimpacts%>%filter(term=="Control_Trend" & Response=="Household_asset")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.ma.hh.labs.i + plot.guides.MPAimpact.summ


# - Boats no motor

MPAimpact.summ.ma.boatnomotor.i <- ggplot(data=macp.flotim.allimpacts[!grepl("Impact",macp.flotim.allimpacts$term) & 
                                                               macp.flotim.allimpacts$Response=="BoatNoMotor",],
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
             aes(x=x,y=BoatNoMotor,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["BoatNoMotor","TwoYr"],x=0.35,
           label=macp.flotim.allimpacts%>%filter(term=="Impact" & Response=="BoatNoMotor")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend" & Response=="Household_asset")%>%select(estimate)>
                                             macp.flotim.allimpacts%>%filter(term=="Control_Trend" & Response=="Household_asset")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.ma.boatnomotor.labs.i + plot.guides.MPAimpact.summ


# - Motorized boats

MPAimpact.summ.ma.boatmotor.i <- ggplot(data=macp.flotim.allimpacts[!grepl("Impact",macp.flotim.allimpacts$term) & 
                                                                        macp.flotim.allimpacts$Response=="Boats_motorized",],
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
             aes(x=x,y=BoatMotor,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["BoatMotor","TwoYr"],x=0.35,
           label=macp.flotim.allimpacts%>%filter(term=="Impact" & Response=="Boats_motorized")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend" & Response=="Household_asset")%>%select(estimate)>
                                             macp.flotim.allimpacts%>%filter(term=="Control_Trend" & Response=="Household_asset")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.ma.boatmotor.labs.i + plot.guides.MPAimpact.summ


# - Land vehicles

MPAimpact.summ.ma.vehicle.i <- ggplot(data=macp.flotim.allimpacts[!grepl("Impact",macp.flotim.allimpacts$term) & 
                                                                        macp.flotim.allimpacts$Response=="Vehicles",],
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
             aes(x=x,y=Vehicle,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["Vehicle","TwoYr"],x=0.35,
           label=macp.flotim.allimpacts%>%filter(term=="Impact" & Response=="Vehicles")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.allimpacts%>%filter(term=="Treatment_Trend" & Response=="Household_asset")%>%select(estimate)>
                                             macp.flotim.allimpacts%>%filter(term=="Control_Trend" & Response=="Household_asset")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.ma.vehicles.labs.i + plot.guides.MPAimpact.summ



# ---- 2.3 Place Attachment ----

MPAimpact.summ.pa.i <- ggplot(data=macp.flotim.impacts[!grepl("Impact",macp.flotim.impacts$term) & 
                                                         macp.flotim.impacts$Response=="PAIndex",],
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
             aes(x=x,y=PA,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["PA","TwoYr"],x=0.35,
           label=macp.flotim.impacts%>%filter(term=="Impact" & Response=="PAIndex")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="PAIndex")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="PAIndex")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.pa.labs.i + plot.guides.MPAimpact.summ


# ---- 2.4 Marine Tenure ----

MPAimpact.summ.mt.i <- ggplot(data=macp.flotim.impacts[!grepl("Impact",macp.flotim.impacts$term) & 
                                                         macp.flotim.impacts$Response=="MTIndex",],
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
             aes(x=x,y=MT,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["MT","TwoYr"],x=0.35,
           label=macp.flotim.impacts%>%filter(term=="Impact" & Response=="MTIndex")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="MTIndex")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="MTIndex")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.mt.labs.i + plot.guides.MPAimpact.summ


# ---- 2.5 School Enrollment ----

MPAimpact.summ.se.i <- ggplot(data=macp.flotim.impacts[!grepl("Impact",macp.flotim.impacts$term) & 
                                                         macp.flotim.impacts$Response=="SERate",],
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
             aes(x=x,y=SE,shape="2yr"),
             fill="black",
             size=2.75,
             show.legend=F) +
  annotate("text",y=sig.pos["SE","TwoYr"],x=0.35,
           label=macp.flotim.impacts%>%filter(term=="Impact" & Response=="SERate")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("Treatment_Trend","Control_Trend"),
                    values=fill.cols.MPAimpacts) +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="SERate")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="SERate")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.se.labs.i + plot.guides.MPAimpact.summ



# ---- LEGEND ----

MPAimpact.summ.legend <- ggplot(data=macp.flotim.impacts[!grepl("Impact",macp.flotim.impacts$term) & 
                                                         macp.flotim.impacts$Response=="FSIndex",],
                              aes(x=term,y=estimate)) +
  geom_bar(aes(fill=term),
           stat="identity",
           position="dodge",
           width=0.9) +
  geom_hline(aes(yintercept=0,size="Baseline Score"),
             linetype="longdash",
             colour="#303030") +
  geom_segment(aes(x=0.3,xend=0.4,y=estimate[term=="Control_Trend"],
                   yend=estimate[term=="Control_Trend"], colour="black"),
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
             size=2.75) +
  annotate("text",y=sig.pos["FS","TwoYr"],x=0.35,
           label=macp.flotim.impacts%>%filter(term=="Impact" & Response=="FSIndex")%>%select(sig.labs),
           colour="black",
           size=4,
           lineheight=0.4) +
  scale_fill_manual(labels=c("MPA","Control"),
                    values=fill.cols.MPAimpacts,
                    name="Group") +
  scale_colour_manual(labels="Impact of MPA",
                      values="black") +
  scale_x_discrete(labels=impact.x.labs) +
  scale_size_manual(values=0.75) +
  scale_shape_manual(values=c("2yr"=ifelse(macp.flotim.impacts%>%filter(term=="Treatment_Trend" & Response=="FSIndex")%>%select(estimate)>
                                             macp.flotim.impacts%>%filter(term=="Control_Trend" & Response=="FSIndex")%>%select(estimate),24,25)),
                     labels="Direction of\nImpact (+/-)") +
  expand_limits(x=c(-0.2,3)) +
  plot.theme.impact + plot.fs.labs.i + plot.guides.MPAimpact.summ


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Standardized, "Snapshot" Plot, Big Five ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

macp.flotim.std.impacts <- 
  import('x_Flat_data_files/1_Social/Outputs/impact_analysis/Flores Timur/macp_plots_output.csv') %>%
  filter(MPAID==16 & grepl("_z",Response) & term=="Impact") %>%
  mutate(p.val=2*pnorm(-abs(z.score)),
         impact.direction=ifelse(estimate<0,"Negative",ifelse(estimate>0,"Positive","Zero"))) 


snapshot.sig.labs <- mapply(i=t(macp.flotim.std.impacts%>%select(p.val)),
                            j=c("Food Security","Material\nAssets","Marine\nTenure","Place\nAttachment","School\nEnrollment"),
                            function(i,j){
                              c(ifelse(i<0.01,paste("***",j,sep=""),
                                       ifelse(i<0.05 & i>=0.01,paste("**",j,sep=""),
                                              ifelse(i<0.1 & i>=0.05,paste("*",j,sep=""),paste(j)))))
                            })


# ---- 4.1 Snapshot plot, all Big Five standardized impacts on one plot ----

# - 2 year impacts
snapshot.MPAimpact.summ.2yr <- ggplot(data=macp.flotim.std.impacts,
                                       aes(x=Response,
                                           y=estimate)) +
  geom_bar(aes(fill=impact.direction),
           stat="identity",
           position="dodge",
           width=1) +
  geom_errorbar(aes(x=Response,
                    ymin=estimate-std.error,
                    ymax=estimate+std.error,
                    colour=impact.direction),
                width=0.1,
                size=0.5,
                show.legend=F) +
  geom_hline(aes(yintercept=0),
             linetype="solid",
             size=1,
             colour="#505050",
             show.legend=F) +
  scale_x_discrete(labels=snapshot.sig.labs) +
  scale_y_continuous(limits=c(-0.6,0.6),
                     breaks=c(seq(-0.5,0.5,by=0.2))) +
  scale_fill_manual(values=c(alpha("#65B65E",0.95),alpha("#2C7FB8",0.95)),
                    name="Direction of\nImpact",
                    labels=c("Negative","Positive")) +
  scale_colour_manual(values=c(alpha("#2B5027",0.95),alpha("#1B4D6F",0.95))) +
  labs(x="",y="\n MPA Impact",title="") +
  coord_flip() + snapshot.plot.theme.MPAimpact.summ + 
  snapshot.plot.guide.MPAimpact.summ 


# # ---- 4.2 Arrange grob ----
# 
# Damp.snapshot.legend.plot <-
#   ggplot(data=Damp.4yr.std.impacts,
#          aes(x=Domain,
#              y=Std.impact)) +
#   geom_bar(data=seascape.impact.4yr,
#            aes(x=Domain,
#                y=Std.impact,
#                linetype=""),
#            stat="identity") +
#   geom_bar(aes(fill=impact.direction),
#            stat="identity") +
#   scale_fill_manual(values=c(alpha("#65B65E",0.7),alpha("#1B448B",0.7)),
#                     name="Direction of\nImpact",
#                     labels=c("Positive","Negative")) +
#   scale_colour_manual(values=c(alpha("#2B5027",0.7),alpha("#072258",0.7))) +
#   scale_linetype_manual(values=1,
#                         name="Bird's Head\nSeascape\nMean",
#                         guide="legend") +
#   coord_flip() + snapshot.plot.theme.MPAimpact.summ + 
#   snapshot.plot.guide.MPAimpact.summ 
# 
# Damp.snapshot.legend <- g_legend(Damp.snapshot.legend.plot)
# 
# snapshot.Damp.MPAimpact.bothyr <-
#   grid.arrange(arrangeGrob(snapshot.Damp.MPAimpact.summ.2yr,
#                            snapshot.Damp.MPAimpact.summ.4yr,
#                            ncol=2),
#                Damp.snapshot.legend,
#                ncol=2,widths=c(3,0.5))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: WRITE TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


dir.create(paste("x_Flat_data_files/1_Social/Outputs/impact_analysis/Flores Timur/Figures--produced",
                 format(Sys.Date(),format="%Y_%m_%d"),sep="_"))

FigureFileName <- paste("x_Flat_data_files/1_Social/Outputs/impact_analysis/Flores Timur/Figures--produced",
                        format(Sys.Date(),format="%Y_%m_%d"),sep="_")


# ---- EXAMPLE PLOT FOR INTERPRETATION GUIDE ----

png(paste(FigureFileName,"example.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.example.i) 
dev.off()


# ---- 5.1 FOOD SECURITY ----


png(paste(FigureFileName,"FS.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.fs.i)
dev.off()


# ---- 5.2 MATERIAL ASSETS ----

png(paste(FigureFileName,"MA.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.ma.i)
dev.off()

# -- Material assets sub-classes --

# - Household items
png(paste(FigureFileName,"MA.HH.subclass.impact.png",sep="/"),
    units="in",height=6,width=5,res=400) 
plot(MPAimpact.summ.ma.hh.i)
dev.off()

# - Boat no motor
png(paste(FigureFileName,"MA.boatnomotor.subclass.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.ma.boatnomotor.i)
dev.off()

# - Motorized boat
png(paste(FigureFileName,"MA.boatmotor.subclass.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.ma.boatmotor.i)
dev.off()

# - Land vehicles
png(paste(FigureFileName,"MA.vehicle.subclass.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.ma.vehicle.i)
dev.off()


# ---- 5.3 MARINE TENURE ----

png(paste(FigureFileName,"MT.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.mt.i)
dev.off()


# ---- 5.4 PLACE ATTACHMENT ----

png(paste(FigureFileName,"PA.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.pa.i)
dev.off()


# ---- 5.5 SCHOOL ENROLLMENT ----

png(paste(FigureFileName,"SE.impact.png",sep="/"),
    units="in",height=6,width=5,res=400)
plot(MPAimpact.summ.se.i)
dev.off()


# ---- 5.6 SNAPSHOT PLOT ----

png(paste(FigureFileName,"standardized.impacts.png",sep="/"),
    units="in",height=8,width=12,res=400)
plot(snapshot.MPAimpact.summ.2yr)
dev.off()
