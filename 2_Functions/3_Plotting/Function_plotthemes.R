# 
# code: Plotting themes, labels, legend guides
# 
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: June 2017
# modified: July 2020
# 
# 
# ---- code sections ----
#  1) Report Plot Themes, Legends, and Guides
#  2) Report Labels
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Report Plot Themes, Legends, and Guides  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 MPA Technical Report plot themes ----

plot.theme <- theme(axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.x=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.y=element_blank(),
                    plot.margin=margin(t=0,r=20,b=5,l=5,unit="pt"),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                                  angle=0,
                                                  colour="#303030",
                                                  lineheight=0.7),
                    legend.position="top",
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"))

plot.theme.manysetts <- theme(axis.ticks=element_blank(),
                              panel.background=element_rect(fill="white",
                                                            colour="#909090"),
                              panel.border=element_rect(fill=NA,
                                                        size=0.25,
                                                        colour="#C0C0C0"),
                              panel.grid.major.x=element_line(colour="#C0C0C0",
                                                              size=0.25,
                                                              linetype=3),
                              panel.grid.major.y=element_blank(),
                              plot.margin=margin(t=0,r=20,b=5,l=5,unit="pt"),
                              axis.title=element_text(size=10,
                                                      angle=0,
                                                      face="bold",
                                                      colour="#303030"),
                              axis.text=element_text(size=8,
                                                     angle=0,
                                                     colour="#303030",
                                                     lineheight=0.7),
                              legend.position="top",
                              legend.justification="right",
                              legend.box.spacing=unit(0.1,"cm"))

age.gender.plot.theme <- theme(axis.ticks=element_blank(),
                               panel.background=element_rect(fill="white",
                                                             colour="#909090"),
                               panel.border=element_rect(fill=NA,
                                                         size=0.25,
                                                         colour="#C0C0C0"),
                               panel.grid.major.x=element_line(colour="#C0C0C0",
                                                               size=0.25,
                                                               linetype=3),
                               panel.grid.major.y=element_blank(),
                               axis.title=element_text(size=rel(0.8),
                                                       angle=0,
                                                       face="bold",
                                                       colour="#303030"),
                               axis.text=element_text(size=rel(0.6),
                                                      angle=0,
                                                      colour="#303030"),
                               legend.position="top",
                               legend.justification="right",
                               legend.box.spacing=unit(0.1,"cm"))


plot.theme.impact <- theme(axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.y=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.x=element_blank(),
                    plot.margin=margin(t=0,r=20,b=5,l=5,unit="pt"),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                           angle=0,
                                           colour="#303030",
                                           lineheight=0.7),
                    legend.position="top",
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"))

# fill colors
fillcols.status <- c("NotDummy"=alpha("#2C7FB8",0.95),"Dummy"=alpha("#FFFFFF",0))
fillcols.trend <- c(alpha("#2C7FB8",0.95))
fillcols.cont.trend <- c("MPA"=alpha("#2C7FB8",0.95),"Control"=alpha("#505050",0.95))

errcols.status <- c("NotDummy"=alpha("#21577C",0.95),"Dummy"=alpha("#FFFFFF",0))
errcols.trend <- c(alpha("#21577C",0.95))
errcols.cont.trend <- c("MPA"=alpha("#21577C",0.95),"Control"=alpha("#202020",0.95))



multianswer.fillcols.status <- list(Gender=c("HHH.male"=alpha("#253494",0.95),
                                             "HHH.female"=alpha("#7FCDBB",0.95)),
                                    Religion=c("Percent.Rel.Christian"=alpha("#253494",0.95),
                                               "Percent.Rel.Muslim"=alpha("#7FCDBB",0.95),
                                               "Percent.Rel.Other"=alpha("#E1E198",0.95)),
                                    ReligionOther=c("Percent.Rel.Muslim"=alpha("#253494",0.95),
                                                    "Percent.Rel.Buddhist"=alpha("#7FCDBB",0.95)),
                                    PrimaryOcc=c("Percent.PrimaryOcc.Farm"=alpha("#253494",0.95), 
                                                 "Percent.PrimaryOcc.HarvestForest"=alpha("#7FCDBB",0.95), 
                                                 "Percent.PrimaryOcc.Fish"=alpha("#2C7FB8",0.95),                                         
                                                 "Percent.PrimaryOcc.WageLabor"=alpha("#E1E198",0.95), 
                                                 "Percent.PrimaryOcc.Tourism"=alpha("#E31A1C",0.95), 
                                                 "Percent.PrimaryOcc.Aquaculture"=alpha("#FDBF6F",0.95), 
                                                 "Percent.PrimaryOcc.Extraction"=alpha("#FB9A99",0.95),
                                                 "Percent.PrimaryOcc.Other"=alpha("#FF7F00",0.95)), 
                                    SecondaryOcc=c("Percent.SecondaryOcc.Farm"=alpha("#7FCDBB",0.95),  
                                                 "Percent.SecondaryOcc.HarvestForest"=alpha("#253494",0.95), 
                                                 "Percent.SecondaryOcc.Fish"=alpha("#2C7FB8",0.95),                                      
                                                 "Percent.SecondaryOcc.WageLabor"=alpha("#E1E198",0.95),  
                                                 "Percent.SecondaryOcc.Tourism"=alpha("#E31A1C",0.95),  
                                                 "Percent.SecondaryOcc.Aquaculture"=alpha("#FDBF6F",0.95), 
                                                 "Percent.SecondaryOcc.Extraction"=alpha("#FB9A99",0.95), 
                                                 "Percent.SecondaryOcc.Other"=alpha("#FF7F00",0.95)), 
                                    FreqFish=c("Prop.Fish.AlmostNever"=alpha("#E1E198",0.95),
                                               "Prop.Fish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                               "Prop.Fish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                               "Prop.Fish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                               "Prop.Fish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    FreqSellFish=c("Prop.SellFish.AlmostNever"=alpha("#E1E198",0.95),
                                                   "Prop.SellFish.FewTimesPer6Mo"=alpha("#7FCDBB",0.95),
                                                   "Prop.SellFish.FewTimesPerMo"=alpha("#2CA9B8",0.95),
                                                   "Prop.SellFish.FewTimesPerWk"=alpha("#2C7FB8",0.95),
                                                   "Prop.SellFish.MoreFewTimesWk"=alpha("#253494",0.95)),
                                    IncFish=c("Prop.IncFish.None"=alpha("#E1E198",0.95),
                                              "Prop.IncFish.Some"=alpha("#7FCDBB",0.95),
                                              "Prop.IncFish.Half"=alpha("#2CA9B8",0.95),
                                              "Prop.IncFish.Most"=alpha("#2C7FB8",0.95),
                                              "Prop.IncFish.All"=alpha("#253494",0.95)),
                                    FishTech=c("Prop.FishTech.ByHand"=alpha("#E1E198",0.95),
                                               "Prop.FishTech.StatNet"=alpha("#7FCDBB",0.95),
                                               "Prop.FishTech.MobileNet"=alpha("#2CA9B8",0.95),
                                               "Prop.FishTech.StatLine"=alpha("#2C7FB8",0.95),
                                               "Prop.FishTech.MobileLine"=alpha("#253494",0.95)),
                                    ChildFS=c("Child.FS.no"=alpha("#253494",0.95),
                                              "Child.FS.yes"=alpha("#7FCDBB",0.95)),
                                    OccDiverse=c("Percent.OneOcc.Diverse"=alpha("#253494",0.95),
                                                 "Percent.MultipleOcc.Diverse"=alpha("#7FCDBB",0.95)),
                                    Protein=c("ProteinFish.None"=alpha("#E1E198",0.95),
                                              "ProteinFish.Some"=alpha("#7FCDBB",0.95),
                                              "ProteinFish.Half"=alpha("#2CA9B8",0.95),
                                              "ProteinFish.Most"=alpha("#2C7FB8",0.95),
                                              "ProteinFish.All"=alpha("#253494",0.95)),
                                    MarineMember=c("MarineMember.Yes"=alpha("#253494",0.95),
                                             "MarineMember.No"=alpha("#7FCDBB",0.95)),
                                    PropRules=c("PropRuleHab"=alpha("#253494",0.95),
                                                "PropRuleSpp"=alpha("#7FCDBB",0.95)),
                                    MarineAttendance=c("MarineMeeting.Yes"=alpha("#253494",0.95),
                                                 "MarineMeeting.No"=alpha("#7FCDBB",0.95)),
                                    FSCategorical=c("Percent.FoodInsecure.YesHunger"=alpha("#E1E198",0.95),
                                                    "Percent.FoodInsecure.NoHunger"=alpha("#7FCDBB",0.95),
                                                    "Percent.FoodSecure"=alpha("#253494",0.95)),
                                    EconStatus=c("Econ.Status.Much.Worse"=alpha("#E1E198",0.95),
                                                 "Econ.Status.Slighly.Worse"=alpha("#7FCDBB",0.95),
                                                 "Econ.Status.Neutral"=alpha("#2CA9B8",0.95),
                                                 "Econ.Status.Slightly.Better"=alpha("#2C7FB8",0.95),
                                                 "Econ.Status.Much.Better"=alpha("#253494",0.95)),
                                    SocialConflict=c("Percent.GreatlyDecreased.SocConflict"=alpha("#E1E198",0.95),
                                                     "Percent.Decreased.SocConflict"=alpha("#7FCDBB",0.95),
                                                     "Percent.Same.SocConflict"=alpha("#2CA9B8",0.95),
                                                     "Percent.Increased.SocConflict"=alpha("#2C7FB8",0.95),
                                                     "Percent.GreatlyIncreased.SocConflict"=alpha("#253494",0.95)),
                                    NumLocalThreats=c("Threat.None"=alpha("#E1E198",0.95),
                                                 "Threat.One"=alpha("#7FCDBB",0.95),
                                                 "Threat.Two"=alpha("#2CA9B8",0.95),
                                                 "Threat.Three"=alpha("#2C7FB8",0.95),
                                                 "Threat.Four"=alpha("#253494",0.95),
                                                 "Threat.Minimum.Five"=alpha("#101324", 0.95)),
                                    ThreatType1=c("Pollution"=alpha("#253494",0.95), 
                                                 "DestructiveFishing"=alpha("#2C7FB8",0.95), 
                                                 "IllegalFishing"=alpha("#7FCDBB",0.95), 
                                                 "ClimateChange"=alpha("#E1E198",0.95),
                                                 "HabitatLoss"=alpha("#FB9A99",0.95),
                                                 "NaturalProcesses"=alpha("#E31A1C",0.95),
                                                 "OtherMarineUses"=alpha("#FDBF6F",0.95),
                                                 "Other"=alpha("#FF7F00",0.95)),
                                    ThreatType2=c("NoThreat"=alpha("#FF7F00",0.95),
                                                     "Aquaculture"=alpha("#E31A1C",0.95), 
                                                     "InadequateProc"=alpha("#FB9A99",0.95),
                                                     "Tourism"=alpha("#E1E198",0.95),
                                                     "NaturalPhenomenon"=alpha("#7FCDBB",0.95), 
                                                     "Pollution"=alpha("#2C7FB8",0.95),
                                                     "UnsustainableFish"=alpha("#253494",0.95)),
                                    Participate=c("ParticipateOrg"=alpha("#7FC97F",0.95),
                                                  "ParticipateEstablish"=alpha("#FFFFB3",0.95),
                                                  "ParticipateBnd"=alpha("#2C7FB8",0.95),
                                                  "ParticipateRules"=alpha("#253494",0.95)),
                                    HHHEducation=c("HHHEducNone"=alpha("#E1E198",0.95),
                                                   "HHHEducPre"=alpha("#7FCDBB",0.95),
                                                   "HHHEducPrim"=alpha("#2CA9B8",0.95),
                                                   "HHHEducMid"=alpha("#2C7FB8",0.95),
                                                   "HHHEducSec"=alpha("#253494",0.95),
                                                   "HHHEducHigher"=alpha("#101324", 0.95)),
                                    AdultEducation=c("AdultEducNone"=alpha("#E1E198",0.95),
                                                     "AdultEducPre"=alpha("#7FCDBB",0.95),
                                                     "AdultEducPrim"=alpha("#2CA9B8",0.95),
                                                     "AdultEducMid"=alpha("#2C7FB8",0.95),
                                                     "AdultEducSec"=alpha("#253494",0.95),
                                                     "AdultEducHigher"=alpha("#101324", 0.95)))

annex.alpha <- list(numyear2=c(0.6,1),
                    numyear3=c(0.4,0.7,1),
                    numyear4=c(0.3,0.5,0.7,1),
                    numyear5=c(0.2,0.4,0.6,0.8,1))


# ---- 1.2 MPA Impact Summary plot themes ----

fill.cols.MPAimpact.summ <- c("MPA"=alpha("#1B448B",0.85),"Control"=alpha("#6B6B6B",0.4))
err.cols.MPAimpact.summ <- c(alpha("#0A1D4E",0.5),alpha("#242424",0.25))

snapshot.plot.theme.MPAimpact.summ <- theme(panel.background=element_blank(),
                                            panel.grid.major.x=element_line(size=0.25,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.major.y=element_line(size=0.5,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.minor.x=element_blank(),
                                            axis.ticks=element_blank(),
                                            axis.text=element_text(size=11,
                                                                   angle=0,
                                                                   colour="#505050",
                                                                   hjust=0.5),
                                            axis.title=element_text(size=12,
                                                                    angle=0,
                                                                    face="bold",
                                                                    colour="#505050"),
                                            plot.title=element_text(colour="#505050",
                                                                    face="bold",
                                                                    size=14),
                                            panel.border=element_rect(size=0.5,
                                                                      colour="#D0D0D0",
                                                                      linetype=3,
                                                                      fill=NA))

snapshot.plot.theme.MPAimpact.factsheet <- theme(panel.background=element_blank(),
                                            panel.grid.major.x=element_line(size=0.25,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.major.y=element_line(size=0.5,
                                                                            colour="#D0D0D0D0"),
                                            panel.grid.minor.x=element_blank(),
                                            plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                                            axis.ticks=element_blank(),
                                            axis.text=element_text(size=14,
                                                                   angle=0,
                                                                   colour="#505050",
                                                                   hjust=0.5),
                                            axis.title=element_text(size=16,
                                                                    angle=0,
                                                                    face="bold",
                                                                    colour="#505050"),
                                            plot.title=element_text(colour="#505050",
                                                                    face="bold",
                                                                    size=18),
                                            panel.border=element_rect(size=0.5,
                                                                      colour="#D0D0D0",
                                                                      linetype=3,
                                                                      fill=NA),
                                            legend.position="top",
                                            legend.justification="right")

plot.theme.MPAimpact.summ <- theme(axis.ticks=element_blank(),
                                   axis.text=element_text(vjust=0.5,
                                                          size=rel(1),
                                                          colour="#505050"),
                                   axis.title.y=element_text(face="bold",
                                                             size=rel(1),
                                                             angle=90,
                                                             colour="#505050"),
                                   axis.title.x=element_blank(),
                                   plot.title=element_blank(),
                                   panel.background=element_rect(fill="white",
                                                                 colour="#D0D0D0"),
                                   panel.grid.major.x=element_blank(),
                                   panel.grid.major.y=element_line(size=0.5,
                                                                   colour="#808080",
                                                                   linetype=3),
                                   panel.grid.minor.y=element_blank(),
                                   panel.border=element_rect(fill=NA,
                                                             colour="#D0D0D0"))

fs.st.plot.theme.MPAimpact.summ <- theme(axis.ticks.x=element_blank(),
                                         axis.ticks.y=element_line(colour="#505050"),
                                         axis.text=element_text(vjust=0.5,
                                                                size=rel(1.1),
                                                                colour="#505050"),
                                         axis.title.y=element_text(face="bold",
                                                                   size=rel(1.15),
                                                                   angle=90,
                                                                   colour="#505050"),
                                         axis.title.x=element_blank(),
                                         plot.title=element_blank(),
                                         panel.background=element_rect(fill="white",
                                                                       colour="#D0D0D0"),
                                         panel.grid.major.x=element_blank(),
                                         panel.grid.major.y=element_blank(),
                                         panel.grid.minor.y=element_blank(),
                                         panel.border=element_rect(fill=NA,
                                                                   colour="#D0D0D0"))


# ---- 1.3 MPA Technical Report plot legend guide ----


plot.guides.techreport <- guides(alpha=guide_legend(title.hjust=1,
                                                    title.theme=element_text(face="bold",
                                                                             size=rel(9),
                                                                             angle=0,
                                                                             colour="#505050",
                                                                             lineheight=0.75),
                                                    label.vjust=0.5,
                                                    label.theme=element_text(size=rel(8),
                                                                             angle=0,
                                                                             colour="#505050",
                                                                             lineheight=0.75),
                                                    direction="horizontal",
                                                    ncol=2,
                                                    title.position="left",
                                                    label.position="right",
                                                    keywidth=unit(0.75,"cm"),
                                                    keyheight=unit(0.5,"cm")),
                                 fill=guide_legend(title.hjust=1,
                                                   title.theme=element_text(face="bold",
                                                                            size=rel(9),
                                                                            angle=0,
                                                                            colour="#505050",
                                                                            lineheight=0.75),
                                                   label.vjust=0.5,
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
                                                   reverse=T),
                                 colour=guide_legend(title=element_blank(),
                                                     label.position="bottom",
                                                     label.theme=element_text(size=9,
                                                                              angle=0,
                                                                              colour="#505050"),
                                                     label.hjust=0.5,
                                                     keywidth=unit(2.5,"cm")),
                                 shape=guide_legend(title=element_blank(),
                                                    label.position="right",
                                                    label.theme=element_text(size=9,
                                                                             angle=0,
                                                                             colour="#505050"),
                                                    label.hjust=0.6))

# - Function to create common legend between multiple ggplots
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---- 1.4 MPA Impact Summary plot legend guide ----

plot.guides.MPAimpact.summ <- guides(size=guide_legend(title=element_blank(),
                                                       label.theme=element_text(size=9,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       label.position="bottom",
                                                       keywidth=unit(2.5,"cm"),
                                                       label.hjust=0.5,
                                                       order=2),
                                     fill=guide_legend(title.hjust=0.5,
                                                       title.theme=element_text(face="bold",
                                                                                size=10,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       label.theme=element_text(size=9,
                                                                                angle=0,
                                                                                colour="#505050"),
                                                       order=1,
                                                       ncol=2,
                                                       nrow=1,
                                                       title.position="top",
                                                       label.position="bottom",
                                                       keywidth=unit(1.2,"cm"),
                                                       label.hjust=0.5),
                                     colour=guide_legend(title=element_blank(),
                                                         label.position="bottom",
                                                         label.theme=element_text(size=9,
                                                                                  angle=0,
                                                                                  colour="#505050"),
                                                         label.hjust=0.5,
                                                         keywidth=unit(2.5,"cm"),
                                                         order=3),
                                     shape=guide_legend(title=element_blank(),
                                                        label.position="right",
                                                        label.theme=element_text(size=9,
                                                                                 angle=0,
                                                                                 colour="#505050"),
                                                        label.hjust=0.6,
                                                        order=4))

snapshot.plot.guide.MPAimpact.summ <- guides(fill=guide_legend(order=1,
                                                               keywidth=unit(1,"cm"),
                                                               label.theme=element_text(size=9,
                                                                                        angle=0,
                                                                                        colour="#505050"),
                                                               label.position="right",
                                                               label.hjust=0.5,
                                                               label.vjust=0.5,
                                                               title.theme=element_text(size=10,
                                                                                        angle=0,
                                                                                        colour="#505050",
                                                                                        face="bold",
                                                                                        lineheight=0.8),
                                                               title.hjust=0.5,
                                                               reverse=T),
                                             linetype=guide_legend(order=3,
                                                                   keywidth=unit(2.5,"cm"),
                                                                   title.theme=element_text(size=10,
                                                                                            angle=0,
                                                                                            colour="#505050",
                                                                                            lineheight=0.8,
                                                                                            face="bold"),
                                                                   title.hjust=0.5,
                                                                   label=F))

snapshot.plot.guide.MPAimpact.factsheet <- guides(fill=guide_legend(order=1,
                                                               keywidth=unit(1,"cm"),
                                                               label.theme=element_text(size=12,
                                                                                        angle=0,
                                                                                        colour="#505050"),
                                                               label.position="right",
                                                               label.hjust=0.5,
                                                               label.vjust=0.5,
                                                               title.theme=element_text(size=13,
                                                                                        angle=0,
                                                                                        colour="#505050",
                                                                                        face="bold",
                                                                                        lineheight=0.8),
                                                               title.hjust=0.5,
                                                               reverse=F,
                                                               ncol=2),
                                             linetype=guide_legend(order=3,
                                                                   keywidth=unit(2.5,"cm"),
                                                                   title.theme=element_text(size=13,
                                                                                            angle=0,
                                                                                            colour="#505050",
                                                                                            lineheight=0.8,
                                                                                            face="bold"),
                                                                   title.hjust=0.5,
                                                                   label=F))


# ---- 1.5 MPA continuous variables distribution plot theme ----

dist.plot.theme <- theme(axis.ticks=element_blank(),
                         plot.title=element_text(face="bold",
                                                 colour="#303030",
                                                 hjust=0.5),
                         panel.background=element_rect(fill="white",
                                                       colour="#909090"),
                         panel.border=element_rect(fill=NA,
                                                   size=0.25,
                                                   colour="#C0C0C0"),
                         panel.grid.major.x=element_line(colour="#C0C0C0",
                                                         size=0.25),
                         panel.grid.major.y=element_line(colour="#C0C0C0",
                                                         size=0.25,
                                                         linetype=3),
                         axis.title=element_text(size=11,
                                                 angle=0,
                                                 face="bold",
                                                 colour="#303030"),
                         axis.text=element_text(size=10,
                                                angle=0,
                                                colour="#303030"))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Report Labels  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Labels in ENGLISH ----

# NOTE: "\n" in any label means that it will provide a line break, which is necessary for particularly long labels.

# MPA technical reports

Statusplot.labs <- list(FS=labs(y="Mean household food security",x="Settlement"),
                        MA=labs(y="Mean household assets",x="Settlement"),
                        PA=labs(y="Mean place attachment",x="Settlement"),
                        MT=labs(y="Mean household marine tenure",x="Settlement"),
                        SE=labs(y="School enrollment (% children ages 5-18 years old)",x="Settlement"),
                        Time=labs(y="Distance to market (hours)",x="Settlement"),
                        Unwell=labs(y="Mean time suffering from illness or injury in past 4 weeks (days)",
                                    x="Settlement"),
                        Gender=labs(y="Gender (% head of household)",x="Settlement"),
                        Religion=labs(y="Religion (% head of household)",x="Settlement"),
                        PrimaryOcc=labs(y="Primary occupation (% households)",x="Settlement"),
                        FreqFish=labs(y="Frequency of fishing (% households)",x="Settlement"),
                        FreqSellFish=labs(y="Frequency of selling at least some catch (% households)",
                                          x="Settlement"),
                        FishProtein=labs(y="Dietary protein from fish in past 6 months (% households)",
                                         x="Settlement"),
                        IncFish=labs(y="Income from fishing in past 6 months (% households)",
                                     x="Settlement"),
                        FishTech=labs(y="Fishing technique most often used in past 6 months (% households)",
                                      x="Settlement"),
                        ChildFS=labs(y="Child hunger (% households)",x="Settlement"),
                        Ethnicity=labs(y="Mean number of ethnicities (Settlement ethnicity frequency)",x="Settlement"),
                        AdultEduc=labs(y="Education completed (% adults 18 years and older)",x="Settlement"),
                        HHHEduc=labs(y="Education completed (% household heads)",x="Settlement"),
                        EconStatus=labs(y="Change in economic status of households (% households)",x="Settlement"),
                        SocialConflict=labs(y="Change in social conflict over marine resources in past 12 months\n(% households)",x="Settlement"),
                        NumLocalThreats=labs(y="Number of identified local threats to marine environment (% households)",x="Settlement"),
                        ThreatTypes=labs(y="Types of local threats to marine environment (% threats identified)",x="Settlement"),
                        MarineMember=labs(y="Households who are a member of a marine organization",x="Settlement"),
                        MarineAttendance=labs(y="Households attending a meeting of a marine organization",x="Settlement"),
                        MarineContribution=labs(y="Mean household contribution to marine organization (Indonesian Rupiah)",x="Settlement"),
                        FSCategorical= labs(y="Food security by category (% households)",x="Settlement"),
                        Rules= labs(y="Percent of important species or habitats subject to specific harvest rules",x="Settlement"),
                        Participation= labs(y="Percent of important user groups participating in marine resource decisions",x="Settlement"))


continuous.variables.plotlabs <- c("Mean household food security","Mean household assets",
                                   "Mean place attachment","Mean household marine tenure",
                                   "School enrollment (% children ages 5-18 years old)", "Distance to market (hours)",
                                   "Mean time suffering from illness or injury in past 4 weeks (days)")

prop.variables.labs <- c("Primary occupation (% households)","Frequency of fishing (% households)","Frequency of selling at least some catch (% households)",
                         "Income from fishing in past 6 months (% households)","Fishing technique most often used in past 6 months (% households)","Child hunger (% households)",
                         "Dietary protein from fish in past 6 months (% households)","Change in economic status of fishing households (% households)",
                         "Number of identified local threats to marine environment (% households)","Secondary occupation (% households)","Occupational diversity (% households)")


# MPA impact summary "Big Five" plot labels

plot.fs.labs.st <- labs(y="Household Food Security\n ",title="STATUS AND TREND")
plot.ma.labs.st <- labs(y="Household Material Assets\n ",title="STATUS AND TREND")
plot.pa.labs.st <- labs(y="Household Place Attachment\n ",title="STATUS AND TREND")
plot.mt.labs.st <- labs(y="Household Marine Tenure\n ",title="STATUS AND TREND")
plot.se.labs.st <- labs(y="Enrollment Rate\n ",title="STATUS AND TREND")

plot.fs.labs.i <- labs(x="",y="Change in Household Food Security\nsince Baseline",title="")
plot.ma.labs.i <- labs(x="",y="Change in Household Material Assets\nsince Baseline",title="")
plot.pa.labs.i <- labs(x="",y="Change in Household Place Attachment\nsince Baseline",title="")
plot.mt.labs.i <- labs(x="",y="Change in Household Marine Tenure\nsince Baseline",title="")
plot.se.labs.i <- labs(x="",y="Change in Enrollment Rate\nsince Baseline",title="")

plot.ma.hh.labs.i <- labs(x="",y="Change in Household Assets\nsince Baseline",title="Household assets sub-class")
plot.ma.boatnomotor.labs.i <- labs(x="",y="Change in Motor-less Boat Assets\nsince Baseline",title="Motor-less boat assets sub-class")
plot.ma.boatmotor.labs.i <- labs(x="",y="Change in Motorized Boat Assets\nsince Baseline",title="Motorized boat assets sub-class")
plot.ma.vehicles.labs.i <- labs(x="",y="Change in Land Vehicle Assets\nsince Baseline",title="Land vehicle assets sub-class")

impact.x.labs <- c("MPA\nHouseholds","Control\nHouseholds")



# ---- 2.1 Labels in BAHASA ----

# NOTE: "\n" in any label means that it will provide a line break, which is necessary for particularly long labels.

# MPA technical reports 

Statusplot.labs.bahasa <- list(FS=labs(y="Rata-rata ketahanan pangan rumah tangga",x="Permukiman"),
                               MA=labs(y="Rata-rata aset rumah tangga",x="Permukiman"),
                               PA=labs(y="Rata-rata ikatan/kelekatan terhadap tempat",x="Permukiman"),
                               MT=labs(y="Rata-rata hak penguasaan/kepemilikan laut\noleh rumah tangga",x="Permukiman"),
                               SE=labs(y="Tingkat Pendidikan (% anak usia 5-18 tahun)",x="Permukiman"),
                               Time=labs(y="Jarak menuju pasar (jam)",x="Permukiman"),
                               Unwell=labs(y="Rata-rata waktu menderita sakit atau cedera\ndalam 4 minggu terakhir (hari)",
                                           x="Permukiman"),
                               Gender=labs(y="Jenis kelamin (% kepala rumah tangga)",x="Permukiman"),
                               Religion=labs(y="Agama (% kepala rumah tangga)",x="Permukiman"),
                               PrimaryOcc=labs(y="Pekerjaan utama (% rumah tangga)",x="Permukiman"),
                               FreqFish=labs(y="Frekuensi penangkapan ikan (% rumah tangga)",x="Permukiman"),
                               FreqSellFish=labs(y="Frekuensi penjualan setidaknya pada beberapa hasil tangkapan\n(% rumah tangga)",
                                                 x="Permukiman"),
                               FishProtein=labs(y="Protein makanan dari ikan dalam 6 bulan terakhir (% rumah tangga)",
                                                x="Permukiman"),
                               IncFish=labs(y="Penghasilan dari penangkapan ikan dalam 6 bulan terakhir\n(% rumah tangga)",
                                            x="Permukiman"),
                               FishTech=labs(y="Teknik penangkapan ikan yang paling sering digunakan\ndalam 6 bulan terakhir (% rumah tangga)",
                                             x="Permukiman"),
                               ChildFS=labs(y="Kelaparan pada anak (% rumah tangga)",x="Permukiman"),
                               AdultEduc=labs(y="Pendidikan Terakhir (% usia 18 tahun ke atas)",x="Permukiman"),
                               HHHEduc=labs(y="Tamat Pendidikan terakhir (% kepala rumah tangga)",x="Permukiman"),
                               EconStatus=labs(y="Perubahan status ekonomi rumah tangga (% rumah tangga)",x="Permukiman"),
                               SocialConflict=labs(y="Perubahan dalam konflik sosial atas sumber daya laut\ndalam 12 bulan terakhir(% rumah tangga)",x="Permukiman"),
                               NumLocalThreats=labs(y="Jumlah ancaman lokal yang teridentifikasi terhadap lingkungan laut\n(% rumah tangga)",x="Permukiman"),
                               ThreatType=labs(y="Jenis ancaman lokal terhadap lingkungan laut\n(% ancaman teridentifikasi)",x="Permukiman"),
                               MarineMember=labs(y="Rumah tangga yang merupakan anggota organisasi kelautan\n(% rumah tangga)",x="Permukiman"),
                               MarineAttendance=labs(y="Rumah tangga menghadiri pertemuan organisasi\ndalam bidang kelautan",x="Permukiman"),
                               MarineContribution=labs(y="Rata-rata kontribusi rumah tangga dalam organisasi bidang kaluatan\n(Rupiah)",x="Permukiman"),
                               FSCategorical= labs(y="Ketahanan pangan berdasarkan kategori (% rumah tangga)",x="Permukiman"),
                               AgeGender=labs(y="Distribusi populasi (% individu berdasarkan jenis kelamin)",x="Usia"),
                               AgeGenderUpperPlot=labs(y="",x="Usia"))


continuous.variables.plotlabs.bahasa <- c("Rata-rata ketahanan pangan rumah tangga","Rata-rata aset rumah tangga",
                                          "Rata-rata ikatan/kelekatan terhadap tempat","Rata-rata hak penguasaan/kepemilikan laut oleh rumah tangga",
                                          "Tingkat Pendidikan (% anak usia 5-18 tahun)", "Jarak menuju pasar (jam)",
                                          "Rata-rata waktu menderita sakit atau cedera\ndalam 4 minggu terakhir (hari)")

prop.variables.labs.bahasa <- c("Pekerjaan utama (% rumah tangga)",
                                "Frekuensi penangkapan ikan (% rumah tangga)",
                                "Frekuensi penjualan setidaknya pada beberapa hasil tangkapan\n(% rumah tangga)",
                                "Penghasilan dari penangkapan ikan dalam 6 bulan terakhir\n(% rumah tangga)",
                                "Teknik penangkapan ikan yang paling sering digunakan\ndalam 6 bulan terakhir (% rumah tangga)",
                                "Kelaparan pada anak (% rumah tangga)",
                                "Protein makanan dari ikan dalam 6 bulan terakhir (% rumah tangga)",
                                "Perubahan status ekonomi rumah tangga nelayan (% rumah tangga)",
                                "Jumlah ancaman lokal yang teridentifikasi terhadap lingkungan laut\n(% rumah tangga)",
                                "Pekerjaan sekunder (% rumah tangga)",
                                "Keberagaman pekerjaan (% rumah tangga)")

prop.variables.threat.bahasa <- "Jenis ancaman lokal terhadap lingkungan laut\n(% ancaman teridentifikasi)"


sett.names.bahasa <- list(ControlSett="Permukiman Kontrol", 
                          Use="Luar NTZ", 
                          NoTake="Sekitar NTZ",
                          Treatment="Perlakuan",
                          Control="Kontrol",
                          MonYear="Tahun Pemantauan")

legend.labs.bahasa <- list(AgeGender=c("Perempuan","Laki-laki"),
                           FoodSecure="Aman Pangan",
                           NoHunger="Rawan Pangan\ntanpa kelaparan",
                           YesHunger="Rawan Pangan\ndengan kelaparan",
                           Religion=c("Lainnya","Islam","Kristen"),
                           ReligionOther=c("Budha","Islam"),
                           PrimaryOcc=c("Lainnya", "Budidaya perairan", "Pariwisata", "Pemanfaatan sumber daya laut\nyang tidak terbarukan",  
                                        "Jenis pekerjaan berupah/\nburuh lainnya", "Hasil hutan", "Penangkapan ikan", "Pertanian"),
                           FreqFish=c("Lebih dari beberapa kali perminggu","Beberapa kali perminggu","Beberapa kali perbulan",
                                      "Beberapa kali dalam enam bulan","Hampir tidak pernah"),
                           FreqSellFish=c("Lebih dari beberapa kali perminggu","Beberapa kali perminggu",
                                          "Beberapa kali perbulan","Beberapa kali dalam enam bulan",
                                          "Hampir tidak pernah"),
                           IncFish=c("Seluruhnya","Sebagian besar","Sebagian","Sebagian kecil","Tidak ada"),
                           FishTech=c("Mobile line","Stationary line","Mobile net","Stationary net","Penangkapan dengan tangan"),
                           ChildFS=c("Kejadian kelaparan\npada anak","Tidak ada kejadian\nkelaparan pada anak"),
                           Protein=c("Seluruhnya","Sebagian besar","Sebagian","Sebagian kecil","Tidak ada"),
                           FSCategorical=c("Rawan Pangan dengan kelaparan", "Rawan Pangan tanpa kelaparan","Aman Pangan"),
                           EconStatus=c("Jauh lebih baik","Sedikit lebih baik","Tidak lebih baik maupun lebih buruk","Sedikit lebih buruk","Jauh lebih buruk"),
                           MarineMember=c("Bukan anggota","Anggota"),
                           MarineAttendance=c("Tidak menghadiri pertemuan","Menghadiri pertemuan"),
                           SocialConflict=c("Sangat meningkat","Meningkat","Tidak meningkat maupun menurun","Menurun","Sangat menurun"),
                           NumLocalThreats=c("Lebih dari empat ancaman","Empat ancaman","Tiga ancaman","Dua ancaman","Satu ancaman", "Tidak ada ancaman"),
                           SecondaryOcc=c("Lainnya", "Budidaya perairan", "Pariwisata", "Pemanfaatan sumber daya laut\nyang tidak terbarukan",  
                                          "Jenis pekerjaan berupah/\nburuh lainnya", "Hasil hutan", "Penangkapan ikan", "Pertanian"),
                           OccDiverse=c("Banyak Pekerjaan","Satu Pekerjaan"),
                           ThreatType=c("Tidak ada ancaman", "Budidaya perairan", "Perlindungan yang tidak memadai", "Pariwisata", 
                                        "Fenomena alam", "Polusi", "Penangkapan ikan yang\ntidak berkelanjutan"),
                           AdultEduc=c("Pendidikan tinggi lanjutan","SMA (Sekolah Menengah Atas)","SMP (Sekolah Menengah Pertama)","SD (Sekolah Dasar)",
                                       "TK (Taman Kanak-kanak)", "Tidak memiliki pendidikan formal"),
                           HHHEduc=c("Pendidikan tinggi lanjutan","SMA (Sekolah Menengah Atas)","SMP (Sekolah Menengah Pertama)","SD (Sekolah Dasar)",
                                     "TK (Taman Kanak-kanak)", "Tidak memiliki pendidikan formal"))


# MPA impact summary "Big Five" plot labels

plot.fs.labs.i.bahasa <- labs(x="",y="Perubahan Ketahanan Pangan Rumah Tangga\nsejak Baseline",title="")
plot.ma.labs.i.bahasa <- labs(x="",y="Perubahan Aset Rumah Tangga\nsejak Baseline",title="")
plot.pa.labs.i.bahasa <- labs(x="",y="Perubahan Rumah Tangga dalam Kelekatan\nTempat sejak Baseline",title="")
plot.mt.labs.i.bahasa <- labs(x="",y="Perubahan Hak Penguasaan/Kepemilikan Laut Rumah Tangga\nsejak Baseline",title="")
plot.se.labs.i.bahasa <- labs(x="",y="Perubahan Tingkat Pendidikan\nsejak Baseline",title="")

plot.ma.hh.labs.i.bahasa <- labs(x="",y="Perubahan Aset Rumah Tangga\nsejak Baseline",title="Subkelas aset rumah tangga")
plot.ma.boatnomotor.labs.i.bahasa <- labs(x="",y="Perubahan Aset Perahu tanpa Motor\nsejak Baseline",title="Subkelas aset perahu tanpa motor")
plot.ma.boatmotor.labs.i.bahasa <- labs(x="",y="Perubahan Aset Perahu Bermotor\nsejak Baseline",title="Subkelas aset perahu bermotor")
plot.ma.vehicles.labs.i.bahasa <- labs(x="",y="Perubahan Aset Kendaraan Darat\nsejak Baseline",title="Subkelas aset kendaraan darat")

impact.x.labs.bahasa <- c("Rumah tangga\nPerlakuan","Rumah tangga\nKontrol")
