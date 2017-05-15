ghgs <- read.csv('GHGs -- global carbon budget.csv',header=T)

plot.theme <- theme(axis.ticks=element_blank(),
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
                    panel.margin=unit(0,"cm"),
                    axis.title=element_text(size=10,
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=8,
                                           angle=0,
                                           colour="#303030"))

ghg.labs <- labs(x="Year",y="CO2 (Gt)")

ghgplot <- ggplot(data=ghgs,aes(Year)) +
  geom_line(aes(y=CO2),
            color="#1B448B",
            size=1) + 
  scale_y_continuous(limits=c(1,8),
                     breaks=seq(1,8,by=1)) +
  ggtitle(expression(atop(bold("GHG EMISSIONS from AG"),
                          atop(bold("Gigatonnes of CO2 emitted from land use changes"))))) +
  plot.theme + ghg.labs
  