library(plyr)
library(dplyr)
library(ggplot2)

setwd('C:/Users/claborn-intern/Dropbox (MPAMystery)/Kelly_ProjectMGMT/1_Dashboard/TEMP - Food Ambient Monitoring/')

wateruse <- read.csv('wateruse.since1960.csv',header=T)
wateruse$timeinterval <- ifelse(wateruse$Year=="1958" |
                                  wateruse$Year=="1959" |
                                  wateruse$Year=="1960" |
                                  wateruse$Year=="1961" |
                                  wateruse$Year=="1962",
                                "t1",
                                ifelse(wateruse$Year=="1963" |
                                         wateruse$Year=="1964" |
                                         wateruse$Year=="1965" |
                                         wateruse$Year=="1966" |
                                         wateruse$Year=="1967",
                                       "t2",
                                       ifelse(wateruse$Year=="1968" |
                                                wateruse$Year=="1969" |
                                                wateruse$Year=="1970" |
                                                wateruse$Year=="1971" |
                                                wateruse$Year=="1972",
                                              "t3",
                                              ifelse(wateruse$Year=="1973" |
                                                       wateruse$Year=="1974" |
                                                       wateruse$Year=="1975" |
                                                       wateruse$Year=="1976" |
                                                       wateruse$Year=="1977",
                                                     "t4",
                                                     ifelse(wateruse$Year=="1978" |
                                                              wateruse$Year=="1979" |
                                                              wateruse$Year=="1980" |
                                                              wateruse$Year=="1981" |
                                                              wateruse$Year=="1982",
                                                            "t5",
                                                            ifelse(wateruse$Year=="1983" |
                                                                     wateruse$Year=="1984" |
                                                                     wateruse$Year=="1985" |
                                                                     wateruse$Year=="1986" |
                                                                     wateruse$Year=="1987",
                                                                   "t6",
                                                                   ifelse(wateruse$Year=="1988" |
                                                                            wateruse$Year=="1989" |
                                                                            wateruse$Year=="1990" |
                                                                            wateruse$Year=="1991" |
                                                                            wateruse$Year=="1992",
                                                                          "t7",
                                                                          ifelse(wateruse$Year=="1993" |
                                                                                   wateruse$Year=="1994" |
                                                                                   wateruse$Year=="1995" |
                                                                                   wateruse$Year=="1996" |
                                                                                   wateruse$Year=="1997",
                                                                                 "t8",
                                                                                 ifelse(wateruse$Year=="1998" |
                                                                                          wateruse$Year=="1999" |
                                                                                          wateruse$Year=="2000" |
                                                                                          wateruse$Year=="2001" |
                                                                                          wateruse$Year=="2002",
                                                                                        "t9",
                                                                                        ifelse(wateruse$Year=="2003" |
                                                                                                 wateruse$Year=="2004" |
                                                                                                 wateruse$Year=="2005" |
                                                                                                 wateruse$Year=="2006" |
                                                                                                 wateruse$Year=="2007",
                                                                                               "t10",
                                                                                               ifelse(wateruse$Year=="2008" |
                                                                                                        wateruse$Year=="2009" |
                                                                                                        wateruse$Year=="2010" |
                                                                                                        wateruse$Year=="2011" |
                                                                                                        wateruse$Year=="2012",
                                                                                                      "t11",
                                                                                                      ifelse(wateruse$Year=="2013" |
                                                                                                               wateruse$Year=="2014" |
                                                                                                               wateruse$Year=="2015" |
                                                                                                               wateruse$Year=="2016" |
                                                                                                               wateruse$Year=="2017",
                                                                                                             "t12",NA))))))))))))

wateruse.prop <- wateruse[wateruse$Variable.Id==4250,c(1,3,5,6,8)]
wateruse.prop <- 
  wateruse.prop %>%
  full_join(wateruse[wateruse$Variable.Id==4253,c(1,5,6)], by=c("Area","Year"))
colnames(wateruse.prop) <- c("Area","Variable.Name","Year","AgWithdrawal","TimeInterval","TotalWithdrawal")

wateruse.prop <- wateruse.prop[!is.na(wateruse.prop$AgWithdrawal) &
                                 !is.na(wateruse.prop$TotalWithdrawal),]

wateruse.numberintervals <- 
  wateruse.prop %>%
  group_by(Area) %>%
  summarise(numberintervals=length(unique(TimeInterval)))

wateruse.mostfreqtime <-
  wateruse.prop %>%
  group_by(TimeInterval) %>%
  summarise(numbercountries=length(unique(Area)))

wateruse.plotdata <-
  wateruse.prop %>%
  group_by(TimeInterval) %>%
  summarise(percent=(sum(AgWithdrawal,na.rm=T)/sum(TotalWithdrawal,na.rm=T))*100)

wateruse.plotdata <- wateruse.plotdata[c(1:3,9:11),]
wateruse.plotdata$TimeInterval <- factor(wateruse.plotdata$TimeInterval,
                                         levels=c("t7","t8","t9","t10","t11","t12"),
                                         ordered=T)
wateruse.plotdata$Year <- factor(ifelse(wateruse.plotdata$TimeInterval=="t7","1988-1992",
                                 ifelse(wateruse.plotdata$TimeInterval=="t8","1993-1997",
                                        ifelse(wateruse.plotdata$TimeInterval=="t9","1998-2002",
                                               ifelse(wateruse.plotdata$TimeInterval=="t10","2003-2007",
                                                      ifelse(wateruse.plotdata$TimeInterval=="t11","2008-2012",
                                                             ifelse(wateruse.plotdata$TimeInterval=="t12","2013-2017",NA)))))),
                                 levels=c("1988-1992","1993-1997","1998-2002","2003-2007","2008-2012","2013-2017"),
                                 ordered=T)
wateruse.plotdata <- wateruse.plotdata[order(wateruse.plotdata$Year),c(3,2)]

plot.theme.wateruse <- theme(axis.ticks=element_blank(),
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
                    axis.text.x=element_text(hjust=1,
                                           size=8,
                                           angle=45,
                                           colour="#303030"),
                    axis.text.y=element_text(size=8,
                                             angle=0,
                                             colour="#303030"))



wateruse.labs <- labs(x="Year",y="Percent agricultural withdrawal")

wateruse.plot <- ggplot(wateruse.plotdata,
                        aes(Year,group=1)) +
  geom_line(aes(y=percent),
            color="#1B448B",
            size=1) +
  scale_x_discrete(labels=c("1988-1992","1993-1997","1998-2002","2003-2007","2008-2012","2013-2017"),
                   expand=c(0,0)) +
  scale_y_continuous(limits=c(55,75)) +
  ggtitle(expression(atop(bold("AG WATER UPTAKE"),
                          atop(bold("Ag water withdrawal as % of total water withdrawal"))))) +
  plot.theme.wateruse + wateruse.labs 