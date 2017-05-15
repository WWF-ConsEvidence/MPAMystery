library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)

setwd('C:/Users/kclaborn05/Dropbox/Kelly_ProjectMGMT/1_Dashboard/TEMP - Food Ambient Monitoring/')

grid.area <- read.csv('Land use/gridarea-landconversions.csv',header=T)
area.converted <- function(x) {
  df <- x
  for(i in seq(4,723,by=1)) {
    df[,i] <- x[,i]*0.01
  }
  df.area <- df
  for(i in colnames(df)[4:723]) {
    df.area[,i] <- df[,i]*grid.area[,i]
  }
  df.area
}

remove.oceans <- function(x) {
  df <- x
  for(i in length(colnames(x))) {
    df[,i] <- ifelse(x[,i]==-1 |
                       x[,i]==-99,NA,x[,i])
  }
  df
}

conversion.type <- function(a){
  a[,"conversion.type"] <- ifelse(a[,"transitions"]==2 | a[,"transitions"]==6 |
                                    a[,"transitions"]==10 | a[,"transitions"]==14 |
                                    a[,"transitions"]==18 | a[,"transitions"]==22 |
                                                  a[,"transitions"]==26 | a[,"transitions"]==50 |
                                                  a[,"transitions"]==53 | a[,"transitions"]==56 |
                                                  a[,"transitions"]==59 | a[,"transitions"]==62 |
                                                  a[,"transitions"]==65 | a[,"transitions"]==68,"forest to crop",
                                                ifelse(a[,"transitions"]==29 | a[,"transitions"]==32 |
                                                         a[,"transitions"]==35 | a[,"transitions"]==38 |
                                                         a[,"transitions"]==41 | a[,"transitions"]==44 |
                                                         a[,"transitions"]==47,"non-forest to crop",
                                                       ifelse(a[,"transitions"]==88 | a[,"transitions"]==91,"rehabbed",
                                                              "REMOVE")))
  a
}

conversions.90 <- read.csv('Land use/landconversions-1990.csv',header=T)
conversions.90 <- conversion.type(conversions.90)
conversions.90 <- conversions.90[conversions.90$conversion.type!="REMOVE",]
conversions.90 <- remove.oceans(conversions.90)
conversions.90 <- area.converted(conversions.90)

conversions.91 <- read.csv('Land use/landconversions-1991.csv',header=T)
conversions.91 <- conversion.type(conversions.91)
conversions.91 <- conversions.91[conversions.91$conversion.type!="REMOVE",]
conversions.91 <- remove.oceans(conversions.91)
conversions.91 <- area.converted(conversions.91)

conversions.92 <- read.csv('Land use/landconversions-1992.csv',header=T)
conversions.92 <- conversion.type(conversions.92)
conversions.92 <- conversions.92[conversions.92$conversion.type!="REMOVE",]
conversions.92 <- remove.oceans(conversions.92)
conversions.92 <- area.converted(conversions.92)

conversions.93 <- read.csv('Land use/landconversions-1993.csv',header=T)
conversions.93 <- conversion.type(conversions.93)
conversions.93 <- conversions.93[conversions.93$conversion.type!="REMOVE",]
conversions.93 <- remove.oceans(conversions.93)
conversions.93 <- area.converted(conversions.93)

conversions.94 <- read.csv('Land use/landconversions-1994.csv',header=T)
conversions.94 <- conversion.type(conversions.94)
conversions.94 <- conversions.94[conversions.94$conversion.type!="REMOVE",]
conversions.94 <- remove.oceans(conversions.94)
conversions.94 <- area.converted(conversions.94)

conversions.95 <- read.csv('Land use/landconversions-1995.csv',header=T)
conversions.95 <- conversion.type(conversions.95)
conversions.95 <- conversions.95[conversions.95$conversion.type!="REMOVE",]
conversions.95 <- remove.oceans(conversions.95)
conversions.95 <- area.converted(conversions.95)

conversions.96 <- read.csv('Land use/landconversions-1996.csv',header=T)
conversions.96 <- conversion.type(conversions.96)
conversions.96 <- conversions.96[conversions.96$conversion.type!="REMOVE",]
conversions.96 <- remove.oceans(conversions.96)
conversions.96 <- area.converted(conversions.96)

conversions.97 <- read.csv('Land use/landconversions-1997.csv',header=T)
conversions.97 <- conversion.type(conversions.97)
conversions.97 <- conversions.97[conversions.97$conversion.type!="REMOVE",]
conversions.97 <- remove.oceans(conversions.97)
conversions.97 <- area.converted(conversions.97)

conversions.98 <- read.csv('Land use/landconversions-1998.csv',header=T)
conversions.98 <- conversion.type(conversions.98)
conversions.98 <- conversions.98[conversions.98$conversion.type!="REMOVE",]
conversions.98 <- remove.oceans(conversions.98)
conversions.98 <- area.converted(conversions.98)

conversions.99 <- read.csv('Land use/landconversions-1999.csv',header=T)
conversions.99 <- conversion.type(conversions.99)
conversions.99 <- conversions.99[conversions.99$conversion.type!="REMOVE",]
conversions.99 <- remove.oceans(conversions.99)
conversions.99 <- area.converted(conversions.99)

conversions.00 <- read.csv('Land use/landconversions-2000.csv',header=T)
conversions.00 <- conversion.type(conversions.00)
conversions.00 <- conversions.00[conversions.00$conversion.type!="REMOVE",]
conversions.00 <- remove.oceans(conversions.00)
conversions.00 <- area.converted(conversions.00)

conversions.01 <- read.csv('Land use/landconversions-2001.csv',header=T)
conversions.01 <- conversion.type(conversions.01)
conversions.01 <- conversions.01[conversions.01$conversion.type!="REMOVE",]
conversions.01 <- remove.oceans(conversions.01)
conversions.01 <- area.converted(conversions.01)

conversions.02 <- read.csv('Land use/landconversions-2002.csv',header=T)
conversions.02 <- conversion.type(conversions.02)
conversions.02 <- conversions.02[conversions.02$conversion.type!="REMOVE",]
conversions.02 <- remove.oceans(conversions.02)
conversions.02 <- area.converted(conversions.02)

conversions.03 <- read.csv('Land use/landconversions-2003.csv',header=T)
conversions.03 <- conversion.type(conversions.03)
conversions.03 <- conversions.03[conversions.03$conversion.type!="REMOVE",]
conversions.03 <- remove.oceans(conversions.03)
conversions.03 <- area.converted(conversions.03)

conversions.04 <- read.csv('Land use/landconversions-2004.csv',header=T)
conversions.04 <- conversion.type(conversions.04)
conversions.04 <- conversions.04[conversions.04$conversion.type!="REMOVE",]
conversions.04 <- remove.oceans(conversions.04)
conversions.04 <- area.converted(conversions.04)

conversions.05 <- read.csv('Land use/landconversions-2005.csv',header=T)
conversions.05 <- conversion.type(conversions.05)
conversions.05 <- conversions.05[conversions.05$conversion.type!="REMOVE",]
conversions.05 <- remove.oceans(conversions.05)
conversions.05 <- area.converted(conversions.05)

conversions.06 <- read.csv('Land use/landconversions-2006.csv',header=T)
conversions.06 <- conversion.type(conversions.06)
conversions.06 <- conversions.06[conversions.06$conversion.type!="REMOVE",]
conversions.06 <- remove.oceans(conversions.06)
conversions.06 <- area.converted(conversions.06)

conversions.07 <- read.csv('Land use/landconversions-2007.csv',header=T)
conversions.07 <- conversion.type(conversions.07)
conversions.07 <- conversions.07[conversions.07$conversion.type!="REMOVE",]
conversions.07 <- remove.oceans(conversions.07)
conversions.07 <- area.converted(conversions.07)

conversions.08 <- read.csv('Land use/landconversions-2008.csv',header=T)
conversions.08 <- conversion.type(conversions.08)
conversions.08 <- conversions.08[conversions.08$conversion.type!="REMOVE",]
conversions.08 <- remove.oceans(conversions.08)
conversions.08 <- area.converted(conversions.08)

conversions.09 <- read.csv('Land use/landconversions-2009.csv',header=T)
conversions.09 <- conversion.type(conversions.09)
conversions.09 <- conversions.09[conversions.09$conversion.type!="REMOVE",]
conversions.09 <- remove.oceans(conversions.09)
conversions.09 <- area.converted(conversions.09)

conversions.10 <- read.csv('Land use/landconversions-2010.csv',header=T)
conversions.10 <- conversion.type(conversions.10)
conversions.10 <- conversions.10[conversions.10$conversion.type!="REMOVE",]
conversions.10 <- remove.oceans(conversions.10)
conversions.10 <- area.converted(conversions.10)

conversions.combined.94 <- cbind.data.frame(conversions.94[,c(1:3,724)],
                                            area.94=rowSums(conversions.94[,4:723],na.rm=T))
conversions.combined.98 <- cbind.data.frame(conversions.98[,c(1:3,724)],
                                            area.98=rowSums(conversions.98[,4:723],na.rm=T))
conversions.combined.99 <- cbind.data.frame(conversions.99[,c(1:3,724)],
                                            area.99=rowSums(conversions.99[,4:723],na.rm=T))

conversions.combined <- cbind.data.frame(conversions.90[,c(1:3,724)],
                                         area.90=rowSums(conversions.90[,4:723],na.rm=T),
                                         area.91=rowSums(conversions.91[,4:723],na.rm=T),
                                         area.92=rowSums(conversions.92[,4:723],na.rm=T),
                                         area.93=rowSums(conversions.93[,4:723],na.rm=T),
                                         area.95=rowSums(conversions.95[,4:723],na.rm=T),
                                         area.96=rowSums(conversions.96[,4:723],na.rm=T),
                                         area.97=rowSums(conversions.97[,4:723],na.rm=T),
                                         area.00=rowSums(conversions.00[,4:723],na.rm=T),
                                         area.01=rowSums(conversions.01[,4:723],na.rm=T),
                                         area.02=rowSums(conversions.02[,4:723],na.rm=T),
                                         area.03=rowSums(conversions.03[,4:723],na.rm=T),
                                         area.04=rowSums(conversions.04[,4:723],na.rm=T),
                                         area.05=rowSums(conversions.05[,4:723],na.rm=T),
                                         area.06=rowSums(conversions.06[,4:723],na.rm=T),
                                         area.07=rowSums(conversions.07[,4:723],na.rm=T),
                                         area.08=rowSums(conversions.08[,4:723],na.rm=T),
                                         area.09=rowSums(conversions.09[,4:723],na.rm=T),
                                         area.10=rowSums(conversions.10[,4:723],na.rm=T))
conversions.sums.94 <- 
  conversions.combined.94 %>%
  group_by(transitions) %>%
  summarise(tot.area.94=sum(area.94),
            conversion.type=unique(conversion.type))
conversions.sums.98 <- 
  conversions.combined.98 %>%
  group_by(transitions) %>%
  summarise(tot.area.98=sum(area.98),
            conversion.type=unique(conversion.type))
conversions.sums.99 <- 
  conversions.combined.99 %>%
  group_by(transitions) %>%
  summarise(tot.area.99=sum(area.99),
            conversion.type=unique(conversion.type))

conversions.sums <- 
  conversions.combined %>%
  group_by(transitions) %>%
  summarise(tot.area.90=sum(area.90),
            tot.area.91=sum(area.91),
            tot.area.92=sum(area.92),
            tot.area.93=sum(area.93),
            tot.area.95=sum(area.95),
            tot.area.96=sum(area.96),
            tot.area.97=sum(area.97),
            tot.area.00=sum(area.00),
            tot.area.01=sum(area.01),
            tot.area.02=sum(area.02),
            tot.area.03=sum(area.03),
            tot.area.04=sum(area.04),
            tot.area.05=sum(area.05),
            tot.area.06=sum(area.06),
            tot.area.07=sum(area.07),
            tot.area.08=sum(area.08),
            tot.area.09=sum(area.09),
            tot.area.10=sum(area.10),
            conversion.type=unique(conversion.type))

forest.conversions <- data.frame("1990"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",2]),
                                 "1991"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",3]),
                                 "1992"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",4]),
                                 "1993"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",5]),
                                 "1995"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",6]),
                                 "1996"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",7]),
                                 "1997"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",8]),
                                 "2000"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",9]),
                                 "2001"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",10]),
                                 "2002"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",11]),
                                 "2003"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",12]),
                                 "2004"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",13]),
                                 "2005"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",14]),
                                 "2006"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",15]),
                                 "2007"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",16]),
                                 "2008"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",17]),
                                 "2009"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",18]),
                                 "2010"=colSums(conversions.sums[conversions.sums$conversion.type=="forest to crop",19]))
forest.conversions <- as.data.frame(t(forest.conversions))

nonforest.conversions <- data.frame("1990"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",2]),
                                    "1991"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",3]),
                                    "1992"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",4]),
                                    "1993"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",5]),
                                    "1995"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",6]),
                                    "1996"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",7]),
                                    "1997"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",8]),
                                    "2000"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",9]),
                                    "2001"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",10]),
                                    "2002"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",11]),
                                    "2003"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",12]),
                                    "2004"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",13]),
                                    "2005"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",14]),
                                    "2006"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",15]),
                                    "2007"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",16]),
                                    "2008"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",17]),
                                    "2009"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",18]),
                                    "2010"=colSums(conversions.sums[conversions.sums$conversion.type=="non-forest to crop",19]))
nonforest.conversions <- as.data.frame(t(nonforest.conversions))

rehabs <- data.frame("1990"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",2]),
                     "1991"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",3]),
                     "1992"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",4]),
                     "1993"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",5]),
                     "1995"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",6]),
                     "1996"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",7]),
                     "1997"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",8]),
                     "2000"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",9]),
                     "2001"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",10]),
                     "2002"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",11]),
                     "2003"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",12]),
                     "2004"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",13]),
                     "2005"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",14]),
                     "2006"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",15]),
                     "2007"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",16]),
                     "2008"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",17]),
                     "2009"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",18]),
                     "2010"=colSums(conversions.sums[conversions.sums$conversion.type=="rehabbed",19]))
rehabs <- as.data.frame(t(rehabs))

conversions <- cbind.data.frame("Year"=c(1990,1991,1992,1993,1995,1996,1997,2000,2001,
                                         2002,2003,2004,2005,2006,2007,2008,2009,2010),
                                "forest"=forest.conversions[,1],
                                "nonforest"=nonforest.conversions[,1],
                                "rehabbed"=rehabs[,1])


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

landuse.guide <- guides(colour=guide_legend(title=element_blank(),
                                     label.position="right",
                                     label.theme=element_text(size=9,
                                                              face="bold",
                                                              angle=0,
                                                              colour="#505050"),
                                     keywidth=unit(1,"cm")))

landuse.labs <- labs(x="Year",y="Area (square meters)")


landuseplot <- ggplot(data=conversions,
                  aes(Year)) +
  geom_line(aes(y=forest,colour="Forests"),size=1) + 
  geom_line(aes(y=nonforest,colour="Non-forests"),size=1) + 
  geom_line(aes(y=rehabbed,colour="Rehabbed"),size=1) + 
  scale_colour_manual(values=c("Forests"="#3D7832",
                               "Non-forests"="#C3B921",
                               "Rehabbed"="#A8A8A8")) +
  ggtitle(expression(atop(bold("AG LAND USE CONVERSION"),
                          atop(bold("SqM of land converted to cropland"))))) +
  plot.theme + landuse.guide + landuse.labs

write.xlsx(conversions,"AmbientMonitoring_FoodDashboard_2017_0510.xlsx",sheetName="Land_use",row.names=F)
write.xlsx(as.data.frame(wateruse.plotdata),"AmbientMonitoring_FoodDashboard_2017_0510.xlsx",sheetName="Water_use",row.names=F,append=T)
write.xlsx(ghgs,"AmbientMonitoring_FoodDashboard_2017_0510.xlsx",sheetName="GHG_emissions",row.names=F,append=T)
