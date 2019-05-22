# Function scripts = global lot themes, plotting asterisk functions, att.significance
# Script 1 = Pull and clean data from ODBC and/or flat file(s), basic analysis (calculate indices, etc.)
# Script 2 = Consolidate data, basic grouping summaries, etc.
# Script 3 = Matching (output = master pairs lists)
# Script 4 = Post-matching data analysis and wrangling (att.significance function, and wrangling for plots)
# Script 5 = Impact plots, by MPA and Seascape level (could be broken into multiple scripts)

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Source code and upload data from MPAMystery GitHub repo ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.2 Source scripts / dataframes ----

source('3_Analysis/1_Social/4_Post_matching/Calculate_ATTs_BHS.R')


# ---- 1.3 Define plotting-specific dataframe ----

# define asterisk data for BHS-level impact plot
asterisks.impactplots.seascape <- function(x) {
  p.val <- x$p.val
  year <- x$year
  
  result <- cbind.data.frame(year,
                             data.frame(asterisk=rep(NA,length(year))))
  
  for(i in 1:length(year)) {
    result[i,"asterisk"] <- ifelse(p.val[i] < 0.01, "***", 
                                   ifelse(p.val[i] < 0.05 & p.val[i] >= 0.01, "**",
                                          ifelse(p.val[i] < 0.1 & p.val[i] >= 0.05, "*", "")))
  }
  
  result
  
}

# put all BHS data together for plots, including asterisk data
BigFive.impactplotdata.BHS <-
  cbind.data.frame(variable=c(rep("hfs",length(hfs.att.BHS.seascape$year)),rep("asset",length(asset.att.BHS.seascape$year)),
                              rep("tenure",length(tenure.att.BHS.seascape$year)),rep("enrol",length(enrol.att.BHS.seascape$year)),
                              rep("attach",length(attach.att.BHS.seascape$year))),
                   rbind.data.frame(left_join(hfs.att.BHS.seascape[,c("year","smd","lower.ci","upper.ci","est","se")],
                                              asterisks.impactplots.seascape(hfs.att.BHS.seascape), by = "year"),
                                    left_join(asset.att.BHS.seascape[,c("year","smd","lower.ci","upper.ci","est","se")],
                                              asterisks.impactplots.seascape(asset.att.BHS.seascape), by = "year"),
                                    left_join(tenure.att.BHS.seascape[,c("year","smd","lower.ci","upper.ci","est","se")],
                                              asterisks.impactplots.seascape(tenure.att.BHS.seascape), by = "year"),
                                    left_join(enrol.att.BHS.seascape[,c("year","smd","lower.ci","upper.ci","est","se")],
                                              asterisks.impactplots.seascape(enrol.att.BHS.seascape), by = "year"),
                                    left_join(attach.att.BHS.seascape[,c("year","smd","lower.ci","upper.ci","est","se")],
                                              asterisks.impactplots.seascape(attach.att.BHS.seascape), by = "year")))

BigFive.impactplotdata.BHS <- BigFive.impactplotdata.BHS[c(9:10,5:6,7:8,1:2,3:4),]

BigFive.impactplotdata.BHS$variable <- factor(BigFive.impactplotdata.BHS$variable,
                                              unique(BigFive.impactplotdata.BHS$variable),
                                              ordered=T)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Plot themes for seascape-level impact plot ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# define global plotting theme for plots
plot.theme.impactplots <- theme(axis.ticks=element_blank(),
                                panel.background=element_rect(fill="white",
                                                              colour="#909090"),
                                panel.border=element_rect(fill=NA,
                                                          size=0.25,
                                                          colour="#707070"),
                                panel.grid.major.x=element_line(colour="#C0C0C0",
                                                                size=0.4,
                                                                linetype=3),
                                panel.grid.major.y=element_blank(),
                                axis.title=element_text(size=rel(1.6),
                                                        angle=0,
                                                        face="bold",
                                                        colour="#505050"),
                                axis.text=element_text(size=rel(1.6),
                                                       angle=0,
                                                       colour="#505050"),
                                plot.title=element_text(hjust=0.5),
                                legend.position="top",
                                legend.justification="right",
                                legend.box.spacing=unit(0.1,"cm"),
                                legend.text=element_text(size=rel(1.2),
                                                         angle=0,
                                                         colour="#505050"))


# BHS-level BigFive impact plot, by year
BigFive.impactplot.BHS <- 
  ggplot(BigFive.impactplotdata.BHS) +
  annotate('rect',xmin=0.5,xmax=1.5,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.2) +
  annotate('rect',xmin=2.5,xmax=3.5,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.2) +
  annotate('rect',xmin=4.5,xmax=5.5,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.2) +
  annotate('rect',xmin=0.5,xmax=0.51,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.3) +
  annotate('rect',xmin=1.49,xmax=1.5,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.3) +
  annotate('rect',xmin=2.5,xmax=2.51,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.3) +
  annotate('rect',xmin=3.49,xmax=3.5,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.3) +
  annotate('rect',xmin=4.5,xmax=4.51,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.3) +
  annotate('rect',xmin=5.49,xmax=5.5,ymin=-0.3,ymax=0.3,fill="#909090",alpha=0.3) +
  annotate('rect',xmin=5.5,xmax=6,ymin=-0.3,ymax=0.3,fill="#FFFFFF",alpha=0) +
  annotate('text',x=5.85,y=-0.09,label="< NEGATIVE IMPACT",colour="#505050",size=4,fontface="bold") +
  annotate('text',x=5.85,y=0.085,label="POSITIVE IMPACT >",colour="#505050",size=4,fontface="bold") +
  geom_bar(aes(x = variable, y = smd, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.75) +
  geom_errorbar(aes(x = variable, ymin = lower.ci, ymax = upper.ci, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.75),
                size = 0.6,
                width = 0.07,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.4,
             linetype = 2,
             colour = "#707070") +
  geom_text(aes(x = variable, group = year, y = ifelse(smd > 0, 
                                                       upper.ci + 0.13*upper.ci,
                                                       lower.ci + 0.13*lower.ci), label = asterisk),
            size = 4.5,
            position = position_dodge(width = 0.75),
            colour="#505050") +
  scale_x_discrete(name = "",
                   labels = c("tenure" = "Marine\nTenure", "asset" = "Material\nAssets", "attach" = "Place\nAttachment",
                              "enrol" = "School\nEnrollment", "hfs" = "Food\nSecurity")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(name = "",
                    labels = c("2 Year", "4 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c(alpha("#5E988B",0.4), alpha("#8494F7",0.4))) +
  coord_flip() + plot.theme.impactplots + labs(y="\nImpact\n(standardized average treatment effect)")


# read to .png file
png('x_Flat_data_files/1_Social/Outputs/BHS.impactplots.bySeascape.png',
    units="in",height=8,width=8,res=800)
grid.newpage()
grid.draw(BigFive.impactplot.BHS)
dev.off()


# ---- BHS-LEVEL BIG FIVE IMPACT PLOTS, IN RAW UNITS ----

# BHS-level food security impacts, raw units
hfs.impactplot.BHS <- 
  ggplot(BigFive.impactplotdata.BHS[BigFive.impactplotdata.BHS$variable=="hfs",]) +
  geom_bar(aes(x = year, y = est, fill = year),
           stat = "identity",
           position = "dodge",
           width = 0.5,
           show.legend = F) +
  geom_errorbar(aes(x = year, ymin = est - se.standard, ymax = est + se.standard, colour = year),
                stat = "identity",
                position = position_dodge(width = 0.5),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.2,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = year,
                y = ifelse(est > 0,
                           est + se.standard + 0.2*se.standard,
                           est - se.standard - 0.2*se.standard), 
                label = asterisk),
            size = 3,
            position = position_dodge(width=0.45)) +
  scale_x_discrete(name = "",
                   labels = c("t2" = "Two Year",
                              "t4" = "Four Year")) +
  scale_y_continuous(name = "",
                     limits = c(-0.5,0.5)) +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Household Food Security")


# BHS-level material assets impacts, raw units
asset.impactplot.BHS <- 
  ggplot(BigFive.impactplotdata.BHS[BigFive.impactplotdata.BHS$variable=="asset",]) +
  geom_bar(aes(x = year, y = est, fill = year),
           stat = "identity",
           position = "dodge",
           width = 0.5,
           show.legend = F) +
  geom_errorbar(aes(x = year, ymin = est - se.standard, ymax = est + se.standard, colour = year),
                stat = "identity",
                position = position_dodge(width = 0.5),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.2,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = year,
                y = ifelse(est > 0,
                           est + se.standard + 0.2*se.standard,
                           est - se.standard - 0.2*se.standard), 
                label = asterisk),
            size = 3,
            position = position_dodge(width=0.45)) +
  scale_x_discrete(name = "",
                   labels = c("t2" = "Two Year",
                              "t4" = "Four Year")) +
  scale_y_continuous(name = "",
                     limits = c(-2,2)) +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Material Assets")



# BHS-level marine tenure impacts, raw units
tenure.impactplot.BHS <- 
  ggplot(BigFive.impactplotdata.BHS[BigFive.impactplotdata.BHS$variable=="tenure",]) +
  geom_bar(aes(x = year, y = est, fill = year),
           stat = "identity",
           position = "dodge",
           width = 0.5,
           show.legend = F) +
  geom_errorbar(aes(x = year, ymin = est - se.standard, ymax = est + se.standard, colour = year),
                stat = "identity",
                position = position_dodge(width = 0.5),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.2,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = year,
                y = ifelse(est > 0,
                           est + se.standard + 0.2*se.standard,
                           est - se.standard - 0.2*se.standard), 
                label = asterisk),
            size = 3,
            position = position_dodge(width=0.45)) +
  scale_x_discrete(name = "",
                   labels = c("t2" = "Two Year",
                              "t4" = "Four Year")) +
  scale_y_continuous(name = "",
                     limits = c(-0.3, 0.3)) +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Marine Tenure")



# BHS-level school enrollment impacts, raw units
enrol.impactplot.BHS <- 
  ggplot(BigFive.impactplotdata.BHS[BigFive.impactplotdata.BHS$variable=="enrol",]) +
  geom_bar(aes(x = year, y = est, fill = year),
           stat = "identity",
           position = "dodge",
           width = 0.5,
           show.legend = F) +
  geom_errorbar(aes(x = year, ymin = est - se.standard, ymax = est + se.standard, colour = year),
                stat = "identity",
                position = position_dodge(width = 0.5),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.2,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = year,
                y = ifelse(est > 0,
                           est + se.standard + 0.2*se.standard,
                           est - se.standard - 0.2*se.standard), 
                label = asterisk),
            size = 3,
            position = position_dodge(width=0.45)) +
  scale_x_discrete(name = "",
                   labels = c("t2" = "Two Year",
                              "t4" = "Four Year")) +
  scale_y_continuous(name = "",
                     limits = c(-0.08, 0.08)) +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "School Enrollment")


# BHS-level place attachment impacts, raw units
attach.impactplot.BHS <- 
  ggplot(BigFive.impactplotdata.BHS[BigFive.impactplotdata.BHS$variable=="attach",]) +
  geom_bar(aes(x = year, y = est, fill = year),
           stat = "identity",
           position = "dodge",
           width = 0.5,
           show.legend = F) +
  geom_errorbar(aes(x = year, ymin = est - se.standard, ymax = est + se.standard, colour = year),
                stat = "identity",
                position = position_dodge(width = 0.5),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.2,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = year,
                y = ifelse(est > 0,
                           est + se.standard + 0.2*se.standard,
                           est - se.standard - 0.2*se.standard), 
                label = asterisk),
            size = 3,
            position = position_dodge(width=0.45)) +
  scale_x_discrete(name = "",
                   labels = c("t2" = "Two Year",
                              "t4" = "Four Year")) +
  scale_y_continuous(name = "",
                     limits = c(-0.1, 0.1)) +
  scale_fill_manual(name = "",
                    labels = c("4 Year", "2 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Place Attachment")


# grid.arrange all plots into one figure
BigFive.rawunits.impactplots.BHS <- arrangeGrob(hfs.impactplot.BHS, asset.impactplot.BHS, tenure.impactplot.BHS, 
                                 enrol.impactplot.BHS, attach.impactplot.BHS, 
                                 ncol = 3) # adjust number of columns if adding or removing number of plots shown in one image


# read to .png file
png('2_Social/FlatDataFiles/BHS/BigFive.rawunits.impactplots.BHS.png',
    units="in",height=3,width=9,res=600)
grid.newpage()
grid.draw(BigFive.rawunits.impactplots.BHS)
dev.off()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Define BHS-level impacts for Middle15 ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# output child food security att & AI error, by year
chfs.att.t2.BHS <- att.significance(outcomes = chfs.outcome.t2, HHData = HHData)
chfs.att.t4.BHS <- att.significance(outcomes = chfs.outcome.t4, HHData = HHData)

chfs.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(chfs.att.t2.BHS,chfs.att.t4.BHS))


# output right to access att & AI error, by year
access.att.t2.BHS <- att.significance(outcomes = access.outcome.t2, HHData = HHData)
access.att.t4.BHS <- att.significance(outcomes = access.outcome.t4, HHData = HHData)

access.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(access.att.t2.BHS,access.att.t4.BHS))


# output right to manage att & AI error, by year
manage.att.t2.BHS <- att.significance(outcomes = manage.outcome.t2, HHData = HHData)
manage.att.t4.BHS <- att.significance(outcomes = manage.outcome.t4, HHData = HHData)

manage.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(manage.att.t2.BHS,manage.att.t4.BHS))


# output occupational dependence att & AI error, by year
OD.att.t2.BHS <- att.significance(outcomes = OD.outcome.t2, HHData = HHData)
OD.att.t4.BHS <- att.significance(outcomes = OD.outcome.t4, HHData = HHData)

OD.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(OD.att.t2.BHS,OD.att.t4.BHS))


# output marine organization participation att & AI error, by year
MP.att.t2.BHS <- att.significance(outcomes = MP.outcome.t2, HHData = HHData)
MP.att.t4.BHS <- att.significance(outcomes = MP.outcome.t4, HHData = HHData)

MP.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(MP.att.t2.BHS,MP.att.t4.BHS))


# output other organization participation att & AI error, by year
OP.att.t2.BHS <- att.significance(outcomes = OP.outcome.t2, HHData = HHData)
OP.att.t4.BHS <- att.significance(outcomes = OP.outcome.t4, HHData = HHData)

OP.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(OP.att.t2.BHS,OP.att.t4.BHS))


# output economic status decline att & AI error, by year
econ.decline.att.t2.BHS <- att.significance(outcomes = econ.decline.outcome.t2, HHData = HHData)
econ.decline.att.t4.BHS <- att.significance(outcomes = econ.decline.outcome.t4, HHData = HHData)

econ.decline.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(econ.decline.att.t2.BHS,econ.decline.att.t4.BHS))


# output economic status increase att & AI error, by year
econ.increase.att.t2.BHS <- att.significance(outcomes = econ.increase.outcome.t2, HHData = HHData)
econ.increase.att.t4.BHS <- att.significance(outcomes = econ.increase.outcome.t4, HHData = HHData)

econ.increase.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(econ.increase.att.t2.BHS,econ.increase.att.t4.BHS))


# output social conflict decrease att & AI error, by year
con.decrease.att.t2.BHS <- att.significance(outcomes = con.decrease.outcome.t2, HHData = HHData)
con.decrease.att.t4.BHS <- att.significance(outcomes = con.decrease.outcome.t4, HHData = HHData)

con.decrease.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(con.decrease.att.t2.BHS,con.decrease.att.t4.BHS))


# output social conflict increase att & AI error, by year
con.increase.att.t2.BHS <- att.significance(outcomes = con.increase.outcome.t2, HHData = HHData)
con.increase.att.t4.BHS <- att.significance(outcomes = con.increase.outcome.t4, HHData = HHData)

con.increase.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(con.increase.att.t2.BHS,con.increase.att.t4.BHS))


# output morbidity att & AI error, by year
morbidity.att.t2.BHS <- att.significance(outcomes = morbidity.outcome.t2, HHData = HHData)
morbidity.att.t4.BHS <- att.significance(outcomes = morbidity.outcome.t4, HHData = HHData)

morbidity.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(morbidity.att.t2.BHS,morbidity.att.t4.BHS))


# output female school enrollment att & AI error, by year
f.enrol.att.t2.BHS <- att.significance(outcomes = f.enrol.outcome.t2, HHData = HHData)
f.enrol.att.t4.BHS <- att.significance(outcomes = f.enrol.outcome.t4, HHData = HHData)

f.enrol.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(f.enrol.att.t2.BHS,f.enrol.att.t4.BHS))


# output male school enrollment att & AI error, by year
m.enrol.att.t2.BHS <- att.significance(outcomes = m.enrol.outcome.t2, HHData = HHData)
m.enrol.att.t4.BHS <- att.significance(outcomes = m.enrol.outcome.t4, HHData = HHData)

m.enrol.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(m.enrol.att.t2.BHS,m.enrol.att.t4.BHS))


# output educational attainment 4 att & AI error, by year
attain.4.att.t2.BHS <- att.significance(outcomes = attain.4.outcome.t2, HHData = HHData)
attain.4.att.t4.BHS <- att.significance(outcomes = attain.4.outcome.t4, HHData = HHData)

attain.4.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(attain.4.att.t2.BHS,attain.4.att.t4.BHS))


# output educational attainment 5 att & AI error, by year
attain.5.att.t2.BHS <- att.significance(outcomes = attain.5.outcome.t2, HHData = HHData)
attain.5.att.t4.BHS <- att.significance(outcomes = attain.5.outcome.t4, HHData = HHData)

attain.5.att.BHS <- 
  data.frame(year = c("t2","t4"), 
             rbind.data.frame(attain.5.att.t2.BHS,attain.5.att.t4.BHS))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Plot BHS-level impacts for Middle15, and read to .png ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# put all BHS Middle 15 data together for plots, including asterisk data
Middle15.impactplotdata.BHS <-
  cbind.data.frame(variable=c(rep("chfs",length(chfs.att.BHS$year)),rep("access",length(access.att.BHS$year)),
                              rep("manage",length(manage.att.BHS$year)),rep("OD",length(OD.att.BHS$year)),
                              rep("MP",length(MP.att.BHS$year)),rep("OP",length(OP.att.BHS$year)),
                              rep("econ.decline",length(econ.decline.att.BHS$year)),rep("econ.increase",length(econ.increase.att.BHS$year)),
                              rep("con.decrease",length(con.decrease.att.BHS$year)),rep("con.increase",length(con.increase.att.BHS$year)),
                              rep("f.enrol",length(f.enrol.att.BHS$year)),rep("m.enrol",length(m.enrol.att.BHS$year)),
                              rep("morbidity",length(morbidity.att.BHS$year)),
                              rep("attain.4",length(attain.4.att.BHS$year)),rep("attain.5",length(attain.5.att.BHS$year))),
                   rbind.data.frame(left_join(chfs.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(chfs.att.BHS), by = "year"),
                                    left_join(access.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(access.att.BHS), by = "year"),
                                    left_join(manage.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(manage.att.BHS), by = "year"),
                                    left_join(OD.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(OD.att.BHS), by = "year"),
                                    left_join(MP.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(MP.att.BHS), by = "year"),
                                    left_join(OP.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(OP.att.BHS), by = "year"),
                                    left_join(econ.decline.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(econ.decline.att.BHS), by = "year"),
                                    left_join(econ.increase.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(econ.increase.att.BHS), by = "year"),
                                    left_join(con.decrease.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(con.decrease.att.BHS), by = "year"),
                                    left_join(con.increase.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(con.increase.att.BHS), by = "year"),
                                    left_join(f.enrol.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(f.enrol.att.BHS), by = "year"),
                                    left_join(m.enrol.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(m.enrol.att.BHS), by = "year"),
                                    left_join(morbidity.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(morbidity.att.BHS), by = "year"),
                                    left_join(attain.4.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(attain.4.att.BHS), by = "year"),
                                    left_join(attain.5.att.BHS[,c("year","smd","lower.ci","upper.ci")],
                                              asterisks.impactplots.seascape(attain.5.att.BHS), by = "year")))

Middle15.impactplotdata.BHS$variable <- factor(Middle15.impactplotdata.BHS$variable, 
                                               levels=unique(Middle15.impactplotdata.BHS$variable),
                                               ordered=T)


# BHS-level Middle 15 impact plot, by year
Middle15.impactplot.BHS <- 
  ggplot(Middle15.impactplotdata.BHS) +
  geom_bar(aes(x = variable, y = smd, fill = year, group = year),
           stat = "identity",
           position = "dodge",
           width = 0.3) +
  geom_errorbar(aes(x = variable, ymin = lower.ci, ymax = upper.ci, colour = year, group = year),
                stat = "identity",
                position = position_dodge(width = 0.3),
                size = 0.2,
                width = 0.05,
                show.legend = F) +
  geom_hline(aes(yintercept = 0),
             size = 0.1,
             linetype = 2,
             colour = "#505050") +
  geom_text(aes(x = variable, group = year, y = ifelse(smd > 0, 
                                                       upper.ci + 0.2*upper.ci,
                                                       lower.ci + 0.2*lower.ci), label = asterisk),
            size = 3,
            position = position_dodge(width = 0.45)) +
  scale_x_discrete(name = "",
                   labels = c("chfs" = "Child Food\nSecurity", "access" = "Right to Access\nMarine Resources", 
                              "manage" = "Right to Manage\nMarine Resources", "OD" = "Occupational Dependence\non Fisheries", 
                              "MP" = "Marine Group\nParticipation", "OP" = "Non-Marine Group\nParticipation", 
                              "econ.decline" = "Decline in\nEconomic Status", "econ.increase" = "Increase in\nEconomic Status", 
                              "con.decrease" = "Decline in\nSocial Conflict", "con.increase" = "Increase in\nSocial Conflict", 
                              "morbidity" = "Morbidity",
                              "f.enrol" = "Female School\nEnrollment", "m.enrol" = "Male School\nEnrollment",
                              "attain.4" = "Educational Attainment\n(Level 4)", "attain.5" = "Educational Attainment\n(Level 5)")) +
  scale_y_continuous(name = "") +
  scale_fill_manual(name = "",
                    labels = c("2 Year", "4 Year"),
                    values = c(alpha("#7FCDBB",0.95),
                               alpha("#253494",0.95))) +
  scale_colour_manual(values = c("#5E988B", "#8494F7")) +
  coord_flip() + plot.theme.impactplots + labs(title = "Bird's Head Seascape Middle 15 Impacts")


# read to .png file
png('2_Social/FlatDataFiles/BHS/Middle15.impactplots.BHS.png',
    units="in",height=10,width=7,res=400)
grid.newpage()
grid.draw(Middle15.impactplot.BHS)
dev.off()


