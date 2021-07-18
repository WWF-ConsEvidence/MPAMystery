#install.packages('pacman')
pacman::p_load(rio,skimr,cowplot,corrgram,corrr,psych,tidyverse)
today.date <- gsub("-","",Sys.Date())
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))
}

#--Create path to folder that holds multiple .csv files
wd <- getwd()
dropboxdir <- ifelse(wd == "/Users/kendallmaryjefferys/Desktop/gov_analysis3", gsub("Desktop.*$","",wd),gsub("Dropbox.*$","",getwd())) 
input.dir <- paste0(dropboxdir,"/Dropbox/Governance analysis/tables/")
list.files(input.dir)

#import data
gov.data <- last.file(input.dir, "_WWF.Governance.Data.Analyzed.V4.csv")
soc.data <-  last.file(input.dir, "sett_avg_hh_data.csv")
settl.data <-  last.file(input.dir, "HH_tbl_SETTLEMENT_20200316.xls")
impact.data <- last.file(input.dir, "settlevel_impacts_1-3match_20201218.csv")
sett.imp.data <- last.file(input.dir, "Le_HH_data.csv")
soc.imp.data <- last.file(input.dir, "sett_avg_hh_data.csv")

#####################################################################
## gov and soc data 
####################################################################
soc.data <- soc.data %>%
  rename("SiteCode" = "MPAID",
         "Year" = "MonitoringYear"
  ) %>%
  group_by(SiteCode) %>% 
  mutate(monitor.yr = ifelse(Year==min(Year),"baseline","post"),
         wealthIndex.pca=-PovertyIndex_pca) 

gov.data <- gov.data %>% 
  left_join(select(settl.data,SettlementID,Treatment), by="SettlementID") %>% 
  group_by(SiteCode) %>% 
  mutate(monitor.yr = ifelse(Year==min(Year),"baseline","post")) 

# full.data <- gov.data %>%
#   full_join(soc.data, by = c("SettlementID", "SiteCode", "Year")) %>% 
#   group_by(SiteCode) %>% 
#   mutate(monitor.yr = ifelse(Year==min(Year),"baseline","post")) 

#check for duplicates
#n_distinct(paste0(full.data$SettlementID,"_",full.data$SiteCode,"_",full.data$Year))==nrow(full.data)                   
#summary(full.data)

###################################################################
# MPA Post Data frames 
##################################################################

## filter impact data 
final.impact.data <- impact.data %>%
  filter(term=="Impact") %>%
  group_by(SettlementID, Response) %>%
  summarise(estimate=mean(estimate, na.rm=T),
            std.error=mean(std.error, na.rm=T),
            statistic=mean(statistic, na.rm=T),
            p.value=mean(p.value,na.rm=T)) 

summary(final.impact.data)

###So that MPA ID can be added into full data frame later
MPA.ID <- impact.data %>%
  group_by(SettlementID) %>%
  summarise(MPAID=median(MPAID)) 

## Using the full joined gov and soc data 
#mpa.post.data <- filter(full.data, monitor.yr=="post", Treatment==1) %>%
# ungroup() 

## Using just the governance data, post MPA
gov.mpa.post.data <- gov.data %>%
  filter(monitor.yr=="post" & Treatment==1) %>% 
  ungroup()

## Using just social data baseline MPA
## filter to baseline social data in MPA
soc.mpa.data <- soc.data %>%
  filter(monitor.yr=="baseline" & Treatment==1) %>% 
  ungroup() %>% 
  ##Create community cohesion variable by taking max % participation 
  mutate(group.part = pmax(MarineGroup,OtherGroup,na.rm = T))

soc.mpa.data %>%
  select(MarineGroup,OtherGroup,group.part) %>% head()

############################################################
#SKIM DATA 
############################################################
###Skim data set to look at distributions of governance and social variables 
full.data %>%
  dplyr::group_by(SiteCode) %>%
  select(boundaries.defined.FGD_MPA) %>%
  skim()
full.data %>%
  dplyr::group_by(Year) %>%
  select(boundaries.defined.FGD_MPA) %>%
  skim()
##initial increase in mean number of ways boundaries are defined after the baseline year, but then begins to drop off. 
gov.mpa.post.data %>%
  dplyr::group_by(SiteCode) %>%
  select(boundaries.defined.FGD_MPA) %>%
  skim()
## higher mean number of ways boundaries are defined for MPA sites after baseline year.
## a lot of variation among MPAs 
gov.mpa.post.data %>%
  dplyr::group_by(Year) %>%
  select(boundaries.defined.FGD_MPA) %>%
  skim()
##decline in mean number of ways boundaries defined over time.

##############################################################
# CORRELATION TESTS
##############################################################

##correlation test for gov variables MPAs after baseline year
corr.gov.post <- select(gov.mpa.post.data,boundaries.defined.FGD_MPA:graduated.sanctions)
psych::alpha(corr.gov.post)
corr.bnd.post <- select(gov.mpa.post.data,boundaries.defined.FGD_MPA:internal.boundaries)
psych::alpha(corr.bnd.post)

##correlation test for soc variables MPAs 
# corr.soc.post <- soc.mpa.data %>%
#   select("RightsExclude", "RightsTransfer", "pri.farming", "pri.harv.forest", 
#              "pri.fishing","MAIndex", "EconStatusTrend", "FSIndex", "MTIndex",
#              "RightsAccess", "RightsHarvest", "RightsManage", "MarineGroup", "OtherGroup", 
#              "FreqEatFish", "FreqSaleFish", "PercentIncFish", "PAIndex", "group.part","wealthIndex.pca", 
#               "ed.level", "boat.own", "PercentProteinFish")
# 
# psych::alpha(corr.soc.post,check.keys=TRUE)
# corrgram(corr.soc.post, order=TRUE, lower.panel=panel.shade,
#          upper.panel=panel.pie, text.panel=panel.txt,cor.method="pearson",
#          main="Social variables")
# corr.group.post <- select(soc.mpa.data, MarineGroup:OtherGroup)
# psych::alpha(corr.group.post,check.keys=TRUE)

# Social indicators
marine.rel.col <- c("FreqEatFish","FreqSaleFish","PercentIncFish","pri.fishing", "FreqFish")
wellbeing.col <- c("FSIndex","wealthIndex.pca", "ed.level")
rights.manage.col <- c("RightsManage","RightsExclude","RightsTransfer")
rights.use.col <- c("RightsAccess","RightsHarvest")

psych::alpha(select(soc.mpa.data,marine.rel.col),check.keys=TRUE)

corrgram(select(soc.mpa.data,marine.rel.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Marine Reliance")

psych::alpha(select(soc.mpa.data,wellbeing.col),check.keys=TRUE)

corrgram(select(soc.mpa.data,wellbeing.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Social variables")

psych::alpha(select(soc.mpa.data,rights.manage.col),check.keys=TRUE)

corrgram(select(soc.mpa.data,rights.manage.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Social variables")

psych::alpha(select(soc.mpa.data,rights.use.col),check.keys=TRUE)

corrgram(select(soc.mpa.data,rights.use.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Social variables")

# Goverance indicators
corrgram(select(gov.mpa.post.data,boundaries.defined.FGD_MPA:graduated.sanctions), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Governance variables")

decision.part.col <- c("decision.making.participation","rule.making.participation")
acct.monitoring.col <- c("monitor.eco", "monitor.soc", "monitor.compliance", "accountable.enforcement", "enforcement.frequency",
                         "enforcement.freq", "eco.monitor.sanctions", "soc.monitor.sanctions", "comp.monitor.sanctions", "pen.monitor.sanctions")
congruence.col <- c("user.rule","DRuleEco","DRuleSoc")

psych::alpha(select(gov.mpa.post.data,decision.part.col),check.keys=TRUE)

corrgram(select(gov.mpa.post.data,decision.part.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Decision Making Participation")

psych::alpha(select(gov.mpa.post.data,acct.monitoring.col),check.keys=TRUE)

corrgram(select(gov.mpa.post.data,acct.monitoring.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Accountable Monitoring")

psych::alpha(select(gov.mpa.post.data,congruence.col),check.keys=TRUE)

corrgram(select(gov.mpa.post.data,congruence.col), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,cor.method="spearman",
         main="Congruence with Local Conditions")

####################################################################################
# Group Variables based on internal Consistency Using Z-score 
####################################################################################

##Z score function 
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

##Z-score for Governance Variables 
gov.col <- c("decision.making.participation", "rule.making.participation",
             "monitor.compliance", "internal.boundaries", "external.boundaries",
             "monitor.eco", "monitor.soc", "accountable.enforcement", "enforcement.frequency",
             "enforcement.freq", "eco.monitor.sanctions", "soc.monitor.sanctions",
             "comp.monitor.sanctions", "pen.monitor.sanctions",
             "user.rule", "user.rules.inc",
             "DRuleEco","DRuleSoc", "boundaries.defined.FGD_MPA", 
             "nested.governance", "graduated.sanctions", "compliance.incentives", 
             "govt.support")

##Avg grouped variables by settlement without Z-score 
final.gov.mpa.post.data <- gov.mpa.post.data %>% 
  ungroup() %>% 
  mutate(part.dec.making = rowMeans(select(.,decision.part.col),na.rm = T)) %>%
  mutate(acct.monitoring = rowMeans(select(.,acct.monitoring.col),na.rm = T)) %>%
  mutate(congruency = rowMeans(select(.,congruence.col),na.rm = T)) %>%
  group_by(SettlementID) %>%
  summarise(part.dec.making=mean(part.dec.making, na.rm=T),
            acct.monitoring=mean(acct.monitoring, na.rm=T),
            congruency=mean(congruency, na.rm=T), 
            compliance.incentives=mean(compliance.incentives, na.rm=T),
            graduated.sanctions=mean(graduated.sanctions, na.rm=T),
            nested.governance=mean(nested.governance, na.rm=T),
            govt.support=mean(govt.support, na.rm=T),
            boundaries.defined.FGD_MPA=mean(boundaries.defined.FGD_MPA, na.rm=T))

### Scale all variables by Z-score 
gov.mpa.post.data.z <- gov.mpa.post.data %>%
  group_by(SiteCode) %>%
  mutate(across(gov.col, scale2)) 

##Avg grouped variables by settlement using Z-score 
final.gov.mpa.post.data.z <- gov.mpa.post.data.z %>% 
  ungroup() %>% 
  mutate(part.dec.making = rowMeans(select(.,decision.part.col),na.rm = T)) %>%
  mutate(acct.monitoring = rowMeans(select(.,acct.monitoring.col),na.rm = T)) %>%
  mutate(congruency = rowMeans(select(.,congruence.col),na.rm = T)) %>%
  group_by(SettlementID) %>%
  summarise(part.dec.making=mean(part.dec.making, na.rm=T),
            acct.monitoring=mean(acct.monitoring, na.rm=T),
            congruency=mean(congruency, na.rm=T), 
            compliance.incentives=mean(compliance.incentives, na.rm=T),
            graduated.sanctions=mean(graduated.sanctions, na.rm=T),
            nested.governance=mean(nested.governance, na.rm=T),
            govt.support=mean(govt.support, na.rm=T),
            boundaries.defined.FGD_MPA=mean(boundaries.defined.FGD_MPA, na.rm=T))

## social variables 

soc.col <- c("RightsExclude", "RightsTransfer", "pri.farming", "pri.harv.forest", "TimeMarket",
             "pri.fishing", "EconStatusTrend", "FSIndex", "FreqFish","wealthIndex.pca", "ed.level",
             "ed.level" , "RightsAccess", "RightsHarvest", "RightsManage", "MarineGroup", "OtherGroup", 
             "FreqEatFish", "FreqSaleFish", "PercentIncFish", "PAIndex", "group.part") 

##Settlement Average for grouped variables (marine reliance, rights.use, rights.manage)
final.soc.mpa.data <- soc.mpa.data %>% 
  ungroup() %>% 
  mutate(reliance.marine = rowMeans(select(.,marine.rel.col),na.rm = T)) %>%
  mutate(rights.use = rowMeans(select(.,rights.use.col),na.rm = T)) %>%
  mutate(rights.manage = rowMeans(select(.,rights.manage.col),na.rm = T)) %>%
  group_by(SettlementID) %>%
  summarise(reliance.marine=mean(reliance.marine, na.rm=T),
            rights.use=mean(rights.use , na.rm=T),
            rights.manage=mean(rights.manage , na.rm=T),
            wealthIndex.pca=mean(wealthIndex.pca, na.rm=T),
            EconStatusTrend=mean(EconStatusTrend, na.rm=T), 
            ed.level=mean(ed.level, na.rm=T),
            FSIndex=mean(FSIndex, na.rm=T),
            PAIndex=mean(PAIndex, na.rm=T),
            TimeMarket=mean(TimeMarket, na.rm=T),
            group.part=mean(group.part, na.rm=T))

##Scale social variables by Z-score
soc.mpa.data.z <- soc.mpa.data %>%
  group_by(SiteCode) %>%
  mutate(across(soc.col, scale2)) 

##Settlement average Z-score for grouped social variables
final.soc.mpa.data.z <- soc.mpa.data.z %>% 
  ungroup() %>% 
  mutate(reliance.marine = rowMeans(select(.,marine.rel.col),na.rm = T)) %>%
  mutate(rights.use = rowMeans(select(.,rights.use.col),na.rm = T)) %>%
  mutate(rights.manage = rowMeans(select(.,rights.manage.col),na.rm = T)) %>%
  group_by(SettlementID) %>%
  summarise(reliance.marine=mean(reliance.marine, na.rm=T),
            rights.use=mean(rights.use , na.rm=T),
            rights.manage=mean(rights.manage , na.rm=T),
            wealthIndex.pca=mean(wealthIndex.pca, na.rm=T),
            EconStatusTrend=mean(EconStatusTrend, na.rm=T), 
            ed.level=mean(ed.level, na.rm=T),
            FSIndex=mean(FSIndex, na.rm=T),
            PAIndex=mean(PAIndex, na.rm=T),
            TimeMarket=mean(TimeMarket, na.rm=T),
            group.part=mean(group.part, na.rm=T))

## Now I have two data sets: MPA post governance data for governance variables, 
## in units of standard deviation and averaged at the settlement level. And MPA baseline 
## social variables, in units of standard deviation, averaged at the settlement level

##There is one with data scaled by Z-scores and one without 

#########################################################################################
### Time to left join all three datasets plus MPAID 
#########################################################################################

final.gov.soc.impact.data <- final.impact.data %>%
  left_join(final.gov.mpa.post.data, by = "SettlementID") %>%
  left_join(final.soc.mpa.data, by= "SettlementID") %>%
  left_join(MPA.ID, by = "SettlementID") 

##Z-score dataset 
final.gov.soc.impact.data.z <- final.impact.data %>%
  left_join(final.gov.mpa.post.data.z, by = "SettlementID") %>%
  left_join(final.soc.mpa.data.z, by= "SettlementID") %>%
  left_join(MPA.ID, by = "SettlementID") 

#####################################################
##Correlation of Gov, Impact, and Soc Variables 
#####################################################

gov.col <- c("part.dec.making", "acct.monitoring",
             "boundaries.defined", "congruency","nested.governance", 
             "graduated.sanctions", "compliance.incentives",
             "govt.support")

soc.col <- c("reli.marine",
             "PAIndex","TimeMarket", "group.part", "wealth.index", "ed.level",
             "econ.status", "FSIndex", "rights.use", "rights.manage")

gov.soc.col.grouped <- c( "estimate","part.dec.making", "acct.monitoring",
                          "boundaries.defined", "congruency",
                          "nested.governance", "graduated.sanctions", "compliance.incentives",
                          "govt.support","reli.marine",
                          "PAIndex","TimeMarket", "group.part", "wealth.index", "ed.level",
                          "econ.status", "FSIndex", "rights.use", "rights.manage")

FSImpact.gov.soc.impact.data <-  final.gov.soc.impact.data %>%
  group_by(SettlementID) %>%
  filter(Response=="FSIndex_z") %>%
  ungroup() %>%
  rename("boundaries.defined"="boundaries.defined.FGD_MPA",
         "econ.status" = "EconStatusTrend",
         "reli.marine" = "reliance.marine",
         "wealth.index" = "wealthIndex.pca")

FSImpact.gov.soc.impact.data.z <-  final.gov.soc.impact.data.z %>%
  group_by(SettlementID) %>%
  filter(Response=="FSIndex_z") %>%
  ungroup() %>%
  rename("boundaries.defined"="boundaries.defined.FGD_MPA",
         "econ.status" = "EconStatusTrend",
         "reli.marine" = "reliance.marine",
         "wealth.index" = "wealthIndex.pca" ) 

##Gov Indices Corrgam, Z-score
corrgram(select(FSImpact.gov.soc.impact.data.z, all_of(gov.col)), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt, label.pos = c(0.5, 0.5), label.srt = 5, cor.method="spearman",
         main="Governance Variables") 


##Soc Indices Corrgram, Z-score  
corrgram(select(FSImpact.gov.soc.impact.data.z, all_of(soc.col)), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt, label.pos = c(0.5, 0.5), label.srt = 5, cor.method="spearman",
         main="Social Variables")

##Combined Corrgram 
##Labels cut off, might be able to fix by renaming
corrgram(select(FSImpact.gov.soc.impact.data, all_of(gov.soc.col.grouped)), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt, label.pos = c(0.5, 0.5), label.srt = 5, cor.method="spearman",
         main="Governance, Impact and Social Variables")

##Combined corrgram with Z-score 
##Labels cut off, might be able to fix by renaming
corrgram(select(FSImpact.gov.soc.impact.data.z, all_of(gov.soc.col.grouped)), order=NULL, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt, label.pos = c(0.5, 0.5), label.srt = 5, cor.method="spearman",
         main="Governance, Impact and Social Variables")

# scatter plots
pairs(select(FSImpact.gov.soc.impact.data, estimate,FSIndex, congruency,part.dec.making,reli.marine,rights.use,group.part ) %>% ungroup)
#linear relationship between baseline food security and food security impact 

#scatter plots Z
pairs(select(FSImpact.gov.soc.impact.data.z, estimate,FSIndex, congruency,part.dec.making,reli.marine,rights.use,group.part ) %>% ungroup)

plot.dat <- FSImpact.gov.soc.impact.data %>% 
  ungroup() %>% 
  select(estimate,FSIndex, congruency,part.dec.making,reli.marine,rights.use,group.part) %>% 
  gather(key=var,val=val, FSIndex:group.part)
head(plot.dat)

p.plot.dat <- ggplot(plot.dat, aes(x=val, y=estimate )) +
  geom_point() + 
  facet_grid(.~var, scale = "free") +
  ggtitle("Scatter plot") +
  scale_color_brewer(palette = "Dark2") +
  #  geom_smooth(method=lm, se=FALSE) +
  geom_smooth(method=loess, se=FALSE) +
  labs(x=NULL,y="FS Impact", title="Original scale")

##scatterplots of FSimpact and gov/soc variables
plot.dat.z <- FSImpact.gov.soc.impact.data.z %>% 
  ungroup() %>% 
  select(estimate,FSIndex, congruency,part.dec.making,reli.marine,rights.use,group.part) %>% 
  gather(key=var,val=val, FSIndex:group.part)
head(plot.dat.z)

p.plot.dat.z <- ggplot(plot.dat.z, aes(x=val, y=estimate )) +
  geom_point() + 
  facet_grid(.~var, scale = "free") +
  ggtitle("Scatter plot") +
  scale_color_brewer(palette = "Dark2") +
  #  geom_smooth(method=lm, se=FALSE) +
  geom_smooth(method=loess, se=FALSE) +
  labs(x=NULL,y="FS Impact", title="Z scores")

plot_grid(p.plot.dat.z,p.plot.dat, nrow=2)

##Participation Decision Making is bimodal; divide into "low" and "high"
part.dat.z <- FSImpact.gov.soc.impact.data.z %>% 
  mutate(particip=ifelse(part.dec.making<0,"low","high")) %>% 
  ungroup() %>% 
  filter(!is.na(particip) & !is.na(estimate)) %>% 
  select(estimate,particip,part.dec.making) 

##Standard Error Bars  
part.plot.dat.z <- part.dat.z %>% 
  group_by(particip) %>% 
  summarise(fs.estimate=mean(estimate), fs.sd=sd(estimate), num=n()) %>% 
  mutate(se=fs.sd/sqrt(num),se.lower=fs.estimate-(2*se),se.upper=fs.estimate+(2*se))

p.particip <- ggplot(part.plot.dat.z,aes(x=particip, y=fs.estimate)) + 
  geom_errorbar(aes(ymin=se.lower, ymax=se.upper), width=0) +
  geom_point(shape = 16) +
  geom_hline(yintercept = 0)



# linear model FSIndex and estimate 
summary(lm(estimate~FSIndex, data=FSImpact.gov.soc.impact.data))
##The linear relationship between baseline food security and the food security impact estimate 
## is statistically significant with a p-value of 3.945e-06. The adjusted R-squared value is .2502,
##Meaning 25% of variation in the FS impact estimate is due to baseline food security index (Food security measured before the MPA was established)

#linear model FSIndex and estimate z-score
summary(lm(estimate~FSIndex, data=FSImpact.gov.soc.impact.data.z))
##The linear relationship between baseline food security and the food security impact estimate 
## is statistically significant with a p-value of 7.855e-05. The adjusted R-squared value is .1871,
##Meaning 18% of variation in the FS impact estimate is due to baseline food security index (Food security measured before the MPA was established)

# t-test, participation in decision making, Z-score 
t.test(estimate~particip,data=filter(part.dat.z))
##p-value is .07, the difference in mean FSImpact between high and low participation groups is not statistically significant

#linear model, congruency 
summary(lm(estimate~congruency, data=FSImpact.gov.soc.impact.data))
##The correlation between congruency and FSImpact is not statistically significant (p-value: 0.2184)

##linear model, congruency Z-score
summary(lm(estimate~congruency, data=FSImpact.gov.soc.impact.data.z))
##The correlation between congruency and FSImpact is not statistically significant (p-value: 0.2084)

##linear model, group.part 
summary(lm(estimate~group.part, data=FSImpact.gov.soc.impact.data))
##p-value: 0.06936

##linear model, group.part Z-score 
summary(lm(estimate~group.part, data=FSImpact.gov.soc.impact.data.z))
## The negative linear relationship between group participation (proxy for community cohesion) and 
## FSImpact is statistically significant with a p-value p-value: 0.04059. Adjusted R-squared:  0.04447

##linear model, reli.marine 
summary(lm(estimate~reli.marine, data=FSImpact.gov.soc.impact.data))
##p-value: 0.866

##linear model, reli.marine Z-score 
summary(lm(estimate~reli.marine, data=FSImpact.gov.soc.impact.data.z))
##p-value: 0.1388

##linear model, rights.use 
summary(lm(estimate~rights.use, data=FSImpact.gov.soc.impact.data))
##p-value: 0.1764

##linear model, rights.use Z-score
summary(lm(estimate~rights.use, data=FSImpact.gov.soc.impact.data.z))
## The positive linear relationship between food security impacts and rights use is statistically 
## significant with a p-value of 0.002428. Adjusted R-squared:  0.1099

##### FSImpact Conclusions ########
## FSIndex and rights use significant positive correlation 
##group.part significant negative correlation


##### Tests, Relationship between Governance Variables #######

##Scatterplots
pairs(select(FSImpact.gov.soc.impact.data, nested.governance, govt.support, acct.monitoring,graduated.sanctions, compliance.incentives) %>% ungroup)
##linear relationship: acct.monitoring, graduated.sanctions, compliance.incentives

##Scatterplots Z
pairs(select(FSImpact.gov.soc.impact.data.z, nested.governance, govt.support, acct.monitoring,graduated.sanctions, compliance.incentives) %>% ungroup)
##linear relationship: acct.monitoring, graduated.sanctions, compliance.incentives, gov.support; gov.support and nested.governance 



##########################################################################################
# Food Security Outcomes and Governance
##########################################################################################

#Single Linear Regression, Food Security and Participation in Decision Making 
final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = part.dec.making, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Participation in Decision Making", x = "Participation Decision Making", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = acct.monitoring, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Accountable Monitoring",
       x = "Accountable Monitoring", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = congruency, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Congruency",
       x = "Congruency", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = compliance.incentives, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Compliance Incentives",
       x = "Compliance Incentives", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = graduated.sanctions, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Graduated Sanctions",
       x = "Graduated Sanctions", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = nested.governance, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Nested Governance",
       x = "Nested Governance", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = govt.support, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Government Support",
       x = "Government Support", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 


final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = boundaries.defined.FGD_MPA, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Boundaries Defined",
       x = "Boundaries Defined", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white")) 

###########################################################################
##Outcomes Social 
###########################################################################

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = reliance.marine, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Baseline Marine Reliance",
       x = "Marine Reliance", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

#####Negative Relationship between Baseline Food Security and Food Security Impact
final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = FSIndex, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes and Baseline Food Security Index",
       x = "Basline Food Security Index", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = EconStatusTrend, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Economic Status",
       x = "Basline Econ Status Trend", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))


final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = wealthIndex.pca, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Wealth Index",
       x = "Basline Wealth Index", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = rights.use, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Marine Rights Use Tenure Index",
       x = "Basline Marine Rights Use Tenure Index", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = rights.manage, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Marine Rights Manage Tenure Index",
       x = "Basline Marine Rights Manage Tenure Index", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = PAIndex, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Place Attachment Index",
       x = "Basline Personal Assets Index", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = group.part, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Group Participation",
       x = "Basline Group Participation Index", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = ed.level, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Education Level",
       x = "Basline Education Level", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

final.gov.soc.impact.data %>%
  filter(Response=="FSIndex_z") %>%
  ggplot(aes(x = TimeMarket, y = estimate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPA Food Security Outcomes Baseline Time to Market",
       x = "Basline Time to Market", y = "Food Security Index") +
  theme(panel.background = element_rect(color = "black", fill = "white"))
