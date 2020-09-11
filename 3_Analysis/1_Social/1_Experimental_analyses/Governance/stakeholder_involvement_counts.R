
# Packages
install.packages('pacman') # only need to do once! This package helps you install or load the packages you need
pacman::p_load(rio,tidyverse)# load or install necessary packages

# Directories (change to the location where you have files)
inputdir <- "C:/Users/david/Dropbox/Governance analysis/tables/"
outputdir <- "C:/Users/david/Dropbox/Governance analysis/tables/"

# Read in data
fg.data <- import(paste0(inputdir,"20200506_MPA_goverance_unified_master.xlsx"), which = "FGD_STAKEHOLDERS") %>% 
  as.tibble() 

# Look at unique stakeholder group names
sort(unique(fg.data$StakeholderNameL))

# Clean data (have a look to see if there are any more that need to be cleaned)
fg.data <- fg.data %>% 
  mutate(StakeholderNameL=tolower(StakeholderNameL),
         StakeholderNameL=gsub("pemerintah lokal.*","pemerintah lokal",StakeholderNameL), 
         StakeholderNameL=gsub("lainnya.*","lainnya",StakeholderNameL),
         StakeholderNameL=gsub("pemerintahprovinsi","pemerintah provinsi",StakeholderNameL),
         StakeholderNameL=gsub("pemerintah propinsi","pemerintah provinsi",StakeholderNameL),
         StakeholderNameL=gsub("provincial government","pemerintah provinsi",StakeholderNameL),
         StakeholderNameL=gsub("provincial ngo","organisasi non-pemerintah provinsi",StakeholderNameL))

sort(unique(fg.data$StakeholderNameL))

# One way to group and count data
est.group.by.site <- fg.data %>% 
  filter(ParticipateEstablish==1) %>% 
  group_by(SiteCode,MonitoringYear,StakeholderNameL) %>% 
  count()

bound.group.by.site <- fg.data %>% 
  filter(ParticipateBoundaries==1) %>% 
  group_by(SiteCode,MonitoringYear,StakeholderNameL) %>% 
  count()

comb.data <- full_join(est.group.by.site,bound.group.by.site, by=c("SiteCode","MonitoringYear","StakeholderNameL"))
arrange(comb.data,SiteCode,StakeholderNameL)

# Another way to group and count data (think about the groupings if they make sense e.g. monitoring years)
comb.data <- fg.data %>% 
  select(SettlementCode,SiteCode,MonitoringYear,StakeholderNameL,ParticipateEstablish:ParticipateRules) %>% 
  gather(key=partic.type,value=val,ParticipateEstablish:ParticipateRules) %>% 
  filter(val==1) %>% 
  group_by(SiteCode,MonitoringYear,StakeholderNameL,partic.type) %>% 
  count() 

arrange(comb.data,SiteCode,StakeholderNameL,partic.type)

# Plot bar graph (many ways to group and plot, think about which options make sense)
ggplot(data=comb.data,aes(x=SiteCode,y=n,fill=StakeholderNameL)) +
  geom_bar(stat = "identity") 

ggplot(data=comb.data,aes(x=SiteCode,y=n,fill=StakeholderNameL)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~MonitoringYear)

ggplot(data=comb.data,aes(x=StakeholderNameL,y=n,fill=SiteCode)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))
