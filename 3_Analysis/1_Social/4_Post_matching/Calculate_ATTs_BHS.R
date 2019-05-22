# ---
# code: Calculate outcome family indices for BHS MPA social monitoring 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: May 2018
# modified: December 2018


pacman::p_load(MBESS,Match,xlsx)


# ---- 1.1 Source functions & data frames ----

source('2_Functions/2_Analysis/Function_outcome_ATT_method1.R')
source('2_Functions/2_Analysis/Function_att_significance_by_MPA.R')
source('2_Functions/2_Analysis/Function_att_significance_by_seascape.R')


# BigFive
# Middle 15


# ---- 1.2 Import flat file outputs ----

#-- Master pairs lists, t2 and t4
master.t2.A <- xlsx::read.xlsx('x_Flat_data_files/1_Social/Inputs/BHS/t2_impacts/master_t2_panelA.xlsx', sheetName='Sheet 1')
master.t4.A <- xlsx::read.xlsx('x_Flat_data_files/1_Social/Inputs/BHS/t4_impacts/master_t4_panelA.xlsx', sheetName='Sheet 1')


# ---- 1.3 Calculate household-level outcome / ATT dataframes ----

#-- BigFive, t2
hfs.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = HHData, var = FSIndex)
asset.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = HHData, var = MAIndex)
tenure.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = HHData, var = MTIndex)
attach.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = HHData, var = PAIndex)
enrol.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = HHData, var = SERate)

#-- BigFive, t4
hfs.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = HHData, var = FSIndex)
asset.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = HHData, var = MAIndex)
tenure.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = HHData, var = MTIndex)
attach.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = HHData, var = PAIndex)
enrol.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = HHData, var = SERate)

# #-- Middle15, t2
# chfs.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = CFSIndex.inv)
# access.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = Acc.Harv)
# manage.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = Man.Excl.Trans)
# OD.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = ODIndex)
# MP.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = MarineGroup)                                     #
# OP.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = OtherGroup)
# econ.decline.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = EconDecline)
# econ.stable.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = EconStable)
# econ.increase.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = EconIncrease)
# con.decrease.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = ConDecrease)
# con.stable.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = ConStable)
# con.increase.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = ConIncrease)
# morbidity.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = DaysUnwell)
# f.enrol.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = FemaleSERate)
# m.enrol.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = MaleSERate)
# attain.4.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = Attain.4)
# attain.5.outcome.t2 <- outcome_ATT_method1_allpairs(pairs = master.t2.A, outcomes = Middle15, var = Attain.5)
# 
# #-- Middle15, t4
# chfs.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = CFSIndex.inv)
# access.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = Acc.Harv)
# manage.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = Man.Excl.Trans)
# OD.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = ODIndex)
# MP.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = MarineGroup)                                     #
# OP.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = OtherGroup)
# econ.decline.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = EconDecline)
# econ.stable.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = EconStable)
# econ.increase.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = EconIncrease)
# con.decrease.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = ConDecrease)
# con.stable.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = ConStable)
# con.increase.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = ConIncrease)
# morbidity.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = DaysUnwell)
# f.enrol.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = FemaleSERate)
# m.enrol.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = MaleSERate)
# attain.4.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = Attain.4)
# attain.5.outcome.t4 <- outcome_ATT_method1_allpairs(pairs = master.t4.A, outcomes = Middle15, var = Attain.5)
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Output MPA-level ATTs and Abadie Imbens errors into dataframes for each variable ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# output household food security att & AI error, by year
hfs.t2.att.byMPA <- att.significance.by.group(outcomes = hfs.outcome.t2, HHData = HHData, grouping_var = "MPAID")
hfs.t4.att.byMPA <- att.significance.by.group(outcomes = hfs.outcome.t4, HHData = HHData, grouping_var = "MPAID")

hfs.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(hfs.t2.att.byMPA,hfs.t4.att.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# output household material assets att & AI error, by year
asset.t2.att.byMPA <- att.significance.by.group(outcomes = asset.outcome.t2, HHData = HHData, grouping_var = "MPAID")
asset.t4.att.byMPA <- att.significance.by.group(outcomes = asset.outcome.t4, HHData = HHData, grouping_var = "MPAID")

asset.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(asset.t2.att.byMPA,asset.t4.att.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# output marine tenure att & AI error, by year
tenure.t2.att.byMPA <- att.significance.by.group(outcomes = tenure.outcome.t2, HHData = HHData, grouping_var = "MPAID")
tenure.t4.att.byMPA <- att.significance.by.group(outcomes = tenure.outcome.t4, HHData = HHData, grouping_var = "MPAID")

tenure.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(tenure.t2.att.byMPA,tenure.t4.att.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# output enrollment att & AI error, by year
enrol.t2.att.byMPA <- att.significance.by.group(outcomes = enrol.outcome.t2, HHData = HHData, grouping_var = "MPAID")
enrol.t4.att.byMPA <- att.significance.by.group(outcomes = enrol.outcome.t4, HHData = HHData, grouping_var = "MPAID")

enrol.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(enrol.t2.att.byMPA,enrol.t4.att.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# output place attachment att & AI error, by year
attach.t2.att.byMPA <- att.significance.by.group(outcomes = attach.outcome.t2, HHData = HHData, grouping_var = "MPAID")
attach.t4.att.byMPA <- att.significance.by.group(outcomes = attach.outcome.t4, HHData = HHData, grouping_var = "MPAID")

attach.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(attach.t2.att.byMPA,attach.t4.att.byMPA)) %>%
  dplyr::rename(mpa = get.group.)



# length of matched HH per MPA

# (master.t2.A from Calculate_ATTs_BHS.R)
num.obs.t2.perMPA <-
  master.t2.A %>%
  dplyr::rename(HouseholdID=HouseholdID.tr1.t2) %>%
  left_join(HHData,by="HouseholdID") %>%
  group_by(MPAID) %>%
  summarise(num.HH = length(HouseholdID[!is.na(HouseholdID)]))

num.obs.t4.perMPA <-
  master.t4.A %>%
  dplyr::rename(HouseholdID=HouseholdID.tr1.t4) %>%
  left_join(HHData,by="HouseholdID") %>%
  group_by(MPAID) %>%
  summarise(num.HH = length(HouseholdID[!is.na(HouseholdID)]))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- Output Seascape-level ATTs and Abadie Imbens errors into dataframes for each variable ----
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# output household food security att & AI error, by year
hfs.t2.att.BHS.seascape <- att.significance.by.seascape(outcomes = hfs.outcome.t2, HHData = HHData)
hfs.t4.att.BHS.seascape <- att.significance.by.seascape(outcomes = hfs.outcome.t4, HHData = HHData)

hfs.att.BHS.seascape <- 
  data.frame(year = c("t2", "t4"), 
             rbind.data.frame(hfs.t2.att.BHS.seascape,hfs.t4.att.BHS.seascape))


# output household material assets att & AI error, by year
asset.t2.att.BHS.seascape <- att.significance.by.seascape(outcomes = asset.outcome.t2, HHData = HHData)
asset.t4.att.BHS.seascape <- att.significance.by.seascape(outcomes = asset.outcome.t4, HHData = HHData)

asset.att.BHS.seascape <- 
  data.frame(year = c("t2", "t4"), 
             rbind.data.frame(asset.t2.att.BHS.seascape,asset.t4.att.BHS.seascape))


# output marine tenure att & AI error, by year
tenure.t2.att.BHS.seascape <- att.significance.by.seascape(outcomes = tenure.outcome.t2, HHData = HHData)
tenure.t4.att.BHS.seascape <- att.significance.by.seascape(outcomes = tenure.outcome.t4, HHData = HHData)

tenure.att.BHS.seascape <- 
  data.frame(year = c("t2", "t4"), 
             rbind.data.frame(tenure.t2.att.BHS.seascape,tenure.t4.att.BHS.seascape))


# output enrollment att & AI error, by year
enrol.t2.att.BHS.seascape <- att.significance.by.seascape(outcomes = enrol.outcome.t2, HHData = HHData)
enrol.t4.att.BHS.seascape <- att.significance.by.seascape(outcomes = enrol.outcome.t4, HHData = HHData)

enrol.att.BHS.seascape <- 
  data.frame(year = c("t2", "t4"), 
             rbind.data.frame(enrol.t2.att.BHS.seascape,enrol.t4.att.BHS.seascape))


# output place attachment att & AI error, by year
attach.t2.att.BHS.seascape <- att.significance.by.seascape(outcomes = attach.outcome.t2, HHData = HHData)
attach.t4.att.BHS.seascape <- att.significance.by.seascape(outcomes = attach.outcome.t4, HHData = HHData)

attach.att.BHS.seascape <- 
  data.frame(year = c("t2", "t4"), 
             rbind.data.frame(attach.t2.att.BHS.seascape,attach.t4.att.BHS.seascape))




rm(asset.t2.att.byMPA,asset.t4.att.byMPA,attach.t2.att.byMPA,attach.t4.att.byMPA,enrol.t2.att.byMPA,
   enrol.t4.att.byMPA,hfs.t2.att.byMPA,hfs.t4.att.byMPA,tenure.t2.att.byMPA,tenure.t4.att.byMPA,
   asset.t2.att.BHS.seascape,asset.t4.att.BHS.seascape,attach.t2.att.BHS.seascape,attach.t4.att.BHS.seascape,enrol.t2.att.BHS.seascape,
   enrol.t4.att.BHS.seascape,hfs.t2.att.BHS.seascape,hfs.t4.att.BHS.seascape,tenure.t2.att.BHS.seascape,tenure.t4.att.BHS.seascape)




# # ---- 2.1 Health index ---- 
# # (household food security, child food security, household morbidity)
# 
# health.index.t2.data <- 
#   full_join(plyr::rename(hfs.outcome.t2,c('MPA.outcome' = 'hfs.MPA.outcome',
#                                           'Control.outcome' = 'hfs.Control.outcome',
#                                           'ATT' = 'hfs.ATT')),
#             plyr::rename(chfs.outcome.t2,c('MPA.outcome' = 'chfs.MPA.outcome',
#                                            'Control.outcome' = 'chfs.Control.outcome',
#                                            'ATT' = 'chfs.ATT')),
#             by=c('tr1tx','tr0tx','tr1t0','tr0t0')) %>%
#   full_join(plyr::rename(morbidity.outcome.t2,c('MPA.outcome' = 'morbidity.MPA.outcome',
#                                                 'Control.outcome' = 'morbidity.Control.outcome',
#                                                 'ATT' = 'morbidity.ATT')),
#             by=c('tr1tx','tr0tx','tr1t0','tr0t0')) %>%
#   left_join(HHData[,c("HouseholdID","SettlementID","MPAID")],by=c('tr1tx'='HouseholdID'))
#   
# health.index.contmeans.t2 <-
#   health.index.t2.data %>%
#   group_by(MPAID) %>%
#   summarise(hfs.Control.mean = mean(hfs.Control.outcome,na.rm=T),
#             hfs.Control.sd = sd(hfs.Control.outcome,na.rm=T),
#             chfs.Control.mean = mean(chfs.Control.outcome,na.rm=T),
#             chfs.Control.sd = sd(chfs.Control.outcome,na.rm=T),
#             morbidity.Control.mean = mean(morbidity.Control.outcome,na.rm=T),
#             morbidity.Control.sd = sd(morbidity.Control.outcome,na.rm=T))
#   
# health.index.t2 <-
#   left_join(health.index.t2.data,health.index.contmeans.t2,by="MPAID") %>%
#   mutate(hfs.na = ifelse(is.na(hfs.MPA.outcome),1,0),
#          chfs.na = ifelse(is.na(chfs.MPA.outcome),1,0),
#          morbidity.na = ifelse(is.na(morbidity.MPA.outcome),1,0),
#          total.na = hfs.na + chfs.na + morbidity.na,
#          std.hfs.MPA = ifelse(!is.na(hfs.MPA.outcome),
#                                       (hfs.MPA.outcome - hfs.Control.mean)/hfs.Control.sd,
#                                       NA),
#          std.hfs.Control = ifelse(!is.na(hfs.Control.outcome),
#                                           (hfs.Control.outcome - hfs.Control.mean)/hfs.Control.sd,
#                                           NA),
#          std.chfs.MPA = ifelse(!is.na(chfs.MPA.outcome),
#                                        (chfs.MPA.outcome - chfs.Control.mean)/chfs.Control.sd,
#                                        NA),
#          std.chfs.Control = ifelse(!is.na(chfs.Control.outcome),
#                                            (chfs.Control.outcome - chfs.Control.mean)/chfs.Control.sd,
#                                            NA),
#          std.morbidity.MPA = ifelse(!is.na(morbidity.MPA.outcome),
#                                             (morbidity.MPA.outcome - morbidity.Control.mean)/morbidity.Control.sd,
#                                             NA),
#          std.morbidity.Control = ifelse(!is.na(morbidity.Control.outcome),
#                                                 (morbidity.Control.outcome - morbidity.Control.mean)/morbidity.Control.sd,
#                                                 NA)) %>%
#   mutate(health.MPA = rowMeans(.[,c("std.hfs.MPA","std.chfs.MPA","std.morbidity.MPA")],na.rm=T),
#          health.Control = rowMeans(.[,c("std.hfs.Control","std.chfs.Control","std.morbidity.Control")],na.rm=T))
# 
# health.contindex.t2 <-
#   health.index.t2 %>%
#   group_by(MPAID) %>%
#   summarise(mean.health.Control=mean(health.Control,na.rm=T),
#             sd.health.Control=sd(health.Control,na.rm=T))
# 
# std.health.index.t2 <-
#   left_join(health.index.t2,health.contindex.t2,by="MPAID") %>%
#   mutate(std.health.MPA = ifelse(!is.na(health.MPA),
#                                  (health.MPA-mean.health.Control)/sd.health.Control,
#                                  NA))
# MPA.level.health.index.t2 <-
#   std.health.index.t2 %>%
#   group_by(MPAID) %>%
#   summarise(mean.health.index=mean(std.health.MPA,na.rm=T),
#             sd.health.index=sd(std.health.MPA,na.rm=T))
# 
# rm(health.index.t2.data,health.index.contmeans.t2,health.index.t2,health.contindex.t2)
# 
# # ---- 2.2 Economic well-being index ---- 
# # (household material assets, occupational dependence on fishing, increase in economic status)
# 
# econ.index.t2.data <- 
#   full_join(plyr::rename(asset.outcome.t2,c('MPA.outcome' = 'asset.MPA.outcome',
#                                           'Control.outcome' = 'asset.Control.outcome',
#                                           'ATT' = 'asset.ATT')),
#             plyr::rename(OD.outcome.t2,c('MPA.outcome' = 'OD.MPA.outcome',
#                                            'Control.outcome' = 'OD.Control.outcome',
#                                            'ATT' = 'OD.ATT')),
#             by=c('tr1tx','tr0tx','tr1t0','tr0t0')) %>%
#   full_join(plyr::rename(econ.increase.outcome.t2,c('MPA.outcome' = 'econ.increase.MPA.outcome',
#                                                 'Control.outcome' = 'econ.increase.Control.outcome',
#                                                 'ATT' = 'econ.increase.ATT')),
#             by=c('tr1tx','tr0tx','tr1t0','tr0t0')) %>%
#   left_join(HHData[,c("HouseholdID","SettlementID","MPAID")],by=c('tr1tx'='HouseholdID'))
# 
# econ.index.contmeans.t2 <-
#   econ.index.t2.data %>%
#   group_by(MPAID) %>%
#   summarise(asset.Control.mean = mean(asset.Control.outcome,na.rm=T),
#             asset.Control.sd = sd(asset.Control.outcome,na.rm=T),
#             OD.Control.mean = mean(OD.Control.outcome,na.rm=T),
#             OD.Control.sd = sd(OD.Control.outcome,na.rm=T),
#             econ.increase.Control.mean = mean(econ.increase.Control.outcome,na.rm=T),
#             econ.increase.Control.sd = sd(econ.increase.Control.outcome,na.rm=T))
# 
# econ.index.t2 <-
#   left_join(econ.index.t2.data,econ.index.contmeans.t2,by="MPAID") %>%
#   mutate(asset.na = ifelse(is.na(asset.MPA.outcome),1,0),
#          OD.na = ifelse(is.na(OD.MPA.outcome),1,0),
#          econ.increase.na = ifelse(is.na(econ.increase.MPA.outcome),1,0),
#          total.na = asset.na + OD.na + econ.increase.na,
#          std.asset.MPA = ifelse(!is.na(asset.MPA.outcome),
#                               (asset.MPA.outcome - asset.Control.mean)/asset.Control.sd,
#                               NA),
#          std.asset.Control = ifelse(!is.na(asset.Control.outcome),
#                                   (asset.Control.outcome - asset.Control.mean)/asset.Control.sd,
#                                   NA),
#          std.OD.MPA = ifelse(!is.na(OD.MPA.outcome),
#                                (OD.MPA.outcome - OD.Control.mean)/OD.Control.sd,
#                                NA),
#          std.OD.Control = ifelse(!is.na(OD.Control.outcome),
#                                    (OD.Control.outcome - OD.Control.mean)/OD.Control.sd,
#                                    NA),
#          std.econ.increase.MPA = ifelse(!is.na(econ.increase.MPA.outcome),
#                                     (econ.increase.MPA.outcome - econ.increase.Control.mean)/econ.increase.Control.sd,
#                                     NA),
#          std.econ.increase.Control = ifelse(!is.na(econ.increase.Control.outcome),
#                                         (econ.increase.Control.outcome - econ.increase.Control.mean)/econ.increase.Control.sd,
#                                         NA)) %>%
#   mutate(econ.MPA = rowMeans(.[,c("std.asset.MPA","std.OD.MPA","std.econ.increase.MPA")],na.rm=T),
#          econ.Control = rowMeans(.[,c("std.asset.Control","std.OD.Control","std.econ.increase.Control")],na.rm=T))
# 
# econ.contindex.t2 <-
#   econ.index.t2 %>%
#   group_by(MPAID) %>%
#   summarise(mean.econ.Control=mean(econ.Control,na.rm=T),
#             sd.econ.Control=sd(econ.Control,na.rm=T))
# 
# std.econ.index.t2 <-
#   left_join(econ.index.t2,econ.contindex.t2,by="MPAID") %>%
#   mutate(std.econ.MPA = ifelse(!is.na(econ.MPA),
#                                  (econ.MPA-mean.econ.Control)/sd.econ.Control,
#                                  NA))
# MPA.level.econ.index.t2 <-
#   std.econ.index.t2 %>%
#   group_by(MPAID) %>%
#   summarise(mean.econ.index=mean(std.econ.MPA,na.rm=T),
#             sd.econ.index=sd(std.econ.MPA,na.rm=T))
# 
# rm(econ.index.t2.data,econ.index.contmeans.t2,econ.index.t2,econ.contindex.t2)
# 
# 
# # ---- 2.3 Empowerment index ----
# # (marine tenure, participation in marine groups, participation in other groups)
# 
# marine tenure
# marine group
# other group
# 
# # ---- 2.4 Enrollment index ----
# # (percent of female children enrolled, percent of male children enrolled)
# 
# f.enrollment
# m.enrollment
# 
# # ---- 2.5 Culture index ----
# # (place attachment, decrease in social conflict)
# 
# place attachment
# social conflict decrease
# 
