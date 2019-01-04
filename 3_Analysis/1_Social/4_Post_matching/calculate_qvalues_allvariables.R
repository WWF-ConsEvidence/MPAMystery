# ---
# code: Calculate q values for all outcomes, for BHS MPA social monitoring 
# author: Kelly Claborn, Louise Glew, louise.glew@gmail.com
# created: May 2018
# modified: 



#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Call in libraries, import raw data and source functions ----
#         1.1 Call libraries
#         1.2 Source functions
#         1.3 Import raw data
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Call libraries ----

pacman::p_load(qvalue)


# ---- 1.2 Source functions ----
# !!! SOURCE ATT SIG FUNCTION


# ---- 1.3 Import raw data ----

# upload all 2 year outcome data from .xlsx  (each variable is a different tab from same .xlsx file)
hfs.outcome.t2 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t2_impacts/BigFive_panel_outcomes_t2.xlsx',sheet='hfs_outcome_t2')
asset.outcome.t2 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t2_impacts/BigFive_panel_outcomes_t2.xlsx',sheet='asset_outcome_t2')
tenure.outcome.t2 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t2_impacts/BigFive_panel_outcomes_t2.xlsx',sheet='tenure_outcome_t2')
enrol.outcome.t2 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t2_impacts/BigFive_panel_outcomes_t2.xlsx',sheet='enrol_outcome.t2')
attach.outcome.t2 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t2_impacts/BigFive_panel_outcomes_t2.xlsx',sheet='attach_outcome_t2')

# upload all 4 year outcome data from .xlsx  (each variable is a different tab from same .xlsx file)
hfs.outcome.t4 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t4_impacts/BigFive_panel_outcomes_t4.xlsx',sheet='hfs_outcome_t4')
asset.outcome.t4 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t4_impacts/BigFive_panel_outcomes_t4.xlsx',sheet='asset_outcome_t4')
tenure.outcome.t4 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t4_impacts/BigFive_panel_outcomes_t4.xlsx',sheet='tenure_outcome_t4')
enrol.outcome.t4 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t4_impacts/BigFive_panel_outcomes_t4.xlsx',sheet='enrol_outcome.t4')
attach.outcome.t4 <- readxl::read_xlsx('2_Social/FlatDataFiles/BHS/t4_impacts/BigFive_panel_outcomes_t4.xlsx',sheet='attach_outcome_t4')


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 2: Output ATTs and Abadie Imbens errors into dataframes ----
#         2.1 Economic well-being
#         2.2 Culture
#         2.3 Empowerment
#         2.4 Health
#         2.5 Education
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Economic well-being ----

  #-- 2.1.1 Household material assets att & AI error, by year
asset.att.t2.byMPA <- att.significance(outcomes = asset.outcome.t2, HHData = HHData, grouping_var = "MPAID")
asset.att.t4.byMPA <- att.significance(outcomes = asset.outcome.t4, HHData = HHData, grouping_var = "MPAID")

asset.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(asset.att.t2.byMPA,asset.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.1.2 Occupational dependence on fishing att & AI error, by year
OD.att.t2.byMPA <- att.significance(outcomes = OD.outcome.t2, HHData = HHData, grouping_var = "MPAID")
OD.att.t4.byMPA <- att.significance(outcomes = OD.outcome.t4, HHData = HHData, grouping_var = "MPAID")

OD.att.byMPA <-
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(OD.att.t2.byMPA,OD.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.1.3 Economic status decline att & AI error, by year
econ.decline.att.t2.byMPA <- att.significance(outcomes = econ.decline.outcome.t2, HHData = HHData, grouping_var = "MPAID")
econ.decline.att.t4.byMPA <- att.significance(outcomes = econ.decline.outcome.t4, HHData = HHData, grouping_var = "MPAID")

econ.decline.att.byMPA <-
  data.frame(year = c(rep("t2", 6), rep("t4", 6)),
             rbind.data.frame(econ.decline.att.t2.byMPA,econ.decline.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.1.4 Economic status increase att & AI error, by year
econ.increase.att.t2.byMPA <- att.significance(outcomes = econ.increase.outcome.t2, HHData = HHData, grouping_var = "MPAID")
econ.increase.att.t4.byMPA <- att.significance(outcomes = econ.increase.outcome.t4, HHData = HHData, grouping_var = "MPAID")

econ.increase.att.byMPA <-
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(econ.increase.att.t2.byMPA,econ.increase.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# ---- 2.2 Culture ----

  #-- 2.2.1 Place attachment att & AI error, by year
attach.att.t2.byMPA <- att.significance(outcomes = attach.outcome.t2, HHData = HHData, grouping_var = "MPAID")
attach.att.t4.byMPA <- att.significance(outcomes = attach.outcome.t4, HHData = HHData, grouping_var = "MPAID")

attach.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(attach.att.t2.byMPA,attach.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.2.2 Social conflict decrease att & AI error, by year
con.decrease.att.t2.byMPA <- att.significance(outcomes = con.decrease.outcome.t2, HHData = HHData, grouping_var = "MPAID")
con.decrease.att.t4.byMPA <- att.significance(outcomes = con.decrease.outcome.t4, HHData = HHData, grouping_var = "MPAID")

con.decrease.att.byMPA <-
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(con.decrease.att.t2.byMPA,con.decrease.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.2.3 Social conflict increase att & AI error, by year
con.increase.att.t2.byMPA <- att.significance(outcomes = con.increase.outcome.t2, HHData = HHData, grouping_var = "MPAID")
con.increase.att.t4.byMPA <- att.significance(outcomes = con.increase.outcome.t4, HHData = HHData, grouping_var = "MPAID")

con.increase.att.byMPA <-
  data.frame(year = c(rep("t2", 6), rep("t4", 6)),
             rbind.data.frame(con.increase.att.t2.byMPA,con.increase.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# ---- 2.3 Empowerment ----

  #-- 2.3.1 Marine tenure att & AI error, by year
tenure.att.t2.byMPA <- att.significance(outcomes = tenure.outcome.t2, HHData = HHData, grouping_var = "MPAID")
tenure.att.t4.byMPA <- att.significance(outcomes = tenure.outcome.t4, HHData = HHData, grouping_var = "MPAID")

tenure.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(tenure.att.t2.byMPA,tenure.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.3.2 Marine group participation att & AI error, by year
MP.att.t2.byMPA <- att.significance(outcomes = MP.outcome.t2, HHData = HHData, grouping_var = "MPAID")
MP.att.t4.byMPA <- att.significance(outcomes = MP.outcome.t4, HHData = HHData, grouping_var = "MPAID")

MP.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(MP.att.t2.byMPA,MP.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.3.3 Other group participation att & AI error, by year
OP.att.t2.byMPA <- att.significance(outcomes = OP.outcome.t2, HHData = HHData, grouping_var = "MPAID")
OP.att.t4.byMPA <- att.significance(outcomes = OP.outcome.t4, HHData = HHData, grouping_var = "MPAID")

OP.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(OP.att.t2.byMPA,OP.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# ---- 2.4 Health ----

  #-- 2.4.1 Household food security att & AI error, by year
hfs.att.t2.byMPA <- att.significance(outcomes = hfs.outcome.t2, HHData = HHData, grouping_var = "MPAID")
hfs.att.t4.byMPA <- att.significance(outcomes = hfs.outcome.t4, HHData = HHData, grouping_var = "MPAID")

hfs.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(hfs.att.t2.byMPA,hfs.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.4.2 Child food security att & AI error, by year
chfs.att.t2.byMPA <- att.significance(outcomes = chfs.outcome.t2, HHData = HHData, grouping_var = "MPAID")
chfs.att.t4.byMPA <- att.significance(outcomes = chfs.outcome.t4, HHData = HHData, grouping_var = "MPAID")

chfs.att.byMPA <- 
  data.frame(year = c(rep("t2", 4), rep("t4", 4)), 
             rbind.data.frame(chfs.att.t2.byMPA,chfs.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.4.3 Household morbidity att & AI error, by year
morbidity.att.t2.byMPA <- att.significance(outcomes = morbidity.outcome.t2, HHData = HHData, grouping_var = "MPAID")
morbidity.att.t4.byMPA <- att.significance(outcomes = morbidity.outcome.t4, HHData = HHData, grouping_var = "MPAID")

morbidity.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(morbidity.att.t2.byMPA,morbidity.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# ---- 2.5 Education ----

  #-- 2.5.1 Female enrollment att & AI error, by year
f.enrol.att.t2.byMPA <- att.significance(outcomes = f.enrol.outcome.t2, HHData = HHData, grouping_var = "MPAID")
f.enrol.att.t4.byMPA <- att.significance(outcomes = f.enrol.outcome.t4, HHData = HHData, grouping_var = "MPAID")

f.enrol.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(f.enrol.att.t2.byMPA,f.enrol.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)

  #-- 2.5.2 Male enrollment att & AI error, by year
m.enrol.att.t2.byMPA <- att.significance(outcomes = m.enrol.outcome.t2, HHData = HHData, grouping_var = "MPAID")
m.enrol.att.t4.byMPA <- att.significance(outcomes = m.enrol.outcome.t4, HHData = HHData, grouping_var = "MPAID")

m.enrol.att.byMPA <- 
  data.frame(year = c(rep("t2", 6), rep("t4", 6)), 
             rbind.data.frame(m.enrol.att.t2.byMPA,m.enrol.att.t4.byMPA)) %>%
  dplyr::rename(mpa = get.group.)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 3: Calculate q values for all ATTs, across time and space ----
#         3.1 Consolidate p values into single data frame
#         3.2 Check p value histogram
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 3.1 Consolidate p values into single data frame ----

all.pvalues <- 
  rbind.data.frame(cbind.data.frame(Indicator="hfs",
                                    hfs.att.byMPA),
                   cbind.data.frame(Indicator="chfs",
                                    chfs.att.byMPA),
                   cbind.data.frame(Indicator="morbidity",
                                    morbidity.att.byMPA),
                   cbind.data.frame(Indicator="asset",
                                    asset.att.byMPA),
                   cbind.data.frame(Indicator="occ.depend",
                                    OD.att.byMPA),
                   cbind.data.frame(Indicator="econ.status.decline",
                                    econ.decline.att.byMPA),
                   cbind.data.frame(Indicator="econ.status.increase",
                                    econ.increase.att.byMPA),
                   cbind.data.frame(Indicator="tenure",
                                    tenure.att.byMPA),
                   cbind.data.frame(Indicator="marine.group",
                                    MP.att.byMPA),
                   cbind.data.frame(Indicator="other.group",
                                    OP.att.byMPA),
                   cbind.data.frame(Indicator="female.enrol",
                                    f.enrol.att.byMPA),
                   cbind.data.frame(Indicator="male.enrol",
                                    m.enrol.att.byMPA),
                   cbind.data.frame(Indicator="attach",
                                    attach.att.byMPA),
                   cbind.data.frame(Indicator="social.con.decrease",
                                    con.decrease.att.byMPA),
                   cbind.data.frame(Indicator="social.con.increase",
                                    con.increase.att.byMPA))


# ---- 3.2 Check p value histogram ----

hist(all.pvalues$p.val, nclass = 20)


# ---- 3.3 Calculate q values ----

all.qvalues <- qvalue(all.pvalues$p.val)

max(all.qvalues$qvalues[all.qvalues$pvalues<=0.05])

summary(all.qvalues)

plot(all.qvalues)


# ---- Remove clutter ----

rm(hfs.outcome.t2, hfs.outcome.t4, hfs.att.t2.byMPA, hfs.att.t4.byMPA,
   asset.outcome.t2, asset.outcome.t4, asset.att.t2.byMPA, asset.att.t4.byMPA,
   teunre.outcome.t2, tenure.outcome.t4, tenure.att.t2.byMPA, tenure.att.t4.byMPA,
   enrol.outcome.t2, enrol.outcome.t4, enrol.att.t2.byMPA, enrol.att.t4.byMPA,
   attach.outcome.t2, attach.outcome.t4, attach.att.t2.byMPA, attach.att.t4.byMPA)
