# ---
# code: Compute final sample sizes and power post-matching for BHS MPAs
# author: Kelly Claborn, Louise Glew, louise.glew@gmail.com
# created: June 2018
# modified: -

# -----
# inputs: 

# Master matched dataframes (dataframes identifing which households match to which for t2 and t4)
#master_t2_A.xlsx
#master_t4_A.xlsx

# Population data for each MPA
# extracted from original sampling sheets (source:  2010 census by Badan Pusat Statistik)

# -----
# outputs:
# Post-match sample sizes, t2, t4
# Post-match power, t2, t4

# -----
# Code sections
#  1) Call libraries and import raw data
#  2) Compute sample sizes
#  3) Compute power
#  4) Combine into dataframe and export

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- Section 1: Call in libraries, import raw data and source functions ----
# 1.1 libraries
# 1.2 Source functions
# 1.3 Import raw data
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Call libraries ----
pacman::p_load(dplyr, readxl, tidyr)

# ---- 1.2 Source functions ----

# ---- 1.3 Import raw data ----
master.t2 <- read_excel("Dropbox/BHS/t2_impacts/master_t2_panelA.xlsx")
master.t4 <- read_excel("Dropbox/BHS/t4_impacts/master_t4_panelA.xlsx")
MPA_pop <- read_excel("Dropbox/BHS/MPA_population.xlsx")
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- Section 2: Compute post-match sample sizes ----
# 2.1 t2 sample size
# 2.2 t4 sample size
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 t2 sample size ----

  master.t2 <- left_join(master.t2,MPA, by=c("HouseholdID.tr1.t2" = "HouseholdID"))

  post.match.n.t2 <- master.t2 %>%
    group_by(MPAID) %>%
    summarise(mpa.n = n())

# ---- 2.2 t4 sample size ----

  
  master.t4 <- left_join(master.t4,MPA, by=c("HouseholdID.tr1.t4" = "HouseholdID"))
  
  post.match.n.t4 <- master.t4 %>%
    group_by(MPAID) %>%
    summarise(mpa.n = n())
  
  
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- Section 3: Compute post-match power ----
# 3.1 t2 post-match power
# 3.2 t4 post-match power
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 t2 post-match power ----
  
  
# ---- 3.2 t4 post-match power ----


