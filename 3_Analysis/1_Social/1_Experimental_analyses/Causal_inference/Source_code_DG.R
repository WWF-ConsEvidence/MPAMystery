# 
# code: Source code for all MPAMystery analyses
# 
# github: WWF-ConsEvidence/MPAMystery
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: October 2017
# modified: 
# 
# 
# ---- code sections ----
#  1) Load libraries
#  2) MPAMystery/2_Social
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Load libraries ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

pacman::p_load(plyr,ggplot2,reshape2,reldist,grid,gridExtra,varhandle,Rmisc,
               RODBC,Matching,optmatch,tidyr,RItools,Hmisc,MBESS,rbounds,Kendall,corrplot,cowplot,dplyr)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: MPAMystery/2_Social ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Required for all BHS analysis ----
library(dplyr)
source('2_Social/SourcedScripts/BHS_MPA_Mystery.R')
# - Dependencies: SQLqueries_AccessODBC.R
# - Inputs:


# ---- 2.2 Required for BHS 2 year impact analysis ----

source('2_Social/SourcedScripts/Matching_2yr_impacts.R')
# - Dependencies: Compute_bigfive_matching.R
#                 Function_export_xlsx.R
#                 Function_variable_outcome.R
# - Inputs: 

source('2_Social/SourcedScripts/BHS_impact_data.R')
# - Dependencies: Matching_2yr_impacts.R
# - Inputs:

source('2_Social/SourcedScripts/Function_summarise_bigfive_impacts.R')
# - Dependencies: none
# - Inputs: none


# ---- 2.3 Required for all SBS analysis ----

source('2_Social/SourcedScripts/SBS_MPA_Mystery.R')
# - Dependencies: 
# - Inputs:


# ---- 2.4 Required for all figure plotting ----

source('2_Social/SourcedScripts/Function_define_asteriskplotting.R')
# - Dependencies: none
# - Inputs: none