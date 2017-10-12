# 
# code: Source code for all WWF-conservation-evidence analyses
# 
# github: kaclaborn/WWF-conservation-evidence
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: October 2017
# modified: 
# 
# 
# ---- code sections ----
#  1) Load libraries
#  2) MPAMystery/Social
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Load libraries ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

library(plyr)
library(dplyr)
library(ggplot2)
library(varhandle)
library(xlsx)
library(RODBC)
library(Matching)
library(optmatch)
library(tidyr)
library(RItools)
library(Hmisc)
library(MBESS)
library(rbounds)
library(Kendall)
library(gridExtra)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: MPAMystery/Social ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Required for all BHS analysis ----

source('MPAMystery/Social/SourcedScripts/BHS_MPA_Mystery.R')
# - Dependencies: SQLqueries_AccessODBC.R
# - Inputs:


# ---- 2.2 Required for BHS 2 year impact analysis ----

source('MPAMystery/Social/SourcedScripts/Matching_2yr_impacts.R')
# - Dependencies: Compute_bigfive_matching.R
#                 Function_export_xlsx.R
#                 Function_variable_outcome.R
# - Inputs: 

source('MPAMystery/Social/SourcedScripts/BHS_2yr_impact_data.R')
# - Dependencies: Matching_2yr_impacts.R
# - Inputs:

source('MPAMystery/Social/SourcedScripts/Function_summarise_bigfive_impactdata.R')
# - Dependencies: none
# - Inputs: none
