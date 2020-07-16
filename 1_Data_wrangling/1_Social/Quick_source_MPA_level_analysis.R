# 
# code:  Quick-source all relevant scripts if running piecemeal status/trends analyses at the MPA level
# 
# github: WWF-ConsEvidence/MPAMystery/1_Data_wrangling/1_Social/
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2020
# modified:
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & SOURCE SCRIPTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# The purpose of this script is to source all relevant scripts if you are planning to run any of the individual status or trends 
# scripts without running the entire wrapper function defined in '2_Functions/Function_calculate_trends.R'

# Prior to running this script, you must still identify which specific MPA you would like to run the analysis for


# ---- 1.1 Load libraries ----

pacman::p_load(rio, reldist, Kendall, reshape2, ggplot2, grid, gridExtra, dplyr)


# ---- 1.2 Define MPA for analysis

# DEFINE MPA HERE BY MPAID
MPA <- 999



# ---- 1.3 Source scripts for data sourcing, calculating indicators, aggregating to settlement and MPA level ----

# source data, clean/post-code data, subset to MPA
source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_for_function.R', local=T)

# define status year
status <- as.numeric(as.character(max(HHData$InterviewYear)))

# define number of repeat monitoring years
num.years <- length(unique(HHData$MonitoringYear))

# calculate indicators at household level, settlement level, and MPA level
source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_household_indices.R', local=T)
source('3_Analysis/1_Social/2_Status_trends/Sett_MPA_level_means.R', local=T)
ifelse(MPA==21, source('3_Analysis/1_Social/2_Status_trends/Sett_MPA_level_means_byzone.R', local=T), NA) # Wakatobi doesn't have control settlements, but can be broken down by zone, so has additional analyses by zone

# source plotting functions 
source('2_Functions/3_Plotting/Function_plotthemes.R', local=T)
source('2_Functions/3_Plotting/Function_define_asteriskplotting.R', local=T)
