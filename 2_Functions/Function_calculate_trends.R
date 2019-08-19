# 
# code:  Define function to calculate status & trends, by MPA
# 
# github: WWF-ConsEvidence/MPAMystery/3_Analysis/1_Social/2_Status_trends
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2019
# 
# 
# ---- inputs ----
#  1) Exported most recent HH_tbl_WELLBEING.xlsx in x_Flat_data_files/Inputs
#  2) Exported most recent HH_tbl_DEMOGRAPHIC.xlsx in x_Flat_data_files/Inputs
#  3) Exported most recent HH_tbl_SETTLEMENT.xlsx in x_Flat_data_files/Inputs
#  4) Exported HH_tbl_MPA.xlsx in x_Flat_data_files/Inputs
# 
# ---- outputs ----
#  1) HHData data frame for all analyses on the BigFive & Middle15 variables on your MPA of choice
#      (used in technical reports and impact summaries)
#  2) IndDemos data frame for all analyses on the BigFive & Middle15 variables on your MPA of choice
#      (used in technical reports and impact summaries)
#  3) Output tables with data and significance test p values for all variables in status & trends reports
# 
# ---- code sections ----
#  1) CALCULATE TRENDS FUNCTION
# 
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: CALCULATE TRENDS FUNCTION ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Define function ----

mpa.trends <- function(MPA=NULL) {
  
  pacman::p_load(rio,reldist,ggplot2,dplyr)
  
  MPA.name <- 
    import('x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_tbl_MPA.xlsx') %>%
    filter(.,MPAID==MPA) %>%
    transmute(MPAName=MPAName,
              MPAName.nospace=gsub(" ","",MPAName),
              MPAName.final=gsub("MPA","",MPAName.nospace))
  
  # source data, clean/post-code data, subset to MPA
  source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_for_function.R', local=T)
  
  # define status year
  status <- as.character(max(HHData$InterviewYear))
  
  # define number of repeat monitoring years
  num.years <- length(unique(HHData$MonitoringYear))
  
  # calculate indicators at household level, settlement level, and MPA level
  source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_BigFive.R', local=T)
  source('3_Analysis/1_Social/2_Status_trends/Sett_MPA_level_means.R', local=T)
  
  # significance tests, based on number of repeat monitoring years
  ifelse(num.years==1, source('3_Analysis/1_Social/2_Status_trends/Status_trends_norepeat_sigtests.R', local=T), 
         ifelse(num.years==2, source('3_Analysis/1_Social/2_Status_trends/Status_trends_onerepeat_sigtests.R', local=T), 
                ifelse(num.years==3, source('3_Analysis/1_Social/2_Status_trends/Status_trends_tworepeat_sigtests.R', local=T), NA)))
  
  # plotting datasets, based on number of repeat monitoring years
  ifelse(num.years==1, source('4_Products/1_Social/1_Status_trends_reports/Status_trends_norepeat_datasets.R', local=T),
         ifelse(num.years==2, source('4_Products/1_Social/1_Status_trends_reports/Status_trends_onerepeat_datasets.R', local=T), 
                ifelse(num.years==3, source('4_Products/1_Social/1_Status_trends_reports/Status_trends_tworepeat_datasets.R', local=T), NA)))
  
  # plots, based on number of repeat monitoring years
  ifelse(num.years==1, source('4_Products/1_Social/1_Status_trends_reports/Status_trends_norepeat_plots.R', local=T), 
         ifelse(num.years==2, source('4_Products/1_Social/1_Status_trends_reports/Status_trends_onerepeat_plots.R', local=T),
                ifelse(num.years==3, source('4_Products/1_Social/1_Status_trends_reports/Status_trends_tworepeat_plots,R', local=T), NA)))
  
  # define output directory
  dir.create(paste(paste("x_Flat_data_files/1_Social/Outputs/Status_trends_analysis", MPA.name$MPAName.final, sep="/"),
                   format(Sys.Date(),format="%Y%m%d"),sep="_"))
  
  OutputFileName <- paste(paste("x_Flat_data_files/1_Social/Outputs/Status_trends_analysis", MPA.name$MPAName.final, sep="/"),
                           format(Sys.Date(),format="%Y%m%d"),sep="_")
  
  # output HHData & IndDemos
  export(HHData, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"HHData.xlsx",sep="_"))
  export(IndDemos, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"IndDemos.xlsx",sep="_"))
  export(Sett.Level.Means, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"Sett.Level.Means.xlsx",sep="_"))
  
}

