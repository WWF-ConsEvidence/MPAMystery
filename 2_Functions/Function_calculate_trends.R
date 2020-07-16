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
  
  pacman::p_load(rio, reldist, Kendall, reshape2, ggplot2, grid, gridExtra, dplyr)
  
  
  # source data, clean/post-code data, subset to MPA
  source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_for_function.R', local=T)
  
  # define status year
  status <- as.numeric(as.character(max(HHData$InterviewYear)))
  
  # define number of repeat monitoring years
  num.years <- length(unique(HHData$MonitoringYear))
  
  # source plotting/labeling/reference functions first
  source('2_Functions/3_Plotting/Function_plotthemes.R', local=T)
  source('2_Functions/3_Plotting/Function_define_asteriskplotting.R', local=T)
  
  
  # ---- ANALYSIS ----
  
  # calculate indicators at household level, settlement level, and MPA level
  source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_household_indices.R', local=T)
  source('3_Analysis/1_Social/2_Status_trends/Sett_MPA_level_means.R', local=T)
  ifelse(MPA==21, source('3_Analysis/1_Social/2_Status_trends/Sett_MPA_level_means_byzone.R', local=T), NA) # Wakatobi doesn't have control settlements, but can be broken down by zone, so has additional analyses by zone
  
  # significance tests, based on number of repeat monitoring years
  ifelse(num.years==1, source('3_Analysis/1_Social/2_Status_trends/Significance_tests/Status_trends_norepeat_sigtests.R', local=T), 
         ifelse(num.years==2, source('3_Analysis/1_Social/2_Status_trends/Significance_tests/Status_trends_onerepeat_sigtests.R', local=T), 
                ifelse(num.years==3, source('3_Analysis/1_Social/2_Status_trends/Significance_tests/Status_trends_tworepeat_sigtests.R', local=T), 
                       ifelse(num.years==4, source('3_Analysis/1_Social/2_Status_trends/Significance_tests/Status_trends_threerepeat_sigtests.R', local=T),
                              ifelse(num.years==5, source('3_Analysis/1_Social/2_Status_trends/Significance_tests/Status_trends_fourrepeat_sigtests.R', local=T), NA)))))
  
  
  # ---- SOURCE DATASETS AND PLOTTING ----
  
  # plotting datasets, based on number of repeat monitoring years
  ifelse(num.years==1, source('4_Products/1_Social/1_Status_trends_reports/Datasets/Status_trends_norepeat_datasets.R', local=T),
         ifelse(num.years>1, source('4_Products/1_Social/1_Status_trends_reports/Datasets/Status_trends_multirepeat_datasets.R', local=T), NA))
  
  # source plot scripts (English language plots)
  ifelse(num.years==1 & MPA==21, source('4_Products/1_Social/1_Status_trends_reports/Plots/Status_trends_norepeat_plots_byzone.R', local=T),
         ifelse(num.years==1 & MPA!=21,source('4_Products/1_Social/1_Status_trends_reports/Plots/Status_trends_norepeat_plots.R', local=T), 
                ifelse(num.years>1 & MPA!=21, source('4_Products/1_Social/1_Status_trends_reports/Plots/Status_trends_multirepeat_plots.R', local=T), NA)))
  
  # source plot scripts (Bahasa language plots)
  ifelse(num.years==1 & MPA==21, source('4_Products/1_Social/1_Status_trends_reports/Plots/Status_trends_norepeat_plots_byzone_bahasa.R', local=T),
         ifelse(num.years==1 & MPA!=21,source('4_Products/1_Social/1_Status_trends_reports/Plots/Status_trends_norepeat_plots_bahasa.R', local=T),
                ifelse(num.years>1 & MPA!=21, source('4_Products/1_Social/1_Status_trends_reports/Plots/Status_trends_multirepeat_plots_bahasa.R', local=T), NA)))
  
  
  # ---- EXPORT ----
  
  # define output directory
  dir.create(paste(paste("x_Flat_data_files/1_Social/Outputs/Status_trends_analysis", MPA.name$MPAName.final, sep="/"),
                   format(Sys.Date(),format="%Y%m%d"),sep="_"))
  
  OutputFileName <- paste(paste("x_Flat_data_files/1_Social/Outputs/Status_trends_analysis", MPA.name$MPAName.final, sep="/"),
                           format(Sys.Date(),format="%Y%m%d"),sep="_")
  
  # output HHData, IndDemos, local threats & steps
  export(HHData, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"HHData.xlsx",sep="_"))
  export(IndDemos, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"IndDemos.xlsx",sep="_"))
  export(Sett.Level.Means, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"Sett_Level_Means.xlsx",sep="_"))
  export(LThreat_forexport, paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"LTHREAT.xlsx",sep="_"))
  
  # output plot-formatted datasets, with pvalues
  ifelse(num.years==1,
         export(list(Continuous_status=Sett.level.ContData.status.PLOTFORMAT,
                     Proportional_status=Sett.level.PropData.status.PLOTFORMAT),
                paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"Sett_Level_Data_forplotting.xlsx",sep="_")),
         export(list(Continuous_status=Sett.level.ContData.status.PLOTFORMAT,
                     Proportional_status=Sett.level.PropData.status.PLOTFORMAT,
                     Continuous_trend=Sett.level.ContData.annex.PLOTFORMAT,
                     Continuous_trend_pvalues=annex.sigvals), 
                paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"Sett_Level_Data_forplotting.xlsx",sep="_")))
  
  if(num.years>1){
         export(list(Continuous_trend=MPA.level.ContData.trend.PLOTFORMAT,
              Proportional_trend=MPA.level.PropData.trend.PLOTFORMAT,
              Proportional_trend_pvalues=propdata.trend.test),
         paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"MPA_Level_Data_forplotting.xlsx",sep="_"))
  }
  
  if(MPA.name$MPAID==21){
    export(Detailed.FishTechnique,
           paste(paste(OutputFileName,MPA.name$MPAName.final,sep="/"),"Detailed_Fishing_Technique_data.xlsx",sep="_"))
  }
  
  # output plots
  ifelse(num.years==1, source('4_Products/1_Social/1_Status_trends_reports/Export_status_plots.R', local=T),
         source('4_Products/1_Social/1_Status_trends_reports/Export_status_trends_plots.R', local=T))
  
}

