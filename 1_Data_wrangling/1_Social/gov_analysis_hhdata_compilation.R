source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_tbl_MPA.xlsx")
pacman::p_load(rio,fastDummies, tidyverse)
mean2=function(x){mean(x,na.rm=TRUE)}
today.date <- gsub("-","",Sys.Date())
output.dir <- "C:/Users/david/Dropbox/Governance analysis/tables/"




# splitting livelihood data
names(HHData)
pri.livl <- dummy_cols(HHData$PrimaryLivelihood)
summary(pri.livl)
pri.livl <- select(pri.livl,.data_1:.data_7)
names(pri.livl) <- c("pri.farming","pri.harv.forest","pri.fishing","pri.aquculture","pri.res.extraction","pri.tourism","pri.other.wage")

sec.livl <- dummy_cols(HHData$SecondaryLivelihood)
summary(sec.livl)
sec.livl <- select(sec.livl,.data_1:.data_7)
names(sec.livl) <- c("sec.farming","sec.harv.forest","sec.fishing","sec.aquculture","sec.res.extraction","sec.tourism","sec.other.wage")

ter.livl <- dummy_cols(HHData$TertiaryLivelihood)
summary(ter.livl)
ter.livl <- select(ter.livl,.data_1:.data_7)
names(ter.livl) <- c("ter.farming","ter.harv.forest","ter.fishing","ter.aquculture","ter.res.extraction","ter.tourism","ter.other.wage")

# compile all wanted variables
hh.subset <- HHData %>% 
  select(SettlementID,InterviewYear,MarineGroup,OtherGroup,FreqEatFish,FreqSaleFish,PercentIncFish,
         PAIndex,TimeMarket,MAIndex,EconStatusTrend,FSIndex,MTIndex,
         RightsAccess, RightsHarvest, RightsManage, RightsExclude,RightsTransfer) %>% 
  bind_cols(pri.livl,sec.livl,ter.livl)

# average to settlement level
sett.avg <- hh.subset %>% 
  group_by(SettlementID,InterviewYear) %>% 
  summarise_all(mean2)
  
summary(sett.avg)  

# join to settlement data
sett.out <- Settlements %>% 
  select(-Zone) %>% 
  left_join(select(mpa.nam,MPACode,MPAName),by=c("MPAID"="MPACode")) %>% 
  left_join(sett.avg,by="SettlementID")
summary(sett.out)
export(sett.out,paste0(output.dir,today.date,"_sett_avg_hh_data.csv"))
