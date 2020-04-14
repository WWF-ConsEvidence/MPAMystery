#### load and install packages ####


pacman::p_load(readxl, readr, dplyr, tidyverse)

#### save workbook as individual CSVs ####

path_to_xlsx <- "C:/Users/denardo/OneDrive/Governance_Data_Assessment/Input/MPA_goverance_unified_master_v2.0_11-MAR-2020.xlsx"

sheet_names <- readxl::excel_sheets(path_to_xlsx)

sheets <- purrr::map(sheet_names, ~ readxl::read_excel(path_to_xlsx, sheet = .x))

base::names(sheets) <- sheet_names

purrr::iwalk(sheets, ~ readr::write_excel_csv(x = .x, path = paste0(.y, ".csv")))

folder <- "C:/Users/denardo/OneDrive/Governance_Data_Assessment/Input/"

#### Import data ####

HH <- rio::import("C:/Users/denardo/OneDrive - World Wildlife Fund, Inc/Dropbox(Personal)/ICCB/Governance/Output/HH_tbl_WELLBEING_05-MAR-2020.xlsx")


#--Create list of all .csv files in folder
file_list <- list.files(path=folder, pattern="*.csv") 

#--Read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder, file_list[i], sep='')))}

#### Full availability #### 

FGD.Full.Availability <-
  FGD_MPA.csv  %>%
  group_by(SiteCode, YEAR, SettlementCode)  %>%
  summarise(Unique_Settlement = unique(SettlementCode)) %>%
  select(-c(SettlementCode)) %>%
  rename(MPAID = "SiteCode", InterviewYear = "YEAR") %>%
  rename_all(paste0, "_FGD")
  

KII.Full.Availability <-
  KII_MPA.csv  %>%
  group_by(SiteCode, INTERVIEW.YEAR, SettlementCode)  %>%
  summarise(Unique_Settlement = unique(SettlementCode)) %>%
  select(-c(SettlementCode)) %>%
  rename(MPAID = "SiteCode", InterviewYear = "INTERVIEW.YEAR") %>%
  rename_all(paste0, "_KII")

HH.Full.Availability <-
  HH %>%
  group_by(MPAID, InterviewYear, SettlementID) %>%
  summarise(Unique_Settlement = unique(SettlementID)) %>%
  select(-c(SettlementID)) %>%
  rename_all(paste0, "_HH")

HH.FGD.Full.Join <- 
  left_join(HH.Full.Availability, FGD.Full.Availability, 
          by=c(MPAID_HH = "MPAID_FGD", 
               InterviewYear_HH = "InterviewYear_FGD"))

HH.FGD.KII.Full.Join <- 
  full_join(HH.FGD.Full.Join, KII.Full.Availability,
            by=c(MPAID_HH = "MPAID_KII", 
                 InterviewYear_HH = "InterviewYear_KII"))

HH.FGD.KII.Full.Join.Clean <- HH.FGD.KII.Full.Join %>%
  rename(MPAID = "MPAID_HH",
         InterviewYear = "InterviewYear_HH") %>%
  arrange(MPAID)


#### Settlements by MPA, including count and list of settlement IDs ####

FGD.Settlements.byMPA <-
  FGD_MPA.csv  %>%
  arrange(SettlementCode) %>%
  group_by(SiteCode, YEAR)  %>%
  summarise(Settlement_count = n_distinct(SettlementCode),
            SettlementCodeList = paste0(unique(SettlementCode), collapse = ", ")) %>%
  rename(MPAID = "SiteCode", InterviewYear = "YEAR") %>%
  rename_all(paste0, "_FGD")

KII.Settlements.byMPA <-
  KII_MPA.csv  %>%
  arrange(SettlementCode) %>%
  group_by(SiteCode, INTERVIEW.YEAR)  %>%
  summarise(Settlement_count = n_distinct(SettlementCode),
            SettlementCodeList = paste0(unique(SettlementCode), collapse = ", ")) %>%
  rename(MPAID = "SiteCode", InterviewYear = "INTERVIEW.YEAR") %>%
  rename_all(paste0, "_KII")

HH.Settlements.byMPA <-
  HH %>%
  arrange(SettlementID) %>%
  group_by(MPAID, InterviewYear) %>%
  summarise(Settlement_count = n_distinct(SettlementID),
            SettlementCodeList = paste0(unique(SettlementID), collapse = ", ")) %>%
  rename_all(paste0, "_HH")

HH.FGD.Settlements.byMPA <- 
  full_join(HH.Settlements.byMPA, FGD.Settlements.byMPA, 
            by=c(MPAID_HH = "MPAID_FGD", 
                 InterviewYear_HH = "InterviewYear_FGD"))

HH.FGD.KII.Settlements.byMPA <- 
  full_join(HH.FGD.Settlements.byMPA, KII.Settlements.byMPA,
            by=c(MPAID_HH = "MPAID_KII", 
                 InterviewYear_HH = "InterviewYear_KII"))

HH.FGD.KII.Settlements.byMPA.Clean <- HH.FGD.KII.Settlements.byMPA %>%
  rename(MPAID = "MPAID_HH",
         InterviewYear = "InterviewYear_HH") %>%
  mutate(FGD_KII_Sett_Num_Match = ifelse(Settlement_count_FGD == Settlement_count_KII, "Yes", "No")) %>%
  arrange(MPAID) %>%
  select("MPAID", "InterviewYear", "Settlement_count_HH",  
         "Settlement_count_FGD",  "Settlement_count_KII", 
         "FGD_KII_Sett_Num_Match",
         "SettlementCodeList_HH", "SettlementCodeList_FGD","SettlementCodeList_KII")


write.csv(HH.FGD.KII.Settlements.byMPA.Clean, "Governance_Data_Availability_Check_11-MAR-2020.csv", row.names=FALSE)
