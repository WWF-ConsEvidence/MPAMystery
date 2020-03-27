# 
# - code: checking master governance data availability against master household database 

# ---- load packages ----

pacman::p_load(rio, openxlsx, dplyr)


# ---- import data ----

HH <- import("x_Flat_data_files/1_Social/Inputs/Master_database_exports/HH_tbl_WELLBEING_20200316.xlsx")
FGD_MPA <- import('x_Flat_data_files/1_Social/Inputs/Governance_database_exports/FGD_MPA_20200325.csv')
KII_MPA <- import('x_Flat_data_files/1_Social/Inputs/Governance_database_exports/KII_MPA_20200325.csv')


 # ---- full data availability, per HH/FGD/KII ----

FGD.Full.Availability <-
  FGD_MPA  %>%
  group_by(SiteCode, YEAR, SettlementCode)  %>%
  summarise(SettlementID = unique(SettlementCode)) %>%
  select(-c(SettlementCode)) %>%
  rename(MPAID = "SiteCode", InterviewYear = "YEAR") %>%
  ungroup() %>%
  mutate(FGD="Yes")
  

KII.Full.Availability <-
  KII_MPA  %>%
  group_by(SiteCode, `INTERVIEW YEAR`, SettlementCode)  %>%
  summarise(SettlementID = as.numeric(unique(SettlementCode))) %>%
  select(-c(SettlementCode)) %>%
  rename(MPAID = "SiteCode", InterviewYear = "INTERVIEW YEAR") %>%
  ungroup() %>%
  mutate(KII="Yes")

HH.Full.Availability <-
  HH %>%
  group_by(MPAID, InterviewYear, SettlementID) %>%
  summarise(SettlementCode=unique(SettlementID)) %>%
  select(-c(SettlementID)) %>%
  ungroup() %>%
  rename(SettlementID = "SettlementCode") %>%
  mutate(HH="Yes")


# ---- join together and compare which settlements/interviewyear combos exist across HH/FGD/KII ----

HH.FGD.KII <-
  full_join(FGD.Full.Availability,KII.Full.Availability,by=c("MPAID","InterviewYear","SettlementID")) %>%
  full_join(HH.Full.Availability,by=c("MPAID","InterviewYear","SettlementID"))
  

HH.FGD.KII.Setts.byMPA <-
  HH.FGD.KII %>%
  group_by(MPAID,InterviewYear) %>%
  summarise(NumSetts.HH=length(HH[!is.na(HH) & HH=="Yes"]),
            NumSetts.FGD=length(FGD[!is.na(FGD) & FGD=="Yes"]),
            NumSetts.KII=length(KII[!is.na(KII) & KII=="Yes"]),
            UniqueSetts.HH=paste0(unique(SettlementID[!is.na(HH) & HH=="Yes"]), collapse=", "),
            UniqueSetts.FGD=paste0(unique(SettlementID[!is.na(FGD) & FGD=="Yes"]), collapse=", "),
            UniqueSetts.KII=paste0(unique(SettlementID[!is.na(KII) & KII=="Yes"]), collapse=", "),
            Setts.HH.noFGD=paste0(unique(SettlementID[is.na(FGD) & HH=="Yes"]), collapse=", "),
            Setts.HH.noKII=paste0(unique(SettlementID[is.na(KII) & HH=="Yes"]), collapse=", "),
            Setts.FGD.noHH=paste0(unique(SettlementID[is.na(HH) & FGD=="Yes"]), collapse=", "),
            Setts.FGD.noKII=paste0(unique(SettlementID[is.na(KII) & HH=="Yes"]), collapse=", "),
            Setts.KII.noHH=paste0(unique(SettlementID[is.na(HH) & KII=="Yes"]), collapse=", "),
            Setts.KII.noFGD=paste0(unique(SettlementID[is.na(FGD) & KII=="Yes"]), collapse=", "))


# ---- write data to excel & export ----

wb <- createWorkbook("Governance_Data_Check_20200325") 
addWorksheet(wb,"HH_FGD_KII_Settlements")
addWorksheet(wb,"HH_FGD_KII_byMPA")

writeData(wb,"HH_FGD_KII_Settlements",HH.FGD.KII)
writeData(wb,"HH_FGD_KII_byMPA",HH.FGD.KII.Setts.byMPA)

saveWorkbook(wb,'x_Flat_data_Files/1_Social/Outputs/Governance_Data_Check_20200325.xlsx')

