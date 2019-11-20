# ---- separate out threats and actions in Koon 2018 & Kei 2019 social monitoring ---- 

pacman::p_load(rio, reshape2, tidyr, dplyr)


# - import Koon post-QAQC
# -- note: For Koon, separate threats and steps identified by same household are separated by a colon in a single cell
Koon_LTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_LTHREATS")

Koon_LSTEPS <-
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_LSTEPS")
Koon_GTHREAT <-
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_GTHREATS")

Koon_GSTEPS <-
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_GSTEPS")

# - import Kei post-QAQC
# -- note: For Kei, separate threats and steps identified by same household are separated by a comma in a single cell
Kei_LTHREAT <- 
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_LTHREATS")

Kei_LSTEPS <-
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_LSTEPS")
Kei_GTHREAT <-
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_GTHREATS")

Kei_GSTEPS <-
  import('C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_post-QAQC_2019_1111.xlsx',
         sheet="HH_tbl_GSTEPS")

# Koon fixes
Koon_LTHREAT <- 
  Koon_LTHREAT %>%
  separate(col=LocalMarineThreat,into=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),sep=":") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),value.name="LocalMarineThreat") %>%
  na.omit(cols="LocalMarineThreat") %>%
  .[order(.$HouseholdID),] %>%
  mutate(LThreatID=seq(1:length(HouseholdID))) %>%
  select(-variable)

Koon_LSTEPS <- 
  Koon_LSTEPS %>%
  separate(col=LocalSteps,into=c("Step1","Step2","Step3","Step4","Step5","Step6"),sep=":") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Step1","Step2","Step3","Step4","Step5","Step6"),value.name="LocalSteps") %>%
  na.omit(cols="LocalSteps") %>%
  .[order(.$HouseholdID),] %>%
  mutate(LocalStepsID=seq(1:length(HouseholdID))) %>%
  select(-variable)

Koon_GTHREAT <- 
  Koon_GTHREAT %>%
  separate(col=GLobalMarineThreat,into=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),sep=":") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),value.name="GLobalMarineThreat") %>%
  na.omit(cols="GLobalMarineThreat") %>%
  .[order(.$HouseholdID),] %>%
  mutate(GlobalThreatID=seq(1:length(HouseholdID))) %>%
  select(-variable)

Koon_GSTEPS <- 
  Koon_GSTEPS %>%
  separate(col=GLobalMarineSteps,into=c("Step1","Step2","Step3","Step4","Step5","Step6"),sep=":") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Step1","Step2","Step3","Step4","Step5","Step6"),value.name="GLobalMarineSteps") %>%
  na.omit(cols="GLobalMarineSteps") %>%
  .[order(.$HouseholdID),] %>%
  mutate(GlobalStepsID=seq(1:length(HouseholdID))) %>%
  select(-variable)


# Kei fixes
Kei_LTHREAT <- 
  Kei_LTHREAT %>%
  separate(col=LocalMarineThreat,into=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),sep=",") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),value.name="LocalMarineThreat") %>%
  na.omit(cols="LocalMarineThreat") %>%
  .[order(.$HouseholdID),] %>%
  mutate(LThreatID=seq(1:length(HouseholdID))) %>%
  select(-variable)

Kei_LSTEPS <- 
  Kei_LSTEPS %>%
  separate(col=LocalSteps,into=c("Step1","Step2","Step3","Step4","Step5","Step6"),sep=",") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Step1","Step2","Step3","Step4","Step5","Step6"),value.name="LocalSteps") %>%
  na.omit(cols="LocalSteps") %>%
  .[order(.$HouseholdID),] %>%
  mutate(LocalStepsID=seq(1:length(HouseholdID))) %>%
  select(-variable)

Kei_GTHREAT <- 
  Kei_GTHREAT %>%
  separate(col=GLobalMarineThreat,into=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),sep=",") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Threat1","Threat2","Threat3","Threat4","Threat5","Threat6"),value.name="GLobalMarineThreat") %>%
  na.omit(cols="GLobalMarineThreat") %>%
  .[order(.$HouseholdID),] %>%
  mutate(GlobalThreatID=seq(1:length(HouseholdID))) %>%
  select(-variable)

Kei_GSTEPS <- 
  Kei_GSTEPS %>%
  separate(col=GLobalMarineSteps,into=c("Step1","Step2","Step3","Step4","Step5","Step6"),sep=",") %>%
  melt(id.vars="HouseholdID",measure.vars=c("Step1","Step2","Step3","Step4","Step5","Step6"),value.name="GLobalMarineSteps") %>%
  na.omit(cols="GLobalMarineSteps") %>%
  .[order(.$HouseholdID),] %>%
  mutate(GlobalStepsID=seq(1:length(HouseholdID))) %>%
  select(-variable)


# ---- export ----

export(Koon_LTHREAT,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_LTHREAT")
export(Koon_LSTEPS,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_LSTEPS")
export(Koon_GTHREAT,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_GTHREAT")
export(Koon_GSTEPS,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2018_KOON/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KOON_2018_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_GSTEPS")

export(Kei_LTHREAT,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_LTHREAT")
export(Kei_LSTEPS,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_LSTEPS")
export(Kei_GTHREAT,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_GTHREAT")
export(Kei_GSTEPS,'C://Users/claborn-intern/Dropbox (MPAMystery)/MPA_social_data/2_QUALITY_CONTROL/2019_KEI_KECIL/3_QAQC/1_HWB/2_Post-QAQC_FlatFiles_2019_1111/KEI_2019_THREAT_STEPS_2019_1111.xlsx',
       which="HH_tbl_GSTEPS")
