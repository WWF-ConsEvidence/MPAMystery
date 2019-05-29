
source(paste0(code.dir,'1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files_from_dropbox.R'))
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")

# --- DiD specification (some bugs in the glm)
DiD.data <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
#  filter(complete.cases(FSIndex)) %>%
  mutate(post.period=recode_factor(as.character(MonitoringYear), "Baseline" = "t0","2 Year Post" = "t2","4 Year Post" = "t4", .ordered = T),
         post.period=C(post.period, treatment),
         MPAID=as.factor(as.character(MPAID)),
         MPAName=gsub(" MPA","",MPAName))
summary(DiD.data)

#Example indicators
DiD.glm.FS <- glm(FSIndex~ Treatment + post.period + post.period:Treatment + 
                  MPAName + MPAName:post.period + InterviewYear +
                  TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
                data=DiD.data)
summary(DiD.glm.FS)

DiD.glm.MT <- glm(MTIndex~ Treatment + post.period + post.period:Treatment + 
                    MPAName + MPAName:post.period + InterviewYear +
                    TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
                  data=DiD.data)
summary(DiD.glm.MT)

#Example MPAs
DiD.glm.FS.Dampier <- glm(FSIndex~ Treatment + post.period + post.period:Treatment + 
                    SettlementID + SettlementID:post.period + InterviewYear +
                    TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
                  data=filter(DiD.data,MPAName=="Selat Dampier"))
summary(DiD.glm.FS.Dampier)

DiD.glm.FS.Kaimana <- glm(FSIndex~ Treatment + post.period + post.period:Treatment + 
                            SettlementID + SettlementID:post.period + InterviewYear +
                            TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
                          data=filter(DiD.data,MPAName=="Kofiau dan Pulau  Boo"))
summary(DiD.glm.FS.Kaimana)


# --- PSM DID
psm.data.t0 <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  tidyr::drop_na(TimeMarket,n.child, ed.level, dom.eth, YearsResident, IndividualGender,IndividualAge) %>%
  mutate(post.period=recode_factor(as.character(MonitoringYear), "Baseline" = "t0","2 Year Post" = "t2","4 Year Post" = "t4", .ordered = T),
         post.period=C(post.period, treatment),
         MPAID=as.factor(as.character(MPAID)),
         MPAName=gsub(" MPA","",MPAName)) %>% 
  filter(MonitoringYear=="Baseline")
summary(psm.data.t0)

pscore.psm.t0 <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
                  data=psm.data.t0)$fitted.values
psm.data <- cbind(psm.data,pscore.psm)



# --- Panel match (doesn't work)
library(devtools)
install_github("insongkim/PanelMatch", dependencies=TRUE)
library(PanelMatch)
names(match.covariate)

tscs <- match.covariate %>% 
  mutate(time.id=as.integer(recode(MonitoringYear, "Baseline" = 1,"2 Year Post" = 2,"4 Year Post" = 3)))

PM.results <- PanelMatch(lag = 2, time.id = "time.id", unit.id = "HouseholdID", 
                         treatment = "Treatment", refinement.method = "ps.match", 
                         data = tscs, match.missing = F, 
                         covs.formula = ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
                         size.match = 5, qoi = "att", ,outcome.var,lead = 0, forbid.treatment.reversal = TRUE)

