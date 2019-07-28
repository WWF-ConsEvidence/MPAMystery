#.libPaths()
#.libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")
library(lfe)
library(Matrix)
library(stargazer)

##Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 

source('R:/Gill/DLe/MPAMystery/2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")


# --- DiD specification 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  filter(MPAID<=6) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear)) %>% 
  filter(!is.na(IndividualGender))

#summary(DiD.data)

pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  

###export to DiD.data and other raw source data frames to Stata
#write.dta(DiD.data, "D:/Dropbox/Indonesia MPA/data/MPA_06192019.dta")
#write.dta(HHData, "D:/Dropbox/Indonesia MPA/data/HHData.dta")
#write.dta(IndDemos, "D:/Dropbox/Indonesia MPA/data/IndDemos.dta")
#write.dta(education.lkp1, "D:/Dropbox/Indonesia MPA/data/Educ_lookup.dta")
#write.dta(ethnic.lkp1, "D:/Dropbox/Indonesia MPA/data/Ethnic_lookup.dta")



###############
###############using lfe (felm) for high dimensional FE DiD (similar to reghdfe)

###1. 
DiD.felm.FS <- felm(FSIndex ~ Treatment + yearsPostF + Treatment:yearsPostF + TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge
                                | InterviewYear + MPAID:InterviewYear| 0 | SettlementID, 
                    data=DiD.data,exactDOF = TRUE)

summary(DiD.felm.FS)
stargazer(DiD.felm.FS, out = "R:/Gill/DLe/MPAMystery/4_Products/1_Social/DiD.felm.FSaaaaaaa.html")








#---------------------------------Individual MPAs' ATE

# Food security
DiD.glm.FS <- glm(FSIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.FS)

# Material Assets
DiD.glm.MA <- glm(MAIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.MA)

# School Education
DiD.glm.SE <- glm(SERate~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.SE)

# Marine Tenure
DiD.glm.MT <- glm(MTIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.MT)

# Place Attachment
DiD.glm.PA <- glm(PAIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.PA)
stargazer(DiD.glm.FS,DiD.glm.MA,DiD.glm.SE,DiD.glm.MT,DiD.glm.PA, out = "R:/Gill/DLe/MPAMystery/4_Products/1_Social/DiD.glm-MPA.html")





# ----------------------Produce plots for 5 indexes
####food security
DiD.glm.FS.broom <- tidy(DiD.glm.FS) %>% 
  filter(grepl('TreatFactor', term)) %>% 
  mutate(term=gsub("TreatFactor","MPA ",term),
         term=gsub("yearsPostF","yr",term))

pd <- position_dodge(width=.3) # move them .05 to the left and right

ggplot(DiD.glm.FS.broom,aes(x=term,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.2, position = pd ) +
  labs(x="parameter",y="estimate",title="Food security")


ggsave(paste0(outputdir,"foodsecurity.jpg"),width = 12, height = 6)



####assets
DiD.glm.MA.broom <- tidy(DiD.glm.MA) %>% 
  filter(grepl('TreatFactor', term)) %>% 
  mutate(term=gsub("TreatFactor","MPA ",term),
         term=gsub("yearsPostF","yr",term))

pd <- position_dodge(width=.3) # move them .05 to the left and right

ggplot(DiD.glm.MA.broom,aes(x=term,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.2, position = pd ) +
  labs(x="parameter",y="estimate",title="Material Assets")


ggsave(paste0(outputdir,"assets.jpg"),width = 12, height = 6)





####school education
DiD.glm.SE.broom <- tidy(DiD.glm.SE) %>% 
  filter(grepl('TreatFactor', term)) %>% 
  mutate(term=gsub("TreatFactor","MPA ",term),
         term=gsub("yearsPostF","yr",term))

pd <- position_dodge(width=.3) # move them .05 to the left and right

ggplot(DiD.glm.SE.broom,aes(x=term,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.2, position = pd ) +
  labs(x="parameter",y="estimate",title="School Education")


ggsave(paste0(outputdir,"schooleduc.jpg"),width = 12, height = 6)




####Marine Tenure
DiD.glm.MT.broom <- tidy(DiD.glm.MT) %>% 
  filter(grepl('TreatFactor', term)) %>% 
  mutate(term=gsub("TreatFactor","MPA ",term),
         term=gsub("yearsPostF","yr",term))

pd <- position_dodge(width=.3) # move them .05 to the left and right

ggplot(DiD.glm.MT.broom,aes(x=term,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.2, position = pd ) +
  labs(x="parameter",y="estimate",title="Marine Tenure")


ggsave(paste0(outputdir,"marinetenure.jpg"),width = 12, height = 6)




####Place Attachment
DiD.glm.PA.broom <- tidy(DiD.glm.PA) %>% 
  filter(grepl('TreatFactor', term)) %>% 
  mutate(term=gsub("TreatFactor","MPA ",term),
         term=gsub("yearsPostF","yr",term))

pd <- position_dodge(width=.3) # move them .05 to the left and right

ggplot(DiD.glm.PA.broom,aes(x=term,y=estimate)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=0.2, position = pd ) +
  labs(x="parameter",y="estimate",title="Place Attachment")


ggsave(paste0(outputdir,"placeattachment.jpg"),width = 12, height = 6)



#-----------------------------------------------------
#---------------------------------------------------
#---------------------------------Entire Seascape ATE
# Food security
DiD.glm.FS <- glm(FSIndex~ Treatment + yearsPostF + Treatment:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.FS)

# Material Assets
DiD.glm.MA <- glm(MAIndex~ Treatment + yearsPostF + Treatment:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.MA)

# School Education
DiD.glm.SE <- glm(SERate~ Treatment + yearsPostF + Treatment:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.SE)

# Marine Tenure
DiD.glm.MT <- glm(MTIndex~ Treatment + yearsPostF + Treatment:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.MT)

# Place Attachment
DiD.glm.PA <- glm(PAIndex~ Treatment + yearsPostF + Treatment:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.PA)
stargazer(DiD.glm.FS,DiD.glm.MA,DiD.glm.SE,DiD.glm.MT,DiD.glm.PA, out = "R:/Gill/DLe/MPAMystery/4_Products/1_Social/DiD.glm-Seascape.html")

#------- Linear mixed effects models
# glm model
DiD.glm.FS <- glm(FSIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                    factor(SettlementID) + MPAID + InterviewYear + MPAID:InterviewYear,
                  data=DiD.data)
summary(DiD.glm.FS)

# linear mixed effects model (random factor for settlement)
lmefit.sett.FS <- lmer(FSIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                  MPAID + InterviewYear + MPAID:InterviewYear + (1|SettlementID), 
                           data=DiD.data, REML=F) # random intercept
summary(lmefit.sett.FS)


# Fit linear mixed effects model with settlement nested in interview year
lmefit.sett.FS2 <- lmer(FSIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                  MPAID + MPAID:InterviewYear + (1|InterviewYear/SettlementID), 
                data=DiD.data, REML=F) # random intercept
summary(lmefit.sett.FS2)

# Compete LMEFIT1 against LMEFIT3 with likelihood ratio test 
anova(DiD.glm.FS, lmefit.sett.FS)

# Compare best model to simple linear model (seems like family makes no difference!)
nlmefit <- lme(FSIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                 MPAID + InterviewYear + MPAID:InterviewYear,
               random=~1|SettlementID,
               na.action=na.omit,data=DiD.data,method="REML") # same as lmefit1
glsfit <- gls(FSIndex~ TreatFactor + yearsPostF + TreatFactor:yearsPostF + pscore + 
                MPAID + InterviewYear + MPAID:InterviewYear,
              na.action=na.omit,data=DiD.data, method = "REML")
anova(glsfit, nlmefit)
summary(nlmefit)


