# ---
# code: Localized average treatment effects of BHS MPAs
# author: Louise Glew, louise.glew@gmail.com
# created: May 2018
# modified: 

# -----
# inputs: 
# HH.data - data on MPA outcomes, plus identifying variables
# Matched pairs dataframe
# MPA outcomes data for BigFive at each time period
# Match covariate data
# BigFive data (to extract household assets at baseline)

# -----
# outputs:


# -----

# -----
# Code sections
#  1) Call libraries and import raw data

# =================
#
# ---- SECTION 1: Call libraries, functions and import raw data----

# =================

# 1.1 Call in libraries
# 1.2 Source functions
# 1.3 Import data

# ---
# 1.1 Call in libraries
pacman::p_load(dplyr, lme4,multcomp)

# ---
# 1.2 Source functions
#

# ---
# 1.3 Import data

#match.covariate
#MPA outcomes
#BigFive
#matched pairs dataframe 

# =================
#
# ---- SECTION 2: Create sub-groups ----

# =================
# 2.1 Wealth quintiles
# 2.2 Gender of household head
# 2.3 Fisher vs. non-fisher

# ---
# 2.1 Wealth quintiles

asset.quintile <- BigFive %>%
  dplyr:: filter (MonitoringYear=="Baseline") %>%
  dplyr:: select (HouseholdID,MAIndex, SettlementID, MPAID)%>%
  dplyr:: mutate (asset.quintile =as.factor(ntile(MAIndex,5))) %>%
  dplyr:: select (HouseholdID, asset.quintile, SettlementID, MPAID) #pulling MPAID & settlementID here, for later hierarchical model
  
# ---
# 2.2 Gender of household head

gender <- match.covariate %>%    # add gender to BigFive output then, remove match.covariate from this script
  dplyr:: filter (MonitoringYear=="t0") %>%
  dplyr:: select (HouseholdID,IndividualGender)

# ---
# 2.3 Fisher vs. non-fisher

fisher <- HHData %>%
    dplyr:: select (HouseholdID,PrimaryLivelihoodClean,SecondaryLivelihoodClean,TertiaryLivelihoodClean) %>%
    dplyr:: mutate (
                  fisher.binary = ifelse ((PrimaryLivelihoodClean == 3 |SecondaryLivelihoodClean == 3 |TertiaryLivelihoodClean == 3),1,0),
                  occ.dependence = ifelse(PrimaryLivelihoodClean == 3,2,(ifelse ((SecondaryLivelihoodClean == 3 |TertiaryLivelihoodClean == 3),2,0)))
                  ) %>%
    dplyr::select(HouseholdID, fisher.binary, occ.dependence)
                
# =================
#
# ---- SECTION 3: Join sub-groups to master dataframe at baseline ----

# =================
# 3.1 t2
# 3.2 t4

## Subgroups are computed using baseline treatment household information, but then passed to outcomes dataframe that run on repeat treatment IDs. ##This is not an error.

# ---
# 3.1 t2
master.t2.subgroups <- left_join (master.t2.A,asset.quintile, by=c("HouseholdID.tr1.t0" ="HouseholdID"))%>%
  left_join(., fisher, by=c("HouseholdID.tr1.t0" ="HouseholdID"))%>%
  left_join(., gender, by=c("HouseholdID.tr1.t0" ="HouseholdID"))

master.t2.subgroups <- master.t2.subgroups %>%
  dplyr::mutate_at(.vars=c("IndividualGender","fisher.binary","occ.dependence","asset.quintile"),
                   .funs= funs(factor(.,))) %>%
  dplyr:: select (HouseholdID.tr1.t2,fisher.binary, occ.dependence,asset.quintile,IndividualGender, SettlementID, MPAID)

# ---
# 3.2 t4
master.t4.subgroups <- left_join (master.t4.A,asset.quintile, by=c("HouseholdID.tr1.t0" ="HouseholdID"))%>%
  left_join(., fisher, by=c("HouseholdID.tr1.t0" ="HouseholdID"))%>%
  left_join(., gender, by=c("HouseholdID.tr1.t0" ="HouseholdID"))

master.t4.subgroups <- master.t4.subgroups %>%
    dplyr:: mutate_at(.vars=c("IndividualGender","fisher.binary","occ.dependence","asset.quintile"),
                   .funs= funs(factor(.,))) %>%
    dplyr:: select (HouseholdID.tr1.t4,fisher.binary, occ.dependence,asset.quintile,IndividualGender,SettlementID, MPAID)
  

# =================

# ---- SECTION 4: Join sub-groups to BigFive outcomes  ----

# =================

# 4.1 t2 Big Five
# 4.2 t4 Big Five


# ---
# 4.1 t2 Big Five

hfs.outcome.t2.subgroup <- na.omit(left_join(hfs.outcome.t2, master.t2.subgroups, by=c("tr1tx"="HouseholdID.tr1.t2")))
asset.outcome.t2.subgroup <- left_join(asset.outcome.t2, master.t2.subgroups, by=c("tr1tx"="HouseholdID.tr1.t2"))
tenure.outcome.t2.subgroup <- left_join(tenure.outcome.t2, master.t2.subgroups, by=c("tr1tx"="HouseholdID.tr1.t2"))
enrol.outcome.t2.subgroup <- left_join(enrol.outcome.t2, master.t2.subgroups, by=c("tr1tx"="HouseholdID.tr1.t2"))
attach.outcome.t2.subgroup <- left_join(attach.outcome.t2, master.t2.subgroups, by=c("tr1tx"="HouseholdID.tr1.t2"))

# ---
# 4.2 t4 Big Five

hfs.outcome.t4.subgroup <- left_join(hfs.outcome.t4, master.t4.subgroups, by=c("tr1tx"="HouseholdID.tr1.t4"))
asset.outcome.t4.subgroup <- left_join(asset.outcome.t4, master.t4.subgroups, by=c("tr1tx"="HouseholdID.tr1.t4"))
tenure.outcome.t4.subgroup <- left_join(tenure.outcome.t4, master.t4.subgroups, by=c("tr1tx"="HouseholdID.tr1.t4"))
enrol.outcome.t4.subgroup <- left_join(enrol.outcome.t4, master.t4.subgroups, by=c("tr1tx"="HouseholdID.tr1.t4"))
attach.outcome.t4.subgroup <- left_join(attach.outcome.t4, master.t4.subgroups, by=c("tr1tx"="HouseholdID.tr1.t4"))

# =================

# ---- SECTION 5: t2 Localized average treatment effects ----

# =================

# 5.1 Food security
# 5.2 Household material assets
# 5.3 Household marine tenure
# 5.4 School enrollment
# 5.5 Place attachment

# ---
# 5.1 Food security

hfoodsec.subgroup.t2.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=hfs.outcome.t2.subgroup)
summary((glht(hfoodsec.subgroup.t2.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(hfoodsec.subgroup.t2.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(hfoodsec.subgroup.t2.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(hfoodsec.subgroup.t2.ATT), residuals(hfoodsec.subgroup.t2.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(hfoodsec.subgroup.t2.ATT), residuals(hfoodsec.subgroup.t2.ATT)))

hfs.quintiles.t2 <- hfs.outcome.t2.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
              se = sd(ATT)/sqrt(length(ATT)))


# ---
# 5.2 Household material assets

asset.subgroup.t2.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=asset.outcome.t2.subgroup)
summary((glht(asset.subgroup.t2.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(asset.subgroup.t2.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(asset.subgroup.t2.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(asset.subgroup.t2.ATT), residuals(asset.subgroup.t2.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(asset.subgroup.t2.ATT), residuals(asset.subgroup.t2.ATT)))

asset.quintiles.t2 <- asset.outcome.t2.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(asset.quintiles.t2$mean)

# ---
# 5.3 Household marine tenure

tenure.subgroup.t2.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=tenure.outcome.t2.subgroup)
summary((glht(tenure.subgroup.t2.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(tenure.subgroup.t2.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(tenure.subgroup.t2.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(tenure.subgroup.t2.ATT), residuals(tenure.subgroup.t2.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(tenure.subgroup.t2.ATT), residuals(tenure.subgroup.t2.ATT)))

tenure.quintiles.t2 <- tenure.outcome.t2.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(tenure.quintiles.t2$mean)

# ---
# 5.4 School enrollment

enrol.subgroup.t2.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=enrol.outcome.t2.subgroup)
summary((glht(enrol.subgroup.t2.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(enrol.subgroup.t2.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(enrol.subgroup.t2.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(enrol.subgroup.t2.ATT), residuals(enrol.subgroup.t2.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(enrol.subgroup.t2.ATT), residuals(enrol.subgroup.t2.ATT)))

enrol.quintiles.t2 <- enrol.outcome.t2.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(enrol.quintiles.t2$mean)

# ---
# 5.5 Place attachment
attach.subgroup.t2.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=attach.outcome.t2.subgroup)
summary((glht(attach.subgroup.t2.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(attach.subgroup.t2.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(attach.subgroup.t2.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(attach.subgroup.t2.ATT), residuals(attach.subgroup.t2.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(attach.subgroup.t2.ATT), residuals(attach.subgroup.t2.ATT)))

attach.quintiles.t2 <- attach.outcome.t2.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(attach.quintiles.t2$mean)


# =================

# ---- SECTION 5: t4 Localized average treatment effects ----

# =================

# 5.1 Food security
# 5.2 Household material assets
# 5.3 Household marine tenure
# 5.4 School enrollment
# 5.5 Place attachment

# ---
# 5.1 Food security

hfoodsec.subgroup.t4.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=hfs.outcome.t4.subgroup)
summary((glht(hfoodsec.subgroup.t4.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(hfoodsec.subgroup.t4.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(hfoodsec.subgroup.t4.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(hfoodsec.subgroup.t4.ATT), residuals(hfoodsec.subgroup.t4.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(hfoodsec.subgroup.t4.ATT), residuals(hfoodsec.subgroup.t4.ATT)))

hfs.quintiles.t4 <- hfs.outcome.t4.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

# ---
# 5.2 Household material assets
asset.subgroup.t4.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=asset.outcome.t4.subgroup)
summary((glht(asset.subgroup.t4.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(asset.subgroup.t4.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(asset.subgroup.t4.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(asset.subgroup.t4.ATT), residuals(asset.subgroup.t4.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(asset.subgroup.t4.ATT), residuals(asset.subgroup.t4.ATT)))

asset.quintiles.t4 <- asset.outcome.t4.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(asset.quintiles.t4$mean)

# ---
# 5.3 Household marine tenure

tenure.subgroup.t4.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=tenure.outcome.t4.subgroup)
summary((glht(tenure.subgroup.t4.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(tenure.subgroup.t4.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(tenure.subgroup.t4.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(tenure.subgroup.t4.ATT), residuals(tenure.subgroup.t4.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(tenure.subgroup.t4.ATT), residuals(tenure.subgroup.t4.ATT)))

tenure.quintiles.t4 <- tenure.outcome.t4.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(tenure.quintiles.t4$mean)

# ---
# 5.4 School enrollment

enrol.subgroup.t4.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=enrol.outcome.t4.subgroup)
summary((glht(enrol.subgroup.t4.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(enrol.subgroup.t4.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(enrol.subgroup.t4.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(enrol.subgroup.t4.ATT), residuals(enrol.subgroup.t4.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(enrol.subgroup.t4.ATT), residuals(enrol.subgroup.t4.ATT)))

enrol.quintiles.t4 <- enrol.outcome.t4.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(enrol.quintiles.t4$mean)


# ---
# 5.5 Place attachment
attach.subgroup.t4.ATT <- lmer(ATT~(1|MPAID)+ fisher.binary + IndividualGender + asset.quintile,data=attach.outcome.t4.subgroup)
summary((glht(attach.subgroup.t4.ATT,linfct=mcp(fisher.binary="Tukey"))))
summary((glht(attach.subgroup.t4.ATT,linfct=mcp(IndividualGender="Tukey"))))
summary(glht(attach.subgroup.t4.ATT,linfct=mcp(asset.quintile="Tukey")))

#Check residuals
#plot(fitted(attach.subgroup.t4.ATT), residuals(attach.subgroup.t4.ATT), xlab = "Fitted Values", ylab = "Residuals")
#abline(h = 0, lty = 2)
#lines(smooth.spline(fitted(attach.subgroup.t4.ATT), residuals(attach.subgroup.t4.ATT)))

attach.quintiles.t4 <- attach.outcome.t4.subgroup %>%
  group_by(asset.quintile)%>%
  summarise(mean = mean(ATT),
            se = sd(ATT)/sqrt(length(ATT)))

barplot(attach.quintiles.t4$mean)

impacts_theme <- ggthemr(palette = "dust", layout = "clear", spacing = 1.6, text_size = 12, type = "inner", line_weight = 0.5, pos = 1, envir = as.environment(pos), set_theme = TRUE)

