# Load in Alor/Flotim 2017 governance files and adjust their ID's to add to the Master
# Amari Bauer, July 2019

# ---- 1.1 Load libraries & data ----
library(dplyr)
library(plyr)
library(openxlsx)
library(chron)
library(lubridate)

Alor_FGD_Habitats <- read.csv("FGD_Habitats.csv")
Alor_FGD_MPA <- read.csv("FGD_MPA.csv")
Alor_FGD_Rules <- read.csv("FGD_Rules.csv")
Alor_FGD_Species <- read.csv("FGD_species.csv")
Alor_FGD_Stakeholders <- read.csv("FGD_Stakeholders.csv")
Alor_FGD_Users <- read.csv("FGD_users.csv")
Alor_KII_habrules <- read.csv("KII_habRULES.csv")
Alor_KII_MPA <- read.csv("KII_MPA.csv")
Alor_KII_Org <- read.csv("KII_Org.csv")
Alor_KII_Rights <- read.csv("KII_Rights.csv")
Alor_KII_SppRules <- read.csv("KII_sppRULES.csv")
Alor_KII_Zones <- read.csv("KII_zones.csv")

Flotim_FGD_Habitats <- read.csv("FGD_HabitatsFlo.csv")
Flotim_FGD_MPA <- read.csv("FGD_MPAFlo.csv")
Flotim_FGD_Rules <- read.csv("FGD_RulesFlo.csv")
Flotim_FGD_Species <- read.csv("FGD_SpeciesFlo.csv")
Flotim_FGD_Stakeholders <- read.csv("FGD_StakeholdersFlo.csv")
Flotim_FGD_Users <- read.csv("FGD_UsersFlo.csv")
Flotim_KII_habrules <- read.csv("KII_habRULESFlo.csv")
Flotim_KII_MPA <- read.csv("KII_MPAFlo.csv")
Flotim_KII_Org <- read.csv("KII_OrgFlo.csv")
Flotim_KII_Rights <- read.csv("KII_RightsFlo.csv")
Flotim_KII_SppRules <- read.csv("KII_sppRulesFlo.csv")
Flotim_KII_Zones <- read.csv("KII_zonesFlo.csv")

Max_ID_per_table_master <- data.frame(FGD_MPA=230,
                                      FGD_USERS=1418,
                                      FGD_HABITATS=853,
                                      FGD_SPECIES=1332,
                                      FGD_RULES=806,
                                      FGD_STAKEHOLDERS=1675,
                                      KII_MPA=424,
                                      KII_ZONES=796,
                                      KII_sppRULES=2347,
                                      KII_habRULES=1329,
                                      KII_RIGHTS=3364,
                                      KII_ORG=176)

# ---- 2.1 ALOR: plyr::rename and add new IDs----

Month.to.number <- data.frame(Num=c(10,11,11,11,12,1),
                  "MONTH"=c("Oktober","November","Nopember","nopember","Desember","Januari"))

Alor_FGD_MPA <- left_join(Alor_FGD_MPA,Month.to.number,by="MONTH")

Alor_FGD_MPA <- Alor_FGD_MPA %>%
  plyr::rename(c("TraditionalGov"="TraditionalGovernance")) %>%
  .[order(.$FGDID),] %>%
  mutate(Npenalty=NA, VerbalSanction=NA, PhysicalSanction=NA,MonetarySanction=NA,
         DAY=Date, MONTH=Num,
         Date=paste0(Num,"/",Date,"/",YEAR),
         FGDID=seq(Max_ID_per_table_master$FGD_MPA+1,Max_ID_per_table_master$FGD_MPA+length(FGDID),by=1)) %>%
  select(FGDID,CountryCode,SiteCode,SettlementCode,FGDCode,FacilitatorCode,NotetakerCode,
         Date,DAY,MONTH,YEAR,StartTime,EndTime,MaleParticipants,FemaleParticipants,
         TotalParticipants,FGDVersion,FGroundName,FGroundBoat,FGroundTime,FGroundDist,	
         FGroundSize,MPAName,MPABoat,MPATime,MPADist,MPASize,NTName,NTBoat,NTTime,NTDist,
         NTSize,MPAHistL,BndLandmark,BndMarkers,BndSigns,BndGovNotice,BndWOutreach,
         BndAOutreach,BndVOutreach,BndWord,BndOtherOutreach,BndOther,BndOtherSpecifyL,
         PenaltyVerbal,PenaltyWritten,PenaltyAccess,PenaltyEquipment,PenaltyFines,
         PenaltyPrison,PenaltyOther,PenaltyOtherSpecifyL,Npenalty,VerbalSanction,PhysicalSanction,
         MonetarySanction,ConflictL,ConflitUserTime,ConflictOfficialTime,ConflictUserCost,
         ConflictOfficialCost,ConflictUserDist,ConflictOfficialDist,OtherInfoL,OtherPeopleL,
         OtherSourcesL,TraditionalGovernance,ConflictN,ConGroup,ConBTWGroups,ConBTWGroupNGov,ConGov,
         ConTypeMarine,ConTypeGov,ConTypeUsers,ConTypeRec,ConTypeOther,ConTypeOther_SpecifyL,
         DataEntryCode,DataCheckCode,NotesL,QAQCNOtes)

Alor_FGD_Habitats <- Alor_FGD_Habitats %>%
  plyr::rename(c("FGDID"="FGDCode")) %>%
  .[order(.$HabitatID,.$FGDCode),] %>%
  mutate(HabitatID=seq(Max_ID_per_table_master$FGD_HABITATS+1,Max_ID_per_table_master$FGD_HABITATS+length(HabitatID), by=1),
         HabitatType=NA, 
         FGDCode=FGDCode+230) %>%
  select(HabitatID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,HabitatCode,HabitatTypeL,HabitatType)

Alor_FGD_Rules <- Alor_FGD_Rules %>%
  plyr::rename(c("FGDID"="FGDCode")) %>%
  .[order(.$RuleID,.$FGDCode),] %>%
  mutate(RuleID=seq(Max_ID_per_table_master$FGD_RULES+1,Max_ID_per_table_master$FGD_RULES+length(RuleID),by=1), 
         FGDCode=FGDCode+230) %>%
  select(RuleID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,RuleCode,RuleDescriptionL) %>%
  filter(RuleID<837)

Alor_FGD_Species <- Alor_FGD_Species %>%
  plyr::rename(c("FGDID"="FGDCode")) %>%
  .[order(.$SpeciesID,.$FGDCode),] %>%
  mutate(SpeciesID=seq(Max_ID_per_table_master$FGD_SPECIES+1,Max_ID_per_table_master$FGD_SPECIES+length(SpeciesID),by=1), 
         FGDCode=FGDCode+230) %>%
  select(SpeciesID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,Species,SpeciesLFamily,SpeciesLGenus,
         SpeciesLSpecies,SpeciesEnglish) %>%
  filter(SpeciesID < 1447)

Alor_FGD_Stakeholders <- Alor_FGD_Stakeholders %>%
  plyr::rename(c("Stakeh0lderID"="StakeholderID","FGDID"="FGDCode","C0untryC0de"="CountryCode","SiteC0de"="SiteCode",
           "SettlementC0de"="SettlementCode")) %>%
  .[order(.$StakeholderID,.$FGDCode),] %>%
  mutate(StakeholderID=seq(Max_ID_per_table_master$FGD_STAKEHOLDERS+1,Max_ID_per_table_master$FGD_STAKEHOLDERS+length(StakeholderID),by=1), 
         FGDCode=FGDCode+230) %>%
  select(StakeholderID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,StakeholderNameL,
         ParticipateEstablish,ParticipateBoundaries,ParticipateAdmin,ParticipateRules,
         MonitorEco,MonitorSoc,MonitorCompliance,EnforceFreq) %>%
  filter(StakeholderID < 1862)

Alor_FGD_Users <- Alor_FGD_Users %>%
  plyr::rename(c("FGDID"="FGDCODE")) %>%
  .[order(.$USERID,.$FGDCODE),] %>%
  mutate(USERID=seq(Max_ID_per_table_master$FGD_USERS+1,Max_ID_per_table_master$FGD_USERS+length(USERID),by=1), 
         FGDCODE=FGDCODE+230) %>%
  select(USERID,FGDCODE,YEAR,CountryCode,SiteCode,SettlementCode,UserCode,UserNameL,UserNameE,ExtBnd,
         IntBnd,ParticipateEstablish,ParticipateBoundaries,ParticipateAdmin,ParticipateRules,MonitorEco,
         MonitorSoc,MonitorCompliance,EnforceFreq,ContributionRank,BenefitRank)

Month.to.number <- data.frame(Num=c(10,11,11,11,12,1),
                              'INTERVIEW.MONTH'=c("Oktober","November","Nopember","nopember","Desember","Januari"))

Alor_KII_MPA <- left_join(Alor_KII_MPA,Month.to.number,by='INTERVIEW.MONTH')

Alor_KII_MPA <- Alor_KII_MPA %>%
  plyr::rename(c("KII_Field_Code"="KIICode","INTERVIEW.MONTH"="INTERVIEW MONTH","INTERVIEW.YEAR"="INTERVIEW YEAR")) %>%
  .[order(.$KII_ID),] %>%
  mutate('INTERVIEW DATE'= InterviewDate,
        InterviewDate= paste0(Num,"/",InterviewDate,"/",`INTERVIEW YEAR`),
        'INTERVIEW MONTH'=Num,
         KII_ID=seq(Max_ID_per_table_master$KII_MPA+1,Max_ID_per_table_master$KII_MPA+length(KII_ID),by=1)) %>%
  select(KII_ID,CountryCode,SiteCode,SettlementCode,KIICode,RefFGDCode,InformantName,KeyInformantRole,
         InterviewerCode,NotetakerCode,InterviewDate,`INTERVIEW DATE`,`INTERVIEW MONTH`,`INTERVIEW YEAR`,
         StartTime,EndTime,KIIVersion,MPAHistoryL,PilotNZones,EcoZone,SocZone,DRuleEco,DRuleSoc,
         PilotNestedness,RuleCommL,RuleAwareL,RulePracticeL,InformalRuleL,RuleParticipationL,MonitorL,
         PenVerbal,PenWritten,PenAccess,PenEquipment,PenFines,PenIncarceraton,PenOtherL,PenaltyFreq,
         PenPrevious,PenEco,PenEcon,PenSoc,PenWealth,PenPower,PenStatus,PenOtherL,PenOtherSpecifyL,IncenEd,
         IncenSkills,IncenEquipment,IncenPurchase,IncenLoan,IncenPayment,IncenEmploy,IncenOtherL,
         IncenOtherSpecifyL,EcoMonVerbal,EcoMonWritten,EcoMonAccess,EcoMonPosition,EcoMonEquipment,
         EcoMonFine,EcoMonIncarceration,EcoMonOtherL,SocMonVerbal,SocMonWritten,SocMonAccess,
         SocMonPosition,SocMonEquipment,SocMonFine,SocMonIncarceration,SocMonOtherL,CompMonVerbal,
         CompMonWritten,CompMonAccess,CompMonPosition,CompMonEquipment,CompMonFine,CompMonIncarceration,
         CompMonOtherL,PenMonVerbal,PenMonWritten,PenMonAccess,PenMonPosition,PenMonEquipment,PenMonFine,
         PenMonIncarceration,PenMonOtherL,ConflictResL,EcoImpactL,SocImpactL,ContributionL,BenefitL,
         AnyOtherInfoL,AnyOtherKIL,AnyOtherDocsL,NotesL,DataEntry,DataCheck,ViolationFreq) %>%
  filter(KII_ID < 506)

Alor_KII_habrules <- Alor_KII_habrules %>%
  plyr::rename(c("KII_ID"="KIICode")) %>%
  .[order(.$HABRulesID,.$KIICode),] %>%
  mutate(HabSpecRule=NA,
         KIICode=KIICode+424,
         HABRulesID=seq(Max_ID_per_table_master$KII_habRULES+1,Max_ID_per_table_master$KII_habRULES+length(HABRulesID),by=1)) %>%
  select(HABRulesID,KIICode,Year,CountryCode,SiteCode,SettlementCode,HabitatNameL,HabRule,HabSpecRuleL,
         HabSpecRule) %>%
  filter(HABRulesID < 1605)

Alor_KII_Org <- Alor_KII_Org %>%
  .[order(.$OrgID,.$KII_ID),] %>%
  mutate(OrgID=seq(Max_ID_per_table_master$KII_ORG+1,Max_ID_per_table_master$KII_ORG+length(OrgID),by=1),
         KII_ID=KII_ID+424) %>%
  select(OrgID,KII_ID,YEAR,CountryCode,SiteCode,SettlementCode,OrganizationNameL)																			

Alor_KII_Rights <- Alor_KII_Rights %>%
  plyr::rename(c("KII_ID"="KIICode")) %>%
  .[order(.$RightsID,.$KIICode),] %>%
  mutate(ISSUES=NA,
         UserSpecRule=NA,
         KIICode=KIICode+424,
         RightsID=seq(Max_ID_per_table_master$KII_RIGHTS+1,Max_ID_per_table_master$KII_RIGHTS+length(RightsID),by=1)) %>%
  select(RightsID,KIICode,YEAR,CountryCode,SiteCode,SettlementCode,UserNameL,UserRule,UserSpecRuleL,
         UserSpecRule,GovtSupport,UserRulesInc,ISSUES) %>%
  filter(RightsID < 3596)


Alor_KII_SppRules <- Alor_KII_SppRules %>%
  plyr::rename(c("KII_ID"="KIICode","SpeciesLFamily"="Family","SpeciesLGenus"="Genus",
           "SpeciesLSpecies"="Species")) %>%
  .[order(.$SPPRulesID,.$KIICode),] %>%
  mutate(KII_order=NA, SppSpecificRule=NA,KIICode=KIICode+424,
         SPPRulesID=seq(Max_ID_per_table_master$KII_sppRULES+1,Max_ID_per_table_master$KII_sppRULES+length(SPPRulesID),by=1)) %>%
  select(SPPRulesID,KIICode,YEAR,CountryCode,SiteCode,SettlementCode,KII_order,SpeciesNameL,Family,
         Genus,Species,SppRule,SppSpecificRuleL,SppSpecificRule) %>%
  filter(SPPRulesID < 2694)

Alor_KII_Zones <- Alor_KII_Zones %>%
  plyr::rename(c("KII_ID"="KIICode")) %>%
  .[order(.$ZoneID,.$KIICode),] %>%
  mutate(KIIOrder=NA, KIICode=KIICode+424,
         ZoneID=seq(Max_ID_per_table_master$KII_ZONES+1,Max_ID_per_table_master$KII_ZONES+length(ZoneID),by=1)) %>%
  select(ZoneID,KIICode,YEAR,Country,SiteCode,SettlementCode,KIIOrder,ZoneTypeL,ZoneQuantity,ZoneOrg,
         ZoneCoord) %>%
  filter(ZoneID < 878)

# ---- 2.2 FLOTIM: plyr::rename and add new IDs----

Month.to.number <- data.frame(Num=c(10,11,11,11,12,1),
                              "MONTH"=c("Oktober","November","Nopember","nopember","Desember","Januari"))

Flotim_FGD_MPA <- left_join(Flotim_FGD_MPA,Month.to.number,by="MONTH")


Flotim_FGD_MPA <- Flotim_FGD_MPA %>%
  plyr::rename(c("TraditionalGov"="TraditionalGovernance","FGroundTime..minute."="FGroundTime",
                 "FGroundDist..kilometer."="FGroundDist","FGroundSize..kilometer.square."="FGroundSize")) %>%
  .[order(.$FGDID,.$FGDCode),] %>%
  mutate(DAY=Date,Date=paste0(Num,"/",DAY,"/",YEAR), MONTH=Num,
    Npenalty=NA, VerbalSanction=NA, PhysicalSanction=NA,MonetarySanction=NA,
         FGDID=seq(max(Alor_FGD_MPA$FGDID)+1,max(Alor_FGD_MPA$FGDID)+length(FGDID),by=1)) %>%
  select(FGDID,CountryCode,SiteCode,SettlementCode,FGDCode,FacilitatorCode,NotetakerCode,
         Date,DAY,MONTH,YEAR,StartTime,EndTime,MaleParticipants,FemaleParticipants,
         TotalParticipants,FGDVersion,FGroundName,FGroundBoat,FGroundTime,FGroundDist,	
         FGroundSize,MPAName,MPABoat,MPATime,MPADist,MPASize,NTName,NTBoat,NTTime,NTDist,
         NTSize,MPAHistL,BndLandmark,BndMarkers,BndSigns,BndGovNotice,BndWOutreach,
         BndAOutreach,BndVOutreach,BndWord,BndOtherOutreach,BndOther,BndOtherSpecifyL,
         PenaltyVerbal,PenaltyWritten,PenaltyAccess,PenaltyEquipment,PenaltyFines,
         PenaltyPrison,PenaltyOther,PenaltyOtherSpecifyL,Npenalty,VerbalSanction,PhysicalSanction,
         MonetarySanction,ConflictL,ConflitUserTime,ConflictOfficialTime,ConflictUserCost,
         ConflictOfficialCost,ConflictUserDist,ConflictOfficialDist,OtherInfoL,OtherPeopleL,
         OtherSourcesL,TraditionalGovernance,ConflictN,ConGroup,ConBTWGroups,ConBTWGroupNGov,ConGov,
         ConTypeMarine,ConTypeGov,ConTypeUsers,ConTypeRec,ConTypeOther,ConTypeOther_SpecifyL,
         DataEntryCode,DataCheckCode,NotesL,QAQCNOtes) %>%
  filter(FGDID < 284)

Flotim_FGD_Habitats <- Flotim_FGD_Habitats %>%
  plyr::rename(c("FGDID"="FGDCode")) %>%
  .[order(.$HabitatID,.$FGDCode),] %>%
  mutate(HabitatID=seq(max(Alor_FGD_Habitats$HabitatID)+1,max(Alor_FGD_Habitats$HabitatID)+length(HabitatID),by=1),
         HabitatType=NA,
         FGDCode=FGDCode+283) %>%
  select(HabitatID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,HabitatCode,HabitatTypeL,HabitatType) %>%
  filter(HabitatID < 1020)

Flotim_FGD_Rules <- Flotim_FGD_Rules %>%
  plyr::rename(c("FGDID"="FGDCode")) %>%
  .[order(.$RuleID,.$FGDCode),] %>%
  mutate(RuleID=seq(max(Alor_FGD_Rules$RuleID)+1,(max(Alor_FGD_Rules$RuleID)+length(RuleID)),by=1),
         FGDCode=FGDCode+283) %>%
  select(RuleID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,RuleCode,RuleDescriptionL) %>%
  filter(RuleID < 863)

Flotim_FGD_Species <- Flotim_FGD_Species %>%
  plyr::rename(c("FGDID"="FGDCode")) %>%
  .[order(.$SpeciesID,.$FGDCode),] %>%
  mutate(SpeciesID=seq(max(Alor_FGD_Species$SpeciesID)+1,max(Alor_FGD_Species$SpeciesID)+length(SpeciesID),by=1),
         FGDCode=FGDCode+283) %>%
  select(SpeciesID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,Species,SpeciesLFamily,SpeciesLGenus,
         SpeciesLSpecies,SpeciesEnglish) %>%
  filter(SpeciesID < 1571)

Flotim_FGD_Stakeholders <- Flotim_FGD_Stakeholders %>%
  plyr::rename(c("Stakeh0lderID"="StakeholderID","FGDID"="FGDCode","CountryC0de"="CountryCode","SiteC0de"="SiteCode",
           "SettlementC0de"="SettlementCode")) %>%
  .[order(.$StakeholderID,.$FGDCode),] %>%
  mutate(StakeholderID=seq(max(Alor_FGD_Stakeholders$StakeholderID)+1,max(Alor_FGD_Stakeholders$StakeholderID)+length(StakeholderID),by=1),
         FGDCode=FGDCode+283) %>%
  select(StakeholderID,FGDCode,YEAR,CountryCode,SiteCode,SettlementCode,StakeholderNameL,
         ParticipateEstablish,ParticipateBoundaries,ParticipateAdmin,ParticipateRules,
         MonitorEco,MonitorSoc,MonitorCompliance,EnforceFreq) %>%
  filter(StakeholderID < 2044)

Flotim_FGD_Users <- Flotim_FGD_Users %>%
  plyr::rename(c("FGDID"="FGDCODE")) %>%
  .[order(.$USERID,.$FGDCODE),] %>%
  mutate(USERID=seq(max(Alor_FGD_Users$USERID)+1,max(Alor_FGD_Users$USERID)+length(USERID),by=1),
         FGDCODE=FGDCODE+283) %>%
  select(USERID,FGDCODE,YEAR,CountryCode,SiteCode,SettlementCode,UserCode,UserNameL,UserNameE,ExtBnd,
         IntBnd,ParticipateEstablish,ParticipateBoundaries,ParticipateAdmin,ParticipateRules,MonitorEco,
         MonitorSoc,MonitorCompliance,EnforceFreq,ContributionRank,BenefitRank) %>%
  filter(USERID < 1665)

Month.to.number <- data.frame(Num=c(10,11,11,11,12,1),
                              "INTERVIEW.MONTH"=c("Oktober","November","Nopember","nopember","Desember","Januari"))

Flotim_KII_MPA <- left_join(Flotim_KII_MPA,Month.to.number,by="INTERVIEW.MONTH")

Flotim_KII_MPA <- Flotim_KII_MPA %>%
  plyr::rename(c("KII_Field_Code"="KIICode","INTERVIEW.MONTH"="INTERVIEW MONTH","INTERVIEW.YEAR"="INTERVIEW YEAR")) %>%
  .[order(.$KII_ID),] %>%
  mutate(`INTERVIEW DATE`=day(mdy(InterviewDate)), `INTERVIEW MONTH`=Num, 
         KII_ID=seq(max(Alor_KII_MPA$KII_ID)+1,max(Alor_KII_MPA$KII_ID)+length(KII_ID),by=1)) %>%
  select(KII_ID,CountryCode,SiteCode,SettlementCode,KIICode,RefFGDCode,InformantName,KeyInformantRole,
         InterviewerCode,NotetakerCode,InterviewDate,`INTERVIEW DATE`,`INTERVIEW MONTH`,`INTERVIEW YEAR`,
         StartTime,EndTime,KIIVersion,MPAHistoryL,PilotNZones,EcoZone,SocZone,DRuleEco,DRuleSoc,
         PilotNestedness,RuleCommL,RuleAwareL,RulePracticeL,InformalRuleL,RuleParticipationL,MonitorL,
         PenVerbal,PenWritten,PenAccess,PenEquipment,PenFines,PenIncarceraton,PenOtherL,PenaltyFreq,
         PenPrevious,PenEco,PenEcon,PenSoc,PenWealth,PenPower,PenStatus,PenOtherL,PenOtherSpecifyL,IncenEd,
         IncenSkills,IncenEquipment,IncenPurchase,IncenLoan,IncenPayment,IncenEmploy,IncenOtherL,
         IncenOtherSpecifyL,EcoMonVerbal,EcoMonWritten,EcoMonAccess,EcoMonPosition,EcoMonEquipment,
         EcoMonFine,EcoMonIncarceration,EcoMonOtherL,SocMonVerbal,SocMonWritten,SocMonAccess,
         SocMonPosition,SocMonEquipment,SocMonFine,SocMonIncarceration,SocMonOtherL,CompMonVerbal,
         CompMonWritten,CompMonAccess,CompMonPosition,CompMonEquipment,CompMonFine,CompMonIncarceration,
         CompMonOtherL,PenMonVerbal,PenMonWritten,PenMonAccess,PenMonPosition,PenMonEquipment,PenMonFine,
         PenMonIncarceration,PenMonOtherL,ConflictResL,EcoImpactL,SocImpactL,ContributionL,BenefitL,
         AnyOtherInfoL,AnyOtherKIL,AnyOtherDocsL,NotesL,DataEntry,DataCheck,ViolationFreq) %>%
  filter(KII_ID < 584)

Flotim_KII_habrules <- Flotim_KII_habrules %>%
  plyr::rename(c("KII_ID"="KIICode")) %>%
  .[order(.$HABRulesID,.$KIICode),] %>%
  mutate(HabSpecRule=NA, KIICode=KIICode+505,
         HABRulesID=seq(max(Alor_KII_habrules$HABRulesID)+1,max(Alor_KII_habrules$HABRulesID)+length(HABRulesID),by=1)) %>%
  select(HABRulesID,KIICode,Year,CountryCode,SiteCode,SettlementCode,HabitatNameL,HabRule,HabSpecRuleL,
         HabSpecRule) %>%
  filter(HABRulesID < 1827)

Flotim_KII_Org <- Flotim_KII_Org %>%
  .[order(.$OrgID,.$KII_ID),] %>%
  mutate(OrgID=seq(max(Alor_KII_Org$OrgID)+1,max(Alor_KII_Org$OrgID)+length(OrgID),by=1),
         KII_ID=KII_ID+505) %>%
  select(OrgID,KII_ID,YEAR,CountryCode,SiteCode,SettlementCode,OrganizationNameL)	

Flotim_KII_Rights <- Flotim_KII_Rights %>%
  plyr::rename(c("KII_ID"="KIICode")) %>%
  .[order(.$RightsID,.$KIICode),] %>%
  mutate(ISSUES=NA,
         UserSpecRule=NA,
         KIICode=KIICode+505,
         RightsID=seq(max(Alor_KII_Rights$RightsID)+1,max(Alor_KII_Rights$RightsID)+length(RightsID),by=1)) %>%
  select(RightsID,KIICode,YEAR,CountryCode,SiteCode,SettlementCode,UserNameL,UserRule,UserSpecRuleL,
         UserSpecRule,GovtSupport,UserRulesInc,ISSUES) %>%
  filter(RightsID < 3826)


Flotim_KII_SppRules <- Flotim_KII_SppRules %>%
  plyr::rename(c("KII_ID"="KIICode","SpeciesLFamily"="Family","SpeciesLGenus"="Genus",
           "SpeciesLSpecies"="Species")) %>%
  .[order(.$SPPRulesID,.$KIICode),] %>%
  mutate(KII_order=NA, SppSpecificRule=NA,  KIICode=KIICode+505,
         SPPRulesID=seq(max(Alor_KII_SppRules$SPPRulesID)+1,max(Alor_KII_SppRules$SPPRulesID)+length(SPPRulesID),by=1)) %>%
  select(SPPRulesID,KIICode,YEAR,CountryCode,SiteCode,SettlementCode,KII_order,SpeciesNameL,Family,
         Genus,Species,SppRule,SppSpecificRuleL,SppSpecificRule) %>%
  filter(SPPRulesID < 3066)

Flotim_KII_Zones <- Flotim_KII_Zones %>%
  plyr::rename(c("KII_ID"="KIICode")) %>%
  .[order(.$ZoneID,.$KIICode),] %>%
  mutate(KIIOrder=NA, KIICode=KIICode+505,
         ZoneID=seq(max(Alor_KII_Zones$ZoneID)+1,max(Alor_KII_Zones$ZoneID)+length(ZoneID),by=1)) %>%
  select(ZoneID,KIICode,YEAR,Country,SiteCode,SettlementCode,KIIOrder,ZoneTypeL,ZoneQuantity,ZoneOrg,
         ZoneCoord) %>%
  filter(ZoneID < 956)

# ---- 3.1 Write to xlsx for final data type check before pasting to master database ----

wb <- createWorkbook("Alor_Flotim_Gov_2017") 

addWorksheet(wb,"FGD_MPA")
addWorksheet(wb,"FGD_USERS")
addWorksheet(wb,"FGD_HABITATS")
addWorksheet(wb,"FGD_SPECIES")
addWorksheet(wb,"FGD_RULES")
addWorksheet(wb,"FGD_STAKEHOLDERS")
addWorksheet(wb,"KII_MPA")
addWorksheet(wb,"KII_ZONES")
addWorksheet(wb,"KII_sppRULES")
addWorksheet(wb,"KII_habRULES")
addWorksheet(wb,"KII_RIGHTS")
addWorksheet(wb,"KII_ORG")

writeData(wb,"FGD_MPA",rbind.data.frame(Alor_FGD_MPA,Flotim_FGD_MPA))
writeData(wb,"FGD_USERS",rbind.data.frame(Alor_FGD_Users,Flotim_FGD_Users))
writeData(wb,"FGD_HABITATS",rbind.data.frame(Alor_FGD_Habitats,Flotim_FGD_Habitats))
writeData(wb,"FGD_SPECIES",rbind.data.frame(Alor_FGD_Species,Flotim_FGD_Species))
writeData(wb,"FGD_RULES",rbind.data.frame(Alor_FGD_Rules,Flotim_FGD_Rules))
writeData(wb,"FGD_STAKEHOLDERS",rbind.data.frame(Alor_FGD_Stakeholders,Flotim_FGD_Stakeholders))
writeData(wb,"KII_MPA",rbind.data.frame(Alor_KII_MPA,Flotim_KII_MPA))
writeData(wb,"KII_ZONES",rbind.data.frame(Alor_KII_Zones,Flotim_KII_Zones))
writeData(wb,"KII_sppRULES",rbind.data.frame(Alor_KII_SppRules,Flotim_KII_SppRules))
writeData(wb,"KII_habRULES",rbind.data.frame(Alor_KII_habrules,Flotim_KII_habrules))
writeData(wb,"KII_RIGHTS",rbind.data.frame(Alor_KII_Rights,Flotim_KII_Rights))
writeData(wb,"KII_ORG",rbind.data.frame(Alor_KII_Org,Flotim_KII_Org))

saveWorkbook(wb,'C:/Users/HP/Dropbox/this one/MyWork/READYTOMERGE.xlsx')








