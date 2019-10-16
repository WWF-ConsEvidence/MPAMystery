# Plots that require post-coding before being able to be run

source('2_Functions/3_Plotting/Function_plotthemes.R')

# PostCodeData.PLOTFORMAT <-
# IMPORT ThreatTypeData, EthnicityData, EducationData, ParticipationData, RulesData
# sort settlementName in reverse order, rbind to MPA-level analysis of same data


# - THREAT TYPES
ThreatType.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("Other", "OtherMarineUses", "NaturalProcesses", "HabitatLoss",
                                               "ClimateChange", "IllegalFishing", "DestructiveFishing", "Pollution")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ThreatType"]],
                    labels=c("Other", "Other marine resource uses", "Natural processes", "Habitat loss",
                             "Climate change", "Illegal fishing", "Destructive fishing", "Pollution")) +
  coord_flip() + plot.theme + Statusplot.labs["ThreatTypes"] + plot.guides.techreport


# - NUMBER ETHNICITIES
Ethnicity.statusplot <- 
  ggplot(data=PostCodeData.PLOTFORMAT,
         aes(x=SettlementName)) +
  geom_bar(aes(y=Num.EthnicGroups,
               fill="NotDummy"),
           stat="identity",
           position="dodge",
           width=0.75,
           show.legend=F) +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T) +
                                0.03*max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
  scale_fill_manual(values=fillcols.status) +
  scale_colour_manual(values=errcols.status) +
  coord_flip() + Statusplot.labs["Ethnicity"] + plot.theme


# - ADULT EDUCATION
AdultEduc.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("AdultEducHigher", "AdultEducSec", "AdultEducMid",
                                               "AdultEducPrim", "AdultEducPre", "AdultEducNone")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["AdultEducation"]],
                    labels=c("Further or higher education","High school education","Middle school education","Primary school education","Pre-school education", "No formal education")) +
  coord_flip() + plot.theme + Statusplot.labs["AdultEduc"] + plot.guides.techreport


# - HOUSEHOLD HEAD EDUCATION
HHHEduc.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("HHHEducHigher", "HHHEducSec", "HHHEducMid",
                                               "HHHEducPrim", "HHHEducPre", "HHHEducNone")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["HHHEducation"]],
                    labels=c("Further or higher education","High school education","Middle school education","Primary school education","Pre-school education", "No formal education")) +
  coord_flip() + plot.theme + Statusplot.labs["HHHEduc"] + plot.guides.techreport


# - RULES
Rules.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("PropRuleHab", "PropRuleSpp")) %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["PropRules"]],
                    labels=c("Important species","Important habitats")) +
  coord_flip() + plot.theme + Statusplot.labs["Rules"] + plot.guides.techreport


# - PARTICIPATION IN DECISION-MAKING
Participation.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars="SettlementName",measure.vars=c("ParticipateRules","ParticipateBnd","ParticipateOrg", "ParticipateEstablish")) %>%
  filter(., SettlementName!= "Control\nSettlements") %>%
  ggplot(aes(x=SettlementName,y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="dodge",
           width=0.75,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=2),
             linetype=2,
             size=0.35,
             colour="#505050") +
  scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["Participate"]],
                    labels=c("Setting appropriation rules", "MPA boundary delineation", "Design of MPA management body", "Design of MPA-managing organization")) +
  coord_flip() + plot.theme + Statusplot.labs["Participation"] + plot.guides.techreport
