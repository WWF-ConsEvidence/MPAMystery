
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: METADATA DISCREPANCIES & DATA PREP ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 CHECK NUM HOUSEHOLDS AND SETTLEMENTS PER MPA, BETWEEN UNIPA & WWF DATABASES FOR t0,t2,t4 ----

UNIPA.metadata <- UNIPA.HHData %>%
  filter(InterviewYear<2017) %>%
  dplyr::group_by(MPAID,InterviewYear) %>%
  dplyr::summarise(NumHH=length(HouseholdID),
            NumSett=length(unique(SettlementID)))

WWF.metadata <- WWF.HHData %>% 
  dplyr::group_by(MPAID,InterviewYear) %>%
  dplyr::summarise(NumHH=length(HouseholdID),
            NumSett=length(unique(SettlementID)))



# --- 1.2 Recode Kaimana Settlements to three MPAs (Buruway, Etna, Triton) ---

UNIPA.HHData <-
  UNIPA.HHData %>%
  mutate(MPAID=ifelse(SettlementID %in% c(113,82,81,83,84),7,
                      ifelse(SettlementID %in% c(114,115,93,94,92),8,
                             ifelse(SettlementID %in% c(85:90,95,91),9,MPAID))))


WWF.HHData <-
  WWF.HHData %>% 
  mutate(MPAID=ifelse(SettlementID %in% c(113,82,81,83,84),7,
                      ifelse(SettlementID %in% c(114,115,93,94,92),8,
                             ifelse(SettlementID %in% c(85:90,95,91),9,MPAID))))



# ---- 1.3 Define data sets for figure testing ----

# - Data for figures EXCLUDING control settlements
# --- Includes Settlement 84, despite it not having any post-baseline data & has not recoded Settlements 83, 91, or 92 to be controls
WWF.A <- 
  WWF.HHData %>% filter(MPAID<10 & Treatment==1)


UNIPA.A <-
  UNIPA.HHData %>% filter(InterviewYear<2017 & Treatment==1)


# --- Excludes Settlement 84 and recodes Settlements 83, 91, and 92 as controls (and excludes from dataframe) 
WWF.B <-
  WWF.HHData %>% 
  mutate(Treatment=ifelse(SettlementID %in% c(83,91,92),0,Treatment)) %>%
  filter(MPAID<10 & SettlementID!=84 & Treatment==1)


UNIPA.B <-
  UNIPA.HHData %>% 
  mutate(Treatment=ifelse(SettlementID %in% c(83,91,92),0,Treatment)) %>%
  filter(InterviewYear<2017 & SettlementID!=84 & Treatment==1)


  
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: DEFINE PLOT THEMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


source('2_Functions/3_Plotting/Function_plotthemes.R')


plot.theme.1 <- theme(axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.x=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.y=element_blank(),
                    plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                           angle=0,
                                           colour="#303030",
                                           lineheight=0.7),
                    legend.position="top",
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: FIGURE TESTING, BIG FIVE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- WWF.A & UNIPA.A figures ----

# --- Material assets
MA.WWF.A <-
  WWF.A %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(MAMean=mean(MAIndex,na.rm=T),
            MAErr=sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),
            NumHH=length(HouseholdID[!is.na(MAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,ymax=MAMean+MAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,30)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.A Material Assets")

MA.UNIPA.A <-
  UNIPA.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(MAMean=mean(MAIndex,na.rm=T),
            MAErr=sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),
            NumHH=length(HouseholdID[!is.na(MAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,ymax=MAMean+MAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,30)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.A Material Assets")


# --- Food security

FS.WWF.A <-
  WWF.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(FSMean=mean(FSIndex,na.rm=T),
            FSErr=sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),
            NumHH=length(HouseholdID[!is.na(FSIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=FSMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=FSMean-FSErr,ymax=FSMean+FSErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.2)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.A Food Security")


FS.UNIPA.A <-
  UNIPA.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(FSMean=mean(FSIndex,na.rm=T),
            FSErr=sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),
            NumHH=length(HouseholdID[!is.na(FSIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=FSMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=FSMean-FSErr,ymax=FSMean+FSErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.2)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.A Food Security")


# --- Marine tenure

MT.WWF.A <-
  WWF.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(MTMean=mean(MTIndex,na.rm=T),
            MTErr=sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),
            NumHH=length(HouseholdID[!is.na(MTIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MTMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,ymax=MTMean+MTErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.A Marine Tenure")


MT.UNIPA.A <-
  UNIPA.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(MTMean=mean(MTIndex,na.rm=T),
            MTErr=sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),
            NumHH=length(HouseholdID[!is.na(MTIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MTMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,ymax=MTMean+MTErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.A Marine Tenure")


# --- Place attachment

PA.WWF.A <-
  WWF.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(PAMean=mean(PAIndex,na.rm=T),
            PAErr=sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),
            NumHH=length(HouseholdID[!is.na(PAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=PAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=PAMean-PAErr,ymax=PAMean+PAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.A Place Attachment")


PA.UNIPA.A <-
  UNIPA.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(PAMean=mean(PAIndex,na.rm=T),
            PAErr=sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),
            NumHH=length(HouseholdID[!is.na(PAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=PAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=PAMean-PAErr,ymax=PAMean+PAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.A Place Attachment")


# --- School enrollment

SE.WWF.A <-
  WWF.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(SEMean=mean(SERate,na.rm=T),
            SEErr=sd(SERate,na.rm=T)/sqrt(length(SERate)),
            NumHH=length(HouseholdID[!is.na(SERate)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=SEMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=SEMean-SEErr,ymax=SEMean+SEErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.A School Enrollment")


SE.UNIPA.A <-
  UNIPA.A %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(SEMean=mean(SERate,na.rm=T),
            SEErr=sd(SERate,na.rm=T)/sqrt(length(SERate)),
            NumHH=length(HouseholdID[!is.na(SERate)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=SEMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=SEMean-SEErr,ymax=SEMean+SEErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.A School Enrollment")



# ---- WWF.B & UNIPA.B figures ----

# --- Material assets
MA.WWF.B <-
  WWF.B %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(MAMean=mean(MAIndex,na.rm=T),
            MAErr=sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),
            NumHH=length(HouseholdID[!is.na(MAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,ymax=MAMean+MAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,30)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.B Material Assets")

MA.UNIPA.B <-
  UNIPA.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(MAMean=mean(MAIndex,na.rm=T),
            MAErr=sd(MAIndex,na.rm=T)/sqrt(length(MAIndex)),
            NumHH=length(HouseholdID[!is.na(MAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MAMean-MAErr,ymax=MAMean+MAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,30)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.B Material Assets")


# --- Food security

FS.WWF.B <-
  WWF.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(FSMean=mean(FSIndex,na.rm=T),
            FSErr=sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),
            NumHH=length(HouseholdID[!is.na(FSIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=FSMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=FSMean-FSErr,ymax=FSMean+FSErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.2)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.B Food Security")


FS.UNIPA.B <-
  UNIPA.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(FSMean=mean(FSIndex,na.rm=T),
            FSErr=sd(FSIndex,na.rm=T)/sqrt(length(FSIndex)),
            NumHH=length(HouseholdID[!is.na(FSIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=FSMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=FSMean-FSErr,ymax=FSMean+FSErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,6.2)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.B Food Security")


# --- Marine tenure

MT.WWF.B <-
  WWF.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(MTMean=mean(MTIndex,na.rm=T),
            MTErr=sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),
            NumHH=length(HouseholdID[!is.na(MTIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MTMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,ymax=MTMean+MTErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.B Marine Tenure")


MT.UNIPA.B <-
  UNIPA.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(MTMean=mean(MTIndex,na.rm=T),
            MTErr=sd(MTIndex,na.rm=T)/sqrt(length(MTIndex)),
            NumHH=length(HouseholdID[!is.na(MTIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=MTMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=MTMean-MTErr,ymax=MTMean+MTErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.B Marine Tenure")


# --- Place attachment

PA.WWF.B <-
  WWF.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(PAMean=mean(PAIndex,na.rm=T),
            PAErr=sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),
            NumHH=length(HouseholdID[!is.na(PAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=PAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=PAMean-PAErr,ymax=PAMean+PAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.B Place Attachment")


PA.UNIPA.B <-
  UNIPA.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(PAMean=mean(PAIndex,na.rm=T),
            PAErr=sd(PAIndex,na.rm=T)/sqrt(length(PAIndex)),
            NumHH=length(HouseholdID[!is.na(PAIndex)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=PAMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=PAMean-PAErr,ymax=PAMean+PAErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,5)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.B Place Attachment")


# --- School enrollment

SE.WWF.B <-
  WWF.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(SEMean=mean(SERate,na.rm=T),
            SEErr=sd(SERate,na.rm=T)/sqrt(length(SERate)),
            NumHH=length(HouseholdID[!is.na(SERate)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=SEMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=SEMean-SEErr,ymax=SEMean+SEErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="WWF.B School Enrollment")


SE.UNIPA.B <-
  UNIPA.B %>%
  group_by(MPAID,MonitoringYear) %>%
  summarise(SEMean=mean(SERate,na.rm=T),
            SEErr=sd(SERate,na.rm=T)/sqrt(length(SERate)),
            NumHH=length(HouseholdID[!is.na(SERate)])) %>%
  ungroup() %>%
  mutate(MPAID=as.character(MPAID),
         MonitoringYear=factor(MonitoringYear,levels=c("Baseline","2 Year Post","4 Year Post"),ordered=T)) %>%
  ggplot(aes(x=MPAID,y=SEMean)) +
  geom_bar(aes(group=MonitoringYear, alpha=MonitoringYear),
           stat="identity",position="dodge", fill="#1B448BD9",width=0.75) +
  geom_errorbar(aes(ymin=SEMean-SEErr,ymax=SEMean+SEErr,group=MonitoringYear),
                position=position_dodge(width=0.75),
                width=0.25,
                size=0.5,
                colour="#0A1D4E80") +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_x_discrete(labels=c("1"="Telma","2"="TNTC","3"="Kaimana","4"="Kofiau","5"="Dampier",
                            "6"="Misool","7"="Buruway","8"="Etna","9"="Triton")) +
  scale_alpha_manual(values=c("Baseline"=0.3,"2 Year Post"=0.7,"4 Year Post"=1)) +
  plot.theme.1 + labs(title="UNIPA.B School Enrollment")
