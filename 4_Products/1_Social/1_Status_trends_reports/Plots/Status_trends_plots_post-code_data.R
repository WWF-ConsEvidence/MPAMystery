# Plots that require post-coding before being able to be run

# ---CURRENTLY RUNNING ONLY THREAT STATUS DATA FOR WAKATOBI WITH THIS SCRIPT AS IT'S CURRENTLY WRITTEN.  

# ---- source scripts, import lookup tables ----

pacman::p_load(rio,ggplot2,reshape2,dplyr)

source('1_Data_wrangling/1_Social/2_Source_data/Source_social_data_flat_files.R')
source('1_Data_wrangling/1_Social/3_Calculating_indicators/Calculate_household_indices.R', local=T)
source('3_Analysis/1_Social/2_Status_trends/Sett_MPA_level_means.R')
source('2_Functions/3_Plotting/Function_plotthemes.R')
source('2_Functions/3_Plotting/Function_define_asteriskplotting.R')

ed.lkp <- import('x_Flat_data_files/1_Social/Inputs/education_lkp.xlsx')

MPA.name.filtered <- 
  MPA.name %>%
  filter(.,MPAID==15)

# -- Sunda Banda Seascape
# MPA=15 # Selat Pantar
# MPA=16 # Flores Timur
# MPA=17 # Kei Kecil
# MPA=18 # Koon
# MPA=19 # Yamdena
# MPA=20 # Sulawesi Tenggara
# MPA=21 # Wakatobi



# ----- import post-coded threat type data from MPA of choice & analyze at settlement and MPA level ----

ThreatType_Import <-  
  # import('C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/Kei Kecil/KeiKecil_LTHREAT-postcod.xlsx')
  # import('C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/Koon/Koon_LTHREAT-PostCod.xlsx')
  import('C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/Selat Pantar/SelatPantar_LTHREAT 2014&2017.xlsx')
  # import('C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/Sultra/SulawesiTenggara_LTHREAT-post coding.xlsx')
  # import('C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/Wakatobi/WakatobiNP_LTHREAT_Postcode.xlsx')
  



ThreatType <-
  ThreatType_Import %>%
  # select(-c(SettlementName)) %>%
  left_join(Settlements[Settlements$MPAID==15,c("SettlementID","SettlementName")],.,by="SettlementID") %>%
  mutate(MPAID=15)
  # filter(!is.na(SettlementName))

Sett.level.ThreatType <- 
  ThreatType %>% 
  mutate(MonitoringYear = ordered(MonitoringYear, levels=c("Baseline", "2 Year Post", "3 Year Post", "4 Year Post", "5 Year Post", "6 Year Post",
                                                                    "7 Year Post", "8 Year Post", "9 Year Post", "10 Year Post"))) %>%
  filter(Treatment==1) %>%
  group_by(SettlementID,SettlementName, MonitoringYear, InterviewYear) %>%
  summarise(UnsustainableFish=(length(HouseholdID[MainThreat=="UnsustainableFishing" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            Pollution=(length(HouseholdID[MainThreat=="Pollution" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            NaturalPhenomenon=(length(HouseholdID[MainThreat=="Natural phenomenon" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            Tourism=(length(HouseholdID[MainThreat=="Tourism & coastal development" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            InadequateProc=(length(HouseholdID[MainThreat=="Inadequate protection" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            Aquaculture=(length(HouseholdID[MainThreat=="Aquaculture" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            NoThreat=(length(HouseholdID[MainThreat=="NoThreat" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100) %>%
  ungroup() %>%
  select(-SettlementID) %>%
  .[rev(order(.$SettlementName)),]


MPA.level.ThreatType <- 
  ThreatType %>%
  filter(., MPAID==15 )%>%
  mutate(SettlementID=NA,
         SettlementName=ifelse(Treatment==1,MPA.name.filtered$MPAName,"Control Settlements"),
         MonitoringYear = ordered(MonitoringYear, levels=c("Baseline", "2 Year Post", "3 Year Post", "4 Year Post", "5 Year Post", "6 Year Post",
                                                                                                      "7 Year Post", "8 Year Post", "9 Year Post", "10 Year Post"))) %>%
  group_by(MPAID,SettlementID,SettlementName,Treatment, MonitoringYear, InterviewYear) %>%
  summarise(UnsustainableFish=(length(HouseholdID[MainThreat=="UnsustainableFishing" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            Pollution=(length(HouseholdID[MainThreat=="Pollution" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            NaturalPhenomenon=(length(HouseholdID[MainThreat=="Natural phenomenon" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            Tourism=(length(HouseholdID[MainThreat=="Tourism & coastal development" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            InadequateProc=(length(HouseholdID[MainThreat=="Inadequate protection" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            Aquaculture=(length(HouseholdID[MainThreat=="Aquaculture" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100,
            NoThreat=(length(HouseholdID[MainThreat=="NoThreat" & !is.na(MainThreat)])/length(HouseholdID[!is.na(MainThreat)]))*100) %>%
  ungroup() %>%
  select(-c(MPAID,SettlementID,Treatment))
  # slice(., 1:(n()-1)) #remove last row in dataframe


null.row.ThreatType <- 
  cbind.data.frame(matrix(rep(NA,length(colnames(MPA.level.ThreatType))),
                          ncol=length(colnames(MPA.level.ThreatType)),
                          dimnames=list(NULL,colnames(MPA.level.ThreatType))))



ThreatType.status <-
  rbind.data.frame(filter(MPA.level.ThreatType, as.integer(MonitoringYear) == max(as.integer(MonitoringYear))),
                   null.row.ThreatType,
                   filter(Sett.level.ThreatType, as.integer(MonitoringYear) == max(as.integer(MonitoringYear)))) %>%
  mutate(SettlementName=ifelse(is.na(SettlementName),
                               "",
                               as.character(SettlementName)),
         SettlementName=factor(SettlementName,levels=unique(SettlementName),ordered=T),
         SettLevel=ifelse(SettlementName=="","Dummy","NotDummy"),
         SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]],
                                      ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]],
                                             ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
                                                    ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name.filtered$MPAName.bahasa,as.character(SettlementName))))),
         SettlementName.bahasa=factor(SettlementName.bahasa,levels=unique(SettlementName.bahasa),ordered=T))

# ThreatType.all <-
#   rbind.data.frame(MPA.level.ThreatType,
#                    null.row.ThreatType,
#                    Sett.level.ThreatType) %>%
#   mutate(SettlementName=ifelse(is.na(SettlementName),
#                                "",
#                                as.character(SettlementName)),
#          SettlementName=factor(SettlementName,levels=unique(SettlementName),ordered=T),
#          SettLevel=ifelse(SettlementName=="","Dummy","NotDummy"),
#          SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]],
#                                       ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]],
#                                              ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
#                                                     ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name.filtered$MPAName.bahasa,as.character(SettlementName))))),
#          SettlementName.bahasa=factor(SettlementName.bahasa,levels=unique(SettlementName.bahasa),ordered=T))
# 
# 


# - define frequency table for trend post-coded data (threat type)

FreqTables.ThreatType <- 
  ThreatType %>% filter(Treatment==1) %>%
  group_by(InterviewYear) %>%
  summarise(UnsustainableFish=length(MainThreat[MainThreat=="UnsustainableFishing" &  !is.na(MainThreat)]),
            Pollution=length(MainThreat[MainThreat=="Pollution" &  !is.na(MainThreat)]),
            NaturalPhenomenon=length(MainThreat[MainThreat=="Natural phenomenon" &  !is.na(MainThreat)]),
            Tourism=length(MainThreat[MainThreat=="Tourism & coastal development" &  !is.na(MainThreat)]),
            InadequateProc=length(MainThreat[MainThreat=="Inadequate protection" &  !is.na(MainThreat)]),
            Aquaculture=length(MainThreat[MainThreat=="Aquaculture" &  !is.na(MainThreat)]),
            NoThreat=length(MainThreat[MainThreat=="NoThreat" &  !is.na(MainThreat)]))

FreqTables.ThreatType <- 
  as.data.frame(t(FreqTables.ThreatType[,-1]))

colnames(FreqTables.ThreatType) <- 
  # c("t0")
  c("t0","repeat1")

FreqTables.ThreatType$Category <- rownames(FreqTables.ThreatType)
FreqTables.ThreatType$Variable <- "ThreatType"

# - chi squared test for significant changes in proportion of threat types through time

propdata.trend.test  <- data.frame(ThreatType=NA)
p.for.function <- NA
data.for.function <- NA


propdata.trend.test  <- 
  as.data.frame(mapply(a="ThreatType",
                       function(a) {
                         p.for.function <- 
                           if(sum(FreqTables.ThreatType$t0[FreqTables.ThreatType$Variable==a])==0) {
                             FreqTables.ThreatType$repeat1[FreqTables.ThreatType$Variable==a &
                                                             FreqTables.ThreatType$repeat1!=0] 
                           } else {FreqTables.ThreatType$t0[FreqTables.ThreatType$Variable==a &
                                                              FreqTables.ThreatType$t0!=0] }
                         data.for.function <- 
                           if(sum(FreqTables.ThreatType$t0[FreqTables.ThreatType$Variable==a])==0) {
                             FreqTables.ThreatType$repeat1[FreqTables.ThreatType$Variable==a]
                           } else {FreqTables.ThreatType$repeat1[FreqTables.ThreatType$Variable==a &
                                                                   FreqTables.ThreatType$t0!=0]}
                         propdata.trend.test [a] <- ifelse(length(data.for.function)>1,
                                                           chisq.test(data.for.function,
                                                                      p=p.for.function,
                                                                      rescale.p=TRUE,correct=TRUE)["p.value"],
                                                           NA)
                         propdata.trend.test [a] <- ifelse(is.na(propdata.trend.test [a]),100,propdata.trend.test [a])
                       }))

propdata.trend.test <-
  rbind(propdata.trend.test, 
        "Types of local threats to marine environment\n(% threats identified)",
        "Jenis ancaman lokal terhadap lingkungan laut\n(% ancaman teridentifikasi)")

# - define labels with asterisks for trend plots of threat type 

Proptrendplot.ylabs <- 
  define.proptrendplot.ylabels.withasterisks(propdata.trend.test)

Trendplot.labs <- list(ThreatType=labs(y=as.character(Proptrendplot.ylabs),x="Monitoring Year"))

Proptrendplot.ylabs.bahasa <- 
  define.proptrendplot.ylabels.withasterisks.bahasa(propdata.trend.test)

Trendplot.labs.bahasa <- list(ThreatType=labs(y=as.character(Proptrendplot.ylabs.bahasa),x=sett.names.bahasa[["MonYear"]]))


# ---- analyze other post-coded data (education, etc.) at MPA and settlement level ----

MPA.level.PostCodeData <-
  IndDemos %>%
  left_join(ed.lkp, by="IndividualEducation") %>%
  left_join(HHData[c("HouseholdID","SettlementName","InterviewYear","MonitoringYear","Treatment")], by="HouseholdID") %>%
  filter(MPAID==15 & IndividualAge>=18 & ed.level<=5) %>%
  filter(InterviewYear==max(InterviewYear)) %>%
  mutate(SettlementID=NA,
         SettlementName=ifelse(Treatment==1,MPA.name.filtered$MPAName,"Control Settlements")) %>%
  group_by(MPAID, SettlementID, SettlementName) %>%
  summarise(HHHEducNone=(length(DemographicID[ed.level==0 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducPre=(length(DemographicID[ed.level==1 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducPrim=(length(DemographicID[ed.level==2 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducMid=(length(DemographicID[ed.level==3 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducSec=(length(DemographicID[ed.level==4 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducHigher=(length(DemographicID[ed.level==5 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            AdultEducNone=(length(DemographicID[ed.level==0 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducPre=(length(DemographicID[ed.level==1 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducPrim=(length(DemographicID[ed.level==2 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducMid=(length(DemographicID[ed.level==3 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducSec=(length(DemographicID[ed.level==4 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducHigher=(length(DemographicID[ed.level==5 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100) %>% 
  ungroup() %>%
  select(-c(MPAID,SettlementID))

Sett.level.PostCodeData <-
  IndDemos %>%
  left_join(ed.lkp, by="IndividualEducation") %>%
  left_join(HHData[c("HouseholdID","SettlementName","Treatment","InterviewYear","MonitoringYear")], by="HouseholdID") %>%
  filter(MPAID==15 & IndividualAge>=18 & ed.level<=5 & Treatment==1) %>%
  filter(InterviewYear==max(InterviewYear)) %>%
  group_by(SettlementID,SettlementName) %>%
  summarise(HHHEducNone=(length(DemographicID[ed.level==0 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducPre=(length(DemographicID[ed.level==1 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducPrim=(length(DemographicID[ed.level==2 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducMid=(length(DemographicID[ed.level==3 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducSec=(length(DemographicID[ed.level==4 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            HHHEducHigher=(length(DemographicID[ed.level==5 & !is.na(ed.level) & RelationHHH==0])/length(DemographicID[!is.na(ed.level) & RelationHHH==0]))*100,
            AdultEducNone=(length(DemographicID[ed.level==0 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducPre=(length(DemographicID[ed.level==1 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducPrim=(length(DemographicID[ed.level==2 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducMid=(length(DemographicID[ed.level==3 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducSec=(length(DemographicID[ed.level==4 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100,
            AdultEducHigher=(length(DemographicID[ed.level==5 & !is.na(ed.level)])/length(DemographicID[!is.na(ed.level)]))*100) %>%
  ungroup() %>%
  select(-SettlementID) %>%
  .[rev(order(.$SettlementName)),]

null.row.PostCodeData <- 
  cbind.data.frame(matrix(rep(NA,length(colnames(MPA.level.PostCodeData))),
                          ncol=length(colnames(MPA.level.PostCodeData)),
                          dimnames=list(NULL,colnames(MPA.level.PostCodeData))))




# ---- define plotting data sets ----

PostCodeData.PLOTFORMAT <-
  rbind.data.frame(MPA.level.PostCodeData,
                   null.row.PostCodeData,
                   Sett.level.PostCodeData) %>%
  left_join(.,rbind.data.frame(MPA.level.ThreatType[MPA.level.ThreatType$InterviewYear==max(MPA.level.ThreatType$InterviewYear),],
                               null.row.ThreatType,
                               Sett.level.ThreatType) %>% select(-InterviewYear),
            by=c("SettlementName")) %>%
  mutate(SettlementName=ifelse(is.na(SettlementName), 
                               "", 
                               as.character(SettlementName)),
         SettlementName=factor(SettlementName,levels=unique(SettlementName),ordered=T),
         SettLevel=ifelse(SettlementName=="","Dummy","NotDummy"),
         SettlementName.bahasa=ifelse(grepl("Use Settlements",SettlementName),sett.names.bahasa[["Use"]],  
                                      ifelse(grepl("No Take Settlements",SettlementName),sett.names.bahasa[["NoTake"]], 
                                             ifelse(grepl("Control Settlements",SettlementName),sett.names.bahasa[["ControlSett"]],
                                                    ifelse(grepl("MPA",SettlementName,ignore.case=F),MPA.name.filtered$MPAName.bahasa,as.character(SettlementName))))),
         SettlementName.bahasa=factor(SettlementName.bahasa,levels=unique(SettlementName.bahasa),ordered=T))

PostCode.trend.PLOTFORMAT <-
  # data.frame(MonitoringYear=rep.int(c("Baseline","3 Year Post"),2),MPA.level.ThreatType) %>%
  rbind.data.frame(MPA.level.ThreatType,null.row.ThreatType) %>%
  left_join(.,define.year.monitoryear.column(MPA.Level.Means[MPA.Level.Means$MPAID==15,]),by="MonitoringYear") %>%
  left_join(.,define.year.monitoryear.column.bahasa(MPA.Level.Means[MPA.Level.Means$MPAID==15,]),by="MonitoringYear") %>%
  mutate(order=c(1,2,4,5,3),
         Label=ifelse(is.na(Label),"",as.character(Label)),
         Label.bahasa=ifelse(is.na(Label.bahasa),"",as.character(Label.bahasa)))


# ---- plots ----

# - THREAT TYPES
ThreatType.statusplot <- 
  melt(ThreatType.status,
       id.vars=c("SettlementName"),measure.vars=c("NoThreat", "Aquaculture", "InadequateProc","Tourism","NaturalPhenomenon", "Pollution","UnsustainableFish")) %>%
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
                    values=multianswer.fillcols.status[["ThreatType2"]],
                    labels=c("No Threat", "Aquaculture", "Inadequate Protection", "Tourism", 
                             "Natural Phenomenon", "Pollution", "Unsustainable Fishing")) +
  coord_flip() + 
  Statusplot.labs["ThreatTypes"] + 
  plot.guides.techreport +
  if(MPA.name.filtered$MPAID==21) { plot.theme.manysetts
  } else { plot.theme
  }


ThreatType.trendplot <-
  PostCode.trend.PLOTFORMAT %>%
  melt(.,id.vars="order",
       measure.vars=c("NoThreat", "Aquaculture", "InadequateProc","Tourism","NaturalPhenomenon", "Pollution","UnsustainableFish")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),size=0.25,colour="#505050") +
  geom_text(aes(x=3.25,y=.91,label="Treatment",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=2.95,y=.91,label="Control",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=PostCode.trend.PLOTFORMAT$Label[order(PostCode.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ThreatType2"]],
                    labels=c("No Threat", "Aquaculture", "Inadequate Protection", "Tourism",
                             "Natural Phenomenon", "Pollution", "Unsustainable Fishing")) +
  coord_flip() + plot.theme + Trendplot.labs + plot.guides.techreport

# - bahasa
ThreatType.statusplot.bahasa <- 
  melt(ThreatType.status,
       id.vars=c("SettlementName.bahasa"),measure.vars=c("NoThreat", "Aquaculture", "InadequateProc","Tourism","NaturalPhenomenon", "Pollution","UnsustainableFish")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    values=multianswer.fillcols.status[["ThreatType2"]],
                    labels=legend.labs.bahasa[["ThreatType"]]) +
  coord_flip() + Statusplot.labs.bahasa["ThreatType"] + plot.guides.techreport +
  if(MPA.name.filtered$MPAID==21) { plot.theme.manysetts
  } else { plot.theme
  }


ThreatType.trendplot.bahasa <-
  PostCode.trend.PLOTFORMAT %>%
  melt(.,id.vars="order",
       measure.vars=c("NoThreat", "Aquaculture", "InadequateProc","Tourism","NaturalPhenomenon", "Pollution","UnsustainableFish")) %>%
  ggplot(aes(x=factor(order),y=value,fill=variable)) +
  geom_bar(stat="identity",
           position="fill",
           width=0.8,
           size=0.15,
           colour="#505050") +
  geom_vline(aes(xintercept=3),size=0.25,colour="#505050") +
  geom_text(aes(x=3.25,y=.91,label="Perlakuan",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  geom_text(aes(x=2.95,y=.91,label="Kontrol",fontface=2),
            size=rel(3.5),vjust=1,lineheight=0.8,colour="#505050") +
  scale_y_continuous(expand=c(0,0),
                     labels=scales::percent_format()) +
  scale_x_discrete(labels=PostCode.trend.PLOTFORMAT$Label.bahasa[order(PostCode.trend.PLOTFORMAT$order)]) +
  scale_fill_manual(name="",
                    values=multianswer.fillcols.status[["ThreatType2"]],
                    labels=legend.labs.bahasa[["ThreatType"]]) +
  coord_flip() + plot.theme + Trendplot.labs.bahasa + plot.guides.techreport

#
# # - NUMBER ETHNICITIES
# Ethnicity.statusplot <-
#   ggplot(data=PostCodeData.PLOTFORMAT,
#          aes(x=SettlementName)) +
#   geom_bar(aes(y=Num.EthnicGroups,
#                fill="NotDummy"),
#            stat="identity",
#            position="dodge",
#            width=0.75,
#            show.legend=F) +
#   geom_vline(aes(xintercept=3),
#              linetype=2,
#              size=0.35,
#              colour="#505050") +
#   scale_y_continuous(expand=c(0,0),
#                      limits=c(0,max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T) +
#                                 0.03*max(Alor.SBSPropData.Techreport.status.PLOTFORMAT$Num.EthnicGroups,na.rm=T))) +
#   scale_fill_manual(values=fillcols.status) +
#   scale_colour_manual(values=errcols.status) +
#   coord_flip() + Statusplot.labs["Ethnicity"] + plot.theme


# - ADULT EDUCATION
AdultEduc.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars=c("SettlementName"),measure.vars=c("AdultEducHigher", "AdultEducSec", "AdultEducMid",
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
  coord_flip() + Statusplot.labs["AdultEduc"] + plot.guides.techreport +
  if(MPA.name.filtered$MPAID==21) { plot.theme.manysetts
  } else { plot.theme
  }


AdultEduc.statusplot.bahasa <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars=c("SettlementName.bahasa"),measure.vars=c("AdultEducHigher", "AdultEducSec", "AdultEducMid",
                                               "AdultEducPrim", "AdultEducPre", "AdultEducNone")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["AdultEduc"]]) +
  coord_flip() + Statusplot.labs.bahasa["AdultEduc"] + plot.guides.techreport +
  if(MPA.name.filtered$MPAID==21) { plot.theme.manysetts
  } else { plot.theme
  }

# - HOUSEHOLD HEAD EDUCATION
HHHEduc.statusplot <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars=c("SettlementName"),measure.vars=c("HHHEducHigher", "HHHEducSec", "HHHEducMid",
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
  coord_flip() + Statusplot.labs["HHHEduc"] + plot.guides.techreport +
  if(MPA.name.filtered$MPAID==21) { plot.theme.manysetts
  } else { plot.theme
  }


HHHEduc.statusplot.bahasa <-
  melt(PostCodeData.PLOTFORMAT,
       id.vars=c("SettlementName.bahasa"),measure.vars=c("HHHEducHigher", "HHHEducSec", "HHHEducMid",
                                               "HHHEducPrim", "HHHEducPre", "HHHEducNone")) %>%
  ggplot(aes(x=SettlementName.bahasa,y=value,fill=variable)) +
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
                    labels=legend.labs.bahasa[["HHHEduc"]]) +
  coord_flip() + Statusplot.labs.bahasa["HHHEduc"] + plot.guides.techreport +
  if(MPA.name.filtered$MPAID==21) { plot.theme.manysetts
  } else { plot.theme
  }

# 
# # - RULES
# Rules.statusplot <-
#   melt(PostCodeData.PLOTFORMAT,
#        id.vars="SettlementName",measure.vars=c("PropRuleHab", "PropRuleSpp")) %>%
#   ggplot(aes(x=SettlementName,y=value,fill=variable)) +
#   geom_bar(stat="identity",
#            position="dodge",
#            width=0.75,
#            size=0.15,
#            colour="#505050") +
#   geom_vline(aes(xintercept=3),
#              linetype=2,
#              size=0.35,
#              colour="#505050") +
#   scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["PropRules"]],
#                     labels=c("Important species","Important habitats")) +
#   coord_flip() + plot.theme + Statusplot.labs["Rules"] + plot.guides.techreport
# 
# 
# # - PARTICIPATION IN DECISION-MAKING
# Participation.statusplot <-
#   melt(PostCodeData.PLOTFORMAT,
#        id.vars="SettlementName",measure.vars=c("ParticipateRules","ParticipateBnd","ParticipateOrg", "ParticipateEstablish")) %>%
#   filter(., SettlementName!= "Control\nSettlements") %>%
#   ggplot(aes(x=SettlementName,y=value,fill=variable)) +
#   geom_bar(stat="identity",
#            position="dodge",
#            width=0.75,
#            size=0.15,
#            colour="#505050") +
#   geom_vline(aes(xintercept=2),
#              linetype=2,
#              size=0.35,
#              colour="#505050") +
#   scale_y_continuous(expand = c(0, 0), limits=c(0,100)) +
#   scale_fill_manual(name="",
#                     values=multianswer.fillcols.status[["Participate"]],
#                     labels=c("Setting appropriation rules", "MPA boundary delineation", "Design of MPA management body", "Design of MPA-managing organization")) +
#   coord_flip() + plot.theme + Statusplot.labs["Participation"] + plot.guides.techreport



# ---- export ----

# define figure output directory

FigureFileName.english <- 'C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT'

FigureFileName.bahasa <- 'C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT'


# ---- Threat Type ----

png(paste(FigureFileName.english,"ThreatType.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(ThreatType.statusplot)
dev.off()


png(paste(FigureFileName.bahasa,"ThreatType.status.bahasa.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(ThreatType.statusplot.bahasa)
dev.off()

export(ThreatType.status,'C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/ThreatType.bysett.xlsx')

png(paste(FigureFileName.english,"ThreatType.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(ThreatType.trendplot)
dev.off()

png(paste(FigureFileName.bahasa,"ThreatType.trend.bahasa.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(ThreatType.trendplot.bahasa)
dev.off()


# ---- Adult Education ----

png(paste(FigureFileName.english,"AdultEduc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(AdultEduc.statusplot)
dev.off()

png(paste(FigureFileName.bahasa,"AdultEduc.status.bahasa.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(AdultEduc.statusplot.bahasa)
dev.off()

export(PostCodeData.PLOTFORMAT,'C:/Users/Matheus DeNardo/Dropbox (MPAMystery)/CU37_Kadek_MPA_Social_Reports_Consultancy/THREAT/PostCode_Education_statusplot_data.xlsx')


# ---- Head of Household Education ----

png(paste(FigureFileName.english,"HHHEduc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(HHHEduc.statusplot)
dev.off()

png(paste(FigureFileName.bahasa,"HHHEduc.status.bahasa.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(HHHEduc.statusplot.bahasa)
dev.off()



