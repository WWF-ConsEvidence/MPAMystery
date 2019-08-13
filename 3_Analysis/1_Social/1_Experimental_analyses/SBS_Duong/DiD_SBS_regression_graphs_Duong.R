
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##

####---0. Sourcing and creating data frame for status/trend and impact
####---0. Sourcing and creating data frame for status/trend and impact
####---0. Sourcing and creating data frame for status/trend and impact
####---0. Sourcing and creating data frame for status/trend and impact

source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
outPath.TrendPlots <- "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/"

library(lfe) #regressions
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)
library(tidyverse)
library(qvalue)
library(psych) #summary statistics by groups


# --- DiD specification 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear)) %>% 
  filter(!is.na(IndividualGender))

# --- summary(DiD.data)
DiD.data.summary <- DiD.data %>% 
  select(MPAID,MonitoringYear,InterviewYear, yearsPost, MPAName) %>% 
  group_by(MPAID,MonitoringYear) %>%
  summarise(yearsPost = mean(yearsPost),InterviewYear = first(InterviewYear), MPAName=first(MPAName))  

# --- Filter the 2 SBS MPAs with t0 and t3 (Selat Pantar - MPA==15; Flores Timur - MPA==16)
DiD.data <- DiD.data %>% 
  filter(MPAID==15|MPAID==16)
                                        

# calculate Z scores (standardized values for each of the Big Five)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}


# calculate p scores
pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  


##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##

####---1. Finding group means for trend and summary statistics tables
####---1. Finding group means for trend and summary statistics tables
####---1. Finding group means for trend and summary statistics tables
####---1. Finding group means for trend and summary statistics tables


#Seascape-wide stats
sumStat.BigFive.Seascape <- DiD.data %>% 
  select(Treatment, MPAID, yearsPostF,FSIndex,MAIndex,MTIndex,PAIndex,SERate, FSIndex_z,MAIndex_z,MTIndex_z,PAIndex_z,SERate_z) %>% 
  group_by(Treatment, yearsPostF) %>% 
  summarise_at(vars(FSIndex:SERate_z), .funs = list(~ mean(., na.rm = TRUE),~ sd(., na.rm = TRUE), ~ median(., na.rm = TRUE))) %>% 
  mutate(MPAID=0,
         MPAID=as.factor(MPAID)) #assign 0 for mpaid for seascapewide; to merge with mpa-specific sumstat later


#MPA-specific stats
sumStat.BigFive <- DiD.data %>% 
  select(Treatment, MPAID, yearsPostF,FSIndex,MAIndex,MTIndex,PAIndex,SERate, FSIndex_z,MAIndex_z,MTIndex_z,PAIndex_z,SERate_z) %>% 
  group_by(MPAID,Treatment, yearsPostF) %>% 
  summarise_at(vars(FSIndex:SERate_z), .funs = list(~ mean(., na.rm = TRUE),~ sd(., na.rm = TRUE), ~ median(., na.rm = TRUE))) %>% 
  rbind(.,sumStat.BigFive.Seascape) %>% #merge seascape stats (mpaid==0) into this table
  mutate(MPAName_short = ifelse(MPAID==15,"Selat Pantar",
                                ifelse(MPAID==16,"Flores Timur",
                                      ifelse(MPAID==0,"Seascape",""))))

summary(sumStat.BigFive)



mySum <- DiD.data %>% 
  my_summarize(FSIndex,MPAID,Treatment,yearsPostF)

## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Seascape","Selat Pantar", "Flores Timur")

pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPAID to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  mpa.status.data <- sumStat.BigFive %>% 
    filter(MPAName_short==mpa) %>% 
    ungroup() %>% 
    mutate(Group=ifelse(Treatment==1,"Treatment","Control"))
  
  MT.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=MTIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + 
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MTIndex_mean - MTIndex_sd, ymax=MTIndex_mean + MTIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Marine Tenure Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/",mpa,"_MT.jpg"),width = 12, height = 6)
  
  
  MA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=MAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MAIndex_mean - MAIndex_sd, ymax=MAIndex_mean + MAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Material Assets Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/",mpa,"_MA.jpg"),width = 12, height = 6)
  
  
  PA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=PAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=PAIndex_mean - PAIndex_sd, ymax=PAIndex_mean + PAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Place Attachment Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/",mpa,"_PA.jpg"),width = 12, height = 6)
  
  
  FS.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=FSIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=FSIndex_mean - FSIndex_sd, ymax=FSIndex_mean + FSIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Food Security Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/",mpa,"_FS.jpg"),width = 12, height = 6)
  
  
  SE.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=SERate_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=SERate_mean - SERate_sd, ymax=SERate_mean + SERate_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: School Enrollment Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/",mpa,"_SE.jpg"),width = 12, height = 6)
  
}




##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##-----------Repeat the loop, removing title and legend to combine plots later-----------------------------------##
## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Seascape","Selat Pantar", "Flores Timur")

pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPAID to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  mpa.status.data <- sumStat.BigFive %>% 
    filter(MPAName_short==mpa) %>% 
    ungroup() %>% 
    mutate(Group=ifelse(Treatment==1,"Treatment","Control"))
  
  MT.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=MTIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MTIndex_mean - MTIndex_sd, ymax=MTIndex_mean + MTIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Marine Tenure")  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  
  
  MA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=MAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw()+ theme(legend.position="none") + 
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MAIndex_mean - MAIndex_sd, ymax=MAIndex_mean + MAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Material Assets")  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  
  
  PA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=PAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw()+ theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=PAIndex_mean - PAIndex_sd, ymax=PAIndex_mean + PAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Place Attachment")  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  
  
  FS.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=FSIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw()+ theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=FSIndex_mean - FSIndex_sd, ymax=FSIndex_mean + FSIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Food Security")  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  
  
  SE.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=SERate_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=SERate_mean - SERate_sd, ymax=SERate_mean + SERate_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="School Enrollment")  +
    scale_colour_manual(values = c("grey93", "royalblue4")) +
    scale_fill_manual(values = c("grey93", "royalblue4")) 
  
  
  ########################Combine BigFive MPAlevel graphs
  comb.plot <-  plot_grid(FS.status.plot,MT.status.plot,PA.status.plot,SE.status.plot,MA.status.plot,ncol=3)
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/SBS/",mpa,"_BigFive.png"),width = 12, height = 6)
  
}







##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##

##2. DiD Impact Plots -- for MACP reports (i.e. show trends for COntrol vs Treatment)
##2. DiD Impact Plots -- for MACP reports (i.e. show trends for COntrol vs Treatment)
##2. DiD Impact Plots -- for MACP reports (i.e. show trends for COntrol vs Treatment)
##2. DiD Impact Plots -- for MACP reports (i.e. show trends for COntrol vs Treatment)

##-------Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)----##
##-------Seascape level impacts-----------------------------------------------## 
##--------------------NOTE--------------------------##
##-------Exclude time and settlement FEs in order to plot trends--------------##
###generating short MPA names
mpa.nam <- mpa.nam %>% 
  mutate(MPAName_short = ifelse(MPACode==1,"  Telma",
                                ifelse(MPACode==2,"  TNTC",
                                       ifelse(MPACode==3," Kaimana",
                                              ifelse(MPACode==4," Kofiau",
                                                     ifelse(MPACode==5,"Dampier",
                                                            ifelse(MPACode==6,"Misool",
                                                                   ifelse(MPACode==15,"Selat Pantar",
                                                                          ifelse(MPACode==16,"Flores Timur","")))))))))

    
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out.MACP.impact <- data.frame()


for (mpaid in 15:16) {
  DiD.MACP.impact <- DiD.data %>% filter(MPAID==mpaid)
  print(mpaid)
  
    for (i in varNames) {
    print(i)
    
    Y <- DiD.MACP.impact[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                     | MPAID | 0 | SettlementID,
                     data=DiD.MACP.impact,exactDOF = TRUE)
    summary(regValue)
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid)
    
    model.out.MACP.impact <- rbind(model.out.MACP.impact,reg.broom)
  }
}


##keeping only 2 terms yearsPostF3 (i.e. time trend) and Treatment:yearsPostF3 (i.e. DiD impact)
model.out.MACP.impact1 <- model.out.MACP.impact %>% 
  filter(term%in%c("yearsPostF3", "Treatment:yearsPostF3")) %>% 
  mutate(term=gsub("Treatment:yearsPostF3","Impact",term)) %>% 
  mutate(term=gsub("yearsPostF3","Control_Trend",term))  

## spead dataframe to compute treatment trend (=control trend + Impact)
model.out.MACP.impact1.rearrange<-model.out.MACP.impact1 %>% 
  select(Response,MPAID,term,estimate) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = Control_Trend + Impact) %>% 
  select(Response, MPAID, estimate) %>% 
  mutate(term="Treatment_Trend",std.error=0,statistic=0,p.value=0)

model.out.MACP.impact1<-rbind(model.out.MACP.impact1,model.out.MACP.impact1.rearrange)

model.out.MACP.impact1 <- model.out.MACP.impact1 %>% 
  mutate(std.error.toUse=ifelse(term=="Control_Trend",0,std.error)) %>% 
  mutate(Group=ifelse(term=="Control_Trend"," Control",
                       ifelse(term=="Treatment_Trend"," Treatment","Impact"))) %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"  Telma",
                                ifelse(MPAID==2,"  TNTC",
                                       ifelse(MPAID==3," Kaimana",
                                              ifelse(MPAID==4," Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==15,"Selat Pantar",
                                                                          ifelse(MPAID==16,"Flores Timur","")))))))))


model.out.MACP.impact2 <- model.out.MACP.impact1 %>% 
  filter(Group%in%c(" Control", " Treatment")) 
  
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Approach 1------------------------------------------------------------##
##----------Approach 1------------------------------------------------------------##
##----------Approach 1------------------------------------------------------------##
## Produce impact plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Selat Pantar", "Flores Timur")
pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPA to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  mpa.impact.data <- model.out.MACP.impact1 %>% 
    filter(MPAName_short==mpa) %>% 
    ungroup() 
  
  MT.impact.plot <- ggplot(filter(mpa.impact.data,Response=="MTIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() + 
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Marine Tenure Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 1/",mpa,"_MT.jpg"),width = 12, height = 6)
  
 
  MA.impact.plot <- ggplot(filter(mpa.impact.data,Response=="MAIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Material Assets Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 1/",mpa,"_MA.jpg"),width = 12, height = 6)
  
  
  PA.impact.plot <- ggplot(filter(mpa.impact.data,Response=="PAIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Place Attachment Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 1/",mpa,"_PA.jpg"),width = 12, height = 6)
  
  
  FS.impact.plot <- ggplot(filter(mpa.impact.data,Response=="FSIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Food Security Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 1/",mpa,"_FS.jpg"),width = 12, height = 6)
  
  
  SE.impact.plot <- ggplot(filter(mpa.impact.data,Response=="SERate"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: School Enrollment Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 1/",mpa,"_SE.jpg"),width = 12, height = 6)
  
}



##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Approach 2------------------------------------------------------------##
##----------Approach 2------------------------------------------------------------##
##----------Approach 2------------------------------------------------------------##
## Produce impact plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Selat Pantar", "Flores Timur")
pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPA to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  mpa.impact.data <- model.out.MACP.impact2 %>% 
    filter(MPAName_short==mpa) %>% 
    ungroup() 
  
  MT.impact.plot <- ggplot(filter(mpa.impact.data,Response=="MTIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() + 
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Marine Tenure Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 2/",mpa,"_MT.jpg"),width = 12, height = 6)
  
  
  MA.impact.plot <- ggplot(filter(mpa.impact.data,Response=="MAIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Material Assets Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 2/",mpa,"_MA.jpg"),width = 12, height = 6)
  
  
  PA.impact.plot <- ggplot(filter(mpa.impact.data,Response=="PAIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Place Attachment Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 2/",mpa,"_PA.jpg"),width = 12, height = 6)
  
  
  FS.impact.plot <- ggplot(filter(mpa.impact.data,Response=="FSIndex"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: Food Security Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 2/",mpa,"_FS.jpg"),width = 12, height = 6)
  
  
  SE.impact.plot <- ggplot(filter(mpa.impact.data,Response=="SERate"),aes(x=Group,y=estimate, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black") + theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=16,face="bold")) +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=estimate - std.error.toUse, ymax=estimate + std.error.toUse), width=0.2, position = position_dodge(.7), linetype = "longdash") +
    labs(x=" Three Year Post-Baseline",y="Change Since Baseline", title=paste0(mpa, " Impact Plot: School Enrollment Index"))  +
    scale_colour_manual(values = c("grey93", "royalblue4","white")) +
    scale_fill_manual(values = c("grey93", "royalblue4","white")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Impact_MACP_result/SBS/approach 2/",mpa,"_SE.jpg"),width = 12, height = 6)
  
}



##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##

## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Selat Pantar", "Flores Timur")

pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPAID to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  mpa.impact.data <- model.out.mpalevel1 %>%
    mutate(MPAName_short=MPAName) %>% 
    filter(MPAName_short==mpa) %>% 
    ungroup() 
  
  FS.plot <- ggplot(filter(mpa.impact.data,Response=="FSIndex"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Food Security")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_FS_",mpa,".jpg"),width = 12, height = 6)
  
  MT.plot <- ggplot(filter(mpa.impact.data,Response=="MTIndex"),aes(x=term,y=estimate)) +  theme(legend.position="none") +
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Marine Tenure")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_MT_",mpa,".jpg"),width = 12, height = 6)
  
  
  MA.plot <- ggplot(filter(mpa.impact.data,Response=="MAIndex"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() +  theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Material Assets")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_MA_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  PA.plot <- ggplot(filter(mpa.impact.data,Response=="PAIndex"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Place Attachment") 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_PA_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  SE.plot <- ggplot(filter(mpa.impact.data,Response=="SERate"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="School Enrollment")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_SE_",mpa,".jpg"),width = 12, height = 6)
  
  
  #library(cowplot)
  #Combine "regular BigFive"
  plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_BigFive_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  #####################Repeat the 5 plots, now using standardized scores
  
  
  FS.plot <- ggplot(filter(mpa.impact.data,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Food Security") 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_FS_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  MT.plot <- ggplot(filter(mpa.impact.data,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Marine Tenure")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_MT_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  MA.plot <- ggplot(filter(mpa.impact.data,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Material Assets")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_MA_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  PA.plot <- ggplot(filter(mpa.impact.data,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Place Attachment")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_PA_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  SE.plot <- ggplot(filter(mpa.impact.data,Response=="SERate_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="School Enrollment")  
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_SE_z_",mpa,".jpg"),width = 12, height = 6)
  
  #Combine "standardize BigFive"
  plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/",mpa,"/DiD_BigFive_z_",mpa,".jpg"),width = 12, height = 6)
  
}






##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##

##3. Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
##3. Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
##3. Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
##3. Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
##3. Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
##3. Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 

##################################################################################
##################################################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
##################################################################################
##################################################################################
##################################################################################
##-------Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)----##
##-------Seascape level impacts-----------------------------------------------## 

varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out <- data.frame()

for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                   | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                   data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i)
  
  model.out <- rbind(model.out,reg.broom)
}


###########################
#####BigFive's Impact plots
###########################

##keeping only 2 terms yearsPostF3 (i.e. time trend) and Treatment:yearsPostF3 (i.e. DiD impact)
model.out1 <- model.out %>% 
  filter(term%in%c("Treatment:yearsPostF3")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term))

##-------------PLOTS---------------------------##
##-------------PLOTS---------------------------##
##-------------PLOTS---------------------------##
##-------------PLOTS---------------------------##
pd <- position_dodge(width=.3) # move them .05 to the left and right


FS.plot <- ggplot(filter(model.out1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out1,Response=="MTIndex"),aes(x=term,y=estimate)) +  theme(legend.position="none") +
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() +  theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out1,Response=="SERate"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="School Enrollment")  


#library(cowplot)
#Combine "regular BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_BigFive_seascape.jpg"),width = 12, height = 6)



#####################Repeat the 5 plots, now using standardized scores


FS.plot <- ggplot(filter(model.out1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="Impact estimate", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_BigFive_z_seascape.jpg"),width = 12, height = 6)







##################################################################################
##################################################################################
###MPA-level Impact############################################################
###MPA-level Impact############################################################
###MPA-level Impact############################################################
###MPA-level Impact############################################################
##################################################################################
##################################################################################
##################################################################################
##############
##############Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)

###generating short MPA names
mpa.nam <- mpa.nam %>% 
  mutate(MPAName_short = ifelse(MPACode==1,"  Telma",
                                ifelse(MPACode==2,"  TNTC",
                                       ifelse(MPACode==3," Kaimana",
                                              ifelse(MPACode==4," Kofiau",
                                                     ifelse(MPACode==5,"Dampier",
                                                            ifelse(MPACode==6,"Misool",
                                                                   ifelse(MPACode==15,"Selat Pantar",
                                                                          ifelse(MPACode==16,"Flores Timur","")))))))))

varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out.mpalevel <- data.frame()

for (i in varNames) {
  for (mpaid in 15:16) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                     | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                     data=DiD.data,exactDOF = TRUE)
    
    reg.broom <- tidy(regValue) %>%
       mutate(Response=i, MPAID=mpaid)
    model.out.mpalevel <- rbind(model.out.mpalevel,reg.broom)
  }
}

###########################
#####BigFive's Impact plots
###########################
model.out.mpalevel1 <- model.out.mpalevel %>% 
  filter(term%in%c("Treatment:yearsPostF3")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  left_join(mpa.nam) %>% 
  mutate(MPAName=gsub(" MPA","",MPAName),
         MPAName=gsub("Teluk ","",MPAName))


###edit model.out1 (with seascape level reg values; ready to merge/include to model.out.mpalevel1)
model.out1$MPAID <-0
model.out1$MPAName <-" Seascape"


model.out.mpalevel1 <- model.out.mpalevel1 %>% 
  select(term, estimate, std.error, statistic, p.value, Response, MPAID, MPAName)

model.out.mpalevel1 <- rbind(model.out.mpalevel1,model.out1)
  
summary(model.out.mpalevel1)
head(mpa.nam)


pd <- position_dodge(width=.3) # move them .05 to the left and right


############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))
#+ facet_grid(.~Response)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot <- ggplot(filter(model.out.mpalevel1,Response=="SERate"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_SE_MPAlevel.jpg"),width = 12, height = 6)






############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))
FS.plot

ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_z_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_z_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) +   
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_z_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_z_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="SERate_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_z_SE_MPAlevel.jpg"),width = 12, height = 6)








###################Combine all Five individual MPA plots; repeat to remove legends (save space) 
############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 


PA.plot <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))


MA.plot <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))


SE.plot <- ggplot(filter(model.out.mpalevel1,Response=="SERate"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))

########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_BigFive_MPAlevel.jpg"),width = 12, height = 6)





############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Food Security")  +
  scale_colour_manual(values = c("black", "blue"))


MT.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("black", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) +   
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("black", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="Material Assets")  +
  scale_colour_manual(values = c("black", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="SERate_z"),aes(x=MPAName,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="Impact estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("black", "blue"))



########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,MA.plot_z,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/SBS/main_DD/","DiD_z_BigFive_MPAlevel.jpg"),width = 12, height = 6)








