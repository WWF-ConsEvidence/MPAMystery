#.libPaths()
#.libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")

####---DID/DIDID regression analysis for BHS---########
####---Outcomes: Big Fives---------------########
####---1. Seascape-level impacts
####---2. Individual MPA-level impacts
####---3. Finding group means for trend and summary statistics tables

####---4. SubGroup Heterogeneity (Triple Difference; Seascape-level)
####------4.1 Fisher vs. non-Fisher
####------4.2 Male vs. Female
####------4.3 Dominant vs. non-dominant ethnic groups
####------4.4 By wealth quintiles


source('2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")
outPath <- "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/"

library(lfe) #regressions
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)
library(tidyverse)
library(qvalue)
library(psych) #summary statistics by groups

#install.packages(qvalue)

# #------------Installing qvalue package
# library(devtools)
# install_github("jdstorey/qvalue")
# #------------Installing qvalue package
# 

##Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 
## Seascape level Impact
## Seascape level Impact
## Seascape level Impact
## Seascape level Impact
## Seascape level Impact

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


# test <- DiD.data %>% 
#   group_by(MPAID, yearsPostF) %>% 
#   summarise(InterviewYear=first(InterviewYear))
# test


# calculate Z scores (standardized values for each of the Big Five)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  

###############
###############using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out <- data.frame()

regValue.list <-list()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                   | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                   data=DiD.data,exactDOF = TRUE)
  
  regValue.list[[i]] <- regValue
  
  # # summary(regValue)
  #  reg.broom <- tidy(regValue) %>% 
  #    mutate(Response=i)
  #  model.out <- rbind(model.out,reg.broom)
}

summary(regValue.list)
head(regValue.list)

#########produce publishable tables
##save texts
stargazer(regValue.list$FSIndex, regValue.list$PAIndex, regValue.list$MAIndex,regValue.list$MTIndex, regValue.list$SERate,
          out = "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/result_tables/DiD-seascape.txt", type = "text",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))

  
##save htmls
stargazer(regValue.list$FSIndex, regValue.list$PAIndex, regValue.list$MAIndex,regValue.list$MTIndex, regValue.list$SERate,
          out = "R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/result_tables/DiD-seascape.html", type = "html",
          keep = c("Treatment:yearsPostF2", "Treatment:yearsPostF4"), covariate.labels=c("Treatment X t2","Treatment X t4"),
          title="DiD Regression Results",
          align=TRUE, column.labels=names(regValue.list)[1:5], dep.var.labels=" ",
          add.lines = list(c("Settlement FE",rep("Yes",5)), c("Year FE",rep("Yes",5)),c("MPAxYear FE",rep("Yes",5))))




##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##

####---3. Finding group means for trend and summary statistics tables
####---3. Finding group means for trend and summary statistics tables
####---3. Finding group means for trend and summary statistics tables
####---3. Finding group means for trend and summary statistics tables

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
  mutate(MPAName_short = ifelse(MPAID==1,"Telma",
                                ifelse(MPAID==2,"TNTC",
                                       ifelse(MPAID==3,"Kaimana",
                                              ifelse(MPAID==4,"Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==0,"Seascape",""))))))))

summary(sumStat.BigFive)

## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Seascape","Telma","TNTC","Kaimana","Kofiau","Dampier","Misool")

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
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MTIndex_mean - MTIndex_sd, ymax=MTIndex_mean + MTIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Marine Tenure Index"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/",mpa,"_MT.jpg"),width = 12, height = 6)
  
  
  MA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=MAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MAIndex_mean - MAIndex_sd, ymax=MAIndex_mean + MAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Material Assets Index"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/",mpa,"_MA.jpg"),width = 12, height = 6)
  
  
  PA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=PAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=PAIndex_mean - PAIndex_sd, ymax=PAIndex_mean + PAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Place Attachment Index"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/",mpa,"_PA.jpg"),width = 12, height = 6)
  
  
  FS.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=FSIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=FSIndex_mean - FSIndex_sd, ymax=FSIndex_mean + FSIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: Food Security Index"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/",mpa,"_FS.jpg"),width = 12, height = 6)
  

  SE.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=SERate_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=SERate_mean - SERate_sd, ymax=SERate_mean + SERate_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title=paste0(mpa, " Status Plot: School Enrollment Index"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/",mpa,"_SE.jpg"),width = 12, height = 6)

}




##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##-----------Repeat the loop, removing title and legend to combine plots later-----------------------------------##
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
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 

  
  MA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=MAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw()+ theme(legend.position="none") + 
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=MAIndex_mean - MAIndex_sd, ymax=MAIndex_mean + MAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Material Assets")  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 

  
  PA.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=PAIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw()+ theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=PAIndex_mean - PAIndex_sd, ymax=PAIndex_mean + PAIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Place Attachment")  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 

  
  FS.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=FSIndex_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw()+ theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=FSIndex_mean - FSIndex_sd, ymax=FSIndex_mean + FSIndex_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="Food Security")  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 

  
  SE.status.plot <- ggplot(mpa.status.data,aes(x=yearsPostF,y=SERate_mean, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=SERate_mean - SERate_sd, ymax=SERate_mean + SERate_sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Index Value", title="School Enrollment")  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 

  
  ########################Combine BigFive MPAlevel graphs
 comb.plot <-  plot_grid(FS.status.plot,MT.status.plot,PA.status.plot,SE.status.plot,MA.status.plot,ncol=3)
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/",mpa,"_BigFive.png"),width = 12, height = 6)
  
}


####----------------------------------------------------------------------------------------------###
####----------------------------------------------------------------------------------------------###
####----------------------------------------------------------------------------------------------###
####----------------------------------------------------------------------------------------------###
####-------------------Find and plot time-difference----------------------------------------------###
####-------------------Find and plot time-difference----------------------------------------------###

###Temporal Difference for seascape
sumStat.BigFive.diff.seascape <- DiD.data %>% 
  gather(key=outcome, value=value, MAIndex:SERate, -MPAID) %>% 
  group_by(SettlementID,Treatment,MonitoringYear,outcome) %>% 
  summarise(value=mean(value,na.rm=T)) %>% 
  spread(MonitoringYear,value) %>% 
  mutate(t2=`2 Year Post`-Baseline,
         t4=`4 Year Post`-Baseline) %>% 
  select(Treatment:outcome,t2,t4) %>% 
  gather(key=YearsPostF, value=value,t2:t4) %>% 
  mutate(YearsPostF=paste0(YearsPostF,".0"))

sumStat.BigFive.diff.seascape <- sumStat.BigFive.diff.seascape %>% 
  group_by(Treatment,YearsPostF,outcome) %>% 
  summarise(outcome.diff=mean(value,na.rm=T),
            outcome.diff.sd=mean(replicate(1000, sd(sample(value, replace=T),na.rm=T)/sqrt(length(value))),na.rm=T)) %>% 
  mutate(MPAName_short = "Seascape", MPAID=0, MPAID=as.factor(MPAID))
                                
###Temporal Difference for MPAs
sumStat.BigFive.diff <- DiD.data %>% 
  gather(key=outcome, value=value, MAIndex:SERate, -MPAID) %>% 
  group_by(MPAID,SettlementID,Treatment,MonitoringYear,outcome) %>% 
  summarise(value=mean(value,na.rm=T)) %>% 
  spread(MonitoringYear,value) %>% 
  mutate(t2=`2 Year Post`-Baseline,
         t4=`4 Year Post`-Baseline) %>% 
  select(MPAID:outcome,t2,t4) %>% 
  gather(key=YearsPostF, value=value,t2:t4) %>% 
  mutate(YearsPostF=paste0(YearsPostF,".0"))

sumStat.BigFive.diff <- sumStat.BigFive.diff %>% 
  group_by(MPAID,Treatment,YearsPostF,outcome) %>% 
  summarise(outcome.diff=mean(value,na.rm=T),
            outcome.diff.sd=mean(replicate(1000, sd(sample(value, replace=T),na.rm=T)/sqrt(length(value))),na.rm=T)) %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"Telma",
                                ifelse(MPAID==2,"TNTC",
                                       ifelse(MPAID==3,"Kaimana",
                                              ifelse(MPAID==4,"Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==0,"Seascape","")))))))) %>% 
  rbind(.,sumStat.BigFive.diff.seascape)

summary(sumStat.BigFive.diff)

## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Seascape","Telma","TNTC","Kaimana","Kofiau","Dampier","Misool")
#varName <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate")

pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPAID to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  
  mpa.trend.data <- sumStat.BigFive.diff %>% 
      filter(MPAName_short==mpa) %>% 
      ungroup() %>% 
      mutate(Group=ifelse(Treatment==1,"Treatment","Control"))
  
  FS.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="FSIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) +
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) +
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0(mpa, " Temporal Differences: Food Security"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray"))
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_FS.jpg"),width = 12, height = 6)

  MA.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="MAIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0(mpa, " Temporal Differences: Material Assets"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_MA.jpg"),width = 12, height = 6)
  
  MT.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="MTIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0(mpa, " Temporal Differences: Marine Tenure"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_MT.jpg"),width = 12, height = 6)
  
  PA.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="PAIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0(mpa, " Temporal Differences: Place Attachment"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_PA.jpg"),width = 12, height = 6)
  
  SE.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="SERate"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0(mpa, " Temporal Differences: School Enrollment"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_SE.jpg"),width = 12, height = 6)
}









## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Seascape","Telma","TNTC","Kaimana","Kofiau","Dampier","Misool")
#varName <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate")

pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPAID to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  
  mpa.trend.data <- sumStat.BigFive.diff %>% 
    filter(MPAName_short==mpa) %>% 
    ungroup() %>% 
    mutate(Group=ifelse(Treatment==1,"Treatment","Control"))
  
  FS.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="FSIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) +
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) +
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0("Food Security"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray"))
  #ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_FS.jpg"),width = 12, height = 6)
  
  MA.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="MAIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0("Material Assets"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  #ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_MA.jpg"),width = 12, height = 6)
  
  MT.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="MTIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0("Marine Tenure"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  #ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_MT.jpg"),width = 12, height = 6)
  
  PA.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="PAIndex"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0("Place Attachment"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  #ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_PA.jpg"),width = 12, height = 6)
  
  SE.trend.plot <- ggplot(mpa.trend.data %>% filter(outcome=="SERate"), aes(x=YearsPostF,y=outcome.diff, fill=Group)) + 
    geom_bar(stat="identity", position=position_dodge(), width=0.6, color="black") + theme_bw() + theme(legend.position="none") +
    geom_line(position = pd) + 
    geom_errorbar(aes(ymin=outcome.diff - outcome.diff.sd, ymax=outcome.diff + outcome.diff.sd), width=0.2, position = position_dodge(.6), linetype = "longdash") +
    labs(x="Years since Baseline",y="Difference", title=paste0("School Enrollment"))  +
    scale_colour_manual(values = c("white", "darkgray")) +
    scale_fill_manual(values = c("white", "darkgray")) 
  #ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_SE.jpg"),width = 12, height = 6)

  
  ########################Combine BigFive MPAlevel graphs
  comb.plot <-  plot_grid(FS.trend.plot,MT.trend.plot,PA.trend.plot,SE.trend.plot,MA.trend.plot,ncol=3)
  ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/Trend_result/BHS/Trend/",mpa,"_BigFive.png"),width = 12, height = 6)
  
  }
