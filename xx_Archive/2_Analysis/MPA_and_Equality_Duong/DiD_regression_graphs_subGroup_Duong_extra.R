##-------------------------------------------
##REPEAT EVERYTHING AS IN DiD_regression_graphs_subGroup_Duong.R to generate DiDiD impact plots
##ONLY CHANGE: instead of 90% CI band, now show SE band 

# .libPaths()
# .libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")
library(lfe)
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)
library(rio)
library(dplyr)


##Code Difference-in-difference Analysis for social impacts of BHS/SBS MPAs 

source('R:/Gill/DLe/MPAMystery/2_Functions/2_Analysis/Function_process_covariates.R')
mpa.nam <- rio::import("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Inputs/HH_tbl_MPA.xlsx")


# --- DiD specification 
DiD.data <- match.covariate %>% 
  left_join(select(HHData,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear, PrimaryLivelihood:TertiaryLivelihood), by="HouseholdID") %>% 
  left_join(mpa.nam,by="MPAID") %>% 
  select(HouseholdID:InterviewYear,MPAName, PrimaryLivelihood:TertiaryLivelihood) %>% 
  filter(MPAID<=6) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear)) %>% 
  filter(!is.na(IndividualGender))

#summary(DiD.data)

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

summary(DiD.data)

DiD.data <- DiD.data %>% 
  mutate(Male=ifelse(IndividualGender==1,1,0),
         ethDom=dom.eth,
         Fisher=ifelse(PrimaryLivelihood==1 | SecondaryLivelihood==1 | TertiaryLivelihood==1,1,0)) 
DiD.data <- DiD.data %>% 
  mutate(Fisher=ifelse(is.na(Fisher),0,Fisher))


varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

##----Differential impact by gender
##----Differential impact by gender
##----Differential impact by gender
##----Differential impact by gender
##----Differential impact by gender
##----Differential impact by gender
##################################################################################
##################################################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
##################################################################################
##################################################################################
##################################################################################
##############
##############Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
##############Seascape level impacts
model.out.gender <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue <- felm(Y  ~  n.child  + ed.level  + dom.eth + YearsResident + IndividualGender + IndividualAge + 
                     Treatment + yearsPostF + Treatment:yearsPostF + Male:Treatment + Male:yearsPostF + Male:Treatment:yearsPostF 
                   | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                   data=DiD.data,exactDOF = TRUE)
  summary(regValue)
  reg.broom <- tidy(regValue) %>% 
    mutate(Response=i)
  
  model.out.gender <- rbind(model.out.gender,reg.broom)
}


##########################
#####BigFive's Impact plots
###########################

###Rename Treatment:yearsPostF2:Male into t2 and so on
model.out.gender1 <- model.out.gender %>% 
  filter(term%in%c("Treatment:yearsPostF2:Male","Treatment:yearsPostF4:Male")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  filter(term%in%c("t2:Male","t4:Male")) %>% 
  mutate(term=gsub(":Male","",term))


pd <- position_dodge(width=.3) # move them .05 to the left and right


FS.plot <- ggplot(filter(model.out.gender1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.gender1,Response=="MTIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.gender1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.gender1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.gender1,Response=="SERate"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  


#library(cowplot)
#Combine "regular BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_BigFive_seascape.jpg"),width = 12, height = 6)


#####################Repeat the 5 plots, now using standardized scores

FS.plot <- ggplot(filter(model.out.gender1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.gender1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.gender1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.gender1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.gender1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_BigFive_z_seascape.jpg"),width = 12, height = 6)




##################################################################################
##################################################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
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
                                                            ifelse(MPACode==6,"Misool","")))))))

varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out.mpalevel.gender <- data.frame()

for (i in varNames) {
  for (mpaid in 1:6) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level  + dom.eth + YearsResident + IndividualGender + IndividualAge + 
                       Treatment + yearsPostF + Treatment:yearsPostF + Male:Treatment + Male:yearsPostF + Male:Treatment:yearsPostF 
                     | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                     data=DiD.data.mpalevel,exactDOF = TRUE)
    
    
    summary(regValue)
    
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid)
    
    model.out.mpalevel.gender <- rbind(model.out.mpalevel.gender,reg.broom)
  }
}




###########################
#####BigFive's Impact plots
###########################
###Rename Treatment:yearsPostF2:Male into t2 and so on
model.out.mpalevel.gender1 <- model.out.mpalevel.gender %>% 
  filter(term%in%c("Treatment:yearsPostF2:Male","Treatment:yearsPostF4:Male")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  filter(term%in%c("t2:Male","t4:Male")) %>% 
  mutate(term=gsub(":Male","",term)) %>% 
  left_join(mpa.nam) %>% 
  mutate(MPAName=gsub(" MPA","",MPAName),
         MPAName=gsub("Teluk ","",MPAName))

###Select only needed variables
model.out.mpalevel.gender1 <- model.out.mpalevel.gender1 %>% 
  select(term,estimate,std.error,statistic,p.value,Response,MPAID,MPAName,MPAName_short)


###edit model.out.gender1 (with seascape level reg values; ready to merge/include to model.out.mpalevel.gender1)
model.out.gender1$MPAID <-0
model.out.gender1$MPAName <-"Bird's head Seascape"
model.out.gender1$MPAName_short <-"  BHS"

model.out.mpalevel.gender1 <- rbind(model.out.mpalevel.gender1,model.out.gender1)

pd <- position_dodge(width=.3) # move them .05 to the left and right


############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_SE_MPAlevel.jpg"),width = 12, height = 6)






############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
FS.plot

ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_z_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_z_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_z_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_z_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_z_SE_MPAlevel.jpg"),width = 12, height = 6)






###################Combine all Five individual MPA plots; repeat to remove legends (save space) 
############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot <- ggplot(filter(model.out.mpalevel.gender1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))

########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_BigFive_MPAlevel.jpg"),width = 12, height = 6)





############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MT.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel.gender1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))



########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,MA.plot_z,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/gender/stderr_band/","DDD_gender_z_BigFive_MPAlevel.jpg"),width = 12, height = 6)



###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


##----Differential impact by Fisher vs non-Fisher
##----Differential impact by Fisher vs non-Fisher
##----Differential impact by Fisher vs non-Fisher
##----Differential impact by Fisher vs non-Fisher


##################################################################################
##################################################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
##################################################################################
##################################################################################
##################################################################################
##############
##############Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
##############Seascape level impacts
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")

model.out.fisher <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue.fisher <- felm(Y  ~  n.child  + ed.level  + dom.eth + YearsResident + IndividualGender + IndividualAge +
                            Treatment + yearsPostF + Treatment:yearsPostF + Fisher:Treatment + Fisher:yearsPostF + Fisher:Treatment:yearsPostF
                          | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                          data=DiD.data,exactDOF = TRUE)
  summary(regValue.fisher)
  reg.broom <- tidy(regValue.fisher) %>%
    mutate(Response=i)
  
  model.out.fisher <- rbind(model.out.fisher,reg.broom)
}

##########################
#####BigFive's Impact plots
###########################

###Rename Treatment:yearsPostF2:Fisher into t2 and so on
model.out.fisher1 <- model.out.fisher %>% 
  filter(term%in%c("Treatment:yearsPostF2:Fisher","Treatment:yearsPostF4:Fisher")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  filter(term%in%c("t2:Fisher","t4:Fisher")) %>% 
  mutate(term=gsub(":Fisher","",term))


pd <- position_dodge(width=.3) # move them .05 to the left and right


FS.plot <- ggplot(filter(model.out.fisher1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.fisher1,Response=="MTIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.fisher1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.fisher1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.fisher1,Response=="SERate"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  


#library(cowplot)
#Combine "regular BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_BigFive_seascape.jpg"),width = 12, height = 6)


#####################Repeat the 5 plots, now using standardized scores

FS.plot <- ggplot(filter(model.out.fisher1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.fisher1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.fisher1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.fisher1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.fisher1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_BigFive_z_seascape.jpg"),width = 12, height = 6)




##################################################################################
##################################################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
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
                                                            ifelse(MPACode==6,"Misool","")))))))

varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out.mpalevel.fisher <- data.frame()

for (i in varNames) {
  for (mpaid in 1:6) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level  + dom.eth + YearsResident + IndividualGender + IndividualAge + 
                       Treatment + yearsPostF + Treatment:yearsPostF + Fisher:Treatment + Fisher:yearsPostF + Fisher:Treatment:yearsPostF 
                     | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                     data=DiD.data.mpalevel,exactDOF = TRUE)
    
    
    summary(regValue)
    
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid)
    
    model.out.mpalevel.fisher <- rbind(model.out.mpalevel.fisher,reg.broom)
  }
}




###########################
#####BigFive's Impact plots
###########################
###Rename Treatment:yearsPostF2:Fisher into t2 and so on
model.out.mpalevel.fisher1 <- model.out.mpalevel.fisher %>% 
  filter(term%in%c("Treatment:yearsPostF2:Fisher","Treatment:yearsPostF4:Fisher")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  filter(term%in%c("t2:Fisher","t4:Fisher")) %>% 
  mutate(term=gsub(":Fisher","",term)) %>% 
  left_join(mpa.nam) %>% 
  mutate(MPAName=gsub(" MPA","",MPAName),
         MPAName=gsub("Teluk ","",MPAName))

###Select only needed variables
model.out.mpalevel.fisher1 <- model.out.mpalevel.fisher1 %>% 
  select(term,estimate,std.error,statistic,p.value,Response,MPAID,MPAName,MPAName_short)


###edit model.out.fisher1 (with seascape level reg values; ready to merge/include to model.out.mpalevel.fisher1)
model.out.fisher1$MPAID <-0
model.out.fisher1$MPAName <-"Bird's head Seascape"
model.out.fisher1$MPAName_short <-"  BHS"

model.out.mpalevel.fisher1 <- rbind(model.out.mpalevel.fisher1,model.out.fisher1)

pd <- position_dodge(width=.3) # move them .05 to the left and right


############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_SE_MPAlevel.jpg"),width = 12, height = 6)






############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
FS.plot

ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_z_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_z_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_z_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_z_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_z_SE_MPAlevel.jpg"),width = 12, height = 6)






###################Combine all Five individual MPA plots; repeat to remove legends (save space) 
############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot <- ggplot(filter(model.out.mpalevel.fisher1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))

########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_BigFive_MPAlevel.jpg"),width = 12, height = 6)





############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MT.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel.fisher1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))



########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,MA.plot_z,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/fisher/stderr_band/","DDD_fisher_z_BigFive_MPAlevel.jpg"),width = 12, height = 6)



###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


##----Differential impact by ethDom vs non-ethDom
##----Differential impact by ethDom vs non-ethDom
##----Differential impact by ethDom vs non-ethDom
##----Differential impact by ethDom vs non-ethDom


##################################################################################
##################################################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
###1. Seascape-level Impact#######################################################
##################################################################################
##################################################################################
##################################################################################
##############
##############Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)
##############Seascape level impacts
varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")


##----Differential impact by eth.dom
model.out.ethDom <- data.frame()
for (i in varNames) {
  print(i)
  Y <- DiD.data[,i]
  regValue.ethDom <- felm(Y  ~  n.child  + ed.level  + dom.eth + YearsResident + IndividualGender + IndividualAge +
                            Treatment + yearsPostF + Treatment:yearsPostF + ethDom:Treatment + ethDom:yearsPostF + ethDom:Treatment:yearsPostF
                          | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                          data=DiD.data,exactDOF = TRUE)
  summary(regValue.ethDom)
  reg.broom <- tidy(regValue.ethDom) %>%
    mutate(Response=i)
  
  model.out.ethDom <- rbind(model.out.ethDom,reg.broom)
}


##########################
#####BigFive's Impact plots
###########################

###Rename Treatment:yearsPostF2:ethDom into t2 and so on
model.out.ethDom1 <- model.out.ethDom %>% 
  filter(term%in%c("Treatment:yearsPostF2:ethDom","Treatment:yearsPostF4:ethDom")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  filter(term%in%c("t2:ethDom","t4:ethDom")) %>% 
  mutate(term=gsub(":ethDom","",term))


pd <- position_dodge(width=.3) # move them .05 to the left and right


FS.plot <- ggplot(filter(model.out.ethDom1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.ethDom1,Response=="MTIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.ethDom1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.ethDom1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.ethDom1,Response=="SERate"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  


#library(cowplot)
#Combine "regular BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_BigFive_seascape.jpg"),width = 12, height = 6)


#####################Repeat the 5 plots, now using standardized scores

FS.plot <- ggplot(filter(model.out.ethDom1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.ethDom1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out.ethDom1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out.ethDom1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out.ethDom1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_BigFive_z_seascape.jpg"),width = 12, height = 6)




##################################################################################
##################################################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
###2. MPA-level Impact############################################################
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
                                                            ifelse(MPACode==6,"Misool","")))))))

varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out.mpalevel.ethDom <- data.frame()

for (i in varNames) {
  for (mpaid in 1:6) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level  + dom.eth + YearsResident + IndividualGender + IndividualAge + 
                       Treatment + yearsPostF + Treatment:yearsPostF + ethDom:Treatment + ethDom:yearsPostF + ethDom:Treatment:yearsPostF 
                     | SettlementID + InterviewYear + MPAID:InterviewYear | 0 | SettlementID,
                     data=DiD.data.mpalevel,exactDOF = TRUE)
    
    
    summary(regValue)
    
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid)
    
    model.out.mpalevel.ethDom <- rbind(model.out.mpalevel.ethDom,reg.broom)
  }
}




###########################
#####BigFive's Impact plots
###########################
###Rename Treatment:yearsPostF2:ethDom into t2 and so on
model.out.mpalevel.ethDom1 <- model.out.mpalevel.ethDom %>% 
  filter(term%in%c("Treatment:yearsPostF2:ethDom","Treatment:yearsPostF4:ethDom")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  filter(term%in%c("t2:ethDom","t4:ethDom")) %>% 
  mutate(term=gsub(":ethDom","",term)) %>% 
  left_join(mpa.nam) %>% 
  mutate(MPAName=gsub(" MPA","",MPAName),
         MPAName=gsub("Teluk ","",MPAName))

###Select only needed variables
model.out.mpalevel.ethDom1 <- model.out.mpalevel.ethDom1 %>% 
  select(term,estimate,std.error,statistic,p.value,Response,MPAID,MPAName,MPAName_short)


###edit model.out.ethDom1 (with seascape level reg values; ready to merge/include to model.out.mpalevel.ethDom1)
model.out.ethDom1$MPAID <-0
model.out.ethDom1$MPAName <-"Bird's head Seascape"
model.out.ethDom1$MPAName_short <-"  BHS"

model.out.mpalevel.ethDom1 <- rbind(model.out.mpalevel.ethDom1,model.out.ethDom1)

pd <- position_dodge(width=.3) # move them .05 to the left and right


############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_SE_MPAlevel.jpg"),width = 12, height = 6)






############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
FS.plot

ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_z_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_z_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_z_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_z_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_z_SE_MPAlevel.jpg"),width = 12, height = 6)






###################Combine all Five individual MPA plots; repeat to remove legends (save space) 
############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))

########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_BigFive_MPAlevel.jpg"),width = 12, height = 6)





############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MT.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel.ethDom1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1*std.error, ymax=estimate+1*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DDD Estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))



########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,MA.plot_z,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/BHS/subGroup_DDD/ethDom/stderr_band/","DDD_ethDom_z_BigFive_MPAlevel.jpg"),width = 12, height = 6)








