#.libPaths()
#.libPaths("C:/Users/Duong Le/Documents/R/R-3.6.0/library")
library(lfe)
library(cowplot)
library(Matrix)
library(stargazer)
library(broom)


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
model.out1 <- model.out %>% 
  filter(term%in%c("Treatment:yearsPostF2","Treatment:yearsPostF4")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term))

pd <- position_dodge(width=.3) # move them .05 to the left and right


FS.plot <- ggplot(filter(model.out1,Response=="FSIndex"),aes(x=term,y=estimate)) + 
            geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
            geom_line( position = pd) + 
            geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            labs(x="",y="DiD estimate", title="Food Security")  
            #+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out1,Response=="MTIndex"),aes(x=term,y=estimate)) + 
              geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
              geom_line( position = pd) + 
              geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
              geom_hline(yintercept = 0, linetype = "dashed") +
              labs(x="",y="DiD estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out1,Response=="MAIndex"),aes(x=term,y=estimate)) + 
              geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
              geom_line( position = pd) + 
              geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
              geom_hline(yintercept = 0, linetype = "dashed") +
              labs(x="",y="DiD estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out1,Response=="PAIndex"),aes(x=term,y=estimate)) + 
              geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
              geom_line( position = pd) + 
              geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
              geom_hline(yintercept = 0, linetype = "dashed") +
              labs(x="",y="DiD estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out1,Response=="SERate"),aes(x=term,y=estimate)) + 
              geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
              geom_line( position = pd) + 
              geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
              geom_hline(yintercept = 0, linetype = "dashed") +
              labs(x="",y="DiD estimate", title="School Enrollment")  


#library(cowplot)
#Combine "regular BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_BigFive_seascape.jpg"),width = 12, height = 6)



#####################Repeat the 5 plots, now using standardized scores


FS.plot <- ggplot(filter(model.out1,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DiD estimate", title="Food Security")  
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out1,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DiD estimate", title="Marine Tenure")  


MA.plot <- ggplot(filter(model.out1,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DiD estimate", title="Material Assets")  


PA.plot <- ggplot(filter(model.out1,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DiD estimate", title="Place Attachment")  


SE.plot <- ggplot(filter(model.out1,Response=="SERate_z"),aes(x=term,y=estimate)) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x="",y="DiD estimate", title="School Enrollment")  

#Combine "standardize BigFive"
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_BigFive_z_seascape.jpg"),width = 12, height = 6)







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
model.out.mpalevel <- data.frame()

for (i in varNames) {
  for (mpaid in 1:6) {
    print(i)
    print(mpaid)
    DiD.data.mpalevel <- DiD.data %>% 
      filter(MPAID==mpaid)
    Y <- DiD.data.mpalevel[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                     | SettlementID + InterviewYear + MPAID:InterviewYear  | 0 | SettlementID,
                     data=DiD.data.mpalevel, exactDOF = TRUE)
    
    
    summary(regValue)
    
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid)
    
    model.out.mpalevel <- rbind(model.out.mpalevel,reg.broom)
  }
}

###########################
#####BigFive's Impact plots
###########################
model.out.mpalevel1 <- model.out.mpalevel %>% 
  filter(term%in%c("Treatment:yearsPostF2","Treatment:yearsPostF4")) %>% 
  mutate(term=gsub("Treatment:yearsPostF","t",term)) %>% 
  left_join(mpa.nam) %>% 
  mutate(MPAName=gsub(" MPA","",MPAName),
         MPAName=gsub("Teluk ","",MPAName))


###edit model.out1 (with seascape level reg values; ready to merge/include to model.out.mpalevel1)
model.out1$MPAID <-0
model.out1$MPAName <-"Bird's head Seascape"
model.out1$MPAName_short <-"  BHS"

model.out.mpalevel1 <- rbind(model.out.mpalevel1,model.out1)
summary(model.out.mpalevel1)
head(mpa.nam)


pd <- position_dodge(width=.3) # move them .05 to the left and right


############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot <- ggplot(filter(model.out.mpalevel1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_SE_MPAlevel.jpg"),width = 12, height = 6)






############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
FS.plot

ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_z_FS_MPAlevel.jpg"),width = 12, height = 6)

MT.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_z_MT_MPAlevel.jpg"),width = 12, height = 6)


PA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_z_PA_MPAlevel.jpg"),width = 12, height = 6)


MA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_z_MA_MPAlevel.jpg"),width = 12, height = 6)


SE.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_z_SE_MPAlevel.jpg"),width = 12, height = 6)






###################Combine all Five individual MPA plots; repeat to remove legends (save space) 
############################################################
########Producing Big Five plots using "regular" index
FS.plot <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))
#+ facet_grid(.~Response)

MT.plot <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot <- ggplot(filter(model.out.mpalevel1,Response=="SERate"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))

########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_BigFive_MPAlevel.jpg"),width = 12, height = 6)





############################################################
########Producing Big Five plots using "standardized" index
FS.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="FSIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Food Security")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MT.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MTIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Marine Tenure")  +
  scale_colour_manual(values = c("skyblue3", "blue")) 


PA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="PAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) +   
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Place Attachment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


MA.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="MAIndex_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="Material Assets")  +
  scale_colour_manual(values = c("skyblue3", "blue"))


SE.plot_z <- ggplot(filter(model.out.mpalevel1,Response=="SERate_z"),aes(x=MPAName_short,y=estimate, color=term),group=1) + 
  geom_point(stat="identity", position =pd, fill='blue', size=3)+ theme_bw() +
  geom_line( position = pd) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1.5, linetype = "dotdash") +
  labs(x="",y="DiD estimate", title="School Enrollment")  +
  scale_colour_manual(values = c("skyblue3", "blue"))



########################Combine BigFive MPAlevel graph
#library(cowplot)
plot_grid(FS.plot_z,MT.plot_z,PA.plot_z,SE.plot_z,MA.plot_z,ncol=3)
ggsave(paste0("R:/Gill/MPAMystery/x_Flat_data_files/1_Social/Outputs/DiD_result/","DiD_z_BigFive_MPAlevel.jpg"),width = 12, height = 6)











