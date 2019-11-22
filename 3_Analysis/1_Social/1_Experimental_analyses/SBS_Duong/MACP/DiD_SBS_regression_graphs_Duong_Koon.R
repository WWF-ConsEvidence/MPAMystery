
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
  left_join(select(HHData,DidNotLast:EconStatusReason,MAIndex:FSIndex,SERate,HouseholdID,InterviewYear), by="HouseholdID") %>% 
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


## --- Modifying Asset Items to generate sub-Asset Groups (i.e. Household assets (discretionary & appliances), Productive Marine-based Assets (the boats), and land-based (vehicles)
DiD.data <- DiD.data %>% 
  mutate(Entertain = Entertain,
         PhoneCombined = PhoneCombined/2,
         Satellite = Satellite/3,
         TV = TV/4,
         Generator = Generator/5,
         BoatNoMotor = BoatNoMotor/6,
         BoatOutboard = BoatOutboard/7,
         BoatInboard = BoatInboard/8,
         Bicycle = Bicycle/9,
         Motorcycle = Motorcycle/10,
         CarTruck = CarTruck/11) %>% 
  mutate(Entertain_dum = ifelse(Entertain>0,1,0),
         PhoneCombined_dum = ifelse(PhoneCombined>0,1,0),
         Satellite_dum = ifelse(Satellite>0,1,0),
         TV_dum = ifelse(TV>0,1,0),
         Generator_dum = ifelse(Generator>0,1,0),
         BoatNoMotor_dum = ifelse(BoatNoMotor>0,1,0),
         BoatOutboard_dum = ifelse(BoatOutboard>0,1,0),
         BoatInboard_dum = ifelse(BoatInboard>0,1,0),
         Bicycle_dum = ifelse(Bicycle>0,1,0),
         Motorcycle_dum = ifelse(Motorcycle>0,1,0),
         CarTruck_dum = ifelse(CarTruck>0,1,0)) %>%
  mutate(Household_asset = Entertain + 2*PhoneCombined + 3*Satellite + 4*TV + 5*Generator,
         Boats_w1 = BoatNoMotor + 2*BoatOutboard + 3*BoatInboard,
         Boats_w2 = 6*BoatNoMotor + 7*BoatOutboard + 8*BoatInboard,
         Boats_motor_w1 = 1*BoatOutboard + 2*BoatInboard,
         Boats_motor_w2 = 7*BoatOutboard + 8*BoatInboard,
         Vehicles_w1 = Bicycle + 2*Motorcycle + 3*CarTruck,
         Vehicles_w2 = 9*Bicycle + 10*Motorcycle + 11*CarTruck) %>% 
  mutate(Boats_dum = ifelse(BoatNoMotor>0 | BoatOutboard >0 | BoatInboard >0,1,0),
         Boats_motor_dum = ifelse(BoatOutboard >0 | BoatInboard >0,1,0),
         Vehicles_dum = ifelse(Bicycle>0 | Motorcycle>0 | CarTruck>0,1,0))


# calculate Z scores (standardized values for each of the Big Five)
DiD.data <- DiD.data %>% 
  group_by(MPAID,MonitoringYear) %>% 
  mutate_at(vars(MAIndex:SERate, Household_asset:Vehicles_w2), .funs = list(`z`= ~ (.-mean(.,na.rm=T))/sd(.,na.rm = T))) %>% 
  ungroup()

# mean2 <- function(x){ mean(x,na.rm=T)}
# sd2 <- function(x){ sd(x,na.rm=T)}


# calculate p scores
pscore <- glm(Treatment ~ TimeMarket + n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge,
              data=DiD.data)$fitted.values

DiD.data <- cbind(DiD.data,pscore)  




##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
# --- (Kei Kecil - MPA==17 (Baseline 2016, Postline 2019 -- note: Kei doesn't have assetTV in t3); Koon - MPA==18 (Baseline 2016, Postline 2018))
# --- Filter Koon  

##1. Post-QAQC check on all variables
DiD.data.Koon <- DiD.data %>% 
  filter(MPAID ==18)

for (i in 1:ncol(DiD.data.Koon)){
  
  print(paste0(names(DiD.data.Koon)[i]," (",class(DiD.data.Koon[,i]),")"))
  
  if(length(unique(DiD.data.Koon[,i]))>25 & class(DiD.data.Koon[,i])!="character"){
    print(summary(DiD.data.Koon[,i]))
  }
  
  else{
    if (length(unique(DiD.data.Koon[,i]))<25){
      print(table(DiD.data.Koon[,i]))}
    else{
      print(head(DiD.data.Koon[,i]))}
  }
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
                                                                          ifelse(MPACode==16,"Flores Timur",
                                                                                 ifelse(MPACode==17,"Kei Kecil",
                                                                                        ifelse(MPACode==18,"Koon","")))))))))))


varNames <- c("FSIndex","MAIndex","MTIndex","PAIndex","SERate", "FSIndex_z","MAIndex_z","MTIndex_z","PAIndex_z","SERate_z")
model.out.MACP.impact <- data.frame()


###Regression loops to get 1. Trend of control Group (i.e. coef associated with yearsPostF); 2. Impact (i.e. coef associated with Treatment:yearsPostF)
### and 3. manually derive Trend of Treatment Group 
for (mpaid in 18:18) {
  DiD.MACP.impact <- DiD.data %>% filter(MPAID==mpaid)
  print(mpaid)
  
  
  for (i in varNames) {
    print(i)
    
    Y <- DiD.MACP.impact[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                     | MPAID | 0 | SettlementID,
                     data=DiD.MACP.impact,exactDOF = TRUE)
    summary(regValue)
    
    ###Get covariance value between yearsPostF3 and Treatment:yearsPostF3 (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
    vcov.matrix<-vcov(regValue) 
    vcov <- vcov.matrix[8,9] 
    
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid,vcov_alpha2_3=vcov)
    
    model.out.MACP.impact <- rbind(model.out.MACP.impact,reg.broom)
  }
}


##keeping only 2 terms yearsPostF2 (i.e. time trend) and Treatment:yearsPostF2 (i.e. DiD impact)
model.out.MACP.impact1 <- model.out.MACP.impact %>% 
  filter(term%in%c("yearsPostF2", "Treatment:yearsPostF2")) %>% 
  mutate(term=gsub("Treatment:yearsPostF2","Impact",term)) %>% 
  mutate(term=gsub("yearsPostF2","Control_Trend",term))  


# ##keeping only 2 terms yearsPostF3 (i.e. time trend) and Treatment:yearsPostF3 (i.e. DiD impact)
# model.out.MACP.impact1 <- model.out.MACP.impact %>% 
#   filter(term%in%c("yearsPostF3", "Treatment:yearsPostF3")) %>% 
#   mutate(term=gsub("Treatment:yearsPostF3","Impact",term)) %>% 
#   mutate(term=gsub("yearsPostF3","Control_Trend",term))  

## spead dataframe to compute treatment trend's estimates (=control trend + Impact)
model.out.MACP.impact1.rearrange.estimate<-model.out.MACP.impact1 %>% 
  select(Response,MPAID,term,estimate,vcov_alpha2_3) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = Control_Trend + Impact) %>% 
  select(Response, MPAID, estimate) %>% 
  mutate(term="Treatment_Trend",statistic=0,p.value=0,vcov_alpha2_3=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out.MACP.impact1.rearrange.stderr<-model.out.MACP.impact1 %>% 
  select(Response,MPAID,term,std.error,vcov_alpha2_3) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(Control_Trend^2 + Impact^2 + 2*Control_Trend*Impact*vcov_alpha2_3))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out.MACP.impact1.rearrange.estimate<-cbind(model.out.MACP.impact1.rearrange.estimate,model.out.MACP.impact1.rearrange.stderr) 

model.out.MACP.impact1<-rbind(model.out.MACP.impact1,model.out.MACP.impact1.rearrange.estimate)

model.out.MACP.impact1 <- model.out.MACP.impact1 %>% 
  mutate(Group=ifelse(term=="Control_Trend"," Control",
                      ifelse(term=="Treatment_Trend"," Treatment","Impact"))) %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"  Telma",
                                ifelse(MPAID==2,"  TNTC",
                                       ifelse(MPAID==3," Kaimana",
                                              ifelse(MPAID==4," Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==15,"Selat Pantar",
                                                                          ifelse(MPAID==16,"Flores Timur",
                                                                                 ifelse(MPAID==17,"Kei Kecil",
                                                                                        ifelse(MPAID==18,"Koon","")))))))))))


model.out.MACP.impact2 <- model.out.MACP.impact1 %>% 
  filter(Group%in%c(" Control", " Treatment")) 


##Export out the output frame used to plot MACP impacts
export(model.out.MACP.impact1 %>% 
         select(term, estimate, Response, MPAID, std.error, Group, MPAName_short) %>%  
         arrange(MPAID,Response), 
       "D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/macp_plots_output.csv")


##Export out the output frame used to plot big five compilation
export(model.out.MACP.impact1 %>% 
         select(term, estimate, std.error, Response, MPAID, MPAName_short) %>%  
         filter(term%in%c("Impact")) %>% 
         filter(Response%in%c("FSIndex_z","MAIndex_z","PAIndex_z","MTIndex_z","SERate_z")) %>% 
         arrange(MPAID,Response), 
       "D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/macp_plots_output_standardized_Big5.csv")






##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------##

##2.1 DiD Impact Plots -- for MACP reports (asset_subClasses)
##2.1 DiD Impact Plots -- for MACP reports (asset_subClasses)
##2.1 DiD Impact Plots -- for MACP reports (asset_subClasses)
##2.1 DiD Impact Plots -- for MACP reports (asset_subClasses)

##-------Using lfe (felm) for high dimensional FE DiD (similar to reghdfe)----##
##-------Seascape level impacts-----------------------------------------------## 
##--------------------NOTE--------------------------##
##-------Exclude time and settlement FEs in order to plot trends--------------##
###generating short MPA names
summary(DiD.data)
varNames <- c("Household_asset", "Boats_w1", "Boats_motor_w1", "BoatNoMotor_dum", "BoatOutboard_dum", "BoatInboard_dum", "Vehicles_w1", "Vehicles_w2")
model.out.MACP.impact.subAsset <- data.frame()


###Checking Alor (Selat Pantar)
# test <- DiD.data %>% filter(MPAID==15)
# summary(test)
# 
# test1 <- DiD.data %>% filter(MPAID==16)
# summary(test1)

###Regression loops to get 1. Trend of control Group (i.e. coef associated with yearsPostF); 2. Impact (i.e. coef associated with Treatment:yearsPostF)
### and 3. manually derive Trend of Treatment Group 
for (mpaid in 18:18) {
  DiD.MACP.impact.subAsset <- DiD.data %>% filter(MPAID==mpaid)
  print(mpaid)
  
  
  for (i in varNames) {
    print(i)
    
    Y <- DiD.MACP.impact.subAsset[,i]
    
    regValue <- felm(Y  ~  n.child  + ed.level + dom.eth + YearsResident + IndividualGender + IndividualAge + Treatment + yearsPostF + Treatment:yearsPostF
                     | MPAID | 0 | SettlementID,
                     data=DiD.MACP.impact.subAsset,exactDOF = TRUE)
    summary(regValue)
    
    ###Get covariance value between yearsPostF3 and Treatment:yearsPostF3 (need this for calculating S.E. of treatment trend later (alpha2 + alpha3))
    vcov.matrix<-vcov(regValue) 
    vcov <- vcov.matrix[8,9] 
    
    reg.broom <- tidy(regValue) %>% 
      mutate(Response=i, MPAID=mpaid,vcov_alpha2_3=vcov)
    
    model.out.MACP.impact.subAsset <- rbind(model.out.MACP.impact.subAsset,reg.broom)
  }
}


##keeping only 2 terms yearsPostF3 (i.e. time trend) and Treatment:yearsPostF3 (i.e. DiD impact)
model.out.MACP.impact1.subAsset <- model.out.MACP.impact.subAsset %>% 
  filter(term%in%c("yearsPostF2", "Treatment:yearsPostF2")) %>% 
  mutate(term=gsub("Treatment:yearsPostF2","Impact",term)) %>% 
  mutate(term=gsub("yearsPostF2","Control_Trend",term))  

# ##keeping only 2 terms yearsPostF3 (i.e. time trend) and Treatment:yearsPostF3 (i.e. DiD impact)
# model.out.MACP.impact1.subAsset <- model.out.MACP.impact.subAsset %>% 
#   filter(term%in%c("yearsPostF3", "Treatment:yearsPostF3")) %>% 
#   mutate(term=gsub("Treatment:yearsPostF3","Impact",term)) %>% 
#   mutate(term=gsub("yearsPostF3","Control_Trend",term))  

## spead dataframe to compute treatment trend's estimates (=control trend + Impact)
model.out.MACP.impact1.rearrange.estimate.subAsset<-model.out.MACP.impact1.subAsset %>% 
  select(Response,MPAID,term,estimate,vcov_alpha2_3) %>% 
  spread(term,estimate) %>% 
  mutate(estimate = Control_Trend + Impact) %>% 
  select(Response, MPAID, estimate) %>% 
  mutate(term="Treatment_Trend",statistic=0,p.value=0,vcov_alpha2_3=0)

## spead dataframe to compute treatment trend's std.error (=squareRoot(var(alpha2) + var(alpha3) + 2cov(alpha2,3)Xstderr2Xstderr3)
model.out.MACP.impact1.rearrange.stderr.subAsset<-model.out.MACP.impact1.subAsset %>% 
  select(Response,MPAID,term,std.error,vcov_alpha2_3) %>% 
  spread(term,std.error) %>% 
  mutate(std.error = sqrt(abs(Control_Trend^2 + Impact^2 + 2*Control_Trend*Impact*vcov_alpha2_3))) %>% ##getting std.error for treatment trend here
  select(std.error) 

model.out.MACP.impact1.rearrange.estimate.subAsset<-cbind(model.out.MACP.impact1.rearrange.estimate.subAsset,model.out.MACP.impact1.rearrange.stderr.subAsset) 

model.out.MACP.impact1.subAsset<-rbind(model.out.MACP.impact1.subAsset,model.out.MACP.impact1.rearrange.estimate.subAsset)

model.out.MACP.impact1.subAsset <- model.out.MACP.impact1.subAsset %>% 
  mutate(Group=ifelse(term=="Control_Trend"," Control",
                      ifelse(term=="Treatment_Trend"," Treatment","Impact"))) %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"  Telma",
                                ifelse(MPAID==2,"  TNTC",
                                       ifelse(MPAID==3," Kaimana",
                                              ifelse(MPAID==4," Kofiau",
                                                     ifelse(MPAID==5,"Dampier",
                                                            ifelse(MPAID==6,"Misool",
                                                                   ifelse(MPAID==15,"Selat Pantar",
                                                                          ifelse(MPAID==16,"Flores Timur",
                                                                                 ifelse(MPAID==17,"Kei Kecil",
                                                                                        ifelse(MPAID==18,"Koon","")))))))))))


model.out.MACP.impact2.subAsset <- model.out.MACP.impact1.subAsset %>% 
  filter(Group%in%c(" Control", " Treatment")) 


##Export out the output frame used to plot MACP impacts
export(model.out.MACP.impact1.subAsset %>% 
         select(term, estimate, Response, MPAID, std.error, Group, MPAName_short) %>%  
         arrange(MPAID,Response), 
       "D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/macp_plots_output_Asset_subClasses.csv")



##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##
##----------Individual MPA Impact plots (mainly for MACP repots)-------------------##

## Produce trend plots (separately for control and treatment settlements) for each MPA
MPA_name <- c("Koon")

pd <- position_dodge() # move them .05 to the left and right

##---loop through each MPAID to generate independent plots
for (mpa in MPA_name) {
  print(mpa)
  mpa.impact.data <- model.out.MACP.impact1 %>%
    mutate(MPAName_short = ifelse(MPAID==1,"  Telma",
                                  ifelse(MPAID==2,"  TNTC",
                                         ifelse(MPAID==3," Kaimana",
                                                ifelse(MPAID==4," Kofiau",
                                                       ifelse(MPAID==5,"Dampier",
                                                              ifelse(MPAID==6,"Misool",
                                                                     ifelse(MPAID==15,"Selat Pantar",
                                                                            ifelse(MPAID==16,"Flores Timur",
                                                                                   ifelse(MPAID==17,"Kei Kecil",
                                                                                          ifelse(MPAID==18,"Koon",""))))))))))) %>% 
  
    filter(MPAName_short==mpa) %>% 
    ungroup() 
  
  FS.plot <- ggplot(filter(mpa.impact.data,Response=="FSIndex"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Food Security")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_FS_",mpa,".jpg"),width = 12, height = 6)
  
  MT.plot <- ggplot(filter(mpa.impact.data,Response=="MTIndex"),aes(x=term,y=estimate)) +  theme(legend.position="none") +
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Marine Tenure")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_MT_",mpa,".jpg"),width = 12, height = 6)
  
  
  MA.plot <- ggplot(filter(mpa.impact.data,Response=="MAIndex"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() +  theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Material Assets")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_MA_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  PA.plot <- ggplot(filter(mpa.impact.data,Response=="PAIndex"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Place Attachment") 
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_PA_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  SE.plot <- ggplot(filter(mpa.impact.data,Response=="SERate"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="School Enrollment")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_SE_",mpa,".jpg"),width = 12, height = 6)
  
  
  #library(cowplot)
  #Combine "regular BigFive"
  plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_BigFive_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  #####################Repeat the 5 plots, now using standardized scores
  
  
  FS.plot <- ggplot(filter(mpa.impact.data,Response=="FSIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Food Security") 
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_FS_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  MT.plot <- ggplot(filter(mpa.impact.data,Response=="MTIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Marine Tenure")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_MT_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  
  MA.plot <- ggplot(filter(mpa.impact.data,Response=="MAIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Material Assets")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_MA_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  PA.plot <- ggplot(filter(mpa.impact.data,Response=="PAIndex_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="Place Attachment")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_PA_z_",mpa,".jpg"),width = 12, height = 6)
  
  
  SE.plot <- ggplot(filter(mpa.impact.data,Response=="SERate_z"),aes(x=term,y=estimate)) + 
    geom_point(stat="identity", position =pd, fill='black', size=3)+ theme_bw() + theme(legend.position="none") +
    geom_line( position = pd) + 
    geom_errorbar(aes(ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error), width=0.2, position = pd ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x="",y="Impact estimate", title="School Enrollment")  
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_SE_z_",mpa,".jpg"),width = 12, height = 6)
  
  #Combine "standardize BigFive"
  plot_grid(FS.plot,MT.plot,PA.plot,SE.plot,MA.plot,ncol=3)
  ggsave(paste0("D:/Dropbox/1_Social/Outputs/impact_analysis/Koon/plots/",mpa,"/DiD_BigFive_z_",mpa,".jpg"),width = 12, height = 6)
  
}



