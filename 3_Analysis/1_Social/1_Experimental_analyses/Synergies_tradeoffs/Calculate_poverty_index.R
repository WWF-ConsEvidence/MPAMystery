## December 22 2020
## Extracted script to produce asset-based poverty alleviation index 
# author: Duong Le


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 0. SOURCING ---- 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


##---0. Sourcing and creating data frame for status/trend and impact 


# --- DiD dataframe 
DiD.data <- match.covariate %>% 
  filter(!is.na(HouseholdID)) %>% 
  left_join(select(HHData, DidNotLast:EconStatusReason, SocialConflict:NumGlobalAction, MAIndex:FSIndex, SERate, HouseholdID, InterviewYear), by="HouseholdID") %>% 
  select(HouseholdID:InterviewYear) %>% 
  mutate(TreatFactor= as.factor(ifelse(Treatment==0,0,MPAID)),
         Post = as.factor(ifelse(yearsPost==0,0,1)),
         Control = ifelse(Treatment==1,0,1), 
         yearsPostF=as.factor(yearsPost),
         MPAID=as.factor(MPAID),
         InterviewYear=as.factor(InterviewYear), 
         Fisher=ifelse(PrimaryLivelihood==3 | SecondaryLivelihood==3 | TertiaryLivelihood==3,1,0), 
         Male = IndividualGender, 
         Boat = BoatNoMotor + BoatOutboard + BoatInboard,
         PrimaryLivelihood.bin = as.factor(ifelse(PrimaryLivelihood==1,1,
                                                  ifelse(PrimaryLivelihood==2,2,
                                                         ifelse(PrimaryLivelihood==3|PrimaryLivelihood==4,3,
                                                                ifelse(PrimaryLivelihood==6|PrimaryLivelihood==7,4,5)))))) %>% 
  filter(!is.na(IndividualGender))


# PrimaryLivelihood.bin:
# 1.Farming
# 2.Harvesting forest products
# 3.Fishing + Aquaculture
# 4.Marine tourism + wage labor
# 5.Extractives + other


# --- Filter to look at 6 BHS and 4 SBS MPAs (with t2 and/or t4 data) --- Jan 2020
DiD.data <- DiD.data %>% 
  filter(MPAID%in%c(1:6, 15:18))

DiD.data.test <- DiD.data %>% 
  filter(MPAID%in%c(1:6))

#plyr::count(DiD.data, "indigenous")

# --- two additional outcome variables --- Feb 2020
DiD.data <- DiD.data %>% 
  mutate(MTIndex_AccHarv = RightsAccess + RightsHarvest, 
         MTIndex_ManExcTrans= RightsManage + RightsExclude + RightsTransfer, 
         SocialConflict_increase = ifelse(is.na(SocialConflict), NA, 
                                          ifelse(SocialConflict==1|SocialConflict==2, 1, 0)), 
         EconTrend_decrease = ifelse(is.na(EconStatusTrend), NA, 
                                     ifelse(EconStatusTrend==1|EconStatusTrend==2, 1, 0)))

# ---- Categorizing cooking fuel indicator 
plyr::count(HHData, "CookingFuel")
cooking.fuel.df <- HHData %>% 
  select(HouseholdID, CookingFuel) %>% 
  mutate(CookingFuel=ifelse(CookingFuel==0| CookingFuel>=998, NA, CookingFuel),
         fuel.elec=ifelse(CookingFuel==1,1,0),
         fuel.oil=ifelse(CookingFuel==2,1,0),
         fuel.wood=ifelse(CookingFuel==3,1,0),
         fuel.other=ifelse(CookingFuel==4|CookingFuel==5|CookingFuel==6,1,0))

DiD.data <- DiD.data %>% 
  select(-CookingFuel.Biomass) %>% 
  left_join(cooking.fuel.df, by=c("HouseholdID")) %>% 
  mutate(fuel.biomass=ifelse(fuel.elec==0 & fuel.oil==0, 1, 0),
         fuel.nonBiomass=ifelse(fuel.elec==1 | fuel.oil==1, 1, 0))

## --- Modifying Asset Items to generate sub-Asset Groups (i.e. Household assets (discretionary & appliances), Productive Marine-based Assets (the boats), and land-based (vehicles)
##--> note: generate the asset-ownership binary indicators below  
DiD.data <- DiD.data %>% 
  mutate(Entertain_yes = ifelse(Entertain>0,1,0),
         PhoneCombined_yes = ifelse(PhoneCombined>0,1,0),
         Satellite_yes = ifelse(Satellite>0,1,0),
         TV_yes = ifelse(TV>0,1,0),
         Generator_yes = ifelse(Generator>0,1,0),
         BoatNoMotor_yes = ifelse(BoatNoMotor>0,1,0),
         BoatOutboard_yes = ifelse(BoatOutboard>0,1,0),
         BoatInboard_yes = ifelse(BoatInboard>0,1,0),
         Boat_yes = ifelse(Boat>0,1,0),
         Bicycle_yes = ifelse(Bicycle>0,1,0),
         Motorcycle_yes = ifelse(Motorcycle>0,1,0),
         CarTruck_yes = ifelse(CarTruck>0,1,0)) 
  

  
  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 2. PCA CONSTRUCTION---- 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##---2. Conducting Principle Component Analysis (PCA) for Asset weights

# ##---dummies (negative) only (no boat)
# MA.df.2 <- DiD.data %>%
#   subset(RemoveMA=="No") %>%
#   select(Entertain_z, PhoneCombined_z, Satellite_z, TV_z, Generator_z, Bicycle_z, Motorcycle_z, CarTruck_z, Boat_z) %>%                  
#   na.omit()


##---dummies (negative) only (with boat)
poverty.df <- DiD.data %>%
  subset(RemoveMA=="No") %>%
  select(Entertain_yes, PhoneCombined_yes, Satellite_yes, TV_yes, Generator_yes, Bicycle_yes, Motorcycle_yes, CarTruck_yes, Boat_yes, fuel.nonBiomass) %>%                  
  na.omit()


##--function to get weights: 
# Function to clean string variables (lower case, remove punctuation)
get_pca_weight <- function(df) {
  require(dplyr)
  
  ##---Using z values: Perform PCA to get eigenvalues (i.e. loading factors) for each asset's weight-----##
  poverty.pca.z <- prcomp(df, scale = FALSE)
  
  #Get eigenvalues, or variance percentage, for each principle components (i.e. how well each of PCs contribute in explaning the total variance in our asset data set)
  asset.eig.val <- get_eigenvalue(poverty.pca.z)
  asset.eig.val
  fviz_eig(poverty.pca.z) #a scree plot to illustrate the above
  asset.eig.val.keep <- asset.eig.val %>% 
    #subset(eigenvalue>=1) %>% 
    mutate(pc.contrib = variance.percent/sum(variance.percent)) #keep only PCs with eigenvalue >=1 and find % of explained variance that each PC contributes
  asset.eig.val.keep <- as.data.frame(asset.eig.val.keep)
  
  #Get the contribution of each asset items in each PCs 
  #Importantly, literature use (1) the contribution factors associated with the first component (i.e. Dimension 1) 
  #to construct the asset weights in computing composite asset-based index, or (2) all Dimensions with eigenvalue >=1
  asset.weight.frame <- get_pca_var(poverty.pca.z)$contrib
  asset.weight.frame <- asset.weight.frame[ ,1:nrow(asset.eig.val.keep)] 
  asset.weight.frame <- as.data.frame(t(asset.weight.frame))
  asset.weight.frame <- cbind(asset.weight.frame,asset.eig.val.keep) 
  
  #Extract weights (scoring factors) using the variance contribution of each assets to the First PC
  asset.weight.pc1 <- asset.weight.frame[1,] %>% 
    select(-c("eigenvalue", "variance.percent", "cumulative.variance.percent", "pc.contrib")) 
  return(asset.weight.pc1)
}

#Produce 2-dimensional PCA plots (i.e. looking at the first 2 PCs) to visualize the correlation of each assets in contributing to the first 2 PCs
poverty.pca.z <- prcomp(poverty.df, scale = FALSE)
# fviz_pca_var(poverty.pca.z, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseSCale/povertyAllev_PCA_dimension.png"),width = 12, height = 8)

##--- excecute fn get_pca_weights to get weights
asset.weight.pc1 <- get_pca_weight(poverty.df)

CarTruck_w <- (1/100) * asset.weight.pc1$CarTruck_yes
Bicycle_w <- (1/100) * asset.weight.pc1$Bicycle_yes
Motorcycle_w <- (1/100) * asset.weight.pc1$Motorcycle_yes
PhoneCombined_w <- (1/100) * asset.weight.pc1$PhoneCombined_yes
TV_w <- (1/100) * asset.weight.pc1$TV_yes
Entertain_w <- (1/100) * asset.weight.pc1$Entertain_yes
Satellite_w <- (1/100) * asset.weight.pc1$Satellite_yes
Generator_w <- (1/100) * asset.weight.pc1$Generator_yes
fuel.nonBiomass_w <- (1/100) * asset.weight.pc1$fuel.nonBiomass
Boat_w <- (1/100) * asset.weight.pc1$Boat_yes


#----------## computing the asset-based "poverty alleviation index" using the calculated weights above
DiD.data <- DiD.data %>% 
  mutate(PovertyIndex_pca = Entertain_w*Entertain_yes + PhoneCombined_w*PhoneCombined_yes + Satellite_w*Satellite_yes + TV_w*TV_yes +
           Generator_w*Generator_yes + Bicycle_w*Bicycle_yes + Motorcycle_w*Motorcycle_yes + CarTruck_w*CarTruck_yes + Boat_w*Boat_yes + fuel.nonBiomass_w*fuel.nonBiomass)     


# histogram of PovertyIndex_pca
hist(DiD.data$PovertyIndex_pca, main="Distribution of Asset-based Poverty", xlab="Poverty Index (PCA)")
#ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseSCale/povertyIndex_PCA_histo.png"),width = 12, height = 8)






# ---- STOP ----




# ##----------------------------------------------------------------#
# ##--- correlation between Poverty Index and other indicators of poverty
# 
# ## ---- a. correlation bw Poverty Index and social indicators ----
# 
# # corr.poverty.df <- DiD.data %>% 
# #   select(PovertyIndex_pca, EconStatusTrend, EconTrend_decrease, MAIndex, FSIndex, SERate, PAIndex, MTIndex, DidNotLast:Hungry) 
# # 
# # library("Hmisc", corrplot)
# # corr.poverty <- rcorr(as.matrix(corr.poverty.df))
# # corr.poverty
# # corrplot(corr.poverty$r, method="number", type = "upper", order = "hclust", tl.col = "black", diag=FALSE)
# 
# ## -- scatterplots to see the relationship between poverty_index_pca and other indicators of poverty
# ##-- With FSIndex
# Poverty_FS.plot <- ggplot(DiD.data.baseline, aes(x=FSIndex, y=PovertyIndex_pca)) +  
#   stat_summary(geom="errorbar", fun.data=mean_cl_normal, size= 1, width=0) +
#   stat_summary(geom="point", fun.y = mean, shape=21, color="black", fill="#69b3a2", size=5) +
#   scale_x_continuous(breaks=c(0,1.56, 4.02, 6.06)) +
#   theme_bw() + theme(axis.text.x=element_text(size=12, angle = 0, hjust = 0.5, vjust = 1),
#                      axis.text.y=element_text(size=12, angle = 90, hjust = 0.5, vjust = 1),
#                      axis.title.x=element_text(size=13,face="bold"),
#                      axis.title.y=element_text(size=13,face="bold")) +
#   labs(x="Food Security Index",y="Poverty Alleviation Index", title="Correlation: Poverty Alleviation Index & Food Security Index")
# Poverty_FS.plot
# Poverty_FS.fig <- plotly::ggplotly(Poverty_FS.plot)
# Poverty_FS.fig
# 
# # create legend: 0-1.56: Food Insecure with Hunger; 1.56-4.02: Food Insecure without Hunger; 4.02-6.06: Food Secure 
# 
# ##-- With ed.level
# Poverty_Educ.plot <- ggplot(DiD.data.baseline, aes(x=ed.level.round, y=PovertyIndex_pca)) +  
#   stat_summary(geom="errorbar", fun.data=mean_cl_normal, size= 1, width=0) +
#   stat_summary(geom="point", fun.y = mean, shape=21, color="black", fill="#69b3a2", size=5) +
#   theme_bw() + theme(axis.text.x=element_text(size=11, angle = 0, hjust = 0.5, vjust = 1),
#                      axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
#                      axis.title.x=element_text(size=13,face="bold"),
#                      axis.title.y=element_text(size=13,face="bold")) +
#   labs(x="Educational level of household's head",y="Poverty Alleviation Index", title="Correlation: Poverty Alleviation Index & Educational Attainment")
# Poverty_Educ.fig <- plotly::ggplotly(Poverty_Educ.plot)
# Poverty_Educ.fig
# 
# plot_grid(Poverty_FS.plot, Poverty_Educ.plot, ncol=1)
# #ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/toUse_final_reverseScale/Poverty_Heath_Educ_corr.jpg"), width = 10, height = 10)
# 
# 
# #------------------------------------------------------------------------------------------------------#
# 
# # ---- b. Baseline social inequality (Point plots) ----
# 
# pd <- position_dodge(.05) # move them .05 to the left and right
# ##----Gender
# Gender.baseline <- DiD.data.baseline %>% 
#   mutate(Male = factor(Male)) %>% 
#   group_by(Male) %>% 
#   summarise(mean= mean(PovertyIndex_pca, na.rm=TRUE), 
#             sd = sd(PovertyIndex_pca, na.rm=TRUE), 
#             median = median(PovertyIndex_pca, na.rm=TRUE),
#             n=length(PovertyIndex_pca), 
#             se = sd/sqrt(n)) %>% 
#   mutate(Gender=ifelse(Male==0,"Female","Male"))
# 
# test.val <- round(t.test(DiD.data.baseline$PovertyIndex_pca~DiD.data.baseline$Gender)$statistic, digits = 3)
# 
# Gender.point.plot <- ggplot(Gender.baseline,aes(x=Gender,y=mean, label=paste0("n= ",n))) + 
#   geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.4, position = pd) +
#   geom_point(position = pd, aes(color=factor(Gender)), size=5, shape="square") + 
#   scale_color_manual(values = c("#00AFBB", "#E7B800")) +
#   #geom_point(aes(x=Gender,y=median), fill='red', shape=24, size=4) + 
#   geom_text(aes(y = mean+1.96*se + 0.02),position = position_dodge(0.9),vjust = 0, size=4) +
#   geom_line( position = pd) + theme_bw() +
#   theme(legend.position="none",
#         axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, vjust = 1),
#         axis.text.y=element_text(size=11, angle = 90, hjust = 0.5, vjust = 1),
#         axis.title.x=element_text(size=13,face="bold"),
#         axis.title.y=element_text(size=13,face="bold")) +
#   labs(x="",y="Poverty Alleviation Index", title="Gender (Female vs. Male) ", subtitle=paste0("Baseline means difference t-test = ",test.val))  
# Gender.point.plot
# #ggsave(paste0(resultPath, "Paper 1-MPA and Equity/results/2020/plots/baseline_inequality/Gender_baseIneq_nov2020",".jpg"))


