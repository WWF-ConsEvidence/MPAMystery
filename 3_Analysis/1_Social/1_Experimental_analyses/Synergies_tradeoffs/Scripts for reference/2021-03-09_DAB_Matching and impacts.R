# ---
# Matching MPA sites with control sites
# ---

# Set working directory

rm(list=ls())
setwd("/Users/andradi-brown/Dropbox (MPAMystery)/MPAMystery Team Folder/Cross_Cutting/Manuscripts/In Progress/2020_Synergies and tradeoffs/Eco impact analysis/matching")

# Read in data      
df1 <- read.csv('2020-12-22_DAB_Contextual variables with fish and benthic data.csv', header=T, stringsAsFactors = FALSE)



library(Matching)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(stats)
library(corrplot)
library(tidyr)
library(plyr)
library(dplyr)
library('Hmisc')
library("RItools")
library('MatchIt')


str(df1)
head(df1)


# check sample sizes in each MPA ("None" are Control sites)
table(df1$MPA)
table(df1$Type_of_Zone)

aggregate(Site_ID ~ MPA_Name + Type_of_Zone, data=df1, FUN=length)


# ================================================================================================
#
# section 1: incorporate values from partial dependence plots for continuous variables 
# coding the factors correctly
# ================================================================================================
#
# Based on partial dependence plots for distance to market, settlement, mangroves, etc
# re-code continuous variables and turn factor variables into numeric:

# Longitude
df1$Lon_new <- as.numeric(df1$Lon)

# Reef slope
df1$reef_slope_new <- as.numeric(factor(df1$Reef_Slope, levels=c('Flat', 'Slope', 'Wall')))

# Pollution threat
df1$pol_new <- as.numeric(factor(df1$pollution_threat, levels=c('Low', 'Medium', 'High')))

# Time series length
df1$t_length <- df1$t1 - df1$t0


# Airport distance
df1$Airport_Distance_km_new <- cut(as.numeric(df1$Airport_Distance_km),
                                   breaks=c(seq(0, 50, 50), seq(150, 350, 100)),
                                  labels=FALSE)

# Mangrove distance
df1$mangrove_dist_km_new <- cut(as.numeric(df1$mangrove_dist_km),
                                breaks=c(0, 5, 15, 40, 100),
                                labels=FALSE)


# Distance to deep water
df1$deepwater_dist_km_new <- cut(as.numeric(df1$deepwater_dist_km),
                                breaks=c(-0.1, 0.25, 0.5, 2, 10),
                               labels=FALSE)



# Wave mean
df1$wave_mean_new <- as.numeric(df1$wave_mean)

# ================================================================================================
#
#  2) match model & balance statistics, save output
# 
# ================================================================================================

# removing any sites with NA for key data
df1 <- df1[complete.cases(df1),] # keep only the complete rows (without any NAs)

# We want to compare sites inside MPA (NTZ + Use) to outside (Control) --> Treat_out (0,1)

df1$Treat_out <- NA
df1$Treat_out[df1$Type_of_Zone %in% c("Use","NTZ")]<-1 # assign 1=treatment
df1$Treat_out[df1$Type_of_Zone %in% c("Control")]<-0   # assign 0=outside MPA

table(df1$Treat_out)                                    # 78 control, 230 treatment
Treat_out <- df1$Treat_out



####
# Generate propensity score
####

# using logistic regression
ps <- glm(Treat_out ~ Lon_new +
            reef_slope_new +
            Airport_Distance_km_new +
            pop2015_20km +
            wave_mean_new +
            pol_new +
            deepwater_dist_km_new +
            mangrove_dist_km_new +
            t0 +
            t_length,
          data =df1, family = binomial())
summary(ps)

# store the propensity score values
df1$psvalue <- predict(ps, type = "response")


####
# Matching
###

m.nn <- matchit(Treat_out ~ Lon_new +
                  reef_slope_new +
                  Airport_Distance_km_new +
                  pop2015_20km +
                  wave_mean_new +
                  pol_new +
                  deepwater_dist_km_new +
                  mangrove_dist_km_new +
                  t0 +
                  t_length,
                  data=df1,
                  method="nearest", ratio = 2,
                caliper = 0.25,
                replace=TRUE)
####
# Examine match balance and extract
####

summary(m.nn)

temp <- summary(m.nn)

#write.csv(temp$sum.all, paste(Sys.Date(), "DAB", "Eco impact balance pre-matching.csv", sep="_"), row.names = TRUE)
#write.csv(temp$sum.matched, paste(Sys.Date(), "DAB", "Eco impact balance post-matching.csv", sep="_"), row.names = TRUE)


# outputting the matched dataset
match.data <- match.data(m.nn, drop.unmatched=TRUE)
paired.match.data <- get_matches(m.nn)

# Identifying the matched pairs

matches <- data.frame(m.nn$match.matrix)

# Need to rep the MPA sites to then align with the second control site given 1:2 matching
MPA.sites <- rep(match(row.names(matches), row.names(match.data)), 2)
Control.sites <- c(match(matches[,1], row.names(match.data)), match(matches[,2], row.names(match.data)))

# Examining sites that were matched and frequency of use for controls
table(match.data$Site_ID[MPA.sites])
sort(table(match.data$Site_ID[Control.sites]))



# Examining how many sites per MPA and split of NTZ vs Use
sites <- data.frame(MPA_Name=match.data$MPA_Name[MPA.sites],
                    Type_of_Zone=match.data$Type_of_Zone[MPA.sites],
                    Site_ID=match.data$Site_ID[MPA.sites])


sites <- distinct(sites)

aggregate(Site_ID ~ MPA_Name + Type_of_Zone, data=sites, FUN=length)


##
# Plotting the balance
##

#jpeg("Match balance.jpeg",
#     width=800, height=1200, quality=75,
#     pointsize = 20)

par(mfrow=c(3,1))

## Pre-matching balance
histbackback(split(df1$psvalue, df1$Treat_out),
             main= "Propensity score before matching",
             xlab=c("control", "treatment"),
             probability = TRUE, brks=c(seq(0, 1, 0.1)), xlim=c(-5, 5))

## Post-matching balance

histbackback(split(paired.match.data$psvalue,
                   paired.match.data$Treat_out),
             main= "Propensity score after matching", xlab=c("control", "treatment"),
             probability = TRUE, brks=c(seq(0, 1, 0.1)), xlim=c(-5, 5))


#plot(m.nn, type = "jitter")

#dev.off()


#####
# Computing indices of covariate imbalance pre- vs. post- matching
######

# calculation function
calc <- function(x)
{100*(mean(x[treated1])- mean(x[!treated1]))/(sqrt(0.5*(var(x[treated1]) + var(x[!treated1]))))}


### A. Standardized difference post matching
treated1 <- (paired.match.data$Treat_out==1)
cov1 <- paired.match.data[ , c("Lon_new",
                        "reef_slope_new",
                      "Airport_Distance_km_new",
                      "pop2015_20km",
                      "wave_mean_new",
                      "pol_new",
                      "deepwater_dist_km_new",
                      "mangrove_dist_km_new",
                      "t0",
                      "t_length")]


std.diff.post <- apply(cov1, 2, calc) 

# Standardized difference post-matching
round(abs(std.diff.post), 1)

# Chi-squared balance test
xBalance(Treat_out ~ Lon_new +
           reef_slope_new +
           Airport_Distance_km_new +
           pop2015_20km +
           wave_mean_new +
           pol_new +
           deepwater_dist_km_new +
           mangrove_dist_km_new +
           t0 +
           t_length,
         data = paired.match.data, report = c("chisquare.test"))


### B. Standardized difference pre matching
treated1 <- (Treat_out==1)
cov1 <- df1[ , c("Lon_new",
                        "reef_slope_new",
                        "Airport_Distance_km_new",
                        "pop2015_20km",
                        "wave_mean_new",
                        "pol_new",
                        "deepwater_dist_km_new",
                        "mangrove_dist_km_new",
                        "t0",
                        "t_length")]


std.diff.pre <- apply(cov1, 2, calc) 

# Standardized difference pre-matching
round(abs(std.diff.pre), 1)

# Chi-squared balance test
xBalance(Treat_out ~ Lon_new +
           reef_slope_new +
           Airport_Distance_km_new +
           pop2015_20km +
           wave_mean_new +
           pol_new +
           deepwater_dist_km_new +
           mangrove_dist_km_new +
           t0 +
           t_length,
         data = df1, report = c("chisquare.test"))


### Combining the standardized difference

std.diff <- data.frame(Pre=std.diff.pre, Post=std.diff.post)

#write.csv(std.diff, paste(Sys.Date(), "DAB", "Eco impact matching variate standardized difference.csv", sep="_"), row.names = TRUE)



# ================================================================================================
#
#  3) Post-hoc regression adjustment
# 
# ================================================================================================

#' Steps
#' 1. Calculate outcomes for each site (t1 - t0 for each site)
#' 2. Regress outcome as a function of covariates just for included control sites
#' 3. Use generated regression model to predict outcome data for full dataset (both MPA and control sites)
#' 4. Take raw difference in predicted outcomes for each pair of MPA and control sites as the impact
#' 5. Repeat the above steps for each outcome  of interest (e.g. total fish biomass, herbivore fish biomass, hard coral cover)


# Fish impact function
# requires matched data, then t0 and t1 biomass, also runs based on getImpact function defined below
fishImpact <- function(match.data, t0=t0_total_biomass_kg_ha, t1=t1_total_biomass_kg_ha,
                       MPA.sites = MPA.sites, Control.sites = Control.sites)
  {
  # 1. Calculate raw outcomes for each site (t1 - t0 for each site)

  match.data$raw_outcome <- match.data[[t1]] - match.data[[t0]]

  #' 2. Regress outcome as a function of covariates just for included control sites
  # maintain sign but third root transform biomass outcome
  match.data$raw_outcome_tr <- sign(match.data$raw_outcome) * (abs(match.data$raw_outcome))^(1/3)

  # fit glm
  mod1 <- glm(raw_outcome_tr ~ Lon_new +
             reef_slope_new +
             Airport_Distance_km_new +
             pop2015_20km +
             wave_mean_new +
             pol_new +
             deepwater_dist_km_new +
             mangrove_dist_km_new +
             t0 +
             t_length,
           data = match.data[which(match.data$Treat_out==0), ],
           family=gaussian(link="identity"))

  #plot model
  par(mfrow=c(2,2))
  plot(mod1)

  #' 3. Use generated regression model to predict outcome data for full dataset (both MPA and control sites)
  match.data$adj.outcome <- predict(mod1, newdata=match.data)

  # back transform the outcome
  match.data$adj.outcome <- sign(match.data$adj.outcome) * abs(match.data$adj.outcome)^3

  #' 4. Take raw difference in predicted outcomes for each pair of MPA and control sites as the impact
  
  # MPA predicted outcomes
  MPA.sites <- match.data[MPA.sites, c('MPA_Name', 'Site_ID', 'Type_of_Zone', 'raw_outcome', 'adj.outcome' )]
  # Control predicted outcomes
  Control.sites <- match.data[Control.sites, c('Site_ID', 'raw_outcome', 'adj.outcome' )]
  
  
  # Impact (difference between MPA and Control raw minus difference between adjustment)
  MPA.sites$adj.impact <- MPA.sites$raw_outcome - Control.sites$raw_outcome
  MPA.sites$adj.impact <- MPA.sites$adj.impact - MPA.sites$adj.outcome - Control.sites$adj.outcome
  
  MPA.sites  <- MPA.sites[complete.cases(MPA.sites), ]
  
  # Mean and into final dataframe
  
  MPA.sites <- aggregate(adj.impact ~ MPA_Name + Type_of_Zone + Site_ID, data=MPA.sites, FUN=mean)
  return(MPA.sites)

  } # closes the fishImpact function

# Benthic impact function
# requires matched data, then t0 and t1 biomass, also runs based on getImpact function defined below
benthicImpact <- function(match.data=match.data, t0='t0_hard_coral_cover', t1='t1_hard_coral_cover',
                       MPA.sites = MPA.sites, Control.sites = Control.sites)
{
  # 1. Calculate raw outcomes for each site (t1 - t0 for each site)
  
  match.data$raw_outcome <- match.data[[t1]] - match.data[[t0]]
  
  #' 2. Regress outcome as a function of covariates just for included control sites

  # fit glm
  mod1 <- glm(raw_outcome ~ Lon_new +
                reef_slope_new +
                Airport_Distance_km_new +
                pop2015_20km +
                wave_mean_new +
                pol_new +
                deepwater_dist_km_new +
                mangrove_dist_km_new +
                t0 +
                t_length,
              data = match.data[which(match.data$Treat_out==0), ],
              family=gaussian(link="identity"))
  
  #plot model
  par(mfrow=c(2,2))
  plot(mod1)
  
  #' 3. Use generated regression model to predict outcome data for full dataset (both MPA and control sites)
  match.data$adj.outcome <- predict(mod1, newdata=match.data)
  
  
  # Calculate adjusted impact
  
  
  #' 4. Take raw difference in predicted outcomes for each pair of MPA and control sites as the impact
  
  # MPA predicted outcomes
  MPA.sites <- match.data[MPA.sites, c('MPA_Name', 'Site_ID', 'Type_of_Zone', 'raw_outcome', 'adj.outcome' )]
  # Control predicted outcomes
  Control.sites <- match.data[Control.sites, c('Site_ID', 'raw_outcome', 'adj.outcome' )]
  
  
  # Impact (difference between MPA and Control raw minus difference between adjustment)
  MPA.sites$adj.impact <- MPA.sites$raw_outcome - Control.sites$raw_outcome
  MPA.sites$adj.impact <- MPA.sites$adj.impact - MPA.sites$adj.outcome - Control.sites$adj.outcome
  
  MPA.sites  <- MPA.sites[complete.cases(MPA.sites), ]
  
  # Mean and into final dataframe
  
  MPA.sites <- aggregate(adj.impact ~ MPA_Name + Type_of_Zone + Site_ID, data=MPA.sites, FUN=mean)
  return(MPA.sites)
} # closes the function





####
# Hard coral cover
#####

coral.cover <- benthicImpact(match.data = match.data,
                            t0='t0_hard_coral_cover',
                            t1='t1_hard_coral_cover',
                            MPA.sites = MPA.sites,
                            Control.sites = Control.sites)

####
# Macroalage  cover
#####

algae.cover <- benthicImpact(match.data = match.data,
                             t0='t0_macroalgae_cover',
                             t1='t1_macroalgae_cover',
                             MPA.sites = MPA.sites,
                             Control.sites = Control.sites)

####
# Total fish biomass
#####

total.biomass <- fishImpact(match.data = match.data,
                            t0='t0_total_biomass_kg_ha',
                            t1='t1_total_biomass_kg_ha',
                            MPA.sites = MPA.sites,
                            Control.sites = Control.sites)

###
# Herbivore fish biomass
###

herb.biomass <- fishImpact(match.data = match.data,
                            t0='t0_herb_biomass_kg_ha',
                            t1='t1_herb_biomass_kg_ha',
                           MPA.sites = MPA.sites,
                           Control.sites = Control.sites)


###
# Key fish biomass
###

key.biomass <- fishImpact(match.data = match.data,
                           t0='t0_key_biomass_kg_ha',
                           t1='t1_key_biomass_kg_ha',
                          MPA.sites = MPA.sites,
                          Control.sites = Control.sites)




# ================================================================================================
#
#  4) Exporting ecological impacts based on matched pairs
# 
# ================================================================================================

colnames(coral.cover)[4] <- 'coral.impact'
colnames(algae.cover)[4] <- 'algae.impact'

colnames(total.biomass)[4] <- 'total.biomass.impact'
colnames(herb.biomass)[4] <- 'herb.biomass.impact'
colnames(key.biomass)[4] <- 'key.biomass.impact'


eco.impacts <- left_join(coral.cover, algae.cover)
eco.impacts <- left_join(eco.impacts, total.biomass)
eco.impacts <- left_join(eco.impacts, herb.biomass)
eco.impacts <- left_join(eco.impacts, key.biomass)

# View eco impacts per MPA and zone
aggregate(total.biomass.impact ~ MPA_Name + Type_of_Zone, data=eco.impacts, FUN=mean)
aggregate(herb.biomass.impact ~ MPA_Name + Type_of_Zone, data=eco.impacts, FUN=mean)
aggregate(key.biomass.impact ~ MPA_Name + Type_of_Zone, data=eco.impacts, FUN=mean)

#write.csv(eco.impacts, paste(Sys.Date(), "DAB", "Adjusted ecological impacts.csv", sep="_"), row.names = TRUE)



