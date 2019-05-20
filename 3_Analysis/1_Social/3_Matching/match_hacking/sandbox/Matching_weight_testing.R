####Call relevant packages
#require(Matching) # matching package (called directly in line 106 rather than loaded)
require(geosphere)
require(rio)
require(sf)
require(tidyverse)

####### Do not distribute, share or cite without permission from Louise Glew (louise.glew@gmail.com) & David Gill (gill.david.a@gmail.com)###
##Covariates: 'lat','long', 'Depth', 'Habitat','Exposure','SHORE_DIST', 'MARKET_DIS','HPopDen100','ISO3','SurveyYear','Ecoregion','DepthErr', 'Exp_Error'
## Matching binary (treatment sites):INSIDE_AFT_GIS and Tr
###Identification variables: ECOID (sites), MPAID (MPAs)
# This is the code used to perform the matching analysis. The final output of this code is the matched ecological data used in the final analysis.
# Requires:
# Combined_Eco_data_22Dec17.csv (compiled (unmatched) ecological data); MPA_master_7Dec17.csv (MPA master dataset)


###################### --------------------Preparing data
# Directories
#setwd('C:/Users/dag71/Dropbox/data/analysis')
pcdir <- gsub("Dropbox.*","",getwd())
setwd(paste0(pcdir,'/Dropbox/data/analysis'))
inputdir <- "MPAderivatives/data/orig.fish.data/"
codedir <- "MPAderivatives/git/fish life history compilation/"
MPA_master<-import(paste0(inputdir,"MPA_master_7Dec17.csv"))


# get updated original data
source("MPAderivatives/git/update_orig_fish_data.R")
source('C:/Users/dag71/Dropbox/data/analysis/MPAderivatives/git/fish life history compilation/function_att_significance.R')

data <- orig.data %>% 
  left_join(MPA_master[,c("MPAID","Year")],by="MPAID") %>% 
#Updating variables dependant on MPA age from the Master file: MPA_Age and INSIDE_AFT_GIS (treatment sites)
  mutate(EstYear=Year,
         MPA_Age=SurveyYear-EstYear,                             
         # 'Inside' only if the MPA is at least 3 years old
         INSIDE_AFT_GIS=ifelse(INSIDE_GIS==1 & MPA_Age>=3,1,0),  
         # if no year data are available, assuming that it was inside after
         INSIDE_AFT_GIS=ifelse(INSIDE_GIS==1 & is.na(EstYear),1,INSIDE_AFT_GIS),
         # identify sites that are in MPAs that are too 'young' (1-2 years old)  
         YearBuffer=ifelse(INSIDE_GIS==1 & MPA_Age%in%c(1,2),1,0)) %>% 
         # removing sites with missing biomass data (n=2)
 filter(!is.na(sum_biomass)) %>% 
 #removing sites within the 1km spillover/buffer zone and those in recently established MPAs
 filter(X1KmBuffer!=1 & YearBuffer!=1) %>%
 #removing sites in whale sanctuaries (except the Hawaiian humpback sanctuary) given the ambiguity of whether or not to include them
 mutate(INSIDE_GIS=ifelse(INSIDE_GIS==1 & WhaleMPA==1,2,INSIDE_GIS),
        # but use Hawaiian humpback sanctuary sites as controls
        INSIDE_GIS=ifelse(MPAID==997,0,INSIDE_GIS),
        INSIDE_AFT_GIS=ifelse(MPAID==997,0,INSIDE_AFT_GIS)) %>%
#removing sites where habitat is unknown
 filter(Habitat!="Unknown" & !is.na(Habitat)) %>%  
# removing  sites where it was ambiguous whether or not they were inside or outside (e.g. whale MPAs)
 filter(INSIDE_GIS!=2)

###########################################################################################################################
# Create a binary vector to identify treatment/control sites
Tr<-as.vector(data$INSIDE_AFT_GIS)                          # should be 14096 with whales excluded and HIHWSanct included. 13790 w/o whales. 
# summary(data)
# summary(as.factor(data$INSIDE_AFT_GIS))                     # number of treatment and control sites

##########################################################################################################################################################
# Set thresholds or transform covariates, based on distributions and relationships with biomass shown in partial dependency plots from a random forest model

#summary(data$Ew)
data$EXPO<-data$Ew
data$EXPO[data$EXPO>quantile(data$Ew,probs=c(.95))]<-9999999           # matching avoids the extreme 5%, i.e. high exposed sites
#hist(data$EXPO,xlim = c(0,120),breaks = seq(0,10000000,5))

#ecdf(data$MARKET_DIS)(1000)
data$MRKT<-data$MARKET_DIS/1000                                        # convert to km
data$MRKT[data$MRKT>1000]=9999999                                     # matching focused on sites within 1000 km of market, beyond which there is no effect

#ecdf(data$SHORE_DIST)(20000)
data$SHORE<-data$SHORE_DIST/1000                                       # convert to km
data$SHORE[data$SHORE>20]<-9999999                                     # matching focused on sites within 20 km of shore,  <10% of data beyond this point, and little effect on biomass

#ecdf(data$chlomean)(1)
data$CHLORO<-data$chlomean
data$CHLORO[data$CHLORO>quantile(data$chlomean,probs=c(.95))]<-9999999 # matching excludes the extreme 5% 

#ecdf(data$HPopDen100)(1500000)
data$HPOP<-data$HPopDen100/1000000                                     # convert to human population (millions within 100km of site)
data$HPOP[data$HPOP>1.5]<-9999999                                      # matching excludes the extreme 5% 

####Create a matrix of the matching covariates
X<-data[,c('SiteLat','SiteLong', 'Depth','EXPO','SHORE', 'MRKT','HPOP','SurveyYear','sstmin','CHLORO','DepthErr2','Exp_Error', 'Habitat','ISO3',
           'Ecoregion','Source')]

####Convert factors to integers for matching process
X$Habitat=as.integer(as.factor(X$Habitat))
X$ISO3<-as.integer(as.factor(X$ISO3))
X$Ecoregion<-as.integer(as.factor(X$Ecoregion))
X$Source<-as.integer(as.factor(X$Source))
X$SurveyYear <- as.integer(as.factor(X$SurveyYear))
#X$DepthErr2 <- as.integer(as.factor(X$DepthErr2))
X$Exp_Error <- as.integer((X$Exp_Error))

X <- as.data.frame(sapply(X,as.integer))
str(X)
#summary(X)
X <- X[,-c(12)]
X <- X[,1:7]

Y <- log(data$sum_biomass)
W <- data %>% 
  mutate(MPAID=ifelse(INSIDE_AFT_GIS==1,MPAID,0)) %>% 
  add_count(MPAID) %>% 
  mutate(w=ifelse(INSIDE_AFT_GIS==1,1/n,1))  %>% 
  pull(w)
sum(W)

head(W)
# data$MPAID[data$INSIDE_AFT_GIS==0] <- NA
# Grp <- data$MPAID
M=2

m2full<-Matching::Match(Y=Y, Tr,X, M=M, replace=T, ties=T, BiasAdjust = T)
m2weight<-Matching::Match(Y=Y, Tr,X, M=M, replace=T, ties=T, BiasAdjust = T, weights=W)
m2weight$mdata$orig.weighted.treated.nobs
summary(m2weight)
t.stat = m2weight$est/m2weight$se
p.val = (1 - pnorm(abs(m2weight$est/m2weight$se))) * 2

m.t1t2<-Matching::Match(Y=Y[i], Tr,X, M=M)

ret <- data.frame(est=NA,se=NA,lowerci=NA)
for (i in 1:ncol(Y)){
  m.temp<-Matching::Match(Y=Y[i], Tr,X, M=M, match.out =m.t1t2)
  out.temp <- att.significance(m.temp)
  ret.temp <- data.frame(est=m.temp$est, se = m.temp$se, lowerci=out.temp$lower.ci)
  ret[i,] <- ret.temp
  return(ret)
} 
ret
source(paste0(codedir,"function_att_significance.R"))
att.significance(match.obj=m2full)
m2full$est.noadj;m2full$est;m2full$se.standard

summary(m2full)
##########################################################################################################################################################
# ------------------------------------------------------------- Matching --------------------------------------------------------------------
############################################################################################################################################################
#Malanobis matching
#names(X)
# Create caliper and say which matches should be exact
# Caliper controls permissable match. Depth: matches only sites surveyed within a moving 10m window, SurveyYear:3 year window, Latitude: 2 degrees
Xcaliper<-c(2/sd(X$SiteLat),100,10/sd(X$Depth),100,100,100,100,3/sd(X$SurveyYear),100,3,100,100,100,100,100,100)  # PAR included
# Exact matching on habitat, country,ecoregion and source
Xexact<-c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1)


### Run match algorithm (COVARIATE MATCHING (Malanobis) with calipers
## Always run with replace=TRUE, and ties=TRUE, vary M (# matches)
# m1<-Match(Y=NULL, Tr,X, M=1, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=TRUE)
# summary(m1)
m2full<-Matching::Match(Y=Y, Tr,X, M=2, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=F)
m2$est;m2$est.noadj;m2$se
summary(m2)
# m3<-Match(Y=NULL, Tr,X, M=3, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=TRUE)
# summary(m3)
# m4<-Match(Y=NULL, Tr,X, M=4, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=TRUE)
# summary(m4)
# m5<-Match(Y=NULL, Tr,X, M=5, caliper=Xcaliper, exact=Xexact, replace=TRUE, ties=TRUE)
# summary(m5)

# Are dropped sites geographically well distributed?
# Dropped <- data[m2$index.dropped,]
# coordinates(Dropped) <- ~SiteLong+SiteLat
# projection(Dropped) <- '+proj=longlat +datum=WGS84'
# map('world')
# plot(Dropped, add=T, col='blue')

#### Compute match balance statistics (don't need stats on exact-matched variables) ######
# Mahanobis
# mb1<-MatchBalance(Tr~SiteLat+SiteLong+Depth+EXPO+SHORE+MRKT+HPOP+SurveyYear+DepthErr2+Exp_Error+sstmin+CHLORO+par2,data=data,match.out=m1,ks=TRUE, nboots=1000,digits=3)
# mb2<-Matching::MatchBalance(Tr~SiteLat+SiteLong+Depth+EXPO+SHORE+MRKT+HPOP+SurveyYear+DepthErr2+Exp_Error+sstmin+CHLORO,data=data,match.out=m2,ks=TRUE, nboots=1000,digits=3)
# mb3<-MatchBalance(Tr~SiteLat+SiteLong+Depth+EXPO+SHORE+MRKT+HPOP+SurveyYear+DepthErr2+Exp_Error+sstmin+CHLORO+par2,data=data,match.out=m3,ks=TRUE, nboots=1000,digits=3)
# # Propensity score
# mb4<-MatchBalance(Tr~SiteLat+SiteLong+Depth+EXPO+SHORE+MRKT+HPOP+SurveyYear+DepthErr2+Exp_Error+sstmin+CHLORO+par2,data=data,match.out=mp1,ks=TRUE, nboots=1000,digits=3)
# mb5<-MatchBalance(Tr~SiteLat+SiteLong+Depth+EXPO+SHORE+MRKT+HPOP+SurveyYear+DepthErr2+Exp_Error+sstmin+CHLORO+par2,data=data,match.out=mp2,ks=TRUE, nboots=1000,digits=3)
# mb6<-MatchBalance(Tr~SiteLat+SiteLong+Depth+EXPO+SHORE+MRKT+HPOP+SurveyYear+DepthErr2+Exp_Error+sstmin+CHLORO+par2,data=data,match.out=mp3,ks=TRUE, nboots=1000,digits=3)

############################################################################################################################################################
# get row indices and then ECOIDs
i.treat<-m2$index.treated
i.control<-m2$index.control  
MPA.pairs <- as.data.frame(cbind(data[i.treat,c("ECOID")],data[i.control,c("ECOID")]))
names(MPA.pairs) <- c('treat.id','ctrl.id')

# join Tr and Ctrl values, calc. response ratios
matched.data<-as.data.frame(cbind(
  data[i.treat,c("ECOID","SurveyID","Site","SiteLat","SiteLong","mpa_id","MPAID","MPA","No_take","sum_biomass","SurveyYear","Source")],
  data[i.control,c("ECOID","sum_biomass")])) 
names(matched.data) <- c('ECOID','SurveyID',"site",'lat','long',"mpa_id",'MPAID',"MPA",'no_take','t.biomass','srvy.yr','source',
                      'ctrl.id','c.biomass')
matched.data <- matched.data %>% 
  #left_join(mpa.gis,by="MPAID")
  mutate(lnRR=log(t.biomass/c.biomass)) %>% 
  dplyr::select(ECOID,SurveyID,site,mpa_id,MPAID,MPA,lat,long,srvy.yr,no_take,lnRR,t.biomass,ctrl.id,c.biomass,source) 

# Distance between control and treatment sites
T.Loc<-data[i.treat,c('SiteLong','SiteLat')]
C.Loc<-data[i.control,c('SiteLong','SiteLat')]
matched.data$match.dist.km<-(distGeo(T.Loc,C.Loc))/1000

# site-level data
site.data <- data[c("ECOID","SurveyID","Site","SiteCode","SiteCode1","SiteLat","SiteLong","mpa_id",'MPAID',"WDPAID","MPA","ISO3","Country","No_take","sum_biomass",
                    "SurveyYear","Source","Depth","Habitat","MARKET_DIS","SHORE_DIST","Ecoregion","HPopDen100",
                    "Ew","sstmean","sstmin","parmean","chlomean","DepthErr2","Exp_Error")]
names(site.data) <- c('ECOID','SurveyID',"site_code","site_code1","site",'lat','long',"mpa_id",'MPAID',"WDPAID","MPA","iso3","country",'no_take','sum_biomass',
                      'srvy.yr','source',"depth","habitat","mrkt.dist","shre.dist","ecoregion","hpop100",
                      "wave.exp","sstmean","sstmin","parmean","chlomean","depthErr2","expErr")

#chck names line up
# cbind(names(site.data),
#       c('ECOID','SurveyID',"site",'lat','long',"mpa_id",'MPAID',"WDPAID","MPA","iso3","country",'no_take','sum_biomass',
#   'srvy.yr','source',"depth","habitat","mrkt.dist","shre.dist","ecoregion","hpop100",
#   "wave.exp","sstmean","sstmin","parmean","chlomean","depthErr2","expErr"))

rm(X,m2,data,i.treat,i.control,Tr,Xcaliper,Xexact,inputdir,MPA_master,T.Loc,C.Loc)
#head(MPA.pairs)
#head(matched.data)
