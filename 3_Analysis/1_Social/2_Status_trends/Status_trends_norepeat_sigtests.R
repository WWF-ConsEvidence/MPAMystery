# 
# code:  Status Significance Tests, for data with no repeat
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: October 2019
# 
# 
# ---- code sections ----
#  1) Import Data
#  2) Define Lists of Settlements, to be used in functions
#  3) Plot Variable Distributions (to test normality assumption)
#  4) Non-parametric Significance Test Functions (using Mann-Whitney U test)
#  5) Chi-square Tests for Categorical Variables
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Import and Subset Data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Subset datasets ----

# - "MPA Household Data" dataset -- includes household level data for only treatment households
MPA.HHData <- 
  HHData %>%
  filter(Treatment==1) %>% 
  mutate(SettlementName=factor(SettlementName))


# - "MPA versus Control" dataset -- includes household level data for treatment and control
MPA.v.Control <- 
  HHData


# - "MPA Settlement Means" dataset -- includes settlement level data for only treatment settlements
MPA.Sett.Means <-
  if(MPA.name$MPAID==21) {
    Sett.Level.Means.byZone
  } else { Sett.Level.Means %>% filter(Treatment==1)
}

# Removing the settlement with an NA in order for function to run
MPA.Sett.Means <- 
  MPA.Sett.Means[!is.na(MPA.Sett.Means$SettlementName),]


# ---- 1.2 Define list of settlement names in MPA ----

sett.names <- factor(MPA.Sett.Means$SettlementName)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Define Lists of Settlements, to be used in functions ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Create list of median settlement for each continuous variable (whether the variable is parametric or non-parametric) ----

even.number.setts.function <- 
  mapply(a=MPA.Sett.Means[,c("FSMean","MAMean","MTMean","PAMean","SEMean","TimeMarketMean","UnwellMean")],
         b=MPA.HHData[,c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell")],
         function(a,b){
           med <- median(a,na.rm=T)
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           sett.equal.med <- c(MPA.Sett.Means$SettlementName[which(a==med)])
           upper.sett <- MPA.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)==2,
                                ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)==3,
                                       ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[MPA.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       ifelse(length(upper.sett)>3,
                                              as.character(upper.sett[1]),
                                              as.character(upper.sett))))
           lower.sett <- MPA.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)==2,
                                ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)==3,
                                       ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[MPA.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[MPA.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[MPA.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       ifelse(length(lower.sett)>3,
                                              as.character(lower.sett[1]),
                                              as.character(lower.sett))))
           sett.equal.med <- ifelse(length(sett.equal.med)==2,
                                    ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)==3,
                                           ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[MPA.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[MPA.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[MPA.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[MPA.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(length(sett.equal.med)>3,
                                                  as.character(sett.equal.med[1]),
                                                  ifelse(is.na(sett.equal.med),
                                                         NA,
                                                         as.character(sett.equal.med)))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[MPA.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[MPA.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[MPA.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

median.setts <- 
  mapply(i=MPA.Sett.Means[,c("FSMean","MAMean","MTMean","PAMean","SEMean","TimeMarketMean","UnwellMean")],
         j=names(even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(sett.names)%%2!=0,
                                      as.character(MPA.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(even.number.setts.function[j])),
                               levels=levels(sett.names))})


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Plot Variable Distributions (to test normality assumption) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# # ---- 3.1 Food security score distribution ----
# 
# dist.FS <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=FSIndex,y=..density..),
#                  bins=5,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$FSIndex),
#                                     sd=sd(MPA.HHData$FSIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Food Security Scores\n(per household)",
#        y="Density",
#        title=paste("Food Security Score Distribution",unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.FS
# 
# qqnorm(MPA.HHData$FSIndex)
# qqline(MPA.HHData$FSIndex,col="green")
# 
# 
# # ---- 3.2 Material assets score distribution ----
# 
# dist.MA <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=MAIndex,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$MAIndex),
#                                     sd=sd(MPA.HHData$MAIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Material Assets\n(per household)",
#        y="Density",
#        title=paste("HH Material Assets Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.MA
# 
# log.MA <- log(MPA.HHData$MAIndex[MPA.HHData$MAIndex!=0])
# 
# qqnorm(log.MA)
# qqline(log.MA,col="green")
# 
# 
# # ---- 3.3 Place attachment score distribution ----
# 
# dist.PA <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=PAIndex,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$PAIndex),
#                                     sd=sd(MPA.HHData$PAIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Place Attachment Score\nPer Household",
#        y="Density",
#        title=paste("Place Attachment Score Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.PA
# 
# qqnorm(MPA.HHData$PAIndex)
# qqline(MPA.HHData$PAIndex,col="green")
# 
# 
# # ---- 3.4 Marine tenure score distribution ----
# dist.MT <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=MTIndex,y=..density..),
#                  binwidth=1,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$MTIndex),
#                                     sd=sd(MPA.HHData$MTIndex)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Marine Tenure Score\n(per household)",
#        y="Density",
#        title=paste("Marine Tenure Score Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.MT
# 
# qqnorm(MPA.HHData$MTIndex)
# qqline(MPA.HHData$MTIndex,col="green")
# 
# 
# # ---- 3.5 School enrollment rate distribution ----
# 
# dist.SE <-
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=SERate,y=..density..),
#                  bins=15,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$SERate),
#                                     sd=sd(MPA.HHData$SERate)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="School Enrollment Rate\n(per household)",
#        y="Density",
#        title=paste("School Enrollment Rate Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.SE
# 
# qqnorm(MPA.HHData$SERate)
# qqline(MPA.HHData$SERate,col="green")
# 
# 
# # ---- 3.6 Time to market distribution ----
# 
# dist.TimeMarket <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=TimeMarket,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$TimeMarket),
#                                     sd=sd(MPA.HHData$TimeMarket)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Average Time to Market\n(in hours)",
#        y="Density",
#        title=paste("Time to Market Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.TimeMarket
# 
# qqnorm(MPA.HHData$TimeMarket)
# qqline(MPA.HHData$TimeMarket,col="green")
# 
# 
# # ---- 3.7 Days unwell distribution ----
# 
# dist.DaysUnwell <- 
#   ggplot(MPA.HHData) +
#   geom_histogram(aes(x=DaysUnwell,y=..density..),
#                  bins=20,fill="#5971C7",colour="#262F52",na.rm=T) +
#   stat_function(fun=dnorm,args=list(mean=mean(MPA.HHData$DaysUnwell),
#                                     sd=sd(MPA.HHData$DaysUnwell)),
#                 colour="#262F52",size=1,na.rm=T) +
#   labs(x="Days Unwell\n(per household)",
#        y="Density",
#        title=paste("HH Days Unwell Distribution", unique(MPA.HHData$InterviewYear),sep=", ")) +
#   dist.plot.theme
# dist.DaysUnwell
# 
# qqnorm(MPA.HHData$DaysUnwell)
# qqline(MPA.HHData$DaysUnwell,col="green")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Non-parametric Significance Test Functions (using Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# NOTE: Typically, food security, material assets, marine tenure, enrollment rate, and place attachment end up being non-parametric; 
#       however, if the distribution appears to be normal, then PARAMETRIC tests are more powerful and the better choice.


# ---- 4.1 Create function that will output significance values for non-parametric variables, BY SETTLEMENT ----
#          (for status plots)
non.parametric.test.settlements <- 
  data.frame(mapply(a=c("FSMean","MAMean","MTMean","PAMean","SEMean","TimeMarketMean","UnwellMean"),
                    b=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a,b){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(sett.names[which(sett.names!=median.setts[a])]),
                                                               as.character(median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=sett.names[which(sett.names!=median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       MPA.HHData[MPA.HHData$SettlementName==i | MPA.HHData$SettlementName==median.setts[a],b]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=MPA.HHData[MPA.HHData$SettlementName==i | MPA.HHData$SettlementName==median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


# - Alphabetize each column of settlement names.  Now all settlement names are in same order.
sigvals.Sett  <- 
  cbind.data.frame(non.parametric.test.settlements[order(non.parametric.test.settlements$"FSMean.SettlementName"),
                                                   c("FSMean.SettlementName","FSMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"MAMean.SettlementName"),
                                                   c("MAMean.SettlementName","MAMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"MTMean.SettlementName"),
                                                   c("MTMean.SettlementName","MTMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"PAMean.SettlementName"),
                                                   c("PAMean.SettlementName","PAMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"SEMean.SettlementName"),
                                                   c("SEMean.SettlementName","SEMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"TimeMarketMean.SettlementName"),
                                                   c("TimeMarketMean.SettlementName","TimeMarketMean.p.value")],
                   non.parametric.test.settlements[order(non.parametric.test.settlements$"UnwellMean.SettlementName"),
                                                   c("UnwellMean.SettlementName","UnwellMean.p.value")])

# - Remove all settlement name columns except for one. 
sigvals.Sett <- 
  sigvals.Sett %>%
  select(FSMean.SettlementName, FSMean.p.value, MAMean.p.value, MTMean.p.value, 
         PAMean.p.value, SEMean.p.value, TimeMarketMean.p.value, UnwellMean.p.value) %>%
  rename(SettlementName = FSMean.SettlementName, FS.pval = FSMean.p.value, MA.pval = MAMean.p.value, MT.pval = MTMean.p.value,
         PA.pval = PAMean.p.value, SE.pval = SEMean.p.value, TimeMarket.pval = TimeMarketMean.p.value, Unwell.pval = UnwellMean.p.value) %>%
  .[rev(order(.$SettlementName)),]


# ---- 4.2 Create function that will output significance values for non-parametric variables, MPA VS. Control ----
#          (for status plots, comparing MPA households to control households)

non.parametric.test.MPAvControl  <-
  if(MPA.name$MPAID==21) {
    data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                      function(a){
                        var <- MPA.v.Control[,a]
                        wilcox.test(var~Zone,
                                    data=MPA.v.Control,
                                    exact=F)}))["p.value",]
  } else {
    data.frame(mapply(a=c("FSIndex","MAIndex","MTIndex","PAIndex","SERate","TimeMarket","DaysUnwell"),
                    function(a){
                      var <- MPA.v.Control[,a]
                      wilcox.test(var~Treatment,
                                  data=MPA.v.Control,
                                  exact=F)}))["p.value",]
    }

sigvals.MPA  <- 
  if(MPA.name$MPAID==21) {
    cbind.data.frame("No Take Settlements",non.parametric.test.MPAvControl)
    } else {
      cbind.data.frame(MPA.name$MPAName,non.parametric.test.MPAvControl)
  } 
      
colnames(sigvals.MPA) <- colnames(sigvals.Sett)

null.row.sigvals  <- 
  matrix(rep(NA,length(sigvals.MPA)),ncol=length(sigvals.MPA),
         dimnames=list(1,colnames(sigvals.Sett)))

# - Define data frame with p-values for status plots
#   (households in each settlement are compared to those in the median settlement for the given variable,
#   using Mann Whitney U-Test -- so, interpretation is "compared to the median settlement, this settlement 
#   [is/is not] significantly different")
# 
#   (for MPA p-values, households in the MPA were compared to those in the control settlements (for the MPA),
#   also using Mann-Whitney U test)

sigvals <- rbind.data.frame(sigvals.MPA,
                            null.row.sigvals,
                            sigvals.Sett)


sigvals[,c("FS.pval", "MA.pval", "MT.pval" , "PA.pval", "SE.pval", "TimeMarket.pval", "Unwell.pval")] <- 
  unlist(sigvals[,c("FS.pval", "MA.pval", "MT.pval", "PA.pval","SE.pval","TimeMarket.pval","Unwell.pval")])
