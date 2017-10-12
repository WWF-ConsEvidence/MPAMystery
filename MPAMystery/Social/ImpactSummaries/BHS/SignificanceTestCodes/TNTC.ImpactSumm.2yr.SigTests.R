# 
# code:  Cenderawasih 2Yr Impact Summary Statistical Tests
# 
# github: kaclaborn/WWF-conservation-evidence/MPAMystery/Social/ImpactSummaries/BHS/SignificanceTestCodes
# --- Duplicate all code from "Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: October 2017
# 
# 
# ---- inputs ----
#  Dependencies: BHS_MPA_Mystery.R
#                Matching_2yr_impacts.R
#                BHS_2yr_impact_data.R
#                Function_summarise_bigfive_impacts.R
#                
# 
# ---- code sections ----
#  1) Data Configuration and Subsetting
#  2) Statistical Analysis (Mann-Whitney U test)
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Data Configuration and Subsetting ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Datasets for MPA impact summary plots ----

TNTC.BigFive.summary <- summarise.bigfive(impact.2yr[impact.2yr$MPAID==2 & !is.na(impact.2yr$MPAID),])

# - Status & trend plot dataset, using matched data
TNTC.2yr.status.trend <- TNTC.BigFive.summary[1:4,]


# - Impact plot dataset
TNTC.2yr.impacts <- TNTC.BigFive.summary[5:6,c(1,3:12)]
  
# - Snapshot impact plots (all five indicators on one plot, standardized impacts)
TNTC.2yr.std.impacts <- std.impact.2yr[std.impact.2yr$MPA=="Teluk Cenderawasih National Park",]
TNTC.2yr.std.impacts$MPA <- factor(TNTC.2yr.std.impacts$MPA,
                                   labels="TNTC")


# ---- 1.2 Dataset for statistical analysis ----

TNTC.BigFive.t0 <- cbind.data.frame(Year="Baseline",
                                    impact.2yr[impact.2yr$MPAID==2 & !is.na(impact.2yr$MPAID),
                                               colnames(impact.2yr[grep("mpa.t0",colnames(impact.2yr))])])
colnames(TNTC.BigFive.t0) <- c("Year","FS","MA","PA","MT","SE")

TNTC.BigFive.t2 <- cbind.data.frame(Year="2 Year Post",
                                    impact.2yr[impact.2yr$MPAID==2 & !is.na(impact.2yr$MPAID),
                                                         colnames(impact.2yr[grep("mpa.t2",colnames(impact.2yr))])])
colnames(TNTC.BigFive.t2) <- c("Year","FS","MA","PA","MT","SE")

TNTC.BigFive <- rbind.data.frame(TNTC.BigFive.t0,TNTC.BigFive.t2)
  

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Statistical Analysis (Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Mann-Whitney U test for significant MPA differences in Big Five between baseline and 2 year post ----

TNTC.wilcox.MPAimpact.summ.base.2yr <- as.data.frame(sapply(TNTC.BigFive[,c(2:6)],
                                                            function(i){
                                                              wilcox.test(i~Year,
                                                                          data=TNTC.BigFive,
                                                                          exact=F)
                                                            }))
TNTC.wilcox.MPAimpact.summ.base.2yr <- data.frame(Indicator=colnames(TNTC.wilcox.MPAimpact.summ.base.2yr),
                                                  p.val=sapply(TNTC.wilcox.MPAimpact.summ.base.2yr,
                                                               function(i){
                                                                 c(i[[3]])
                                                               }))
TNTC.wilcox.MPAimpact.summ.base.2yr$Indicator <- factor(TNTC.wilcox.MPAimpact.summ.base.2yr$Indicator,
                                                        levels=c("PA","SE","MT","FS","MA"),
                                                        ordered=T)
TNTC.wilcox.MPAimpact.summ.base.2yr <- TNTC.wilcox.MPAimpact.summ.base.2yr[order(TNTC.wilcox.MPAimpact.summ.base.2yr$Indicator),]


# ---- 2.2 Create plot labels for impact summary plots, indicating significance (***) ----

TNTC.sig.labs <- data.frame(xaxis.st.baselabs=rep(paste("Baseline\nscore\n","(",
                                                        unique(BigFive$InterviewYear[BigFive$MPAID==2 & 
                                                                                       BigFive$MonitoringYear=="Baseline"]),")",sep=""),5),
                            xaxis.st.2yrlabs=mapply(i=TNTC.wilcox.MPAimpact.summ.base.2yr$p.val,
                                                    j=rep(unique(BigFive$InterviewYear[BigFive$MPAID==2 & BigFive$MonitoringYear=="2 Year Post"]),5),
                                                    function(i,j){
                                                      c(ifelse(i<0.01,paste("Two year\npost-baseline\n","(",j,")","***",sep=""),
                                                               ifelse(i<0.05 & i>=0.01,paste("Two year\npost-baseline\n","(",j,")","**",sep=""),
                                                                      ifelse(i<0.1 & i>=0.05,paste("Two year\npost-baseline\n","(",j,")","*",sep=""),
                                                                             paste("Two year\npost-baseline\n","(",j,")",sep="")))))
                                                    }),
                            snapshot.labs=mapply(i=p.vals.tntc[order(p.vals.tntc$Indicator),"p.val"],
                                                 j=c("Culture","Education","Political\nEmpowerment","Health","Economic\nWell-Being"),
                                                 function(i,j){
                                                   c(ifelse(i<0.01,paste(j,"***",sep=""),
                                                            ifelse(i<0.05 & i>=0.01,paste(j,"**",sep=""),
                                                                   ifelse(i<0.1 & i>=0.05,paste(j,"*",sep=""),paste(j)))))
                                                 }),
                            impact.labs=sapply(p.vals.tntc[order(p.vals.tntc$Indicator),"p.val"],
                                               function(i){
                                                 c(ifelse(i<0.01,"*\n*\n*",
                                                          ifelse(i<0.05 & i>=0.01,"*\n*",
                                                                 ifelse(i<0.1 & i>=0.05,"*",""))))
                                               }))
row.names(TNTC.sig.labs) <- c("PA","SE","MT","FS","MA")

# ---- 2.3 Create vector of impact plot arrows, based on magnitude of impact ----

TNTC.impact.arrows <- data.frame(mapply(i=TNTC.2yr.impacts[TNTC.2yr.impacts$Treatment=="MPA",c("PA","SE","MT","FS","MA")],
                                        j=TNTC.2yr.impacts[TNTC.2yr.impacts$Treatment=="Control",c("PA","SE","MT","FS","MA")],
                                        function(i,j){
                                          if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                            if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                              if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                  if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                        }))
TNTC.impact.arrows <- cbind.data.frame(TNTC.impact.arrows,
                                       x=rep(0.6,4))

# ---- 2.4 Create vector of (x,y) positions for significance asterisks on impact plots ----

TNTC.sig.pos <- data.frame(mapply(i=TNTC.2yr.impacts[TNTC.2yr.impacts$Treatment=="MPA",c("PA","SE","MT","FS","MA")],
                                  j=TNTC.2yr.impacts[TNTC.2yr.impacts$Treatment=="Control",c("PA","SE","MT","FS","MA")],
                                  k=TNTC.sig.labs$impact.labs,
                                  outcome.sd=c(sd(impact.2yr[impact.2yr$MPAID==2 & !is.na(impact.2yr$MPAID),22],na.rm=T),
                                               (sd(impact.2yr[impact.2yr$MPAID==2 & !is.na(impact.2yr$MPAID),36],na.rm=T)/100),
                                               sapply(impact.2yr[impact.2yr$MPAID==2 & !is.na(impact.2yr$MPAID),c(29,7,14)],sd,na.rm=T)),
                                  function(i,j,k,outcome.sd){
                                    if(k=="*\n*\n*" & i<j) {i-0.03*outcome.sd} else
                                      if(k=="*\n*\n*" & i>j) {i+0.03*outcome.sd} else
                                        if(k=="*\n*" & i<j) {i-0.02*outcome.sd} else
                                          if(k=="*\n*" & i>j) {i+0.02*outcome.sd} else
                                            if(k=="*" & i<j) {i-0.01*outcome.sd} else
                                              if(k=="*" & i>j) {i+0.01*outcome.sd} else {0}
                                  }))
row.names(TNTC.sig.pos) <- c("PA","SE","MT","FS","MA")
