# 
# code:  Dampier 2Yr Impact Summary Statistical Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/ImpactSummaries/BHS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
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

Damp.BigFive.summary <- summarise.bigfive(impact.2yr[impact.2yr$MPAID==5 & !is.na(impact.2yr$MPAID),])

# - Status & trend plot dataset, using matched data
Damp.2yr.status.trend <- Damp.BigFive.summary[1:4,]


# - Impact plot dataset
Damp.2yr.impacts <- Damp.BigFive.summary[5:6,c(1,3:12)]

# - Snapshot impact plots (all five indicators on one plot, standardized impacts)
Damp.2yr.std.impacts <- std.impact.2yr[std.impact.2yr$MPA=="Selat Dampier MPA",]
Damp.2yr.std.impacts$MPA <- factor(Damp.2yr.std.impacts$MPA,
                                   labels="Dampier")


# ---- 1.2 Dataset for statistical analysis ----

Damp.BigFive.t0 <- cbind.data.frame(Year="Baseline",
                                    impact.2yr[impact.2yr$MPAID==5 & !is.na(impact.2yr$MPAID),
                                               colnames(impact.2yr[grep("mpa.t0",colnames(impact.2yr))])])
colnames(Damp.BigFive.t0) <- c("Year","FS","MA","PA","MT","SE")

Damp.BigFive.t2 <- cbind.data.frame(Year="2 Year Post",
                                    impact.2yr[impact.2yr$MPAID==5 & !is.na(impact.2yr$MPAID),
                                               colnames(impact.2yr[grep("mpa.t2",colnames(impact.2yr))])])
colnames(Damp.BigFive.t2) <- c("Year","FS","MA","PA","MT","SE")

Damp.BigFive <- rbind.data.frame(Damp.BigFive.t0,Damp.BigFive.t2)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Statistical Analysis (Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Mann-Whitney U test for significant MPA differences in Big Five between baseline and 2 year post ----

Damp.wilcox.MPAimpact.summ.base.2yr <- as.data.frame(sapply(Damp.BigFive[,c(2:6)],
                                                            function(i){
                                                              wilcox.test(i~Year,
                                                                          data=Damp.BigFive,
                                                                          exact=F)
                                                            }))
Damp.wilcox.MPAimpact.summ.base.2yr <- data.frame(Indicator=colnames(Damp.wilcox.MPAimpact.summ.base.2yr),
                                                  p.val=sapply(Damp.wilcox.MPAimpact.summ.base.2yr,
                                                               function(i){
                                                                 c(i[[3]])
                                                               }))
Damp.wilcox.MPAimpact.summ.base.2yr$Indicator <- factor(Damp.wilcox.MPAimpact.summ.base.2yr$Indicator,
                                                        levels=c("PA","SE","MT","FS","MA"),
                                                        ordered=T)
Damp.wilcox.MPAimpact.summ.base.2yr <- Damp.wilcox.MPAimpact.summ.base.2yr[order(Damp.wilcox.MPAimpact.summ.base.2yr$Indicator),]


# ---- 2.2 Create plot labels for impact summary plots, indicating significance (***) ----

Damp.sig.labs <- data.frame(xaxis.st.baselabs=rep(paste("Baseline\nscore\n","(",
                                                        unique(BigFive$InterviewYear[BigFive$MPAID==5 & 
                                                                                       BigFive$MonitoringYear=="Baseline"]),")",sep=""),5),
                            xaxis.st.2yrlabs=mapply(i=Damp.wilcox.MPAimpact.summ.base.2yr$p.val,
                                                    j=rep(unique(BigFive$InterviewYear[BigFive$MPAID==5 & BigFive$MonitoringYear=="2 Year Post"]),5),
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
row.names(Damp.sig.labs) <- c("PA","SE","MT","FS","MA")

# ---- 2.3 Create vector of impact plot arrows, based on magnitude of impact ----

Damp.impact.arrows <- data.frame(mapply(i=Damp.2yr.impacts[Damp.2yr.impacts$Treatment=="MPA",c("PA","SE","MT","FS","MA")],
                                        j=Damp.2yr.impacts[Damp.2yr.impacts$Treatment=="Control",c("PA","SE","MT","FS","MA")],
                                        function(i,j){
                                          if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                            if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                              if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                  if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                        }))
Damp.impact.arrows <- cbind.data.frame(Damp.impact.arrows,
                                       x=rep(0.6,4))

# ---- 2.4 Create vector of (x,y) positions for significance asterisks on impact plots ----

Damp.sig.pos <- data.frame(mapply(i=Damp.2yr.impacts[Damp.2yr.impacts$Treatment=="MPA",c("PA","SE","MT","FS","MA")],
                                  j=Damp.2yr.impacts[Damp.2yr.impacts$Treatment=="Control",c("PA","SE","MT","FS","MA")],
                                  k=Damp.sig.labs$impact.labs,
                                  outcome.sd=sapply(impact.2yr[impact.2yr$MPAID==5 & !is.na(impact.2yr$MPAID),
                                                               c("attach.ATT","enrol.ATT","tenure.ATT","HFS.ATT","assets.ATT")],sd,na.rm=T),
                                  function(i,j,k,outcome.sd){
                                    if(k=="*\n*\n*" & i<j) {i-0.03*outcome.sd} else
                                      if(k=="*\n*\n*" & i>j) {i+0.03*outcome.sd} else
                                        if(k=="*\n*" & i<j) {i-0.02*outcome.sd} else
                                          if(k=="*\n*" & i>j) {i+0.02*outcome.sd} else
                                            if(k=="*" & i<j) {i-0.01*outcome.sd} else
                                              if(k=="*" & i>j) {i+0.01*outcome.sd} else {0}
                                  }))
row.names(Damp.sig.pos) <- c("PA","SE","MT","FS","MA")
