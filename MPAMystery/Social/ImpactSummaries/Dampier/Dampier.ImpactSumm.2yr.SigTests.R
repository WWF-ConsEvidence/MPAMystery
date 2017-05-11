# ----
# code:  Dampier 2Yr Impact Summary Statistical Tests
# git branch: MPAMystery --> Social --> ImpactSummaries --> Dampier
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: February 2017
# 
# ----
# code sections:
#  1) Source MPA.Mystery code
#  2) Data Configuration and Subsetting
#  3) Statistical Analysis (Mann-Whitney U Test)
# 
# ----
############################################################
#
# SECTION 1: Source MPA.Mystery code
#
############################################################
# ----

source('MPAMystery/Social/MPA.Mystery.R')

# ----
############################################################
#
# SECTION 2: Data Configuration and Subsetting
#
############################################################
# ----

# 2.1 Datasets for MPA Impact Summary Plots
# --- Status & trend plot dataset, using matched data
Damp.BigFive.MPAimpact.summ.st <- cbind.data.frame(rep.int(c("MPA","Control"),2),
                                                   c(rep("Baseline",2),rep("2 Year Post",2)),
                                                   rbind.data.frame(c(summarise(Damp.2yr.HHimpacts,
                                                                                FS=round(mean(HFS.mpa.t0,na.rm=T),2),
                                                                                FSErr=round(sd(HFS.mpa.t0,na.rm=T)/sqrt(length(HFS.mpa.t0)),2),
                                                                                MA=round(mean(assets.mpa.t0,na.rm=T),2),
                                                                                MAErr=round(sd(assets.mpa.t0,na.rm=T)/sqrt(length(assets.mpa.t0)),2),
                                                                                PA=round(mean(attach.mpa.t0,na.rm=T),2),
                                                                                PAErr=round(sd(attach.mpa.t0,na.rm=T)/sqrt(length(attach.mpa.t0)),2),
                                                                                MT=round(mean(tenure.mpa.t0,na.rm=T),2),
                                                                                MTErr=round(sd(tenure.mpa.t0,na.rm=T)/sqrt(length(tenure.mpa.t0)),2),
                                                                                SE=round(mean(enrol.mpa.t0,na.rm=T),2)/100,
                                                                                SEErr=round(sd(enrol.mpa.t0,na.rm=T)/sqrt(length(enrol.mpa.t0)),2)/100)),
                                                                    c(summarise(Damp.2yr.HHimpacts,
                                                                                FS=round(mean(HFS.c.t0,na.rm=T),2),
                                                                                FSErr=round(sd(HFS.c.t0,na.rm=T)/sqrt(length(HFS.c.t0)),2),
                                                                                MA=round(mean(assets.c.t0,na.rm=T),2),
                                                                                MAErr=round(sd(assets.c.t0,na.rm=T)/sqrt(length(assets.c.t0)),2),
                                                                                PA=round(mean(attach.c.t0,na.rm=T),2),
                                                                                PAErr=round(sd(attach.c.t0,na.rm=T)/sqrt(length(attach.c.t0)),2),
                                                                                MT=round(mean(tenure.c.t0,na.rm=T),2),
                                                                                MTErr=round(sd(tenure.c.t0,na.rm=T)/sqrt(length(tenure.c.t0)),2),
                                                                                SE=round(mean(enrol.c.t0,na.rm=T),2)/100,
                                                                                SEErr=round(sd(enrol.c.t0,na.rm=T)/sqrt(length(enrol.c.t0)),2)/100)),
                                                                    c(summarise(Damp.2yr.HHimpacts,
                                                                                FS=round(mean(HFS.mpa.t2,na.rm=T),2),
                                                                                FSErr=round(sd(HFS.mpa.t2,na.rm=T)/sqrt(length(HFS.mpa.t2)),2),
                                                                                MA=round(mean(assets.mpa.t2,na.rm=T),2),
                                                                                MAErr=round(sd(assets.mpa.t2,na.rm=T)/sqrt(length(assets.mpa.t2)),2),
                                                                                PA=round(mean(attach.mpa.t2,na.rm=T),2),
                                                                                PAErr=round(sd(attach.mpa.t2,na.rm=T)/sqrt(length(attach.mpa.t2)),2),
                                                                                MT=round(mean(tenure.mpa.t2,na.rm=T),2),
                                                                                MTErr=round(sd(tenure.mpa.t2,na.rm=T)/sqrt(length(tenure.mpa.t2)),2),
                                                                                SE=round(mean(enrol.mpa.t2,na.rm=T),2)/100,
                                                                                SEErr=round(sd(enrol.mpa.t2,na.rm=T)/sqrt(length(enrol.mpa.t2)),2)/100)),
                                                                    c(summarise(Damp.2yr.HHimpacts,
                                                                                FS=round(mean(HFS.c.t2,na.rm=T),2),
                                                                                FSErr=round(sd(HFS.c.t2,na.rm=T)/sqrt(length(HFS.c.t2)),2),
                                                                                MA=round(mean(assets.c.t2,na.rm=T),2),
                                                                                MAErr=round(sd(assets.c.t2,na.rm=T)/sqrt(length(assets.c.t2)),2),
                                                                                PA=round(mean(attach.c.t2,na.rm=T),2),
                                                                                PAErr=round(sd(attach.c.t2,na.rm=T)/sqrt(length(attach.c.t2)),2),
                                                                                MT=round(mean(tenure.c.t2,na.rm=T),2),
                                                                                MTErr=round(sd(tenure.c.t2,na.rm=T)/sqrt(length(tenure.c.t2)),2),
                                                                                SE=round(mean(enrol.c.t2,na.rm=T),2)/100,
                                                                                SEErr=round(sd(enrol.c.t2,na.rm=T)/sqrt(length(enrol.c.t2)),2)/100))))
colnames(Damp.BigFive.MPAimpact.summ.st) <- c("Treatment","Year",colnames(Damp.2yr.impacts[,2:11]))
Damp.BigFive.MPAimpact.summ.st$Treatment <- factor(Damp.BigFive.MPAimpact.summ.st$Treatment,
                                                   levels=c("MPA","Control"),
                                                   ordered=T)
Damp.BigFive.MPAimpact.summ.st$Year <- factor(Damp.BigFive.MPAimpact.summ.st$Year,
                                              levels=c("Baseline","2 Year Post","4 Year Post"),
                                              ordered=T)

# --- Impact plot dataset
Damp.2yr.HHimpacts <- impact.2yr[impact.2yr$MPAID==5 & !is.na(impact.2yr$MPAID),]
Damp.2yr.impacts <- cbind.data.frame(c("MPA","Control"),
                                     rbind.data.frame(c(summarise(Damp.2yr.HHimpacts,
                                                                  FS=round(mean(HFS.MPA.outcome,na.rm=T),2),
                                                                  FSErr=round(sd(HFS.MPA.outcome,na.rm=T)/sqrt(length(HFS.MPA.outcome)),2),
                                                                  MA=round(mean(assets.MPA.outcome,na.rm=T),2),
                                                                  MAErr=round(sd(assets.MPA.outcome,na.rm=T)/sqrt(length(assets.MPA.outcome)),2),
                                                                  PA=round(mean(attach.MPA.outcome,na.rm=T),2),
                                                                  PAErr=round(sd(attach.MPA.outcome,na.rm=T)/sqrt(length(attach.MPA.outcome)),2),
                                                                  MT=round(mean(tenure.MPA.outcome,na.rm=T),2),
                                                                  MTErr=round(sd(tenure.MPA.outcome,na.rm=T)/sqrt(length(tenure.MPA.outcome)),2),
                                                                  SE=round(mean(enrol.MPA.outcome,na.rm=T),2)/100,
                                                                  SEErr=round(sd(enrol.MPA.outcome,na.rm=T)/sqrt(length(enrol.MPA.outcome)),2)/100)),
                                                      c(summarise(Damp.2yr.HHimpacts,
                                                                  FS=round(mean(HFS.control.outcome,na.rm=T),2),
                                                                  FSErr=round(sd(HFS.control.outcome,na.rm=T)/sqrt(length(HFS.control.outcome)),2),
                                                                  MA=round(mean(assets.control.outcome,na.rm=T),2),
                                                                  MAErr=round(sd(assets.control.outcome,na.rm=T)/sqrt(length(assets.control.outcome)),2),
                                                                  PA=round(mean(attach.control.outcome,na.rm=T),2),
                                                                  PAErr=round(sd(attach.control.outcome,na.rm=T)/sqrt(length(attach.control.outcome)),2),
                                                                  MT=round(mean(tenure.control.outcome,na.rm=T),2),
                                                                  MTErr=round(sd(tenure.control.outcome,na.rm=T)/sqrt(length(tenure.control.outcome)),2),
                                                                  SE=round(mean(enrol.control.outcome,na.rm=T),2)/100,
                                                                  SEErr=round(sd(enrol.control.outcome,na.rm=T)/sqrt(length(enrol.control.outcome)),2)/100))))
colnames(Damp.2yr.impacts) <- c("Treatment",colnames(Damp.2yr.impacts[2:11]))
Damp.2yr.impacts$Treatment <- factor(Damp.2yr.impacts$Treatment,
                                     levels=Damp.2yr.impacts$Treatment,
                                     ordered=T)

# --- Snapshot impact plots (all five indicators on one plot, standardized impacts)
Damp.2yr.std.impacts <- subset(std.impact.2yr[std.impact.2yr$MPA=="Selat Dampier MPA",])
Damp.2yr.std.impacts$MPA <- factor(Damp.2yr.std.impacts$MPA,
                                   labels="Dampier")

# 2.2 Dataset for statistical analysis
Damp.2yr.HHimpacts.1 <- Damp.2yr.HHimpacts[,c(2,5,9,12,16,19,23,26,30,33)]
colnames(Damp.2yr.HHimpacts.1) <- c("FS","FS","MA","MA","PA","PA","MT","MT","SE","SE")
Damp.BigFive <- cbind.data.frame(c(rep("Baseline",length(Damp.2yr.HHimpacts.1[,1])),
                                   rep("2 Year Post",length(Damp.2yr.HHimpacts.1[,1]))),
                                 rbind.data.frame(c(Damp.2yr.HHimpacts.1[,c(1,3,5,7,9)]),
                                                  c(Damp.2yr.HHimpacts.1[,c(2,4,6,8,10)])))
colnames(Damp.BigFive) <- c("Year","FS","MA","PA","MT","SE")

# ----
############################################################
#
# SECTION 3: Statistical Analysis (Mann-Whitney U Test)
#
############################################################
# ----

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
                            snapshot.labs=mapply(i=p.vals.dampier[order(p.vals.dampier$Indicator),"p.val"],
                                                 j=c("Culture","Education","Political\nEmpowerment","Health","Economic\nWell-Being"),
                                                 function(i,j){
                                                   c(ifelse(i<0.01,paste(j,"***",sep=""),
                                                            ifelse(i<0.05 & i>=0.01,paste(j,"**",sep=""),
                                                                   ifelse(i<0.1 & i>=0.05,paste(j,"*",sep=""),paste(j)))))
                                                 }),
                            impact.labs=sapply(p.vals.dampier[order(p.vals.dampier$Indicator),"p.val"],
                                               function(i){
                                                 c(ifelse(i<0.01,"*\n*\n*",
                                                          ifelse(i<0.05 & i>=0.01,"*\n*",
                                                                 ifelse(i<0.1 & i>=0.05,"*",""))))
                                               }))
row.names(Damp.sig.labs) <- c("PA","SE","MT","FS","MA")

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

Damp.sig.pos <- data.frame(mapply(i=Damp.2yr.impacts[Damp.2yr.impacts$Treatment=="MPA",c("PA","SE","MT","FS","MA")],
                                  j=Damp.2yr.impacts[Damp.2yr.impacts$Treatment=="Control",c("PA","SE","MT","FS","MA")],
                                  k=Damp.sig.labs$impact.labs,
                                  outcome.sd=c(sd(Damp.2yr.HHimpacts[,c(21)],na.rm=T),
                                               (sd(Damp.2yr.HHimpacts[,c(35)],na.rm=T)/100),
                                               sapply(Damp.2yr.HHimpacts[,c(28,6,13)],sd,na.rm=T)),
                                  function(i,j,k,outcome.sd){
                                    if(k=="*\n*\n*" & i<j) {i-0.03*outcome.sd} else
                                      if(k=="*\n*\n*" & i>j) {i+0.03*outcome.sd} else
                                        if(k=="*\n*" & i<j) {i-0.02*outcome.sd} else
                                          if(k=="*\n*" & i>j) {i+0.02*outcome.sd} else
                                            if(k=="*" & i<j) {i-0.01*outcome.sd} else
                                              if(k=="*" & i>j) {i+0.01*outcome.sd} else {0}
                                  }))
row.names(Damp.sig.pos) <- c("PA","SE","MT","FS","MA")