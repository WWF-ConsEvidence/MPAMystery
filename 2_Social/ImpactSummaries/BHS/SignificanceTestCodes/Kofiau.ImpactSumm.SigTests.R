# 
# code:  Kofiau Impact Summary Statistical Tests
# 
# github: WWF-ConsEvidence/MPAMystery/2_Social/ImpactSummaries/BHS/SignificanceTestCodes
# --- Duplicate all code from "2_Social" onward, to maintain file structure for sourced code
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: November 2016
# modified: January 2018
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
#  3) Create Data Frames for Additional Plotting Symbols
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Data Configuration and Subsetting ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Datasets for MPA impact summary plots ----

Kof.BigFive.summary.2yr <- summarise.bigfive.2yr(impact.2yr[impact.2yr$MPAID==4 & !is.na(impact.2yr$MPAID),])
Kof.BigFive.summary.4yr <- summarise.bigfive.4yr(impact.4yr[impact.4yr$MPAID==4 & !is.na(impact.4yr$MPAID),])

# - Status & trend plot dataset, using matched data
Kof.2yr.status.trend <- Kof.BigFive.summary.2yr[1:4,]
Kof.4yr.status.trend <- Kof.BigFive.summary.4yr[1:4,]

Kof.mean.baseline.status <- 
  rbind.data.frame(Kof.2yr.status.trend[1:2,],Kof.4yr.status.trend[1:2,]) %>%
  group_by(Treatment,Year) %>%
  summarise_all(mean)

Kof.status.trend <- rbind.data.frame(Kof.mean.baseline.status,Kof.2yr.status.trend[3:4,],Kof.4yr.status.trend[3:4,])

# - Impact plot dataset
Kof.2yr.impacts <- Kof.BigFive.summary.2yr[5:6,c(1,3:12)]
Kof.4yr.impacts <- Kof.BigFive.summary.4yr[5:6,c(1,3:12)]

Kof.impacts <- data.frame(Year=c(rep("2 Year",2),rep("4 Year",2)),
                           rbind.data.frame(Kof.2yr.impacts,Kof.4yr.impacts))

# - Snapshot impact plots (all five indicators on one plot, standardized impacts)
Kof.2yr.std.impacts <- std.impact.2yr[std.impact.2yr$MPA=="Kofiau dan Pulau Boo MPA",]
Kof.2yr.std.impacts$MPA <- factor(Kof.2yr.std.impacts$MPA,
                                   labels="Kofiau")

Kof.4yr.std.impacts <- std.impact.4yr[std.impact.4yr$MPA=="Kofiau dan Pulau Boo MPA",]
Kof.4yr.std.impacts$MPA <- factor(Kof.4yr.std.impacts$MPA,
                                   labels="Kofiau")


# ---- 1.2 Dataset for statistical analysis ----

# Baseline - 2 year, for individual Mann-Whitney U test
Kof.BigFive.t0.t2 <- cbind.data.frame(Year="Baseline",
                                       impact.2yr[impact.2yr$MPAID==4 & !is.na(impact.2yr$MPAID),
                                                  colnames(impact.2yr[grep("mpa.t0",colnames(impact.2yr))])])
colnames(Kof.BigFive.t0.t2) <- c("Year","FS","MA","PA","MT","SE")

Kof.BigFive.t2 <- cbind.data.frame(Year="2 Year Post",
                                    impact.2yr[impact.2yr$MPAID==4 & !is.na(impact.2yr$MPAID),
                                               colnames(impact.2yr[grep("mpa.t2",colnames(impact.2yr))])])
colnames(Kof.BigFive.t2) <- c("Year","FS","MA","PA","MT","SE")

Kof.BigFive.sigtestdata.t2 <- rbind.data.frame(Kof.BigFive.t0.t2,Kof.BigFive.t2)


# Baseline - 4 year, for individual Mann-Whitney U test
Kof.BigFive.t0.t4 <- cbind.data.frame(Year="Baseline",
                                       impact.4yr[impact.4yr$MPAID==4 & !is.na(impact.4yr$MPAID),
                                                  colnames(impact.4yr[grep("mpa.t0",colnames(impact.4yr))])])
colnames(Kof.BigFive.t0.t4) <- c("Year","FS","MA","PA","MT","SE")

Kof.BigFive.t4 <- cbind.data.frame(Year="4 Year Post",
                                    impact.4yr[impact.4yr$MPAID==4 & !is.na(impact.4yr$MPAID),
                                               colnames(impact.4yr[grep("mpa.t4",colnames(impact.4yr))])])
colnames(Kof.BigFive.t4) <- c("Year","FS","MA","PA","MT","SE")

Kof.BigFive.sigtestdata.t4 <- rbind.data.frame(Kof.BigFive.t0.t4,Kof.BigFive.t4)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Statistical Analysis (Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Mann-Whitney U test for significant MPA differences in Big Five between baseline and 2 year post ----

Kof.wilcox.MPAimpact.summ.base.2yr <- as.data.frame(sapply(Kof.BigFive.sigtestdata.t2[,c(2:6)],
                                                            function(i){
                                                              wilcox.test(i~Year,
                                                                          data=Kof.BigFive.sigtestdata.t2,
                                                                          exact=F)
                                                            }))

Kof.wilcox.MPAimpact.summ.base.2yr <- data.frame(Indicator=colnames(Kof.wilcox.MPAimpact.summ.base.2yr),
                                                  p.val=sapply(Kof.wilcox.MPAimpact.summ.base.2yr,
                                                               function(i){
                                                                 c(i[[3]])
                                                               }))

Kof.wilcox.MPAimpact.summ.base.2yr$Indicator <- factor(Kof.wilcox.MPAimpact.summ.base.2yr$Indicator,
                                                        levels=c("PA","SE","MT","FS","MA"),
                                                        ordered=T)

Kof.wilcox.MPAimpact.summ.base.2yr <- Kof.wilcox.MPAimpact.summ.base.2yr[order(Kof.wilcox.MPAimpact.summ.base.2yr$Indicator),]


# ---- 2.2 Mann-Whitney U test for significant MPA differences in Big Five between baseline and 4 year post ----

Kof.wilcox.MPAimpact.summ.base.4yr <- as.data.frame(sapply(Kof.BigFive.sigtestdata.t4[,c(2:6)],
                                                            function(i){
                                                              wilcox.test(i~Year,
                                                                          data=Kof.BigFive.sigtestdata.t4,
                                                                          exact=F)
                                                            }))

Kof.wilcox.MPAimpact.summ.base.4yr <- data.frame(Indicator=colnames(Kof.wilcox.MPAimpact.summ.base.4yr),
                                                  p.val=sapply(Kof.wilcox.MPAimpact.summ.base.4yr,
                                                               function(i){
                                                                 c(i[[3]])
                                                               }))

Kof.wilcox.MPAimpact.summ.base.4yr$Indicator <- factor(Kof.wilcox.MPAimpact.summ.base.4yr$Indicator,
                                                        levels=c("PA","SE","MT","FS","MA"),
                                                        ordered=T)

Kof.wilcox.MPAimpact.summ.base.4yr <- Kof.wilcox.MPAimpact.summ.base.4yr[order(Kof.wilcox.MPAimpact.summ.base.4yr$Indicator),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Create Data Frames for Additional Plotting Symbols  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Create plot labels for impact summary plots, indicating significance (***) ----

Kof.sig.labs <- data.frame(xaxis.st.baselabs=rep(paste("Baseline\nscore\n","(",
                                                        unique(BigFive$InterviewYear[BigFive$MPAID==4 & 
                                                                                       BigFive$MonitoringYear=="Baseline"]),")",sep=""),5),
                            xaxis.st.2yrlabs=mapply(i=Kof.wilcox.MPAimpact.summ.base.2yr$p.val,
                                                    j=rep(unique(BigFive$InterviewYear[BigFive$MPAID==4 & BigFive$MonitoringYear=="2 Year Post"]),5),
                                                    function(i,j){
                                                      c(ifelse(i<0.01,paste("Two year\npost-baseline\n","(",j,")","***",sep=""),
                                                               ifelse(i<0.05 & i>=0.01,paste("Two year\npost-baseline\n","(",j,")","**",sep=""),
                                                                      ifelse(i<0.1 & i>=0.05,paste("Two year\npost-baseline\n","(",j,")","*",sep=""),
                                                                             paste("Two year\npost-baseline\n","(",j,")",sep="")))))
                                                    }),
                            xaxis.st.4yrlabs=mapply(i=Kof.wilcox.MPAimpact.summ.base.4yr$p.val,
                                                    j=rep(unique(BigFive$InterviewYear[BigFive$MPAID==4 & BigFive$MonitoringYear=="4 Year Post"]),5),
                                                    function(i,j){
                                                      c(ifelse(i<0.01,paste("Four year\npost-baseline\n","(",j,")","***",sep=""),
                                                               ifelse(i<0.05 & i>=0.01,paste("Four year\npost-baseline\n","(",j,")","**",sep=""),
                                                                      ifelse(i<0.1 & i>=0.05,paste("Four year\npost-baseline\n","(",j,")","*",sep=""),
                                                                             paste("Four year\npost-baseline\n","(",j,")",sep="")))))
                                                    }),
                           snapshot.2yrlabs=mapply(i=p.vals.kofiau.2yr[order(p.vals.kofiau.2yr$Indicator),"p.val"],
                                                   j=c("Culture","Education","Marine\nTenure","Health","Economic\nWell-Being"),
                                                   function(i,j){
                                                     c(ifelse(i<0.01,paste("***",j,sep=""),
                                                              ifelse(i<0.05 & i>=0.01,paste("**",j,sep=""),
                                                                     ifelse(i<0.1 & i>=0.05,paste("*",j,sep=""),paste(j)))))
                                                   }),
                           snapshot.4yrlabs=mapply(i=p.vals.kofiau.4yr[order(p.vals.kofiau.4yr$Indicator),"p.val"],
                                                   j=c("Culture","Education","Marine\nTenure","Health","Economic\nWell-Being"),
                                                   function(i,j){
                                                     c(ifelse(i<0.01,paste("***",j,sep=""),
                                                              ifelse(i<0.05 & i>=0.01,paste("**",j,sep=""),
                                                                     ifelse(i<0.1 & i>=0.05,paste("*",j,sep=""),paste(j)))))
                                                   }),
                            impact.2yrlabs=sapply(p.vals.kofiau.2yr[order(p.vals.kofiau.2yr$Indicator),"p.val"],
                                                  function(i){
                                                    c(ifelse(i<0.01,"*\n*\n*",
                                                             ifelse(i<0.05 & i>=0.01,"*\n*",
                                                                    ifelse(i<0.1 & i>=0.05,"*",""))))
                                                  }),
                            impact.4yrlabs=sapply(p.vals.kofiau.4yr[order(p.vals.kofiau.4yr$Indicator),"p.val"],
                                                  function(i){
                                                    c(ifelse(i<0.01,"*\n*\n*",
                                                             ifelse(i<0.05 & i>=0.01,"*\n*",
                                                                    ifelse(i<0.1 & i>=0.05,"*",""))))
                                                  }))

row.names(Kof.sig.labs) <- c("PA","SE","MT","FS","MA")


# ---- 3.2 Create vector of impact plot arrows, based on magnitude of impact ----

Kof.impact.arrows.2yr <- data.frame(mapply(i=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                            j=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                            function(i,j){
                                              if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                                if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                                  if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                      if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                        if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                            }),
                                     x=rep(0.6,4))

Kof.impact.arrows.4yr <- data.frame(mapply(i=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                            j=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                            function(i,j){
                                              if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                                if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                                  if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                      if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                        if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                            }),
                                     x=rep(1.6,4))


# ---- 3.3 Create vector of (x,y) positions for significance asterisks on impact plots ----

Kof.plotrange <- mapply(imin.2yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         imax.2yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         imin.4yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         imax.4yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         jmin.2yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         jmax.2yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         jmin.4yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         jmax.4yr=mapply(a=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         function(imin.2yr,imax.2yr,imin.4yr,imax.4yr,jmin.2yr,jmax.2yr,jmin.4yr,jmax.4yr){
                           max.2yr <- ifelse((imax.2yr>0 & jmax.2yr<=0) | (imax.2yr>0 & jmax.2yr>0 & imax.2yr>jmax.2yr),imax.2yr,
                                             ifelse((imax.2yr<=0 & jmax.2yr>0) | (imax.2yr>0 & jmax.2yr>0 & imax.2yr<jmax.2yr),jmax.2yr,0))
                           min.2yr <- ifelse((imin.2yr<0 & jmin.2yr>0) | (imin.2yr<0 & jmin.2yr<0 & imin.2yr<jmin.2yr),imin.2yr,
                                             ifelse((imin.2yr>0 & jmin.2yr<0) | (imin.2yr<0 & jmin.2yr<0 & imin.2yr>jmin.2yr),jmin.2yr,0))
                           max.4yr <- ifelse((imax.4yr>0 & jmax.4yr<=0) | (imax.4yr>0 & jmax.4yr>0 & imax.4yr>jmax.4yr),imax.4yr,
                                             ifelse((imax.4yr<=0 & jmax.4yr>0) | (imax.4yr>0 & jmax.4yr>0 & imax.4yr<jmax.4yr),jmax.4yr,0))
                           min.4yr <- ifelse((imin.4yr<0 & jmin.4yr>0) | (imin.4yr<0 & jmin.4yr<0 & imin.4yr<jmin.4yr),imin.4yr,
                                             ifelse((imin.4yr>0 & jmin.4yr<0) | (imin.4yr<0 & jmin.4yr<0 & imin.4yr>jmin.4yr),jmin.4yr,0))
                           max <- ifelse(max.2yr>=max.4yr,max.2yr,max.4yr)
                           min <- ifelse(min.2yr<=min.4yr,min.2yr,min.4yr)
                           abs(max)+abs(min)
                         })

Kof.sig.pos <- data.frame(TwoYr=mapply(i=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                        j=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                        k=Kof.sig.labs$impact.2yrlabs,
                                        range=Kof.plotrange,
                                        function(i,j,k,range){
                                          if(k=="*\n*\n*" & i<j) {i-0.03*range} else
                                            if(k=="*\n*\n*" & i>j) {i+0.03*range} else
                                              if(k=="*\n*" & i<j) {i-0.02*range} else
                                                if(k=="*\n*" & i>j) {i+0.02*range} else
                                                  if(k=="*" & i<j) {i-0.01*range} else
                                                    if(k=="*" & i>j) {i+0.01*range} else {0}
                                        }),
                           FourYr=mapply(i=Kof.impacts[Kof.impacts$Treatment=="MPA" & Kof.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                         j=Kof.impacts[Kof.impacts$Treatment=="Control" & Kof.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                         k=Kof.sig.labs$impact.4yrlabs,
                                         range=Kof.plotrange,
                                         function(i,j,k,range){
                                           if(k=="*\n*\n*" & i<j) {i-0.03*range} else
                                             if(k=="*\n*\n*" & i>j) {i+0.03*range} else
                                               if(k=="*\n*" & i<j) {i-0.02*range} else
                                                 if(k=="*\n*" & i>j) {i+0.02*range} else
                                                   if(k=="*" & i<j) {i-0.01*range} else
                                                     if(k=="*" & i>j) {i+0.01*range} else {0}
                                         }))

row.names(Kof.sig.pos) <- c("PA","SE","MT","FS","MA")




# ---- Remove unneeded data frames, to reduce clutter ----

rm(Kof.BigFive.summary.2yr, Kof.BigFive.summary.4yr,
   Kof.2yr.status.trend, Kof.4yr.status.trend, Kof.mean.baseline.status,
   Kof.2yr.impacts, Kof.4yr.impacts,
   Kof.BigFive.t0.t2, Kof.BigFive.t2, Kof.BigFive.t0.t4, Kof.BigFive.t4,
   Kof.BigFive.sigtestdata.t2, Kof.BigFive.sigtestdata.t4, Kof.plotrange,
   Kof.wilcox.MPAimpact.summ.base.2yr, Kof.wilcox.MPAimpact.summ.base.4yr)
