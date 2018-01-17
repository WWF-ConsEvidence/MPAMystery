# 
# code:  Misool Impact Summary Statistical Tests
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

Mis.BigFive.summary.2yr <- summarise.bigfive.2yr(impact.2yr[impact.2yr$MPAID==6 & !is.na(impact.2yr$MPAID),])
Mis.BigFive.summary.4yr <- summarise.bigfive.4yr(impact.4yr[impact.4yr$MPAID==6 & !is.na(impact.4yr$MPAID),])

# - Status & trend plot dataset, using matched data
Mis.2yr.status.trend <- Mis.BigFive.summary.2yr[1:4,]
Mis.4yr.status.trend <- Mis.BigFive.summary.4yr[1:4,]

Mis.mean.baseline.status <- 
  rbind.data.frame(Mis.2yr.status.trend[1:2,],Mis.4yr.status.trend[1:2,]) %>%
  group_by(Treatment,Year) %>%
  summarise_all(mean)

Mis.status.trend <- rbind.data.frame(Mis.mean.baseline.status,Mis.2yr.status.trend[3:4,],Mis.4yr.status.trend[3:4,])

# - Impact plot dataset
Mis.2yr.impacts <- Mis.BigFive.summary.2yr[5:6,c(1,3:12)]
Mis.4yr.impacts <- Mis.BigFive.summary.4yr[5:6,c(1,3:12)]

Mis.impacts <- data.frame(Year=c(rep("2 Year",2),rep("4 Year",2)),
                           rbind.data.frame(Mis.2yr.impacts,Mis.4yr.impacts))

# - Snapshot impact plots (all five indicators on one plot, standardized impacts)
Mis.2yr.std.impacts <- std.impact.2yr[std.impact.2yr$MPA=="Misool Selatan Timur MPA",]
Mis.2yr.std.impacts$MPA <- factor(Mis.2yr.std.impacts$MPA,
                                   labels="Misool")

Mis.4yr.std.impacts <- std.impact.4yr[std.impact.4yr$MPA=="Misool Selatan Timur MPA",]
Mis.4yr.std.impacts$MPA <- factor(Mis.4yr.std.impacts$MPA,
                                   labels="Misool")


# ---- 1.2 Dataset for statistical analysis ----

# Baseline - 2 year, for individual Mann-Whitney U test
Mis.BigFive.t0.t2 <- cbind.data.frame(Year="Baseline",
                                       impact.2yr[impact.2yr$MPAID==6 & !is.na(impact.2yr$MPAID),
                                                  colnames(impact.2yr[grep("mpa.t0",colnames(impact.2yr))])])
colnames(Mis.BigFive.t0.t2) <- c("Year","FS","MA","PA","MT","SE")

Mis.BigFive.t2 <- cbind.data.frame(Year="2 Year Post",
                                    impact.2yr[impact.2yr$MPAID==6 & !is.na(impact.2yr$MPAID),
                                               colnames(impact.2yr[grep("mpa.t2",colnames(impact.2yr))])])
colnames(Mis.BigFive.t2) <- c("Year","FS","MA","PA","MT","SE")

Mis.BigFive.sigtestdata.t2 <- rbind.data.frame(Mis.BigFive.t0.t2,Mis.BigFive.t2)


# Baseline - 4 year, for individual Mann-Whitney U test
Mis.BigFive.t0.t4 <- cbind.data.frame(Year="Baseline",
                                       impact.4yr[impact.4yr$MPAID==6 & !is.na(impact.4yr$MPAID),
                                                  colnames(impact.4yr[grep("mpa.t0",colnames(impact.4yr))])])
colnames(Mis.BigFive.t0.t4) <- c("Year","FS","MA","PA","MT","SE")

Mis.BigFive.t4 <- cbind.data.frame(Year="4 Year Post",
                                    impact.4yr[impact.4yr$MPAID==6 & !is.na(impact.4yr$MPAID),
                                               colnames(impact.4yr[grep("mpa.t4",colnames(impact.4yr))])])
colnames(Mis.BigFive.t4) <- c("Year","FS","MA","PA","MT","SE")

Mis.BigFive.sigtestdata.t4 <- rbind.data.frame(Mis.BigFive.t0.t4,Mis.BigFive.t4)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Statistical Analysis (Mann-Whitney U test) ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Mann-Whitney U test for significant MPA differences in Big Five between baseline and 2 year post ----

Mis.wilcox.MPAimpact.summ.base.2yr <- as.data.frame(sapply(Mis.BigFive.sigtestdata.t2[,c(2:6)],
                                                            function(i){
                                                              wilcox.test(i~Year,
                                                                          data=Mis.BigFive.sigtestdata.t2,
                                                                          exact=F)
                                                            }))

Mis.wilcox.MPAimpact.summ.base.2yr <- data.frame(Indicator=colnames(Mis.wilcox.MPAimpact.summ.base.2yr),
                                                  p.val=sapply(Mis.wilcox.MPAimpact.summ.base.2yr,
                                                               function(i){
                                                                 c(i[[3]])
                                                               }))

Mis.wilcox.MPAimpact.summ.base.2yr$Indicator <- factor(Mis.wilcox.MPAimpact.summ.base.2yr$Indicator,
                                                        levels=c("PA","SE","MT","FS","MA"),
                                                        ordered=T)

Mis.wilcox.MPAimpact.summ.base.2yr <- Mis.wilcox.MPAimpact.summ.base.2yr[order(Mis.wilcox.MPAimpact.summ.base.2yr$Indicator),]


# ---- 2.2 Mann-Whitney U test for significant MPA differences in Big Five between baseline and 4 year post ----

Mis.wilcox.MPAimpact.summ.base.4yr <- as.data.frame(sapply(Mis.BigFive.sigtestdata.t4[,c(2:6)],
                                                            function(i){
                                                              wilcox.test(i~Year,
                                                                          data=Mis.BigFive.sigtestdata.t4,
                                                                          exact=F)
                                                            }))

Mis.wilcox.MPAimpact.summ.base.4yr <- data.frame(Indicator=colnames(Mis.wilcox.MPAimpact.summ.base.4yr),
                                                  p.val=sapply(Mis.wilcox.MPAimpact.summ.base.4yr,
                                                               function(i){
                                                                 c(i[[3]])
                                                               }))

Mis.wilcox.MPAimpact.summ.base.4yr$Indicator <- factor(Mis.wilcox.MPAimpact.summ.base.4yr$Indicator,
                                                        levels=c("PA","SE","MT","FS","MA"),
                                                        ordered=T)

Mis.wilcox.MPAimpact.summ.base.4yr <- Mis.wilcox.MPAimpact.summ.base.4yr[order(Mis.wilcox.MPAimpact.summ.base.4yr$Indicator),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Create Data Frames for Additional Plotting Symbols  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Create plot labels for impact summary plots, indicating significance (***) ----

Mis.sig.labs <- data.frame(xaxis.st.baselabs=rep(paste("Baseline\nscore\n","(",
                                                        unique(BigFive$InterviewYear[BigFive$MPAID==6 & 
                                                                                       BigFive$MonitoringYear=="Baseline"]),")",sep=""),5),
                            xaxis.st.2yrlabs=mapply(i=Mis.wilcox.MPAimpact.summ.base.2yr$p.val,
                                                    j=rep(unique(BigFive$InterviewYear[BigFive$MPAID==6 & BigFive$MonitoringYear=="2 Year Post"]),5),
                                                    function(i,j){
                                                      c(ifelse(i<0.01,paste("Two year\npost-baseline\n","(",j,")","***",sep=""),
                                                               ifelse(i<0.05 & i>=0.01,paste("Two year\npost-baseline\n","(",j,")","**",sep=""),
                                                                      ifelse(i<0.1 & i>=0.05,paste("Two year\npost-baseline\n","(",j,")","*",sep=""),
                                                                             paste("Two year\npost-baseline\n","(",j,")",sep="")))))
                                                    }),
                            xaxis.st.4yrlabs=mapply(i=Mis.wilcox.MPAimpact.summ.base.4yr$p.val,
                                                    j=rep(unique(BigFive$InterviewYear[BigFive$MPAID==6 & BigFive$MonitoringYear=="4 Year Post"]),5),
                                                    function(i,j){
                                                      c(ifelse(i<0.01,paste("Four year\npost-baseline\n","(",j,")","***",sep=""),
                                                               ifelse(i<0.05 & i>=0.01,paste("Four year\npost-baseline\n","(",j,")","**",sep=""),
                                                                      ifelse(i<0.1 & i>=0.05,paste("Four year\npost-baseline\n","(",j,")","*",sep=""),
                                                                             paste("Four year\npost-baseline\n","(",j,")",sep="")))))
                                                    }),
                           snapshot.2yrlabs=mapply(i=p.vals.misool.2yr[order(p.vals.misool.2yr$Indicator),"p.val"],
                                                   j=c("Culture","Education","Marine\nTenure","Health","Economic\nWell-Being"),
                                                   function(i,j){
                                                     c(ifelse(i<0.01,paste("***",j,sep=""),
                                                              ifelse(i<0.05 & i>=0.01,paste("**",j,sep=""),
                                                                     ifelse(i<0.1 & i>=0.05,paste("*",j,sep=""),paste(j)))))
                                                   }),
                           snapshot.4yrlabs=mapply(i=p.vals.misool.4yr[order(p.vals.misool.4yr$Indicator),"p.val"],
                                                   j=c("Culture","Education","Marine\nTenure","Health","Economic\nWell-Being"),
                                                   function(i,j){
                                                     c(ifelse(i<0.01,paste("***",j,sep=""),
                                                              ifelse(i<0.05 & i>=0.01,paste("**",j,sep=""),
                                                                     ifelse(i<0.1 & i>=0.05,paste("*",j,sep=""),paste(j)))))
                                                   }),
                            impact.2yrlabs=sapply(p.vals.misool.2yr[order(p.vals.misool.2yr$Indicator),"p.val"],
                                                  function(i){
                                                    c(ifelse(i<0.01,"*\n*\n*",
                                                             ifelse(i<0.05 & i>=0.01,"*\n*",
                                                                    ifelse(i<0.1 & i>=0.05,"*",""))))
                                                  }),
                            impact.4yrlabs=sapply(p.vals.misool.4yr[order(p.vals.misool.4yr$Indicator),"p.val"],
                                                  function(i){
                                                    c(ifelse(i<0.01,"*\n*\n*",
                                                             ifelse(i<0.05 & i>=0.01,"*\n*",
                                                                    ifelse(i<0.1 & i>=0.05,"*",""))))
                                                  }))

row.names(Mis.sig.labs) <- c("PA","SE","MT","FS","MA")


# ---- 3.2 Create vector of impact plot arrows, based on magnitude of impact ----

Mis.impact.arrows.2yr <- data.frame(mapply(i=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                            j=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                            function(i,j){
                                              if(i>0 & j>0 & i>j) {seq(i-(0.1*(i-j)),j+(0.1*(i-j)),length.out=4)} else
                                                if(i>0 & j>0 & i<j) {seq(i+(0.1*(j-i)),j-(0.1*(j-i)),length.out=4)} else
                                                  if(i<0 & j<0 & i>j) {seq(i-(0.1*(abs(i-j))),j+(0.1*(abs(i-j))),length.out=4)} else
                                                    if(i<0 & j<0 & i<j) {seq(i+(0.1*(abs(j-i))),j-(0.1*(abs(j-i))),length.out=4)} else
                                                      if(i<0 & j>0) {seq(i+(0.1*(abs(i-j))),j-(0.1*(abs(i-j))),length.out=4)} else
                                                        if(i>0 & j<0) {seq(i-(0.1*(abs(j-i))),j+(0.1*(abs(j-i))),length.out=4)} else {NA}
                                            }),
                                     x=rep(0.6,4))

Mis.impact.arrows.4yr <- data.frame(mapply(i=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                            j=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
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

Mis.plotrange <- mapply(imin.2yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         imax.2yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         imin.4yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         imax.4yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         jmin.2yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         jmax.2yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="2 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="2 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a+b}),
                         jmin.4yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="4 Year",
                                                        c("PAErr","SEErr","MTErr","FSErr","MAErr")],
                                         function(a,b){
                                           a-b}),
                         jmax.4yr=mapply(a=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="4 Year",
                                                        c("PA","SE","MT","FS","MA")],
                                         b=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="4 Year",
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

Mis.sig.pos <- data.frame(TwoYr=mapply(i=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                        j=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="2 Year",c("PA","SE","MT","FS","MA")],
                                        k=Mis.sig.labs$impact.2yrlabs,
                                        range=Mis.plotrange,
                                        function(i,j,k,range){
                                          if(k=="*\n*\n*" & i<j) {i-0.03*range} else
                                            if(k=="*\n*\n*" & i>j) {i+0.03*range} else
                                              if(k=="*\n*" & i<j) {i-0.02*range} else
                                                if(k=="*\n*" & i>j) {i+0.02*range} else
                                                  if(k=="*" & i<j) {i-0.01*range} else
                                                    if(k=="*" & i>j) {i+0.01*range} else {0}
                                        }),
                           FourYr=mapply(i=Mis.impacts[Mis.impacts$Treatment=="MPA" & Mis.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                         j=Mis.impacts[Mis.impacts$Treatment=="Control" & Mis.impacts$Year=="4 Year",c("PA","SE","MT","FS","MA")],
                                         k=Mis.sig.labs$impact.4yrlabs,
                                         range=Mis.plotrange,
                                         function(i,j,k,range){
                                           if(k=="*\n*\n*" & i<j) {i-0.03*range} else
                                             if(k=="*\n*\n*" & i>j) {i+0.03*range} else
                                               if(k=="*\n*" & i<j) {i-0.02*range} else
                                                 if(k=="*\n*" & i>j) {i+0.02*range} else
                                                   if(k=="*" & i<j) {i-0.01*range} else
                                                     if(k=="*" & i>j) {i+0.01*range} else {0}
                                         }))

row.names(Mis.sig.pos) <- c("PA","SE","MT","FS","MA")




# ---- Remove unneeded data frames, to reduce clutter ----

rm(Mis.BigFive.summary.2yr, Mis.BigFive.summary.4yr,
   Mis.2yr.status.trend, Mis.4yr.status.trend, Mis.mean.baseline.status,
   Mis.2yr.impacts, Mis.4yr.impacts,
   Mis.BigFive.t0.t2, Mis.BigFive.t2, Mis.BigFive.t0.t4, Mis.BigFive.t4,
   Mis.BigFive.sigtestdata.t2, Mis.BigFive.sigtestdata.t4, Mis.plotrange,
   Mis.wilcox.MPAimpact.summ.base.2yr, Mis.wilcox.MPAimpact.summ.base.4yr)
