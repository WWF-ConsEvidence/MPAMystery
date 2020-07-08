# 
# code:  Export Status and Trends plots
# 
# github: WWF-ConsEvidence/MPAMystery/4_Products/1_Social/2_Status_trends
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: October 2019
# 
# ---- inputs ----
#  1) Requires plots to have been defined using one of the Status_trends_plots.R scripts, depending on how many years of repeat data exist
# 
# ---- outputs ----
#  1) All status and trends plots to be used in comprehensive technical reports, per MPA
# 
# ---- code sections ----
#  1) WRITE TO .PNG
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: WRITE ENGLISH LANGUAGE PLOTS TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# define figure output directory
dir.create(paste(paste(OutputFileName, "Figures--produced", sep="/"),
                 format(Sys.Date(),format="%Y%m%d"),sep="_"))

FigureFileName <- paste(paste(OutputFileName, "Figures--produced", sep="/"),
                        format(Sys.Date(),format="%Y%m%d"),sep="_")


# ---- Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(FS.statusplot)
dev.off()

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(FS.trendplot)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(FS.annexplot)
dev.off()


# ---- Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MA.statusplot)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MA.trendplot)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(MA.annexplot)
dev.off()


# ---- Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(PA.statusplot)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(PA.trendplot)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(PA.annexplot)
dev.off()


# ---- Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MT.statusplot)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MT.trendplot)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(MT.annexplot)
dev.off()


# ---- School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SE.statusplot)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SE.trendplot)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(SE.annexplot)
dev.off()


# ---- Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Time.statusplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Time.trendplot)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Time.annexplot)
dev.off()


# ---- Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Unwell.statusplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Unwell.trendplot)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Unwell.annexplot)
dev.off()


# ---- Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Gender.statusplot)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Gender.trendplot)
dev.off()


# ---- Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Religion.statusplot)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Religion.trendplot)
dev.off()


# ---- Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Primaryocc.statusplot)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Primaryocc.trendplot)
dev.off()


# ---- Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqfish.trendplot)
dev.off()


# ---- Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqsellfish.statusplot)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqsellfish.trendplot)
dev.off()


# ---- Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Incfish.statusplot)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Incfish.trendplot)
dev.off()


# ---- Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Fishtech.statusplot)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Fishtech.trendplot)
dev.off()


# ---- Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Childfs.statusplot)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Childfs.trendplot)
dev.off()


# ---- Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Proteinfish.statusplot)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Proteinfish.trendplot)
dev.off()


# ---- Categorical food security ---- 

png(paste(FigureFileName,"FS.Categorical.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(FSCategorical.statusplot)
dev.off()


# ---- Economic status ---- 

png(paste(FigureFileName,"EconStatus.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(EconStatus.statusplot)
dev.off()

png(paste(FigureFileName,"EconStatus.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(EconStatus.trendplot)
dev.off()

# ---- Member marine organization ---- 

png(paste(FigureFileName,"MarineMember.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineMember.statusplot)
dev.off()


# ---- Attend meeting marine organization ---- 

png(paste(FigureFileName,"MarineMeeting.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineMeeting.statusplot)
dev.off()


# ---- Contribution to marine organization ---- 

png(paste(FigureFileName,"MarineContribution.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineContribution.statusplot)
dev.off()

png(paste(FigureFileName,"MarineContribution.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineContribution.trendplot)
dev.off()


# ---- Social conflict ---- 

png(paste(FigureFileName,"SocialConflict.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SocialConflict.statusplot)
dev.off()


# ---- Number local threats ---- 

png(paste(FigureFileName,"NumLocalThreats.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(NumThreat.statusplot)
dev.off()

png(paste(FigureFileName,"NumLocalThreats.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(NumThreat.trendplot)
dev.off()


# ---- Secondary occupation ----

png(paste(FigureFileName,"SecondaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Secondaryocc.trendplot)
dev.off()


# ---- Occupational diversity ----

png(paste(FigureFileName,"OccDiversity.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(OccDiverse.trendplot)
dev.off()


# ---- Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=if(num.years%in%c(1:3)) { 5 } else { 10 },res=400) 
grid.newpage()
grid.draw(Age.gender.plot)
dev.off()
    


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: WRITE BAHASA LANGUAGE PLOTS TO .PNG ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# define figure output directory
dir.create(paste(paste(OutputFileName, "Bahasa_Figures--produced", sep="/"),
                 format(Sys.Date(),format="%Y%m%d"),sep="_"))

FigureFileName <- paste(paste(OutputFileName, "Bahasa_Figures--produced", sep="/"),
                        format(Sys.Date(),format="%Y%m%d"),sep="_")


# ---- Food security ----

png(paste(FigureFileName,"FS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(FS.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"FS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(FS.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"FS.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(FS.annexplot.bahasa)
dev.off()


# ---- Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MA.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"MA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MA.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"MA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(MA.annexplot.bahasa)
dev.off()


# ---- Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(PA.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"PA.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(PA.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"PA.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(PA.annexplot.bahasa)
dev.off()


# ---- Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MT.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"MT.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MT.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"MT.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(MT.annexplot.bahasa)
dev.off()


# ---- School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SE.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"SE.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SE.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"SE.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(SE.annexplot.bahasa)
dev.off()


# ---- Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Time.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"TimeMarket.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Time.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"TimeMarket.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Time.annexplot.bahasa)
dev.off()


# ---- Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Unwell.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"DaysUnwell.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Unwell.trendplot.bahasa)
dev.off()

png(paste(FigureFileName,"DaysUnwell.annex.png",sep="/"),
    units="in",height=7.5,width=7.5,res=400)
plot(Unwell.annexplot.bahasa)
dev.off()


# ---- Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Gender.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"Gender.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Gender.trendplot.bahasa)
dev.off()


# ---- Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Religion.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"Religion.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Religion.trendplot.bahasa)
dev.off()


# ---- Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Primaryocc.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"PrimaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Primaryocc.trendplot.bahasa)
dev.off()


# ---- Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqfish.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"FreqFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqfish.trendplot.bahasa)
dev.off()


# ---- Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqsellfish.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"FreqSellFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqsellfish.trendplot.bahasa)
dev.off()


# ---- Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Incfish.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"IncFish.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Incfish.trendplot.bahasa)
dev.off()


# ---- Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Fishtech.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"FishTech.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Fishtech.trendplot.bahasa)
dev.off()


# ---- Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Childfs.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"ChildFS.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Childfs.trendplot.bahasa)
dev.off()


# ---- Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Proteinfish.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"FishProtein.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Proteinfish.trendplot.bahasa)
dev.off()


# ---- Categorical food security ---- 

png(paste(FigureFileName,"FS.Categorical.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(FSCategorical.statusplot.bahasa)
dev.off()


# ---- Economic status ---- 

png(paste(FigureFileName,"EconStatus.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(EconStatus.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"EconStatus.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(EconStatus.trendplot.bahasa)
dev.off()

# ---- Member marine organization ---- 

png(paste(FigureFileName,"MarineMember.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineMember.statusplot.bahasa)
dev.off()


# ---- Attend meeting marine organization ---- 

png(paste(FigureFileName,"MarineMeeting.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineMeeting.statusplot.bahasa)
dev.off()


# ---- Contribution to marine organization ---- 

png(paste(FigureFileName,"MarineContribution.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineContribution.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"MarineContribution.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MarineContribution.trendplot.bahasa)
dev.off()


# ---- Social conflict ---- 

png(paste(FigureFileName,"SocialConflict.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SocialConflict.statusplot.bahasa)
dev.off()


# ---- Number local threats ---- 

png(paste(FigureFileName,"NumLocalThreats.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(NumThreat.statusplot.bahasa)
dev.off()

png(paste(FigureFileName,"NumLocalThreats.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(NumThreat.trendplot.bahasa)
dev.off()


# ---- Secondary occupation ----

png(paste(FigureFileName,"SecondaryOcc.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Secondaryocc.trendplot.bahasa)
dev.off()


# ---- Occupational diversity ----

png(paste(FigureFileName,"OccDiversity.trend.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(OccDiverse.trendplot.bahasa)
dev.off()


# ---- Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=if(num.years%in%c(1:3)) { 5 } else { 10 },res=400)
grid.newpage()
grid.draw(Age.gender.plot.bahasa)
dev.off()







