# 
# code:  Export Status plots
# 
# github: WWF-ConsEvidence/MPAMystery/4_Products/1_Social/2_Status_trends
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: October 2019
# 
# ---- inputs ----
#  1) Requires plots to have been defined using one of the _plots.R scripts, depending on how many years of repeat data exist
# 
# ---- outputs ----
#  1) All status plots to be used in comprehensive technical reports, per MPA
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


# ---- Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MA.statusplot)
dev.off()


# ---- Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(PA.statusplot)
dev.off()


# ---- Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MT.statusplot)
dev.off()


# ---- School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SE.statusplot)
dev.off()


# ---- Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Time.statusplot)
dev.off()


# ---- Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Unwell.statusplot)
dev.off()


# ---- Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Gender.statusplot)
dev.off()


# ---- Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Religion.statusplot)
dev.off()

# ONLY RUN THIS FOR MPAID==21
# png(paste(FigureFileName,"Religion.updated.status.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(ReligionOther.statusplot)
# dev.off()


# ---- Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Primaryocc.statusplot)
dev.off()


# ---- Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqfish.statusplot)
dev.off()


# ---- Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqsellfish.statusplot)
dev.off()


# ---- Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Incfish.statusplot)
dev.off()


# ---- Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Fishtech.statusplot)
dev.off()


# ---- Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Childfs.statusplot)
dev.off()


# ---- Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Proteinfish.statusplot)
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


# # ---- Secondary occupation ----
# 
# png(paste(FigureFileName,"SecondaryOcc.trend.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(Secondaryocc.trendplot)
# dev.off()
# 
# 
# # ---- Occupational diversity ----
# 
# png(paste(FigureFileName,"OccDiversity.trend.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(OccDiverse.trendplot)
# dev.off()


# ---- Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=6,width=5,res=400)
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


# ---- Material assets ----

png(paste(FigureFileName,"MA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MA.statusplot.bahasa)
dev.off()


# ---- Place attachment ----

png(paste(FigureFileName,"PA.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(PA.statusplot.bahasa)
dev.off()


# ---- Marine tenure ----

png(paste(FigureFileName,"MT.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(MT.statusplot.bahasa)
dev.off()


# ---- School enrollment ----

png(paste(FigureFileName,"SE.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(SE.statusplot.bahasa)
dev.off()


# ---- Time to market ----

png(paste(FigureFileName,"TimeMarket.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Time.statusplot.bahasa)
dev.off()


# ---- Days unwell ----

png(paste(FigureFileName,"DaysUnwell.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Unwell.statusplot.bahasa)
dev.off()


# ---- Gender of head of household ----

png(paste(FigureFileName,"Gender.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Gender.statusplot.bahasa)
dev.off()


# ---- Religion ----

png(paste(FigureFileName,"Religion.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Religion.statusplot.bahasa)
dev.off()

# ONLY RUN THIS FOR MPAID==21
# png(paste(FigureFileName,"Religion.updated.status.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(ReligionOther.statusplot.bahasa)
# dev.off()


# ---- Primary occupation ----

png(paste(FigureFileName,"PrimaryOcc.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Primaryocc.statusplot.bahasa)
dev.off()


# ---- Fishing frequency ----

png(paste(FigureFileName,"FreqFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqfish.statusplot.bahasa)
dev.off()


# ---- Fish sale frequency ----

png(paste(FigureFileName,"FreqSellFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Freqsellfish.statusplot.bahasa)
dev.off()


# ---- Income from fishing ----

png(paste(FigureFileName,"IncFish.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Incfish.statusplot.bahasa)
dev.off()


# ---- Fishing technique ----

png(paste(FigureFileName,"FishTech.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Fishtech.statusplot.bahasa)
dev.off()


# ---- Childhood food security ----

png(paste(FigureFileName,"ChildFS.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Childfs.statusplot.bahasa)
dev.off()


# ---- Protein from fish ----

png(paste(FigureFileName,"FishProtein.status.png",sep="/"),
    units="in",height=4,width=6,res=400)
plot(Proteinfish.statusplot.bahasa)
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


# # ---- Secondary occupation ----
# 
# png(paste(FigureFileName,"SecondaryOcc.trend.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(Secondaryocc.trendplot.bahasa)
# dev.off()
# 
# 
# # ---- Occupational diversity ----
# 
# png(paste(FigureFileName,"OccDiversity.trend.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(OccDiverse.trendplot.bahasa)
# dev.off()


# ---- Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=6,width=5,res=400)
grid.newpage()
grid.draw(Age.gender.plot.bahasa)
dev.off()







