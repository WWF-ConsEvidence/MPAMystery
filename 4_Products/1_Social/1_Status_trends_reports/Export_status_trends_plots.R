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
# ---- SECTION 1: WRITE TO .PNG ----
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


# ---- Age/Gender ----

png(paste(FigureFileName,"Age.gender.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(Age.gender.plot)
dev.off()


# ---- Categorical food security ---- 

png(paste(FigureFileName,"FS.Categorical.status.png",sep="/"),
    units="in",height=10,width=4,res=400)
grid.newpage()
grid.draw(FSCategorical.statusplot)
dev.off()


# # ---- Number ethnic groups ----
# 
# png(paste(FigureFileName,"Num.Ethnic.png",sep="/"),
#     units="in",height=4,width=6,res=400)
# plot(Ethnic.statusplot)
# dev.off()
