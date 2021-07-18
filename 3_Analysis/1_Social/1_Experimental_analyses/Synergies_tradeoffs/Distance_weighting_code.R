# 
# code: Gravity weighting reef site distance from settlements for BHS and SBS
# 
# author: Robert Fidler
# created: 
# modified: September 2020, by Kelly Claborn (clabornkelly@gmail.com)
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Load libraries ----

pacman::p_load(rio, reshape2, raster, dplyr)

# ---- 1.2 Source script that wrangles flat data (with coordinates for social and eco sites) ----

source('3_Analysis/1_Social/1_Experimental_analyses/Synergies_tradeoffs/Wrangle_data_for_distance_weight.R')

gps.final.2 <- data.for.weighting


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CALCULATE POINT DISTANCE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

MPA.1 <- gps.final.2[which(gps.final.2$MPAID==1),]
MPA.2 <- gps.final.2[which(gps.final.2$MPAID==2),]
MPA.4 <- gps.final.2[which(gps.final.2$MPAID==4),]
MPA.5 <- gps.final.2[which(gps.final.2$MPAID==5),]
MPA.6 <- gps.final.2[which(gps.final.2$MPAID==6),]
MPA.7 <- gps.final.2[which(gps.final.2$MPAID==7),]
MPA.9 <- gps.final.2[which(gps.final.2$MPAID==9),]
MPA.15 <- gps.final.2[which(gps.final.2$MPAID==15),]
MPA.16 <- gps.final.2[which(gps.final.2$MPAID==16),]
MPA.17 <- gps.final.2[which(gps.final.2$MPAID==17),]
MPA.18 <- gps.final.2[which(gps.final.2$MPAID==18),]

site.crosscheck <- rbind(MPA.1, MPA.2, MPA.4, MPA.5, MPA.6, MPA.7, MPA.9, MPA.15, MPA.16, MPA.17, MPA.18)
colnames(site.crosscheck)
site.crosscheck2 <-
  site.crosscheck%>%
  group_by(SiteID)%>%
  summarise_all(funs(mean))

MPA.1x <- unique(MPA.1[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.1y <- unique(MPA.1[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.2x <- unique(MPA.2[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.2y <- unique(MPA.2[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.4x <- unique(MPA.4[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.4y <- unique(MPA.4[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.5x <- unique(MPA.5[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.5y <- unique(MPA.5[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.6x <- unique(MPA.6[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.6y <- unique(MPA.6[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.7x <- unique(MPA.7[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.7y <- unique(MPA.7[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.9x <- unique(MPA.9[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.9y <- unique(MPA.9[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.15x <- unique(MPA.15[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.15y <- unique(MPA.15[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.16x <- unique(MPA.16[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.16y <- unique(MPA.16[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.17x <- unique(MPA.17[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.17y <- unique(MPA.17[,c("SettlementID", "long.y", "lat.y", "MPAID")])
MPA.18x <- unique(MPA.18[,c("SiteID", "long.x", "lat.x", "MPAID")])
MPA.18y <- unique(MPA.18[,c("SettlementID", "long.y", "lat.y", "MPAID")])

# ---- 2.2 Per MPA, run point distance and set column and row names for matrix ----

# MPA 1
# Run PointDistance
MPA.1.dist <- as.data.frame(pointDistance(MPA.1x[,2:3], MPA.1y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.1.dist) <- MPA.1x$SiteID
colnames(MPA.1.dist) <- MPA.1y$SettlementID

# MPA 2
# Run PointDistance
MPA.2.dist <- as.data.frame(pointDistance(MPA.2x[,2:3], MPA.2y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.2.dist) <- MPA.2x$SiteID
colnames(MPA.2.dist) <- MPA.2y$SettlementID

# MPA 4
# Run PointDistance
MPA.4.dist <- as.data.frame(pointDistance(MPA.4x[,2:3], MPA.4y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.4.dist) <- MPA.4x$SiteID
colnames(MPA.4.dist) <- MPA.4y$SettlementID

# MPA 5
# Run PointDistance
MPA.5.dist <- as.data.frame(pointDistance(MPA.5x[,2:3], MPA.5y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.5.dist) <- MPA.5x$SiteID
colnames(MPA.5.dist) <- MPA.5y$SettlementID

# MPA 6
# Run PointDistance
MPA.6.dist <- as.data.frame(pointDistance(MPA.6x[,2:3], MPA.6y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.6.dist) <- MPA.6x$SiteID
colnames(MPA.6.dist) <- MPA.6y$SettlementID

# MPA 7
# Run PointDistance
MPA.7.dist <- as.data.frame(pointDistance(MPA.7x[,2:3], MPA.7y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.7.dist) <- MPA.7x$SiteID
colnames(MPA.7.dist) <- MPA.7y$SettlementID

# MPA 9
# Run PointDistance
MPA.9.dist <- as.data.frame(pointDistance(MPA.9x[,2:3], MPA.9y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.9.dist) <- MPA.9x$SiteID
colnames(MPA.9.dist) <- MPA.9y$SettlementID

# MPA 15
# Run PointDistance
MPA.15.dist <- as.data.frame(pointDistance(MPA.15x[,2:3], MPA.15y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.15.dist) <- MPA.15x$SiteID
colnames(MPA.15.dist) <- MPA.15y$SettlementID

# MPA 16
# Run PointDistance
MPA.16.dist <- as.data.frame(pointDistance(MPA.16x[,2:3], MPA.16y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.16.dist) <- MPA.16x$SiteID
colnames(MPA.16.dist) <- MPA.16y$SettlementID

# MPA 17
# Run PointDistance
MPA.17.dist <- as.data.frame(pointDistance(MPA.17x[,2:3], MPA.17y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.17.dist) <- MPA.17x$SiteID
colnames(MPA.17.dist) <- MPA.17y$SettlementID

# MPA 18
# Run PointDistance
MPA.18.dist <- as.data.frame(pointDistance(MPA.18x[,2:3], MPA.18y[,2:3], lonlat = T, allpairs = T))
# Set Column and Row Names for Matrix
rownames(MPA.18.dist) <- MPA.18x$SiteID
colnames(MPA.18.dist) <- MPA.18y$SettlementID


#rm(MPA.1, MPA.2, MPA.4, MPA.5, MPA.6, MPA.7, MPA.9, MPA.15, MPA.16, MPA.17, MPA.18)
#rm(MPA.1x, MPA.1y, MPA.2x, MPA.2y, MPA.4x, MPA.4y, MPA.5x, MPA.5y, MPA.6x, MPA.6y, MPA.7x, MPA.7y, MPA.9x, MPA.9y, 
#   MPA.15x, MPA.15y, MPA.16x, MPA.16y, MPA.17x, MPA.17y, MPA.18x, MPA.18y)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: POST-PROCESS DISTANCES & CALCULATE RELATIVE WEIGHTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

mpa.matrix.1 <- MPA.1.dist
mpa.matrix.2 <- MPA.2.dist
mpa.matrix.4 <- MPA.4.dist
mpa.matrix.5 <- MPA.5.dist
mpa.matrix.6 <- MPA.6.dist
mpa.matrix.7 <- MPA.7.dist
mpa.matrix.9 <- MPA.9.dist
mpa.matrix.15 <- MPA.15.dist
mpa.matrix.16 <- MPA.16.dist
mpa.matrix.17 <- MPA.17.dist
mpa.matrix.18 <- MPA.18.dist

# ---- 3.1 Transpose Data Frames (so that eco sites are columns, instead of settlements) ----

transpose <- lapply(mget(paste0("mpa.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                    function(mpa.matrix.)
                      mpa.matrix. <- as.data.frame(t(mpa.matrix.))) 

invisible(list2env(transpose, globalenv()))


# ---- 3.2 Square Distances, inverted (for relative weight calculations) ----

mpa.wt.matrix.1 <- as.data.frame(1/(mpa.matrix.1^2))
mpa.wt.matrix.2 <- as.data.frame(1/(mpa.matrix.2^2))
mpa.wt.matrix.4 <- as.data.frame(1/(mpa.matrix.4^2))
mpa.wt.matrix.5 <- as.data.frame(1/(mpa.matrix.5^2))
mpa.wt.matrix.6 <- as.data.frame(1/(mpa.matrix.6^2))
mpa.wt.matrix.7 <- as.data.frame(1/(mpa.matrix.7^2))
mpa.wt.matrix.9 <- as.data.frame(1/(mpa.matrix.9^2))
mpa.wt.matrix.15 <- as.data.frame(1/(mpa.matrix.15^2))
mpa.wt.matrix.16 <- as.data.frame(1/(mpa.matrix.16^2))
mpa.wt.matrix.17 <- as.data.frame(1/(mpa.matrix.17^2))
mpa.wt.matrix.18 <- as.data.frame(1/(mpa.matrix.18^2))


# ---- 3.3 Calculate relative weights, using inverse square distances ----
# NOTE: this allows the smalles distance to have the largest weight

# -- Removing NA Columns
na.b.gone <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                    function(mpa.wt.matrix.)
                      mpa.wt.matrix.[!sapply(mpa.wt.matrix., function(x) all(is.na(x)))]) 

invisible(list2env(na.b.gone, globalenv()))

# -- Converting Distances into Relative Importance Values
# ----- Calculating Sum of Weights
grav.sum <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                   function(mpa.wt.matrix.){
                     mpa.wt.matrix.$gravsum<-apply(mpa.wt.matrix., 1, FUN=sum)
                     
                     return(mpa.wt.matrix.)})
invisible(list2env(grav.sum, globalenv()))

# ----- Moving Minimum Distance to Column 1
movecol <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                  function(mpa.wt.matrix.){
                    mpa.wt.matrix.<-mpa.wt.matrix.%>%dplyr::select(gravsum, everything())
                    
                    return(mpa.wt.matrix.)})
invisible(list2env(movecol, globalenv()))

# ----- Multiplying 1/x by min.dist to Calculate Relative Distance
relative.dist <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                        function(mpa.wt.matrix.)
                          sweep(mpa.wt.matrix., MARGIN = 1, FUN="/", STATS=mpa.wt.matrix.$gravsum)
                        
)
invisible(list2env(relative.dist, globalenv()))

# ----- Removing Min Dist Column
min.b.gone <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                     function(mpa.wt.matrix.)
                       mpa.wt.matrix. <- mpa.wt.matrix.[,-1]
                     
)
invisible(list2env(min.b.gone, globalenv()))

# ----- Calculate Relative Distance Sums
rel.dist.sum <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                       function(mpa.wt.matrix.){
                         mpa.wt.matrix.$rdsum <- rowSums(mpa.wt.matrix., 1)
                         
                         return(mpa.wt.matrix.)})
invisible(list2env(rel.dist.sum, globalenv()))


# ----- Moving Relative Distance Sums to Column 1
movecol2 <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                   function(mpa.wt.matrix.){
                     mpa.wt.matrix. <- mpa.wt.matrix.%>%dplyr::select(rdsum, everything())
                     
                     return(mpa.wt.matrix.)})
invisible(list2env(movecol2, globalenv()))


# ----- Removing RDSum Column
min.b.gone <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                     function(mpa.wt.matrix.)
                       mpa.wt.matrix. <- mpa.wt.matrix.[,-1]
                     )
invisible(list2env(min.b.gone, globalenv()))

# -- Moving Settlement ID from Row Names to Column
site.columns <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                       function(mpa.wt.matrix.){
                         mpa.wt.matrix.$SettlementID <- rownames(mpa.wt.matrix.)
                         
                         return(mpa.wt.matrix.)})
invisible(list2env(site.columns, globalenv()))

# -- Reorganize Matrices into Column Format
melting <- lapply(mget(paste0("mpa.wt.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                  function(mpa.wt.matrix.)
                    melt(mpa.wt.matrix., id.vars = "SettlementID", variable.name = "SiteID", value.name = "Relative_Weight"))

invisible(list2env(melting, globalenv()))



# ---- 3.4 Calculate raw distances ----

# -- Distances as data frames
mpa.dist.matrix.1 <- as.data.frame(mpa.matrix.1)
mpa.dist.matrix.2 <- as.data.frame(mpa.matrix.2)
mpa.dist.matrix.4 <- as.data.frame(mpa.matrix.4)
mpa.dist.matrix.5 <- as.data.frame(mpa.matrix.5)
mpa.dist.matrix.6 <- as.data.frame(mpa.matrix.6)
mpa.dist.matrix.7 <- as.data.frame(mpa.matrix.7)
mpa.dist.matrix.9 <- as.data.frame(mpa.matrix.9)
mpa.dist.matrix.15 <- as.data.frame(mpa.matrix.15)
mpa.dist.matrix.16 <- as.data.frame(mpa.matrix.16)
mpa.dist.matrix.17 <- as.data.frame(mpa.matrix.17)
mpa.dist.matrix.18 <- as.data.frame(mpa.matrix.18)

# -- Moving Settlement ID from Row Names to Column
site.columns <- lapply(mget(paste0("mpa.dist.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                       function(mpa.dist.matrix.){
                         mpa.dist.matrix.$SettlementID <- rownames(mpa.dist.matrix.)
                         
                         return(mpa.dist.matrix.)})
invisible(list2env(site.columns, globalenv()))


# -- Reorganize Matrices into Column Format
melting <- lapply(mget(paste0("mpa.dist.matrix.", c(1,2,4,5,6,7,9,15,16,17,18))),
                  function(mpa.dist.matrix.)
                    melt(mpa.dist.matrix., id.vars = "SettlementID", variable.name = "SiteID", value.name = "Distance_m"))

invisible(list2env(melting, globalenv()))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: OUTPUT DATA FRAME WITH ALL DISTANCES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 4.1 Combining Data Frames ----

all.distances <- 
  rbind(mpa.dist.matrix.1, mpa.dist.matrix.2, mpa.dist.matrix.4, 
        mpa.dist.matrix.5, mpa.dist.matrix.6, mpa.dist.matrix.7,
        mpa.dist.matrix.9, mpa.dist.matrix.15, mpa.dist.matrix.16,
        mpa.dist.matrix.17, mpa.dist.matrix.18) %>%
  mutate(SettlementID = as.character(SettlementID),
         SettlementID = as.numeric(SettlementID))

all.weights <-
  rbind(mpa.wt.matrix.1, mpa.wt.matrix.2, mpa.wt.matrix.4, 
        mpa.wt.matrix.5, mpa.wt.matrix.6, mpa.wt.matrix.7,
        mpa.wt.matrix.9, mpa.wt.matrix.15, mpa.wt.matrix.16,
        mpa.wt.matrix.17, mpa.wt.matrix.18) %>%
  mutate(SettlementID = as.character(SettlementID),
         SettlementID = as.numeric(SettlementID))

all.distances.weights <- 
  left_join(all.distances, all.weights, by=c("SettlementID","SiteID"))


weight.crosscheck <-
  all.weights %>%
  group_by(SettlementID) %>%
  dplyr::summarise(weight.sum=sum(Relative_Weight),
                   num.sites=length(SiteID))


# ---- 4.2 Export to flat file to be read into synergies/tradeoffs analysis ----

export(all.distances.weights, 
       paste0('x_Flat_data_files/1_Social/Outputs/Synergies_tradeoffs/Distance_weights_allsites_persett_', 
              format(Sys.Date(), format = "%Y%m%d"), 
              '.csv', sep = ""))
