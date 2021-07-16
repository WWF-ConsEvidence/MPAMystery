# 
# code: calculate each reef site's nearest distance to 50m+ ocean depth, mangrove habitat, & watershed-based pollution threat assessment
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: December 2020
# modified: 
# 
# ---- inputs ----
# 1) csv file of eco sites for inclusion in BHS/SBS synergies/tradeoffs analysis (with lat/long included)
# 2) shapefile for areas with depth greater than or equal to 50m (https://www.gebco.net/data_and_products/gridded_bathymetry_data/#a1; GEBCO user-defined area, downloaded 12/2020)
#    -- filtered to include elevation -50m or deeper, only including eastern Indonesia, and vectorized in QGIS
# 3) shapefile for mangrove habitat (https://data.unep-wcmc.org/datasets/45; GMW 2015 layer, downloaded 12/2020)
#    -- filtered to only eastern Indonesia in QGIS
# 4) shapefile from Reefs Revisited dataset (https://www.wri.org/publication/reefs-risk-revisited; download Local Threats Data & GIS Meta Data)
#    -- filtered to only eastern Indonesia in QGIS, using rf_sed_poly.shp from Local_Threats/Watershed_Pollution
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD LIBRARIES & PRE-PROCESS DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


pacman::p_load(rio, dplyr, raster, rgdal, rgeos)


# ---- 1.1 Define functions to transform from lat/long to UTM for distance calculations ----
# -- returns the SpatialPointsDataFrame for distance calculations

LongLatToUTM <- function(longitude,latitude) {
  xy <- data.frame(ID = 1:length(longitude), X = longitude, Y = latitude)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, CRS("+proj=utm +datum=WGS84"))
  return(res)
}


# ---- 1.2 Import eco site data & convert to UTM coordinates ----

reef.sites <- import('C:/Users/kclab/OneDrive/WWF/Manuscripts/Synergies_tradeoffs/2_Analysis/Eco/2020-12-17_DAB_Eco sites for inclusion.csv')

# points for reef sites in UTM
pts <- LongLatToUTM(reef.sites$longitude, reef.sites$latitude)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: DISTANCE TO DEEP WATER ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Import deep water shapefile ----

depth.grt50m <- shapefile('C:/Users/kclab/OneDrive/WWF/Manuscripts/Synergies_tradeoffs/2_Analysis/Eco/Covariates_for_matching/GEBCO_2020_18_Dec_2020_74aba7a81c57/GEBCO_Depth_grt50m.shp')

# transform projection to UTM coordinates
deepwater.p <- spTransform(depth.grt50m, CRS("+proj=utm +datum=WGS84"))


# ---- 2.2 Calculate minimum Cartesian (Euclidean) distance from each eco site point to deep water ----

# set up containers for results 
nsites <- length(pts)
nearest.deepwater.poly <- character(nsites)
dist.nearest.site.deepwater <- numeric(nsites)

# calculate distance from each point to nearest edge of each polygon, and choose the minimum 
for (i in seq_along(nearest.deepwater.poly)) {
  gdists <- gDistance(pts[i,], deepwater.p, byid=TRUE)
  nearest.deepwater.poly[i] <- deepwater.p$fid[which.min(gdists)]
  dist.nearest.site.deepwater[i] <- min(gdists)
}


# ---- 2.3 Create data frame with categorizations for distance to deep water ----

deepwater.dists <-
  data.frame(deepwater_dist_m = dist.nearest.site.deepwater) %>%
  mutate(deepwater_dist_category = ifelse(deepwater_dist_m<=50, "0-50m", 
                                          ifelse(deepwater_dist_m>50 & deepwater_dist_m<=250, "50-250m",
                                                 ifelse(deepwater_dist_m>250 & deepwater_dist_m<=500, "250-500m",
                                                        ifelse(deepwater_dist_m>500 & deepwater_dist_m<=1000, "0.5-1km",
                                                               ifelse(deepwater_dist_m>1000 & deepwater_dist_m<=2000, "1-2km",
                                                                      ifelse(deepwater_dist_m>2000 & deepwater_dist_m<=4000, "2-4km",
                                                                             ifelse(deepwater_dist_m>4000, ">4km", NA))))))))
  

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: DISTANCE TO MANGROVE HABITAT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Import mangrove habitat shapefile ----

mangrove.hab <- shapefile('C:/Users/kclab/OneDrive/WWF/Manuscripts/Synergies_tradeoffs/2_Analysis/Eco/Covariates_for_matching/GMW_001_GlobalMangroveWatch_2015/04_Filtered_data/GMW_2015_filtered_Indo.shp')

# transform projection to UTM coordinates
mangrove.p <- spTransform(mangrove.hab, CRS("+proj=utm +datum=WGS84"))


# ---- 3.2 Calculate minimum Cartesian (Euclidean) distance from each eco site point to mangrove habitat ----

# set up containers for results 
nsites <- length(pts)
nearest.mangrove.poly <- character(nsites)
dist.nearest.site.mangrove <- numeric(nsites)

# calculate distance from each point to nearest edge of each polygon, and choose the minimum 
for (i in seq_along(nearest.mangrove.poly)) {
  gdists <- gDistance(pts[i,], mangrove.p, byid=TRUE)
  nearest.mangrove.poly[i] <- mangrove.p$ogc_fid[which.min(gdists)]
  dist.nearest.site.mangrove[i] <- min(gdists)
}

# ---- 3.3 Create data frame with categorizations for distance to mangrove habitat ----

mangrove.dists <-
  data.frame(mangrove_dist_m = dist.nearest.site.mangrove) %>%
  mutate(mangrove_dist_category = ifelse(mangrove_dist_m<=5000, "0-5km", 
                                          ifelse(mangrove_dist_m>5000 & mangrove_dist_m<=10000, "5-10km",
                                                 ifelse(mangrove_dist_m>10000 & mangrove_dist_m<=15000, "10-15km",
                                                        ifelse(mangrove_dist_m>15000 & mangrove_dist_m<=20000, "15-20km",
                                                               ifelse(mangrove_dist_m>20000 & mangrove_dist_m<=25000, "20-25km",
                                                                      ifelse(mangrove_dist_m>25000 & mangrove_dist_m<=45000, "25-45km",
                                                                             ifelse(mangrove_dist_m>45000, ">45km", NA))))))))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: WATERSHED-BASED POLLUTION  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 4.1 Import watershed-based pollution / threats shapefile ----

reefs.risk <- shapefile('C:/Users/kclab/OneDrive/WWF/Manuscripts/Synergies_tradeoffs/2_Analysis/Eco/Covariates_for_matching/ReefsRisk_Local_Threats/rf_sed_poly_filtered_Indo.shp')

# transform projection to UTM coordinates
reefsrisk.p <- spTransform(reefs.risk, CRS("+proj=utm +datum=WGS84"))


# ---- 4.2 Calculate minimum Cartesian (Euclidean) distance from each eco site point to threat-assessed polygons ----

# set up containers for results 
nsites <- length(pts)
nearest.reefsrisk.poly <- character(nsites)
threat.nearest.reefsrisk.poly <- character(nsites)
dist.nearest.site.reefsrisk <- numeric(nsites)

# calculate distance from each point to nearest edge of each polygon, and choose the minimum 
for (i in seq_along(nearest.reefsrisk.poly)) {
  gdists <- gDistance(pts[i,], reefsrisk.p, byid=TRUE)
  nearest.reefsrisk.poly[i] <- reefsrisk.p$ID[which.min(gdists)]
  threat.nearest.reefsrisk.poly[i] <- reefsrisk.p$THREAT_TXT[which.min(gdists)]
  dist.nearest.site.reefsrisk[i] <- min(gdists)
}


# ---- 4.3 Create data frame with categorizations for distance to mangrove habitat ----

reefsrisk.dists.threat <-
  data.frame(threat.nearest.reefsrisk.poly, dist.nearest.site.reefsrisk)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: EXPORT  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 5.1 Compile final dataframe ----

# join each calculated dataframe back to the original reef.sites dataframe
reef.sites <-
  cbind.data.frame(reef.sites, deepwater.dists, mangrove.dists, reefsrisk.dists.threat) %>%
  rename("pollution_threat" = "threat.nearest.reefsrisk.poly",
         "dist_threat_poly_m" = "dist.nearest.site.reefsrisk")


# ---- 5.2 Export ----

export(reef.sites, 'C:/Users/kclab/OneDrive/WWF/Manuscripts/Synergies_tradeoffs/2_Analysis/Eco/Covariates_for_matching/Eco_spatial_covariates_formatch_20201220.csv')
