require(sf)
require(leaflet)
require(rio)
require(tidyverse)
#require(lwgeom)
require(geosphere)

boundir <- ("R:/Gill/RMesa/Gill_Retreat/Spatial/2017_BHS_MPA_Boundary/")
settledir <- ("R:/Gill/RMesa/Gill_Retreat/Spatial/SBS_BHS_Social_Monitoring_Site_Locations/")

# Importing shapefiles
mpa.bound <- st_read(paste0(boundir,"BHS MPA boundaries.shp"))
mpa.bound <- st_zm(mpa.bound)

monitor <- st_read(paste0(settledir,"20190108_socmon_location.shp")) 


# plot(st_geometry(mpa.bound))
# plot(st_geometry(monitor))

## ---- Leaflet for monitoring sites in BHS ----
# Depiction of BHS MPAs boundaries and the monitoring sites differentiated by treatment (blue = 0, red = 1)
treatment.col <- colorFactor(c("blue","red"),
                             domain = as.numeric(c(0:1)))

leaflet() %>% 
  addTiles() %>% 
  addScaleBar() %>% 
  addPolygons(data = mpa.bound,
              color = "orange") %>% 
  addCircleMarkers(data = monitor,
                   radius = 1,
                   color = ~treatment.col(Treatment),
                   label = ~MPA_NAME,
                   popup = ~Settleme_1)


## ---- Geodesic distance ----
# Separating monitoring and control sites ( BHS only: MPAID<7)

tr.sites <- monitor %>% 
  filter(Treatment==1  & MPAID<7) %>% 
  select(Settlement:MPA_NAME,Y,X) %>% 
  st_set_geometry(NULL)

ctrl.sites <- monitor %>% 
  filter(Treatment==0  & MPAID<7) %>% 
  select(Settlement:MPA_NAME,Y,X)%>% 
  st_set_geometry(NULL)

# Creating an empty dataframe with the variables of interest 
dist.table <- data.frame(MPA = tr.sites$MPA_NAME,MPAID=tr.sites$MPAID,Settlement_ID=tr.sites$Settlement,Settlement = tr.sites$Settleme_1,
                         mean.dist = NA,min.dist = NA,max.dist = NA)

# dist.km <- distGeo(tr.sites[1,c("X","Y")],ctrl.sites[c("X","Y")])/1000

# Loop to calculate the mean, minimum and maximum distance from each of the treatment sites to all control sites
for (i in 1:nrow(tr.sites)) {
  dist.km <- distGeo(tr.sites[i,c("X","Y")],ctrl.sites[c("X","Y")])/1000
  dist.table$mean.dist[i] = mean(dist.km)
  dist.table$min.dist[i] = min(dist.km)
  dist.table$max.dist[i] = max(dist.km)
}

# dist.table <- dist.table %>% 
#   group_by(MPAID,MPA) %>% 
#   summarise(mean.dist=mean(mean.dist),
#             min.dist=min(min.dist),
#             max.dist=max(max.dist))
min(dist.table$min.dist);max(dist.table$min.dist);mean(dist.table$min.dist)


