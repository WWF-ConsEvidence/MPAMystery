library(dplyr)
library(raster)

gps.final.2<-data.for.weighting.sites
grav2<-population.for.weighting
  
  
MPA.1<-gps.final.2[which(gps.final.2$MPA.ID==1),]
MPA.2<-gps.final.2[which(gps.final.2$MPA.ID==2),]
MPA.3<-gps.final.2[which(gps.final.2$MPA.ID==3),]
MPA.4<-gps.final.2[which(gps.final.2$MPA.ID==4),]
MPA.5<-gps.final.2[which(gps.final.2$MPA.ID==5),]
MPA.6<-gps.final.2[which(gps.final.2$MPA.ID==6),]
MPA.15<-gps.final.2[which(gps.final.2$MPA.ID==15),]
MPA.16<-gps.final.2[which(gps.final.2$MPA.ID==16),]

site.crosscheck<-rbind(MPA.1, MPA.2, MPA.3, MPA.4, MPA.5, MPA.6, MPA.15, MPA.16)
colnames(site.crosscheck)
site.crosscheck2<-
  site.crosscheck%>%
  group_by(Site_ID)%>%
  summarise_all(funs(mean))

MPA.1x<-unique(MPA.1[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.1y<-unique(MPA.1[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.2x<-unique(MPA.2[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.2y<-unique(MPA.2[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.3x<-unique(MPA.3[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.3y<-unique(MPA.3[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.4x<-unique(MPA.4[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.4y<-unique(MPA.4[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.5x<-unique(MPA.5[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.5y<-unique(MPA.5[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.6x<-unique(MPA.6[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.6y<-unique(MPA.6[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.15x<-unique(MPA.15[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.15y<-unique(MPA.15[,c("Settlement_ID", "x", "y", "MPA.ID")])
MPA.16x<-unique(MPA.16[,c("Site_ID", "long.x", "lat.x", "MPA.ID")])
MPA.16y<-unique(MPA.16[,c("Settlement_ID", "x", "y", "MPA.ID")])

#Run PointDistance
MPA.1.dist<-as.data.frame(pointDistance(MPA.1x[,2:3], MPA.1y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.1.dist)<-MPA.1x$Site_ID
colnames(MPA.1.dist)<-MPA.1y$Settlement_ID

#Run PointDistance
MPA.2.dist<-as.data.frame(pointDistance(MPA.2x[,2:3], MPA.2y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.2.dist)<-MPA.2x$Site_ID
colnames(MPA.2.dist)<-MPA.2y$Settlement_ID

#Run PointDistance
MPA.3.dist<-as.data.frame(pointDistance(MPA.3x[,2:3], MPA.3y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.3.dist)<-MPA.3x$Site_ID
colnames(MPA.3.dist)<-MPA.3y$Settlement_ID

#Run PointDistance
MPA.4.dist<-as.data.frame(pointDistance(MPA.4x[,2:3], MPA.4y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.4.dist)<-MPA.4x$Site_ID
colnames(MPA.4.dist)<-MPA.4y$Settlement_ID

#Run PointDistance
MPA.5.dist<-as.data.frame(pointDistance(MPA.5x[,2:3], MPA.5y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.5.dist)<-MPA.5x$Site_ID
colnames(MPA.5.dist)<-MPA.5y$Settlement_ID

#Run PointDistance
MPA.6.dist<-as.data.frame(pointDistance(MPA.6x[,2:3], MPA.6y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.6.dist)<-MPA.6x$Site_ID
colnames(MPA.6.dist)<-MPA.6y$Settlement_ID

#Run PointDistance
MPA.15.dist<-as.data.frame(pointDistance(MPA.15x[,2:3], MPA.15y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.15.dist)<-MPA.15x$Site_ID
colnames(MPA.15.dist)<-MPA.15y$Settlement_ID

#Run PointDistance
MPA.16.dist<-as.data.frame(pointDistance(MPA.16x[,2:3], MPA.16y[,2:3], lonlat=T, allpairs = T))
#Set Column and Row Names for Matrix
rownames(MPA.16.dist)<-MPA.16x$Site_ID
colnames(MPA.16.dist)<-MPA.16y$Settlement_ID

#rm(MPA.1, MPA.2, MPA.3, MPA.4, MPA.5, MPA.6, MPA.15, MPA.16)
#rm(MPA.1x, MPA.1y, MPA.2x, MPA.2y, MPA.3x, MPA.3y, MPA.4x, MPA.4y, MPA.5x, MPA.5y, MPA.6x, MPA.6y, MPA.15x, MPA.15y, MPA.16x, MPA.16y)

mpa.matrix.1<-MPA.1.dist
mpa.matrix.2<-MPA.2.dist
mpa.matrix.3<-MPA.3.dist
mpa.matrix.4<-MPA.4.dist
mpa.matrix.5<-MPA.5.dist
mpa.matrix.6<-MPA.6.dist
mpa.matrix.15<-MPA.15.dist
mpa.matrix.16<-MPA.16.dist

#--Transpose Data Frames
transpose<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                  function(mpa.matrix.)
                    mpa.matrix.<-as.data.frame(t(mpa.matrix.))) 

invisible(list2env(transpose, globalenv()))


#--Square Distances
mpa.matrix.1<-as.data.frame(mpa.matrix.1^2)
mpa.matrix.2<-as.data.frame(mpa.matrix.2^2)
mpa.matrix.3<-as.data.frame(mpa.matrix.3^2)
mpa.matrix.4<-as.data.frame(mpa.matrix.4^2)
mpa.matrix.5<-as.data.frame(mpa.matrix.5^2)
mpa.matrix.6<-as.data.frame(mpa.matrix.6^2)
mpa.matrix.15<-as.data.frame(mpa.matrix.15^2)
mpa.matrix.16<-as.data.frame(mpa.matrix.16^2)

#--Merge With Pop Data
popdata<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                function(mpa.matrix.)
                  mpa.matrix.<-merge(mpa.matrix., grav2, by.x="row.names", by.y="SettlementID")) 

invisible(list2env(popdata, globalenv()))

#--Fix Row Names
rownames(mpa.matrix.1)<-mpa.matrix.1$Row.names
rownames(mpa.matrix.2)<-mpa.matrix.2$Row.names
rownames(mpa.matrix.3)<-mpa.matrix.3$Row.names
rownames(mpa.matrix.4)<-mpa.matrix.4$Row.names
rownames(mpa.matrix.5)<-mpa.matrix.5$Row.names
rownames(mpa.matrix.6)<-mpa.matrix.6$Row.names
rownames(mpa.matrix.15)<-mpa.matrix.15$Row.names
rownames(mpa.matrix.16)<-mpa.matrix.16$Row.names

#--Remove Row.names Column
removerow<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                  function(mpa.matrix.)
                    mpa.matrix.<-subset(mpa.matrix., select=-c(Row.names))) 

invisible(list2env(removerow, globalenv()))

#--Divide Pop Size by Each Distance
gravity<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                function(mpa.matrix.)
                  mpa.matrix.<-(mpa.matrix.$pop)/mpa.matrix.) 

invisible(list2env(gravity, globalenv()))

#--Remove Row.names Column
removepop<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                  function(mpa.matrix.)
                    mpa.matrix.<-subset(mpa.matrix., select=-c(popfishp))) 

invisible(list2env(removepop, globalenv())) 

#---Transpose back to Original Format
transpose2<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                   function(mpa.matrix.)
                     mpa.matrix.<-as.data.frame(t(mpa.matrix.))) 

invisible(list2env(transpose2, globalenv()))



#Automated Initial Matrix Processing
###########################################################################################################################################################
#--Removing NA Columns
na.b.gone<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                  function(mpa.matrix.)
                    mpa.matrix.[!sapply(mpa.matrix., function(x) all(is.na(x)))]) 

invisible(list2env(na.b.gone, globalenv()))

#--Converting Distances into Relative Importance Values
#-----Calculating Sum of Weights
grav.sum<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                 function(mpa.matrix.){
                   mpa.matrix.$gravsum<-apply(mpa.matrix., 1, FUN=sum)
                   
                   return(mpa.matrix.)})
invisible(list2env(grav.sum, globalenv()))

#-----Moving Minimum Distance to Column 1
movecol<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                function(mpa.matrix.){
                  mpa.matrix.<-mpa.matrix.%>%dplyr::select(gravsum, everything())
                  
                  return(mpa.matrix.)})
invisible(list2env(movecol, globalenv()))



#-----Multiplying 1/x by min.dist to Calculate Relative Distance
relative.dist<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                      function(mpa.matrix.)
                        sweep(mpa.matrix., MARGIN = 1, FUN="/", STATS=mpa.matrix.$gravsum)
                      
)
invisible(list2env(relative.dist, globalenv()))

#-----Removing Min Dist Column
min.b.gone<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                   function(mpa.matrix.)
                     mpa.matrix.<-mpa.matrix.[,-1]
                   
)
invisible(list2env(min.b.gone, globalenv()))

#-----Calculate Relative Distance Sums
rel.dist.sum<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                     function(mpa.matrix.){
                       mpa.matrix.$rdsum<-rowSums(mpa.matrix., 1)
                       
                       return(mpa.matrix.)})
invisible(list2env(rel.dist.sum, globalenv()))

#-----Moving Relative Distance Sums to Column 1
movecol2<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                 function(mpa.matrix.){
                   mpa.matrix.<-mpa.matrix.%>%dplyr::select(rdsum, everything())
                   
                   return(mpa.matrix.)})
invisible(list2env(movecol2, globalenv()))


#-----Removing RDSum Column
min.b.gone<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                   function(mpa.matrix.)
                     mpa.matrix.<-mpa.matrix.[,-1]
                   
)
invisible(list2env(min.b.gone, globalenv()))

#--Moving Site ID from Row Names to Column
site.columns<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                     function(mpa.matrix.){
                       mpa.matrix.$Site_ID<-rownames(mpa.matrix.)
                       
                       return(mpa.matrix.)})
invisible(list2env(site.columns, globalenv()))

#--Reorganize Matrices into Column Format
melting<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                function(mpa.matrix.)
                  melt(mpa.matrix., id.vars=c("Site_ID")))

invisible(list2env(melting, globalenv()))

#--Rename Columns
columnnames<-lapply(mget(paste0("mpa.matrix.", c(1,2,3,4,5,6,15,16))),
                    function(mpa.matrix.){
                      colnames(mpa.matrix.)<-c("Site_ID", "Settlement_ID", "Relative.Weight")
                      return(mpa.matrix.)})

invisible(list2env(columnnames, globalenv()))

#--Combining Data Frames (note this says "all distances" but is actually Relative weights that you then multiply your predictors by)
all.distances<-rbind(mpa.matrix.1, mpa.matrix.2, mpa.matrix.3, mpa.matrix.4, mpa.matrix.5, mpa.matrix.6, mpa.matrix.15, mpa.matrix.16)
all.distances$Settlement_ID<-as.character(all.distances$Settlement_ID)
all.distances$Settlement_ID<-as.numeric(all.distances$Settlement_ID)

weight.crosscheck<-
  all.distances%>%
  group_by(Site_ID)%>%
  dplyr::summarise(weight.sum=sum(Relative.Weight))