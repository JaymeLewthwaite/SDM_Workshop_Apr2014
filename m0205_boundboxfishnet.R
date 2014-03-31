###################################################################################
## Thomas C. Edwards, Jr.
## Species Distribution Modelling Using R
## R-code Building Raster Frames of Different Extent
##   Basis for selecting pseudo-absences
## PROGRAM NAME: m0205-boundboxfishnet.r
## PROGRAM FUNCTIONS: 
##   Build bounding boxes of extent, buffered, convex hull, & points-buffered
##   Assign projections, resolutions and values to bounding boxes
##   Basic plots of bounding boxes
##   Build fishnet for bounding boxes
##   Extract [X,Y] points from fishnet; basis for pseudo-absence selection
## last update:  31 March 2014
###################################################################################


## load libraries now if desired; loaded below when needed
#library(raster)   # raster manipulations
#library(maptools) # import shapefiles
#library(rgeos)    # buffering of points
#library(sp)       # polygon creations from points

## set pathnames
#path.root="~/words/classes/speciesdistrib-apr2014" # madhawk
path.root="~/words-15aug13/classes/speciesdistrib-apr2014" # madfish
path.mod2=paste(path.root,"/data/module02",sep="")
path.gis=paste(path.root,"/data/gis_layers",sep="")
path.figs=paste(path.root,"/class_powerpoints/figures",sep="")


################################################################################
######## START BUILD BOUNDING BOXES - 4 OPTIONS PRESENTED
setwd(path.mod2)
tru.pres=get(load("spp106.pres.RData")) # load presence data
dim(tru.pres); head(tru.pres,2) # examine presence data

######
#### OPTION #1:  build presence bounding box
##   determine extent (bounding box) of presence points
library(raster)
tru.ext=extent(min(tru.pres$wgs_xF),max(tru.pres$wgs_xF),
               min(tru.pres$wgs_yF),max(tru.pres$wgs_yF))
tru.ext # examine
#names(tru.pres)[1:2]=c("x","y")       # alternative call; change col names 1st
#tru.ext=extent(tru.pres[c(1:2)])      # alternative call after col name changes

## assign projection & value to extent
prj.wgs84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # wgs84 projection
tru.box=raster(tru.ext,crs=prj.wgs84)  # convert extent to raster w/wgs84 projection
tru.box # examine; note no resolution or value
######

######
#### OPTION #2:  build buffered bounding box
##   simple rounding by 1 deg, ~100 km in UT; here add .5 deg
tru.extb=extent(min(tru.pres$wgs_xF)-.5,max(tru.pres$wgs_xF)+.5,
                min(tru.pres$wgs_yF)-.5,max(tru.pres$wgs_yF)+.5)
tru.extb # examine

## assign projection and value to extent
tru.buf=raster(tru.extb,crs=prj.wgs84)  # convert extent to raster w/wgs84 projection
tru.buf # examine; note no resolution or value
#res(tru.buf)=res(v1)              # assign resolution to bbox extent
######

######
#### OPTION #3:  build convex hull bounding box
## catechism for build convex hull
tru.c1=chull(tru.pres[c(1:2)]) # identifies vertices of convex polygon
head(tru.c1,2); tail(tru.c1,2) # note start of head() and end of tail() don't match
tru.c2=c(tru.c1,tru.c1[1])     # closes polygon
head(tru.c2,2); tail(tru.c2,2) # now start of head() and end of tail() match
tru.c3=tru.pres[tru.c2,]       # extract x,y of vertices ONLY; drop all other obs
head(tru.c3,2)                 
tru.xy=tru.c3[c(1:2)]          # keep only lat(x) long(y)
head(tru.xy,2)

## build the polygon
library(sp) # pkg for Polygon creation
tru.p1=Polygon(tru.xy)               # creates object spatial class obj; pkg sp
tru.p2=Polygons(list(tru.p1),"p1")   # weird catechism; accept at face value
tru.p3=SpatialPolygons(list(tru.p2)) # ditto
#plot(tru.p3)                        # polygon before rasterizing
#setwd(path.figs)
#savePlot(filename="mod2.5fig05.tiff",type="tiff")
tru.con=rasterize(tru.p3,tru.box)    # rasterize polygon to spatial extent
######

######
#### OPTION #4:  build point-buffered bounding box
## 1 deg, ~100 km in UT; here add .5 deg = ~50 km buffer
tru.xy=(tru.pres[c(1:2)])    # extract presence data
tru.sp=SpatialPoints(tru.xy) # convert to spatial object
#tru.cc=coordinates(tru.sp)   # assign coordinates; not used

## buffer each presence and dissolve all buffers into polygon
library(rgeos) # pkg for buffering points
tru.pb=gBuffer(tru.sp,width=.5) # width in decimal degrees; .5 = ~50km in UT
#tru.allpoly=gBuffer(tru.sp,width=.25,byid=T,quadsegs=2) # keep individual polygons 
tru.poly=rasterize(tru.pb,tru.buf)    # rasterize polygon to spatial extent
## OPTION #1:  convert presence extent to raster box w/projection
######
######## END BUILD BOUNDING BOXES - 4 OPTIONS PRESENTED
################################################################################



################################################################################
######## START ASSIGN RESOLUTION & VALUES TO BOUNDING BOXES
## obtain grain from existing GIS data layer; ex. is elevation
#setwd(path.topo1)
setwd(path.gis)
v1=raster("elev_1k_wgs.img") # load a GIS predictor layer 
res(v1)                      # resolution of existing GIS data layer

## assign resolution
res(tru.box)=res(v1)         # assign resolution to bbox extent
res(tru.buf)=res(v1)         # assign resolution to bbox extent
res(tru.con)=res(v1)         # assign resolution to bbox extent
res(tru.poly)=res(v1)        # assign resolution to bbox extent

## assign value to raster if needed 
values(tru.box)=1            # assign a value to bbox cells; arbitrary
values(tru.buf)=1            # assign a value to bbox cells; arbitrary
values(tru.con)=1            # assign a value to bbox cells; arbitrary
values(tru.poly)=1           # assign a value to bbox cells; arbitrary

#### NOT RUN:  input grain
#grain=c(0.008333333,0.008333333) # assign bbox extent resolution directly
#res(tru.box)=grain                # assign resolution
######## END ASSIGN RESOLUTION & VALUES TO BOUNDING BOXES
################################################################################



################################################################################
######## START PLOT BOUNDING BOXES
## add boundaries for pretty
setwd(path.gis)
library(maptools) # pkg for shapefiles
prj.wgs84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # wgs84 projection
states=readShapePoly("na_states_wgs",proj4string=CRS(prj.wgs84)) # import shapefile

####
## plot buffer bounding box w/presence points
plot(tru.buf,col="gray90",legend=F,main="Buffered (~50 km) bounding box on presence points")
points(tru.pres$wgs_xF,tru.pres$wgs_yF,pch=20,col="darkgreen") # add points to plot
plot(states,add=T,lwd=1.5)                                     # make pretty w/boundaries
plot(tru.ext,col="red",add=T)                                  # plot extent w/out buffer

## save polygon as .img  and .tiff if desired
#setwd(path.gis)
#writeRaster(tru.buf,"tru_buf",format="HFA",overwrite=T) # save as .img file
## outfile plot
#setwd(path.figs)
#savePlot(filename="mod2.5fig02.tiff",type="tiff")
####

####
## plot convex bounding box
plot(tru.con,col="gray90",legend=F,main="Convex bounding box on presence points")
points(tru.pres$wgs_xF,tru.pres$wgs_yF,pch=20,col="darkgreen") # add pnts to plot
points(tru.xy,pch=19,col="red")  # add convex vertices
plot(tru.p3,add=T,border="red")  # plot convex boundary
#points(tru.xy,pch=20,col="red")
plot(states,add=T,lwd=1.5)  # make prety w/state boundaries

## save polygon as .img  and .tiff if desired
#setwd(path.gis)
#writeRaster(tru.con,"tru_con",format="HFA",overwrite=T) # save as .img file
## outfile plot
#setwd(path.figs)
#savePlot(filename="mod2.5fig03.tiff",type="tiff")
####

####
## plot point-buffered bounding box
plot(tru.poly,col="gray90",legend=F,main="Buffered (~50 km) presence points")
plot(tru.pb,add=T,border="red") # add border to buffer pnts
points(tru.pres$wgs_xF,tru.pres$wgs_yF,pch=20,col="darkgreen") # add pnts to plot
plot(states,add=T,lwd=1.5) # make pretty w/boundaries
#plot(tru.ext,col="red",add=T)

## save polygon as .img  and .tiff if desired
#setwd(path.gis)
#writeRaster(tru.con,"tru_con",format="HFA",overwrite=T) # save as .img file
## outfile plot
#setwd(path.figs)
#savePlot(filename="mod2.5fig04.tiff",type="tiff")
####
######## END PLOT BOUNDING BOXES
################################################################################



################################################################################
######## START BUILD FISHNET OF BOUNDING BOX TO EXTRACT PSEUDO-ABSENCES
## ASSUME existing rasters from above of:
##   tru.ext   =>  raster rectangle, extent of presence points
##   tru.buf   =>  raster rectangle, buffered (~50 km) extent of presence points
##   tru.con   =>  raster convex polygon, extent of presence points
##   tru.poly  =>  raster polygon, buffered (~50 km) presence points
## CAUTION: begin extraction from largest of all possible extents; here => tru.buf
##   generates the largest possible fnet from which all other polygons extracted

## build background fishnet from buffered extent (rectangle)
library(raster)
library(sp)
f1=coordinates(tru.buf)               # set spatial coords from tru.buf
f2=cellFromXY(tru.buf,f1)             # extract cell number from buffered extent
tru.bufcc=as.data.frame(cbind(f1,f2)) # build datframe of x,y & cell number
names(tru.bufcc)[1:3]=c("cell.wgs_x","cell.wgs_y","FNETID") # assign names
tru.bufcc=tru.bufcc[c("FNETID","cell.wgs_x","cell.wgs_y")]  # reorder
dim(tru.bufcc); names(tru.bufcc) # fishnet data structure
head(tru.bufcc,2); tail(tru.bufcc,2)      # fishnet data structure

## extract backgrounds by polygon => extent of presence
fnet.buf=extract(tru.box,tru.bufcc[c("cell.wgs_x","cell.wgs_y")]) # vector of NA & 1; 1=in extent box
tru.boxcc=cbind(tru.bufcc,fnet.buf)         # bind buffer & convex fishnets
tru.boxcc=subset(tru.boxcc,fnet.buf==1)     # extract convex fnet from buffer fnet
tru.boxcc=tru.boxcc[c("FNETID","cell.wgs_x","cell.wgs_y")] # reorder
dim(tru.boxcc); names(tru.boxcc)            # convex data structure

## extract backgrounds by polygon => convex of presence
fnet.buf=extract(tru.con,tru.bufcc[c("cell.wgs_x","cell.wgs_y")]) # vector of NA & 1; 1=in convex polygon
tru.concc=cbind(tru.bufcc,fnet.buf)         # bind buffer & convex fishnets
tru.concc=subset(tru.concc,fnet.buf==1)     # extract convex fnet from buffer fnet
tru.concc=tru.concc[c("FNETID","cell.wgs_x","cell.wgs_y")] # reorder
dim(tru.concc); names(tru.concc)            # convex data structure

## extract backgrounds by polygon => buffered presence polygon
fnet.buf=extract(tru.poly,tru.bufcc[c("cell.wgs_x","cell.wgs_y")]) # vector of NA & 1; 1=in presence polygon 
tru.polycc=cbind(tru.bufcc,fnet.buf)         # bind buffer & presence polygon fishnets
tru.polycc=subset(tru.bufcc,fnet.buf==1)     # extract convex fnet from buffer fnet
tru.polycc=tru.polycc[c("FNETID","cell.wgs_x","cell.wgs_y")] # reorder
dim(tru.polycc); names(tru.polycc)           # convex data structure
######## START BUILD FISHNET OF BOUNDING BOX TO EXTRACT PSEUDO-ABSENCES
################################################################################



################################################################################
######## START EXTRACT PSEUDO-ABSENCES [X,Y]'s
## snap presences to fishnet grid; remove presence pnts from backgrounds
##   result is psuedo-absence dataframes
tru.xy=tru.pres[c(1:2)]     # get x,y of true presences
p1=coordinates(tru.xy)      # set spatial coords from tru.buf
p2=cellFromXY(tru.buf,p1)   # extract cell No. of presences from buffered extent
length(p2)                  # should match number of presences
length(tru.pres$SPPRES106)  # it does ... life is good
tru.presFNET=cbind(p2,tru.pres) # add fishnet ID to tru.pres
names(tru.presFNET)[1]="FNETID" # name change

## outfile modified tru.pres dataframe if desired
#setwd(path.mod2)
#save(tru.presFNET,file="spp106.presFNET.RData")

## drop presence cell No.s; remaining are possible psuedo-abs cell No.s
psu.box=tru.boxcc[!tru.boxcc$FNETID %in% p2,]    # psu-abs for extent box
psu.buf=tru.bufcc[!tru.bufcc$FNETID %in% p2,]    # psu-abs for buffered box
psu.con=tru.concc[!tru.concc$FNETID %in% p2,]    # psu-abs for convex
psu.poly=tru.polycc[!tru.polycc$FNETID %in% p2,] # psu-abs for buffered presences
dim(tru.polycc); dim(psu.poly)
dim(tru.polycc)[1]-dim(psu.poly)[1]              # difference = No. presences
head(psu.poly)
psu.poly$SPPRES106=0; head(psu.poly)             # add presence = 0; will use later
head(tru.presFNET,2)

## outfile if desired
#setwd(path.mod2)
#out.files=ls(pattern="psu")
#save(list=out.files,file="spp106.psuabs.RData")

## NOT RUN:  load pseudo-abs $ true pres dataframes if not in workspace
#setwd(path.mod2)
#load("spp106.psuabs.RData"); ls(pattern="psu") # load psu-abs dataframes
#head(psu.poly,2)
#tru.pres=get(load("spp106.presFNET.RData"))  # load spp presence w/FNETID
#head(tru.pres,2)

## options for selecting pseudo-abs from fishnets; buffered presence poly ex.
psu.srs1=psu.poly[sample(1:nrow(psu.poly),dim(tru.pres)[1],replace=F),]  # N=No. of pres
psu.srs2=psu.poly[sample(1:nrow(psu.poly),2*dim(tru.pres)[1],replace=F),]  # N=2*No. pres
psu.srs4=psu.poly[sample(1:nrow(psu.poly),4*dim(tru.pres)[1],replace=F),]  # N=4*No. pres
psu.srs10=psu.poly[sample(1:nrow(psu.poly),10*dim(tru.pres)[1],replace=F),] # N=10*No. pres
dim(psu.srs1); dim(psu.srs10) # data structures

## merge with true presences for predictor extraction
head(tru.presFNET,2); head(psu.srs1,2) # examine; both MUST have FNETID
spp106.tr=merge(tru.presFNET,psu.srs1,by=c("FNETID","SPPRES106"),all=T) # merge
dim(spp106.tr); head(spp106.tr,2) # dim => No. pres + No. psu-abs
spp106.tr[34:36,]  # note diff [X,Y] coords for pres=0 & =1

## create new vars wgs_x & wgs_y; used later in raster stack extraction
spp106.tr$wgs_x=ifelse(spp106.tr$SPPRES106==0,
                       spp106.tr$cell.wgs_x,spp106.tr$wgs_xF)
spp106.tr$wgs_y=ifelse(spp106.tr$SPPRES106==0,
                       spp106.tr$cell.wgs_y,spp106.tr$wgs_yF)

## resulting data frame is model training data set; outfile if desired
setwd(path.mod2)
write.csv(spp106.tr,file="spp106.presabs.csv",row.names=F) # save .csv
save(spp106.tr,file="spp106.presabs.RData")                # save .RData
######## START EXTRACT PSEUDO-ABSENCES [X,Y]'s
################################################################################




#################################################################################################
####  GARBAGE BELOW
#################################################################################################

## ex. code from pkg rgeos, gBuffer fxn
p1 = readWKT("POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1))")
p2 = readWKT("POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1))")
par(mfrow=c(2,3))
plot(gBuffer(p1,width=-0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p1,width=0),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p1,width=0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: 0.2")
plot(gBuffer(p2,width=-0.2),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p2,width=0),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p2,width=0.2),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: 0.2")

## create dataframe of x,y over entire