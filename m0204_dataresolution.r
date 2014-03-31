###################################################################################
## Thomas C. Edwards, Jr.
## Species Distribution Modelling Using R
## R-code Change Data Resolutions
## PROGRAM NAME: m0204-dataresolution.r
## PROGRAM FUNCTIONS: 
##   Apply different resample options to change data resolution
## last update:  31 March 2014
###################################################################################


## load libraries now if desired; loaded below when needed
#library(rgdal)
#library(raster)

## set pathnames
#path.root="~/words/classes/speciesdistrib-apr2014" # madhawk
path.root="~/words-15aug13/classes/speciesdistrib-apr2014" # madfish
path.mod2=paste(path.root,"/data/module02",sep="")
path.gis=paste(path.root,"/data/gis_layers",sep="")
path.figs=paste(path.root,"/class_powerpoints/figures",sep="")



################################################################################
######## START RESAMPLING OPTIONS
## import GIS layer into R; requires pkg raster
library(raster) # load pkg raster
library(rgdal)  # load pkg rgdal

####
## upscale (make coarse) resolution
library(raster)
setwd("/Users/jaymelewthwaite/Documents/Masters/R_Scripts/R_scripts/SDM_Workshop_Apr2014/Module2")
r1=raster("elev_10m_wgs.img"); r1               # import 10m data as raster
r2=aggregate(r1,fact=100,method="bilinear"); r2 # upscale 10m to 1km res

## plot rasters
par(mfrow=c(1,2))
plot(r1,main="10m resolution")
plot(r2,main="1km resolution")
## save plots if desired
#setwd(path.figs)
#savePlot(filename="mod2.4fig01.tiff",type="tiff")

####
## downscale (make finer) resolution
setwd(path.gis)
t1=raster("tave_yr.img"); t1                      # import 1km raster; examine
t2=disaggregate(t1,fact=10,method="bilinear"); t2 # downscale 1km to 100m res

## plot rasters
#par(mfrow=c(1,2))
plot(t1,main="1km resolution")
plot(t2,main="100m resolution")
## save plots if desired
#setwd(path.figs)
#savePlot(filename="mod2.4fig02.tiff",type="tiff")

####
## NOT RUN; resample input raster t1 to resolution of template raster e1
#setwd(path.gis)
#t1=raster("tave_yr.img"); t1         # import 1km raster; examine
#e1=raster("elev_10m_wgs.img"); e1    # import 10m elev; examine
#t2-resample(t1,e1,method="bilinear") # resample to 10m resolution

####
## reproject input raster to template raster
#setwd(path.gis)
#t1=raster("tave_yr.img"); t1              # import 1km raster; examine
#e1=raster("elev_10m_wgs.img"); e1         # import 10m elev; examine
#t2=projectRaster(t1,e1,method="bilinear") # reproject & resample to template e1 info

####
## resample a polygon
######## END RESAMPLING OPTIONS
################################################################################


################################################################################
#### GARBAGE BELOW
################################################################################

library(maptools)
setwd("C:\\Users\\tce\\Documents\\stats\\dwr_deserttortise2012\\gsmsoil_ut\\spatial")
prj.wgs84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # wgs84 projection
s1=readShapePoly("gsmsoilmu_a",proj4string=CRS(prj.wgs84)) #pkg maptools

setwd("C:\\Users\\tce\\Documents\\stats\\dwr_deserttortise2012\\data\\boundaries")
#s2=readShapePoly("bdw_box",proj4string=CRS(prj.wgs84)) #pkg maptools
s2.1=as(extent(s2),"SpatialPolygons")
proj4string(s2.1) <- CRS(proj4string(s1))

library(rgeos)
s3=gIntersection(s1,s2.1,byid=T)










