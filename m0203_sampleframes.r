###################################################################################
## Thomas C. Edwards, Jr.
## Species Distribution Modelling Using R
## R-code Additive Logistic Regression GAM
## PROGRAM NAME: m0203-sampleframes.r
## PROGRAM FUNCTIONS: 
##   Descriptive statistics by several different sample frames
## last update:  31 March 2014
###################################################################################

## load libraries now if desired; loaded below when needed
# none used

## set pathnames
#path.root="~/words/classes/speciesdistrib-apr2014" # madhawk
#path.root="~/words-15aug13/classes/speciesdistrib-apr2014" # madfish
path.root="~/words-15aug13/classes/speciesdistrib-apr2014MON" # madfish
path.mod2=paste(path.root,"/data/module02",sep="")
path.figs=paste(path.root,"/class_powerpoints/figures",sep="")


################################################################################
######## START MEANS AMONG PREDICTORS BY FRAMES
setwd("/Users/jaymelewthwaite/Documents/Masters/R_scripts/R_scripts/SDM_Workshop_Apr2014/Module2")
spp.t=get(load("spp106.topo.RData"))          # frame => all

## subset frames
spp.t.dd=subset(spp.t,dry.domfr==1)           # frame => dry domain
spp.t.fia=subset(spp.t.dd,SPPRES106.fiafr==1) # frame => FIA
spp.t.cmb=subset(spp.t.dd,SPPRES106.cmbfr==1) # frame => FIA+Little
dim(spp.t)                                    # dim of frame => all
dim(spp.t.dd)                                 # dim of frame => dry domain
dim(spp.t.fia)                                # dim of frame => FIA
dim(spp.t.cmb)                                # dim of frame => FIA+Little
#JL how you build the frame has consequences on the amount of data you have; here we have lots of data so we don't have to worry

## topo means by pres/abs
#JL: AGGREGATE: grab some columns, create function, apply function to these columns
aggregate(spp.t[,9:15],list(spp.t$SPPRES106),mean,na.rm=T) # frame=all
#JL: had all the pres/absences; in the 0, 1000m mean for elevation. in the 1, have 1800 for elevation. 
#these are all points, not clipped to DD
aggregate(spp.t.dd[,9:15],list(spp.t.dd$SPPRES106),mean,na.rm=T) # frame=dry domain
#JL: green and red points, but clipped to DD; once some points removed, mean dropped. So these points were in high elevation
aggregate(spp.t.fia[,9:15],list(spp.t.fia$SPPRES106),mean,na.rm=T) # frame=fia
#JL: thses are just the green points: huge change in elevation here; by taking points and buffering ecologically, have a difficult time discriminating between the two groups
aggregate(spp.t.cmb[,9:15],list(spp.t.cmb$SPPRES106),mean,na.rm=T) # frame=fia+Little


#JL: now trying with different vairables:
clim=get(load("spp106.clim.RData"))

spp.clim.dd=subset(spp.t,dry.domfr==1)           # frame => dry domain
spp.clim.fia=subset(spp.t.dd,SPPRES106.fiafr==1) # frame => FIA
spp.clim.cmb=subset(spp.t.dd,SPPRES106.cmbfr==1) # frame => FIA+Little

aggregate(spp.clim[,9:15],list(spp.clim$SPPRES106),mean,na.rm=T) # frame=all
aggregate(spp.clim.dd[,9:15],list(spp.t.dd$SPPRES106),mean,na.rm=T) # frame=dry domain
aggregate(spp.clim.fia[,9:15],list(spp.t.fia$SPPRES106),mean,na.rm=T) # frame=fia
aggregate(spp.clim.cmb[,9:15],list(spp.t.cmb$SPPRES106),mean,na.rm=T) # frame=fia+Little

## topo min/max by pres/abs
aggregate(spp.t[,9:15],list(spp.t$SPPRES106),min,na.rm=T) # frame=all
aggregate(spp.t[,9:15],list(spp.t$SPPRES106),max,na.rm=T) # frame=all
## topo min/max by pres/abs, frame => dry domain
aggregate(spp.t.dd[,9:15],list(spp.t.dd$SPPRES106),min,na.rm=T) # frame=dry domain
aggregate(spp.t.dd[,9:15],list(spp.t.dd$SPPRES106),max,na.rm=T) # frame=dry domain
## topo min/max by pres/abs, frame => fia 
aggregate(spp.t.fia[,9:15],list(spp.t.fia$SPPRES106),min,na.rm=T) # frame=fia
aggregate(spp.t.fia[,9:15],list(spp.t.fia$SPPRES106),max,na.rm=T) # frame=fia
## topo min/max by pres/abs, frame => fia+little
aggregate(spp.t.cmb[,9:15],list(spp.t.cmb$SPPRES106),min,na.rm=T) # frame=fia+Little
aggregate(spp.t.cmb[,9:15],list(spp.t.cmb$SPPRES106),max,na.rm=T) # frame=fia+Little

## topo range by pres/abs
aggregate(spp.t[,9:11],list(spp.t$SPPRES106),range,na.rm=T) # frame=all
aggregate(spp.t.dd[,9:11],list(spp.t.dd$SPPRES106),range,na.rm=T) # frame=dry domain
aggregate(spp.t.fia[,9:11],list(spp.t.fia$SPPRES106),range,na.rm=T) # frame=fia
aggregate(spp.t.cmb[,9:11],list(spp.t.cmb$SPPRES106),range,na.rm=T) # frame=fia+Little

## boxplots by pres/abs and 2 frames
par(mfrow=c(1,2))
boxplot(spp.t[,10]~spp.t$SPPRES106,xlab="Presence:Absence",
        ylab="Elevation",main="Frame=All")
boxplot(spp.t.fia[,10]~spp.t.fia$SPPRES106,xlab="Presence:Absence",
        ylab="Elevation",main="Frame=FIA")
## save plots if desired
#setwd(path.figs)
#savePlot(filename="mod2.3fig.tiff",type="tiff")
######## END MEANS AMONG PREDICTORS BY FRAMES
################################################################################



###################################################################################
## GARBAGE BELOW
###################################################################################



######## START CORRELATIONS AMONG PREDICTORS
#### START MODIFIED panel.cor CORRELATION FUNCTION       
##   determine correlations among predictor variables: modified from 
##   http://addictedtor.free.fr/graphiques/graphcode.php?graph=137
##     function for modified pairs() plot

panel.cor=function(x,y,digits=2,prefix="",cex.cor) {
  usr=par("usr"); on.exit(par(usr)) 
  par(usr=c(0,1,0,1)) 
  #r=abs(cor(x,y)) # activate if want all cor shown as absolute values
  r=cor(x,y)       # show correlation as +/1; no abs applied
  # r=cor(x,y,method="spearman")  # spearman cor instead
  txt=format(c(r,0.123456789),digits=digits)[1] 
  txt=paste(prefix,txt,sep="") 
  if(missing(cex.cor)) cex=0.8/strwidth(txt) 
  test=cor.test(x,y)  # deactivate if desire other cor method
  # test=cor.test(x,y,method="spearman") # spearman cor test instead
  # borrowed from printCoefmat
  Signif=symnum(test$p.value,corr=FALSE,na=FALSE, 
                cutpoints=c(0,0.001,0.01,0.05,0.1,1),
                symbols=c("***","**","*","."," ")) 
  text(0.5,0.5,txt,cex=cex*r) 
  text(.8,.8,Signif,cex=cex,col=2) 
}

## some code options for running panel.cor
#z4=na.omit(z2)  # consider appropriate na.omit options; see help(na.omit)
#pairs(z4[4:11],lower.panel=panel.smooth,upper.panel=panel.cor)
#setwd(path.outgraph)
#savePlot(filename="corrplot_multfxn",type="tiff")
#### END MODIFIED panel.cor CORRELATION FUNCTION

