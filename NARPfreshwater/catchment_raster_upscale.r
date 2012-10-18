#Script to generate databases of climate data aggregated to river basins

library(SDMTools) #load the necessary library
install.packages('raster')
library(raster)

wd = '/home/jc165798/working/NARP_stability/OZ_5km/data'; setwd(wd) #define and set working directory
baseasc = read.asc.gz('base.asc.gz')
tasc=baseasc
pos = read.csv('base.positions.csv',as.is=TRUE)
pos$UID = 1:286244  #add unique identifier to each 5km grid cell
 
tasc[cbind(pos[,'row'],pos[,'col'])] =  pos[,'UID'] # append unique identifier to asc using pos dataframe with unique ID   -  seems to loose 44 points (max=286200)

wd = "/home/jc148322/Hydrology.trials/"; setwd(wd)
in.dir='/home/jc246980/Hydrology.trials/'

CatchmentRaster.asc = read.asc(paste(in.dir,'catchmentraster250m2.asc', sep='')) #read in the base ascii grid file at 250m resolution
pos250 = as.data.frame(which(is.finite(CatchmentRaster.asc),arr.ind=T))  #convert asci to position file

pos250$lat = getXYcoords(CatchmentRaster.asc)$y[pos250$col]      #extract and append lats and longs to position file
pos250$lon = getXYcoords(CatchmentRaster.asc)$x[pos250$row] 

pos250$REACHID = CatchmentRaster.asc[cbind(pos250$row,pos250$col)] #append ReachID (1:1474286)

cellareas.asc <- grid.area(CatchmentRaster.asc)[,]   #calculate areas of 250m grids

pos250$Area250 = cellareas.asc[cbind(pos250$row,pos250$col)] #append 250 area data

pos250$FiveKmUID  = extract.data(cbind(pos250$lon,pos250$lat), tasc)

# pos$soil_ID = extract.data(cbind(pos$lon,pos$lat),soil.asc) 	#extract the soil id
# tt = baseasc; tt[cbind(pos$row,pos$col)] = pos$soil_ID 	#get the soil id

FiveKmUID=CatchmentRaster.asc; FiveKmUID[cbind(pos250$row,pos250$col)]=pos250$FiveKmUID


zlim=range(FiveKmUID,na.rm=T)
png(paste(wd, 'test.png',sep=''),width=dim(FiveKmUID)[1]+30, height=dim(FiveKmUID)[2]*1+60, units='px', pointsize=20, bg='lightgrey')
image(FiveKmUID, zlim=zlim, ann=FALSE,axes=FALSE)
dev.off()


