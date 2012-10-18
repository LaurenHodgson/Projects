library(SDMTools) 
library(dismo)

###define directories
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir = "/home/22/jc148322/Ecocrop/"


#base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
#base.asc[which(is.finite(base.asc))] = 0


library(rgdal); library(SDMTools) #load the library
wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd) #define and set the working directory
load("bil.indata.RData")#bil.data = readGDAL('hwsd.bil') #read in the data
soil = asc.from.sp(bil.data) #convert to an ascii grid format
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0


#01. get soil data for australia

pos.soil = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

pos.soil$long = getXYcoords(base.asc)$x[pos.soil$col] #row 
#pos.soil$long = pos.soil$long-360
pos.soil$lat = getXYcoords(base.asc)$y[pos.soil$row] #col

pts.soil = pos.soil[3:4]

pos.soil$soil.aus = extract.data(pts.soil, soil) #extracts the data from world soil data for only australia
setwd(esoclim.dir)
save(pos.soil, file=paste(out.dir, 'soil_aus2.rData', sep=''))
#load(file=paste(out.dir, 'soil_aus.rData', sep=''))
#02.merge databases

soil.data = read.csv(paste(out.dir,'soil/', 'hwsd_data.csv', sep='')) #read in soil data



soil.data$TPH = (soil.data$SHARE / 100) * soil.data$T_PH_H2O
soil.data$SPH = (soil.data$SHARE / 100) * soil.data$S_PH_H2O
tt = aggregate(soil.data[,c('SHARE','TPH','SPH')],by=list(soil.data$MU_GLOBAL),sum)
tdata = merge(pos.soil,tt,by.x='soil.aus',by.y='Group.1',all.x=TRUE)
tdata$PH = (tdata$TPH*.3) + (tdata $SPH*.7)
#save(tdata, file=paste(out.dir, 'soil_aus.PH.rData', sep=''))
soil.data$Tsalinity = (soil.data$SHARE / 100) * soil.data$T_ECE
soil.data$Ssalinity = (soil.data$SHARE / 100) * soil.data$S_ECE
tt = aggregate(soil.data[,c('Tsalinity','Ssalinity')],by=list(soil.data$MU_GLOBAL),sum)
tdata = merge(tdata,tt,by.x='soil.aus',by.y='Group.1',all.x=TRUE)
tdata$salinity = (tdata$Tsalinity*.3) + (tdata $Ssalinity*.7)
save(tdata, file=paste(out.dir, 'soil_aus.PH.salinity2.rData', sep=''))

