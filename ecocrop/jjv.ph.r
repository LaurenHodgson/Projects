library(rgdal); library(SDMTools) 	#load the library
wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd) 	#define and set the working directory
load("bil.indata.RData")	#bil.data = readGDAL('hwsd.bil') #read in the data
soil.asc = asc.from.sp(bil.data) 	#convert to an ascii grid format
baseasc = read.asc('base.asc') 	#read in the asc file
pos = read.csv('base.pos.csv',as.is=TRUE) 	#read in the base positions
pos$soil_ID = extract.data(cbind(pos$lon,pos$lat),soil.asc) 	#extract the soil id
tt = baseasc; tt[cbind(pos$row,pos$col)] = pos$soil_ID 	#get the soil id
png(); image(tt); dev.off() 	#quickly map the soil ID values

soildata = read.csv('hwsd_data.csv',as.is=TRUE) 	#read in teh soil data
soildata = soildata[,c("MU_GLOBAL","SHARE","T_PH_H2O","S_PH_H2O")] 	#lets just play with ph
#start aggregating the soil data
row.agg = function(x) { 	#function to aggregate the necessary data 
	if (is.finite(sum(x))) { 	#no missing data
		return( sum(x*c(0.3,0.7)) ) 	#30/70 split of top and bottom soil
	} else if (is.finite(x[1])) { 	#ph of top soil
		return( x[1] )
	} else if (is.finite(x[2])) { 	#ph of bottom soil
		return( x[2] )
	} else { return( NA ) }
}
soildata$ph = apply(soildata[,c("T_PH_H2O","S_PH_H2O")],1,row.agg) 	#get the top and bottom proportions
soildata = soildata[which(is.finite(soildata$ph)),]  	#get rid of NA data
soildata = soildata[,-which(names(soildata) %in% c("T_PH_H2O","S_PH_H2O"))] 	#remove extra columns
soildata$ph = (soildata$SHARE / 100) * soildata$ph 	#make the ph a proportion of the 'share'
soils = aggregate(soildata,by=list(soil_ID=soildata$MU_GLOBAL),sum) 	#get the sum of the data
soils$ph = soils$ph / (soils$SHARE / 100) 	#correct for incorrect addition of shares
soils = soils[,c('soil_ID','ph')] 	#keep only columns of interest

pos = merge(pos,soils,all.x=TRUE) 	#merge the soils into the pos object
for (ii in which(is.na(pos$ph))) { cat('.')	#cycle through and replace missing ph data with the average of surrounding cells
	x = pos$col[ii]; y = pos$row[ii] 	#get the row/col
	buff = 0 	#set the defaule 
	tval= NULL	#object to hold ph values
	while (is.null(tval)) {	#continue looping through this until we have at least 5 ph values for the average
		buff = buff+2	#add 2 pixel buffer distances
		for (xx in (-buff:buff)+x) {	#cycle through the possible x values
			for (yy in (-buff:buff)+y) {	#cycle throught eh possible y values
				tt = which(pos$row==yy & pos$col==xx)	#check to see if the xy combinationis a valid location
				if (length(tt) > 0) tval = c(tval,pos$ph[tt])	#if valid location, extract the ph data
			}
		}
		if (length(na.omit(tval))<5) tval = NULL	#if not at least 5 finite ph values, set to null and start loop again with larger buffer
	}
	pos$ph[ii] = mean(tval,na.rm=TRUE)
}

tt = baseasc; tt[cbind(pos$row,pos$col)] = pos$ph #get the soil id
png('ph.png'); image(tt); dev.off() #quickly map the soil ID values
write.asc.gz(tt,'soil.ph.asc')

