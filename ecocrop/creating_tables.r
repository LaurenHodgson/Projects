###hpc:qsub -l nodes=1:ppn=8 -I
###hpc:qsub -l nodes=1:ppn=2:V20Z -I

###load the necessary libraries
library(SDMTools) 
library(dismo)

###define directories
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir = "/home/22/jc148322/Ecocrop/"

setwd(esoclim.dir)

#01. get files of interest
setwd(esoclim.dir)
files = list.files()
files.of.interest=grep("2066",files, value=TRUE) #value=TRUE calls up full name of files
files.of.interest=grep("sresa1b",files.of.interest, value=TRUE)

base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0

pos.max = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos.min = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.rain = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))


for (ttemp in files.of.interest) { cat(ttemp,'\n')
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
setwd(sub.dir)
min.temp.files = grep('tasmin',list.files(), value=TRUE)

for (ii in min.temp.files) {cat(ii,'\n')
	#cycle through run of gcm to calculate weight
	tmin = read.asc.gz(paste(sub.dir,ii, sep=''))
	
	#add column to pos	
	pos.min[ii] = tmin[cbind(pos.min$row,pos.min$col)]
	colnames(pos.min)[3:ncol(pos.min)] =  paste(ttemp, '.',ii, sep='')
}	
}

save(pos.min, file=paste(out.dir, '2080temp_min.rData', sep=''))






#03. write a function that determines optimum growth area
maxtemp = function(files.of.interest) {
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
setwd(sub.dir)
#cycle through the months and drop out too hot, too cold, too wet, too dry
for (ii in 1:12) { cat(ii,'\n')
	tmax = read.asc.gz(paste(sub.dir,'tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) #read in tmax 	
}
return (tmax)
}

mintemp = function(files.of.interest) {
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
setwd(sub.dir)
#cycle through the months and drop out too hot, too cold, too wet, too dry
for (ii in 1:12) { cat(ii,'\n')
	tmin = read.asc.gz(paste(sub.dir,'tasmin',sprintf('%02i',ii),'.asc.gz',sep='')) #read in tmin 		
}
return (tmin)
}




rain = function(files.of.interest) {
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')

for (ii in 1:12) { cat(ii,'\n')
	precip = read.asc.gz(paste(sub.dir,'pr',sprintf('%02i',ii),'.asc.gz',sep=''))	
}
return (precip)
}

#03. create table of row, col, temp and rain for each run of each gcm
base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0

pos.max = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos.min = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.rain = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

#04. create tables of yes/no binary suitability data
for (ttemp in files.of.interest) { cat(ttemp,'\n')
	#cycle through run of gcm to calculate weight
	maxdata=maxtemp(ttemp)
	
	#add column to pos	
	pos.max[ttemp] = maxdata[cbind(pos.max$row,pos.max$col)]
}

save(pos.max, file=paste(out.dir, '2080temp_max.rData', sep=''))

for (ttemp in files.of.interest) { cat(ttemp,'\n')
for (ii in 1:12) {cat(ii,'\n')
	#cycle through run of gcm to calculate weight
	mindata=mintemp(ttemp)
	
	#add column to pos	
	pos.min[ttemp] = mindata[cbind(pos.min$row,pos.min$col)]
}	
}

save(pos.min, file=paste(out.dir, '2080temp_min.rData', sep=''))


for (train in files.of.interest) { cat(train,'\n')
	#cycle through run of gcm to calculate weight
	rdata=rain(train)
	
	#add column to pos	
	pos.rain[train] = rdata[cbind(pos.rain$row,pos.rain$col)]
}
save(pos.rain, file=paste(out.dir, '2080rain.rData', sep=''))
