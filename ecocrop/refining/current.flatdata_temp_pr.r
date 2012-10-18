library(SDMTools) 
library(dismo)

yr="current"

###define directories
esoclim.dir = '/home/22/jc148322/current.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
current.dir = paste("/home/22/jc148322/flatdata/", yr,'/',sep='');
wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd)
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0
pos=as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

#01. get files of interest
setwd(esoclim.dir)

###temperature##########################
pos.temp = pos#put all rows and cols for finite values as dataframe

tmindb= matrix(NA, nr = nrow(pos.temp), nc=12) #create empty matrix for 12 months of tmin for each run of each GCM
tmaxdb = matrix(NA, nr = nrow(pos.temp), nc=12)

for (ii in 1:12) { cat(ii,'\n')
	tmindb[,ii]=read.asc.gz(paste('tasmin',sprintf('%02i',ii),'.asc.gz',sep='')) [cbind(pos.temp$row,pos.temp$col)]
	tmaxdb[,ii]=read.asc.gz(paste('tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) [cbind(pos.temp$row,pos.temp$col)]
}

save(tmindb, file=paste(current.dir,  yr, '.', 'tmin', '.rData', sep=''))
save(tmaxdb, file=paste(current.dir,  yr, '.', 'tmax', '.rData', sep=''))


###rain###################################
pos.rain = pos


sum_pr = NULL
for (ii in 1:12) { cat(ii,'\n')
	precip = read.asc.gz(paste('pr',sprintf('%02i',ii),'.asc.gz',sep=''))
	if (length(sum_pr) == 0) { #if there are no values , populate it
          sum_pr = precip
        } else { #if already has values, add the new values to it
          sum_pr = sum_pr + precip
		 } 

}

#add column to pos	
pos.rain$current.rain = sum_pr[cbind(pos.rain$row,pos.rain$col)]


save(pos.rain, file=paste(current.dir,  yr, '.', 'pr', '.rData', sep=''))
