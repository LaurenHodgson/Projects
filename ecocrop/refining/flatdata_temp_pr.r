library(SDMTools) 
library(dismo)

yr="2080"
scenario="sresa1b"

###define directories
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
tmin.dir = paste("/home/22/jc148322/flatdata/tmin/", yr,'/',sep='');
tmax.dir = paste("/home/22/jc148322/flatdata/tmax/", yr,'/',sep='');
wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd)
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0


#01. get files of interest
yr=as.numeric(yr);yr=yr-14
setwd(esoclim.dir)
files = list.files()
files.of.interest=grep(yr,files, value=TRUE) #value=TRUE calls up full name of files
files.of.interest=grep(scenario,files.of.interest, value=TRUE)

###temperature##########################
pos.temp = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe

for (tfile in files.of.interest) { cat(tfile,'\n')
sub.dir = paste(esoclim.dir, tfile, '/',sep='')
setwd(sub.dir)
tmindb= matrix(NA, nr = nrow(pos.temp), nc=12) #create empty matrix for 12 months of tmin for each run of each GCM
tmaxdb = matrix(NA, nr = nrow(pos.temp), nc=12)

for (ii in 1:12) { cat(ii,'\n')
	tmindb[,ii]=read.asc.gz(paste(sub.dir,'tasmin',sprintf('%02i',ii),'.asc.gz',sep='')) [cbind(pos.temp$row,pos.temp$col)]
	tmaxdb[,ii]=read.asc.gz(paste(sub.dir,'tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) [cbind(pos.temp$row,pos.temp$col)]
}

save(tmindb, file=paste(tmin.dir,  tfile, '.', 'tmin', '.rData', sep=''))
save(tmaxdb, file=paste(tmax.dir,  tfile, '.', 'tmax', '.rData', sep=''))
}

###rain###################################
pos.rain = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

rain = function(files.of.interest) {
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
enough.rain = NULL
sum_pr = NULL
for (ii in 1:12) { cat(ii,'\n')
	precip = read.asc.gz(paste(sub.dir,'pr',sprintf('%02i',ii),'.asc.gz',sep=''))
	if (length(sum_pr) == 0) { #if there are no values , populate it
          sum_pr = precip
        } else { #if already has values, add the new values to it
          sum_pr = sum_pr + precip
		 } 
	
}
	
return (sum_pr)
}

for (train in files.of.interest) { cat(train,'\n')
	#cycle through run of gcm to calculate weight
	rdata=rain(train)
	
	#add column to pos	
	pos.rain[train] = rdata[cbind(pos.rain$row,pos.rain$col)]
}
tt=strsplit(train,'\\.')[[1]][1:6]
tt=paste(tt[1],tt[5],tt[6],sep='.')
save(pos.rain, file=paste(pr.dir,  tt, '.', 'pr', '.rData', sep=''))
