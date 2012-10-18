###hpc:qsub -l nodes=1:ppn=8 -I
###hpc:qsub -l nodes=1:ppn=2:V20Z -I

###load the necessary libraries
library(SDMTools) 
library(dismo)

###define directories
esoclim.dir = '/home/22/jc148322/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir = "/home/22/jc148322/Ecocrop/"


#01. get files of interest
setwd(esoclim.dir)
files = list.files()
files.of.interest=grep("current",files, value=TRUE)


#02. define species constants
data(ECOcrops)
species='Macademia nut'
rowname = which(ECOcrops$NAME==species)
#rowname
#[1]  723 1552
rowname = 723

species.data= ECOcrops[rowname, ]
too.hot = species.data$TMAX
too.cold = species.data$TMIN
too.wet = species.data$RMAX
too.dry = species.data$RMIN


#03. write a function that determines optimum growth area
temp = function(files.of.interest) {
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
setwd(sub.dir)
just.right = NULL #define the basic output
#cycle through the months and drop out too hot, too cold, too wet, too dry
for (ii in 1:12) { cat(ii,'\n')
	tmax = read.asc.gz(paste('tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) #read in tmax 
	tmin = read.asc.gz(paste('tasmin',sprintf('%02i',ii),'.asc.gz',sep='')) #read in tmin 
	
	just.right = tmax
	just.right[which(tmax>too.hot)] = 0
	just.right[which(tmin<too.cold)] = 0
	
}
return (just.right)
}

rain = function(files.of.interest) {
sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
setwd(sub.dir)
enough.rain = NULL
sum_pr = NULL
for (ii in 1:12) { cat(ii,'\n')
	precip = read.asc.gz(paste('pr',sprintf('%02i',ii),'.asc.gz',sep=''))
	if (length(sum_pr) == 0) { #if there are no values , populate it
          sum_pr = precip
        } else { #if already has values, add the new values to it
          sum_pr = sum_pr + precip
		 } 
	
}
enough.rain = sum_pr
	enough.rain[which(sum_pr>too.wet)] = 0 #this doesn't work
	enough.rain[which(sum_pr<too.dry)] = 0 #this doesn't work... maybe not a bad thing.
	
return (sum_pr)
}

#03. create table of row, col, temp and rain for each run of each gcm
base.asc = read.asc.gz(paste(esoclim.dir, "tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0

pos.temp = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos.rain = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

#04. create tables of yes/no binary suitability data
for (ttemp in files.of.interest) { cat(ttemp,'\n')
	#cycle through run of gcm to calculate weight
	tdata=temp(ttemp)
	
	#add column to pos	
	pos.temp[ttemp] = tdata[cbind(pos.temp$row,pos.temp$col)]
}

save(pos.temp, file=paste(out.dir, 'current.temp_macadamia.rData', sep=''))

for (train in files.of.interest) { cat(train,'\n')
	#cycle through run of gcm to calculate weight
	rdata=rain(train)
	
	#add column to pos	
	pos.rain[train] = rdata[cbind(pos.rain$row,pos.rain$col)]
}
save(pos.rain, file=paste(out.dir, 'current.rain_macadamia.rData', sep=''))
#load(file=paste(out.dir, '2080rain_sugarcane2.rData', sep=''))
#04.1.copy the data
copy.temp=pos.temp[3]

#04.2.determine 'weight' of each of the runs for each gcm


#04.3.replace >0 with 1

copy.temp[which(copy.temp[,1] > 0),1] = 1

#######temporary fixes for stupid mistake in function
copy.rain=pos.rain[3]
copy.rain[which(copy.rain[,1] > too.wet),1] = 0
copy.rain[which(copy.rain[,1] < too.dry),1] = 0

copy.rain[which(copy.rain[,1] > 0),1] = 1

#######

#05.determine number of models that indicate suitability for rain and temp


temp.asc=base.asc
temp.asc[cbind(pos.temp$row, pos.temp$col)] = copy.temp$current.esoclim

rain.asc=base.asc
rain.asc[cbind(pos.rain$row, pos.rain$col)] = copy.rain$current.esoclim
#plot of number of runs of models that indicate suitability

####################################trial
load(file=paste(out.dir, '/', 'soil_aus.PH.salinity.rData', sep=''))

ph.min=species.data$PHMIN
ph.max=species.data$PHMAX

pos.ph = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

pos.ph$PH = tdata$PH
copy.ph=pos.ph[3]

copy.ph[which(copy.ph[,1]<ph.min),1] = 0
copy.ph[which(copy.ph[,1]>ph.max),1] = 0
copy2.ph = copy.ph
copy.ph[which(copy.ph[,1]>0),1] = 1

ph.asc = base.asc
ph.asc[cbind(pos.ph$row, pos.ph$col)] = copy2.ph$PH ####!!!! NEED TO CHANGE THIS BACK TO copy.ph range once shown jeremy the data.
####################################

#06.combine the layers
pos.combine = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.combine$temp = copy.temp$current.esoclim
pos.combine$rain = copy.rain$current.esoclim
pos.combine$PH = copy.ph$PH

copy.combine=pos.combine[3:5]
for (ii in 1:ncol(copy.combine)){
	copy.combine[which(copy.combine[,ii]<0.5),ii] = NA
}

copy.combine$total = rowSums(copy.combine[1:3])
copy.combine$total = copy.combine$total

combine.asc=base.asc
combine.asc[cbind(pos.combine$row, pos.combine$col)] = copy.combine$total

#07.make image
cols= c('gray90','black')
phcols=colorRampPalette(c('red','yellow','forestgreen','blue','purple'))(100)

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(out.dir, '/', species, '.', 'current.png',sep=''), width=16, height=14.5, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,2), mar=c(0,2,0,0),oma=c(0,0,6,0))
image(temp.asc, ann=FALSE,axes=FALSE, col=cols)
#text (130, -40, 'Temperature', cex=3)
legend.gradient(pnts,cols=cols, limits=round(range(temp.asc,na.rm=TRUE)), title='Temperature', cex=2.2)
image(rain.asc, ann=FALSE,axes=FALSE, col=cols)
legend.gradient(pnts,cols=cols, limits=round(range(rain.asc,na.rm=TRUE)), title='Rain', cex=2.2)
#text (130, -40, 'Annual rainfall', cex=3)
image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(ph.asc, ann=FALSE,axes=FALSE, zlim=c(0.1,14), col=phcols, add=TRUE)
legend.gradient(pnts,cols=phcols, limits=c(0,14), title=paste('PH',' ','(',round(min(ph.asc,na.rm=TRUE),digits=2),'-',round(max(ph.asc,na.rm=TRUE),digits=2),')',sep=''), cex=2.2)
#text (130, -40, 'PH', cex=3)
image(base.asc,ann=FALSE,axes=FALSE, col='gray90');
image(combine.asc, ann=FALSE,axes=FALSE, col='black', add=TRUE)
#text (130, -40, paste(species, ' ','Model', sep=''), cex=3)
legend.gradient(pnts, cols=cols,limits=c(0,round(max(combine.asc,na.rm=TRUE))), title='Combined', cex=2.2)
title(main=paste(species, ' ', 'Model - Current',sep=''), outer=T,line=3,cex.main=3, font.main=2)
#mtext('Agreement of runs of GCMs',side=3, line=1, outer=T,cex=1.8)
dev.off()

---



