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


#02. define species constants
data(ECOcrops)

ECOcrops$SALMIN = as.character(ECOcrops$SAL)
ECOcrops$SALMAX = as.character(ECOcrops$SALR)

ECOcrops$SALMIN[which(ECOcrops$SALMIN=='H')] = 10; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='H')] = 100
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='M')] = 4; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='M')] = 9.999; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='m')] = 9.999; 
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='L')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='L')] = 3.999
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='l')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='l')] = 3.999

ECOcrops$SALMIN = as.numeric(ECOcrops$SALMIN)
ECOcrops$SALMAX = as.numeric(ECOcrops$SALMAX)




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


#03. create table of row, col, temp and rain for each run of each gcm
wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd)
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0


############################LOAD FILES
load(file=paste(out.dir, '2080temp_macadamia.rData', sep=''))
load(file=paste(out.dir, '2080rain_macadamia.rData', sep=''))
load(file=paste(out.dir, 'soil_aus.PH.salinity2.rData', sep=''))


#04.1.copy the data
copy.temp=pos.temp[3:32]

#04.2.determine 'weight' of each of the runs for each gcm
GCMs = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 
GCM.count=NULL
for (gcm in GCMs){
	GCM.count=c(GCM.count, length(grep(gcm, names(copy.temp))))
}

GCM.weight=1/GCM.count

#04.3.replace >0 with 1
for (ii in 1:ncol(copy.temp)){
	copy.temp[which(copy.temp[,ii] > 0),ii] = 1
}
#######temporary fixes for stupid mistake in function
copy.rain=pos.rain[3:32]
for (ii in 1:ncol(copy.rain)){
	copy.rain[which(copy.rain[,ii] > too.wet),ii] = 0
	copy.rain[which(copy.rain[,ii] < too.dry),ii] = 0
}


for (ii in 1:ncol(copy.rain)){
	copy.rain[which(copy.rain[,ii] > 0),ii] = 1
}
#######

#04.4.determine the weight of the runs of each model
for (ii in 1:ncol(copy.temp)){ #for each column
	#apply weights to column
	tgcm=NULL; 
	for (gcm in GCMs){
		if (length(grep(gcm, names(copy.temp)[ii]))>0) tgcm=gcm 
	}
	weighting=GCM.weight[which(GCMs==tgcm)] #find out the weighting for that GCM
	copy.temp[,ii]=copy.temp[,ii]*weighting #apply the weighting

}

for (ii in 1:ncol(copy.rain)){ #for each column
	#apply weights to column
	tgcm=NULL; 
	for (gcm in GCMs){
		if (length(grep(gcm, names(copy.rain)[ii]))>0) tgcm=gcm 
	}
	weighting=GCM.weight[which(GCMs==tgcm)] #find out the weighting for that GCM
	copy.rain[,ii]=copy.rain[,ii]*weighting #apply the weighting

}
#05.determine number of models that indicate suitability for rain and temp

copy.temp$total = rowSums(copy.temp[1:30])

temp.asc=base.asc
temp.asc[cbind(pos.temp$row, pos.temp$col)] = copy.temp$total

copy.rain$total = rowSums(copy.rain[1:30])

rain.asc=base.asc
rain.asc[cbind(pos.rain$row, pos.rain$col)] = copy.rain$total
#plot of number of runs of models that indicate suitability

####################################PH
ph.min=species.data$PHMIN
ph.max=species.data$PHMAX

pos.ph = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

pos.ph$PH = tdata$PH
copy.ph=pos.ph[3]

copy.ph[which(copy.ph[,1]<ph.min),1] = 0
copy.ph[which(copy.ph[,1]>ph.max),1] = 0
copy.ph2=copy.ph
copy.ph[which(copy.ph[,1]>0),1] = 1

ph.asc = base.asc
ph.asc[cbind(pos.ph$row, pos.ph$col)] = copy.ph2$PH


####################################SALINITY     
sal.min =species.data$SALMIN
sal.max = species.data$SALMAX

pos.sal = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

pos.sal$SAL = tdata$salinity
copy.sal=pos.sal[3]

copy.sal[which(copy.sal[,1]<sal.min),1] = 0
copy.sal[which(copy.sal[,1]>sal.max),1] = 0
copy.sal2=copy.sal
copy.sal[which(copy.sal[,1]>0),1] = 1

sal.asc = base.asc
sal.asc[cbind(pos.sal$row, pos.sal$col)] = copy.sal2$SAL


#COMBINE THE DATA ###################################
#06.combine the layers
pos.combine = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.combine$temp = copy.temp$total/2
pos.combine$rain = copy.rain$total/2
pos.combine$PH = copy.ph$PH
pos.combine$SAL = copy.sal$SAL

copy.combine=pos.combine[3:6]
for (ii in 1:ncol(copy.combine)){
	copy.combine[which(copy.combine[,ii]<0.05),ii] = NA
}

copy.combine$total = rowSums(copy.combine[1:ncol(copy.combine)])
copy.combine$total = copy.combine$total

combine.asc=base.asc
combine.asc[cbind(pos.combine$row, pos.combine$col)] = copy.combine$total

####################################07.make image
cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'black')
phcols=colorRampPalette(c('red','yellow','forestgreen','blue','purple'))(100)

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(out.dir, '/', species, '.', 'gcm.variability4.png',sep=''), width=24, height=14.5, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,3), mar=c(0,2,0,0),oma=c(0,0,6,0))

image(temp.asc, ann=FALSE,axes=FALSE, col=cols)
#text (130, -40, 'Temperature', cex=3)
legend.gradient(pnts,cols=cols, limits=c(0,8), title='Temperature', cex=2.2)

image(rain.asc, ann=FALSE,axes=FALSE, col=cols)
legend.gradient(pnts,cols=cols, limits=round(range(rain.asc,na.rm=TRUE)), title='Rain', cex=2.2)
#text (130, -40, 'Annual rainfall', cex=3)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(ph.asc, ann=FALSE,axes=FALSE, zlim=c(0,14), col=phcols, add=TRUE)
legend.gradient(pnts,cols=phcols, limits=c(0,14), title=paste('PH',' ','(',round(min(ph.asc,na.rm=TRUE),digits=2),'-',round(max(ph.asc,na.rm=TRUE),digits=2),')',sep=''), cex=2.2)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(sal.asc, ann=FALSE,axes=FALSE, col=phcols, add=TRUE)
legend.gradient(pnts,cols=phcols, limits=round(range(sal.asc,na.rm=TRUE)), title='Salinity', cex=2.2)

#text (130, -40, 'PH', cex=3)
image(base.asc,ann=FALSE,axes=FALSE, col='gray90');
image(combine.asc, ann=FALSE,axes=FALSE, col=cols, add=TRUE)
#text (130, -40, paste(species, ' ','Model', sep=''), cex=3)
legend.gradient(pnts, cols=cols,limits=c(0,round(max(combine.asc,na.rm=TRUE))), title='Combined', cex=2.2)

title(main=paste(species, ' ', 'Model',sep=''), outer=T,line=3,cex.main=3, font.main=2)
mtext('Agreement of runs of GCMs',side=3, line=1, outer=T,cex=1.8)
dev.off()
