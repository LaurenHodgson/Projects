library(SDMTools) 
library(dismo)

###################################Data

library(SDMTools) 
library(dismo)

###################################Data

data(ECOcrops)

ECOcrops$SALMIN = as.character(ECOcrops$SAL)
ECOcrops$SALMAX = as.character(ECOcrops$SALR)

ECOcrops$SALMIN[which(ECOcrops$SALMIN=='H')] = 10; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='H')] = 100
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='M')] = 4; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='M')] = 9.999; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='m')] = 9.999; 
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='L')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='L')] = 3.999
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='l')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='l')] = 3.999

ECOcrops$SALMIN = as.numeric(ECOcrops$SALMIN)
ECOcrops$SALMAX = as.numeric(ECOcrops$SALMAX)


######################################Constants

species='Sugarcane'
rowname = which(ECOcrops$NAME==species)
rowname
#rowname
#[1]  723 1552
rowname = 1639

species.data= ECOcrops[rowname, ]
too.hot = species.data$TMAX
too.cold = species.data$TMIN
too.wet = species.data$RMAX
too.dry = species.data$RMIN
gmin =species.data$GMIN/30.41667
gmax = species.data$GMAX/30.41667
ph.min=species.data$PHMIN
ph.max=species.data$PHMAX
sal.min =species.data$SALMIN
sal.max = species.data$SALMAX

#######################################Directories

soil.dir = '/home/22/jc148322/flatdata/soil/'
species.dir = paste("/home/22/jc148322/Ecocrop/",  species, '/', sep='')

wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd)



########################################Files and objects
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0
phasc = read.asc.gz(paste(soil.dir, 'soil.ph.asc.gz', sep=''))
load(file=paste(soil.dir,'soil.sal.rData',sep=''))
load(file=paste(species.dir,'sresa1b.2066.2095.pr.rData',sep=''))
load(file=paste(species.dir,'Sugarcane.temprange2.rData',sep=''))
load(file=paste(species.dir,'Sugarcane.temprange.rData',sep=''))

########################################PH

pos.ph = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.ph$PH = phasc[cbind(pos.ph$row,pos.ph$col)] #hope this works
save(pos.ph, file=paste(soil.dir,'soil.ph.rData',sep=''))

copy.ph=pos.ph[3]

copy.ph[which(copy.ph[,1]<ph.min),1] = 0
copy.ph[which(copy.ph[,1]>ph.max),1] = 0
copy.ph2=copy.ph
copy.ph[which(copy.ph[,1]>0),1] = 1

ph.asc = base.asc
ph.asc[cbind(pos.ph$row, pos.ph$col)] = copy.ph2$PH
#write.asc.gz(ph.asc, paste(species.dir, species, '.ph.asc', sep=''))


####################################SALINITY     

copy.sal=pos.sal[6]

copy.sal[which(copy.sal[,1]<sal.min),1] = 0
copy.sal[which(copy.sal[,1]>sal.max),1] = 0
copy.sal2=copy.sal
copy.sal[which(copy.sal[,1]>0),1] = 1

sal.asc = base.asc
sal.asc[cbind(pos.sal$row, pos.sal$col)] = copy.sal2$sal
write.asc.gz(sal.asc,paste(species.dir, species, '.salinity.asc', sep=''))

#####################################RAIN
pos.rain = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

copy.rain$total = rowSums(copy.rain[1:30])

rain.asc=base.asc
rain.asc[cbind(pos.rain$row, pos.rain$col)] = copy.rain$total
#write.asc.gz(rain.asc, paste(species.dir, species, '.rain.asc', sep=''))

######################################TEMP

temp.asc=base.asc
temp.asc[cbind(pos.temp$row, pos.temp$col)] = copy.temp$total
#write.asc.gz(temp.asc, paste(species.dir, species, '.temp.asc', sep=''))
#save(copy.temp, file=paste(species.dir,'Sugarcane.temprange2.rData',sep=''))

#COMBINE THE DATA ###################################
#06.combine the layers
pos.rain$total.rain = copy.rain$total
pos.temp$total.temp = copy.temp$total
pos.temp = pos.temp[,c('row','col','total.temp')]
pos.sal$total.sal = copy.sal$sal
pos.sal = pos.sal[,c('row','col','total.sal')]

tt = merge(pos.sal,pos.temp,all=TRUE)
tt = merge(tt,pos.rain,all=TRUE)

tt$sum = rowSums(tt[,c('total.sal','total.temp','total.rain')])
tasc = base.asc; tasc[cbind(tt$row,tt$col)] = tt$sum
png()
image(tasc)
dev.off()

#############Test^jjv

pos.rain$total.rain = copy.rain$total
pos.temp$total.temp = copy.temp$total
pos.temp = pos.temp[,c('row','col','total.temp')]
pos.sal$total.sal = copy.sal$sal
pos.sal = pos.sal[,c('row','col','total.sal')]
pos.ph$total.ph = copy.ph$PH
pos.ph = pos.ph[,c('row','col','total.sal')]


pos.combine = merge(pos.sal,pos.temp,all=TRUE)
pos.combine = merge(pos.combine,pos.rain,all=TRUE)
pos.combine = merge(pos.combine,pos.ph,all=TRUE)

copy.combine=pos.combine[3:ncol(pos.combine)]
for (ii in 1:ncol(copy.combine)){
	copy.combine[which(copy.combine[,ii]==0),ii] = NA
}

copy.combine$sum = rowSums(copy.combine[,c('total.sal','total.temp','total.rain','total.ph')])
combine.asc = base.asc; combine.asc[cbind(pos.combine$row,pos.combine$col)] = copy.combine$sum

#write.asc.gz(combine.asc, paste(species.dir, species, '.combine.asc', sep=''))
#save(copy.combine, file=paste(species.dir,'combine.rData',sep=''))
####################################07.make image
cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'black')
phcols=colorRampPalette(c('red','yellow','forestgreen','blue','purple'))(100)
salcols=colorRampPalette(c('moccasin','lightpink','lightpink3','lightpink4'))(100)

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(species.dir, '/', species, '.', '2080.limitstroubleshoot.png',sep=''), width=24, height=14.5, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,3), mar=c(1,2,1,1),oma=c(1,1,7,1))

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
image(sal.asc, ann=FALSE,axes=FALSE, col=salcols, add=TRUE)
legend.gradient(pnts,cols=salcols, limits=round(range(sal.asc,na.rm=TRUE)), title='Salinity', cex=2.2)

#text (130, -40, 'PH', cex=3)
image(base.asc,ann=FALSE,axes=FALSE, col='gray90');
image(combine.asc, ann=FALSE,axes=FALSE, col=cols, add=TRUE)
#text (130, -40, paste(species, ' ','Model', sep=''), cex=3)
legend.gradient(pnts, cols=cols,limits=c(0,round(max(combine.asc,na.rm=TRUE))), title='Combined', cex=2.2)

title(main=paste(species, ' ', 'Model',sep=''), outer=T,line=4,cex.main=3, font.main=2)
mtext('Agreement of runs of GCMs',side=3, line=1, outer=T,cex=1.8)
dev.off()
