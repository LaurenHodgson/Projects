species='Plantain bananas'
yr='2080'

###load the necessary libraries
library(SDMTools) 
library(dismo)

###define directories####################################
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir ="/home/22/jc148322/Ecocrop_output/"
tmin.dir = paste("/home/22/jc148322/flatdata/tmin/", yr,'/',sep='');
tmax.dir = paste("/home/22/jc148322/flatdata/tmax/", yr,'/',sep='');
pr.dir = "/home/22/jc148322/flatdata/pr/"
soil.dir = "/home/22/jc148322/flatdata/soil/"
script.dir="/home/22/jc148322/flatdata/scripts/"
ecocrop.dir = "/home/22/jc148322/Ecocrop_output/"
setwd(ecocrop.dir)
files=list.files()
files=grep(species, files ,value=TRUE)
species.dir = paste("/home/22/jc148322/Ecocrop_output/",  species, '/', sep='')
if (length(files)==0){
 dir.create(species.dir)}
species.dir = paste(species.dir, yr, '/', sep='');dir.create(species.dir)

wd = '/home/22/jc148322/flatdata/'; setwd(wd)

###base#################################################

base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0
pos= as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
###turn ecocrop character to values######################

data(ECOcrops)

ECOcrops$SALMIN = as.character(ECOcrops$SAL)
ECOcrops$SALMAX = as.character(ECOcrops$SALR)

ECOcrops$SALMIN[which(ECOcrops$SALMIN=='H')] = 10; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='H')] = 100
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='M')] = 4; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='M')] = 9.999; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='m')] = 9.999; 
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='L')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='L')] = 3.999
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='l')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='l')] = 3.999

ECOcrops$SALMIN = as.numeric(ECOcrops$SALMIN)
ECOcrops$SALMAX = as.numeric(ECOcrops$SALMAX)


###define 'constants'####################################

rowname = which(ECOcrops$NAME==species)
rowname
#rowname
#[1]  723 1552  #if two rownames, need to define
#rowname = 723

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
if (gmin>11.5) {gmin=12}

###create pos.temp#######################################

#1.1.determine cells of dataframe of 12 months within suitable Tmin
setwd(tmin.dir)
files = list.files()

for (tfile in files) { cat(tfile,'\n')
tmindb=NULL
load(file=tfile)
tmindb[which(tmindb<too.cold | tmindb>too.hot)]=NA
tmindb[which(is.finite(tmindb))]=1
save(tmindb, file=paste(species.dir, tfile, sep=''))
}

#1.2.determine cells of dataframe within suitable Tmax
setwd(tmax.dir)
files = list.files()

for (tfile in files) { cat(tfile,'\n')
tmaxdb=NULL
load(file=tfile)
tmaxdb[which(tmaxdb<too.cold | tmaxdb>too.hot)]=NA
tmaxdb[which(is.finite(tmaxdb))]=1
save(tmaxdb, file=paste(species.dir, tfile, sep=''))
}


setwd(species.dir)
if (round(gmin)==12) {source(paste(script.dir,'twelve_months.r', sep=''))} 
if (gmin<11.5) {source(paste(script.dir,'lessthan_twelve.r', sep=''))}


#6.determine GCM run weight (will be used for rain also)
copy.temp=pos.temp[3:32]
GCMs = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 
GCM.count=NULL
for (gcm in GCMs){
	GCM.count=c(GCM.count, length(grep(gcm, names(copy.temp))))
}

GCM.weight=1/GCM.count

#6.1.determine the weight of the runs of each model
for (ii in 1:ncol(copy.temp)){ #for each column
	#apply weights to column
	tgcm=NULL; 
	for (gcm in GCMs){
		if (length(grep(gcm, names(copy.temp)[ii]))>0) tgcm=gcm 
	}
	weighting=GCM.weight[which(GCMs==tgcm)] #find out the weighting for that GCM
	copy.temp[,ii]=copy.temp[,ii]*weighting #apply the weighting

}
copy.temp$total = rowSums(copy.temp[1:30])

pos.temp$total.temp = copy.temp$total
pos.temp = pos.temp[,c('row','col','total.temp')]

temp.asc=base.asc
temp.asc[cbind(pos.temp$row, pos.temp$col)] = pos.temp$total
write.asc.gz(temp.asc, paste(species.dir, species, '.temp.asc', sep=''))
save(pos.temp, file=paste(species.dir, species,'.temp.rData',sep=''))

###rain#######################################################
load(file=paste(pr.dir,'sresa1b.2066.2095.pr.rData',sep=''))
copy.rain=pos.rain[3:ncol(pos.rain)]
#1.cell suitability
for (ii in 1:ncol(copy.rain)){
	copy.rain[which(copy.rain[,ii] > too.wet),ii] = 0
	copy.rain[which(copy.rain[,ii] < too.dry),ii] = 0
}

for (ii in 1:ncol(copy.rain)){
	copy.rain[which(copy.rain[,ii] > 0),ii] = 1
}

#2.weight of runs of gcm
for (ii in 1:ncol(copy.rain)){ #for each column
	#apply weights to column
	tgcm=NULL; 
	for (gcm in GCMs){
		if (length(grep(gcm, names(copy.rain)[ii]))>0) tgcm=gcm 
	}
	weighting=GCM.weight[which(GCMs==tgcm)] #find out the weighting for that GCM
	copy.rain[,ii]=copy.rain[,ii]*weighting #apply the weighting

}

save(copy.rain, file=paste(species.dir, species, '.pr', '.rData', sep=''))

copy.rain$total = rowSums(copy.rain[1:30])
pos.rain$total.rain = copy.rain$total
pos.rain = pos.rain[,c('row','col','total.rain')]

rain.asc=base.asc
rain.asc[cbind(pos.rain$row, pos.rain$col)] = pos.rain$total
write.asc.gz(rain.asc, paste(species.dir, species, '.rain.asc', sep=''))
save(pos.rain, file=paste(species.dir, species,'.rain.rData',sep=''))

###ph#####################################################

load(file=paste(soil.dir,'soil.ph.rData',sep=''))

copy.ph=pos.ph[3]

copy.ph[which(copy.ph[,1]<ph.min),1] = 0
copy.ph[which(copy.ph[,1]>ph.max),1] = 0
copy.ph2=copy.ph
copy.ph[which(copy.ph[,1]>0),1] = 1

pos.ph$total.ph = copy.ph$PH
pos.ph = pos.ph[,c('row','col','total.ph')]

ph.asc = base.asc
ph.asc[cbind(pos.ph$row, pos.ph$col)] = copy.ph2$PH
write.asc.gz(ph.asc, paste(species.dir, species, '.ph.asc', sep=''))
save(pos.ph, file=paste(species.dir, species,'.ph.rData',sep=''))

###salinity##############################################

load(file=paste(soil.dir,'soil.sal.rData',sep=''))

copy.sal=pos.sal[6]

copy.sal[which(copy.sal[,1]<sal.min),1] = 0
copy.sal[which(copy.sal[,1]>sal.max),1] = 0
copy.sal2=copy.sal
copy.sal[which(copy.sal[,1]>0),1] = 1

pos.sal$total.sal = copy.sal$sal
pos.sal = pos.sal[,c('row','col','total.sal')]

sal.asc = base.asc
sal.asc[cbind(pos.sal$row, pos.sal$col)] = copy.sal2$sal
write.asc.gz(sal.asc,paste(species.dir, species, '.salinity.asc', sep=''))
save(pos.sal, file=paste(species.dir, species,'.sal.rData',sep=''))

###combine###############################################

pos.combine = merge(pos.sal,pos.temp,all=TRUE)
pos.combine = merge(pos.combine,pos.rain,all=TRUE)
pos.combine = merge(pos.combine,pos.ph,all=TRUE)
pos.combine$total.temp=pos.combine$total.temp/2
pos.combine$total.rain=pos.combine$total.rain/2

copy.combine=pos.combine[3:ncol(pos.combine)]
for (ii in 1:ncol(copy.combine)){
	copy.combine[which(copy.combine[,ii]==0),ii] = NA
}

copy.combine$sum = rowSums(copy.combine[,c('total.sal','total.temp','total.rain','total.ph')])
combine.asc = base.asc; combine.asc[cbind(pos.combine$row,pos.combine$col)] = copy.combine$sum

write.asc.gz(combine.asc, paste(species.dir, species, '.combine.asc', sep=''))
save(copy.combine, file=paste(species.dir,'combine.rData',sep=''))

#image############################################

cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'black')
phcols=c('gray90',colorRampPalette(c('red','yellow','forestgreen','blue','purple'))(100))
salcols=c('gray90',colorRampPalette(c('moccasin','lightpink','lightpink3','lightpink4'))(100))

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(species.dir, '/', species, '.', yr, '.png',sep=''), width=21, height=14.5, units='cm', res=300, pointsize=5, bg='white')
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

title(main=paste(species, ' ', 'Model', ' ', yr, sep=''), outer=T,line=4,cex.main=3, font.main=2)
mtext('Agreement of runs of GCMs',side=3, line=1, outer=T,cex=1.8)
dev.off()

