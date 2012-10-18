###load the necessary libraries
library(SDMTools) 
library(dismo)

###define directories
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir = "/home/22/jc148322/Ecocrop/"
tmin.dir = "/home/22/jc148322/flatdata/tmin/";
tmax.dir = "/home/22/jc148322/flatdata/tmax/";
wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd)
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0


#01. get files of interest
setwd(esoclim.dir)
files = list.files()
files.of.interest=grep("2066",files, value=TRUE) #value=TRUE calls up full name of files
files.of.interest=grep("sresa1b",files.of.interest, value=TRUE)


pos.temp = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos.rain = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))



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


data(ECOcrops)

ECOcrops$SALMIN = as.character(ECOcrops$SAL)
ECOcrops$SALMAX = as.character(ECOcrops$SALR)

ECOcrops$SALMIN[which(ECOcrops$SALMIN=='H')] = 10; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='H')] = 100
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='M')] = 4; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='M')] = 9.999; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='m')] = 9.999; 
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='L')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='L')] = 3.999
ECOcrops$SALMIN[which(ECOcrops$SALMIN=='l')] = 0; ECOcrops$SALMAX[which(ECOcrops$SALMAX=='l')] = 3.999

ECOcrops$SALMIN = as.numeric(ECOcrops$SALMIN)
ECOcrops$SALMAX = as.numeric(ECOcrops$SALMAX)




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




setwd(tmin.dir)
species.dir = paste("/home/22/jc148322/Ecocrop/",  species, '/', sep=''); dir.create(species.dir)
files = list.files()

for (tfile in files) { cat(tfile,'\n')
tmindb=NULL
load(file=tfile)
tmindb[which(tmindb<too.cold | tmindb>too.hot)]=NA
tmindb[which(is.finite(tmindb))]=1
save(tmindb, file=paste(species.dir, tfile, sep=''))
}

setwd(tmax.dir)
files = list.files()

for (tfile in files) { cat(tfile,'\n')
tmindb=NULL
load(file=tfile)
tmaxdb[which(tmaxdb<too.cold | tmaxdb>too.hot)]=NA
tmaxdb[which(is.finite(tmaxdb))]=1
save(tmaxdb, file=paste(species.dir, tfile, sep=''))
}

#################################
setwd(species.dir)
files=list.files()
files.of.interest=grep('tmin', files, value=TRUE)


for (tfile in files.of.interest) { cat(tfile,'\n')
load(file=tfile)
tout=NULL
tout = tmindb; tout[,] = NA
for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+gmin)%%12)
	tout[,ii] = rowSums(tmindb[,cols.of.interest],na.rm=TRUE)
}

save(tout, file=paste(species.dir, 'gmin.', tfile, sep=''))
}


files=list.files()
files.of.interest=grep('tmax', files, value=TRUE)

for (tfile in files.of.interest) { cat(tfile,'\n')
load(file=tfile)
tout=NULL
tout = tmaxdb; tout[,] = NA
for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+gmin)%%12)
	tout[,ii] = rowSums(tmaxdb[,cols.of.interest],na.rm=TRUE)
}

save(tout, file=paste(species.dir, 'gmin.', tfile, sep=''))
}

####################################
files=list.files()
files.of.interest=grep('gmin',files, value=TRUE)

tout=NULL

for (tfile in files.of.interest) { cat(tfile,'\n')
load(tfile)
for (ii in 1:12) {
	tout[which(tout[,ii]<gmin),ii] = 0
	tout[which(tout[,ii]>=gmin),ii] = 1

}
save(tout, file=paste(species.dir, 'true.', tfile, sep=''))
}

####################################

pos.tmin = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.tmax = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

files=list.files()
files.of.interest=grep('true',files, value=TRUE)
tmin=grep('tmin',files.of.interest,value=TRUE)
tmax=grep('tmax',files.of.interest,value=TRUE)

for(tfile in tmin) {cat(tfile,'\n')
load(tfile)
tt=NULL
tt=strsplit(tfile,'\\.')[[1]][3:8]
tt=paste(tt[1],tt[2],tt[3],tt[4],tt[5],tt[6],sep='.')
pos.tmin[tt]=apply(tout,1,max)

}
save(pos.tmin, file=paste(species.dir,species,'.tmin.rData', sep=''))


for(tfile in tmax) {cat(tfile,'\n')
load(tfile)
tt=NULL
tt=strsplit(tfile,'\\.')[[1]][3:8]
tt=paste(tt[1],tt[2],tt[3],tt[4],tt[5],tt[6],sep='.')
pos.tmax[tt]=apply(tout,1,max)
}
save(pos.tmax, file=paste(species.dir,species,'.tmax.rData', sep=''))

######################################
copy.tmin=pos.tmin
copy.tmax=pos.tmax
pos.temp = copy.tmin
for (ii in 3:ncol(pos.temp)){cat(ii,'\n')
pos.temp[ii]= copy.tmin[ii]+ copy.tmax[ii]
}

for (ii in 3:ncol(pos.temp)){cat(ii,'\n')
pos.temp[which(pos.temp[,ii]<2),ii]=0
pos.temp[which(pos.temp[,ii]==2),ii]=1
}

save(pos.temp, file=paste(species.dir,species,'.temprange.rData', sep=''))


###########################
###########################making a trial image
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

copy.temp$total = rowSums(copy.temp[1:30])

temp.asc=base.asc
temp.asc[cbind(pos.temp$row, pos.temp$col)] = copy.temp$total

cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'black')

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(out.dir, '/', species, '.', 'season_range.temp.png',sep=''), width=8, height=7, units='cm', res=300, pointsize=5, bg='white')
par( mar=c(0,2,0,0))

image(temp.asc, ann=FALSE,axes=FALSE, col=cols)
#text (130, -40, 'Temperature', cex=3)
legend.gradient(pnts,cols=cols, limits=c(0,8), title='Temperature', cex=2.2)

dev.off()
