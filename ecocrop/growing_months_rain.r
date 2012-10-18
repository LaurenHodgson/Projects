###load the necessary libraries
library(SDMTools) 
library(dismo)

###define directories
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir = "/home/22/jc148322/Ecocrop/"
pr.dir = "/home/22/jc148322/flatdata/pr/"; dir.create(pr.dir)

wd = '/homes/31/jc165798/tmp/soils/'; setwd(wd)
base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0


#01. get files of interest
setwd(esoclim.dir)
files = list.files()
files.of.interest=grep("2066",files, value=TRUE) #value=TRUE calls up full name of files
files.of.interest=grep("sresa1b",files.of.interest, value=TRUE)

 #put all rows and cols for finite values as dataframe
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


species.dir = paste("/home/22/jc148322/Ecocrop/",  species, '/', sep='')

copy.rain=pos.rain[3:ncol(pos.rain)]

for (ii in 1:ncol(copy.rain)){
	copy.rain[which(copy.rain[,ii] > too.wet),ii] = 0
	copy.rain[which(copy.rain[,ii] < too.dry),ii] = 0
}

for (ii in 1:ncol(copy.rain)){
	copy.rain[which(copy.rain[,ii] > 0),ii] = 1
}

GCMs = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 
GCM.count=NULL
for (gcm in GCMs){
	GCM.count=c(GCM.count, length(grep(gcm, names(copy.rain))))
}

GCM.weight=1/GCM.count


for (ii in 1:ncol(copy.rain)){ #for each column
	#apply weights to column
	tgcm=NULL; 
	for (gcm in GCMs){
		if (length(grep(gcm, names(copy.rain)[ii]))>0) tgcm=gcm 
	}
	weighting=GCM.weight[which(GCMs==tgcm)] #find out the weighting for that GCM
	copy.rain[,ii]=copy.rain[,ii]*weighting #apply the weighting

}

save(copy.rain, file=paste(species.dir, tt, '.pr', '.rData', sep=''))

copy.rain$total = rowSums(copy.rain[1:30])

rain.asc=base.asc
rain.asc[cbind(pos.rain$row, pos.rain$col)] = copy.rain$total


cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'black')

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(out.dir, '/', species, '.', 'rain.png',sep=''), width=8, height=7, units='cm', res=300, pointsize=5, bg='white')
par( mar=c(0,2,0,0))

image(rain.asc, ann=FALSE,axes=FALSE, col=cols)
#text (130, -40, 'Temperature', cex=3)
legend.gradient(pnts,cols=cols, limits=range(copy.rain$total), title='Rain', cex=2.2)

dev.off()
