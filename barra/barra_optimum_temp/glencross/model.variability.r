###hpc:qsub -l nodes=1:ppn=8 -I
###hpc:qsub -l nodes=1:ppn=2:V20Z -I

###load the necessary libraries
library(SDMTools) 

#directory locations
esoclim.dir = '/data/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/data/jc165798/Barra/future.esoclim/'
#/home1/31/jc165798/Climate/PCMDI/01.Oz.5km.61.90/future.esoclim/
out.dir = "/home2/22/jc148322/Barra/outputs/glencross/trials/"
setwd(out.dir)

base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0

#load the data
load(file=paste(out.dir, '2030.weighted.mean.rData', sep=''))
load(file=paste(out.dir, '2030.min.max.rData',sep=''))
twenty.thirty=tcopy[1:30] #make a copy

###determine number of models that indicate suitability
twenty.thirty=tcopy[1:30]
for (ii in 1:ncol(twenty.thirty)){
	twenty.thirty[which(twenty.thirty[,ii] > 0),ii] = 1
}

twenty.thirty$total = rowSums(twenty.thirty[1:30])

twenty.thirty.asc=base.asc
twenty.thirty.asc[cbind(pos$row, pos$col)] = twenty.thirty$total

#2050
load(file=paste(out.dir, '2050.weighted.mean.rData', sep=''))
load(file=paste(out.dir, '2050.min.max.rData',sep=''))
twenty.fifty=tcopy[1:30] #make a copy

###determine number of models that indicate suitability
twenty.fifty=tcopy[1:30]
for (ii in 1:ncol(twenty.fifty)){
	twenty.fifty[which(twenty.fifty[,ii] > 0),ii] = 1
}

twenty.fifty$total = rowSums(twenty.fifty[1:30])

twenty.fifty.asc=base.asc
twenty.fifty.asc[cbind(pos$row, pos$col)] = twenty.fifty$total

#2080
load(file=paste(out.dir, '2080.weighted.mean.rData', sep=''))
load(file=paste(out.dir, '2080.min.max.rData',sep=''))
twenty.eighty=tcopy[1:30] #make a copy

###determine number of models that indicate suitability
twenty.eighty=tcopy[1:30]
for (ii in 1:ncol(twenty.eighty)){
	twenty.eighty[which(twenty.eighty[,ii] > 0),ii] = 1
}

twenty.eighty$total = rowSums(twenty.eighty[1:30])

twenty.eighty.asc=base.asc
twenty.eighty.asc[cbind(pos$row, pos$col)] = twenty.eighty$total

#plot of number of runs of models that indicate suitability

cols= c('gray90',colorRampPalette(c('gray60','gray32'))(29),'black')

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(out.dir, '/', 'all.gcm.variability2.png',sep=''), width=19, height=6, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(1,3), mar=c(0,2,2,0))
image(twenty.thirty.asc, ann=FALSE,axes=FALSE, col=cols)
text (130, -40, 2030, cex=4)
legend.gradient(pnts,cols=cols, limits=c(0,30), title='GCMs predicting suitability', cex=3)
image(twenty.fifty.asc, ann=FALSE,axes=FALSE, col=cols)
text (130, -40, 2050, cex=4)
image(twenty.eighty.asc, ann=FALSE,axes=FALSE, col=cols)
text (130, -40, 2080, cex=4)

dev.off()
