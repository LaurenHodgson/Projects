#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###load the necessary libraries
library(SDMTools) 

#directory locations
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/data/jc165798/Barra/future.esoclim/'
#/home1/31/jc165798/Climate/PCMDI/01.Oz.5km.61.90/future.esoclim/
out.dir = "/home/22/jc148322/Barra/temperature_outputs/" ;  #define & setwd to the output directory
script.dir="/home/22/jc148322/scripts/"

base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0
pos.table = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

years=c('2030','2050','2080')

for (yr in years) {
load(file=paste(out.dir, yr, '.weighted.mean.rData', sep=''))
tfile=paste('sum.',yr ,sep='')
tfile=tcopy[1:30]
for (ii in 1:ncol(tfile)){
	tfile[which(tfile[,ii] > 0),ii] = 1
}

tfile$total = rowSums(tfile[1:30])
tfile.asc = paste( 'sum.',yr,'.asc',sep='')
tfile.asc=base.asc
tfile.asc[cbind(pos.table$row, pos.table$col)] = tfile$total
write.asc.gz(tfile.asc, paste(out.dir,'/sum.',yr,'.asc',sep=''))
}

sum.2030.asc=read.asc.gz(paste(out.dir, '/', 'sum.2030.asc.gz',sep=''))
sum.2050.asc=read.asc.gz(paste(out.dir, '/', 'sum.2050.asc.gz',sep=''))
sum.2080.asc=read.asc.gz(paste(out.dir, '/', 'sum.2080.asc.gz',sep=''))
#plot of number of runs of models that indicate suitability
threshold = 0.1
#setup some plot parameters
bins = seq(0,1,length=31); bins = cut(threshold,bins,labels=FALSE)
cols= c(rep('gray90',bins),colorRampPalette(c('gray60','gray11'))(30)[bins:30])
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(out.dir, '/gcm.variability.png',sep=''), width=21, height=7, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(1,3))
image(sum.2030.asc, ann=FALSE,axes=FALSE, col=cols)
text (130, -40, '2030', cex=3)
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
legend.gradient(pnts,cols=cols, limits=range(sum.2030.asc,na.rm=T), title='Agreement of GCM runs',cex=2.5)
image(sum.2050.asc, ann=FALSE,axes=FALSE, col=cols)
text (130, -40, '2050', cex=3)
image(sum.2080.asc, ann=FALSE,axes=FALSE, col=cols)
text (130, -40, '2080', cex=3)
dev.off()
