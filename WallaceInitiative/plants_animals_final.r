library(SDMTools)	#hpc directory
wd = '/home/jc148322/WallaceInitiative/'; setwd(wd);	#defin and set the working directory

library(SDMTools)	#local directory
wd = 'C:/Users/jc148322/Documents/Work Directory/Jeremy Work Directory/world_richness/'; setwd(wd);

taxon='Amphibia'
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
baseasc=cur
baseasc[which(is.finite(baseasc))] = 0

#####################
###combining the layers - skip to where Mollenweide projection files are read in
#####################
#read in your layers to be combined
amphibia.avoid.gradient=read.asc.gz('Amphibia.avoid.gradient.asc.gz')
amphibia.sres.gradient=read.asc.gz('Amphibia.sresa1b.gradient.asc.gz')

aves.avoid.gradient=read.asc.gz('Aves.avoid.gradient.asc.gz')
aves.sres.gradient=read.asc.gz('Aves.sresa1b.gradient.asc.gz')

mammalia.avoid.gradient=read.asc.gz('Mammalia.avoid.gradient.asc.gz')
mammalia.sres.gradient=read.asc.gz('Mammalia.sresa1b.gradient.asc.gz')

reptilia.avoid.gradient=read.asc.gz('Reptilia.avoid.gradient.asc.gz')
reptilia.sres.gradient=read.asc.gz('Reptilia.sresa1b.gradient.asc.gz')

#create data frames

all.avoid.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.avoid.gradient$amphibia = amphibia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$aves=aves.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$mammalia=mammalia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$reptilia=reptilia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$count=rowSums(!is.na(all.avoid.gradient[3:6]))

for (ii in 3:6) {
	all.avoid.gradient[,ii]=all.avoid.gradient[,ii]/all.avoid.gradient$count

}

for (ii in 3:ncol(all.avoid.gradient)){
	all.avoid.gradient[which(all.avoid.gradient[,ii]==0),ii] = 0.00001
	}
for (ii in 3:ncol(all.avoid.gradient)){
	all.avoid.gradient[which(is.na(all.avoid.gradient[,ii])),ii] = 0
}

all.avoid.gradient$total=rowSums(all.avoid.gradient[3:6])
all.avoid.gradient$total[which(all.avoid.gradient$total==0)]=NA
all.avoid.gradient$total[which(all.avoid.gradient$total<=0.00005)]=0
all.avoid.gradient$total[which(all.avoid.gradient$total>1)]=1.01

avoid.gradient=baseasc
avoid.gradient[cbind(all.avoid.gradient$row, all.avoid.gradient$col)] = all.avoid.gradient$total

write.asc.gz(avoid.gradient, 'All.avoid.gradient.asc')

#

all.sres.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.sres.gradient$amphibia = amphibia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$aves=aves.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$mammalia=mammalia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$reptilia=reptilia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$count=rowSums(!is.na(all.sres.gradient[3:6]))

for (ii in 3:6) {
	all.sres.gradient[,ii]=all.sres.gradient[,ii]/all.sres.gradient$count

}

for (ii in 3:ncol(all.sres.gradient)){
	all.sres.gradient[which(all.sres.gradient[,ii]==0),ii] = 0.00001
}
for (ii in 3:ncol(all.sres.gradient)){
	all.sres.gradient[which(is.na(all.sres.gradient[,ii])),ii] = 0
}

all.sres.gradient$total=rowSums(all.sres.gradient[3:6])
all.sres.gradient$total[which(all.sres.gradient$total==0)]=NA
all.sres.gradient$total[which(all.sres.gradient$total<=0.00005)]=0
all.sres.gradient$total[which(all.sres.gradient$total>1)]=1.01

sres.gradient=baseasc
sres.gradient[cbind(all.sres.gradient$row, all.sres.gradient$col)] = all.sres.gradient$total

sres.gain=baseasc; sres.gain[which(sres.gradient>=1)] = 1.01; sres.gain[which(sres.gain!=1.01)]=NA
avoid.gain=baseasc; avoid.gain[which(avoid.gradient>=1)] = 1.01; avoid.gain[which(avoid.gain!=1.01)]=NA

write.asc.gz(sres.gradient, 'All.sresa1b.gradient.asc')

#####################################
#change projection in arcgis
#####################################
#read in new projections

sres.gradient=read.asc('all.sresa1b.gradient_m.asc')
avoid.gradient=read.asc('all.avoid.gradient_m.asc')
baseasc=read.asc('current_m.asc')
sres.gain=baseasc; sres.gain[which(sres.gradient>=1)] = 1.01; sres.gain[which(sres.gain!=1.01)]=NA
avoid.gain=baseasc; avoid.gain[which(avoid.gradient>=1)] = 1.01; avoid.gain[which(avoid.gain!=1.01)]=NA
bg=read.asc('bg_m.asc')
diff.gradient=avoid.gradient-sres.gradient
#diff.gradient[which(diff.gradient<=0)]=NA


#plants
plantae.sres.gradient=read.asc('plantae.sresa1b.gradient_m.asc')
plantae.avoid.gradient=read.asc('plantae.avoid.gradient_m.asc')

plantae.sres.gain=baseasc; plantae.sres.gain[which(plantae.sres.gradient>=1)] = 1.01; plantae.sres.gain[which(plantae.sres.gain!=1.01)]=NA
plantae.avoid.gain=baseasc; plantae.avoid.gain[which(plantae.avoid.gradient>=1)] = 1.01; plantae.avoid.gain[which(plantae.avoid.gain!=1.01)]=NA

plantae.diff.gradient=plantae.avoid.gradient-plantae.sres.gradient
#plantae.diff.gradient[which(plantae.diff.gradient<=0)]=NA


zlim=c(0,1.01)


#cols=c('black',colorRampPalette(c('black','red4','red3','darkorange'))(51),colorRampPalette(c('orange','gold','papayawhip'))(49))
cols=c('black',colorRampPalette(c('black','darkred','red2','darkorange'))(51),colorRampPalette(c('orange','gold','papayawhip'))(49))
cols2=c('papayawhip','papayawhip')

cols3=colorRampPalette(c('papayawhip','papayawhip','papayawhip','papayawhip','papayawhip','papayawhip','papayawhip','papayawhip','papayawhip','papayawhip','darkolivegreen2','forestgreen',"#003300",'black','black','black','black','black','black','black'))(100)
l.cols3=colorRampPalette(c('papayawhip','darkolivegreen2','forestgreen',"#003300",'black'))(100)

pnts2=cbind(x=c(-14000000,-15500000,-15500000,-14000000), y=c(-400000,-400000,50000,50000))  #redefine these
pnts=cbind(x=c(-14000000,-15500000,-15500000,-14000000), y=c(-4500000,-4500000,-1000000,-1000000)) 

png(paste('plants_animals_final.png',sep=''), width=17, height=12, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(3,2), mar=c(0,4,4,0)+0.1)

#animals sres
image(bg,ann=FALSE,axes=FALSE,col='#99B3CC')
image(baseasc,ann=FALSE,axes=FALSE,col='gray70', add=TRUE)
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)
image(sres.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)
mtext('SRES A1B', side=2, line=1, cex=1.5)
mtext('Animals', side=3, line=1, cex=1.5, font=4)

legend.gradient(pnts,cols=cols, limits=c('0%','>100%'), title='Richness',cex=2)


#plants sres

image(bg,ann=FALSE,axes=FALSE,col='#99B3CC')
image(baseasc,ann=FALSE,axes=FALSE,col='gray70', add=TRUE)
image(plantae.sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)
image(plantae.sres.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)

mtext('Plants', side=3, line=1, cex=1.5, font=4)

#animals avoid
image(bg,ann=FALSE,axes=FALSE,col='#99B3CC')
image(baseasc,ann=FALSE,axes=FALSE,col='gray70', add=TRUE)
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)
image(avoid.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)
mtext('AVOID (A1B_A30r5l)', side=2, line=1, cex=1.5)

#plants avoid
image(bg,ann=FALSE,axes=FALSE,col='#99B3CC')
image(baseasc,ann=FALSE,axes=FALSE,col='gray70', add=TRUE)
image(plantae.avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)
image(plantae.avoid.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)

#animals difference
image(bg,ann=FALSE,axes=FALSE,col='#99B3CC')
image(baseasc,ann=FALSE,axes=FALSE,col='gray70', add=TRUE)
image(diff.gradient,ann=FALSE,axes=FALSE,zlim=c(-1.01,1.01), col=cols3,add=TRUE)
mtext('Difference (%)', side=2, line=1, cex=1.5)
legend.gradient(pnts,cols=l.cols3, limits=c('<0%','>40%'),  title='Avoided loss',cex=2)

#plants difference
image(bg,ann=FALSE,axes=FALSE,col='#99B3CC')
image(baseasc,ann=FALSE,axes=FALSE,col='gray70', add=TRUE)
image(plantae.diff.gradient,ann=FALSE,axes=FALSE,zlim=c(-1.01,1.01), col=cols3,add=TRUE)

dev.off()

