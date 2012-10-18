library(SDMTools)	#load the necessary libraries
wd = '/home/jc148322/WallaceInitiative/'; setwd(wd);	#defin and set the working directory

taxon = 'Mammalia'	#define the taxon of interest
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
cur[which(cur==0)]=0.000001

avoid = read.asc.gz(paste(taxon,'/',taxon, '.A16r5l.2080.richness.asc.gz',sep=''))	#read in the avoided richness
baseasc=cur

avoid = avoid / cur	#get the proportionate change in richness

avoid[which(baseasc==0.000001)] = NA

write.asc.gz(avoid, paste(taxon,'.avoid16.gradient.asc',sep=''))



##test image
library(SDMTools)	#load the necessary libraries
wd = '/home/jc148322/WallaceInitiative/'; setwd(wd);	#defin and set the working directory

taxon = 'Plantae'	#define the taxon of interest
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
cur[which(cur==0)]=0.000001
sres = read.asc.gz(paste(taxon,'/2080_SRES_A1B_mean.asc.gz',sep=''))	#read in the no mitigation richness
avoid = read.asc.gz(paste(taxon,'/',taxon, '.A16r5l.2080.richness.asc.gz',sep=''))	#read in the avoided richness
#sres[which(sres==0)]=0.000000001
#avoid[which(avoid==0)]=0.000000001
baseasc=cur
baseasc2=baseasc
sres = sres / cur; avoid = avoid / cur	#get the proportionate change in richness

sres[which(baseasc==0.000001)] = NA
avoid[which(baseasc==0.000001)] = NA

baseasc2[which(is.finite(baseasc))]=0
baseasc2[which(baseasc==0.000001)] = NA

sres.loss=baseasc2; sres.loss[which(sres<1)] = sres[which(sres<1)]
avoid.loss=baseasc2; avoid.loss[which(avoid<1)] = avoid[which(avoid<1)]
sres.gain=baseasc2; sres.gain[which(sres>=1)] = 1.01; sres.gain[which(sres.gain!=1.01)]=NA
avoid.gain=baseasc2; avoid.gain[which(avoid>=1)] = 1.01; avoid.gain[which(avoid.gain!=1.01)]=NA
baseasc=cur


#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
#cols=c('#C62F1F',colorRampPalette(c())(100))
cols=c('black',colorRampPalette(c('black','darkred','red2','darkorange'))(51),colorRampPalette(c('orange','gold','papayawhip'))(49))
cols2=c('papayawhip','papayawhip')



pnts2=cbind(x=c(-150,-170,-170,-150), y=c(-20,-20,-17.5,-17.5))  #redefine these
pnts=cbind(x=c(-150,-170,-170,-150), y=c(-25,-25,-50,-50)) 
png(paste(taxon, '.avoid16.png',sep=''), width=13, height=11, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(0,0,0,0)+0.1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.loss,ann=FALSE,axes=FALSE,col=cols,add=TRUE)
image(sres.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)

text (165, 80, 'a)', cex=1.5)
text (0, -50, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','99'), title='',cex=1)
legend.gradient(pnts2,cols=cols2, limits=c('','100+'), title='Richness (% of current)', cex=1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.loss,ann=FALSE,axes=FALSE, col=cols,add=TRUE)
image(avoid.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)
text (165, 80, 'b)', cex=1.5)
text (10, -50, 'AVOID (A1B_A30r5l)', cex=1.5)

dev.off()

###All test image
library(SDMTools)	#load the necessary libraries
wd = '/home/jc148322/WallaceInitiative/'; setwd(wd);	#defin and set the working directory

taxon = 'All'	#define the taxon of interest
cur = read.asc.gz(paste('Amphibia/current.asc.gz',sep=''))	#read in the current richness
cur[which(cur==0)]=0.000001
sres = read.asc.gz(paste(taxon,'.avoid16.gradient.asc.gz',sep=''))	#read in the no mitigation richness
avoid = read.asc.gz(paste(taxon,'.sresa1b.gradient.asc.gz',sep=''))	#read in the avoided richness
#sres[which(sres==0)]=0.000000001
#avoid[which(avoid==0)]=0.000000001
baseasc=cur
baseasc2=baseasc

baseasc2[which(is.finite(baseasc))]=0
baseasc2[which(baseasc==0.000001)] = NA

sres.loss=baseasc2; sres.loss[which(sres<1)] = sres[which(sres<1)]
avoid.loss=baseasc2; avoid.loss[which(avoid<1)] = avoid[which(avoid<1)]
sres.gain=baseasc2; sres.gain[which(sres>=1)] = 1.01; sres.gain[which(sres.gain!=1.01)]=NA
avoid.gain=baseasc2; avoid.gain[which(avoid>=1)] = 1.01; avoid.gain[which(avoid.gain!=1.01)]=NA
baseasc=cur


#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
#cols=c('#C62F1F',colorRampPalette(c())(100))
cols=c('black',colorRampPalette(c('black','darkred','red2','darkorange'))(51),colorRampPalette(c('orange','gold','papayawhip'))(49))
cols2=c('papayawhip','papayawhip')



pnts2=cbind(x=c(-150,-170,-170,-150), y=c(-20,-20,-17.5,-17.5))  #redefine these
pnts=cbind(x=c(-150,-170,-170,-150), y=c(-25,-25,-50,-50)) 
png(paste(taxon, 'all.avoid16.png',sep=''), width=13, height=11, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(0,0,0,0)+0.1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.loss,ann=FALSE,axes=FALSE,col=cols,add=TRUE)
image(sres.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)

text (165, 80, 'a)', cex=1.5)
text (0, -50, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','99'), title='',cex=1)
legend.gradient(pnts2,cols=cols2, limits=c('','100+'), title='Richness (% of current)', cex=1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.loss,ann=FALSE,axes=FALSE, col=cols,add=TRUE)
image(avoid.gain,ann=FALSE,axes=FALSE,col=cols2,add=TRUE)
text (165, 80, 'b)', cex=1.5)
text (10, -50, 'AVOID (A1B_A30r5l)', cex=1.5)

dev.off()
