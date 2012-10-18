library(SDMTools)	#load the necessary libraries
wd = '/home/22/jc148322/WallaceInitiative'; setwd(wd);	#defin and set the working directory

taxon = 'Mammalia'	#define the taxon of interest
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
sres = read.asc.gz(paste(taxon,'/2080_SRES_A1B_mean.asc.gz',sep=''))	#read in the no mitigation richness
avoid = read.asc.gz(paste(taxon,'/2080_A1B_A30r5l_mean.asc.gz',sep=''))	#read in the avoided richness
sres[which(cur==0)]=NA
avoid[which(cur==0)]=NA
baseasc=cur
avoid.zero=baseasc; avoid.zero[which(cur==0)]=NA;avoid.zero[which(avoid>0)]=NA
sres.zero=baseasc; sres.zero[which(cur==0)]=NA;sres.zero[which(sres>0)]=NA
avoid.min=baseasc; avoid.min[which(cur==0)]=NA;avoid.min[which(avoid>1)]=NA; avoid.min[which(avoid<0.00001)]=NA;
sres.min=baseasc; sres.min[which(cur==0)]=NA;sres.min[which(sres>1)]=NA; avoid.min[which(sres<0.00001 )]=NA
baseasc[which(is.finite(baseasc))]=0
cur[which(cur==0)]=NA

#sres[which(sres==0)]=0.000000001
#avoid[which(avoid==0)]=0.000000001

#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
cols2=c(colorRampPalette(c('#B0DBC3','#189CB0','#0F72A5','midnightblue'))(100))
zlim=c(0,round(max(cur,na.rm=T)))
pnts=cbind(x=c(-170,-180,-180,-170), y=c(75,75,100,100))  #redefine these

png(paste(taxon, '.richness.png',sep=''), width=12.5, height=21, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(3,1),mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(cur,ann=FALSE,axes=FALSE,zlim=zlim,col=cols2,add=TRUE)
legend.gradient(pnts,cols=cols2, limits=c('0',round(max(cur,na.rm=T))), title='Current',cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres,ann=FALSE,axes=FALSE,zlim=zlim,cols2,add=TRUE)
image(sres.min,ann=FALSE,axes=FALSE,col='#F0E391',add=TRUE)
image(sres.zero,ann=FALSE,axes=FALSE,col='#C62F1F',add=TRUE)
legend.gradient(pnts,cols=cols2, limits=c('0',round(max(cur,na.rm=T))), title=paste('SRES A1B (range: ',round(min(sres,na.rm=T)),'-',round(max(sres,na.rm=T)),')',sep=''),cex=1.5)
legend(-160,110, c('0', '<1'), fill=c('#C62F1F','#F0E391'), title='', cex=1.5, bty='n')


image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid,ann=FALSE,axes=FALSE,zlim=zlim,col=cols2,add=TRUE)
image(avoid.min,ann=FALSE,axes=FALSE,col='#F0E391',add=TRUE)
image(avoid.zero,ann=FALSE,axes=FALSE,col='#C62F1F',add=TRUE)
legend.gradient(pnts,cols=cols2, limits=c('0',round(max(cur,na.rm=T))), title=paste('Avoid (range: ',round(min(avoid,na.rm=T)),'-',round(max(avoid,na.rm=T)),')',sep=''),cex=1.5)
legend(-160,110, c('0', '<1'), fill=c('#C62F1F','#F0E391'), title='', cex=1.5, bty='n')


title(main='Species richness', outer=T,line=4,cex.main=2, font.main=2)
mtext(taxon,side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()
