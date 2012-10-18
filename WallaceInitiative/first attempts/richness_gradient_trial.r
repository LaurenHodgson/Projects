library(SDMTools)	#load the necessary libraries
wd = '/home/22/jc148322/WallaceInitiative'; setwd(wd);	#defin and set the working directory

taxon = 'Aves'	#define the taxon of interest
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
cur[which(cur==0)]=0.000001
sres = read.asc.gz(paste(taxon,'/2080_SRES_A1B_mean.asc.gz',sep=''))	#read in the no mitigation richness
avoid = read.asc.gz(paste(taxon,'/2080_A1B_A30r5l_mean.asc.gz',sep=''))	#read in the avoided richness
#sres[which(sres==0)]=0.000000001
#avoid[which(avoid==0)]=0.000000001
baseasc=cur

sres = sres / cur; avoid = avoid / cur	#get the proportionate change in richness

sres[which(baseasc==0.000001)] = NA
avoid[which(baseasc==0.000001)] = NA

baseasc2[which(is.finite(baseasc))]=0
baseasc2[which(baseasc==0.000001)] = NA

sres.loss=baseasc2; sres.loss[which(sres<1)] = sres[which(sres<1)]
avoid.loss=baseasc2; avoid.loss[which(avoid<1)] = avoid[which(avoid<1)]
sres.remain=baseasc2; sres.remain[which(sres==1)] = sres[which(sres==1)]; 
sres.remain[which(sres>1 & sres<1)] = NA; sres.remain[which(sres.remain<1)] = NA
avoid.remain=baseasc2; avoid.remain[which(avoid==1)] = avoid[which(avoid==1)];
avoid.remain[which(avoid.remain>1)] = NA; avoid.remain[which(avoid.remain<1)] = NA
sres.gain=baseasc2; sres.gain[which(sres>1)] = sres[which(sres>1)];sres.gain[which(sres.gain>3)]=NA
avoid.gain=baseasc2; avoid.gain[which(avoid>1)] = avoid[which(avoid>1)];avoid.gain[which(avoid.gain>3)]=NA
sres.200=baseasc2; sres.200[which(is.finite(sres.200))]=NA; sres.200[which(sres>3)] = sres[which(sres>3)]
avoid.200=baseasc2; avoid.200[which(is.finite(avoid.200))]=NA; avoid.200[which(avoid>3)] = avoid[which(avoid>3)]

baseasc=cur

#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B','#F0D082','#F0E391','#F0ECB4'))(50))
cols2=colorRampPalette(c('#6EC1BD','#189CB0','#0F72A5','midnightblue'))(50)


pnts2=cbind(x=c(-170,-180,-180,-170), y=c(87.5,87.5,100,100))  #redefine these
pnts=cbind(x=c(-170,-180,-180,-170), y=c(75,75,87.5,87.5))
png(paste('trials/',taxon, '.gradient2.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.loss,ann=FALSE,axes=FALSE,col=cols,add=TRUE)
image(sres.remain,ann=FALSE,axes=FALSE,col='#B0DBC3',add=TRUE)
image(sres.gain,ann=FALSE,axes=FALSE,zlim=c(1,3),col=cols2,add=TRUE)
image(sres.200,ann=FALSE,axes=FALSE,col='midnightblue',add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('Loss(0-99%)',''), title='',cex=1)
legend.gradient(pnts2,cols=cols2, limits=c('',paste('Retain or gain(100-',round(max(c(max(avoid,na.rm=T),max(sres,na.rm=T)))*100),'%)',sep='')), title='Richness (% of current)', cex=1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.loss,ann=FALSE,axes=FALSE, col=cols,add=TRUE)
image(avoid.remain,ann=FALSE,axes=FALSE,col='#B0DBC3',add=TRUE)
image(avoid.gain,ann=FALSE,axes=FALSE,zlim=c(1,3),col=cols2,add=TRUE)
image(avoid.200,ann=FALSE,axes=FALSE,col='midnightblue',add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext(taxon,side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()
