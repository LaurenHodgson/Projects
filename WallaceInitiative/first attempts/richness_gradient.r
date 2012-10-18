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
sres[which(sres>1)] = 1.01;
avoid[which(avoid>1)] = 1.01; 
sres[which(baseasc==0.000001)] = NA
avoid[which(baseasc==0.000001)] = NA

write.asc.gz(sres,paste(taxon,'.sresa1b.gradient.asc',sep=''))
write.asc.gz(avoid,paste(taxon,'.avoid.gradient.asc',sep=''))

zlim=c(0,1.01)

#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B','#F0D082','#F0E391','#F0ECB4'))(49),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD','#4AB3B9','#189CB0'))(51),'#0F72A5')

pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100))  #redefine these
png(paste(taxon, '.gradient.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext(taxon,side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()
