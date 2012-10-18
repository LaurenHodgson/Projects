library(SDMTools)	#load the necessary libraries
wd = '/home/22/jc148322/WallaceInitiative/'; setwd(wd);	#defin and set the working directory
out.dir = '/home/22/jc148322/WallaceInitiative/33percent/';

taxon = 'Mammalia'	#define the taxon of interest
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

zlim=c(0,1.01)

#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
#cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B','#F0D082','#F0E391','#F0ECB4'))(49),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD','#4AB3B9','#189CB0'))(51),'#0F72A5')
cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B'))(33),colorRampPalette(c('#F0D082','#F0E391','#F0ECB4'))(34),colorRampPalette(c('#90CEC0','#4AB3B9','#0F72A5'))(33),'dodgerblue4')



pnts=cbind(x=c(-150,-170,-170,-150), y=c(-25,-25,-50,-50))  #redefine these
png(paste(out.dir,taxon, '.gradient33.png',sep=''), width=13, height=11, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(0,0,0,0)+0.1)


image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (0, -50, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (0, -50, 'AVOID (A1B_A30r5l)', cex=1.5)
dev.off()
