library(SDMTools)	#load the necessary libraries
wd = '~/working/WallaceInitiative/summaries/GIS/taxa/'; setwd(wd);	#defin and set the working directory

taxon = 'amphibia'	#define the taxon of interest
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
sres = read.asc.gz(paste(taxon,'/2080_SRES_A1B_mean.asc.gz',sep=''))	#read in the no mitigation richness
avoid = read.asc.gz(paste(taxon,'/2080_A1B_A30r5l_mean.asc.gz',sep=''))	#read in the avoided richness

sres = sres / cur; avoid = avoid / cur	#get the proportionate change in richness
sres[which(sres>1)] = 1.5; sres[which(sres<1.5 & sres>=0.5)] = 1; sres[which(sres=0.5)] = 0.5
avoid[which(avoid>1)] = 1.5; avoid[which(avoid<1.5 & avoid>=0.5)] = 1; avoid[which(avoid<0.5)] = 0.5

png(); image(sres,col=heat.colors(3)); image(avoid,col=heat.colors(3)); dev.off()
