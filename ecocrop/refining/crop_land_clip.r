yr='2080'

library(SDMTools)
out.dir ="/home/22/jc148322/Ecocrop_output/"
wd = '/home/22/jc148322/flatdata/'; setwd(wd)

cropdata=read.csv(paste(out.dir,'ecocropdata.csv',sep=''))
trialdata=cropdata[1:10,]
ECOcrops=trialdata

#for (ii in 1:nrow(ECOcrops)) {cat(ii,'\n')
ii=2
###define 'constants'####################################

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)

base.asc = read.asc(paste(wd,'base.asc',sep=''))
landuse.asc = read.asc(paste(wd,'landuse.asc',sep='')) #read in the asc file
temp.asc=read.asc.gz(paste(species, '.temp.asc.gz',sep=''))

rain.asc=read.asc.gz(paste(species, '.rain.asc.gz',sep=''))
ph.asc=read.asc.gz(paste(species, '.ph.asc.gz',sep=''))
sal.asc=read.asc.gz(paste(species, '.salinity.asc.gz',sep=''))
combine.asc=read.asc.gz(paste(species, '.combine.asc.gz',sep=''))

cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'#003300')

phcols=c('gray90',colorRampPalette(c('red','yellow','forestgreen','blue','purple'))(100))
salcols=c('gray90',colorRampPalette(c('moccasin','lightpink','lightpink3','lightpink4'))(100))

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(species.dir, '/', species, '.', yr, '.cropland.png',sep=''), width=21, height=14.5, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,3), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(temp.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=cols)
image(landuse.asc,ann=FALSE,axes=FALSE, col='gray90',add=TRUE)
#text (130, -40, 'Temperature', cex=3)
legend.gradient(pnts,cols=cols, limits=c(0,1), title='Temperature', cex=2.2)

image(rain.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=cols)
image(landuse.asc,ann=FALSE,axes=FALSE, col='gray90',add=TRUE)
legend.gradient(pnts,cols=cols, limits=round(range(rain.asc,na.rm=TRUE)), title='Rain', cex=2.2)
#text (130, -40, 'Annual rainfall', cex=3)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(ph.asc, ann=FALSE,axes=FALSE, zlim=c(0,14), col=phcols, add=TRUE)
image(landuse.asc,ann=FALSE,axes=FALSE, col='gray90',add=TRUE)
legend.gradient(pnts,cols=phcols, limits=c(0,14), title=paste('PH',' ','(',round(min(ph.asc,na.rm=TRUE),digits=2),'-',round(max(ph.asc,na.rm=TRUE),digits=2),')',sep=''), cex=2.2)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(sal.asc, ann=FALSE,axes=FALSE, col=salcols, add=TRUE)
image(landuse.asc,ann=FALSE,axes=FALSE, col='gray90',add=TRUE)
legend.gradient(pnts,cols=salcols, limits=round(range(sal.asc,na.rm=TRUE)), title='Salinity', cex=2.2)

#text (130, -40, 'PH', cex=3)
image(base.asc,ann=FALSE,axes=FALSE, col='gray90');
image(combine.asc, ann=FALSE,axes=FALSE, zlim=c(0,3), col='#003300', add=TRUE)
image(landuse.asc,ann=FALSE,axes=FALSE, col='gray90',add=TRUE)
#text (130, -40, paste(species, ' ','Model', sep=''), cex=3)
legend.gradient(pnts, cols=cols,limits=c(0,3), title='Combined', cex=2.2)

title(main=paste(name, ' (',gsub('_',' ',species), ') ', 'Model', ' ', yr, sep=''), outer=T,line=4,cex.main=3, font.main=2)

dev.off()

#}
