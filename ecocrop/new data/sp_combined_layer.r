yr='2080'

library(SDMTools)
out.dir ="/home/22/jc148322/Ecocrop_output/"
wd = '/home/22/jc148322/flatdata/'; setwd(wd)

cropdata=read.csv(paste(out.dir,'ecocropdata.csv',sep=''))
trialdata=cropdata[1:20,]
ECOcrops=trialdata

for (ii in 1:nrow(ECOcrops)) {cat(ii,'\n')

###define 'constants'####################################

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME

landuse.asc = read.asc(paste(wd,'landuse.asc',sep='')) #read in the asc file


species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)




base.asc = read.asc(paste(wd,'base.asc',sep=''))
combine.asc=read.asc.gz(paste(species, '.combine.asc.gz',sep=''))

pos= as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

pos$combine=combine.asc[cbind(pos$row,pos$col)]
pos$landuse=landuse.asc[cbind(pos$row,pos$col)]
pos$landuse[which(pos$landuse==0)]=1
pos$landuse[which(is.na(pos$landuse))]=0
pos$landuse[which(pos$landuse==1)]=NA
pos$total=rowSums(pos[3:4])
pos$total[which(is.na(pos$total))]=0


landuse.asc=base.asc
landuse.asc[cbind(pos$row,pos$col)]=pos$total
write.asc.gz(landuse.asc,'landuse.asc')

}



###test image current

cols= c('gray90','#003300')
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(species.dir, '/', species, '.', yr, '.combined.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')
#par(mfrow=c(2,3), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=cols)
text (130, -40, paste(species, ', ',yr, sep=''), cex=2)
legend.gradient(pnts, cols=cols,limits=c(0,1), title='Combined', cex=2.2)

dev.off()
