
library(SDMTools)
out.dir ="/home/jc148322/Ecocrop_output/"
species.dir="/home/jc148322/Ecocrop_output/"
ecocrop.dir="/home/jc148322/Ecocrop_output/absolute/"

cropdata=read.csv(paste(out.dir,'ecocrop_new_edit.csv',sep=''))
trialdata=cropdata[1:4,]
ECOcrops=trialdata

files=list.files(ecocrop.dir)



current.cols= c('gray86','darkgreen')
future.cols= c('gray86',colorRampPalette(c('red3','goldenrod','forestgreen'))(8),'darkgreen')
pnts=cbind(x=c(113,116,116,113), y=c(-12.5,-12.5,-18.5,-18.5))

png(paste(out.dir, 'test_new.png',sep=''), width=14, height=21, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(4,2), mar=c(3,2,3,3))

for (ii in 1:4) {cat(ii,'\n')

###define 'constants'####################################

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
spp=strsplit(gsub('_',' ',species), ' ')[[1]][1:2]

yr='current'

species.dir = paste("/home/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')

file.of.interest=grep(species, files ,value=TRUE)
if (file.of.interest==paste(species,'.contains_NAs.txt',sep='')) { 
} else {

setwd(species.dir)
landuse.cur.asc=read.asc.gz('landuse.asc.gz')
landuse.cur.asc[which(landuse.cur.asc>=1)]=1

yr='2080'
species.dir = paste("/home/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.fut.asc=read.asc.gz('landuse.asc.gz')

if (isTRUE(max(landuse.cur.asc,na.rm=T)==0&& max(landuse.fut.asc,na.rm=T)==0)){
}else{

image(landuse.cur.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=current.cols)
text (130, -37, 'Current', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==1) {legend.gradient(pnts, cols=current.cols,limits=c(0,1), title='Current', cex=1.8)}


image(landuse.fut.asc, ann=FALSE,axes=FALSE, zlim=c(0,10), col=future.cols)
text (130, -37, '2080', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==1) {legend.gradient(pnts, cols=future.cols, limits=c(0,10), title='2080', cex=1.8)}
}
}
}

dev.off()







###################NOT YET EDITED BELOW





png(paste(out.dir, 'test2.png',sep=''), width=14, height=21, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(4,2), mar=c(3,2,3,3))

for (ii in 5:8) {cat(ii,'\n')

###define 'constants'####################################
species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
spp=strsplit(gsub('_',' ',species), ' ')[[1]][1:2]

yr='current'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=current.cols)
text (130, -37, 'Current', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==5) {legend.gradient(pnts, cols=current.cols,limits=c(0,1), title='Current', cex=1.8)}

yr='2080'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,10), col=future.cols)
text (130, -37, '2080', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==5) {legend.gradient(pnts, cols=future.cols, limits=c(0,10), title='2080', cex=1.8)}

}

dev.off()

png(paste(out.dir, 'test3.png',sep=''), width=14, height=21, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(4,2), mar=c(3,2,3,3))

for (ii in 9:12) {cat(ii,'\n')

###define 'constants'####################################
species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
spp=strsplit(gsub('_',' ',species), ' ')[[1]][1:2]

yr='current'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=current.cols)
text (130, -37, 'Current', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==9) {legend.gradient(pnts, cols=current.cols,limits=c(0,1), title='Current', cex=1.8)}

yr='2080'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,10), col=future.cols)
text (130, -37, '2080', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==9) {legend.gradient(pnts, cols=future.cols, limits=c(0,10), title='2080', cex=1.8)}

}

dev.off()

png(paste(out.dir, 'test4.png',sep=''), width=14, height=21, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(4,2), mar=c(3,2,3,3))

for (ii in 13:16) {cat(ii,'\n')

###define 'constants'####################################

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
spp=strsplit(gsub('_',' ',species), ' ')[[1]][1:2]

yr='current'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=current.cols)
text (130, -37, 'Current', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==13) {legend.gradient(pnts, cols=current.cols,limits=c(0,1), title='Current', cex=1.8)}

yr='2080'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,10), col=future.cols)
text (130, -37, '2080', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==13) {legend.gradient(pnts, cols=future.cols, limits=c(0,10), title='2080', cex=1.8)}

}

dev.off()

png(paste(out.dir, 'test5.png',sep=''), width=14, height=21, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(4,2), mar=c(3,2,3,3))

for (ii in 17:20) {cat(ii,'\n')

###define 'constants'####################################

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
spp=strsplit(gsub('_',' ',species), ' ')[[1]][1:2]

yr='current'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=current.cols)
text (130, -37, 'Current', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==17) {legend.gradient(pnts, cols=current.cols,limits=c(0,1), title='Current', cex=1.8)}

yr='2080'

species.dir = paste("/home/22/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)
landuse.asc=read.asc.gz('landuse.asc.gz')
image(landuse.asc, ann=FALSE,axes=FALSE, zlim=c(0,10), col=future.cols)
text (130, -37, '2080', cex=1.8, font=1)
text (130, -41, name, cex=1.8, font=2)
text (130, -43, paste(spp[1], ' ',spp[2], sep=''), cex=1.8, font=3)
if (ii==17) {legend.gradient(pnts, cols=future.cols, limits=c(0,10), title='2080', cex=1.8)}

}

dev.off()
