library(SDMTools)

work.dir='E:/freshwater/'; setwd(work.dir)

tasc=read.asc('streanntemp.asc',gz=FALSE)

pos=as.data.frame(which(is.finite(tasc), arr.ind=TRUE))
pos=rbind(pos,as.data.frame(which(is.na(tasc),arr.ind=TRUE))

pos$lon= getXYcoords(tasc)$x[pos$row] #row 

pos$lat = getXYcoords(tasc)$y[pos$col] #col

pnts=pos[3:4]

baseasc = read.asc('base.asc') #read in the asc file
baseasc[which(is.finite(baseasc))] = 0

pos$base=extract.data(pnts,baseasc)

fwbase=tasc
fwbase[cbind(pos$row,pos$col)]=pos$base


write.asc.gz(fwbase, 'fwbase.asc')

####

fwbase=read.asc('fwbase.asc',gz=FALSE)

pos2=as.data.frame(which(is.finite(fwbase), arr.ind=TRUE))

pos2$lon= getXYcoords(fwbase)$x[pos2$row] #row 

pos2$lat = getXYcoords(fwbase)$y[pos2$col] #col

pnts=pos[3:4]

####

fwbase=read.asc.gz('fwbase.asc.gz')
tasc=read.asc('streanntemp.asc',gz=FALSE)

tdata=read.csv('River cooba.csv')
obs=cbind(tdata$Latitude...processed,tdata$Longitude...processed)
locations=unique(obs)

cols = colorRampPalette(c('skyblue','slateblue','forestgreen','yellow','red'))(101)
png(paste('river_cooba.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')

plot(locations[,2],locations[,1], xlim=c(min(pos$lon),max(pos$lon)),ylim=c(min(pos$lat),max(pos$lat)), type='n',axes=FALSE,ann=FALSE)
image(fwbase,add=TRUE,col='gray88')
image(tasc,add=TRUE, col=cols)
points(locations[,2],locations[,1], xlim=c(min(pos$lon),max(pos$lon)),ylim=c(min(pos$lat),max(pos$lat)), col='black', pch=20)

dev.off()

