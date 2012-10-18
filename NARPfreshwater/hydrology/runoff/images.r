library(SDMTools)

delta.dir='/home/jc148322/Hydrology.trials/runoff/delta/'
sd.dir='/home/jc148322/Hydrology.trials/runoff/sd/'
image.dir='/home/jc148322/Hydrology.trials/runoff/images/'; dir.create(image.dir)

base.asc = read.asc.gz('/home/jc165798/working/NARP_stability/OZ_5km/data/base.asc.gz') #read in the base ascii grid file
pos = read.csv('/home/jc165798/working/NARP_stability/OZ_5km/data/base.positions.csv',as.is=TRUE) #read in the base positions file

DELTAs = list.files(delta.dir, pattern='RCP85')
SDs = list.files(sd.dir,pattern='RCP85')

jan_delta=matrix(NA,nr=nrow(pos), nc=length(DELTAs))
jan_sd=matrix(NA,nr=nrow(pos), nc=length(DELTAs))
jul_delta=matrix(NA,nr=nrow(pos), nc=length(DELTAs))
jul_sd=matrix(NA,nr=nrow(pos), nc=length(DELTAs))

for (ii in 1:length(DELTAs)) {
	load(paste(delta.dir,DELTAs[ii],sep=''));load(paste(sd.dir,SDs[ii],sep=''))
	jan_sd[,ii]=Fut_runoff_sd[,1];jul_sd[,ii]=Fut_runoff_sd[,7]
	jan_delta[,ii]=Fut_runoff_delta[,1];jul_delta[,ii]=Fut_runoff_delta[,7]
}

jan_sd.quant = t(apply(jan_sd[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))

jan_delta.quant = t(apply(jan_delta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))

jul_sd.quant = t(apply(jul_sd[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))

jul_delta.quant = t(apply(jul_delta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))

#make asciis

jan_sd10th=base.asc; jan_sd10th[cbind(pos$row,pos$col)]=jan_sd.quant[,1]
jan_sd50th=base.asc; jan_sd50th[cbind(pos$row,pos$col)]=jan_sd.quant[,2]
jan_sd90th=base.asc; jan_sd90th[cbind(pos$row,pos$col)]=jan_sd.quant[,3]

jul_sd10th=base.asc; jul_sd10th[cbind(pos$row,pos$col)]=jul_sd.quant[,1]
jul_sd50th=base.asc; jul_sd50th[cbind(pos$row,pos$col)]=jul_sd.quant[,2]
jul_sd90th=base.asc; jul_sd90th[cbind(pos$row,pos$col)]=jul_sd.quant[,3]

jan_delta10th=base.asc; jan_delta10th[cbind(pos$row,pos$col)]=jan_delta.quant[,1]
jan_delta50th=base.asc; jan_delta50th[cbind(pos$row,pos$col)]=jan_delta.quant[,2]
jan_delta90th=base.asc; jan_delta90th[cbind(pos$row,pos$col)]=jan_delta.quant[,3]

jul_delta10th=base.asc; jul_delta10th[cbind(pos$row,pos$col)]=jul_delta.quant[,1]
jul_delta50th=base.asc; jul_delta50th[cbind(pos$row,pos$col)]=jul_delta.quant[,2]
jul_delta90th=base.asc; jul_delta90th[cbind(pos$row,pos$col)]=jul_delta.quant[,3]


#make images

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
cols = all.cols   # reverse colours for temperature data to make red hotter and use subset to get only red range for this map
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5))
sdlims=c(-1.5,1.5)
deltalims=c(0.6,1.4)

#edit the asciis
jan_delta10th[which(jan_delta10th<deltalims[1])]=deltalims[1];jan_delta10th[which(jan_delta10th>deltalims[2])]=deltalims[2]
jan_delta50th[which(jan_delta50th<deltalims[1])]=deltalims[1];jan_delta50th[which(jan_delta50th>deltalims[2])]=deltalims[2]
jan_delta90th[which(jan_delta90th<deltalims[1])]=deltalims[1];jan_delta90th[which(jan_delta90th>deltalims[2])]=deltalims[2]
jan_sd10th[which(jan_sd10th<sdlims[1])]=sdlims[1];jan_sd10th[which(jan_sd10th>sdlims[2])]=sdlims[2]
jan_sd50th[which(jan_sd50th<sdlims[1])]=sdlims[1];jan_sd50th[which(jan_sd50th>sdlims[2])]=sdlims[2]
jan_sd90th[which(jan_sd90th<sdlims[1])]=sdlims[1];jan_sd90th[which(jan_sd90th>sdlims[2])]=sdlims[2]

jul_delta10th[which(jul_delta10th<deltalims[1])]=deltalims[1];jul_delta10th[which(jul_delta10th>deltalims[2])]=deltalims[2]
jul_delta50th[which(jul_delta50th<deltalims[1])]=deltalims[1];jul_delta50th[which(jul_delta50th>deltalims[2])]=deltalims[2]
jul_delta90th[which(jul_delta90th<deltalims[1])]=deltalims[1];jul_delta90th[which(jul_delta90th>deltalims[2])]=deltalims[2]
jul_sd10th[which(jul_sd10th<sdlims[1])]=sdlims[1];jul_sd10th[which(jul_sd10th>sdlims[2])]=sdlims[2]
jul_sd50th[which(jul_sd50th<sdlims[1])]=sdlims[1];jul_sd50th[which(jul_sd50th>sdlims[2])]=sdlims[2]
jul_sd90th[which(jul_sd90th<sdlims[1])]=sdlims[1];jul_sd90th[which(jul_sd90th>sdlims[2])]=sdlims[2]
	
	
	
#the image	
png(paste(image.dir,'runoff.png',sep=''),width=dim(base.asc)[1]*3+30, height=dim(base.asc)[2]*4+60, units='px', pointsize=20, bg='lightgrey')
	par(mar=c(0,2,2,0),mfrow=c(4,3),cex=1,oma=c(3,3,3,0))
	#jan delta
	
	image(jan_delta10th, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('January', line=3,  side=2, cex=2)
	mtext('Delta', line=1,  side=2, cex=2)
	mtext('10th percentile', line=1,  side=3, cex=2)
	legend.gradient(pnts,cols=cols,limits=deltalims, title='Delta (Proportion of current)', cex=1)
	
	image(jan_delta50th, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('50th percentile', line=1,  side=3, cex=2)

	image(jan_delta90th, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('90th percentile', line=1,  side=3, cex=2)
	
	#jan sd
	image(jan_sd10th, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)
	mtext('January', line=3,  side=2, cex=2)
	mtext('SD', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=sdlims, title='SD', cex=1)
	
	image(jan_sd50th, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)

	image(jan_sd90th, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)
	
	#july delta
	image(jul_delta10th, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('July', line=3,  side=2, cex=2)
	mtext('Delta', line=1,  side=2, cex=2)
	mtext('10th percentile', line=1,  side=3, cex=2)
	legend.gradient(pnts,cols=cols,limits=deltalims, title='Delta (Proportion of current)', cex=1)
	
	image(jul_delta50th, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('50th percentile', line=1,  side=3, cex=2)

	image(jul_delta90th, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('90th percentile', line=1,  side=3, cex=2)
	
	#jul sd
	image(jul_sd10th, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)
	mtext('July', line=3,  side=2, cex=2)
	mtext('SD', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=sdlims, title='SD', cex=1)
	
	image(jul_sd50th, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)

	image(jul_sd90th, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)
	
dev.off()
