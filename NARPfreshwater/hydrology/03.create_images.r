library(SDMTools)

delta.dir='/home/jc148322/NARPfreshwater/deltas/'
image.dir='/home/jc148322/NARPfreshwater/images/deltas/'; dir.create(image.dir)

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.asc.gz') #read in the base ascii grid file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the base positions file

DELTAs = list.files(delta.dir, pattern='RCP85')
vois=c('Eact', 'Epot','Qrun','Rnet')

for (voi in vois) { cat(voi, '\n')

DELTAs=grep(voi, DELTAs, value=T)

jan_delta=matrix(NA,nr=nrow(pos), nc=length(DELTAs))
jul_delta=matrix(NA,nr=nrow(pos), nc=length(DELTAs))


for (ii in 1:length(DELTAs)) {
	load(paste(delta.dir,DELTAs[ii],sep=''));
	jan_delta[,ii]=delta[,1];jul_delta[,ii]=delta[,7]
}

jan_delta.quant = t(apply(jan_delta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))
jul_delta.quant = t(apply(jul_delta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) }))

#make asciis

jan_delta10=base.asc; jan_delta10[cbind(pos$row,pos$col)]=jan_delta.quant[,1]
jan_delta50=base.asc; jan_delta50[cbind(pos$row,pos$col)]=jan_delta.quant[,2]
jan_delta90=base.asc; jan_delta90[cbind(pos$row,pos$col)]=jan_delta.quant[,3]

jul_delta10=base.asc; jul_delta10[cbind(pos$row,pos$col)]=jul_delta.quant[,1]
jul_delta50=base.asc; jul_delta50[cbind(pos$row,pos$col)]=jul_delta.quant[,2]
jul_delta90=base.asc; jul_delta90[cbind(pos$row,pos$col)]=jul_delta.quant[,3]


#make images

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
cols = all.cols   # reverse colours for temperature data to make red hotter and use subset to get only red range for this map
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5))

deltalims=c(0.6,1.4)

#edit the asciis
jan_delta10[which(jan_delta10<deltalims[1])]=deltalims[1];jan_delta10[which(jan_delta10>deltalims[2])]=deltalims[2]
jan_delta50[which(jan_delta50<deltalims[1])]=deltalims[1];jan_delta50[which(jan_delta50>deltalims[2])]=deltalims[2]
jan_delta90[which(jan_delta90<deltalims[1])]=deltalims[1];jan_delta90[which(jan_delta90>deltalims[2])]=deltalims[2]

jul_delta10[which(jul_delta10<deltalims[1])]=deltalims[1];jul_delta10[which(jul_delta10>deltalims[2])]=deltalims[2]
jul_delta50[which(jul_delta50<deltalims[1])]=deltalims[1];jul_delta50[which(jul_delta50>deltalims[2])]=deltalims[2]
jul_delta90[which(jul_delta90<deltalims[1])]=deltalims[1];jul_delta90[which(jul_delta90>deltalims[2])]=deltalims[2]
	
	
#the image	
png(paste(image.dir, voi,'.png',sep=''),width=dim(base.asc)[1]*3+30, height=dim(base.asc)[2]*4+60, units='px', pointsize=20, bg='lightgrey')
	par(mar=c(0,2,2,0),mfrow=c(4,3),cex=1,oma=c(3,3,3,0))
	#jan delta
	
	image(jan_delta10, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('January', line=1,  side=2, cex=2)
	mtext('Delta', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=deltalims, title='Delta (Proportion of current)', cex=1)
	
	image(jan_delta50, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('50 percentile', line=1,  side=3, cex=2)

	image(jan_delta90, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('90 percentile', line=1,  side=3, cex=2)
	

	#july delta
	image(jul_delta10, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('July', line=1,  side=2, cex=2)
	mtext('10 percentile', line=1,  side=3, cex=2)
	legend.gradient(pnts,cols=cols,limits=deltalims, title='Delta (Proportion of current)', cex=1)
	
	image(jul_delta50, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('50 percentile', line=1,  side=3, cex=2)

	image(jul_delta90, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('90 percentile', line=1,  side=3, cex=2)
	

dev.off()
}
