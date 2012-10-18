#script to find mean and standard deviation of wettest and driest quarter, current
#############
#load libraries and base files
library(SDMTools)
base.asc = read.asc.gz('/home/jc165798/working/NARP_stability/OZ_5km/data/base.asc.gz') #read in the base ascii grid file
pos = read.csv('/home/jc165798/working/NARP_stability/OZ_5km/data/base.positions.csv',as.is=TRUE) #read in the base positions file

#define directories
data.dir='/home/jc165798/Climate/AWAP.direct.download/rain/monthly/'
image.dir='/home/jc148322/NARP_stability/images/'
summary.dir='/home/jc148322/NARP_stability/summary/';dir.create(summary.dir)

#prepare data
yrs=c(1976:2005)
files=NULL
for (yr in yrs){
	files=c(files,list.files(data.dir, pattern=as.character(yr)))
}

WET=c('12','01','02')
wet.months=NULL
for (month in WET){
	wet.months=c(wet.months,grep(paste(as.character(month),'.asc.gz',sep=''),files,value=TRUE))
}

DRY=c('06','07','08')
dry.months=NULL
for (month in DRY){
	dry.months=c(dry.months,grep(paste(as.character(month),'.asc.gz',sep=''),files,value=TRUE))
}

curwetmonths=pos
for (month in wet.months){
	curwetmonths[,month]=read.asc.gz(paste(data.dir,month,sep=''))[cbind(curwetmonths$row,curwetmonths$col)]
}
curdrymonths=pos
for (month in dry.months){
	curdrymonths[,month]=read.asc.gz(paste(data.dir,month,sep=''))[cbind(curdrymonths$row,curdrymonths$col)]
}


curwet=matrix(NA, nr = nrow(pos), nc=length(yrs))
colnames(curwet)=yrs
for (ii in 1:length(yrs)){
	curwet[,ii]=rowSums(curwetmonths[(grep(yrs[ii],colnames(curwetmonths),value=TRUE))])
}

curdry=matrix(NA, nr = nrow(pos), nc=length(yrs))
colnames(curdry)=yrs
for (ii in 1:length(yrs)){
	curdry[,ii]=rowSums(curdrymonths[(grep(yrs[ii],colnames(curdrymonths),value=TRUE))])
}

curwet=as.data.frame(curwet)
curdry=as.data.frame(curdry)

curwet$mean=rowMeans(curwet)
curdry$mean=rowMeans(curdry)

curwet$SD=apply(curwet,1,sd)
curdry$SD=apply(curdry,1,sd)

save(curwet,file=paste(summary.dir,'current_DJF.rData'))
save(curdry,file=paste(summary.dir,'current_JJA.rData'))

#############
#script to find standard deviation and delta of wettest and driest quarter, future
#############

future.dir='/home/jc165798/working/NARP_stability/OZ_5km/data/monthly/pre/'

files=list.files(future.dir, pattern='RCP85')
load(paste(future.dir,files[1],sep=''))
ls() #futdata

futurewet=wetsd=wetdelta=matrix(NA, nr = nrow(pos), nc=length(files))
futuredry=drysd=drydelta=matrix(NA, nr = nrow(pos), nc=length(files))
for (ii in 1:length(files)){
	load(paste(future.dir,files[ii],sep=''))
	yoi=futdata[,grep('pre2085',colnames(futdata))];yoi=as.data.frame(yoi)
	wet=yoi[,c(1,2,12)]
	dry=yoi[,c(6,7,8)]
	futurewet[,ii]=rowSums(wet);wetsd[,ii]=(futurewet[,ii]-curwet$mean)/curwet$SD; 
	wetdelta[,ii]=futurewet[,ii]/curwet$mean+0.000001
	
	futuredry[,ii]=rowSums(dry);drysd[,ii]=(futuredry[,ii]-curdry$mean)/curdry$SD; 
	drydelta[,ii]=futuredry[,ii]/curdry$mean+0.000001

}

GCMs=NULL
for (ii in 1:length(files)){
	GCMs=c(GCMs, strsplit(files[ii],'\\.')[[1]][1])
}

colnames(futurewet)=GCMs;colnames(wetsd)=GCMs;colnames(wetdelta)=GCMs;
colnames(futuredry)=GCMs;colnames(drysd)=GCMs;colnames(drydelta)=GCMs;

	outquant = t(apply(wetdelta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
	wet.deltaquant=outquant
	dry.deltaquant= t(apply(drydelta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
	wet.sdquant= t(apply(wetsd[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
	dry.sdquant= t(apply(drysd[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles

	save(wet.deltaquant,dry.deltaquant,wet.sdquant,dry.sdquant,file=paste(summary.dir,'DJF_JJA_RCP85_quantiles.rData'))

#make an image
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
cols = all.cols   # reverse colours for temperature data to make red hotter and use subset to get only red range for this map
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5))
sdlims=c(-1.1,1.1)
deltalims=c(0.6,1.4)
	
png(paste(image.dir,'DJF_JJA_RCP85_50percentile.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[2]*2+60, units='px', pointsize=20, bg='lightgrey')
	par(mar=c(0,2,2,0),mfrow=c(2,2),cex=1,oma=c(3,3,3,0))
	
	wetdelta.asc=base.asc; wetdelta.asc[cbind(pos$row,pos$col)]=wet.deltaquant[,2];
	wetdelta.asc[which(wetdelta.asc>=1.4)]=1.4; wetdelta.asc[which(wetdelta.asc<=0.6)]=0.6
	image(wetdelta.asc, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('Delta', line=1,  side=2, cex=2)
	mtext('DJF', line=1,  side=3, cex=2)
	legend.gradient(pnts,cols=cols,limits=deltalims, title='Delta (Proportion of current)', cex=1)
	
	drydelta.asc=base.asc; drydelta.asc[cbind(pos$row,pos$col)]=dry.deltaquant[,2]
	drydelta.asc[which(drydelta.asc>=1.4)]=1.4; drydelta.asc[which(drydelta.asc<=0.6)]=0.6
	image(drydelta.asc, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('JJA', line=1,  side=3, cex=2)
	
	wetsd.asc=base.asc; wetsd.asc[cbind(pos$row,pos$col)]=wet.sdquant[,2]
	wetsd.asc[which(wetsd.asc>=1.1)]=1.1; wetsd.asc[which(wetsd.asc<=-1.1)]=-1.1
	image(wetsd.asc, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)
	mtext('SD', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=sdlims, title='SD from current', cex=1)
	
	drysd.asc=base.asc; drysd.asc[cbind(pos$row,pos$col)]=dry.sdquant[,2]
	drysd.asc[which(drysd.asc>=1.1)]=1.1; drysd.asc[which(drysd.asc<=-1.1)]=-1.1
	drysd.asc=base.asc; drysd.asc[cbind(pos$row,pos$col)]=dry.sdquant[,2]
	image(drysd.asc, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)

dev.off()
	
	









































