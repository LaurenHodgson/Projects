#name working directories

data.dir='/home/jc165798/working/NARP_stability/OZ_5km/data/'
monthly.dir=paste(data.dir, 'monthly/',sep='')
out.dir='/home/jc148322/NARP_stability/images/'
image.dir='/home/jc148322/NARP_stability/images/'

library(SDMTools)
#view monthly Rdata files

load(paste(monthly.dir,'/pre/1990_mean.Rdata',sep=''))
ls() #object name is annualdata
head(curmean[5,])

outquant = t(apply(curmean[,],1,function(x) { return(quantile(x,c(0.25,0.5,0.75),na.rm=TRUE,type=8)) })) #get the percentiles

test=curmean
test[which(test>outquant[,1])]=NA
test=as.data.frame(test)
test$rainfall=rowSums(test[,1:12],na.rm=T)

tt=curmean
tt[which(tt>outquant[,1])]=NA
tt[which(is.finite(tt))]=1
test$totalmonths=rowSums(tt[,1:12],na.rm=T)


#baseline variance
curdata=read.csv('/home/jc165798/Climate/AWAP.direct.download/summaries/Oz/monthly.csv/rain19402009.csv')
curdata=curdata[,c(grep(197501,colnames(curdata)):grep(200512,colnames(curdata)))] #subset the current data set

pos = read.csv('/home/jc165798/working/NARP_stability/OZ_5km/data/base.positions.csv',as.is=TRUE) #read in the base positions file

dryrainfall=matrix(NA, nr = nrow(pos), nc=length(1976:2005))
drymonths=matrix(NA, nr = nrow(pos), nc=length(1976:2005))
tdata=NULL; tt=1975
for (ii in 1976:2005){
	tdata=as.matrix(curdata[,c(grep(ii, colnames(curdata)))])
	tdata[which(tdata>outquant[,1])]=NA
	dryrainfall[,ii-tt]=rowSums(tdata[,1:12],na.rm=T)
	tdata[which(is.finite(tdata))]=1
	drymonths[,ii-tt]=rowSums(tdata[,1:12],na.rm=T)
}
colnames(dryrainfall)=c(1976:2005)
colnames(drymonths)=c(1976:2005)

dryrainfall=as.data.frame(dryrainfall)
drymonths=as.data.frame(drymonths)

dryrainfall$mean=rowMeans(dryrainfall)
drymonths$mean=rowMeans(drymonths)

dryrainfall$SD=apply(dryrainfall,1,sd)
drymonths$SD=apply(drymonths,1,sd)

save(dryrainfall,file=paste(summary.dir,'current_dryseason_rainfall.rData'))
save(drymonths,file=paste(summary.dir,'current_dryseason_months.rData'))

#############
#script to find standard deviation and delta of dry season rainfall and number of months, future
#############


future.dir='/home/jc165798/working/NARP_stability/OZ_5km/data/monthly/pre/'

files=list.files(future.dir, pattern='RCP85')
ls() #futdata

futdryrain=dryrainsd=dryraindelta=matrix(NA, nr = nrow(pos), nc=length(files))
futdrymonths=drymonthssd=drymonthsdelta=matrix(NA, nr = nrow(pos), nc=length(files))

for (ii in 1:length(files)){
	load(paste(future.dir,files[ii],sep=''))
	futdata=futdata[,grep('pre2085',colnames(futdata))]
	futdata[which(futdata>outquant[,1])]=NA
	futdryrain[,ii]=rowSums(futdata,na.rm=T)
	futdata[which(is.finite(futdata))]=1
	futdrymonths[,ii]=rowSums(futdata,na.rm=T)
	
	# dryrainsd[,ii]=(futdryrain[,ii]-dryrainfall$mean)/dryrainfall$SD; 
	# dryraindelta[,ii]=futdryrain[,ii]/dryrainfall$mean+0.000001
	
	# drymonthssd[,ii]=(futdrymonths[,ii]-drymonths$mean)/drymonths$SD; 
	drymonthsdelta[,ii]=futdrymonths[,ii]-drymonths$mean+0.000001

}


GCMs=NULL
for (ii in 1:length(files)){
	GCMs=c(GCMs, strsplit(files[ii],'\\.')[[1]][1])
}


colnames(futdryrain)=GCMs;colnames(dryrainsd)=GCMs;colnames(dryraindelta)=GCMs;
colnames(futdrymonths)=GCMs;colnames(drymonthssd)=GCMs;colnames(drymonthsdelta)=GCMs;

	rain.deltaquant = t(apply(dryraindelta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
	
	months.deltaquant= t(apply(drymonthsdelta[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
	rain.sdquant= t(apply(dryrainsd[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
	months.sdquant= t(apply(drymonthssd[,],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles

	save(rain.deltaquant,months.deltaquant,rain.sdquant,months.sdquant,file=paste(summary.dir,'dryseason_rainfall_months_quantiles.rData'))

	
all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
cols = all.cols   # reverse colours for temperature data to make red hotter and use subset to get only red range for this map
rev.cols=cols[21:1]
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5))
sdlims=c(-2,2)
deltalims=c(0,2)

base.asc = read.asc.gz('/home/jc165798/working/NARP_stability/OZ_5km/data/base.asc.gz') #read in the base ascii 

	
png(paste(image.dir,'dryseason_50percentile2.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[2]*2+60, units='px', pointsize=20, bg='lightgrey')
	par(mar=c(0,2,2,0),mfrow=c(2,2),cex=1,oma=c(3,3,3,0))
	

	raindelta.asc=base.asc; raindelta.asc[cbind(pos$row,pos$col)]=rain.deltaquant[,2];
	raindelta.asc[which(raindelta.asc>=2)]=2; raindelta.asc[which(raindelta.asc<=0)]=0
	image(raindelta.asc, zlim=deltalims, ann=FALSE,axes=FALSE,col=cols)
	mtext('Rainfall', line=1,  side=3, cex=2)
	mtext('Delta', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=deltalims, title='Delta (Proportion of current)', cex=1)
	
	monthsdelta.asc=base.asc; monthsdelta.asc[cbind(pos$row,pos$col)]=months.deltaquant[,2]
	image(monthsdelta.asc, zlim=c(-7,7), ann=FALSE,axes=FALSE,col=rev.cols)
	legend.gradient(pnts,cols=rev.cols,limits=c(-7,7), title='Delta (months from current)', cex=1)
	mtext('Months', line=1,  side=3, cex=2)
	
	rainsd.asc=base.asc; rainsd.asc[cbind(pos$row,pos$col)]=rain.sdquant[,2]
	rainsd.asc[which(rainsd.asc>=2)]=2; rainsd.asc[which(rainsd.asc<=-2)]=-2
	image(rainsd.asc, zlim=sdlims, ann=FALSE,axes=FALSE,col=cols)
	mtext('SD', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=sdlims, title='SD from current', cex=1)
	
	monthssd.asc=base.asc; monthssd.asc[cbind(pos$row,pos$col)]=months.sdquant[,2]
	monthssd.asc[which(monthssd.asc>=2)]=2; monthssd.asc[which(monthssd.asc<=-2)]=-2
	image(monthssd.asc, zlim=sdlims, ann=FALSE,axes=FALSE,col=rev.cols)
	legend.gradient(pnts,cols=rev.cols,limits=sdlims, title='SD from current', cex=1)
	
dev.off()
	