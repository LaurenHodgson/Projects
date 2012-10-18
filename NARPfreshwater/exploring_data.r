##############EXPLORE THE DATA

#name working directories

data.dir='/home/jc165798/working/NARP_stability/OZ_5km/data/'
ann.dir=paste(data.dir, 'annual/',sep='')
monthly.dir=paste(data.dir, 'monthly/',sep='')
out.dir='/home/jc148322/NARP_stability/images/'

#load libraries and base files
library(SDMTools)
base.asc = read.asc.gz(paste(data.dir,'base.asc.gz',sep='')) #read in the base ascii grid file
pos = read.csv(paste(data.dir,'base.positions.csv',sep=''),as.is=TRUE) #read in the base positions file

#view annual RData files

load(paste(ann.dir,'tmp.Rdata',sep=''))
ls() #object name is annualdata
head(annualdata[5,])

#make some images of 1990 mean and SD

all.cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
cols = all.cols[21:1]   # reverse colours for temperature data to make red hotter and use subset to get only red range for this map
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5))

tasc = base.asc
tasc[cbind(pos$row,pos$col)]=annualdata[,1]

png(paste(out.dir,'annual_mean_1990.png',sep=''), width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=30, bg='lightgrey')

	image(tasc, zlim=range(annualdata[,1],na.rm=T), ann=FALSE,axes=FALSE,col=cols)
	text (130, -40, 'Current (Annual)', cex=4)
	legend.gradient(pnts,cols=cols,limits=round(range(annualdata[,1],na.rm=T)), title='Mean Temperature', cex=3)
	dev.off()

tasc = base.asc
tasc[cbind(pos$row,pos$col)]=annualdata[,2]

png(paste(out.dir,'annual_SD_1990.png',sep=''), width=dim(base.asc)[1]*4+30, height=dim(base.asc)[2]*3+60, units='px', pointsize=30, bg='lightgrey')
	image(tasc, zlim=range(annualdata[,2],na.rm=T), ann=FALSE,axes=FALSE,col=cols)
	text (130, -40, 'Current (Annual)', cex=4)
	legend.gradient(pnts,cols=cols,limits=round(range(annualdata[,2],na.rm=T)), title='Temperature SD', cex=3)
	dev.off()

#make some images of 1990 monthly mean and SD
#view monthly Rdata files

load(paste(monthly.dir,'/tmp/1990_mean.Rdata',sep=''))
ls() #object name is annualdata
head(curmean[5,])

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

#image
png(paste(out.dir,'monthly_mean_1990.png',sep=''), width=4*(dim(base.asc)[1]*4+30), height=3*(dim(base.asc)[2]*3+60), units='px', pointsize=30, bg='lightgrey')

par(mfrow=c(3,4),mar=c(0,1,0,1))

for (ii in 1:12) {cat(ii,'\n')
	tasc = base.asc
	tasc[cbind(pos$row,pos$col)]=curmean[,ii]
	
	image(tasc, zlim=range(curmean,na.rm=T), ann=FALSE,axes=FALSE,col=cols)
	text (130, -40, months[ii], cex=5)
	if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(range(curmean,na.rm=T)), title='Mean Temperature', cex=4)}
	

}
dev.off()

load(paste(monthly.dir,'/tmp/1990_sd.Rdata',sep=''))
ls() #object name is annualdata
head(curmean[5,])


png(paste(out.dir,'monthly_sd_1990.png',sep=''), width=4*(dim(base.asc)[1]*4+30), height=3*(dim(base.asc)[2]*3+60), units='px', pointsize=30, bg='lightgrey')

par(mfrow=c(3,4),mar=c(0,1,0,1))

for (ii in 1:12) {cat(ii,'\n')
	tasc = base.asc
	tasc[cbind(pos$row,pos$col)]=cursd[,ii]
	
	image(tasc, zlim=range(cursd,na.rm=T), ann=FALSE,axes=FALSE,col=cols)
	text (130, -40, months[ii], cex=5)
	if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(range(cursd,na.rm=T)), title='SD Temperature', cex=4)}
	

}
dev.off()

#print version SD current
png(paste(out.dir,'PRINT_monthly_sd_1990.png',sep=''),  width=28, height=19, units='cm', res=300, pointsize=5, bg='lightgrey')

par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))

for (ii in 1:12) {cat(ii,'\n')
	tasc = base.asc
	tasc[cbind(pos$row,pos$col)]=cursd[,ii]
	
	image(tasc, zlim=range(cursd,na.rm=T), ann=FALSE,axes=FALSE,col=cols)
	text (130, -40, months[ii], cex=3)
	if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(range(cursd,na.rm=T)), title='SD Temperature', cex=2)}
	

}
dev.off()

#############ASIDE
#max sd of mean temp of a location throughout the year.

tt=apply(cursd,1,max)
gg=apply(cursd[,c(1,2,12)],1,max)
jj=apply(cursd[,c(6,7,8)],1,max)
tasc.all=base.asc; tasc.all[cbind(pos$row,pos$col)]=tt
tasc.hot=base.asc; tasc.hot[cbind(pos$row,pos$col)]=gg
tasc.cold=base.asc; tasc.cold[cbind(pos$row,pos$col)]=jj
png(paste(out.dir,'max_monthly_SD_1990.png',sep=''), width=dim(base.asc)[1]*3, height=dim(base.asc)[2]*1+25, units='px', pointsize=20, bg='lightgrey')
par(mar=c(0,2,0,0),mfrow=c(1,3),oma=c(0,2,2,0))
	image(tasc.all, zlim=c(0,max(tt,na.rm=T)), ann=FALSE,axes=FALSE,col=cols)
	text (131, -42, 'All months',cex=2.5)
	mtext('Current maximum SD', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=c(0,round(max(tt,na.rm=T))), title='Mean Temp. SD', cex=2)
	
	image(tasc.hot, zlim=c(0,max(tt,na.rm=T)), ann=FALSE,axes=FALSE,col=cols)
	text (131, -42, 'Hottest quarter', cex=2.5)
	
	image(tasc.cold, zlim=c(0,max(tt,na.rm=T)), ann=FALSE,axes=FALSE,col=cols)
	text (131, -42, 'Coldest quarter', cex=2.5)
	dev.off()
	
################

#investigate future data
#annual















