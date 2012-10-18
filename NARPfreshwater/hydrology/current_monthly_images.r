library(SDMTools)

data.dir='/home/jc165798/working/NARP_hydro/output/';setwd(data.dir)
image.dir='/home/jc148322/NARPfreshwater/images/'; 

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.asc.gz') #read in the base ascii grid file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the base positions file

load('Eact.current.Rdata')
load('Epot.current.Rdata')
load('Qrun.current.Rdata')
load('Rnet.current.Rdata')

vois=c('Eact','Epot','Qrun','Rnet')
titles=c('Actual Evaporation', 'Potential Evaporation', 'Runoff', 'Net Radiation')

months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
cols = c('black', colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21)) #define the color ramp
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5)) #define points for legend


i=1
for (voi in vois) { cat(voi,'\n')
	
	png(paste(image.dir,voi,'.jjv.ZEROISBLACK.png',sep=''), width=dim(base.asc)[1]*2+30, height=dim(base.asc)[2]*1.5+60, units='px', pointsize=30, bg='lightgrey')
	
	par(mfrow=c(3,4),mar=c(2,3,2,3), oma=c(0,3,3,0))
	voi=get(voi)
	zlim=range(voi,na.rm=T)

	#loop
	for (ii in 1:12) {  cat(ii,'\n')
		  tasc = base.asc; tasc[cbind(pos$row,pos$col)]=voi[,ii]
		  image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
		  text (130, -40, months[ii], cex=4)
		  if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title=titles[i], cex=3)}

		  }
	
	dev.off()
	i=i+1
}
