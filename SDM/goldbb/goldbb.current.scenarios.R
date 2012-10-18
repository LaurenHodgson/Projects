spp="GOLDBB"

library(SDMTools)
#define & set the working directory
work.dir = 'H:/Barra Work Directory/GOLDBB'; setwd(work.dir) 
#get the threshold
threshold = 0.0097

#setup some plot parameters
bins = seq(0,1,length=101); bins = cut(threshold,bins,labels=FALSE) # get the threshold bin for cols
cols = c(rep('gray86',bins),colorRampPalette(c("#CC9966","forestgreen","darkgreen","#003300"))(100)[bins:100])
legend.pnts = cbind(c(144.9,145.1,145.1,144.9),c(-19.5,-19.5,-18.75,-18.75))

#readin the necessary asciis....
current.asc = read.asc.gz('H:/Barra Work Directory/GOLDBB/1990.realized.asc.gz')
b1 = read.asc.gz('H:/Barra Work Directory/GOLDBB/sresb1.2080.mean.realized.asc.gz')
a1b = read.asc.gz('H:/Barra Work Directory/GOLDBB/sresa1b.2080.mean.realized.asc.gz')
a2 = read.asc.gz('H:/Barra Work Directory/GOLDBB/sresa2.2080.mean.realized.asc.gz')

###tfile is just the file name for the png

#basic plotting of the 3 plot
png(paste('goldbb_scenarios.png',sep=''),width=17,height=12.75,units='cm',res=300,pointsize=6) #start the png file
	par(oma=c(3,3,0.5,0.5),mar=c(0,0,0,0),cex=1,cex.axis=2) #se the parameters of hte plot
	mat = matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,3,3,
				   1,1,1,1,2,2,2,2,3,3,3,3,3,3,
				   1,1,1,1,1,2,2,2,2,3,3,3,3,3,
				   1,1,1,1,1,1,2,2,2,2,3,3,3,3,
				   1,1,1,1,1,1,2,2,2,2,3,3,3,3),nr=5,byrow=TRUE)
	layout(mat) #define and setup the layout of the plot... 3 maps and one time bar
	#plot the maps
	image(b1,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the first map and add associated text & lat/lon info & legend
	text(145.35,-15.5,labels ='SRES B1',cex=3,adj=0)
	axis(1,at=seq(145,147,0.5),labels=c(145,NA,146,NA,147),lwd=0,lwd.ticks=1); axis(2,at=seq(-15.5,-19.5,-0.5),labels=c(NA,-16,NA,-17,NA,-18,NA,-19,NA),lwd=0,lwd.ticks=1)
	legend.gradient(legend.pnts,col=cols, c(0,1), title='Suitability',cex=2.25)
	image(a1b,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the second map and add associated text
	text(145.35,-15.5,labels ='SRES A1B',cex=3,adj=0)
	image(a2,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the third map and add associated text
	text(145.35,-15.5,labels ='SRES A2',cex=3,adj=0)
dev.off()

#modify above plotting to create single for current
png(paste('goldbb_current.png',sep=''),width=8.5,height=12.75,units='cm',res=300,pointsize=6) #start the png file
	par(mar=c(2,2,0.5,0.5),cex=1,cex.axis=1.25) #se the parameters of hte plot
	#plot the maps
	image(current.asc,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the first map and add associated text & lat/lon info & legend
	axis(1,at=seq(145,147,0.5),labels=c(145,NA,146,NA,147),lwd=0,lwd.ticks=1); axis(2,at=seq(-15.5,-19.5,-0.5),labels=c(NA,-16,NA,-17,NA,-18,NA,-19,NA),lwd=0,lwd.ticks=1)
	legend.gradient(legend.pnts,col=cols, c(0,1), title='Suitability',cex=1.5)
dev.off()
