library(SDMTools)

################################################################################
###define some constants, equations & functions

#constants & equation from Glencross 2008
K = 0.539661; 		k = 0.4240000
x = -0.119917; 		y = 0.007434; 		z=-0.000119
rate.gain = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
	return((K + x*Temperature + y*Temperature^2 + z*Temperature^3) * liveweight^k)
}

wt = 20 #define the fish starting weight in grams

#directory locations
work.dir = "H:/Barra Work Directory/current.esoclim/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/glencross/"

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))

cols = colorRampPalette(c("#FF6666","#FFFF66","#99FF66","#66FFCC","#66CCFF","#9966FF","#FF66FF"))(100)
months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

png(paste(out.dir, 'current.monthly.gradient.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))

zlim=c(0.09,3500)
out=NULL

for (ii in c(9:12,1:8)) { cat(ii,'\n')
		Tmean = read.asc.gz(paste(work.dir,'mean.temp',sprintf('%02i',ii),'.asc.gz',sep='')) #read in tmin and tmax, and get tmean
	if (is.null(out)) { out = Tmean; out[which(is.finite(out))] = wt } #create the initial wt ascii raster
	RATE = rate.gain(out,Tmean) #this is a ascii grid of the rate gain
	RATE = log(1+RATE/out) #change the rate to a proportionate change and ln the value for exponential growth estimation
	out = out * exp(RATE * 30) #calculate the weight at the end of the 30 days
	out[which(Tmean<15)] = 0#set any temperature less than 15 to NA as barra will not grow there
	image(Tmean, ann=FALSE,axes=FALSE, col="gray88")
	image(out, ann=FALSE,axes=FALSE, zlim=zlim, col=cols, add=TRUE)
	text (130, -40, months[ii], cex=4)
    if (ii==1) {legend.gradient(pnts,cols=cols, limits=c(0,3500), title='Growth (577-2967) g/year', cex=3)}
}		 


dev.off()


