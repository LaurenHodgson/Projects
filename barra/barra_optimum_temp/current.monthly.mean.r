library(SDMTools,adehabitat)

work.dir = "H:/Barra Work Directory/current.esoclim/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/optimum_temp/"

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = c('tan', colorRampPalette(c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4'))(101))
months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

png(paste(out.dir, 'monthly_opt_range.mean.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))

zlim=c(0, 6.5)

for (ii in 1:12) { cat(ii,'\n')
		tasc.mean = read.asc(paste("mean.temp", sprintf('%02i',ii), ".asc", sep=''))
	out = tasc.mean
	nil = tasc.mean
	too.hot = which(tasc.mean>40)
	too.cold = which(tasc.mean<21)
	no.growth = which (tasc.mean<21 & tasc.mean>=15)
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=NA
	out[too.cold]=NA
	nil[no.growth]=NA
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	image(tasc.mean, ann=FALSE,axes=FALSE,col="tan")
	image(nil, ann=FALSE,axes=FALSE,col="gray88", add=TRUE)
	image(out, ann=FALSE,axes=FALSE, zlim=zlim, col=cols, add=TRUE)
	text (130, -40, months[ii], cex=4)
	if (ii == 1) {legend.gradient(pnts,cols=cols, limits=zlim, title='Suitability', cex=3)}
}		 

dev.off()
