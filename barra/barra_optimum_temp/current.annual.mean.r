library(SDMTools, adehabetat)

work.dir = "H:/Barra Work Directory/bioclim/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/optimum_temp/"

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = c('tan', colorRampPalette(c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4'))(101))

png(paste(out.dir, 'opt_range.annual.mean.png',sep=''), width=7, height=6, units='cm', res=300, pointsize=5, bg='white')

		tasc.mean = read.asc.gz("bioclim_01.asc.gz")
		tasc.mean = tasc.mean/10
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
	
	par(mar=c(0,1,0,0))
	image(tasc.mean, ann=FALSE,axes=FALSE,col="tan")
	image(nil, ann=FALSE,axes=FALSE,col="gray88", add=TRUE)
	image(out, ann=FALSE,axes=FALSE, col=cols, add=TRUE)

	legend.gradient(pnts,cols=cols, limits=round(range(out, na.rm=T)), title='Suitability', cex=1)		 

dev.off()
