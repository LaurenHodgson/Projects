necessary=c("adehabitat")
#check if library is installed
installed = necessary %in% installed.packages()
#if library is not installed, install it
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T)

#load the libraries
for (lib in necessary) library(lib,character.only=T)

library(SDMTools)

work.dir = "H:/Barra Work Directory/bioclim/"; setwd(work.dir)

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('darkslategrey','paleturquoise4','paleturquoise'))(101)

png(paste('barra_opt_range.annual-quart.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')


		tasc.max = read.asc.gz("bioclim_10.asc.gz")
		tasc.min = read.asc.gz("bioclim_11.asc.gz")
		tasc.mean = read.asc.gz("bioclim_01.asc.gz")
	out = tasc.mean
	too.hot = which(tasc.max>400)
	too.cold = which(tasc.min<210)
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=NA
	out[too.cold]=NA
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	image(tasc.mean, ann=FALSE,axes=FALSE,col="gray88")
	image(out, ann=FALSE,axes=FALSE, col=cols, add=TRUE)

	legend.gradient(pnts,cols=cols, limits=round(range(out, na.rm=T)), title='Suitability', cex=1)		 

dev.off()
