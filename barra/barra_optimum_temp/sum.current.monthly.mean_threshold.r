library(SDMTools,adehabitat)

work.dir = "H:/Barra Work Directory/current.esoclim/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/optimum_temp/"

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4'))(101)


sum.growth=NULL

for (ii in 1:12) { cat(ii,'\n')
		tasc.mean = read.asc(paste("mean.temp", sprintf('%02i',ii), ".asc", sep=''))
	out = tasc.mean
	too.hot= which(tasc.mean>40)
    too.cold = which(tasc.mean<21)
	july.min = which(tasc.mean<15)
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=0
	out[too.cold]=0
	out[july.min]=NA
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	if (length(sum.growth) == 0) { #if there are no values , populate it
          sum.growth = out
        } else { #if already has values, add the new values to it
          sum.growth = sum.growth + out
		 } 
}		 

sum.growth = sum.growth/12

png(paste(out.dir, 'big.sum.monthly.mean_threshold.png',sep=''), width=7, height=6, units='cm', res=300, pointsize=5, bg='white')
par(mar=c(0,1,0,1))
	image(tasc.mean, ann=FALSE,axes=FALSE, col="gray88")
	image(sum.growth, ann=FALSE,axes=FALSE, col=cols, add=TRUE)
legend.gradient(pnts,cols=cols, limits=round(range(out,na.rm=T)), title='Suitability (Growth Rate %/d)', cex=1)

dev.off()

#####write the asciis

library(SDMTools,adehabitat)

work.dir = "H:/Barra Work Directory/current.esoclim/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/2080_GCMs/"

sum.growth=NULL

for (ii in 1:12) { cat(ii,'\n')
		tasc.mean = read.asc(paste("mean.temp", sprintf('%02i',ii), ".asc", sep=''))
	out = tasc.mean
	too.hot= which(tasc.mean>40)
    too.cold = which(tasc.mean<21)
	july.min = which(tasc.mean<15)
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=0
	out[too.cold]=0
	out[july.min]=NA
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	if (length(sum.growth) == 0) { #if there are no values , populate it
          sum.growth = out
        } else { #if already has values, add the new values to it
          sum.growth = sum.growth + out
		 } 
}		 

sum.growth = sum.growth/12
write.asc.gz(sum.growth, paste(out.dir, "current.sum.growth", ".asc",sep=''))


