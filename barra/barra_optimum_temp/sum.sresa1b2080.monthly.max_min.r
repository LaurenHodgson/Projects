library(SDMTools)

work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"

###00.create mean.temp for each month

gcm = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 

sub.dir=NULL
for (tt in gcm) {cat(tt,'\n')
		sub.dir = paste(work.dir, paste('sresa1b', tt, 'run1.run1.2066.2095/', sep='.'), sep='')
		
setwd(sub.dir)
		
mean.temp=NULL

for (ii in 1:12) { cat(ii,'\n')
		tasc.max = read.asc.gz(paste("tasmax", sprintf('%02i',ii), ".asc.gz",sep=''))
		tasc.min = read.asc.gz(paste("tasmin", sprintf('%02i',ii), ".asc.gz",sep=''))
	mean.temp = tasc.max + tasc.min
	mean.temp = mean.temp/2
	
	#write the mean.temp ascis
	write.asc.gz(mean.temp, paste(sub.dir, "mean.temp", sprintf('%02i',ii), ".asc",sep=''))
}	
}

###01.create future annual suitability maps
library(SDMTools)
work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"
gcm = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4'))(101)

png(paste(out.dir, '8_gcms.mean_max_min.png',sep=''), width=28, height=13, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,4),mar=c(0,1,0,1), oma=c(0,3,3,0))
sub.dir=NULL

for (tt in gcm) {cat(tt,'\n')
		sub.dir = paste(work.dir, paste('sresa1b', tt, 'run1.run1.2066.2095/', sep='.'), sep='')
		
setwd(sub.dir)
		
sum.growth=NULL
zlim=c(0,6.5)
for (ii in 1:12) { cat(ii,'\n')
		tasc.mean = read.asc.gz(paste("mean.temp", sprintf('%02i',ii), ".asc.gz", sep=''))
		tasc.min = read.asc.gz(paste("tasmin", sprintf('%02i',ii), ".asc.gz", sep=''))
		tasc.max = read.asc.gz(paste("tasmax", sprintf('%02i',ii), ".asc.gz", sep=''))
	out = tasc.mean
	too.hot= which(tasc.max>40)
    too.cold = which(tasc.min<21)
	july.min = which(tasc.min<15)
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
	image(tasc.mean, ann=FALSE,axes=FALSE, col="gray88")
	image(sum.growth, ann=FALSE,axes=FALSE, zlim=zlim, col=cols, add=TRUE)
	text (130, -40, tt, cex=3)
	if(tt=='bccr_bcm2_0') {legend.gradient(pnts,cols=cols, limits=zlim, title='Suitability (Growth Rate %/d)', cex=2.5)}
}


dev.off()



