current.dir='/home/22/jc148322/current.esoclim/'
setwd(current.dir)

pos = pos.table #as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
out=NULL
for (ii in 1:12) { cat(ii,'\n')
	#cycle through run of gcm to calculate weight
	Tmean = (read.asc.gz(paste('tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) + 
		read.asc.gz(paste('tasmin',sprintf('%02i',ii),'.asc.gz',sep=''))) / 2 #read in tmin and tmax, and get tmean
	if (is.null(out)) { out = Tmean; out[which(is.finite(out))] = wt } #create the initial wt ascii raster
	RATE = rate.gain(out,Tmean) #this is a ascii grid of the rate gain
	RATE = log(1+RATE/out) #change the rate to a proportionate change and ln the value for exponential growth estimation
	out = out * exp(RATE * 30) #calculate the weight at the end of the 30 days
	out[which(Tmean<15)] = NA #set any temperature less than 15 to NA as barra will not grow there
	
}

#calculate min and max (remember: if NA is in the row, it will consider NA as max for that row)
current.asc = out
write.asc.gz(current.asc, paste(out.dir, yr,'.asc', sep=''))

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))

cols = colorRampPalette(c('red3','goldenrod2', 'forestgreen','dodgerblue4'))(100)
zlim=c(0,3500)

png(paste(out.dir, 'current.gradient.png',sep=''), width=10, height=9, units='cm', res=300, pointsize=5, bg='transparent')
par(mar=c(0,1,0,1))
	image(base.asc, ann=FALSE,axes=FALSE, col="gray88")
	image(current.asc, ann=FALSE,axes=FALSE, zlim=zlim, col=cols, add=TRUE)
legend.gradient(pnts,cols=cols, limits=c('Low', 'High'), title=paste('Current suitability (', round(min(out,na.rm=T)), '-', round(max(out,na.rm=T)), ')', sep=''),cex=2)

dev.off()

