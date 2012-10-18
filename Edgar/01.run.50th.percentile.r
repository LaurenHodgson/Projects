args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }

setwd(wd)
base.asc=read.asc.gz(current); base.asc[which(is.finite(base.asc))]=0
pos=as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos$lat = getXYcoords(base.asc)$y[pos$col]; pos$lon = getXYcoords(base.asc)$x[pos$row] #append the lat lon

for (yr in YEARs){ cat(yr, '\n')
	tdata=matrix(NA,nc=length(GCMs),nr=nrow(pos))
	for (ii in 1:length(GCMs)){ cat(ii, '\n')
			gcm=GCMs[ii]
			tasc=read.asc.gz(paste(es, '_', gcm, '_', yy, '.asc.gz',sep=''))
			tdata[,ii]=extract.data(cbind(pos$lon,pos$lat),tasc) 
		
		}
		out.quant=t(apply(tdata[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) }))
		out=base.asc; out[cbind(pos$row,pos$col)]=out.quant
		write.asc.gz(out,paste(spp.dir,es,'_', yr,'_50thpercentile',sep=''))
}