#get 50th percentile of future species data

#spp='u1a'

library(SDMTools); library(maptools) #load the necessary libraries
wd = "/home/jc165798/working/NARP_birds/"; setwd(wd)
spp.dir = paste(wd,'models/', gsub('([a-z,\\.])','',spp),'/output/ascii/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory
out.dir=paste('/home/jc148322/Bird_NARP/asciis/50th_percentile/',spp,'/',sep='');dir.create(out.dir)

tfiles = list.files() #get a list of the files
tt = strsplit(tfiles,'_') #string split the file names
ESs = GCMs = YEARs = NULL #set all to null
for (tval in tt) { 
jj=strsplit(tval[3],'\\.')[[1]][1]
ESs = c(ESs,tval[1]); GCMs = c(GCMs,tval[2]); YEARs = c(YEARs, jj)} #extract all possible values
ESs = unique(ESs); GCMs = as.vector(na.omit(unique(GCMs))); YEARs=as.vector(na.omit(unique(YEARs))) #keep only unique

current = ESs[grep('1990',ESs)] #get the current asciis
ESs = ESs[grep('RCP',ESs)] #remove current from ESs

base.asc=read.asc.gz(current); base.asc[which(is.finite(base.asc))]=0
pos=as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos$lat = getXYcoords(base.asc)$y[pos$col]; pos$lon = getXYcoords(base.asc)$x[pos$row] #append the lat lon


tdata=matrix(NA,nc=length(GCMs),nr=nrow(pos))
for (yy in YEARs){ cat(yy, '\n')
	for (es in ESs){ cat(es, '\n')
		for (ii in 1:length(GCMs)){ cat(ii, '\n')
			gcm=GCMs[ii]
			tasc=read.asc.gz(paste(es, '_', gcm, '_', yy, '.asc.gz',sep=''))
			tdata[,ii]=extract.data(cbind(pos$lon,pos$lat),tasc) 
		
		}
		out.quant=t(apply(tdata[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) }))
		out=base.asc; out[cbind(pos$row,pos$col)]=out.quant
		write.asc.gz(out,paste(out.dir,es,'_', yy,'_50thpercentile',sep=''))
	}
}
