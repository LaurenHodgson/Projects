setwd(esoclim.dir)

files = list.files()
files.of.interest=grep(as.numeric(yr)-14,files, value=TRUE) #value=TRUE calls up full name of files
files.of.interest=grep("sresa1b",files.of.interest, value=TRUE)

#02. create function to determine weight
weight = function(files.of.interest) {
		sub.dir = paste(esoclim.dir, files.of.interest, '/',sep='')
		setwd(sub.dir)
out = NULL #define the basic output
#cycle through the months and estimate barra size
for (ii in 1:12) { cat(ii,'\n')
	Tmean = (read.asc.gz(paste(sub.dir,'tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) + 
		read.asc.gz(paste(sub.dir,'tasmin',sprintf('%02i',ii),'.asc.gz',sep=''))) / 2 #read in tmin and tmax, and get tmean
	if (is.null(out)) { out = Tmean; out[which(is.finite(out))] = wt } #create the initial wt ascii raster
	RATE = rate.gain(out,Tmean) #this is a ascii grid of the rate gain
	RATE = log(1+RATE/out) #change the rate to a proportionate change and ln the value for exponential growth estimation
	out = out * exp(RATE * 30) #calculate the weight at the end of the 30 days
	out[which(Tmean<15)] = NA #set any temperature less than 15 to NA as barra will not grow there
}
return(out)
}

#03. create table of row, col, weight for each run of each gcm


pos = pos.table #as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe


for (tfile in files.of.interest) { cat(tfile,'\n')
	#cycle through run of gcm to calculate weight
	tdata=weight(tfile)
	
	#add column to pos	
	pos[tfile] = tdata[cbind(pos$row,pos$col)]
}

save(pos, file=paste(out.dir, yr, 'weight.rData', sep=''))
#load(file=paste(out.dir, '2080weight.rData',sep=''))

#calculate min and max (remember: if NA is in the row, it will consider NA as max for that row)
pos$min = apply(pos[3:32], 1, min)


#make a copy of pos - to replace values with weighted values
tcopy=pos[3:32]
#determine number of runs of each gcm
GCMs = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 
GCM.count=NULL
for (gcm in GCMs){
	GCM.count=c(GCM.count, length(grep(gcm, names(tcopy))))
}
# determine 'weight' of each of the runs for each gcm
GCM.weight=1/GCM.count


for (ii in 1:ncol(tcopy)){ #for each column
	tcopy[which(is.na(tcopy[,ii])),ii] = 0 #set NA values to 0

	#apply weights to column
	tgcm=NULL; 
	for (gcm in GCMs){
		if (length(grep(gcm, names(tcopy)[ii]))>0) tgcm=gcm 
	}
	weighting=GCM.weight[which(GCMs==tgcm)] #find out the weighting for that GCM
	tcopy[,ii]=tcopy[,ii]*weighting #apply the weighting

}

tcopy$mean=rowSums(tcopy)/8
tcopy$mean[which(tcopy$mean==0)]=NA
tcopy$max = apply(tcopy[1:30], 1, max, na.rm=TRUE)
tcopy$max[which(tcopy$max==0)]=NA

save(tcopy, file=paste(out.dir, yr,'.weighted.mean.rData', sep=''))
#load(file=paste(out.dir, '2080.weighted.mean.rData', sep=''))

#make a copy of base.asc
min.asc = base.asc
max.asc = base.asc
mean.asc = base.asc

#add the data from the relevant columns to the asc and write as .asc
min.asc[cbind(pos$row, pos$col)] = pos$min
write.asc.gz(min.asc, paste(out.dir, yr,'.min.asc', sep=''))
max.asc[cbind(pos$row, pos$col)] = tcopy$max
write.asc.gz(max.asc, paste(out.dir, yr,'.max.asc', sep=''))
mean.asc[cbind(pos$row, pos$col)] = tcopy$mean
write.asc.gz(mean.asc, paste(out.dir, yr,'.mean.asc', sep=''))

##############################test image
cols = colorRampPalette(c('wheat3','wheat2','slategray2','steelblue3','steelblue4'))(101)
zlim=c(350,3500)

png(paste(out.dir, '/', yr,'.minmaxmean.trial.png',sep=''), width=21, height=7, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(1,3))
#min image
image(base.asc, ann=FALSE,axes=FALSE,col='gray90')
image(min.asc, ann=FALSE,axes=FALSE,zlim=zlim, col=cols,  add=TRUE)
#max image
image(base.asc, ann=FALSE,axes=FALSE,col='gray90')
image(max.asc, ann=FALSE,axes=FALSE,zlim=zlim, col=cols,  add=TRUE)
#mean image
image(base.asc, ann=FALSE,axes=FALSE,col='gray90')
image(mean.asc, ann=FALSE,axes=FALSE,zlim=zlim, col=cols,  add=TRUE)

dev.off()
