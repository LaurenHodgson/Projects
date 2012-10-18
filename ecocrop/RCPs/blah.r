library(SDMTools)

yr='2085'
wd='/home/jc165798/Climate/CIAS/Australia/5km/monthly_csv/RCP85/'
tmin.dir='/home/jc148322/flatdata/tmin/2085/';dir.create(tmin.dir)
tmax.dir='/home/jc148322/flatdata/tmax/2085/';dir.create(tmax.dir)
pr.dir='/home/jc148322/flatdata/pr/'

DIRS=list.files(wd)

for (gcm in DIRS){ cat(gcm, '\n')
	sub.dir=paste(wd, gcm,'/',sep='')
	setwd(sub.dir)

	tmindb=read.csv('tmn.matrix.csv')
	cois=grep('tmn2085',colnames(tmindb))
	tmindb=tmindb[,cois]
	save(tmindb, file=paste(tmin.dir, gcm, '.', yr, '.tmin.rData',sep=''))
	
	tmaxdb=read.csv('tmx.matrix.csv')
	cois=grep('tmx2085',colnames(tmaxdb))
	tmaxdb=tmaxdb[,cois]
	save(tmaxdb, file=paste(tmax.dir, gcm, '.', yr, '.tmax.rData',sep=''))
	
	
}
prdb=NULL
for (gcm in DIRS[2:length(DIRS)]){ cat(gcm, '\n')
	sub.dir=paste(wd, gcm,'/',sep='')
	setwd(sub.dir)
	
	tdata=read.csv('pre.matrix.csv')
	cois=grep('pre2085',colnames(tdata))
	tdata=tdata[,cois]
	tt=rowSums(tdata,na.rm=T)
	prdb=cbind(prdb, tt)
}
save(prdb, file=paste(tmax.dir, 'pr.', yr, '.rData',sep=''))
