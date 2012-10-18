setwd(species.dir)

pos.temp = pos
pos.pp = pos

files=list.files()
tmin=grep('tmin',files,value=TRUE)
tmax=grep('tmax',files,value=TRUE)

for (ii in 1:length(tmin)){ cat(ii,'\n')
		load(tmin[ii]);load(tmax[ii])
		tdb=(tmindb+tmaxdb)/2
		tdb[which(is.na(tdb))]=0
		
		tout=NULL
		tout = tdb; tout[,] = NA
	for (jj in 1:12) {
			cols.of.interest = 1+(jj:(jj+gmin)%%12)
			tout[,jj] = rowSums(tdb[,cols.of.interest],na.rm=TRUE)
		}
		
		tout[which(tout<gmin)]=0
		tout[which(tout>=gmin)]=1
		
		tt=strsplit(tmin[ii],'.tmin')[[1]][1]
		save(tout, file=paste(species.dir, tt, '.temp.rData', sep=''))
		pos.temp[tt]=apply(tout,1,max)
		
		db.min.max.pp = (tmindb + tmaxdb + ppdb)/3
		db.min.max.pp[which(is.na(db.min.max.pp))]=0
		
		ppout=NULL
		ppout = db.min.max.pp; ppout[,] = NA
	for (jj in 1:12) {
			cols.of.interest = 1+(jj:(jj+gmin)%%12)
			ppout[,jj] = rowSums(db.min.max.pp[,cols.of.interest],na.rm=TRUE)
		}
		
		ppout[which(ppout<gmin)]=0
		ppout[which(ppout>=gmin)]=1
		
		pos.pp[tt]=apply(ppout,1,max)
}