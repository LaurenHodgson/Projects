#2.1.determine cells of dataframe within growing season Tmin
files=list.files()
tmin=grep('tmin',files,value=TRUE)
tmax=grep('tmax',files,value=TRUE)

load(tmin);load(tmax)
tdb=(tmindb+tmaxdb)/2
tdb[which(is.na(tdb))]=0
		
tout = tmindb; tout[,] = NA
for (jj in 1:12) {
	cols.of.interest = 1+(jj:(jj+gmin)%%12)
	tout[,jj] = rowSums(tdb[,cols.of.interest],na.rm=TRUE)
}
		
tout[which(tout<gmin)]=0
tout[which(tout>=gmin)]=1
		
tt=strsplit(tmin,'.tmin')[[1]][1]
save(tout, file=paste(species.dir, tt, '.temp.rData', sep=''))
pos.temp = pos
pos.temp$total.temp=apply(tout,1,max)


#############photoperiod growing season

colnames(ppdb)=NULL

db.min.max.pp = tmindb + tmaxdb + ppdb
db.min.max.pp[which(db.min.max.pp==3)] = 1

ppout=NULL
ppout = db.min.max.pp; ppout[,] = NA
for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+gmin)%%12)
	ppout[,ii] = rowSums(db.min.max.pp[,cols.of.interest],na.rm=TRUE)
}

ppout[which(ppout<gmin)] = 0
ppout[which(ppout>=gmin)] = 1

pos.pp$total.pp = apply(ppout, 1, max) #change back to copy.pp

pp.asc = base.asc
pp.asc[cbind(pos.pp$row, pos.pp$col)] = pos.pp$total.pp
write.asc.gz(pp.asc,paste(species.dir, species, '.photoperiod.asc', sep=''))
pos.pp = pos.pp[,c('row','col','total.pp')]
save(pos.pp, file=paste(species.dir, species,'.pp.rData',sep=''))
