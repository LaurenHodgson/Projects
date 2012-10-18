#2.1.determine cells of dataframe within growing season Tmin
setwd(species.dir)
files=list.files()
files.of.interest=grep('tmin', files, value=TRUE)

load(file=files.of.interest)
tmindb=cbind(pos,tmindb)
tmindb=tmindb[order(tmindb$row,tmindb$col),]
tmindb=as.matrix(tmindb[,3:14])

tout=NULL
tout = tmindb; tout[,] = NA
for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+gmin)%%12)
	tout[,ii] = rowSums(tmindb[,cols.of.interest],na.rm=TRUE)
}
save(tout, file=paste(species.dir, 'gmin.', files.of.interest, sep=''))


#2.2.determine cells of dataframe within growing season Tmax
files=list.files()
files.of.interest=grep('tmax', files, value=TRUE)


load(file=files.of.interest)
tmaxdb=cbind(pos,tmaxdb)
tmaxdb=tmaxdb[order(tmaxdb$row,tmaxdb$col),]
tmaxdb=as.matrix(tmaxdb[,3:14])

tout=NULL
tout = tmaxdb; tout[,] = NA
for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+gmin)%%12)
	tout[,ii] = rowSums(tmaxdb[,cols.of.interest],na.rm=TRUE)
}
save(tout, file=paste(species.dir, 'gmin.', files.of.interest, sep=''))


#3.cellsuitability by growing season
files=list.files()
files.of.interest=grep('gmin',files, value=TRUE)

tout=NULL

for (tfile in files.of.interest) { cat(tfile,'\n')
load(tfile)
for (ii in 1:12) {
	tout[which(tout[,ii]<gmin),ii] = 0
	tout[which(tout[,ii]>=gmin),ii] = 1

}
save(tout, file=paste(species.dir, 'true.', tfile, sep=''))

}

#4.for one row/col, find max (has this pos ever been suitable for gmin?)
pos.tmin = pos#[order(pos$row,pos$col),]
pos.tmax = pos#[order(pos$row,pos$col),]

files=list.files()
files.of.interest=grep('true',files, value=TRUE)
tmin=grep('tmin',files.of.interest,value=TRUE)
tmax=grep('tmax',files.of.interest,value=TRUE)

#4.1 tmin

load(tmin)
tmin.months=tout
# pos.tmin$current=apply(tout,1,max)
# save(pos.tmin, file=paste(species.dir,species,'.tmin.rData', sep=''))

#4.2.tmax

load(tmax)
tmax.months=tout
# tt=NULL
# pos.tmax$current=apply(tout,1,max)

# save(pos.tmax, file=paste(species.dir,species,'.tmax.rData', sep=''))

#5.positions suitable both for min and max temp
pos.temp = pos[order(pos$row,pos$col),]

temp.copy=tmin.months+tmax.months
temp.copy[which(temp.copy<2)]=0
temp.copy[which(temp.copy==2)]=1
pos.temp$total.temp=apply(temp.copy,1,max)

# copy.tmin=pos.tmin
# copy.tmax=pos.tmax


# pos.temp$total.temp= copy.tmin$current+ copy.tmax$current

# pos.temp$total.temp[which(pos.temp$total.temp<2)]=0
# pos.temp$total.temp[which(pos.temp$total.temp==2)]=1

#############photoperiod growing season

colnames(ppdb)=NULL
colnames(temp.copy)=NULL
# ppdb.copy=temp.copy+ppdb
# ppdb.copy[which(ppdb.copy<2)]=NA
# ppdb.copy[which(ppdb.copy==2)]=1
# ppdb.copy[which(is.na(ppdb.copy))]=0

db.min.max.pp = tmindb + tmaxdb + ppdb
db.min.max.pp[which(db.min.max.pp==3)] = 1

ppout=NULL
ppout = db.min.max.pp; ppout[,] = NA
for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+gmin)%%12)
	ppout[,ii] = rowSums(db.min.max.pp[,cols.of.interest],na.rm=TRUE)
}
for (ii in 1:12) {
	ppout[which(ppout[,ii]<gmin),ii] = 0
	ppout[which(ppout[,ii]>=gmin),ii] = 1

}


pos.pp$total.pp = apply(ppout, 1, max) #change back to copy.pp


pp.asc = base.asc
pp.asc[cbind(pos.pp$row, pos.pp$col)] = pos.pp$total.pp
write.asc.gz(pp.asc,paste(species.dir, species, '.photoperiod.asc', sep=''))
pos.pp = pos.pp[,c('row','col','total.pp')]
save(pos.pp, file=paste(species.dir, species,'.pp.rData',sep=''))
