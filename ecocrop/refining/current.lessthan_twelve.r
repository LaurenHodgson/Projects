#2.1.determine cells of dataframe within growing season Tmin
setwd(species.dir)
files=list.files()
files.of.interest=grep('tmin', files, value=TRUE)

load(file=files.of.interest)
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
pos.tmin = pos
pos.tmax = pos

files=list.files()
files.of.interest=grep('true',files, value=TRUE)
tmin=grep('tmin',files.of.interest,value=TRUE)
tmax=grep('tmax',files.of.interest,value=TRUE)

#4.1 tmin

load(tmin)
pos.tmin$current=apply(tout,1,max)
save(pos.tmin, file=paste(species.dir,species,'.tmin.rData', sep=''))

#4.2.tmax

load(tmax)
tt=NULL
pos.tmax$current=apply(tout,1,max)

save(pos.tmax, file=paste(species.dir,species,'.tmax.rData', sep=''))

#5.positions suitable both for min and max temp
copy.tmin=pos.tmin
copy.tmax=pos.tmax
pos.temp = pos

pos.temp$total.temp= copy.tmin$current+ copy.tmax$current

pos.temp$total.temp[which(pos.temp$total.temp<2)]=0
pos.temp$total.temp[which(pos.temp$total.temp==2)]=1


