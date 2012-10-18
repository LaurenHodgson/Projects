setwd(species.dir)

pos.tmin = pos
pos.tmax = pos

#4.1 tmin
tmindb=as.data.frame(tmindb)
pos.tmin$total.temp=apply(tmindb,1,max)

#4.2.tmax
tmaxdb=as.data.frame(tmaxdb)
pos.tmax$total.temp=apply(tmaxdb,1,max)

save(pos.tmin, file=paste(species.dir,species,'.tmin.rData', sep=''))

save(pos.tmax, file=paste(species.dir,species,'.tmax.rData', sep=''))


#5.positions suitable both for min and max temp
copy.tmin=pos.tmin
copy.tmax=pos.tmax
pos.temp = copy.tmin

pos.temp[3]= copy.tmin[3]+ copy.tmax[3]
pos.temp$total.temp[which(is.na(pos.temp$total.temp))]=0
pos.temp[3]=pos.temp[3]/2



