setwd(species.dir)

pos.tmin = pos
pos.tmax = pos

files=list.files()
tmin=grep('tmin',files,value=TRUE)
tmax=grep('tmax',files,value=TRUE)

#4.1 tmin
for(tfile in tmin) {cat(tfile,'\n')
load(tfile)
tmindb=as.data.frame(tmindb)
pos.tmin[tfile]=NULL
pos.tmin[tfile]=apply(tmindb,1,max)
}

#4.2.tmax
for(tfile in tmax) {cat(tfile,'\n')
load(tfile)
tmaxdb=as.data.frame(tmaxdb)
pos.tmax[tfile]=apply(tmaxdb,1,max)
}

save(pos.tmin, file=paste(species.dir,species,'.tmin.rData', sep=''))

save(pos.tmax, file=paste(species.dir,species,'.tmax.rData', sep=''))


#5.positions suitable both for min and max temp
copy.tmin=pos.tmin
copy.tmax=pos.tmax
pos.temp = copy.tmin
for (ii in 3:ncol(pos.temp)){cat(ii,'\n')
pos.temp[ii]= copy.tmin[ii]+ copy.tmax[ii]
pos.temp[which(is.na(pos.temp[,ii])),ii]=0
pos.temp[ii]=pos.temp[ii]/2

}

