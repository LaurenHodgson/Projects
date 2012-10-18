setwd(species.dir)


pos.temp = pos

tdb=tmindb+tmaxdb
tdb[which(tdb<2)]=0
tdb[which(tdb==2)]=1
tdb[which(is.na(tdb))]=0
pos.temp$total.temp=apply(tdb,1,min) #if it cannot grow one month, it cannot grow.


#############photoperiod growing season

colnames(ppdb)=NULL

db.min.max.pp = tmindb + tmaxdb + ppdb
db.min.max.pp[which(db.min.max.pp==3)] = 1
db.min.max.pp[which(is.na(db.min.max.pp))]=0

pos.pp$total.pp = apply(db.min.max.pp, 1, min) #if it cannot grow one month, it cannot grow.
pos.pp$total.pp[which(is.na(pos.pp$total.pp))]=0

pp.asc = base.asc
pp.asc[cbind(pos.pp$row, pos.pp$col)] = pos.pp$total.pp
write.asc.gz(pp.asc,paste(species.dir, species, '.photoperiod.asc', sep=''))
pos.pp = pos.pp[,c('row','col','total.pp')]
save(pos.pp, file=paste(species.dir, species,'.pp.rData',sep=''))
