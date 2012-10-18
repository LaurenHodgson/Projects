setwd(species.dir)


pos.temp = pos

temp.copy=tmindb+tmaxdb
temp.copy[which(temp.copy<2)]=0
temp.copy[which(temp.copy==2)]=1
temp.copy[which(is.na(temp.copy))]=0
pos.temp$total.temp=apply(temp.copy,1,min) #if it cannot grow one month, it cannot grow.


#############photoperiod growing season

colnames(ppdb)=NULL
colnames(temp.copy)=NULL


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
