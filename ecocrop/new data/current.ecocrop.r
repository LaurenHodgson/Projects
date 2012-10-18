###create species.dir

setwd(ecocrop.dir)
files=list.files()
files=grep(species, files ,value=TRUE)
species.dir = paste(ecocrop.dir,  species, '/', sep='')
if (length(files)==0){
 dir.create(species.dir)}
species.dir = paste(species.dir, yr, '/', sep='');dir.create(species.dir)

###determine photoperiod#################################
setwd(wd)

load(file='photoperiod.rData')
pos.pp=pos.pp[order(pos.pp$row,pos.pp$col),]

ppdb = as.matrix(pos.pp[5:16])
ppdb[which(ppdb<12)]=1
ppdb[which(ppdb>14)]=3
ppdb[which(ppdb>3)]=2
ppdb[which(ppdb<pp.min|ppdb >pp.max)] = NA
ppdb[which(is.finite(ppdb))]=1
save(ppdb, file=paste(species.dir, yr, '.photoperiod.rData',sep=''))


###create pos.temp#######################################

#1.1.determine cells of dataframe of 12 months within suitable Tmin
setwd(current.dir)

load(file='current.tmin.rData')
tmindb[which(tmindb<too.cold | tmindb>too.hot)]=NA
tmindb[which(is.finite(tmindb))]=1
save(tmindb, file=paste(species.dir, yr,'.tmin.rData', sep=''))

tmindb=cbind(pos,tmindb) #reorder tmindb
tmindb=tmindb[order(tmindb$row,tmindb$col),]
tmindb=as.matrix(tmindb[,3:14])

#1.2.determine cells of dataframe within suitable Tmax

load(file='current.tmax.rData')
tmaxdb[which(tmaxdb<too.cold | tmaxdb>too.hot)]=NA
tmaxdb[which(is.finite(tmaxdb))]=1
save(tmaxdb, file=paste(species.dir, yr, '.tmax.rData',sep=''))

tmaxdb=cbind(pos,tmaxdb) #reorder tmaxdb
tmaxdb=tmaxdb[order(tmaxdb$row,tmaxdb$col),]
tmaxdb=as.matrix(tmaxdb[,3:14])



setwd(species.dir)
if (species.data$LIFESPAN==1){

if (round(gmin)==12) {source(paste(script.dir,'current.twelve_months.r', sep=''))} 
if (gmin<11.5) {source(paste(script.dir,'current.lessthan_twelve.r', sep=''))}
}
if (species.data$LIFESPAN==2) {source(paste(script.dir,'current.twelve_months.r', sep=''))} 

#6.determine GCM run weight (will be used for rain also)

temp.asc=base.asc
temp.asc[cbind(pos.temp$row, pos.temp$col)] = pos.temp$total.temp
write.asc.gz(temp.asc, paste(species.dir, species, '.temp.asc', sep=''))

###rain#######################################################
load(file=paste(current.dir,'current.pr.rData',sep=''))
copy.rain=pos.rain
#1.cell suitability

copy.rain$current[which(copy.rain$current > too.wet)] = 0
copy.rain$current[which(copy.rain$current < too.dry)] = 0
copy.rain$current[which(copy.rain$current > 0)] = 1

save(copy.rain, file=paste(species.dir, species, '.pr', '.rData', sep=''))

pos.rain$total.rain = copy.rain$current

rain.asc=base.asc
rain.asc[cbind(pos.rain$row, pos.rain$col)] = pos.rain$total.rain
write.asc.gz(rain.asc, paste(species.dir, species, '.rain.asc', sep=''))


###ph#####################################################

load(file=paste(soil.dir,'soil.ph.rData',sep=''))

copy.ph=pos.ph[3]

copy.ph[which(copy.ph[,1]<ph.min),1] = 0
copy.ph[which(copy.ph[,1]>ph.max),1] = 0
copy.ph2=copy.ph
copy.ph[which(copy.ph[,1]>0),1] = 1

pos.ph$total.ph = copy.ph$PH
pos.ph = pos.ph[,c('row','col','total.ph')]

ph.asc = base.asc
ph.asc[cbind(pos.ph$row, pos.ph$col)] = copy.ph2$PH
write.asc.gz(ph.asc, paste(species.dir, species, '.ph.asc', sep=''))
save(pos.ph, file=paste(species.dir, species,'.ph.rData',sep=''))

###salinity##############################################

load(file=paste(soil.dir,'soil.sal.rData',sep=''))

copy.sal=pos.sal[6]

copy.sal[which(copy.sal[,1]<sal.min),1] = 0
copy.sal[which(copy.sal[,1]>sal.max),1] = 0
copy.sal2=copy.sal
copy.sal[which(copy.sal[,1]>0),1] = 1

pos.sal$total.sal = copy.sal$sal
pos.sal = pos.sal[,c('row','col','total.sal')]

sal.asc = base.asc
sal.asc[cbind(pos.sal$row, pos.sal$col)] = copy.sal2$sal
write.asc.gz(sal.asc,paste(species.dir, species, '.salinity.asc', sep=''))
save(pos.sal, file=paste(species.dir, species,'.sal.rData',sep=''))

###combine###############################################
pos.combine=NULL
pos.combine = merge(pos.sal,pos.temp,all=TRUE)
pos.combine = merge(pos.combine,pos.rain,all=TRUE)
pos.combine = merge(pos.combine,pos.ph,all=TRUE)
pos.combine = merge(pos.combine,pos.pp,all=TRUE)
pos.combine$total.temp=pos.combine$total.temp/2
pos.combine$total.rain=pos.combine$total.rain/2

copy.combine=pos.combine[3:ncol(pos.combine)]
for (ii in 1:ncol(copy.combine)){
	copy.combine[which(copy.combine[,ii]==0),ii] = NA
}

copy.combine$sum = rowSums(copy.combine[,c('total.sal','total.temp','total.rain','total.ph','total.pp')])
combine.asc = base.asc; combine.asc[cbind(pos.combine$row,pos.combine$col)] = copy.combine$sum

write.asc.gz(combine.asc, paste(species.dir, species, '.combine.asc', sep=''))
save(copy.combine, file=paste(species.dir,'combine.rData',sep=''))

#image############################################

cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'#003300')

phcols=c('gray90',colorRampPalette(c('red','yellow','forestgreen','blue','purple'))(100))
salcols=c('gray90',colorRampPalette(c('moccasin','lightpink','lightpink3','lightpink4'))(100))

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
png(paste(species.dir, '/', species, '.', yr, '.png',sep=''), width=21, height=14.5, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,3), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(temp.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=cols)
#text (130, -40, 'Temperature', cex=3)
legend.gradient(pnts,cols=cols, limits=c(0,1), title='Temperature', cex=2.2)

image(rain.asc, ann=FALSE,axes=FALSE, zlim=c(0,1), col=cols)
legend.gradient(pnts,cols=cols, limits=round(range(rain.asc,na.rm=TRUE)), title='Rain', cex=2.2)
#text (130, -40, 'Annual rainfall', cex=3)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(ph.asc, ann=FALSE,axes=FALSE, zlim=c(0,14), col=phcols, add=TRUE)
legend.gradient(pnts,cols=phcols, limits=c(0,14), title=paste('PH',' ','(',round(min(ph.asc,na.rm=TRUE),digits=2),'-',round(max(ph.asc,na.rm=TRUE),digits=2),')',sep=''), cex=2.2)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(sal.asc, ann=FALSE,axes=FALSE, col=salcols, add=TRUE)
legend.gradient(pnts,cols=salcols, limits=round(range(sal.asc,na.rm=TRUE)), title='Salinity', cex=2.2)

image(base.asc,ann=FALSE,axes=FALSE, col='gray90')
image(pp.asc, ann=FALSE,axes=FALSE, col=cols, add=TRUE)
legend.gradient(pnts,cols=cols, limits=round(range(pp.asc,na.rm=TRUE)), title='Photoperiod', cex=2.2)

#text (130, -40, 'PH', cex=3)
image(base.asc,ann=FALSE,axes=FALSE, col='gray90');
image(combine.asc, ann=FALSE,axes=FALSE, zlim=c(0,4), col='#003300', add=TRUE)
#text (130, -40, paste(species, ' ','Model', sep=''), cex=3)
legend.gradient(pnts, cols=cols,limits=c(0,4), title='Combined', cex=2.2)

title(main=paste(name, ' (',gsub('_',' ',species), ') ', 'Model', ' ', yr, sep=''), outer=T,line=4,cex.main=3, font.main=2)

dev.off()
