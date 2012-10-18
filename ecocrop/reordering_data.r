library(SDMTools) 
library(dismo)

###define directories####################################
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir ="/home/jc148322/Ecocrop_output/"
tmin.dir = paste("/home/jc148322/flatdata/tmin/", yr,'/',sep='');
tmax.dir = paste("/home/jc148322/flatdata/tmax/", yr,'/',sep='');
pr.dir = "/home/jc148322/flatdata/pr/"
current.dir="/home/jc148322/flatdata/current/"
soil.dir = "/home/jc148322/flatdata/soil/"
script.dir="/home/jc148322/scripts/ecocrop/"
ecocrop.dir = "/home/jc148322/Ecocrop_output/absolute/"

wd = '/home/jc148322/flatdata/'; setwd(wd)

###base#################################################

base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0
pos= as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

#photoperiod

setwd(wd)

load(file='photoperiod.rData')
pos.pp=pos.pp[order(pos.pp$row,pos.pp$col),]

ppdb = as.matrix(pos.pp[5:16])
ppdb[which(ppdb<12)]=1
ppdb[which(ppdb>14)]=3
ppdb[which(ppdb>3)]=2

pos.pp=cbind(pos.pp[1:4],ppdb)

save(pos.pp,file='photoperiod.rData')

#soil
load(file=paste(soil.dir,'soil.sal.rData',sep=''))
pos.sal=pos.sal[order(pos.sal$row,pos.sal$col),]
save(pos.sal,file='soil.sal.rData')

#ph
load(file=paste(soil.dir,'soil.ph.rData',sep=''))
pos.ph=pos.ph[order(pos.ph$row,pos.ph$col),]
save(pos.ph,file='soil.ph.rData')

#rain current
load(file=paste(current.dir,'current.pr.rData',sep=''))
pos.rain=pos.rain[order(pos.rain$row,pos.rain$col),]
save(pos.rain,file='current.pr.rData')

#rain future
load(file=paste(pr.dir,'sresa1b.2066.2095.pr.rData',sep=''))
pos.rain=pos.rain[order(pos.rain$row,pos.rain$col),]
save(pos.rain,file='sresa1b.2066.2095.pr.rData')

#temp current
setwd(current.dir)

load(file='current.tmin.rData')
tmindb=cbind(pos,tmindb) #reorder tmindb
tmindb=tmindb[order(tmindb$row,tmindb$col),]
tmindb=as.matrix(tmindb[,3:14])
save(tmindb, file='current.tmin.rData')

load(file='current.tmax.rData')
tmaxdb=cbind(pos,tmaxdb) #reorder tmaxdb
tmaxdb=tmaxdb[order(tmaxdb$row,tmaxdb$col),]
tmaxdb=as.matrix(tmaxdb[,3:14])
save(tmaxdb, file='current.tmax.rData')

#temp future
setwd(tmax.dir)
files=list.files()

for (tfile in files){
	load(file=tfile)
	tmaxdb=cbind(pos,tmaxdb) #reorder tmaxdb
	tmaxdb=tmaxdb[order(tmaxdb$row,tmaxdb$col),]
	tmaxdb=as.matrix(tmaxdb[,3:14])
	save(tmaxdb, file=tfile)
	}
	
setwd(tmin.dir)
files=list.files()

for (tfile in files){
	load(file=tfile)
	tmindb=cbind(pos,tmindb) #reorder tmindb
	tmindb=tmindb[order(tmindb$row,tmindb$col),]
	tmindb=as.matrix(tmindb[,3:14])
	save(tmindb, file=tfile)
	}
	

