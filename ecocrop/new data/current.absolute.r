yr='current'

###load the necessary libraries
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
#pos=pos[order(pos$row,pos$col),]


###turn ecocrop character to values######################

cropdata=read.csv(paste(out.dir,'ecocrop_new_edit.csv',sep=''))
trialdata=cropdata[1:4,]
ECOcrops=trialdata

ii=2
#for (ii in 1:nrow(ECOcrops)) {cat(ii,'\n')

###define 'constants'####################################
pos= as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
pos.new=pos[order(pos$row,pos$col),]

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
too.hot = species.data$TMAX
too.cold = species.data$TMIN
too.wet = species.data$RMAX
too.dry = species.data$RMIN
gmin =species.data$GMIN/30.41667
gmax = species.data$GMAX/30.41667
ph.min=species.data$PHMIN
ph.max=species.data$PHMAX
sal.min =species.data$SALMIN
sal.max = species.data$SALMAX
pp.min = species.data$PPmin
pp.max = species.data$PPmax
if (gmin>11.5) {gmin=12}

if(is.na(too.hot | too.cold | too.wet | too.dry | gmin | gmax | ph.min | ph.max | sal.min | sal.max | pp.min | pp.max)) {stop()}


source(paste(script.dir,'current.ecocrop.r', sep=''))

landuse.asc = read.asc(paste(wd,'allyears.asc',sep='')) #read in the asc file

species.dir = paste("/home/jc148322/Ecocrop_output/absolute/",  species, '/', yr, '/', sep='')
setwd(species.dir)

base.asc = read.asc(paste(wd,'base.asc',sep=''))
combine.asc=read.asc.gz(paste(species, '.combine.asc.gz',sep=''))

pos= as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

pos$combine=combine.asc[cbind(pos$row,pos$col)]
pos$landuse=landuse.asc[cbind(pos$row,pos$col)]
pos$landuse[which(pos$landuse==0)]=1
pos$landuse[which(is.na(pos$landuse))]=0
pos$landuse[which(pos$landuse==1)]=NA
pos$total=rowSums(pos[3:4])
pos$total[which(is.na(pos$total))]=0


landuse.asc=base.asc
landuse.asc[cbind(pos$row,pos$col)]=pos$total
write.asc.gz(landuse.asc,'landuse.asc')

}

