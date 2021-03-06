yr='current'

###load the necessary libraries
library(SDMTools) 
library(dismo)

###define directories####################################
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/home/31/jc165798 - jeremy's drive
#/scratch/data/portlet/jc165798
out.dir ="/home/22/jc148322/Ecocrop_output/"
tmin.dir = paste("/home/22/jc148322/flatdata/tmin/", yr,'/',sep='');
tmax.dir = paste("/home/22/jc148322/flatdata/tmax/", yr,'/',sep='');
pr.dir = "/home/22/jc148322/flatdata/pr/"
current.dir="/home/22/jc148322/flatdata/current/"
soil.dir = "/home/22/jc148322/flatdata/soil/"
script.dir="/home/22/jc148322/scripts/ecocrop/"
ecocrop.dir = "/home/22/jc148322/Ecocrop_output/"

wd = '/home/22/jc148322/flatdata/'; setwd(wd)

###base#################################################

base.asc = read.asc('base.asc') #read in the asc file
base.asc[which(is.finite(base.asc))] = 0
pos= as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))
###turn ecocrop character to values######################

cropdata=read.csv(paste(out.dir,'ecocropdata.csv',sep=''))
trialdata=cropdata[1:20,]
ECOcrops=trialdata

for (ii in 1:nrow(ECOcrops)) {cat(ii,'\n')

###define 'constants'####################################

species.data= ECOcrops[ii, ]
species=species.data$SCIENTNAME
species=gsub(' ','_',species)
name=species.data$NAME
too.hot = species.data$TOPMX
too.cold = species.data$TOPMN
too.wet = species.data$ROPMX
too.dry = species.data$ROPMN
gmin =species.data$GMIN/30.41667
gmax = species.data$GMAX/30.41667
ph.min=species.data$PHOPMN
ph.max=species.data$PHOPMX
sal.min =species.data$SALMIN
sal.max = species.data$SALMAX
if (gmin>11.5) {gmin=12}

source(paste(script.dir,'current.ecocrop.r', sep=''))
}

