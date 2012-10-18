#calculating mean areas for all gcms

###05.binned maps and barplots
library(SDMTools)
library(Hmisc)
esoclim.dir='/scratch/data/portlet/jc165798/Barra/future.esoclim/';
in.dir='/home/22/jc148322/Barra/temperature_outputs/'; setwd(in.dir)
out.dir = "/home/22/jc148322/Barra/temperature_outputs/area/" ;  #define & setwd to the output directory
script.dir="/home/22/jc148322/scripts/"
base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0
pos.table = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

############################

yr='2080';source(paste(script.dir,'area_map_table.r', sep=''))
yr='2050';source(paste(script.dir,'area_map_table.r', sep=''))
yr='2030';source(paste(script.dir,'area_map_table.r', sep=''))

