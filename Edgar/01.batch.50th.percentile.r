#get 50th percentile of future species data
#/scratch/jc155857/EdgarMaster/modelling/outputs/
#spp='1030'

library(SDMTools); library(maptools) #load the necessary libraries
wd = "/scratch/jc155857/EdgarMaster/modelling/outputs/"; setwd(wd)
spp.dir = paste(wd,spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory


tfiles = list.files(pattern='.asc') #get a list of the files
current=tfiles[1]
tfiles=list.files(pattern='RCP')
tt = strsplit(tfiles,'_') #string split the file names
ESs = GCMs = YEARs = NULL #set all to null
for (tval in tt) { 
jj=tval[3]; jj=strsplit(jj,'\\.')[[1]][1]
ESs = c(ESs,tval[1]); GCMs = c(GCMs,tval[2]); YEARs = c(YEARs, jj)} #extract all possible values
ESs = unique(ESs); GCMs = unique(GCMs); YEARs=unique(YEARs) #keep only unique

base.asc=read.asc(current); base.asc[which(is.finite(base.asc))]=0
pos=as.data.frame(which(is.finite(base.asc), arr.ind=TRUE)) #put all rows and cols for finite values as dataframe
pos$lat = getXYcoords(base.asc)$y[pos$col]; pos$lon = getXYcoords(base.asc)$x[pos$row] #append the lat lon




for (es in ESs){ cat(es, '\n')
		#how to batch out the RCPs???
				
		zz = file(paste('01',spp,'.',es,'.50th.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 #cat("R CMD BATCH --no-save --no-restore '--args .........RCP3PD &
		 cat("R CMD BATCH --no-save --no-restore '--args gcm=\"",gcm,"\" es=\"",es,"\" wd=\"",wd,"\" current=\"",current,"\" spp.dir=\"",spp.dir,"\" yr=\"",yr,"\" ' ~/scripts/Edgar/01.run.50th.percentile.r 01.",spp,'.',es,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n 01.', spp,'.',es,'.50th.sh',sep=''))
		
	}

