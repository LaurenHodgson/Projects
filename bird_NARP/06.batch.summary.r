
#drafted by Lauren Hodgson ( lhodgson86@gmail.com ...  )
#GNU General Public License .. feel free to use / distribute ... no warranties
#1km resolution
################################################################################
library(SDMTools)#load the necessary libraries
model.dir='models_1km' #or 'models'
resolution='1km' #or 5km
wd = paste("/home/jc165798/working/NARP_birds/",model.dir,"/",sep=''); setwd(wd) #define and set the working directory

species=list.files()

sh.dir='/home/jc148322/scripts/NARP_birds/summary.sh/';dir.create(sh.dir) #dir to write sh scripts to
for (spp in species){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste('06.',spp,'.summary.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" model.dir=\"",model.dir,"\" resolution=\"",resolution,"\" ' ~/scripts/NARP_birds/06.run.summary.r 06.",spp,'.summary.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -l nodes=2 -l pmem=2gb 06.',spp,'.summary.sh',sep=''))
}




#5km resolution - using data previously generated
################################################################################
library(SDMTools); library(maptools) #load the necessary libraries
model.dir='models' #or 'models'
resolution='5km' #or 5km
wd = paste("/home/jc165798/working/NARP_birds/",model.dir,"/",sep=''); setwd(wd) #define and set the working directory

species=list.files()
spp1k=list.files('/home/jc165798/working/NARP_birds/models_1km/')
species=setdiff(species,spp1k)

sh.dir='/home/jc148322/scripts/NARP_birds/summary.sh/';dir.create(sh.dir) #dir to write sh scripts to
for (spp in species[1301:length(species)]){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste('06.',spp,'.summary.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" model.dir=\"",model.dir,"\" resolution=\"",resolution,"\" ' ~/scripts/NARP_birds/06.tmp.r 06.",spp,'.summary.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n 06.',spp,'.summary.sh',sep=''))
}
