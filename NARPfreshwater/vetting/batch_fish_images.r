
#drafted by Lauren Hodgson ( lhodgson86@gmail.com ...  )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools); #load the necessary libraries

species.dir="/home/jc246980/NorthernOZFish/Data/"
species = list.files(species.dir)
species=gsub('.csv','',species)
species=species[1:10]

sh.dir='/home/jc148322/scripts/NARP_freshwater/images.sh/';dir.create(sh.dir,recursive=t) #dir to write sh scripts to
for (spp in species){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste(spp,'.images.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" ' ~/scripts/NARP_freshwater/run.northern_oz_fish.r ",spp,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n ',spp,'.images.sh',sep=''))
}
