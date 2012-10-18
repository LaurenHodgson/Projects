#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################
#set some constants

#set the working dir and output dir
work.dir = '/home/jc165798/working/AWT.NERP/future.SDM/models/'; setwd(work.dir)
script.file = '/home/jc148322/scripts/NERP/01.script2run.r'
################################################################################
#do something

#get a list of all the species
species = list.files()

for (spp in species) {
	#create a folder for the species
	spp.folder = paste(work.dir,spp,"/",sep=""); setwd(spp.folder)
	
	spp.arg = paste('spp="',spp,'" ',sep='')
	base.dir.arg = paste('base.dir="',work.dir,'" ',sep='')
	
	##create the sh file
	zz = file(paste('03.summarize.',spp,'.sh',sep=''),'w')
		cat('##################################\n',file=zz)
		cat('#!/bin/sh\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat("R CMD BATCH '--args ",spp.arg,base.dir.arg,"' ",script.file,' 03.summarize.Rout --no-save \n',sep='',file=zz)
		cat('##################################\n',file=zz)
	close(zz)
			
	#submit the job
	system(paste('qsub -m n -l nodes=1:ppn=2:V20Z 03.summarize.',spp,'.sh',sep=''))
}
	