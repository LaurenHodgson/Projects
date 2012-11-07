
library(SDMTools)
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) 
species=list.files('/home/jc165798/working/NARP_birds/models/')

for (spp in species){ cat(spp,'\n')

	spp.dir = paste('/home/jc165798/working/NARP_birds/models/',spp,'/output/ascii/',sep=''); setwd(spp.dir) #define the overarching species directory
	out.dir=paste("/home/jc148322/Bird_NARP/species.outputs/",spp,'/',sep='');dir.create(out.dir)

	
	files=list.files()

	pot.mat=matrix(NA, nr=nrow(pos),nc=length(files))
	columns=NULL
	i=1

	for (tfile in files) { cat(tfile, '\n')
		tasc=read.asc.gz(tfile)
		pot.mat[,i]=tasc[which(is.finite(tasc))]
		
		tt=strsplit(tfile,'\\.')[[1]][1]
		columns=c(columns, tt)
		i=i+1
		}
	colnames(pot.mat)=columns

	save(pot.mat, file=paste(out.dir,spp,'.potential.dist.mat.Rdata',sep=''))
}
	