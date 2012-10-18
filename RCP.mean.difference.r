#calculating RCP mean annual temperature difference to current - for Australia
library(SDMTools)
data.dir='/home/jc165798/Climate/CIAS/Australia/5km/';setwd(data.dir)

current=read.asc.gz(paste(data.dir,'baseline.76to05/bioclim/bioclim_01.asc.gz', sep=''))
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE)

future.dir=paste(data.dir,'bioclim_asc/',sep='')

curmat=matrix(NA,nr=nrow(pos), nc=1)
curmat[,1]=current[which(is.finite(current))]

RCPs=c('RCP3PD','RCP45','RCP6','RCP85')
medians=matrix(NA, nr=nrow(pos),nc=length(RCPs))
t=1
for (rcp in RCPs) { cat(rcp, '\n')
	setwd(future.dir)
	files=list.files(pattern=rcp)
	files=grep('2085',files, value=TRUE)
	tdata=matrix(NA, nr=nrow(pos),nc=18)
	i=1
	for (tfile in files) { cat(tfile,'\n')
		wd=paste(future.dir, tfile, '/',sep=''); setwd(wd)
		tasc=read.asc.gz('bioclim_01.asc.gz')
		tdata[,i]=tasc[which(is.finite(tasc))]
		i=i+1
		}
	outquant= t(apply(tdata[,],1,function(x) { return(quantile(x,c(0.1,0.5),na.rm=TRUE,type=8)) })) #get the percentiles
	medians[,t]=outquant[,2]-curmat
	t=t+1
	}

RCPdiff=colMeans(medians,na.rm=TRUE)
