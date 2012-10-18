
library(SDMTools)

fut.dir='/home/jc246980/DrySeason/Futuredat/Quantiles_delta/'; setwd(fut.dir)
image.dir='/home/jc148322/Hydrology.trials/images/dryseason/'; dir.create(image.dir)

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base ascii grid file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the base positions file

#######################################################
#Load and subset data
#load and subset fut severity data
load('delta.clust.severity_delta.Rdata') #outdelta
severity=outdelta
cois=grep('RCP85_2085',colnames(severity))
severity=severity[,cois]

#load and subset fut clust.length data
load('delta.max.clust.length_delta.Rdata') #outdelta
clust.length=outdelta
cois=grep('RCP85_2085',colnames(clust.length))
clust.length=clust.length[,cois]

#load and subset fut num.month data
load('delta.num.month_delta.Rdata') #outdelta
num.month=outdelta
cois=grep('RCP85_2085',colnames(num.month))
num.month=num.month[,cois]

#load and subset fut total severity data
load('delta.total.severity_delta.Rdata') #outdelta
tot.sev=outdelta
cois=grep('RCP85_2085',colnames(tot.sev))
tot.sev=tot.sev[,cois]

#######################################################
#make asciis
severity10=base.asc; severity10[cbind(pos$row,pos$col)]=severity[,1]
severity50=base.asc; severity50[cbind(pos$row,pos$col)]=severity[,2]
severity90=base.asc; severity90[cbind(pos$row,pos$col)]=severity[,3]

clust.length10=base.asc; clust.length10[cbind(pos$row,pos$col)]=clust.length[,1]
clust.length50=base.asc; clust.length50[cbind(pos$row,pos$col)]=clust.length[,2]
clust.length90=base.asc; clust.length90[cbind(pos$row,pos$col)]=clust.length[,3]

num.month10=base.asc; num.month10[cbind(pos$row,pos$col)]=num.month[,1]
num.month50=base.asc; num.month50[cbind(pos$row,pos$col)]=num.month[,2]
num.month90=base.asc; num.month90[cbind(pos$row,pos$col)]=num.month[,3]

tot.sev10=base.asc; tot.sev10[cbind(pos$row,pos$col)]=tot.sev[,1]
tot.sev50=base.asc; tot.sev50[cbind(pos$row,pos$col)]=tot.sev[,2]
tot.sev90=base.asc; tot.sev90[cbind(pos$row,pos$col)]=tot.sev[,3]

#######################################################
#determine appropriate limits
sevlims=range(severity,na.rm=T) # 0.09206906 1.97284286
sevlims=c(0.1,1.9)
clustlims=range(clust.length,na.rm=T) # -3.866667  5.633333
clustlims=c(-3.8,3.8)
monthlims=range(num.month,na.rm=T) #  -4.4  2.8
monthlims=c(-2.8,2.8)
totlims=range(tot.sev,na.rm=T) #  0.1257682 1.5349181
totlims=c(0.5,1.5)
#edit the asciis to suit the lims
severity10[which(severity10<sevlims[1])]=sevlims[1];severity10[which(severity10>sevlims[2])]=sevlims[2]
severity50[which(severity50<sevlims[1])]=sevlims[1];severity50[which(severity50>sevlims[2])]=sevlims[2]
severity90[which(severity90<sevlims[1])]=sevlims[1];severity90[which(severity90>sevlims[2])]=sevlims[2]

clust.length10[which(clust.length10<clustlims[1])]=clustlims[1];clust.length10[which(clust.length10>clustlims[2])]=clustlims[2]
clust.length50[which(clust.length50<clustlims[1])]=clustlims[1];clust.length50[which(clust.length50>clustlims[2])]=clustlims[2]
clust.length90[which(clust.length90<clustlims[1])]=clustlims[1];clust.length90[which(clust.length90>clustlims[2])]=clustlims[2]

num.month10[which(num.month10<monthlims[1])]=monthlims[1];num.month10[which(num.month10>monthlims[2])]=monthlims[2]
num.month50[which(num.month50<monthlims[1])]=monthlims[1];num.month50[which(num.month50>monthlims[2])]=monthlims[2]
num.month90[which(num.month90<monthlims[1])]=monthlims[1];num.month90[which(num.month90>monthlims[2])]=monthlims[2]

tot.sev10[which(tot.sev10<totlims[1])]=totlims[1];tot.sev10[which(tot.sev10>totlims[2])]=totlims[2]
tot.sev50[which(tot.sev50<totlims[1])]=totlims[1];tot.sev50[which(tot.sev50>totlims[2])]=totlims[2]
tot.sev90[which(tot.sev90<totlims[1])]=totlims[1];tot.sev90[which(tot.sev90>totlims[2])]=totlims[2]


#make images
setwd(image.dir)
cols = colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(21) #define the color ramp
rev.cols=cols[21:1]
pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5)) #define points for legend

png('dry_season.png',width=dim(base.asc)[1]*3+30, height=dim(base.asc)[2]*4+30, units='px', pointsize=30, bg='lightgrey')
	
	par(mfrow=c(4,3),mar=c(1,1,1,1), oma=c(3,3,3,0))

	#total severity
	image(tot.sev10, zlim=totlims, ann=FALSE,axes=FALSE,col=rev.cols)
		mtext('Total severity', line=1,  side=2, cex=2)
		mtext('10th percentile', line=1,  side=3, cex=1.7)
		legend.gradient(pnts,cols=rev.cols,limits=totlims, title='Total severity', cex=1.5)
		
	image(tot.sev50, zlim=totlims, ann=FALSE,axes=FALSE,col=rev.cols)
		mtext('50th percentile', line=1,  side=3, cex=1.7)
	image(tot.sev90, zlim=totlims, ann=FALSE,axes=FALSE,col=rev.cols)
		mtext('90th percentile', line=1,  side=3, cex=1.7)
		
	#num months
	image(num.month10, zlim=monthlims, ann=FALSE,axes=FALSE,col=cols)
		mtext('Dry season length', line=1,  side=2, cex=2)
		legend.gradient(pnts,cols=cols,limits=monthlims, title='# months', cex=1.5)
		
	image(num.month50, zlim=monthlims, ann=FALSE,axes=FALSE,col=cols)

	image(num.month90, zlim=monthlims, ann=FALSE,axes=FALSE,col=cols)
	
	#severity
	image(severity10, zlim=sevlims, ann=FALSE,axes=FALSE,col=rev.cols)
		mtext('Cluster severity', line=1,  side=2, cex=2)
		
		legend.gradient(pnts,cols=rev.cols,limits=sevlims, title='Severity', cex=1.5)
		
	image(severity50, zlim=sevlims, ann=FALSE,axes=FALSE,col=rev.cols)
	

	image(severity90, zlim=sevlims, ann=FALSE,axes=FALSE,col=rev.cols)

	
	#cluster
	image(clust.length10, zlim=clustlims, ann=FALSE,axes=FALSE,col=cols)
		mtext('Cluster length', line=1,  side=2, cex=2)
		legend.gradient(pnts,cols=cols,limits=clustlims, title='# months', cex=1.5)
		
	image(clust.length50, zlim=clustlims, ann=FALSE,axes=FALSE,col=cols)

	image(clust.length90, zlim=clustlims, ann=FALSE,axes=FALSE,col=cols)
		
dev.off()
