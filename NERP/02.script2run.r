#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
##get the command line arguements
args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
 eval(parse(text=args[[i]]))
}

#need to have read in
# 
# spp='MTHORN'

################################################################################

library(SDMTools)

#function to plot min mean and max maps for a single emmission scenario & year
plot.minmeanmax = function(tmin,tmean,tmax,tfile,tyear,tes) { #tex is the emission scenario, tyear is the year of interest
	png(tfile,width=17,height=12.75,units='cm',res=150,pointsize=6) #start the png file
		par(oma=c(3,3,0.5,0.5),mar=c(0,0,0,0),cex=1,cex.axis=1.5) #se the parameters of hte plot
		mat = matrix(c(1,1,1,1,2,2,2,2,3,3,4,4,4,4,
					   1,1,1,1,2,2,2,2,3,3,3,3,3,3,
					   1,1,1,1,1,2,2,2,2,3,3,3,3,3,
					   1,1,1,1,1,1,2,2,2,2,3,3,3,3,
					   1,1,1,1,1,1,2,2,2,2,3,3,3,3),nr=5,byrow=TRUE)
		layout(mat) #define and setup the layout of the plot... 3 maps and one time bar
		#plot the maps
		image(tmin,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the first map and add associated text & lat/lon info & legend
		text(145.35,-15.5,labels ='Minimum',cex=3,adj=0)
		axis(1,at=seq(145,147,0.5),labels=c(145,NA,146,NA,147),lwd=0,lwd.ticks=1); axis(2,at=seq(-15.5,-19.5,-0.5),labels=c(NA,-16,NA,-17,NA,-18,NA,-19,NA),lwd=0,lwd.ticks=1)
		legend.gradient(legend.pnts,col=cols, c(0,1), title='suitability',cex=2)
		image(tmean,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the second map and add associated text
		text(145.35,-15.5,labels ='Mean',cex=3,adj=0)
		image(tmax,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the third map and add associated text
		text(145.35,-15.5,labels ='Maximum',cex=3,adj=0)
		#add the time bar
		par(mar=c(3,2,0,2)) #change the plot parameters for the time plot
		plot(1,1,xlim=c(0,10),ylim=c(0,1),type='n',axes=FALSE,ann=FALSE) #create an empty plot
		tval=10-((2080-tyear)/10) #work out what year should be as a bar
		polygon(c(0,0,tval,tval),c(0,0.25,0.25,0),col='black') #add the polygon to represent the year
		text(4.5,0.5,labels=tes,cex=2,font=2) #add text describing the emmision scenario
		axis(1,at=seq(1,10,3),labels=c(1990,2020,2050,2080),cex.axis=1.75) #add axes labels
		axis(1,at=seq(0,10,1),labels=F)	
	dev.off()
}

#function to plot means of the different emmission scenarios & year
plot.es.means = function(a1b,a2,b1,tfile,tyear) { #tex is the emission scenario, tyear is the year of interest
	png(tfile,width=17,height=12.75,units='cm',res=150,pointsize=6) #start the png file
		par(oma=c(3,3,0.5,0.5),mar=c(0,0,0,0),cex=1,cex.axis=1.5) #se the parameters of hte plot
		mat = matrix(c(1,1,1,1,2,2,2,2,3,3,4,4,4,4,
					   1,1,1,1,2,2,2,2,3,3,3,3,3,3,
					   1,1,1,1,1,2,2,2,2,3,3,3,3,3,
					   1,1,1,1,1,1,2,2,2,2,3,3,3,3,
					   1,1,1,1,1,1,2,2,2,2,3,3,3,3),nr=5,byrow=TRUE)
		layout(mat) #define and setup the layout of the plot... 3 maps and one time bar
		#plot the maps
		image(b1,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the first map and add associated text & lat/lon info & legend
		text(145.35,-15.5,labels ='SRES B1',cex=3,adj=0)
		axis(1,at=seq(145,147,0.5),labels=c(145,NA,146,NA,147),lwd=0,lwd.ticks=1); axis(2,at=seq(-15.5,-19.5,-0.5),labels=c(NA,-16,NA,-17,NA,-18,NA,-19,NA),lwd=0,lwd.ticks=1)
		legend.gradient(legend.pnts,col=cols, c(0,1), title='suitability',cex=2)
		image(a1b,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the second map and add associated text
		text(145.35,-15.5,labels ='SRES A1B',cex=3,adj=0)
		image(a2,axes=FALSE,ann=FALSE,zlim=c(0,1),col=cols) #image the third map and add associated text
		text(145.35,-15.5,labels ='SRES A2',cex=3,adj=0)
		#add the time bar
		par(mar=c(3,2,0,2)) #change the plot parameters for the time plot
		plot(1,1,xlim=c(0,10),ylim=c(0,1),type='n',axes=FALSE,ann=FALSE) #create an empty plot
		tval=10-((2080-tyear)/10) #work out what year should be as a bar
		polygon(c(0,0,tval,tval),c(0,0.25,0.25,0),col='black') #add the polygon to represent the year
		axis(1,at=seq(1,10,3),labels=c(1990,2020,2050,2080),cex.axis=1.75) #add axes labels
		axis(1,at=seq(0,10,1),labels=F)	
	dev.off()
}

################################################################################
#define & set the working directory
work.dir = paste('/data/jc165798/AWT.future.sdm/models/',spp,'/',sep=''); setwd(work.dir) 
out.dir=paste(work.dir,'summary/images/',sep=''); dir.create(out.dir)

#get the threshold
threshold = read.csv("output/maxentResults.csv",as.is=TRUE)$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[1]

#ssetup some plot parameters
bins = seq(0,1,length=101); bins = cut(threshold,bins,labels=FALSE) # get the threshold bin for cols
cols = c(rep('#E5E5E5',bins),colorRampPalette(c("tan","forestgreen"))(100)[bins:100])
legend.pnts = cbind(c(144.9,145.1,145.1,144.9),c(-19.5,-19.5,-18.75,-18.75))

## first plot the current
# for (type in c('potential','realized')) {
	# tasc = read.asc.gz(paste('summary/1990.',type,'.asc.gz',sep=''))
	## create the min mean max plots
	# plot.minmeanmax(tasc,tasc,tasc,paste(out.dir,type,'.sresa1b.1990.png',sep=''),1990,'SRES A1B')
	# plot.minmeanmax(tasc,tasc,tasc,paste(out.dir,type,'.sresa2.1990.png',sep=''),1990,'SRES A2')
	# plot.minmeanmax(tasc,tasc,tasc,paste(out.dir,type,'.sresb1.1990.png',sep=''),1990,'SRES B1')
	## plot the ES means
	# plot.es.means(tasc,tasc,tasc,paste(out.dir,type,'.ES.means.1990.png',sep=''),1990)
# }
# rm(tasc) ###free up memory

## cycle through the years
# for (YEAR in seq(2000,2080,10)) {
	# for (type in c('potential','realized')) {
		## read in the data
		# a1b.min = read.asc.gz(paste('summary/sresa1b.',YEAR,'.min.',type,'.asc.gz',sep=''))
		# a1b.mean = read.asc.gz(paste('summary/sresa1b.',YEAR,'.mean.',type,'.asc.gz',sep=''))
		# a1b.max = read.asc.gz(paste('summary/sresa1b.',YEAR,'.max.',type,'.asc.gz',sep=''))
		# a2.min = read.asc.gz(paste('summary/sresa2.',YEAR,'.min.',type,'.asc.gz',sep=''))
		# a2.mean = read.asc.gz(paste('summary/sresa2.',YEAR,'.mean.',type,'.asc.gz',sep=''))
		# a2.max = read.asc.gz(paste('summary/sresa2.',YEAR,'.max.',type,'.asc.gz',sep=''))
		# b1.min = read.asc.gz(paste('summary/sresb1.',YEAR,'.min.',type,'.asc.gz',sep=''))
		# b1.mean = read.asc.gz(paste('summary/sresb1.',YEAR,'.mean.',type,'.asc.gz',sep=''))
		# b1.max = read.asc.gz(paste('summary/sresb1.',YEAR,'.max.',type,'.asc.gz',sep=''))
		## create the min mean max plots
		# plot.minmeanmax(a1b.min,a1b.mean,a1b.max,paste(out.dir,type,'.sresa1b.',YEAR,'.png',sep=''),YEAR,'SRES A1B')
		# plot.minmeanmax(a2.min,a2.mean,a2.max,paste(out.dir,type,'.sresa2.',YEAR,'.png',sep=''),YEAR,'SRES A2')
		# plot.minmeanmax(b1.min,b1.mean,b1.max,paste(out.dir,type,'.sresb1.',YEAR,'.png',sep=''),YEAR,'SRES B1')
		## plot the ES means
		# plot.es.means(a1b.mean,a2.mean,b1.mean,paste(out.dir,type,'.ES.means.',YEAR,'.png',sep=''),YEAR)
	# }
# }

#now create summary images based on abundance, area, Istat, num patches, mean perimeter area ratio, aggregation index for each ES
#define plot generics
cols = c('#FF0000','#228B22','#000080') #define the line colors -- colors align with IPCC fig 5 ...
cols.fill = paste(cols,'50',sep='') #define the polygon fill colors
vois = c('prop.area','prop.abund','Istat','n.patches','mean.perim.area.ratio','aggregation.index') #define the variables of interest



############lauren addition
setwd(paste(base.dir,spp,sep=''))
futs = list.files('output/',pattern='\\.asc\\.gz',recursive=TRUE,full.names=TRUE); futs=gsub('//','/',futs)
varnames = gsub('output/','',futs); varnames = gsub('\\.asc\\.gz','',varnames)

#extract ES, GCM, year information
ESs = GCMs = YEARs = current = NULL
for (ii in 1:length(varnames)) { 
	tt = strsplit(varnames[ii],'\\_')[[1]]
	if (length(tt)==1) { current = tt[1] } else { ESs = c(ESs,tt[1]); GCMs = c(GCMs,tt[2]); YEARs = c(YEARs,tt[3]) }
}
ESs = unique(ESs); GCMs = unique(GCMs); YEARs = unique(YEARs)
#################replaces:
#ESs = c('sresa2','sresa1b','sresb1') #define the emission scenarios




tdata = NULL #setup the temporary dataset
for (ES in ESs) { tdata = rbind(tdata, read.csv(paste('summary/',ES,'.summary.data.csv',sep=''),as.is=TRUE)) } #read in and append the data
tdata.realized = tdata[which(tdata$dist.type=='realized'),] #keep realized data for summarizing Istat
tdata = tdata[which(tdata$dist.type=='realized.NO.small.patches'),] #only need to summarize realized data ... only for realized with small patches removed
if (length(which(tdata.realized[,2:6] != tdata[,2:6]))<1) { tdata$Istat=tdata.realized$Istat } #append the Istat informaiton
diffs = 1-(tdata[1,7:46] / tdata.realized[1,7:46]) #get the difference for removing small patches
tdata$prop.area = tdata$total.area/tdata$total.area[1] #define the proportion change in area
es.gcm.yr = aggregate(tdata[,vois],by=list(ES=tdata$ES,GCM=tdata$GCM,year=tdata$year),function(x) { return(mean(x,na.rm=TRUE)) } ) #get the means for es, gcm & year (to avoid bias for inidividual realizations)
es.yr.mean = aggregate(es.gcm.yr[,vois],by=list(ES=es.gcm.yr$ES,year=es.gcm.yr$year),function(x) { return(mean(x,na.rm=TRUE)) } ) #get the means for ES & year	
es.yr.sd = aggregate(es.gcm.yr[,vois],by=list(ES=es.gcm.yr$ES,year=es.gcm.yr$year),function(x) { return(sd(x,na.rm=TRUE)) } ) #get the SD for the ES & year
es.yr.min = es.yr.max = es.yr.mean; #get the min & max data associated with +- 1 SD
es.yr.min[,vois] = es.yr.min[,vois] - es.yr.sd[,vois] #get the min & max data associated with +- 1 SD
es.yr.max[,vois] = es.yr.max[,vois] + es.yr.sd[,vois] #get the min & max data associated with +- 1 SD
es.yr.mean = rbind(tdata[which(tdata$year==1990)[1],names(es.yr.mean)],es.yr.mean) #append the 1990 current data

#define a plot function for summary information
tplot = function(y.limits,tmain,tylab,voi,tsub=NULL) {
	plot(c(1990,2080),y.limits,ylab=tylab,xlab='year',type='n',axes=F,main=tmain,sub=tsub) #create base plot
	axis(2); axis(1,at=seq(1990,2080,10),labels=c(1990,NA,NA,2020,NA,NA,2050,NA,NA,2080)) #add the axes
	for (ii in length(ESs):1) { #add the polygons for each ES
		tpos = which(es.yr.min$ES==ESs[ii])
		polygon(c(1990,es.yr.min$year[tpos],es.yr.min$year[tpos[length(tpos):1]],1990),c(es.yr.mean[1,voi],es.yr.min[tpos,voi],es.yr.max[tpos[length(tpos):1],voi],es.yr.mean[1,voi]),col=cols.fill[ii],border=NA)
	}
	for (ii in length(ESs):1) { #add the mean lines for each ES
		tpos = which(es.yr.mean$ES==ESs[ii])
		lines(es.yr.mean$year[c(1,tpos)],es.yr.mean[c(1,tpos),voi],col=cols[ii],lwd=1)
	}
}


#create the summary plots
png(paste(out.dir,"basic.summaries.png",sep=''), width=12, height=8, units="cm", pointsize=4, res=300)
	par(mfrow=c(2,3),cex=1,cex.main=1.3)
	tplot(c(0,max(c(es.yr.max$prop.area,es.yr.max$prop.abund,1),na.rm=T)),'Distribution Area','Proportion','prop.area',paste(round(diffs$total.area*100,1),'% area removed in small patches',se='')) #plot the change in distribution area
	legend('bottomleft',legend=toupper(gsub('sres','sres ',ESs)),col=cols,lty=1,lwd=2,bty='n',cex=1.1)
	tplot(c(0,max(c(es.yr.max$prop.abund,es.yr.max$prop.area,1),na.rm=T)),'Abundance','Proportion','prop.abund',paste(round(diffs$sum.suitability*100,1),'% abundance removed in small patches',se='')) #plot the change in abundance
	tplot(c(0,1),'I similarity statistic','I value','Istat') #plot the change in I statistic
	tplot(range(c(es.yr.max$n.patches,es.yr.min$n.patches),na.rm=T),'Number of Patches','Count','n.patches',paste(round(diffs$n.patches*100,1),'% small patches removed',se='')) #plot the change in number of patches
	tplot(range(c(es.yr.max$mean.perim.area.ratio,es.yr.min$mean.perim.area.ratio),na.rm=T),'Perimeter Area Ratio','Mean ratio','mean.perim.area.ratio') #plot the change in perimeter area ratio
	tplot(range(c(es.yr.max$aggregation.index,es.yr.min$aggregation.index),na.rm=T),'Aggregation Index','Per cent','aggregation.index') #plot the change in aggregation index
dev.off()

#create a dataframe and write it out summarizing this species status through time based on abundance
out=data.frame(spp=spp,ES=es.yr.mean$ES,year=es.yr.mean$year,prop.abund.mean=es.yr.mean$prop.abund,prop.abund.minus1SD=c(1,es.yr.min$prop.abund),prop.abund.plus1SD=c(1,es.yr.max$prop.abund))
write.csv(out,paste(work.dir,'summary/IUCN.summary.data.csv',sep=''),row.names=FALSE)


