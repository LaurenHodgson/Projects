#script was written by Jeremy VanDerWal (jjvanderwal@gmail.com)

################################################################################
##get the command line arguements
# args=(commandArgs(TRUE))

#evaluate the arguments
# for(i in 1:length(args)) {
 # eval(parse(text=args[[i]]))
# }

#need to have read in
# base.dir='/home/jc165798/working/AWT.NERP/future.SDM/models/'
# spp='ABT'
# spp='TBBB'
################################################################################

library(SDMTools)

################################################################################
#define the output directory & create it
lauren.dir = '/home/jc148322/NERP/'
out.dir = paste(lauren.dir,spp,'/summary/',sep=''); dir.create(out.dir,recursive=TRUE)

#read in the projections 
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

#get the threshold
threshold = read.csv("output/maxentResults.csv",as.is=TRUE)$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[1]

#define the basic output asc
base.asc = read.asc.gz(futs[1]); base.asc[which(is.finite(base.asc))] = 0
pos = as.data.frame(which(is.finite(base.asc),arr.ind=TRUE))
pos$lat = getXYcoords(base.asc)$y[pos$col] #extract the longitudes
pos$lon = getXYcoords(base.asc)$x[pos$row] #extract the latitudes

#read in and append subregions ... convert subregion to a mask of realized distribution matrix
distmat = read.csv(paste(gsub('models','',base.dir),'RealizedDistMatrix.csv',sep=''),as.is=TRUE)
distmat = distmat[which(distmat$spp_code_raster==spp),]; distmat = names(distmat)[which(distmat[1,]==1)]; distmat = gsub('\\.','-',distmat)
subregionID = read.csv(paste(gsub('models','base.data',base.dir),'subregions_new07.csv',sep=''),as.is=TRUE)
subregionID = subregionID$Value[which(subregionID$Abbreviation %in% distmat)]
pos$subregion = extract.data(cbind(pos$lon,pos$lat),read.asc.gz(paste(gsub('models','base.data',base.dir),'subregions_new07.asc.gz',sep='')))
pos$subregion[-which(pos$subregion %in% c(subregionID,-99))] = 0; pos$subregion[which(pos$subregion > 0)] = 1
pos$area = (grid.area(base.asc)[cbind(pos$row,pos$col)]) / 10000 #get out the area in ha of each cell of data

#read and append DEM
pos$dem = extract.data(cbind(pos$lon,pos$lat),read.asc.gz(paste(gsub('models','base.data',base.dir),'dem250.asc.gz',sep='')))

#write out the basic mask & pos
write.asc.gz(base.asc,paste(out.dir,'base.asc',sep=''))
write.csv(pos,paste(out.dir,'base.pos.csv',sep=''),row.names=FALSE)
tasc = base.asc; tasc[cbind(pos$row,pos$col)] = pos$subregion
write.asc.gz(tasc,paste(out.dir,'clip.realized.asc',sep='')) #wrie out the clipping grid

#define the function to summarize distdata
sum.data = function() { #this is a function to summarize the distribution data
	outdata1 = data.frame(ES=rep(NA,length(vois)),GCM=NA,C20run=NA,C21run=NA,year=NA) #define the basic information
	for (ii in 1:length(vois)) { 
		tt = strsplit(vois[ii],'\\.')[[1]] 
		if (length(tt)==1) { outdata1[ii,1:5] = c(NA,NA,NA,NA,tt[1]) } else { outdata1[ii,1:5] = c(tt[1],tt[2],tt[3],tt[4],tt[5]) }
	}
	outdata1$sum.suitability = colSums(distdata*pos[,'area'],na.rm=TRUE) #get the sum of the environmental suitability
	outdata1$prop.abund = outdata1$sum.suitability / outdata1$sum.suitability[outdata1$year==1990] #calculate proportionate change in abundance
	#calculate the Class-based statistics and Istat, then append to the columns
	cur.asc = base.asc; cur.asc[cbind(pos$row,pos$col)] = distdata[,which(colnames(distdata)=="1990")] #define the current data surface for estimating Istat
	for (ii in 1:nrow(outdata1)) { 
		tasc = base.asc; tasc[cbind(pos$row,pos$col)] = distdata[,ii] #put the data back into a matrix
		Ival = Istat(cur.asc,tasc) #calculate the Istatistic
		tasc[which(tasc>0)] = 1 #now convert binary data
		CS = ClassStat(tasc,latlon=TRUE) #get the class stats
		if (1%in%CS$class) { CS = CS[which(CS$class==1),] } else { CS = CS[1,]; CS[1,] = NA } #only keep info on distriubtion... if no distriubtion, set everything to 0
		if (ii == 1) { cois = NULL; for (jj in c('Istat',names(CS)[-1])) {outdata1[jj] = NA; cois = c(cois,which(names(outdata1)==jj)) } } #if the first summary, create columns to store data and define the column numbers for this data
		outdata1[ii,cois] = c(Ival,CS[,-1])
	}
	return(outdata1)#return the output
}
#define the function to summarize distdata removing small fragments
sum.data.remove.small.patches = function() { #this is a function to summarize the distribution data
	outdata1 = data.frame(ES=rep(NA,length(vois)),GCM=NA,C20run=NA,C21run=NA,year=NA,sum.suitability=NA,prop.abund=NA) #define the basic information
	for (ii in 1:length(vois)) { 
		tt = strsplit(vois[ii],'\\.')[[1]] 
		if (length(tt)==1) { outdata1[ii,1:5] = c(NA,NA,NA,NA,tt[1]) } else { outdata1[ii,1:5] = c(tt[1],tt[2],tt[3],tt[4],tt[5]) }
	}
	#calculate the Class-based statistics and Istat, then append to the columns
	for (ii in 1:nrow(outdata1)) { 
		tasc = base.asc; tasc[cbind(pos$row,pos$col)] = distdata[,ii] #put the data back into a matrix
		patches = tasc; patches[which(patches>0)] = 1; patches = ConnCompLabel(patches)[cbind(pos$row,pos$col)] #get the unique patches 
		patches.ag = aggregate(distdata[,ii]*pos$area,by=list(patch=patches),sum); patches.ag$prop = patches.ag$x/sum(patches.ag$x) #get the patch ES weighted by area and then get proportion
		patches[which(patches %in% patches.ag$patch[which(patches.ag$prop<0.0001)])] = 0; patches[which(patches>0)] = 1 #make patches binary, keeping only patches that contribute more than 0.01% of the total ES
		tasc[cbind(pos$row,pos$col)] = tasc[cbind(pos$row,pos$col)] * patches #filter out small patches
		outdata1$sum.suitability[ii] = sum(distdata[,ii]*pos$area*patches,na.rm=TRUE) #get the sum of the environmental suitability
		outdata1$prop.abund[ii] = outdata1$sum.suitability[ii] / outdata1$sum.suitability[outdata1$year==1990] #calculate proportionate change in abundance
			
		Ival = NA #calculate the Istatistic
		tasc[which(tasc>0)] = 1 #now convert binary data
		CS = ClassStat(tasc,latlon=TRUE) #get the class stats
		if (1%in%CS$class) { CS = CS[which(CS$class==1),] } else { CS = CS[1,]; CS[1,] = NA } #only keep info on distriubtion... if no distriubtion, set everything to 0
		if (ii == 1) { cois = NULL; for (jj in c('Istat',names(CS)[-1])) {outdata1[jj] = NA; cois = c(cois,which(names(outdata1)==jj)) } } #if the first summary, create columns to store data and define the column numbers for this data
		outdata1[ii,cois] = c(Ival,CS[,-1])
	}
	return(outdata1)#return the output
}


#cycle through each of the emmissiton scenarios & create a matrix of data representing the full sets of data
for (ES in ESs) { cat(ES,'...')
	vois = c('current.76to05',varnames[grep(ES,varnames)]) #define the variables of interest for this ES
	#create or load distdata from the model outputs
	distdata = matrix(0.0,nr=nrow(pos),nc=length(vois)) #create a matrix of data that contains all of the projections
	colnames(distdata) = vois #set the column names
	for (ii in 1:length(vois)) { if (ii%%5==0) { cat(round(ii/length(vois)*100,1),'% ... ') }
		distdata[,ii] = read.asc.gz(paste('output/',vois[ii],'.asc.gz',sep=''))[cbind(pos$row,pos$col)] #read / append data to the dataframe
	}
	#first the potential distribution
	distdata[which(distdata<threshold)] = 0 #set anything less than threshold to be 0
	colnames(distdata)[1]=1990
	#extract the output summary data
	outdata = data.frame(dist.type='potential',sum.data())
	outdata = rbind(outdata,data.frame(dist.type='potential.NO.small.patches',sum.data.remove.small.patches()))
	#create min, mean & max spatial predictions
	YEARS = unique(outdata$year); YEARS = YEARS[-which(YEARS=='1990')]
	GCMS = unique(outdata$GCM)
	if (ES==ES[1]) { cur.asc = base.asc; cur.asc[cbind(pos$row,pos$col)] = distdata[,which(colnames(distdata)=="1990")]; write.asc.gz(cur.asc,paste(out.dir,'1990.potential.asc',sep='')) } #define the current data surface
	#cycling through each year
	for (YEAR in YEARs) { cat(YEAR,'...',round(which(YEARs==YEAR)/length(YEARs)*100,1),'% \n')
		yeardata = distdata[,grep(YEAR,vois)]
		#get the min and max predictions
		outmin = apply(yeardata,1,function(x) { return(min(x,na.rm=TRUE)) })
		outmax = apply(yeardata,1,function(x) { return(max(x,na.rm=TRUE)) })
		#weight the values to make all GCM equivalent
		n=length(GCMs)
		# for (GCM in GCMs) {
			# tt = grep(GCM,colnames(yeardata))
			# if (length(tt)>0) n=n+1
			# if (length(tt)>1) { for (ii in tt) yeardata[,ii] = yeardata[,ii] / length(tt) }
		# }
		#extract mean prediction
		outmean = rowSums(yeardata)/n
		#write out the prediction
		out = base.asc; out[cbind(pos$row,pos$col)] = outmin; write.asc.gz(out,paste(out.dir,ES,'.',YEAR,'.min.potential.asc',sep=''))
		out = base.asc; out[cbind(pos$row,pos$col)] = outmax; write.asc.gz(out,paste(out.dir,ES,'.',YEAR,'.max.potential.asc',sep=''))
		out = base.asc; out[cbind(pos$row,pos$col)] = outmean; write.asc.gz(out,paste(out.dir,ES,'.',YEAR,'.mean.potential.asc',sep=''))		
	}
	
	#now assess the realized distribution
	distdata = distdata *  pos[,'subregion'] #set anything less than threshold to be 0
	#define the outputs
	outdata = rbind(outdata,data.frame(dist.type='realized',sum.data()))
	outdata = rbind(outdata,data.frame(dist.type='realized.NO.small.patches',sum.data.remove.small.patches()))
	#write out the summary output data
	write.csv(outdata,paste(out.dir,ES,'.summary.data.csv',sep=''),row.names=FALSE)
	#create min, mean & max spatial predictions
	if (ES==ES[1]) { cur.asc = base.asc; cur.asc[cbind(pos$row,pos$col)] = distdata[,which(colnames(distdata)=="1990")]; write.asc.gz(cur.asc,paste(out.dir,'1990.realized.asc',sep='')) } #define the current data surface
	#cycling through each year
	for (YEAR in YEARs) { cat(YEAR,'...',round(which(YEARs==YEAR)/length(YEARs)*100,1),'% \n')
		yeardata = distdata[,grep(YEAR,vois)]
		#get the min and max predictions
		outmin = apply(yeardata,1,function(x) { return(min(x,na.rm=TRUE)) })
		outmax = apply(yeardata,1,function(x) { return(max(x,na.rm=TRUE)) })
		#weight the values to make all GCM equivalent
		n=length(GCMs)
		# for (GCM in GCMs) {
			# tt = grep(GCM,colnames(yeardata))
			# if (length(tt)>0) n=n+1
			# if (length(tt)>1) { for (ii in tt) yeardata[,ii] = yeardata[,ii] / length(tt) }
		# }
		#extract mean prediction
		outmean = rowSums(yeardata)/n
		#write out the prediction
		out = base.asc; out[cbind(pos$row,pos$col)] = outmin; write.asc.gz(out,paste(out.dir,ES,'.',YEAR,'.min.realized.asc',sep=''))
		out = base.asc; out[cbind(pos$row,pos$col)] = outmax; write.asc.gz(out,paste(out.dir,ES,'.',YEAR,'.max.realized.asc',sep=''))
		out = base.asc; out[cbind(pos$row,pos$col)] = outmean; write.asc.gz(out,paste(out.dir,ES,'.',YEAR,'.mean.realized.asc',sep=''))		
	}	
	
}
