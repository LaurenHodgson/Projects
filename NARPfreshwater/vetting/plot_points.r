library(SDMTools); library(maptools); library(plotrix) #load the necessary libraries
source('/home/jc148322/scripts/libraries/cool_functions.r')

image.dir = "/home/jc148322/NARPfreshwater/vetting/images/"
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

spp='Bidyanus_bidyanus'
spp='Bidyanus_welchi'
###################################
#Bring in all the necessary information

#reaches = readShapePoly(' .shp') }#read in your shapefile

spdata=read.csv(paste('/home/jc246980/ALA_downloads/Data/Fish/',spp,'.csv',sep=''))

assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(spdata$LONGDEC,spdata$LATDEC, padding.percent=5)

setwd(image.dir)

if (max.lat>=-18 & min.lat<=-34 |
	max.lon>=148 & min.lon<=120 ) { 
	xlim=c(min(pos$lon),max(pos$lon)); 
	ylim=c(min(pos$lat),max(pos$lat))

}else{ 
	xlim=c(min.lon,max.lon); 
	ylim=c(min.lat,max.lat)

	}

setwd(image.dir)

png(paste(image.dir, spp,'.test.dynamic.zoom.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
	par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0))
	
	mat = matrix(c( 2,3,3,3,3,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1),nr=5,nc=5,byrow=TRUE) #create a layout matrix for images
	layout(mat) #call layout as defined above

	image(base.asc, ann=FALSE,axes=FALSE,col='grey',  xlim=xlim,ylim=ylim)
	#clip(xlim[1],xlim[2],ylim[1],ylim[2])
	#image(base.asc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
    #plot(reaches, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)
	points(species.data[,'LONGDEC'],species.data[,'LATDEC'],pch=16,cex=2)
	
	assign.list(l,r,b,t) %=% par("usr")

	image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=c(0,1))
	image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)
	
	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(5,10,gsub('_',' ',spp),cex=4)
	

dev.off()




















