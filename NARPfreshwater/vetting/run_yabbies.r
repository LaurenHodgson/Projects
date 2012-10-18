################################################################################
# Script to batch map yabbies

args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }

library(SDMTools);library(maptools)
source('/home/jc148322/scripts/libraries/cool_functions.r')
ala.dir="/home/jc246980/ALA_downloads/Data/Inverts/Invert_species/"
image.dir = "/home/jc148322/NARPfreshwater/vetting/images/yabbies";dir.create(image.dir)

load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")


base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files


species.data.ala = read.csv(paste(ala.dir, '/',spp,'.csv',sep=''))
species.data.ala=species.data.ala[,c("Matched_Scientific_Name","Latitude_processed","Longitude_processed")]
species.data.ala=na.omit(species.data.ala)

assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(species.data.ala$Longitude_processed,species.data.ala$Latitude_processed, padding.percent=10)

setwd(image.dir)



## this creates the mini-australia, called �clip�
if (max.lat>=-18 & min.lat<=-34 |
	max.lon>=148 & min.lon<=120 ) {
	xlim=c(min(pos$lon),max(pos$lon));
	ylim=c(min(pos$lat),max(pos$lat))

}else{
	xlim=c(min.lon,max.lon);
	ylim=c(min.lat,max.lat)
}



png(paste(spp,'.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
	par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0))

	mat = matrix(c( 2,3,3,3,
					  1,1,1,1,
					  1,1,1,1,
					  1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
	layout(mat) #call layout as defined above

	image(base.asc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)

	plot(catchments, ann=FALSE,axes=FALSE, add=TRUE, col='grey93', lwd=1.5)
	plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')

	points(species.data.ala[,'Longitude_processed'],species.data.ala[,'Latitude_processed'],pch=16,cex=2, col='red')

	assign.list(l,r,b,t) %=% par("usr")

	image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=c(0,1))
	image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)

	plot(1:20,axes=FALSE,ann=FALSE,type='n')
	text(7,15,gsub('_',' ',spp),cex=4)

	legend(5,10, inset = c(0.4, 0.2),
	legend = c("ALA records"),
	bty = "n", pch = c(19),           # bty = "n": no box
	col = c(2), pt.cex = c(4),
	lty = c(-1), cex=3)




dev.off()
