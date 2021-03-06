#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###load the necessary libraries
library(SDMTools) 

################################################################################
###define some constants, equations & functions

#constants & equation from Glencross 2008
K = 0.539661; 		k = 0.4240000
x = -0.119917; 		y = 0.007434; 		z=-0.000119
rate.gain = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
	return((K + x*Temperature + y*Temperature^2 + z*Temperature^3) * liveweight^k)
}

wt = 20 #define the fish starting weight in grams

#directory locations
esoclim.dir = 'H:/Barra Work Directory/2080_GCMs/sresa1b.csiro_mk3_5.run1.run1.2066.2095/' #define the directory with monthly tmin/tmax
out.dir = paste("H:/Barra Work Directory/outputs/glencross/", basename(esoclim.dir), '/',sep='') ; dir.create(out.dir); setwd(out.dir) #define & setwd to the output directory

################################################################################
#do some work
out = NULL #define the basic output raster

#cycle through the months and estimate barra size
for (ii in 1:12) { cat(ii,'\n')
	Tmean = (read.asc.gz(paste(esoclim.dir,'tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) + 
		read.asc.gz(paste(esoclim.dir,'tasmin',sprintf('%02i',ii),'.asc.gz',sep=''))) / 2 #read in tmin and tmax, and get tmean
	if (is.null(out)) { out = Tmean; out[which(is.finite(out))] = wt } #create the initial wt ascii raster
	RATE = rate.gain(out,Tmean) #this is a ascii grid of the rate gain
	RATE = log(1+RATE/out) #change the rate to a proportionate change and ln the value for exponential growth estimation
	out = out * exp(RATE * 30) #calculate the weight at the end of the 30 days
	out[which(Tmean<15)] = NA #set any temperature less than 15 to NA as barra will not grow there
}
write.asc.gz(out,'yearly.growth.in.grams.asc') #write out the ascii file.
#points for legend
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
#create a basic plot
base.asc = Tmean; base.asc[which(is.finite(base.asc))] = 0 #create the base to overlay the weight map on
pdf('1st.try.pdf')
	par(mar=c(0,0,0,0))
	image(base.asc,col='grey',axes=FALSE,ann=FALSE)
	image(out,col=rainbow(100),add=TRUE)
	legend.gradient(pnts,cols=rainbow(100), limits=round(range(out,na.rm=TRUE)), title='Growth(g/year)', cex=1)
dev.off()

