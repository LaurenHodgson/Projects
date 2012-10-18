#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###load the necessary libraries
library(SDMTools) 
library(Hmisc)

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
esoclim.dir = 'H:/Barra Work Directory/2080_GCMs/' #define the directory with monthly tmin/tmax
out.dir = paste("H:/Barra Work Directory/outputs/glencross/map_barplot_trial") ; setwd(out.dir) #define & setwd to the output directory

################################################################################


####00.for all 8 gcms and current, write asciis of yearly growth in grams
gcm = c('current_esoclim', 'bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 
sub.dir=NULL

for (tt in gcm) {cat(tt,'\n')
		sub.dir = paste(esoclim.dir, paste('sresa1b', tt, 'run1.run1.2066.2095/', sep='.'), sep='')
		
setwd(sub.dir)
out = NULL #define the basic output raster
#cycle through the months and estimate barra size
for (ii in 1:12) { cat(ii,'\n')
	Tmean = (read.asc.gz(paste(sub.dir,'tasmax',sprintf('%02i',ii),'.asc.gz',sep='')) + 
		read.asc.gz(paste(sub.dir,'tasmin',sprintf('%02i',ii),'.asc.gz',sep=''))) / 2 #read in tmin and tmax, and get tmean
	if (is.null(out)) { out = Tmean; out[which(is.finite(out))] = wt } #create the initial wt ascii raster
	RATE = rate.gain(out,Tmean) #this is a ascii grid of the rate gain
	RATE = log(1+RATE/out) #change the rate to a proportionate change and ln the value for exponential growth estimation
	out = out * exp(RATE * 30) #calculate the weight at the end of the 30 days
	out[which(Tmean<15)] = NA #set any temperature less than 15 to NA as barra will not grow there
}
write.asc.gz(out, paste(out.dir,'/', tt, ".", "yearly.growth.in.grams",sep='')) #write out the ascii file.
}


###01.make png with map and barplot
setwd(out.dir)
growth = list.files(pattern=".asc.gz")
growth = growth[c(4,1:3,5:9)]

#rgb.palette = colorRampPalette(c('red','yellow','blue'), space='rgb')
cols = rainbow(7, alpha=0.5)

png(paste(out.dir, '/', 'yearly.growth.in.grams2.png',sep=''), width=19, height=22, units='cm', res=300, pointsize=5, bg='white')
#make 4 columns of 3 rows of images 
par(mfrow=c(4,3),mar=c(1,5,4,1), oma=c(0,3,2,1))
mat = matrix(c(2,1,1,1,20,20,20,20,21,21,21,21,
			   1,1,1,1,20,20,20,20,21,21,21,21,
			   1,1,1,1,20,20,20,20,21,21,21,21,
			   4,3,3,3,6,5,5,5,8,7,7,7,
			   3,3,3,3,5,5,5,5,7,7,7,7,
			   3,3,3,3,5,5,5,5,7,7,7,7,
			   10,9,9,9,12,11,11,11,14,13,13,13,
			   9,9,9,9,11,11,11,11,13,13,13,13,
			   9,9,9,9,11,11,11,11,13,13,13,13,
			   16,15,15,15,18,17,17,17,19,19,19,19,
			   15,15,15,15,17,17,17,17,19,19,19,19,
			   15,15,15,15,17,17,17,17,19,19,19,19),nr=12,nc=12,byrow=TRUE)
layout(mat)

#make image for future gcms
area.data.frame=NULL
current=NULL
#loop
for (jj in 1:length(growth)) {  
	  ii = growth[jj]; cat(ii,'\n')
      tasc = read.asc.gz(ii)
      tasc.base = read.asc(paste(esoclim.dir, 'base.asc', sep=''))
	  
	  
	  t6= which(tasc>=3000)
	  t5= which(tasc<3000 & tasc>=2500)
	  t4= which(tasc<2500 & tasc>=2000)
      t3= which(tasc<2000 & tasc>=1500)
      t2= which(tasc<1500 & tasc>=1000)
      t1= which(tasc<1000 & tasc>=500)
      t0= which(tasc<500)

      #overwrite the values in the asci

	  tasc[t6]=7
	  tasc[t5]=6
	  tasc[t4]=5
      tasc[t3]=4
      tasc[t2]=3
      tasc[t1]=2
	  tasc[t0]=1
      
      image(tasc.base, ann=FALSE,axes=FALSE,col='gray90')
	  image(tasc, ann=FALSE,axes=FALSE,col=cols, zlim=c(1,7), add=TRUE)
      if (jj==1) {text (130, -40, 'current', cex=3)}
	  if (jj>1) {text (130, -40, gsub('_',' ',strsplit(ii, '\\.')[[1]][1]), cex=3)}
	  
	  cs=ClassStat(tasc, latlon=TRUE)
	  cs$total.area = cs$total.area / 1000000000
	  
	  if (jj==1) {current=c(0,cs$total.area,0)
	  
	  barplot(current, ylim=c(0,2500), col=cols, axes=FALSE)
	  axis(2,at=seq(0,2500,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj>1) {barplot(cs$total.area, ylim=c(0,2500), col=cols, axes=FALSE)
	  axis(2,at=seq(0,2500,500),labels=NA,lwd=1,lwd.ticks=1)
	  
		if (length(area.data.frame) == 0) { #if there are no values , populate it
          area.data.frame = cs$total.area
        } else { #if already has values, add the new values to it
          area.data.frame = rbind(area.data.frame, cs$total.area)
		 } 
	  }
      }

plot(0,0, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ann=FALSE, type='n')
legend(0,1, c('Unsuitable','<500', '500-1000', '1000-1500', '1500-2000', '2000-2500', '2500-3000', '>3000'), fill=c('gray88',cols), title='Weight at 12 months (g)', cex=3, bty='n')	

#calculations from data frame
area.data.frame
a=area.data.frame
	 
	  colMeans(a, na.rm = FALSE, dims = 1)
	  #13.21918   58.92408  638.36883 2168.46448 1240.43655  842.68557
	  mean=colMeans(a, na.rm = FALSE, dims = 1)
	  sd(a)
	  #13.99981  22.21116 233.30498 299.37418  88.13894 154.97380
	  se = sd(a)/sqrt(8)
	  #4.949682   7.852830  82.485768 105.844757  31.161823  54.791512
	  top=mean + se
	  #18.16887   66.77691  720.85460 2274.30924 1271.59837  897.47708
	  bottom=mean-se
	  #8.269502   51.071253  555.883060 2062.619727 1209.274725  787.894054

#barplot of current	  
barplot(current, main='current', ylim=c(0,2500), ylab='Area 1000s km^2', col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2500,500),labels=c(0,NA,1000,NA,2000,NA),lwd=1,lwd.ticks=1,cex.axis=2.5)

#barplot of mean total area of gcms
barplot(mean, main='2080(mean, S.E.)', ylim=c(0,2500), col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2500,500),labels=NA,lwd=1,lwd.ticks=1)

#error bar on barplot
errbar(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), mean, top, bottom, ylim=c(0,3000), add=TRUE)

dev.off()
