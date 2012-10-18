#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###hpc:qsub -l nodes=1:ppn=8 -I
###hpc:qsub -l nodes=1:ppn=2:V20Z -I

###load the necessary libraries
library(SDMTools) 
library(Hmisc)

###define directories
esoclim.dir = '/data/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
out.dir = "/home2/22/jc148322/Barra/outputs/glencross/trials/"
setwd(out.dir)
################################################################################

###01.define base image
base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0 #sets all values that are finite to zero (not necessary?)

###02.define list of files.  includes ascis of weights in grams after 1 year - current, 2030, 2050, 2080 (min, max, mean)
maps = list.files(pattern=".asc.gz")
mean.maps = grep("mean",maps, value=TRUE)
current.map= grep("current",maps, value=TRUE)
weight.maps = c(current.map, mean.maps)

###03.define parameters of the image
cols = rainbow(7, alpha=0.5)
#cols = c('gray70', 'gray60', 'gray50', 'gray40', 'gray30', 'gray20', 'gray10')

ylim=c(-29, -10)
xlim=c(138,153.6)

png(paste(out.dir, '/', 'mean.qld.png',sep=''), width=15, height=10, units='cm', res=300, pointsize=5, bg='white') #call image
#make 4 columns of 4 rows of images
par(mfrow=c(3,2),mar=c(1,2,2,1), oma=c(0,2,2,1)) 

mat = matrix(c(1,1,1,1,2,2,2,2,9,9,9,9,
				1,1,1,1,2,2,2,2,9,9,9,9,
				1,1,1,1,2,2,2,2,9,9,9,9,
				1,1,1,1,2,2,2,2,9,9,9,9,
				4,3,3,3,6,5,5,5,8,7,7,7,
				3,3,3,3,5,5,5,5,7,7,7,7,
				3,3,3,3,5,5,5,5,7,7,7,7,
				3,3,3,3,5,5,5,5,7,7,7,7),nr=8,nc=12,byrow=TRUE)
layout(mat) #call layout as defined above

#image 1-8 - maps and area barplots
current=NULL
#loop through files defined by 'weight.maps'
for (jj in 1:length(weight.maps)) {  
	  ii = weight.maps[jj]; cat(ii,'\n')
      tasc = read.asc.gz(ii)
      
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
      
	  #call map
	  par(mar=c(1,2,2,1))
      image(base.asc, ann=FALSE,axes=FALSE, xlim=xlim, ylim=ylim, col='gray86')
	  image(tasc, ann=FALSE,axes=FALSE,col=cols, xlim=xlim, ylim=ylim, zlim=c(1,7), add=TRUE)
      if (jj==1) {text (148, -12, 'Current', cex=3)} #text for current
	  if (jj>1) {text (148, -12, paste(strsplit(ii, '\\.')[[1]][1],' ',strsplit(ii, '\\.')[[1]][2], sep=''), cex=3)} #text for 2030-2080 (min, mean, max)
	  
	  plot(1,1, type='n', ann=FALSE,axes=FALSE)
	  ##calculate area within each class (1-7)
	  # cs=ClassStat(tasc, latlon=TRUE)
	  # cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
	  
	  ##call barplots 
	  # if (jj==1) {current=c(0,cs$total.area,0) #define all classes for 'current' (has fewer classes than 2080)
	  # par(mar=c(4,6,5,2))
	  # barplot(current, main='Current', ylim=c(0,2000), ylab='Area 1000s km^2', col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
	  # axis(2,at=seq(0,2000,500),labels=c(0,NA,1000,NA,2000),lwd=1,lwd.ticks=1,cex.axis=2.5)
# }
	 
	  # if (jj>1) {barplot(cs$total.area, ylim=c(0,2000), col=cols, axes=FALSE) #call barplot for 2030 - 2080 (min, mean, max) - note: 2030 might have fewer classes too
	  # axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	   
	  }

#image 9 - box legend
plot(0,0, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ann=FALSE, type='n')
legend(0,1, c('Unsuitable','<500', '500-1000', '1000-1500', '1500-2000', '2000-2500', '2500-3000', '>3000'), fill=c('gray88',cols), title='Weight at 12 months (g)', cex=3, bty='n')	

dev.off()
