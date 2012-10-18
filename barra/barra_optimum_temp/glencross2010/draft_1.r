#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###load the necessary libraries
library(SDMTools) 
library(Hmisc)

################################################################################
###define some constants, equations & functions

#constants & equation from Glencross 2008
K = 2.249522916; 		a = -0.00950;			b=0.72000
x = -0.327485829; 		y = 0.014951694; 		z=-0.000203425
rate.gain = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
	return((K + x*Temperature + y*Temperature^2 + z*Temperature^3) * liveweight^(a*Temperature + b))
}

wt = 20 #define the fish starting weight in grams

#directory locations
esoclim.dir = '/scratch/data/portlet/jc165798/Barra/future.esoclim/' #define the directory with monthly tmin/tmax
#/data/jc165798/Barra/future.esoclim/'
#/home1/31/jc165798/Climate/PCMDI/01.Oz.5km.61.90/future.esoclim/
out.dir = "/home/22/jc148322/Barra/temperature_outputs/" ;  #define & setwd to the output directory
script.dir="/home/22/jc148322/scripts/"

################################################################################
base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0
pos.table = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

yr='2080';source(paste(script.dir,'pos_table_weights.r', sep=''))
yr='2050';source(paste(script.dir,'pos_table_weights.r', sep=''))
yr='2030';source(paste(script.dir,'pos_table_weights.r', sep=''))
yr='current';source(paste(script.dir,'current_weight.r', sep=''))
#############################

###real image
setwd(out.dir)
weight.maps = list.files(pattern=".asc.gz")
weight.maps = weight.maps[c(10,3,2,1,6,5,4,9,8,7)] #re-order the files for the loop.

###03.define parameters of the image
gradient = colorRampPalette(c('red3','goldenrod2', 'forestgreen','dodgerblue4'), space='rgb')
cols = gradient(7)

png(paste(out.dir, '/', 'mean.min.max.png',sep=''), width=20, height=20, units='cm', res=300, pointsize=5, bg='white') #call image
#make 4 columns of 4 rows of images
par(mfrow=c(4,4),mar=c(1,2,2,1), oma=c(0,3,2,1)) #set margins and outer margins
mat = matrix(c(1,1,1,1,2,2,2,2,4,3,3,3,23,23,23,23,
				1,1,1,1,2,2,2,2,3,3,3,3,23,23,23,23,
				1,1,1,1,2,2,2,2,3,3,3,3,23,23,23,23,
				1,1,1,1,2,2,2,2,3,3,3,3,23,23,23,23,
				6,5,5,5,8,7,7,7,10,9,9,9,24,24,24,24,
				5,5,5,5,7,7,7,7,9,9,9,9,24,24,24,24,
				5,5,5,5,7,7,7,7,9,9,9,9,24,24,24,24,
				5,5,5,5,7,7,7,7,9,9,9,9,24,24,24,24,
				12,11,11,11,14,13,13,13,16,15,15,15,25,25,25,25,
				11,11,11,11,13,13,13,13,15,15,15,15,25,25,25,25,
				11,11,11,11,13,13,13,13,15,15,15,15,25,25,25,25,
				11,11,11,11,13,13,13,13,15,15,15,15,25,25,25,25,
				18,17,17,17,20,19,19,19,22,21,21,21,26,26,26,26,
				17,17,17,17,19,19,19,19,21,21,21,21,26,26,26,26,
				17,17,17,17,19,19,19,19,21,21,21,21,26,26,26,26,
				17,17,17,17,19,19,19,19,21,21,21,21,26,26,26,26),nr=16,nc=16,byrow=TRUE)
layout(mat) #call layout as defined above

#image 1 - box legend
plot(0,0, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ann=FALSE, type='n')
legend(0,1, c('Unsuitable','<500', '500-1000', '1000-1500', '1500-2000', '2000-2500', '2500-3000', '>3000'), fill=c('gray88',cols), title='Weight at 12 months (g)', cex=3, bty='n')	

#image 2 - no plot
plot(0,0, ylim=c(0,10), xlim=c(0,10), axes=FALSE, ann=FALSE, type='n')

#image 5-22 - maps and corner area barplots

current=NULL
all.classes=NULL
twenty.thirty.min = NULL
twenty.thirty = NULL
twenty.thirty.max = NULL
twenty.fifty.min = NULL
twenty.fifty =NULL
twenty.fifty.max = NULL
twenty.eighty.min = NULL
twenty.eighty = NULL
twenty.eighty.max = NULL
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
      image(base.asc, ann=FALSE,axes=FALSE,col='gray86')
	  image(tasc, ann=FALSE,axes=FALSE,col=cols, zlim=c(1,7), add=TRUE)
      if (jj==1) {text (130, -40, 'current', cex=3)} #text for current
	  if (jj>1) {text (130, -40, paste(strsplit(ii, '\\.')[[1]][1],' ',strsplit(ii, '\\.')[[1]][2], sep=''), cex=3)} #text for 2030-2080 (min, mean, max)
	  
	  #calculate area within each class (1-7)
	  cs=ClassStat(tasc, latlon=TRUE)
	  cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
	  
	  #call barplots (mini - corner)
	  if (jj==1) {current=c(cs$total.area,0) #define all classes for 'current' (has fewer classes than 2080)
	  
	  barplot(current, ylim=c(0,2000), col=cols, axes=FALSE) #call barplot for current
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj==2) {all.classes = c(0,cs$total.area)
	  barplot(all.classes, ylim=c(0,2000), col=cols, axes=FALSE)
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	 
	  if (jj == 3) {barplot(cs$total.area, ylim=c(0,2000), col=cols, axes=FALSE) #call barplot for 2030 - 2080 (min, mean, max) - note: 2030 might have fewer classes too
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj == 4) {barplot(cs$total.area, ylim=c(0,2000), col=cols, axes=FALSE) #call barplot for 2030 - 2080 (min, mean, max) - note: 2030 might have fewer classes too
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj==5) {all.classes = c(0,cs$total.area)
	  barplot(all.classes, ylim=c(0,2000), col=cols, axes=FALSE)
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj>5) {barplot(cs$total.area, ylim=c(0,2000), col=cols, axes=FALSE) #call barplot for 2030 - 2080 (min, mean, max) - note: 2030 might have fewer classes too
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj==2)  { twenty.thirty.min=all.classes }
	  if (jj==3) { twenty.thirty=cs$total.area } #store the area data for  2030
	  if (jj==4) { twenty.thirty.max=cs$total.area }
	  if (jj==5) { twenty.fifty.min=all.classes}
	  if (jj==6) { twenty.fifty=cs$total.area } #store the area date for 2050
	  if (jj==7) { twenty.fifty.max=cs$total.area }
	  if (jj==8) { twenty.eighty.min=cs$total.area }
	  if (jj==9) { twenty.eighty=cs$total.area } #store the area data for 2030
	  if (jj==10) { twenty.eighty.max=cs$total.area }
	   
	  }

par(mar=c(4,6,5,2)) #define new margins for large barplots (if this is possible)

#image 23 - barplot current	  
barplot(current, main='Current', ylim=c(0,2000), ylab='Area 1000s km^2', col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2000,500),labels=c(0,NA,1000,NA,2000),lwd=1,lwd.ticks=1,cex.axis=2.5)

#image 24 - barplot of mean 2030
barplot(twenty.thirty, main='2030 (mean)', ylim=c(0,2000), col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
#points(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), twenty.thirty.min, pch='-', cex=3)
#points(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), twenty.thirty.max, pch='+', cex=3)
axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)
#text (3,2350, '+  Maximum Area', cex=2.5)
#text (3,2100, '-  Minimum Area', cex=2.5)

#image 25 - barplot of mean 2030
barplot(twenty.fifty, main='2050 (mean)', ylim=c(0,2000), col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
#points(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), twenty.fifty.min, pch='-', cex=3)
#points(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), twenty.fifty.max, pch='+', cex=3)
axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)

#image 26 - barplot of mean 2030
barplot(twenty.eighty, main='2080 (mean)',ylim=c(0,2000), col=cols, axes=FALSE, cex.main=3, cex.lab=2.5)
#points(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), twenty.eighty.min, pch='-', cex=3)
#points(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), twenty.eighty.max, pch='+', cex=3)
axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)

#error bar on barplot - reminder: i need to write a whole other script to determine error bars from the tables
#errbar(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), mean, top, bottom, ylim=c(0,3000), add=TRUE)

dev.off()




################################################
#determine number of models that indicate suitability
################################################
model.sum=tcopy[1:30]
for (ii in 1:ncol(model.sum)){
	model.sum[which(model.sum[,ii] > 0),ii] = 1
}

model.sum$total = rowSums(model.sum[1:30])

model.sum.asc=base.asc
model.sum.asc[cbind(pos$row, pos$col)] = model.sum$total

#plot of number of runs of models that indicate suitability
threshold = 0.1
#setup some plot parameters
bins = seq(0,1,length=31); bins = cut(threshold,bins,labels=FALSE)
cols2= c(rep('gray90',bins),colorRampPalette(c('gray60','gray11'))(30)[bins:30])
png(paste(out.dir, '/', '2080.gcm.variability.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')
image(model.sum.asc, ann=FALSE,axes=FALSE, col=cols2)

dev.off()




################################################
#things i may edit to make the images
################################################

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
