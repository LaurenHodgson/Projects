library(SDMTools)

work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"

###00.create mean.temp for each month

gcm = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 

sub.dir=NULL
for (tt in gcm) {cat(tt,'\n')
		sub.dir = paste(work.dir, paste('sresa1b', tt, 'run1.run1.2066.2095/', sep='.'), sep='')
		
setwd(sub.dir)
		
mean.temp=NULL

for (ii in 1:12) { cat(ii,'\n')
		tasc.max = read.asc.gz(paste("tasmax", sprintf('%02i',ii), ".asc.gz",sep=''))
		tasc.min = read.asc.gz(paste("tasmin", sprintf('%02i',ii), ".asc.gz",sep=''))
	mean.temp = tasc.max + tasc.min
	mean.temp = mean.temp/2
	
	#write the mean.temp ascis
	write.asc.gz(mean.temp, paste(sub.dir, "mean.temp", sprintf('%02i',ii), ".asc",sep=''))
}	
}

###01.create future annual suitability maps
library(SDMTools)
work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"
gcm = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
cols = colorRampPalette(c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4'))(101)

png(paste(out.dir, '8_gcms.mean_threshold.png',sep=''), width=28, height=13, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,4),mar=c(0,1,0,1), oma=c(0,3,3,0))
sub.dir=NULL

for (tt in gcm) {cat(tt,'\n')
		sub.dir = paste(work.dir, paste('sresa1b', tt, 'run1.run1.2066.2095/', sep='.'), sep='')
		
setwd(sub.dir)
		
sum.growth=NULL
zlim=c(0,6.5)
for (ii in 1:12) { cat(ii,'\n')
		tasc.mean = read.asc.gz(paste("mean.temp", sprintf('%02i',ii), ".asc.gz", sep=''))
	out = tasc.mean
	too.hot= which(tasc.mean>40)
    too.cold = which(tasc.mean<21)
	july.min = which(tasc.mean<15)
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=0
	out[too.cold]=0
	out[july.min]=NA
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	if (length(sum.growth) == 0) { #if there are no values , populate it
          sum.growth = out
        } else { #if already has values, add the new values to it
          sum.growth = sum.growth + out
		 } 
		 
}		 
sum.growth = sum.growth/12
	image(tasc.mean, ann=FALSE,axes=FALSE, col="gray88")
	image(sum.growth, ann=FALSE,axes=FALSE, zlim=zlim, col=cols, add=TRUE)
	text (130, -40, tt, cex=3)
	if(tt==bccr_bcm2_0) {legend.gradient(pnts,cols=cols, limits=zlim, title='Suitability (Growth Rate %/d)', cex=2.5)}
}

dev.off()

###0.2.write the asciis

library(SDMTools)
work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"
gcm = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 

sub.dir=NULL

for (tt in gcm) {cat(tt,'\n')
		sub.dir = paste(work.dir, paste('sresa1b', tt, 'run1.run1.2066.2095/', sep='.'), sep='')
		
setwd(sub.dir)
		
sum.growth=NULL

for (ii in 1:12) { cat(ii,'\n')
		tasc.mean = read.asc.gz(paste("mean.temp", sprintf('%02i',ii), ".asc.gz", sep=''))
	out = tasc.mean
	too.hot= which(tasc.mean>40)
    too.cold = which(tasc.mean<21)
	july.min = which(tasc.mean<15)
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=0
	out[too.cold]=0
	out[july.min]=NA
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	if (length(sum.growth) == 0) { #if there are no values , populate it
          sum.growth = out
        } else { #if already has values, add the new values to it
          sum.growth = sum.growth + out
		 } 
		 
}		 
sum.growth = sum.growth/12

#write the sum.growth ascis
	write.asc.gz(sum.growth, paste(work.dir, tt, ".", "sum.growth", ".asc",sep=''))

}

dev.off()


###03.bin the growth rates for current and future
library(SDMTools)
work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"

growth = list.files(pattern=".asc.gz")

cols = c('wheat3','wheat2','wheat1','slategray2','slategray3','steelblue3','steelblue4')
gcm = c('current', 'bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0')

png(paste(out.dir, 'bins.growth.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))

#loop
for (ii in growth) {  cat(ii,'\n')
      tasc = read.asc.gz(ii)
     
	  t6= which(tasc>=5.5)
	  t5= which(tasc<5.5 & tasc>=4.5)
	  t4= which(tasc<4.5 & tasc>=3.5)
      t3= which(tasc<3.5 & tasc>=2.5)
      t2= which(tasc<2.5 & tasc>=1.5)
      t1= which(tasc<1.5 & tasc>=0.5)
      t0= which(tasc<0.5)

      #overwrite the values in the asci

	  tasc[t6]=6
	  tasc[t5]=5
	  tasc[t4]=4
      tasc[t3]=3
      tasc[t2]=2
      tasc[t1]=1
      tasc[t0]=0 
      
	  
	  image(tasc, ann=FALSE,axes=FALSE,col=cols)
      text (130, -40, gcm[ii], cex=4)
      if (ii==1) {legend(115,-8, c('0', '1', '2', '3', '4', '5', '6'), fill=cols, title='Growth %/d', cex=3)}
	  
      }

dev.off()

####04.barplot of bins

library(SDMTools)
work.dir = "H:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/future_temp/"

growth = list.files(pattern=".asc.gz")
growth = growth[c(4,1:3,5:9)]

cols = c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4')

png(paste(out.dir, 'barplot.bins.growth.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(10,10,5,10), oma=c(0,0,3,2))
mat = matrix(c(1,0,0,0,
			   2,3,4,5,
			   6,7,8,9),3,4,byrow=TRUE)
layout(mat)

#loop
for (ii in growth) {  cat(ii,'\n')
      tasc = read.asc.gz(ii)
     
	  t6= which(tasc>=5.5)
	  t5= which(tasc<5.5 & tasc>=4.5)
	  t4= which(tasc<4.5 & tasc>=3.5)
      t3= which(tasc<3.5 & tasc>=2.5)
      t2= which(tasc<2.5 & tasc>=1.5)
      t1= which(tasc<1.5)
      

      #overwrite the values in the asci

	  tasc[t6]=6
	  tasc[t5]=5
	  tasc[t4]=4
      tasc[t3]=3
      tasc[t2]=2
      tasc[t1]=1
      
	  
	  ClassStat(tasc, latlon=TRUE)
	  cs=ClassStat(tasc, latlon=TRUE)
	  cs$total.area
	  cs$total.area = cs$total.area / 1000000000
	  barplot(cs$total.area,names.arg=cs$class, main=strsplit(ii, '\\.')[[1]][1], xlim=c(0,6), ylim=c(0,2750), xlab='Growth Rate (%/d)', ylab='Area 1000s km^2', col=cols, cex.axis=2, cex.names=2, cex.main=3, cex.lab=2.5)

      }

dev.off()



###05.binned maps and barplots
library(SDMTools)
library(Hmisc)
work.dir = "I:/Barra Work Directory/2080_GCMs/"; setwd(work.dir)
out.dir = "I:/Barra Work Directory/outputs/future_temp/"

growth = list.files(pattern=".asc.gz")
growth = growth[c(4,1:3,5:9)]

cols = c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4')

png(paste(out.dir, 'map.barplot.bins.growth.png',sep=''), width=19, height=22, units='cm', res=300, pointsize=5, bg='white')
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

sum.area=NULL
area.data.frame=NULL
#loop
for (ii in growth) {  cat(ii,'\n')
      tasc = read.asc.gz(ii)
      tasc.base = read.asc('base.asc')
	  
	  
	  t6= which(tasc>=5.5)
	  t5= which(tasc<5.5 & tasc>=4.5)
	  t4= which(tasc<4.5 & tasc>=3.5)
      t3= which(tasc<3.5 & tasc>=2.5)
      t2= which(tasc<2.5 & tasc>=1.5)
      t1= which(tasc<1.5)
      

      #overwrite the values in the asci

	  tasc[t6]=6
	  tasc[t5]=5
	  tasc[t4]=4
      tasc[t3]=3
      tasc[t2]=2
      tasc[t1]=1
      
      image(tasc.base, ann=FALSE,axes=FALSE,col='gray92')
	  image(tasc, ann=FALSE,axes=FALSE,col=cols, add=TRUE)
      text (130, -40, gsub('_',' ',strsplit(ii, '\\.')[[1]][1]), cex=3)
	  
	  ClassStat(tasc, latlon=TRUE)
	  cs=ClassStat(tasc, latlon=TRUE)
	  cs$total.area
	  cs$total.area = cs$total.area / 1000000000
	  barplot(cs$total.area, ylim=c(0,2750), col=cols, axes=FALSE)
	  axis(2,at=seq(0,2750,500),labels=NA,lwd=1,lwd.ticks=1)
	  
	  if (length(sum.area) == 0) { #if there are no values , populate it
          sum.area = cs$total.area
        } else { #if already has values, add the new values to it
          sum.area = sum.area + cs$total.area
		 } 
		 
	  if (length(area.data.frame) == 0) { #if there are no values , populate it
          area.data.frame = cs$total.area
        } else { #if already has values, add the new values to it
          area.data.frame = rbind(area.data.frame, cs$total.area)
		 } 

      }

plot(0,0, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ann=FALSE, type='n')
legend(0,1, c('Unsuitable','<1.5', '1.5-2.5', '2.5-3.5', '3.5-4.5', '4.5-5.5', '5.5-6.5'), fill=c('gray92',cols), title='Growth %/day', cex=3, bty='n')	

#calculations from data frame
area.data.frame
a=area.data.frame
	  current=a[1,]
	  a[c(2:9),]
	  future=a[c(2:9),]
	  colMeans(future, na.rm = FALSE, dims = 1)
	  #13.21918   58.92408  638.36883 2168.46448 1240.43655  842.68557
	  mean=colMeans(future, na.rm = FALSE, dims = 1)
	  sd(future)
	  #13.99981  22.21116 233.30498 299.37418  88.13894 154.97380
	  se = sd(future)/sqrt(8)
	  #4.949682   7.852830  82.485768 105.844757  31.161823  54.791512
	  top=mean + se
	  #18.16887   66.77691  720.85460 2274.30924 1271.59837  897.47708
	  bottom=mean-se
	  #8.269502   51.071253  555.883060 2062.619727 1209.274725  787.894054

#barplot of current	  
barplot(current, names.arg=cs$class, main='current', ylim=c(0,2750), ylab='Area 1000s km^2', col=cols, axes=FALSE, cex.names=2.5, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2750,500),labels=c(0,NA,1000,NA,2000,NA),lwd=1,lwd.ticks=1,cex.axis=2.5)

#barplot of mean total area of gcms
barplot(mean, names.arg=cs$class, main='2080(mean, S.E.)', ylim=c(0,2750), col=cols, axes=FALSE, cex.names=2.5, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2750,500),labels=NA,lwd=1,lwd.ticks=1)

#error bar on barplot
errbar(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7), mean, top, bottom, ylim=c(0,2750), add=TRUE)

dev.off()



############TESTING
sum.area=NULL
area.data.frame=NULL
#loop
for (ii in growth) {  cat(ii,'\n')
      tasc = read.asc.gz(ii)
      tasc.base = read.asc('base.asc')
	  
	  t6= which(tasc>=5.5)
	  t5= which(tasc<5.5 & tasc>=4.5)
	  t4= which(tasc<4.5 & tasc>=3.5)
      t3= which(tasc<3.5 & tasc>=2.5)
      t2= which(tasc<2.5 & tasc>=1.5)
      t1= which(tasc<1.5)
      

      #overwrite the values in the asci

	  tasc[t6]=6
	  tasc[t5]=5
	  tasc[t4]=4
      tasc[t3]=3
      tasc[t2]=2
      tasc[t1]=1
      
	  
	  ClassStat(tasc, latlon=TRUE)
	  cs=ClassStat(tasc, latlon=TRUE)
	  cs$total.area
	  cs$total.area = cs$total.area / 1000000000
	
	  if (length(sum.area) == 0) { #if there are no values , populate it
          sum.area = cs$total.area
        } else { #if already has values, add the new values to it
          sum.area = sum.area + cs$total.area
		 } 

	  
	  if (length(area.data.frame) == 0) { #if there are no values , populate it
          area.data.frame = cs$total.area
        } else { #if already has values, add the new values to it
          area.data.frame = rbind(area.data.frame, cs$total.area)
		 } 
		
      }

	  a=area.data.frame
	  a[c(2:9),]
	  future=a[c(2:9),]
	  colMeans(future, na.rm = FALSE, dims = 1)
	  #13.21918   58.92408  638.36883 2168.46448 1240.43655  842.68557
	  mean=colMeans(future, na.rm = FALSE, dims = 1)
	  sd(future)
	  #13.99981  22.21116 233.30498 299.37418  88.13894 154.97380
	  sd(future)/2
      #6.999907  11.105579 116.652492 149.687091  44.069472  77.486899
	  bar=sd(future)/2
	  top=mean + bar
	  #20.21909   70.02966  755.02132 2318.15158 1284.50602  920.17247
	  bottom=mean-bar
	  #6.219277   47.818505  521.716336 2018.777393 1196.367075  765.198667
	  
	  errbar(x=c(1,2,3,4,5,6),mean, top, bottom, ylim=c(0,2750), add=TRUE)
	  
	  
