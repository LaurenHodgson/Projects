library(SDMTools)

work.dir = "H:/Barra Work Directory/2080_GCMs/all/sresa1b.bccr_bcm2_0.run1.run1.2066.2095/";setwd(work.dir)
out.dir = paste("H:/Barra Work Directory/outputs/gcm_mean_map_barplot/", basename(work.dir), '/',sep='')
dir.create(out.dir)


###00.create mean.temp for each month
sum.growth=NULL
mean.temp=NULL

for (ii in 1:12) { cat(ii,'\n')
	
		tasc.max = read.asc.gz(paste("tasmax", sprintf('%02i',ii), ".asc.gz",sep=''))
		tasc.min = read.asc.gz(paste("tasmin", sprintf('%02i',ii), ".asc.gz",sep=''))
	mean.temp = tasc.max + tasc.min
	mean.temp = mean.temp/2
	
	#write the mean.temp ascis
	write.asc.gz(mean.temp, paste(out.dir, "mean.temp", sprintf('%02i',ii), sep=''))
    }
	
setwd(out.dir)
###########added to test
pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))
colramp = c('tan', colorRampPalette(c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4'))(101))
months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

png(paste(out.dir, 'monthly.growth.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')
#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))

zlim=c(0, 6.5)
#############

sum.growth=NULL	
for (ii in 1:12) { cat(ii,'\n')	
	
		tasc.mean = read.asc.gz(paste("mean.temp", sprintf('%02i',ii), ".asc.gz", sep=''))
	out = tasc.mean
	####added to test
	nil = tasc.mean
	####
	too.hot= which(tasc.mean>40)
    too.cold = which(tasc.mean<21)
	july.min = which(tasc.mean<15)
	####added to test
	no.growth = which (tasc.mean<21 & tasc.mean>=15)
	####
	out = -51.49 + 3.73*tasc.mean + (-0.06)*tasc.mean^2
	
	out[too.hot]=0
	out[too.cold]=0
	out[july.min]=NA
	####added to test
	nil[no.growth]=NA
	####
	
	tasc.mean[which(is.finite(tasc.mean))] = 1
	
	if (length(sum.growth) == 0) { #if there are no values , populate it
          sum.growth = out
        } else { #if already has values, add the new values to it
          sum.growth = sum.growth + out
		 } 
		 
	####added to test
	image(tasc.mean, ann=FALSE,axes=FALSE,col="tan")
	image(nil, ann=FALSE,axes=FALSE,col="gray88", add=TRUE)
	image(out, ann=FALSE,axes=FALSE, zlim=zlim, col=colramp, add=TRUE)
	text (130, -40, months[ii], cex=4)
	if (ii == 1) {legend.gradient(pnts,cols=colramp, limits=zlim, title='Suitability', cex=3)}
	####
		 
}
dev.off()	 
sum.growth = sum.growth/12

#write the sum.growth asci
	write.asc.gz(sum.growth, paste(out.dir, basename(work.dir), ".", "sum.growth",sep=''))

cols = c('wheat3','wheat2','slategray2','slategray3','steelblue3','steelblue4')
png(paste(out.dir, 'map.barplot.png',sep=''), width=7, height=6, units='cm', res=300, pointsize=5, bg='white')
par(mar=c(3,5,4,1))
mat = matrix(c(2,1,1,1,
			   1,1,1,1,
			   1,1,1,1),nr=3,nc=4,byrow=TRUE)
layout(mat)


	  tasc = read.asc.gz(paste(basename(work.dir),".", "sum.growth", ".asc.gz",sep=''))
      tasc.base = read.asc.gz('mean.temp01.asc.gz')
	  
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
      text (130, -40, gsub('_',' ',strsplit(basename(work.dir), '\\.')[[1]][2]), cex=3)
	  
	  cs=ClassStat(tasc, latlon=TRUE)
	  cs$total.area = cs$total.area / 1000000000
	  barplot(cs$total.area, names.arg=cs$class, ylim=c(0,2750), col=cols, axes=FALSE, xlab='Growth Rate (%/d)', ylab='Area 1000s km^2',  cex.names=0.8, cex.lab=1)
	  
	  axis(2,at=seq(0,2750,500),labels=c(0,NA,1000,NA,2000,NA),lwd=1,lwd.ticks=1)

dev.off()




