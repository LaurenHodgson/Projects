#calculating mean areas for all gcms

###05.binned maps and barplots
library(SDMTools)
library(Hmisc)
esoclim.dir='/scratch/data/portlet/jc165798/Barra/future.esoclim/';
in.dir='/home/22/jc148322/Barra/temperature_outputs/'; setwd(in.dir)
out.dir = "/home/22/jc148322/Barra/temperature_outputs/area/" ;  #define & setwd to the output directory
script.dir="/home/22/jc148322/scripts/"
base.asc = read.asc.gz(paste(esoclim.dir, "sresa1b.bccr_bcm2_0.run1.run1.2066.2095/tasmax01.asc.gz", sep="")); 
base.asc[which(is.finite(base.asc))] = 0
pos.table = as.data.frame(which(is.finite(base.asc), arr.ind=TRUE))

############################

yr='2080'
load(file=paste(in.dir, yr, 'weight.rData',sep=''))
copy.pos=pos[3:32]

for (ii in 1:ncol(copy.pos)){cat(ii,'\n')
jj=colnames(copy.pos)[ii]
jj=strsplit(jj, '\\.')[[1]][2]
tt = base.asc

#add the data from the relevant columns to the asc and write as .asc
tt[cbind(pos$row, pos$col)] = copy.pos[,ii]
write.asc.gz(tt, paste(out.dir, yr, '.', jj,'.asc', sep=''))
}

#############################
setwd(out.dir)
files = list.files(pattern=".asc.gz")
growth= grep(yr, files, value=T)
growth = c(grep('current', files, value=T),growth)

gradient = colorRampPalette(c('red3','goldenrod2', 'forestgreen','dodgerblue4'), space='rgb')
cols = gradient(7)

png(paste(out.dir, yr, '.area.png',sep=''), width=19, height=22, units='cm', res=300, pointsize=5, bg='white')
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
	  
	  if (jj>1) {barplot(cs$total.area, ylim=c(0,2000), col=cols, axes=FALSE)
	  axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
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
legend(0,1, c('Unsuitable','<500', '500-1000', '1000-1500', '1500-2000', '2000-2500', '2500-3000', '>3000'), fill=c('gray86',cols), title='Weight at 12 months (g)', cex=3, bty='n')	

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
barplot(current, names.arg=cs$class, main='current', ylim=c(0,2000), ylab='Area 1000s km^2', col=cols, axes=FALSE, cex.names=2.5, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2000,500),labels=c(0,NA,1000,NA,2000),lwd=1,lwd.ticks=1,cex.axis=2.5)

#barplot of mean total area of gcms
barplot(mean, names.arg=cs$class, main=paste(yr,' (mean, S.E.)', sep=''),ylim=c(0,2000), col=cols, axes=FALSE, cex.names=2.5, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,2000,500),labels=NA,lwd=1,lwd.ticks=1)

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

