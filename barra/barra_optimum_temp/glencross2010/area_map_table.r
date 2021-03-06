load(file=paste(in.dir, yr, '.weighted.mean.rData',sep=''))
copy.pos=tcopy[1:30]
sum.gcms=pos.table
GCMs = c('bccr_bcm2_0', 'csiro_mk3_0', 'csiro_mk3_5', 'giss_aom', 'inmcm3_0', 'miroc3_2_hires', 'miroc3_2_medres', 'ncar_ccsm3_0') 


cols = colnames(copy.pos)
runs=NULL
tt=NULL
for (gcm in GCMs) {
runs=grep(gcm,cols,value=T)

if (length(runs)==1) {
sum.gcms[gcm] = copy.pos[,runs]}

if (length(runs)>1){
sum.gcms[gcm] = rowSums(copy.pos[,runs])}

sum.gcms[which(sum.gcms[,gcm]==0),gcm]=NA
	tt = base.asc
	tt[cbind(sum.gcms$row, sum.gcms$col)] = sum.gcms[,gcm]
	write.asc.gz(tt, paste(out.dir, yr, '.', gcm,'.asc', sep=''))
}



#############################
setwd(out.dir)
files = list.files(pattern=".asc.gz")
growth= grep(yr, files, value=T)
growth = c(grep('current', files, value=T),growth)

gradient = colorRampPalette(c('red3','goldenrod2', 'forestgreen','dodgerblue4'), space='rgb')
cols = gradient(7)

png(paste(out.dir, yr, '.area2.png',sep=''), width=19, height=22, units='cm', res=300, pointsize=5, bg='white')
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
for (jj in 1:length(growth)) {  
	  ii = growth[jj]; cat(ii,'\n')
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
	  
	  barplot(current, ylim=c(0,3000), col=cols, axes=FALSE) #call barplot for current
	  axis(2,at=seq(0,3000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
	  if (jj>1) {barplot(cs$total.area, ylim=c(0,3000), col=cols, axes=FALSE)
	  axis(2,at=seq(0,3000,500),labels=NA,lwd=1,lwd.ticks=1)}
	  
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
barplot(current, names.arg=cs$class, main='current', ylim=c(0,3000), ylab='Area 1000s km^2', col=cols, axes=FALSE, cex.names=2.5, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,3000,500),labels=c(0,NA,1000,NA,2000,NA,3000),lwd=1,lwd.ticks=1,cex.axis=2.5)

#barplot of mean total area of gcms
barplot(mean, names.arg=cs$class, main=paste(yr,' (mean, S.E.)', sep=''),ylim=c(0,3500), col=cols, axes=FALSE, cex.names=2.5, cex.main=3, cex.lab=2.5)
axis(2,at=seq(0,3500,500),labels=NA,lwd=1,lwd.ticks=1)

#error bar on barplot
errbar(x=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), mean, top, bottom, ylim=c(0,2750), add=TRUE)

dev.off()

write.csv(area.data.frame,paste(out.dir, yr, '.area.csv', sep=''))
