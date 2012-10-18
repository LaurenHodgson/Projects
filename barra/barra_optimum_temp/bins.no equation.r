library(SDMTools)

work.dir = "H:/Barra Work Directory/current.esoclim/"; setwd(work.dir)
out.dir = "H:/Barra Work Directory/outputs/optimum_temp/"

cols = c('gray86', 'paleturquoise4', 'paleturquoise3', 'paleturquoise', 'gray86')
months=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

png(paste(out.dir, 'mean.temp_bins.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))

#loop
for (ii in 1:12) {  cat(ii,'\n')
      tasc = read.asc(paste("mean.temp", sprintf('%02i',ii), ".asc", sep=''))
     
	  t4= which(tasc>=37)
      t3= which(tasc<37 & tasc>=27)
      t2= which(tasc<27 & tasc>=22)
      t1= which(tasc<22 & tasc>=15)
      t0= which(tasc<15)

      #overwrite the values in the asci

      tasc[t4]=4
      tasc[t3]=3
      tasc[t2]=2
      tasc[t1]=1
      tasc[t0]=0 
      
      image(tasc, ann=FALSE,axes=FALSE,col=cols, zlim=c(0,4))
      text (130, -40, months[ii], cex=4)
      if (ii==1) {legend(115,-8, c('<17 Death','17-21 No growth','22-26 Sub-optimal','27-36 Optimal','>=37 Suboptimal/Death'), fill=cols, title='Mean Tepmerature', cex=3)}

      }

dev.off()
