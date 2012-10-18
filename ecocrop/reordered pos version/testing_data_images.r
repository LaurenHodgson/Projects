cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'#003300')

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))



zlim=c(min(temp.copy),max(temp.copy))

png(paste('monthly_temp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=temp.copy[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos$row, pos$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable temp', cex=3)}

      }

dev.off()

zlim=c(min(copy.pp),max(copy.pp))

png(paste('monthly_pptemp_pos.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=copy.pp[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos.temp$row, pos.temp$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable pp', cex=3)}

      }

dev.off()

zlim=c(min(ppout),max(ppout))

png(paste('monthly_pptemp_pos.pp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=ppout[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos.pp$row, pos.pp$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable pp', cex=3)}

      }

dev.off()





zlim=c(min(ppdb[which(is.finite(ppdb))]),max(ppdb[which(is.finite(ppdb))]))

png(paste('monthly_pp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=ppdb[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos$row, pos$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable pp', cex=3)}

      }

dev.off()

zlim=c(min(ppdb[which(is.finite(ppdb))]),max(ppdb[which(is.finite(ppdb))]))

png(paste('monthly_pp_pos.pp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=ppdb[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos.pp$row, pos.pp$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable pp', cex=3)}

      }

dev.off()





zlim=c(min(tmaxdb[which(is.finite(tmaxdb))]),max(tmaxdb[which(is.finite(tmaxdb))]))

png(paste('monthly_max.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=tmaxdb[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos.temp$row, pos.temp$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable tmax', cex=3)}

      }

dev.off()


zlim=c(min(tmindb[which(is.finite(tmindb))]),max(tmindb[which(is.finite(tmindb))]))

png(paste('monthly_min.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=tmindb[,ii]
	  pp.asc = base.asc
	  pp.asc[cbind(pos.temp$row, pos.temp$col)] = tt

      image(pp.asc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable tmin', cex=3)}

      }

dev.off()
