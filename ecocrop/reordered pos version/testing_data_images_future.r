cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'#003300')

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))



zlim=c(min(tdb),max(tdb))

png(paste('monthly_temp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=tdb[,ii]
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable temp', cex=3)}

      }

dev.off()

zlim=c(min(tout),max(tout))

png(paste('monthly_temp_gmin.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=tout[,ii]
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable temp', cex=3)}

      }

dev.off()



zlim=c(min(db.min.max.pp),max(db.min.max.pp))

png(paste('monthly_pptemp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=db.min.max.pp[,ii]
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
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
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable pp', cex=3)}

      }

dev.off()

####

cols= c('gray90',colorRampPalette(c('tan','forestgreen'))(99),'#003300')

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))



zlim=c(min(tout),max(tout))

png(paste('monthly_temp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=tout[,ii]
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable temp', cex=3)}

      }

dev.off()

zlim=c(min(ppout),max(ppout))

png(paste('monthly_pptemp.png',sep=''), width=28, height=19, units='cm', res=300, pointsize=5, bg='white')

#make 4 columns of 3 rows of images 
par(mfrow=c(3,4),mar=c(0,1,0,1), oma=c(0,3,3,0))


#loop
for (ii in 1:12) {  cat(ii,'\n')
      tt=ppout[,ii]
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
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
	  tasc = base.asc
	  tasc[cbind(pos$row, pos$col)] = tt

      image(tasc, zlim=zlim, ann=FALSE,axes=FALSE,col=cols)
     
      if (ii==1) {legend.gradient(pnts,cols=cols,limits=round(zlim), title='Suitable pp', cex=3)}

      }

dev.off()
