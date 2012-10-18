load(file=paste(out.dir, 'jul.sept08.rData', sep=''))
sept08 = out;out=NULL
sept08$day = NULL
sept08$day=seq(1,length(sept08$julian.day), 1)


png(paste(out.dir, 'jul.test.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')
par(mar=c(3,5,3,2))
	plot(sept08$day,sept08$wt, ylim=c(0,5000), main="Weight of Barramundi over time",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=1.5, cex.lab=1.3, cex.axis=1, col=cols[1])
				
		lines(sept08$day,sept08$wt, lty=3,col=cols[2])
		lines(sept08$day[which(sept08$wt<=3000)],sept08$wt[which(sept08$wt<=3000)],col=cols[2])
		
		#compare with real data
		
		lines(x=c(548, 548), y=c(2000,3000))
		#points(x=c(274, 456, 639), y=c(1200, 1500, 3000), pch=8)		
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1)
dev.off()
