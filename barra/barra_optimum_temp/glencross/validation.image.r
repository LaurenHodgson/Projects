library(SDMTools)

out.dir='E:/Barra Work Directory/outputs/daily_validation/trials/'; setwd(out.dir)

cols=rainbow(9)


load(file=paste(out.dir, 'mrb.sept06.rData', sep=''))
sept06 = out;out=NULL
sept06$day = NULL
sept06$day=seq(1,length(sept06$julian.day), 1)

load(file=paste(out.dir, 'mrb.oct06.rData', sep=''))
oct06 = out;out=NULL
oct06$day = NULL
oct06$day=seq(1,length(oct06$julian.day), 1)

load(file=paste(out.dir, 'mrb.nov06.rData', sep=''))
nov06 = out;out=NULL
nov06$day = NULL
nov06$day=seq(1,length(nov06$julian.day), 1)

load(file=paste(out.dir, 'mrb.dec06.rData', sep=''))
dec06 = out;out=NULL
dec06$day = NULL
dec06$day=seq(1,length(dec06$julian.day), 1)

load(file=paste(out.dir, 'mrb.jan07.rData', sep=''))
jan07 = out;out=NULL
jan07$day = NULL
jan07$day=seq(1,length(jan07$julian.day), 1)

load(file=paste(out.dir, 'mrb.feb07.rData', sep=''))
feb07 = out;out=NULL
feb07$day = NULL
feb07$day=seq(1,length(feb07$julian.day), 1)

load(file=paste(out.dir, 'mrb.mar07.rData', sep=''))
mar07 = out;out=NULL
mar07$day = NULL
mar07$day=seq(1,length(mar07$julian.day), 1)

load(file=paste(out.dir, 'mrb.apr07.rData', sep=''))
apr07 = out;out=NULL
apr07$day = NULL
apr07$day=seq(1,length(apr07$julian.day), 1)

load(file=paste(out.dir, 'mrb.may07.rData', sep=''))
may07 = out;out=NULL
may07$day = NULL
may07$day=seq(1,length(may07$julian.day), 1)

load(file=paste(out.dir, 'mrb.sept07.rData', sep=''))
sept07 = out;out=NULL
sept07$day = NULL
sept07$day=seq(1,length(sept07$julian.day), 1)

load(file=paste(out.dir, 'mrb.oct07.rData', sep=''))
oct07 = out;out=NULL
oct07$day = NULL
oct07$day=seq(1,length(oct07$julian.day), 1)

load(file=paste(out.dir, 'mrb.nov07.rData', sep=''))
nov07 = out;out=NULL
nov07$day = NULL
nov07$day=seq(1,length(nov07$julian.day), 1)

load(file=paste(out.dir, 'mrb.dec07.rData', sep=''))
dec07 = out;out=NULL
dec07$day = NULL
dec07$day=seq(1,length(dec07$julian.day), 1)

load(file=paste(out.dir, 'mrb.jan08.rData', sep=''))
jan08 = out;out=NULL
jan08$day = NULL
jan08$day=seq(1,length(jan08$julian.day), 1)

load(file=paste(out.dir, 'mrb.feb08.rData', sep=''))
feb08 = out;out=NULL
feb08$day = NULL
feb08$day=seq(1,length(feb08$julian.day), 1)

load(file=paste(out.dir, 'mrb.mar08.rData', sep=''))
mar08 = out;out=NULL
mar08$day = NULL
mar08$day=seq(1,length(mar08$julian.day), 1)

load(file=paste(out.dir, 'mrb.apr08.rData', sep=''))
apr08 = out;out=NULL
apr08$day = NULL
apr08$day=seq(1,length(apr08$julian.day), 1)

load(file=paste(out.dir, 'mrb.may08.rData', sep=''))
may08 = out;out=NULL
may08$day = NULL
may08$day=seq(1,length(may08$julian.day), 1)



png(paste(out.dir, 'mrb.2yr.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')
par(mar=c(3,5,3,2))
	plot(sept06$day,sept06$wt, ylim=c(0,5000), main="Weight of Barramundi over time",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=1.5, cex.lab=1.3, cex.axis=1, col=cols[1])
		lines(sept06$day,sept06$wt, lty=3, col=cols[1])
		lines(sept06$day[which(sept06$wt<=3000)],sept06$wt[which(sept06$wt<=3000)],col=cols[1])
		
		lines(oct06$day,oct06$wt, lty=3,col=cols[2])
		lines(oct06$day[which(oct06$wt<=3000)],oct06$wt[which(oct06$wt<=3000)],col=cols[2])
		
		lines(nov06$day,nov06$wt, lty=3,col=cols[3])
		lines(nov06$day[which(nov06$wt<=3000)],nov06$wt[which(nov06$wt<=3000)],col=cols[3])
		
		lines(dec06$day,dec06$wt, lty=3, col=cols[4])
		lines(dec06$day[which(dec06$wt<=3000)],dec06$wt[which(dec06$wt<=3000)], col=cols[4])
		
		lines(jan07$day,jan07$wt, lty=3,col=cols[5])
		lines(jan07$day[which(jan07$wt<=3000)],jan07$wt[which(jan07$wt<=3000)],col=cols[5])
		
		lines(feb07$day,feb07$wt, lty=3,,col=cols[6])
		lines(feb07$day[which(feb07$wt<=3000)],feb07$wt[which(feb07$wt<=3000)],col=cols[6])
		
		lines(mar07$day,mar07$wt, lty=3,,col=cols[7])
		lines(mar07$day[which(mar07$wt<=3000)],mar07$wt[which(mar07$wt<=3000)],col=cols[7])
		
		lines(apr07$day,apr07$wt, lty=3,,col=cols[8])
		lines(apr07$day[which(apr07$wt<=3000)],apr07$wt[which(apr07$wt<=3000)],col=cols[8])
		
		lines(may07$day,may07$wt, lty=3,,col=cols[9])
		lines(may07$day[which(may07$wt<=3000)],may07$wt[which(may07$wt<=3000)],col=cols[9])
		
		#2nd year
		lines(sept07$day,sept07$wt, lty=3, col=cols[1])
		lines(sept07$day[which(sept07$wt<=3000)],sept07$wt[which(sept07$wt<=3000)],col=cols[1])
		
		lines(oct07$day,oct07$wt, lty=3,col=cols[2])
		lines(oct07$day[which(oct07$wt<=3000)],oct07$wt[which(oct07$wt<=3000)],col=cols[2])
		
		lines(nov07$day,nov07$wt, lty=3,col=cols[3])
		lines(nov07$day[which(nov07$wt<=3000)],nov07$wt[which(nov07$wt<=3000)],col=cols[3])
		
		lines(dec07$day,dec07$wt, lty=3, col=cols[4])
		lines(dec07$day[which(dec07$wt<=3000)],dec07$wt[which(dec07$wt<=3000)], col=cols[4])
		
		lines(jan08$day,jan08$wt, lty=3,col=cols[5])
		lines(jan08$day[which(jan08$wt<=3000)],jan08$wt[which(jan08$wt<=3000)],col=cols[5])
		
		lines(feb08$day,feb08$wt, lty=3,,col=cols[6])
		lines(feb08$day[which(feb08$wt<=3000)],feb08$wt[which(feb08$wt<=3000)],col=cols[6])
		
		lines(mar08$day,mar08$wt, lty=3,,col=cols[7])
		lines(mar08$day[which(mar08$wt<=3000)],mar08$wt[which(mar08$wt<=3000)],col=cols[7])
		
		lines(apr08$day,apr08$wt, lty=3,,col=cols[8])
		lines(apr08$day[which(apr08$wt<=3000)],apr08$wt[which(apr08$wt<=3000)],col=cols[8])
		
		lines(may08$day,may08$wt, lty=3,,col=cols[9])
		lines(may08$day[which(may08$wt<=3000)],may08$wt[which(may08$wt<=3000)],col=cols[9])
		
		#compare with real data
		lines(x=c(274, 274), y=c(900,1200))
		lines(x=c(456, 456), y=c(1200,1500))
		lines(x=c(639, 639), y=c(2500,3000))
		#points(x=c(274, 456, 639), y=c(1200, 1500, 3000), pch=8)		
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1)
dev.off()




############### gain

png(paste(out.dir, 'mrb.gain.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='white')
par(mar=c(3,5,3,2))
	plot(sept06$day,sept06$gain, ylim=c(0,15), main="Weight of Barramundi over time",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=1.5, cex.lab=1.3, cex.axis=1, col=cols[1])
		lines(sept06$day,sept06$gain, lty=3, col=cols[1])
		lines(sept06$day[which(sept06$gain<=3000)],sept06$gain[which(sept06$gain<=3000)],col=cols[1])
		
		lines(nov06$day,nov06$gain, lty=3,col=cols[3])
		lines(nov06$day[which(nov06$gain<=3000)],nov06$gain[which(nov06$gain<=3000)],col=cols[3])
		
	
		lines(jan07$day,jan07$gain, lty=3,col=cols[5])
		lines(jan07$day[which(jan07$gain<=3000)],jan07$gain[which(jan07$gain<=3000)],col=cols[5])
		
		
		
		#points(x=c(274, 456, 639), y=c(1200, 1500, 3000), pch=8)		
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1)
dev.off()


		#2nd year
		lines(sept07$day,sept07$gain, lty=3, col=cols[1])
		lines(sept07$day[which(sept07$gain<=3000)],sept07$gain[which(sept07$gain<=3000)],col=cols[1])
		
		lines(oct07$day,oct07$gain, lty=3,col=cols[2])
		lines(oct07$day[which(oct07$gain<=3000)],oct07$gain[which(oct07$gain<=3000)],col=cols[2])
		
		lines(nov07$day,nov07$gain, lty=3,col=cols[3])
		lines(nov07$day[which(nov07$gain<=3000)],nov07$gain[which(nov07$gain<=3000)],col=cols[3])
		
		lines(dec07$day,dec07$gain, lty=3, col=cols[4])
		lines(dec07$day[which(dec07$gain<=3000)],dec07$gain[which(dec07$gain<=3000)], col=cols[4])
		
		lines(jan08$day,jan08$gain, lty=3,col=cols[5])
		lines(jan08$day[which(jan08$gain<=3000)],jan08$gain[which(jan08$gain<=3000)],col=cols[5])
		
		lines(feb08$day,feb08$gain, lty=3,,col=cols[6])
		lines(feb08$day[which(feb08$gain<=3000)],feb08$gain[which(feb08$gain<=3000)],col=cols[6])
		
		lines(mar08$day,mar08$gain, lty=3,,col=cols[7])
		lines(mar08$day[which(mar08$gain<=3000)],mar08$gain[which(mar08$gain<=3000)],col=cols[7])
		
		lines(apr08$day,apr08$gain, lty=3,,col=cols[8])
		lines(apr08$day[which(apr08$gain<=3000)],apr08$gain[which(apr08$gain<=3000)],col=cols[8])
		
		lines(may08$day,may08$gain, lty=3,,col=cols[9])
		lines(may08$day[which(may08$gain<=3000)],may08$gain[which(may08$gain<=3000)],col=cols[9])
		
		
		
		
		
		
		lines(oct06$day,oct06$gain, lty=3,col=cols[2])
		lines(oct06$day[which(oct06$gain<=3000)],oct06$gain[which(oct06$gain<=3000)],col=cols[2])
		
		lines(nov06$day,nov06$gain, lty=3,col=cols[3])
		lines(nov06$day[which(nov06$gain<=3000)],nov06$gain[which(nov06$gain<=3000)],col=cols[3])
		
		lines(dec06$day,dec06$gain, lty=3, col=cols[4])
		lines(dec06$day[which(dec06$gain<=3000)],dec06$gain[which(dec06$gain<=3000)], col=cols[4])
		
		lines(jan07$day,jan07$gain, lty=3,col=cols[5])
		lines(jan07$day[which(jan07$gain<=3000)],jan07$gain[which(jan07$gain<=3000)],col=cols[5])
		
		lines(feb07$day,feb07$gain, lty=3,,col=cols[6])
		lines(feb07$day[which(feb07$gain<=3000)],feb07$gain[which(feb07$gain<=3000)],col=cols[6])
		
		lines(mar07$day,mar07$gain, lty=3,,col=cols[7])
		lines(mar07$day[which(mar07$gain<=3000)],mar07$gain[which(mar07$gain<=3000)],col=cols[7])
		
		lines(apr07$day,apr07$gain, lty=3,,col=cols[8])
		lines(apr07$day[which(apr07$gain<=3000)],apr07$gain[which(apr07$gain<=3000)],col=cols[8])
		
		lines(may07$day,may07$gain, lty=3,,col=cols[9])
		lines(may07$day[which(may07$gain<=3000)],may07$gain[which(may07$gain<=3000)],col=cols[9])
		
		