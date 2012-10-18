library(SDMTools)

out.dir='E:/Barra Work Directory/outputs/daily_validation/trials/'; setwd(out.dir)

cols=c('red3', 'dodgerblue4', 'forestgreen', 'goldenrod2')


load(file=paste(out.dir, 'jul.sept08.rData', sep=''))
jul = out;out=NULL
jul$day = NULL
jul$day=seq(1,length(jul$julian.day), 1)

load(file=paste(out.dir, 'jul.apr08.rData', sep=''))
jul2 = out;out=NULL
jul2$day = NULL
jul2$day=seq(1,length(jul2$julian.day), 1)

load(file=paste(out.dir, 'dtr.sept08.rData', sep=''))
dtr = out;out=NULL
dtr$day = NULL
dtr$day=seq(1,length(dtr$julian.day), 1)


load(file=paste(out.dir, 'inf.mid-sept08.rData', sep=''))
inf = out;out=NULL
inf$day = NULL
inf$day=seq(1,length(inf$julian.day), 1)

load(file=paste(out.dir, 'inf.mid-apr08.rData', sep=''))
inf2 = out;out=NULL
inf2$day = NULL
inf2$day=seq(1,length(inf2$julian.day), 1)

load(file=paste(out.dir, 'mrb.sept07.rData', sep=''))
mrb = out;out=NULL
mrb$day = NULL
mrb$day=seq(1,length(mrb$julian.day), 1)

load(file=paste(out.dir, 'mrb.apr08.rData', sep=''))
mrb2 = out;out=NULL
mrb2$day = NULL
mrb2$day=seq(1,length(mrb2$julian.day), 1)


png(paste(out.dir, 'all.test2.png',sep=''), width=7, height=7, units='cm', res=300, pointsize=5, bg='transparent')
par(mar=c(3,5,3,2))
	plot(jul$day,jul$wt, ylim=c(0,5000), main="Weight of Barramundi over time",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=1.5, cex.lab=1.3, cex.axis=1, col=cols[1])
				
		lines(jul$day,jul$wt, lty=3,col=cols[1])
		lines(jul$day[which(jul$wt<=3000)],jul$wt[which(jul$wt<=3000)],col=cols[1])
		
		lines(jul2$day,jul2$wt, lty=3,col=cols[1])
		lines(jul2$day[which(jul2$wt<=3000)],jul2$wt[which(jul2$wt<=3000)],col=cols[1], lty=4)
		
		lines(dtr$day,dtr$wt, lty=3,col=cols[2])
		lines(dtr$day[which(dtr$wt<=3000)],dtr$wt[which(dtr$wt<=3000)],col=cols[2])	

		lines(inf$day,inf$wt, lty=3,col=cols[3])
		lines(inf$day[which(inf$wt<=3000)],inf$wt[which(inf$wt<=3000)],col=cols[3])	
		
		lines(inf2$day,inf2$wt, lty=3,col=cols[3])
		lines(inf2$day[which(inf2$wt<=3000)],inf2$wt[which(inf2$wt<=3000)],col=cols[3], lty=4)	
		
		lines(mrb$day,mrb$wt, lty=3,col=cols[4])
		lines(mrb$day[which(mrb$wt<=3000)],mrb$wt[which(mrb$wt<=3000)],col=cols[4])
		
		lines(mrb2$day,mrb2$wt, lty=3,col=cols[4])
		lines(mrb2$day[which(mrb2$wt<=3000)],mrb2$wt[which(mrb2$wt<=3000)],col=cols[4], lty=4)
		
		#compare with real data
		#jul
		lines(x=c(548, 548), y=c(2000,3000), col=cols[1], lwd=3)
		#dtr
		lines(x=c(122, 122), y=c(500,1000),col=cols[2], lwd=3 )
		lines(x=c(670, 670), y=c(4000,5000),col=cols[2], lwd=3)
		#inf
		lines(x=c(60, 60), y=c(200,300),col=cols[3], lwd=3)
		lines(x=c(608, 670), y=c(3000,3000),col=cols[3], lwd=3)
		#mrb
		lines(x=c(274, 274), y=c(900,1200),col=cols[4], lwd=3)
		lines(x=c(456, 456), y=c(1200,1500),col=cols[4], lwd=3)
		lines(x=c(639, 639), y=c(2500,3000),col=cols[4], lwd=3)
		
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1)
dev.off()
