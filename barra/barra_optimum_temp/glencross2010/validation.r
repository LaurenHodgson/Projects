#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###load the necessary libraries

library(SDMTools)
###set working directories
tmin.dir = '/scratch/data/portlet/jc165798/Climate/AWAP.direct.download/tmin/daily/'
tmax.dir = '/scratch/data/portlet/jc165798/Climate/AWAP.direct.download/tmax/daily/'
out.dir= '/home/22/jc148322/Barra/temperature_outputs/validation/'
script.dir="/home/22/jc148322/scripts/"


################################################################################
#constants & equation from Glencross 2010
#gain equation
K = 2.249522916; 		a = -0.00950;			b=0.72000
x = -0.327485829; 		y = 0.014951694; 		z=-0.000203425
rate.gain = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
	return((K + x*Temperature + y*Temperature^2 + z*Temperature^3) * liveweight^(a*Temperature + b))
}

################################################################################
###set the variables
start.wt = 20

#julaten (145.34, -16.58)
start.date=as.Date("2008-05-01")
end.date=as.Date("2010-05-01")

location='jul'
monthyr='may08'
position = cbind(145.34, -16.58) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

start.date=as.Date("2007-09-01")
end.date=as.Date("2009-07-01")

location='jul'
monthyr='sept07'
position = cbind(145.34, -16.58) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

#innisfail (145.99, -17.51)
start.date=as.Date("2008-05-01")
end.date=as.Date("2010-05-01")

location='inf'
monthyr='may08'
position = cbind(145.99, -17.51) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

start.date=as.Date("2007-09-01")
end.date=as.Date("2009-07-01")

location='inf'
monthyr='sept07'
position = cbind(145.99, -17.51) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

#daintree river (145.32, -16.24)
start.date=as.Date("2008-05-01")
end.date=as.Date("2010-05-01")

location='dtr'
monthyr='may08'
position = cbind(145.32, -16.24) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

start.date=as.Date("2007-09-01")
end.date=as.Date("2009-07-01")

location='dtr'
monthyr='sept07'
position = cbind(145.32, -16.24) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

#mareeba (145.42,-16.98)
start.date=as.Date("2008-05-01")
end.date=as.Date("2010-05-01")

location='mrb'
monthyr='may08'
position = cbind(145.42,-16.98) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))

start.date=as.Date("2007-09-01")
end.date=as.Date("2009-07-01")

location='mrb'
monthyr='sept07'
position = cbind(145.42,-16.98) #define the position of interest in lon lat
source(paste(script.dir,'validation_data.r', sep=''))


#townsville (146.6,-19.3)
#bowen (148.25,-19.99)
#mareeba (145.42,-16.98)
#darwin (130.84,-12.43)
#innisfail (145.99, -17.51)
#julaten (145.34, -16.58)
#daintree river (145.32, -16.24)
###############################################################################

cols=c('red3', 'dodgerblue4', 'forestgreen', 'goldenrod2')


load(file=paste(out.dir, 'jul.sept07.rData', sep=''))
jul = out;out=NULL
jul$day = NULL
jul$day=seq(1,length(jul$julian.day), 1)

load(file=paste(out.dir, 'jul.may08.rData', sep=''))
jul2 = out;out=NULL
jul2$day = NULL
jul2$day=seq(1,length(jul2$julian.day), 1)

load(file=paste(out.dir, 'dtr.sept07.rData', sep=''))
dtr = out;out=NULL
dtr$day = NULL
dtr$day=seq(1,length(dtr$julian.day), 1)

load(file=paste(out.dir, 'dtr.may08.rData', sep=''))
dtr2 = out;out=NULL
dtr2$day = NULL
dtr2$day=seq(1,length(dtr2$julian.day), 1)

load(file=paste(out.dir, 'inf.sept07.rData', sep=''))
inf = out;out=NULL
inf$day = NULL
inf$day=seq(1,length(inf$julian.day), 1)

load(file=paste(out.dir, 'inf.may08.rData', sep=''))
inf2 = out;out=NULL
inf2$day = NULL
inf2$day=seq(1,length(inf2$julian.day), 1)

load(file=paste(out.dir, 'mrb.sept07.rData', sep=''))
mrb = out;out=NULL
mrb$day = NULL
mrb$day=seq(1,length(mrb$julian.day), 1)

load(file=paste(out.dir, 'mrb.may08.rData', sep=''))
mrb2 = out;out=NULL
mrb2$day = NULL
mrb2$day=seq(1,length(mrb2$julian.day), 1)


png(paste(out.dir, 'validation.png',sep=''), width=14, height=14, units='cm', res=300, pointsize=5, bg='transparent')
par(mfrow=c(2,2), mar=c(3,5,5,2))
	#jul
	plot(jul$day,jul$wt, ylim=c(0,5000), main="Julaten: Sept - May",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=2, cex.lab=1.7, cex.axis=1.7, col=cols[1])
				
		lines(jul$day,jul$wt, lty=3,col=cols[1])
		lines(jul$day[which(jul$wt<=3000)],jul$wt[which(jul$wt<=3000)],col=cols[1])
		
		lines(jul2$day,jul2$wt, lty=3,col=cols[1])
		lines(jul2$day[which(jul2$wt<=3000)],jul2$wt[which(jul2$wt<=3000)],col=cols[1], lty=4)
		
		lines(x=c(548, 548), y=c(2000,3000), col=cols[1], lwd=3)
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1.7)
	
	#dtr
	plot(dtr$day,dtr$wt, ylim=c(0,5000), main="Daintree: Sept only",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=2, cex.lab=1.7, cex.axis=1.7, col=cols[1])
			
		lines(dtr$day,dtr$wt, lty=3,col=cols[2])
		lines(dtr$day[which(dtr$wt<=3000)],dtr$wt[which(dtr$wt<=3000)],col=cols[2])	
		
		lines(dtr2$day,dtr2$wt, lty=3,col=cols[2])
		lines(dtr2$day[which(dtr2$wt<=3000)],dtr2$wt[which(dtr2$wt<=3000)],col=cols[2], lty=4)
	
		lines(x=c(122, 122), y=c(500,1000),col=cols[2], lwd=3 )
		lines(x=c(670, 670), y=c(4000,5000),col=cols[2], lwd=3)
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1.7)
	
	#inf
	plot(inf$day,inf$wt, ylim=c(0,5000), main="Innisfail: Sept - May",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n', cex.main=2, cex.lab=1.7, cex.axis=1.7, col=cols[1])
					
		lines(inf$day,inf$wt, lty=3,col=cols[3])
		lines(inf$day[which(inf$wt<=3000)],inf$wt[which(inf$wt<=3000)],col=cols[3])	
		
		lines(inf2$day,inf2$wt, lty=3,col=cols[3])
		lines(inf2$day[which(inf2$wt<=3000)],inf2$wt[which(inf2$wt<=3000)],col=cols[3], lty=4)

		lines(x=c(60, 60), y=c(200,300),col=cols[3], lwd=3)
		lines(x=c(608, 670), y=c(3000,3000),col=cols[3], lwd=3)
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1.7)
	
	#mrb
	plot(mrb$day,mrb$wt, ylim=c(0,5000), main="Mareeba: Sept - May",  xaxt='n', xlab=NA, ylab="Weight (g)", type='n',  cex.main=2, cex.lab=1.7, cex.axis=1.7, col=cols[1])
					
		lines(mrb$day,mrb$wt, lty=3,col=cols[4])
		lines(mrb$day[which(mrb$wt<=3000)],mrb$wt[which(mrb$wt<=3000)],col=cols[4])
		
		lines(mrb2$day,mrb2$wt, lty=3,col=cols[4])
		lines(mrb2$day[which(mrb2$wt<=3000)],mrb2$wt[which(mrb2$wt<=3000)],col=cols[4], lty=4)
		
		lines(x=c(274, 274), y=c(900,1200),col=cols[4], lwd=3)
		lines(x=c(456, 456), y=c(1200,1500),col=cols[4], lwd=3)
		lines(x=c(639, 639), y=c(2500,3000),col=cols[4], lwd=3)
		
		axis(1,at=c(1,182.5,365,547.5, 730),labels=c(NA,'6 months',NA, '18 months', NA),lwd=1,lwd.ticks=1.2, cex.axis=1.7)
dev.off()




