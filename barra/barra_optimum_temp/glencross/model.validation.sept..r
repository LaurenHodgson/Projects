#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###load the necessary libraries

library(SDMTools)
###set working directories
tmin.dir = '/data/jc165798/Climate/AWAP.direct.download/tmin/daily/'
tmax.dir = '/data/jc165798/Climate/AWAP.direct.download/tmax/daily/'
out.dir= '/home2/22/jc148322/Barra/outputs/daily_validation/';dir.create(out.dir)

################################################################################
###set the variables
start.date = as.Date("2008-09-01"); end.date = as.Date("2010-08-31")
start.wt = 20
position = cbind(145.42,-16.98) #define the position of interest in lon lat
#townsville (146.6,-19.3)
#bowen (148.25,-19.99)
#mareeba (145.42,-16.98)
#darwin (130.84,-12.43)
#common lenht 150cm, up to 200cm, up to 60kg

################################################################################
###create a data frame of daily tmean from start date to end date at location
out = NULL #define the generic output

for(tdate in start.date:end.date) { cat(as.Date(tdate, origin="1970-01-01"),'\n')
	text.date = format(as.Date(tdate, origin="1970-01-01"),"%Y%m%d") #get the date in the format needed
	Tmean = (read.asc.gz(paste(tmin.dir,text.date,'.asc.gz',sep='')) + 
	read.asc.gz(paste(tmax.dir,text.date,'.asc.gz',sep=''))) / 2 #read in tmin and tmax, and get tmean
	tval = extract.data(position, Tmean) #extract the tmean for the location
	out = rbind(out,data.frame(year=format(as.Date(tdate, origin="1970-01-01"),"%Y"),
	month=format(as.Date(tdate, origin="1970-01-01"),"%m"),
	day=format(as.Date(tdate, origin="1970-01-01"),"%d"),
	julian.day=tdate,Tmean=tval)) #append the data out to a data frame
	#wt=NA,gain=NA,Me=NA,Me.sum=NA,percent.gain=NA)) #append more rows from a new start date if necessary
}



################################################################################
#constants & equation from Glencross 2008
#gain equation
K = 0.539661; k = 0.4240000
x = -0.119917; y = 0.007434; z=-0.000119
rate.gain = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
if (Temperature<15) {return(0)} else {return((K + x*Temperature + y*Temperature^2 + z*Temperature^3) * liveweight^k)}
}

#energy maintenance equation
L = 0.446243; l = 0.8
a = -0.084845; b = 0.004828; c=-0.000075
Me = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
if (Temperature<15) {return((L + a*15 + b*15^2 + c*15^3) * liveweight^l)} 
else {return((L + a*Temperature + b*Temperature^2 + c*Temperature^3) * liveweight^l)}
}

################################################################################

out$percent.gain = out$Me.sum = out$Me = out$gain = out$wt = NA
for (ii in 1:nrow(out)) { #cycle through each row of output data set
if (ii == 1) { out$wt[ii] = start.wt } else { out$wt[ii] = out$wt[ii-1] + out$gain[ii-1] }#define the starting wt or the new wt
out$gain[ii] = rate.gain(out$wt[ii],out$Tmean[ii]) #get rate of gain for that day
out$percent.gain = out$gain/out$wt
out$percent.gain = out$percent.gain*100
out$Me[ii] = Me(out$wt[ii],out$Tmean[ii]) #get rate of Me for that day

if (ii == 1) { out$Me.sum[ii] = out$Me[ii] } else { out$Me.sum[ii] = out$Me.sum[ii-1] + out$Me[ii] }#define the starting Me and cumulative Me

}


#out = read.csv(paste(out.dir, 'mrbwinter_20090101-20101231', '.csv',sep=''))
out$length=NULL
out$length= (out$wt/0.0106)^(1/3.02)
#W = 0.0106*L^3.02

write.csv(out,file=paste(out.dir, 'mrbsept_20080901-20100831', '.csv',sep=''))

png(paste(out.dir, 'mareeba.sept.end_weight.png',sep=''), width=21, height=14, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,3), mar=c(5,7,4,4), oma=c(1,1,4,1))

	plot(out$julian.day,out$wt, ylim=c(0,8000), xlim=c(14122,14877),main="Weight of Barramundi over time", xaxt='n', xlab=NA, ylab="Weight (g)", type='l', lty=3, cex.main=2.5, cex.lab=2.3, cex.axis=1.7)
		lines(out$julian.day[which(out$wt<=3000)],out$wt[which(out$wt<=3000)])
		axis(1,at=seq(14122,14877,200),labels=c(NA,'April 09',NA, 'April 2010'),lwd=1,lwd.ticks=1, cex.axis=1.7)
	plot(out$julian.day, out$gain, ylim=c(0,20),xlim=c(14122,14877), main="Weight gain of Barramundi over time", ylab="Weight (g)", xlab=NA,xaxt='n', type='l', lty=3, cex.main=2.5, cex.lab=2.3, cex.axis=1.7)
		lines(out$julian.day[which(out$wt<=3000)],out$gain[which(out$wt<=3000)])
		axis(1,at=seq(14122,14877,200),labels=c(NA,'April 09',NA, 'April 2010'),lwd=1,lwd.ticks=1, cex.axis=1.7)
	plot(out$julian.day,out$percent.gain, ylim=c(0,8), xlim=c(14122,14877),main="Time vs percent gain", ylab="% gain", xlab=NA,xaxt='n', type='l', lty=3, cex.main=2.5, cex.lab=2.3, cex.axis=1.7)
		lines(out$julian.day[which(out$wt<=3000)],out$percent.gain[which(out$wt<=3000)])
		axis(1,at=seq(14122,14877,200),labels=c(NA,'April 09',NA, 'April 2010'),lwd=1,lwd.ticks=1, cex.axis=1.7)
	plot(out$julian.day,out$Me.sum, ylim=c(0,90000),xlim=c(14122,14877),main="Cumulative energy demand",ylab="kJ/d",xlab=NA,xaxt='n', type='l', lty=3, cex.main=2.5, cex.lab=2.3, cex.axis=1.7)
		lines(out$julian.day[which(out$wt<=3000)],out$Me.sum[which(out$wt<=3000)])
		axis(1,at=seq(14122,14877,200),labels=c(NA,'April 09',NA, 'April 2010'),lwd=1,lwd.ticks=1, cex.axis=1.7)
	plot(out$julian.day,out$Me, ylim=c(0,300), xlim=c(14122,14877),main="Maintenance energy demand", ylab="kJ/d", xlab=NA,xaxt='n', type='l', lty=3, cex.main=2.5, cex.lab=2.3, cex.axis=1.7)
		lines(out$julian.day[which(out$wt<=3000)],out$Me[which(out$wt<=3000)])
		axis(1,at=seq(14122,14877,200),labels=c(NA,'April 09',NA, 'April 2010'),lwd=1,lwd.ticks=1, cex.axis=1.7)
	plot(out$julian.day,out$length, ylim=c(0,90),xlim=c(14122,14877),main="Length over time", ylab="Length(cm)", xlab=NA,xaxt='n', type='l', lty=3, cex.main=2.5, cex.lab=2.3, cex.axis=1.7)
		lines(out$julian.day[which(out$wt<=3000)],out$length[which(out$wt<=3000)])
		axis(1,at=seq(14400,15000,200),labels=c(NA,'April 09',NA, 'April 2010'),lwd=1,lwd.ticks=1, cex.axis=1.7)
	title(main="Mareeba - Sept 08", outer=T,cex.main=3, font.main=3)
dev.off()






