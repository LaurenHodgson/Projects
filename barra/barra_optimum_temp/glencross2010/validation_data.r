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

out$percent.gain = out$gain = out$wt = NA
for (ii in 1:nrow(out)) { #cycle through each row of output data set
if (ii == 1) { out$wt[ii] = start.wt } else { out$wt[ii] = out$wt[ii-1] + out$gain[ii-1] }#define the starting wt or the new wt
out$gain[ii] = rate.gain(out$wt[ii],out$Tmean[ii]) #get rate of gain for that day
out$percent.gain = out$gain/out$wt
out$percent.gain = out$percent.gain*100
}

save(out, file=paste(out.dir, '/', location, '.',monthyr,'.rData',sep=''))

#load(file=paste(out.dir, 'jul.may08.rData', sep=''))
