library('RAtmosphere')
library('SDMTools')

#get lats and lons
work.dir='/home/jc148322/flatdata/';setwd(work.dir)

load('latlon.rData')

#vector of mid month dates
dates=c(as.Date("2012-01-15"),
		as.Date("2012-02-15"),
		as.Date("2012-03-15"),
		as.Date("2012-04-15"),
		as.Date("2012-05-15"),
		as.Date("2012-06-15"),
		as.Date("2012-07-15"),
		as.Date("2012-08-15"),
		as.Date("2012-09-15"),
		as.Date("2012-10-15"),
		as.Date("2012-11-15"),
		as.Date("2012-12-15"))

#get julian date
out=NULL
for(tdate in dates) { cat(as.Date(tdate, origin="1970-01-01"),'\n')
	text.date = format(as.Date(tdate, origin="1970-01-01"),"%Y%m%d") #get the date in the format needed
	out = rbind(out,data.frame(year=format(as.Date(tdate, origin="1970-01-01"),"%Y"),
	month=format(as.Date(tdate, origin="1970-01-01"),"%m"),
	day=format(as.Date(tdate, origin="1970-01-01"),"%d"),
	julian.day=tdate)) #append the data out to a data frame
	#wt=NA,gain=NA,Me=NA,Me.sum=NA,percent.gain=NA)) #append more rows from a new start date if necessary
}


julian.day=out$julian.day

pos$jan=julian.day[1]
pos$feb=julian.day[2]
pos$mar=julian.day[3]
pos$april=julian.day[4]
pos$may=julian.day[5]
pos$june=julian.day[6]
pos$july=julian.day[7]
pos$aug=julian.day[8]
pos$sept=julian.day[9]
pos$oct=julian.day[10]
pos$nov=julian.day[11]
pos$dec=julian.day[12]

for (ii in 5:16) { cat(ii,'\n')
	times=suncalc(pos[ii],Lat=pos$lat, Long=pos$lon)
	times$hours=times$sunset-times$sunrise
	pos[ii]=times$hours
	}

save(pos, file='photoperiod.rData')
	
	
	
	

	
	
base.asc = read.asc(paste(wd,'base.asc',sep=''))
	
photoperiod.asc=base.asc
photoperiod.asc[cbind(pos$row,pos$col)]=pos$total
write.asc.gz(photoperiod.asc,'photoperiod.asc')

