
dates=c(as.Date("2008-08-15"), as.Date("2009-05-15"), as.Date("2009-11-15"), as.Date("2010-05-15"))

for(tdate in dates) { cat(as.Date(tdate, origin="1970-01-01"),'\n')
	text.date = format(as.Date(tdate, origin="1970-01-01"),"%Y%m%d") #get the date in the format needed
	
}
