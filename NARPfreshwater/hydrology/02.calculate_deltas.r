#Calculate sd and deltas of future from current

library(SDMTools)

data.dir = "/home/jc165798/working/NARP_hydro/output"; setwd(data.dir)
delta.dir="/home/jc148322/NARPfreshwater/deltas/";

tfiles=list.files(pattern='RCP')
tt = strsplit(tfiles,'\\.') #string split the file names
vois = ESs = GCMs = YEARs = NULL #set all to null
for (tval in tt) { vois=c(vois,tval[1]);ESs = c(ESs,tval[2]); GCMs = c(GCMs,tval[3]); YEARs = c(YEARs, tval[4])} #extract all possible values
vois=unique(vois); ESs = unique(ESs); GCMs = unique(GCMs); YEARs=unique(YEARs) #keep only unique

YEARs=2085
ESs='RCP85'
vois=vois[2]
for (voi in vois){ cat(voi,'\n')
	curdata=get(load(paste(voi,'.current.Rdata',sep='')))

	out.dir=paste(delta.dir,voi,'/',sep='');dir.create(out.dir)
	for (es in ESs){ cat(es,'\n')
		for (gcm in GCMs) { cat(gcm,'\n')
			for (yr in YEARs) { cat(yr,'\n')
			
				futdata=get(load(paste(voi,'.',es,'.',gcm,'.',yr,'.Rdata', sep=''))) # Q_run data for each emission scenario loaded

				#delta = year_runoff/Q_run_curmean
				delta=futdata/curdata
				delta[which(is.nan(delta))]=1 #0/0 = NaN ...1=no change
				delta[which(!is.finite(delta))]=1000 #1/0 = Inf ... this is a big 'proportional' increase, so give it a big number.
				} 
				save(delta, file=paste(out.dir,voi,'.',es,'.',gcm,'.',yr,'.Rdata',sep=''))			 
			}
	}
}

