#Calculate sd and deltas of future from current

library(SDMTools)

data.dir = "/home/jc246980/Hydrology.trials/"; setwd(data.dir)
lauren.dir="/home/jc148322/Hydrology.trials/";

ESs = c("RCP3PD", "RCP85")
future.dir.pr='/home/jc165798/working/NARP_stability/OZ_5km/data/monthly/pre/'
GCMs = list.files(future.dir.pr,pattern=ESs[1]); GCMs = gsub(paste(ESs[1],'_',sep=''),'',GCMs); GCMs = gsub('.Rdata','',GCMs); #get a list of GCMs
#YOISfut=seq(2015, 2085,10)
YOISfut=2085

load(paste(data.dir,'Q_run_curmean.Rdata')) # Q_run_curmean
load(paste(data.dir,'Q_run_cursd.Rdata'))     # Q_run_cursd

tdata_cursd=Q_run_cursd; tdata_cursd[which(tdata_cursd==0)]=1000000 #set zeros to 1 million
tdata_curmean=Q_run_curmean; tdata_curmean[which(tdata_curmean==0)]=1000000 #set zeros to 1 million


for (es in ESs){ cat(es,'\n')
    load(paste(data.dir,"output_data/",es,"_Q_run_future.Rdata", sep='')) # Q_run data for each emission scenario loaded
    fut_runoff = Q_run
        
        for (gcm in GCMs) { cat(gcm,'\n')
            cois=grep(gcm, colnames(fut_runoff)) #columns of interest. grep finds col number
            gcm_runoff=fut_runoff[,cois] #subset by col number
            
			Fut_runoff_delta=NULL
			Fut_runoff_sd=NULL
            for (yy in YOISfut) {cat(yy,'\n')
                cois=grep(yy, colnames(gcm_runoff))
                year_runoff=gcm_runoff[,cois]
                
                #do the work
                #SD ... Fut_runoff_sd=(year_runoff-Q_run_curmean)/Q_run_cursd
                tdata_sd=(year_runoff-Q_run_curmean)/tdata_cursd
                
                #delta ... year_runoff/Q_run_curmean
                year_runoff[which(year_runoff==0)]=1000000 #set zeros to 1 million - 1million/1million=1, which means proportion is 100%
                
                tdata_delta=year_runoff/tdata_curmean
				#make table of ES and gcm
				Fut_runoff_sd=cbind(Fut_runoff_sd,tdata_sd)
				
            } 
			save(Fut_runoff_sd, file=paste(lauren.dir,'runoff/sd/',es,'_',gcm,'_','Runoff_SD.Rdata',sep=''))
			save(Fut_runoff_delta, file=paste(lauren.dir,'runoff/delta/',es,'_',gcm,'_','Runoff_Delta.Rdata',sep=''))			 
		}

}

#reconsider saved data format
#make images of quantiles.