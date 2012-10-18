
network=read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')

network=network[,c('HydroID','SegmentNo','NextDownID')]
hydro.seg=network[,c('SegmentNo','HydroID',)]
seg.nextdown=network[,c('SegmentNo','NextDownID')]

final=merge(seg.nextdown,seg.hydro,by.x='NextDownID',by.y='HydroID',all.x=T)
final=final[,-1]
colnames(final)=c('SegmentNo','NextDownSegID')
final$NextDownSegID[which(is.na(final$NextDownSegID))]=-1

write.dbf(final,'/home/jc148322/NARPfreshwater/NetworkAttributes/NextDownSegID.dbf')

## -----------------------------------------------------------------

wd = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05'; setwd(wd)     # define and set working directory

load('Reach_runoff_5km.Rdata')
runoff=Reach_runoff
colnames(runoff)=c('SegmentNo','runoff')

network=read.dbf('/home/jc246980/Janet_Stein_data/NetworkAttributes.dbf')
network=network[,c('HydroID','SegmentNo')]

tdata=merge(runoff,network, by='SegmentNo')



	
	
	