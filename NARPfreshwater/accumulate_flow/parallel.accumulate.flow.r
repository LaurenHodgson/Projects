qsub -l nodes=2 -l pmem=5gb -I
# module load R-2.15.1
library(SDMTools)
library(parallel)

flowdata=readLines('/scratch/jc155857/graph_code/out_final_processed.csv') #read in the list of flow relationships
runoff=read.csv('/home/jc148322/NARPfreshwater/final_runoff.csv') #read in runoff

accflow=matrix(NA, nr=length(flowdata),nc=2) #make an empty matrix to put data into

flowdata=strsplit(flowdata, ',') #strinsplit the flowdata on commas

accflow[,1] = as.vector(sapply(flowdata,'[',1)) #print the output of first HydroIDs to out table

flowdata=lapply(flowdata,as.numeric) #change flowdata from character to numeric

## -----------------------------------------------------------------------------------------
## accumulation function
accumulate = function(ids) {
tdata=runoff[which(runoff$HydroID %in% ids),]
headwater=setdiff(ids,tdata$NextDownID)

if(tdata$ChannelType[which(tdata$HydroID==headwater)]=='bi_sub') {
	fromNode=tdata$From_Node[which(tdata$HydroID==headwater)]
	bi_main=runoff$HydroID[which(runoff$From_Node==fromNode & runoff$ChannelType=='bi_main')]
	upID=which(accflow==bi_main)
	upIDs=flowdata[[upID]]
	upIDs=upIDs[which(upIDs!=bi_main)]
	ids=c(ids,upIDs)
	
	tdata=runoff[which(runoff$HydroID %in% ids),]
	headwater=setdiff(ids,tdata$NextDownID)

}

flow=tdata$Runoff[which(tdata$HydroID==headwater)]
nextdown=tdata$NextDownID[which(tdata$HydroID==headwater)]
repeat{
	addflow=tdata$Runoff[which(tdata$HydroID==nextdown)]*tdata$BiProp[which(tdata$HydroID==nextdown)]
	if (length(addflow)==0) { break }	
	flow=flow+addflow
	nextdown=tdata$NextDownID[which(tdata$HydroID==nextdown)]
}
return(flow)}

## -----------------------------------------------------------------------------------------


ncore = 10 #define the number of cores in teh cluster
cl <- makeCluster(getOption("cl.cores", ncore)) #define a cluster with the correct number of cores 
clusterExport(cl,'runoff') #export necessary objects to cores in cluster

accflow[,2]=parSapply(cl, flowdata, accumulate(ids)) 
stopCluster(cl)

write.csv(accflow,'/home/jc148322/NARPfreshwater/acc_flow.csv',row.names=F)

## -----------------------------------------------------------------------------------------
## testing on a subsample

subdata=flowdata[1:100]
subacc=accflow[1:100,]

ncore = 10 #define the number of cores in teh cluster
cl <- makeCluster(getOption("cl.cores", ncore)) #define a cluster with the correct number of cores 
clusterExport(cl,'runoff') #export necessary objects to cores in cluster

subacc[,2]=parSapply(cl, subdata, accumulate(ids)) 
stopCluster(cl)

