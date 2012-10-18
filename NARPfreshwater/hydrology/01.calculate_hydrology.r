#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

####################################################################################
####################################################################################
#required to build code OUTSIDE R
# cd /home/jc165798/SCRIPTS/sdmcode/R_development/hydrology

# R CMD SHLIB Budyko.c # compiles c for use in R

####################################################################################
#START R

####################################################################################
library(SDMTools) #load the necessary library
dyn.load('/home/jc165798/SCRIPTS/sdmcode/R_development/hydrology/Budyko.so') #load the C code

###set directories
wd = '/home/jc165798/working/NARP_hydro/'; setwd(wd) #deifne and set the working directory
climdir = '/home/jc165798/Climate/CIAS/Australia/1km/' #define the climate directory

####read in all necessary inputs
base.asc = read.asc.gz(paste(climdir,'baseline.76to05/base.asc.gz',sep='')) #read in the base asc file
pos = read.csv(paste(climdir,'baseline.76to05/base.positions.csv',sep=''),as.is=TRUE)
pos$PAWHC = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/solpawhc.asc')) #append data to pos
pos$kRs = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/kRs_5km.asc'))
pos$DEM = extract.data(cbind(pos$lon,pos$lat),read.asc('inputs/dem_36sec.asc'))

ESs=list.files(paste(climdir,'monthly_csv/',sep=''),pattern='RCP')
GCMs=list.files(paste(climdir,'monthly_csv/RCP3PD/',sep=''))
YEARs=seq(2015,2085,10)

for (es in ESs) {
	
	for (gcm in GCMs){
		
		for (yr in YEARs){
			tmin = read.csv(paste(climdir, es,'/', gcm,'/tmn.matrix.', yr, '.csv.gz',sep=''),as.is=TRUE)[,-c(1:2)] #read in monthly tmin
			tmax = read.csv(paste(climdir, es,'/', gcm,'/tmx.matrix.', yr, '.csv.gz',sep=''),as.is=TRUE)[,-c(1:2)] #read in monthly tmax
			pr = read.csv(paste(climdir, es,'/', gcm,'/pre.matrix.', yr, '.csv.gz',sep=''),as.is=TRUE)[,-c(1:2)] #read in precipitatation

			###run the analysis and write out data
			tt = .Call('RunBudykoBucketModel_5km',
				pos$dem, #dem info
				as.matrix(pr), # monthly precip
				as.matrix(tmin), #monthly tmin
				as.matrix(tmax), #monthly tmax
				pos$PAWHC, #soil water holding capacity
				pos$kRs, #unknown kRs values 
				pos$lat / (180/pi), #latitudes in radians
				nrow(pr) #number of rows that need run
			)
			Eact = tt[[1]]; save(Eact, file=paste('output/',es,'/',gcm,'/Eact.',yr,'.Rdata',sep='')) #save the actual evapotranspiration
			Epot = tt[[2]]; save(Epot, file=paste('output/',es,'/',gcm,'/Epot.',yr,'.Rdata',sep='')) #save the potential evapotranspiration
			Qrun = tt[[3]]; save(Qrun, file=paste('output/',es,'/',gcm,'/Qrun.',yr,'.Rdata',sep='')) #save the runoff
			Rnet = tt[[4]]; save(Rnet, file=paste('output/',es,'/',gcm,'/Rnet.',yr,'.Rdata',sep='')) #save the net radiation
		}
	}
}