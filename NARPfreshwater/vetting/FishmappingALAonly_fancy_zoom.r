#This script produces the attached images.  On top of this you can plot other information necessary for vetting.

library(SDMTools); library(maptools)#load the necessary libraries
source('/home/jc148322/scripts/libraries/cool_functions.r')

image.dir = "/home/jc246980/ALA_downloads/Distribution_Images/Fish_distributions"          #location of images
work.dir="/home/jc246980/ALA_downloads"             # location of full fish species list
data.dir="/home/jc246980/Janet_Stein_data"       # location of catchment and river files
ala.data = "/home/jc214262/Refugia/Vert_data/ALA_Vertebrate_data"      #location of ALA files
Sp_north.dir="/home/jc246980/NorthernOZFish/Data"   #location of northern fish data


base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

###################################
#create list of freshwater species that are not in northern database

FullList = as.data.frame(read.csv(paste(work.dir,"/","Full_Freshwater_Fish_List.csv",sep=""), header = T,sep = ",", fill = T))   # load full species list
NorthSpecies = list.files(Sp_north.dir)   # list north species csv files
northspeciesName = unlist(strsplit(NorthSpecies,".csv"));northspeciesName=gsub('_',' ',northspeciesName)     # remove '.csv' and substitute ' " for '_' in species names
otherfish <- as.matrix(FullList$full_name[!(FullList$full_name %in% northspeciesName)])    # identify only those species in the full list that don't occur in the northern species list
otherfishcsv = gsub(' ','_',otherfish); otherfishcsv=paste(otherfishcsv,'.csv', sep='')    # format otherfish names so they match ALA file names

###################################
#Bring in all the necessary information

#catchments = readShapePoly('/home/jc246980/Janet_Stein_data/Level2Catchments/NCBLevel2Drainage.shp') #read in your shapefile
#rivers = readShapeLines('/home/jc246980/Janet Stein data/Major_rivers/Major_rivers.shp')
#save(rivers,file=paste(data.dir,'rivers.Rdata',sep=''))
#save(catchments,file=paste(data.dir,'catchments.Rdata',sep=''))
#load("/home/jc246980/Janet_Stein_data/Reaches.Rdata")
load("/home/jc246980/Janet_Stein_data/Rivers.Rdata")
load("/home/jc246980/Janet_Stein_data/catchments.Rdata")

for (sp in 1:length(otherfishcsv)) { cat(otherfishcsv[sp],'\n')

      if(file.exists(paste(ala.data, '/',otherfishcsv[sp],sep=''))){
              species.data.ala = read.csv(paste(ala.data, '/',otherfishcsv[sp],sep=''))

             assign.list(min.lon,max.lon,min.lat,max.lat) %=% dynamic.zoom(species.data.ala$LONGDEC,species.data.ala$LATDEC, padding.percent=5)

              setwd(image.dir)



        ## this creates the mini-australia, called �clip�
        if (max.lat>=-18 & min.lat<=-34 |
                        max.lon>=148 & min.lon<=120 ) {
                        xlim=c(min(pos$lon),max(pos$lon));
                        ylim=c(min(pos$lat),max(pos$lat))
                        
        }else{
                        xlim=c(min.lon,max.lon);
                        ylim=c(min.lat,max.lat)
             }


              png(paste(otherfishcsv[sp],'.png',sep=''),width=dim(base.asc)[1]*2+30, height=dim(base.asc)[1]*2+80, units='px', pointsize=20, bg='white')
                              par(mar=c(2,2,2,2),cex=1,oma=c(3,0,1,0))

                              mat = matrix(c( 2,3,3,3,
                                                                                              1,1,1,1,
                                                                                              1,1,1,1,
                                                                                              1,1,1,1),nr=4,nc=4,byrow=TRUE) #create a layout matrix for images
                              layout(mat) #call layout as defined above

                              image(base.asc, ann=FALSE,axes=FALSE,col='white',  xlim=xlim,ylim=ylim)
                              clip(xlim[1],xlim[2],ylim[1],ylim[2])
                              image(base.asc, ann=FALSE,axes=FALSE,col='grey', xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)

                              plot(catchments, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='grey93', lwd=1)
                              plot(rivers, lwd=2, ann=FALSE,axes=FALSE, add=TRUE, col='cornflowerblue')

                              points(species.data.ala[,'LONGDEC'],species.data.ala[,'LATDEC'],pch=16,cex=2, col='red')


                              legend("bottomleft", inset = c(0.4, 0.2),
                              legend = c("ALA records"),
                              bty = "n", pch = c(19),           # bty = "n": no box
                              col = c(2), pt.cex = c(4),
                              lty = c(-1), cex=2)

                              assign.list(l,r,b,t) %=% par("usr")

                              image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=c(0,1))
                              image(clip.image(l,r,b,t),ann=FALSE,axes=FALSE, col="grey20",add=TRUE)


                              plot(1:20,axes=FALSE,ann=FALSE,type='n')
                              speciesname=gsub('_',' ',otherfishcsv[sp]);speciesname=gsub('.csv',' ',speciesname)
                              text(5,10,speciesname,cex=3)



              dev.off()
 }else{
      species.data.ala=NULL
      }
}
