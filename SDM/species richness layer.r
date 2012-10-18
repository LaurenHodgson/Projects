library(SDMTools)

wd='H:/Barra Work Directory/distributions_realized/'; setwd(wd) #define & set the working directory
out.dir = 'H:/Barra Work Directory/distributions_realized/outputs/'
species = list.files(,pattern='asc.gz') #get a list of the species distributions
richness = NULL #define the summar richness file
for (spp in species) {cat(spp,'\n') #cycle through each of hte species
	tasc = read.asc.gz(spp) #read in the species specific data
	tasc[which(is.finite(tasc) & tasc>0)] = 1 #set all parts of the species distribuiton to 1
	if (is.null(richness)) {richness = tasc; richness[is.finite(richness)] = 0 } #define the output & set everything = 0
	richness = richness + tasc #append the species-specific distribution info
}
write.asc.gz(richness, paste(out.dir,"sum_richness.asc",sep="")) #write out the richness data

#define some plot information
max.richness = max(richness, na.rm=TRUE) #get the max richness value
cols = c("gray", colorRampPalette(c("#CC9966","forestgreen","darkgreen","#003300"))(max.richness)) #get the colors for the plot
pnts = cbind(c(260000,290000,290000,260000),c(7830000,7830000,7920000,7920000)) #define the location of the legend gradient
#creat the plot
png(paste(out.dir,'spp_distrib.png',sep=''), width=4, height=7, units='cm', res=300, pointsize=5, bg='white') #start the plot
	par(mar=c(1,2,1,2)) #remove any plot margins
	image(richness, ann=FALSE,axes=FALSE,col=cols, zlim=c(0,max.richness)) #plot the richness
	legend.gradient(pnts,cols=cols,limits=c(0,max.richness), title='Richness', cex=1)
dev.off()
