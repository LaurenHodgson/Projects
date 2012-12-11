library(SDMTools)
wd='/home/jc165798/working/AWT.NERP/future.SDM/richness/';setwd(wd)

base.asc=read.asc.gz('base.asc.gz')
pos=read.csv('base.pos.csv',as.is=TRUE)
cols = colorRampPalette(c("tan","forestgreen"))(100) #set colour ramp
pnts = cbind(c(146.25,146.75,146.75,146.25),c(-15.75,-15.75,-16.75,-16.75)) #define the location of the legend gradient

files=list.files(pattern='RCP')
ESs=NULL
for (tfile in files) {
	load(tfile)
	tname=strsplit(tfile,'\\.')[[1]][1]
	assign(tname,out)
	ESs=c(ESs,tname)
}

zmin=min(c(min(get(ESs[1]),na.rm=T),min(get(ESs[2]),na.rm=T),min(get(ESs[3]),na.rm=T),min(get(ESs[4]),na.rm=T)))
zmax=max(c(max(get(ESs[1]),na.rm=T),max(get(ESs[2]),na.rm=T),max(get(ESs[3]),na.rm=T),max(get(ESs[4]),na.rm=T)))
zlim=c(zmin,zmax)

for (es in ESs) {
	png(paste(es,'.png',sep=''),width=dim(base.asc)[1]*8+30, height=dim(base.asc)[2]*3+30, units='px', pointsize=20, bg='white')
		par(mar=c(1,3,3,1),mfrow=c(3,8),cex=1,oma=c(1,1,1,1))
		mat = matrix(c (1,4,7,10,13,16,19,22,
						1,4,7,10,13,16,19,22,
						2,5,8,11,14,17,20,23,
						2,5,8,11,14,17,20,23,
						3,6,9,12,15,18,21,24,
						3,6,9,12,15,18,21,24),nr=6,nc=8,byrow=TRUE) #create a layout matrix for images
		layout(mat) #call layout as defined above
		for (i in 1:24) { cat(i, '\n')
			tasc=base.asc; tasc[cbind(pos$row,pos$col)]=get(es)[,i+1]
			image(tasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim)
			if (i==1) { mtext('2015', line=1,side=3,cex=3,font=2)
						mtext('10th percentile', line=1,side=2, cex=3, font=3)}
			if (i==2) { mtext('50th percentile', line=1,side=2, cex=3, font=3)}
			if (i==3) { mtext('90th percentile', line=1,side=2, cex=3, font=3)} 
			if (i==4) { mtext('2025', line=1,side=3,cex=3,font=2)}
			if (i==7) { mtext('2035', line=1,side=3,cex=3,font=2)}			
			if (i==10) { mtext('2045', line=1,side=3,cex=3,font=2)}
			if (i==13) { mtext('2055', line=1,side=3,cex=3,font=2)}
			if (i==16) { mtext('2065', line=1,side=3,cex=3,font=2)}
			if (i==19) { mtext('2075', line=1,side=3,cex=3,font=2)}
			if (i==22) { mtext('2085', line=1,side=3,cex=3,font=2)
						legend.gradient(pnts,cols=cols,limits=zlim, title='Richness', cex=5)}

		}
		dev.off()
}

