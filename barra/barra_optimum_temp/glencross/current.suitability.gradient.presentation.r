library(SDMTools)

#directory locations
work.dir = "E:/Barra Work Directory/outputs/glencross/map_barplot_trial/"
esoclim.dir= "E:/Barra Work Directory/current.esoclim/"
out.dir = "E:/Barra Work Directory/outputs/presentation/"; dir.create(out.dir)

pnts=cbind(x=c(112,116,116,112), y=c(-11,-11,-18.5,-18.5))

cols = colorRampPalette(c('red3','goldenrod2', 'forestgreen','dodgerblue4'))(100)
zlim=c(0,3500)

Tmean=read.asc.gz(paste(esoclim.dir, 'tasmax01.asc.gz', sep=''))
out=read.asc.gz(paste(work.dir, 'current_esoclim.yearly.growth.in.grams.asc.gz', sep=''))

png(paste(out.dir, 'current.gradient.new.png',sep=''), width=10, height=9, units='cm', res=300, pointsize=5, bg='transparent')
par(mar=c(0,1,0,1))
	image(Tmean, ann=FALSE,axes=FALSE, col="gray88")
	image(out, ann=FALSE,axes=FALSE, zlim=zlim, col=cols, add=TRUE)
legend.gradient(pnts,cols=cols, limits=c('Low', 'High'), title='Current suitability for pond-based aquaculture', cex=2)

dev.off()


