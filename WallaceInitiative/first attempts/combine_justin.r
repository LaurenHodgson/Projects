library(SDMTools)	#load the necessary libraries
wd = '/home/22/jc148322/WallaceInitiative'; setwd(wd);	#defin and set the working directory

taxon='Amphibia'
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
baseasc=cur
baseasc[which(is.finite(baseasc))] = 0


pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100)) 

#read in your layers to be combined
amphibia.avoid.gradient=read.asc.gz('Amphibia.avoid.gradient.asc.gz')
amphibia.sres.gradient=read.asc.gz('Amphibia.sresa1b.gradient.asc.gz')


aves.avoid.gradient=read.asc.gz('Aves.avoid.gradient.asc.gz')
aves.sres.gradient=read.asc.gz('Aves.sresa1b.gradient.asc.gz')


mammalia.avoid.gradient=read.asc.gz('Mammalia.avoid.gradient.asc.gz')
mammalia.sres.gradient=read.asc.gz('Mammalia.sresa1b.gradient.asc.gz')


plantae.avoid.gradient=read.asc.gz('Plantae.avoid.gradient.asc.gz')
plantae.sres.gradient=read.asc.gz('Plantae.sresa1b.gradient.asc.gz')


reptilia.avoid.gradient=read.asc.gz('Reptilia.avoid.gradient.asc.gz')
reptilia.sres.gradient=read.asc.gz('Reptilia.sresa1b.gradient.asc.gz')

#create data frames

all.avoid.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.avoid.gradient$amphibia = amphibia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$aves=aves.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$mammalia=mammalia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$plantae=plantae.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$reptilia=reptilia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$count=rowSums(!is.na(all.avoid.gradient[3:7]))

for (ii in 3:7) {
	all.avoid.gradient[,ii]=all.avoid.gradient[,ii]/all.avoid.gradient$count

}

for (ii in 3:ncol(all.avoid.gradient)){
	all.avoid.gradient[which(all.avoid.gradient[,ii]==0),ii] = 0.00001
	}
for (ii in 3:ncol(all.avoid.gradient)){
	all.avoid.gradient[which(is.na(all.avoid.gradient[,ii])),ii] = 0
}

all.avoid.gradient$total=rowSums(all.avoid.gradient[3:7])
all.avoid.gradient$total[which(all.avoid.gradient$total==0)]=NA
all.avoid.gradient$total[which(all.avoid.gradient$total<=0.00005)]=0
all.avoid.gradient$total[which(all.avoid.gradient$total>1)]=1.01

avoid.gradient=baseasc
avoid.gradient[cbind(all.avoid.gradient$row, all.avoid.gradient$col)] = all.avoid.gradient$total

#write.asc.gz(avoid.gradient, 'All.avoid.gradient2.asc')

#

all.sres.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.sres.gradient$amphibia = amphibia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$aves=aves.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$mammalia=mammalia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$plantae=plantae.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$reptilia=reptilia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$count=rowSums(!is.na(all.sres.gradient[3:7]))

for (ii in 3:7) {
	all.sres.gradient[,ii]=all.sres.gradient[,ii]/all.sres.gradient$count

}

for (ii in 3:ncol(all.sres.gradient)){
	all.sres.gradient[which(all.sres.gradient[,ii]==0),ii] = 0.00001
}
for (ii in 3:ncol(all.sres.gradient)){
	all.sres.gradient[which(is.na(all.sres.gradient[,ii])),ii] = 0
}

all.sres.gradient$total=rowSums(all.sres.gradient[3:7])
all.sres.gradient$total[which(all.sres.gradient$total==0)]=NA
all.sres.gradient$total[which(all.sres.gradient$total<=0.00005)]=0
all.sres.gradient$total[which(all.sres.gradient$total>1)]=1.01

sres.gradient=baseasc
sres.gradient[cbind(all.sres.gradient$row, all.sres.gradient$col)] = all.sres.gradient$total

#write.asc.gz(sres.gradient, 'All.sresa1b.gradient2.asc')

zlim=c(0,1.01)
#cols='red3','goldenrod2', 'forestgreen','dodgerblue4'
#cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B'))(24),colorRampPalette(c('#F0D082','#F0E391','#F0ECB4'))(25),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD'))(26),colorRampPalette(c('#4AB3B9','#189CB0','#0F72A5'))(33),'dodgerblue4')
#cols=c('red4',colorRampPalette(c('red3', 'orange','lightgoldenrod1'))(100),'darkolivegreen')

cols=c('red4',colorRampPalette(c('red3', 'tan3','wheat2'))(100),'darkolivegreen')
cols.legend=c('red4',colorRampPalette(c('red3', 'tan3','wheat2'))(100))
cols2=c('darkgreen','darkgreen')


pnts2=cbind(x=c(-150,-170,-170,-150), y=c(-20,-20,-17.5,-17.5))  #redefine these
pnts=cbind(x=c(-150,-170,-170,-150), y=c(-25,-25,-50,-50)) 
png(paste('All.gradient_justin.png',sep=''), width=13, height=11, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(0,0,0,0)+0.1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (0, -50, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols.legend, limits=c('0','99'), title='',cex=1)
legend.gradient(pnts2,cols=cols2, limits=c('','100+'), title='Richness (% of current)', cex=1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (0, -50, 'AVOID (A1B_A30r5l)', cex=1.5)
dev.off()


#avoid.gradient[which(avoid.gradient<=0.75)]=2
#avoid.gradient[which(avoid.gradient<2)]=3

#sres.gradient[which(sres.gradient<=0.75)]=2
#sres.gradient[which(sres.gradient<2)]=3


amphibia.NA=baseasc
amphibia.NA[which(is.finite(amphibia.avoid.gradient))]=NA
aves.NA=baseasc
aves.NA[which(is.finite(aves.avoid.gradient))]=NA
plantae.NA=baseasc
plantae.NA[which(is.finite(plantae.avoid.gradient))]=NA
mammalia.NA=baseasc
mammalia.NA[which(is.finite(mammalia.avoid.gradient))]=NA
reptilia.NA=baseasc
reptilia.NA[which(is.finite(reptilia.avoid.gradient))]=NA

zlim=c(2,3)
cols=c('#F0E391','#4AB3B9')

pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100))  #redefine these
png(paste('All.block25.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend(-180,115, c('75-100', '<75'), fill=cols[2:1], title='Richness (% of current)', cex=1.2, bty='n')

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext('All taxa',side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()

zlim=c(0,1.01)

#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
cols=c('red4',colorRampPalette(c('#C62F1F','#EF613B', '#EF813F', '#F0BF6B','#F0D082'))(74),colorRampPalette(c('#6EC1BD','#4AB3B9','#189CB0','#0F72A5'))(25),'midnightblue')
white.col=adjustcolor('white',alpha.f=0.45)


pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100))  #redefine these
png(paste('All.gradient25.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)
image(amphibia.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(reptilia.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(mammalia.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(plantae.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(aves.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)
image(amphibia.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(reptilia.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(mammalia.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(plantae.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)
image(aves.NA,ann=FALSE,axes=FALSE,col=white.col, add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext('All taxa',side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()


#percent of world losing 50% of species
cs=ClassStat(avoid.gradient, latlon=TRUE)
cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
#  9457.794   136621.886 
# NAarea=  146361-136621.886-9457.794 = 281.32
# >50%area = 136621.886 /146361*100 = 93.34583
# <50%area = 9457.794/146361*100 = 6.461963
cs=ClassStat(sres.gradient, latlon=TRUE)
cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
# 25829.39    120250.29 
# NAarea=146361-120250.29-25829.39 = 281.32
# >50%area = 120250.29/146361*100 = 82.16006
# <50%area = 25829.39/146361*100 = 17.64773

cs=ClassStat(baseasc, latlon=TRUE)
cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
# 146361 

#percent of world losing 25% of species
cs=ClassStat(avoid.gradient, latlon=TRUE)
cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
#  53965.24      92114.44 
# NAarea=146361-120250.29-25829.39 = 281.32
# >75%area =  92114.44/146361*100 = 62.93647
# <75%area = 53965.24/146361*100 = 36.87133
cs=ClassStat(sres.gradient, latlon=TRUE)
cs$total.area = cs$total.area / 1000000000 #area, 1000s km^2
# 89413.22      56666.47 
# NAarea=146361-120250.29-25829.39 = 281.32
# >75%area = 56666.47/146361*100 = 38.71692
# <75%area = 89413.22/146361*100 = 61.09088







######remove plants

#create data frames

all.avoid.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.avoid.gradient$amphibia = amphibia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$aves=aves.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$mammalia=mammalia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$reptilia=reptilia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]
all.avoid.gradient$count=rowSums(!is.na(all.avoid.gradient[3:6]))

for (ii in 3:6) {
	all.avoid.gradient[,ii]=all.avoid.gradient[,ii]/all.avoid.gradient$count

}

for (ii in 3:ncol(all.avoid.gradient)){
	all.avoid.gradient[which(all.avoid.gradient[,ii]==0),ii] = 0.00001
	}
for (ii in 3:ncol(all.avoid.gradient)){
	all.avoid.gradient[which(is.na(all.avoid.gradient[,ii])),ii] = 0
}

all.avoid.gradient$total=rowSums(all.avoid.gradient[3:6])
all.avoid.gradient$total[which(all.avoid.gradient$total==0)]=NA
all.avoid.gradient$total[which(all.avoid.gradient$total<=0.00005)]=0
all.avoid.gradient$total[which(all.avoid.gradient$total>1)]=1.01

avoid.gradient=baseasc
avoid.gradient[cbind(all.avoid.gradient$row, all.avoid.gradient$col)] = all.avoid.gradient$total

#write.asc.gz(avoid.gradient, 'All.avoid.gradient2.asc')

#

all.sres.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.sres.gradient$amphibia = amphibia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$aves=aves.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$mammalia=mammalia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$reptilia=reptilia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]
all.sres.gradient$count=rowSums(!is.na(all.sres.gradient[3:6]))

for (ii in 3:6) {
	all.sres.gradient[,ii]=all.sres.gradient[,ii]/all.sres.gradient$count

}

for (ii in 3:ncol(all.sres.gradient)){
	all.sres.gradient[which(all.sres.gradient[,ii]==0),ii] = 0.00001
}
for (ii in 3:ncol(all.sres.gradient)){
	all.sres.gradient[which(is.na(all.sres.gradient[,ii])),ii] = 0
}

all.sres.gradient$total=rowSums(all.sres.gradient[3:6])
all.sres.gradient$total[which(all.sres.gradient$total==0)]=NA
all.sres.gradient$total[which(all.sres.gradient$total<=0.00005)]=0
all.sres.gradient$total[which(all.sres.gradient$total>1)]=1.01

sres.gradient=baseasc
sres.gradient[cbind(all.sres.gradient$row, all.sres.gradient$col)] = all.sres.gradient$total

#write.asc.gz(sres.gradient, 'All.sresa1b.gradient2.asc')

zlim=c(0,1.01)
#cols='red3','goldenrod2', 'forestgreen','dodgerblue4'
#cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B'))(24),colorRampPalette(c('#F0D082','#F0E391','#F0ECB4'))(25),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD'))(26),colorRampPalette(c('#4AB3B9','#189CB0','#0F72A5'))(33),'dodgerblue4')

cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B'))(33),colorRampPalette(c('#F0D082','#F0E391','#F0ECB4'))(34),colorRampPalette(c('#90CEC0','#4AB3B9','#0F72A5'))(33),'dodgerblue4')

pnts=cbind(x=c(-150,-170,-170,-150), y=c(-25,-25,-50,-50))  #redefine these
png(paste('Animals.gradient4.png',sep=''), width=13, height=11, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(0,0,0,0)+0.1)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (0, -50, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (0, -50, 'AVOID (A1B_A30r5l)', cex=1.5)
dev.off()
