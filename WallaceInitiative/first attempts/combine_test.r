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
#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B','#F0D082','#F0E391','#F0ECB4'))(49),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD','#4AB3B9','#189CB0'))(51),'#0F72A5')

pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100))  #redefine these
png(paste('All.gradient2.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext('All taxa (weighted by data contribution)',side=3, line=2, outer=T,cex=1.5, font=3)
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

all.avoid.block= as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.avoid.block$amphibia = amphibia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4
all.avoid.block$aves=aves.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4
all.avoid.block$mammalia=mammalia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4
all.avoid.block$reptilia=reptilia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4

for (ii in 3:ncol(all.avoid.block)){
	all.avoid.block[which(all.avoid.block[,ii]==0),ii] = 0.00001
}
for (ii in 3:ncol(all.avoid.block)){
all.avoid.block[which(is.na(all.avoid.block[,ii])),ii] = 0
}
all.avoid.block$total=rowSums(all.avoid.block[3:6])
all.avoid.block$total[which(all.avoid.block$total==0)]=NA
all.avoid.block$total[which(all.avoid.block$total<=0.00005)]=0

all.avoid.block$total[which(all.avoid.block$total>1)] = 1.5; 
all.avoid.block$total[which(all.avoid.block$total<1.5 & all.avoid.block$total>=0.5)] = 1; 
all.avoid.block$total[which(all.avoid.block$total<0.5 & all.avoid.block$total>0.00000001)] = 0.5; 
all.avoid.block$total[which(all.avoid.block$total<0.5)] = 0

avoid.block=baseasc
avoid.block[cbind(all.avoid.block$row, all.avoid.block$col)] = all.avoid.block$total

write.asc.gz(avoid.block, 'Animals.avoid.block.asc')

all.avoid.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.avoid.gradient$amphibia = amphibia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4
all.avoid.gradient$aves=aves.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4
all.avoid.gradient$mammalia=mammalia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4
all.avoid.gradient$reptilia=reptilia.avoid.gradient[cbind(all.avoid.gradient$row,all.avoid.gradient$col)]/4

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

write.asc.gz(avoid.gradient, 'Animals.avoid.gradient.asc')


all.sres.block= as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.sres.block$amphibia = amphibia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4
all.sres.block$aves=aves.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4
all.sres.block$mammalia=mammalia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4
all.sres.block$reptilia=reptilia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4

for (ii in 3:ncol(all.sres.block)){
	all.sres.block[which(all.sres.block[,ii]==0),ii] = 0.00001
	}
for (ii in 3:ncol(all.sres.block)){
	all.sres.block[which(is.na(all.sres.block[,ii])),ii] = 0
}

all.sres.block$total=rowSums(all.sres.block[3:6])
all.sres.block$total[which(all.sres.block$total==0)]=NA
all.sres.block$total[which(all.sres.block$total<=0.00005)]=0

all.sres.block$total[which(all.sres.block$total>1)] = 1.5; 
all.sres.block$total[which(all.sres.block$total<1.5 & all.sres.block$total>=0.5)] = 1; 
all.sres.block$total[which(all.sres.block$total<0.5 & all.sres.block$total>0.00000001)] = 0.5; 
all.sres.block$total[which(all.sres.block$total<0.5)] = 0


sres.block=baseasc
sres.block[cbind(all.sres.block$row, all.sres.block$col)] = all.sres.block$total

write.asc.gz(sres.block, 'Animals.sresa1b.block.asc')

all.sres.gradient = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.sres.gradient$amphibia = amphibia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4
all.sres.gradient$aves=aves.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4
all.sres.gradient$mammalia=mammalia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4
all.sres.gradient$reptilia=reptilia.sres.gradient[cbind(all.sres.gradient$row,all.sres.gradient$col)]/4

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

write.asc.gz(sres.gradient, 'Animals.sresa1b.gradient.asc')

zlim=c(0,1.5)
cols=c('#C62F1F','#F0E391','#4AB3B9','#0F72A5')

pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100))  #redefine these
png(paste('Animals.block.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.block,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend(-180,115, c('>100', '50-100', '<50', '0'), fill=cols[4:1], title='Richness (% of current)', cex=1.2, bty='n')

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.block,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext('All animals',side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()

zlim=c(0,1.01)

#cols=c('black',heat.colors(49),terrain.colors(51)[51:1],'darkgreen')
cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B','#F0D082','#F0E391','#F0ECB4'))(49),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD','#4AB3B9','#189CB0'))(51),'#0F72A5')

pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100))  #redefine these
png(paste('Animals.gradient.png',sep=''), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres.gradient,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid.gradient,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext('All animals',side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()






####################################################loop that i was too tired to figure out, hence the above.
sp=c('Amphibia','Aves','Mammalia','Plantae','Reptilia')
em=c('sresa1b','avoid')
tt=c('block','gradient')

tasc=NULL
all=NULL
for (ii in tt) {
	ii=ii
	if (length(tt)==1) { zlim=c(0,7.5); cols=c(heat.colors(3),'forestgreen') }
	if (length(tt)==2) { zlim=c(0,5.05); cols=c('#C62F1F',colorRampPalette(c('#EF613B', '#EF813F', '#F0BF6B','#F0D082','#F0E391','#F0ECB4'))(49),colorRampPalette(c('#B0DBC3','#90CEC0','#6EC1BD','#4AB3B9','#189CB0'))(51),'#0F72A5')

for (hh in em) {
	hh=hh
for (jj in sp) {
	jj=jj
	tasc=read.asc.gz(paste(jj,hh,ii,'asc.gz',sep='.'))
	if (length(all==0)){
		all=tasc
		} else {
		all=sum(all,tasc)
		}
	if(length(jj)==1) {sres = all}
	if(length(jj)==2) {avoid = all}		
}
	write.asc.gz(all,paste('All',hh,ii, 'asc', sep='.'))
	if (length(hh)==2){
png(paste('All',ii,'png',sep='.'), width=13, height=18, units='cm', res=300, pointsize=5, bg='white')
par(mfrow=c(2,1), mar=c(1,2,1,1),oma=c(1,1,7,1))

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(sres,ann=FALSE,axes=FALSE,zlim=zlim,col=cols,add=TRUE)

text (10, -60, 'SRES A1B', cex=1.5)
if (length(tt)==1) { legend(-180,115, c('>100', '50-100', '<50', '0'), fill=c('forestgreen',heat.colors(3)[3:1],'gray86'), title='Richness (% of current)', cex=1.2, bty='n')}
if (length(tt)==2) {legend.gradient(pnts,cols=cols, limits=c('0','>100'), title='Richness (% of current)', cex=1.5)}

image(baseasc,ann=FALSE,axes=FALSE,col='gray86')
image(avoid,ann=FALSE,axes=FALSE,zlim=zlim, col=cols,add=TRUE)

text (10, -60, 'AVOID (A1B_A30r5l)', cex=1.5)
title(main='Avoidable species richness loss - 2080', outer=T,line=4,cex.main=2, font.main=2)
mtext('All taxa',side=3, line=2, outer=T,cex=1.5, font=3)
dev.off()
}

}

}
}

