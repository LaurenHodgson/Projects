library(SDMTools)	#load the necessary libraries
wd = '/home/jc148322/WallaceInitiative/'; setwd(wd);	#defin and set the working directory
out.dir='/home/jc148322/WallaceInitiative/for Jeremy/'; dir.create(out.dir)



library(SDMTools)	#load the necessary libraries
wd = 'C:/Users/jc148322/Documents/Work Directory/Jeremy Work Directory/world_richness/'; setwd(wd);
out.dir='C:/Users/jc148322/Documents/Work Directory/Jeremy Work Directory/world_richness/for Jeremy/'; dir.create(out.dir)


#plants/each individual animal

taxon = 'Plantae'	#define the taxon of interest
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
cur[which(cur==0)]=0.000001
sres = read.asc.gz(paste(taxon,'/2080_SRES_A1B_mean.asc.gz',sep=''))	#read in the no mitigation richness
avoid = read.asc.gz(paste(taxon,'/2080_A1B_A30r5l_mean.asc.gz',sep=''))	#read in the avoided richness
#sres[which(sres==0)]=0.000000001
#avoid[which(avoid==0)]=0.000000001
baseasc=cur
baseasc2=baseasc
sres = sres / cur; avoid = avoid / cur	#get the proportionate change in richness

sres[which(baseasc==0.000001)] = NA
avoid[which(baseasc==0.000001)] = NA
dif=avoid-sres

write.asc.gz(dif, paste(out.dir, 'Plantae.difference.asc', sep=''))
#write.asc.gz(sres, paste(out.dir, 'Plantae.sresa1b.asc', sep=''))
#write.asc.gz(avoid, paste(out.dir,  'Plantae.avoid.asc', sep=''))


#all animals

taxon='Amphibia'
cur = read.asc.gz(paste(taxon,'/current.asc.gz',sep=''))	#read in the current richness
baseasc=cur
baseasc[which(is.finite(baseasc))] = 0

setwd(out.dir)

pnts=cbind(x=c(-160,-180,-180,-160), y=c(75,75,100,100)) 

#read in your layers to be combined
amphibia.avoid=read.asc.gz('Amphibia.avoid.asc.gz')
amphibia.sres=read.asc.gz('Amphibia.sresa1b.asc.gz')


aves.avoid=read.asc.gz('Aves.avoid.asc.gz')
aves.sres=read.asc.gz('Aves.sresa1b.asc.gz')


mammalia.avoid=read.asc.gz('Mammalia.avoid.asc.gz')
mammalia.sres=read.asc.gz('Mammalia.sresa1b.asc.gz')


reptilia.avoid=read.asc.gz('Reptilia.avoid.asc.gz')
reptilia.sres=read.asc.gz('Reptilia.sresa1b.asc.gz')

#create data frames

all.avoid = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.avoid$amphibia = amphibia.avoid[cbind(all.avoid$row,all.avoid$col)]
all.avoid$aves=aves.avoid[cbind(all.avoid$row,all.avoid$col)]
all.avoid$mammalia=mammalia.avoid[cbind(all.avoid$row,all.avoid$col)]
all.avoid$reptilia=reptilia.avoid[cbind(all.avoid$row,all.avoid$col)]
all.avoid$count=rowSums(!is.na(all.avoid[3:6]))

for (ii in 3:6) {
	all.avoid[,ii]=all.avoid[,ii]/all.avoid$count

}

for (ii in 3:ncol(all.avoid)){
	all.avoid[which(all.avoid[,ii]==0),ii] = 0.00001
	}
for (ii in 3:ncol(all.avoid)){
	all.avoid[which(is.na(all.avoid[,ii])),ii] = 0
}

all.avoid$total=rowSums(all.avoid[3:6])
all.avoid$total[which(all.avoid$total==0)]=NA
all.avoid$total[which(all.avoid$total<=0.00005)]=0
#all.avoid$total[which(all.avoid$total>1)]=1.01

avoid=baseasc
avoid[cbind(all.avoid$row, all.avoid$col)] = all.avoid$total

#write.asc.gz(avoid, 'Animals.avoid.asc')

#

all.sres = as.data.frame(which(is.finite(baseasc), arr.ind=TRUE))
all.sres$amphibia = amphibia.sres[cbind(all.sres$row,all.sres$col)]
all.sres$aves=aves.sres[cbind(all.sres$row,all.sres$col)]
all.sres$mammalia=mammalia.sres[cbind(all.sres$row,all.sres$col)]
all.sres$reptilia=reptilia.sres[cbind(all.sres$row,all.sres$col)]
all.sres$count=rowSums(!is.na(all.sres[3:6]))

for (ii in 3:6) {
	all.sres[,ii]=all.sres[,ii]/all.sres$count

}

for (ii in 3:ncol(all.sres)){
	all.sres[which(all.sres[,ii]==0),ii] = 0.00001
}
for (ii in 3:ncol(all.sres)){
	all.sres[which(is.na(all.sres[,ii])),ii] = 0
}

all.sres$total=rowSums(all.sres[3:6])
all.sres$total[which(all.sres$total==0)]=NA
all.sres$total[which(all.sres$total<=0.00005)]=0
#all.sres$total[which(all.sres$total>1)]=1.01

sres=baseasc
sres[cbind(all.sres$row, all.sres$col)] = all.sres$total

#write.asc.gz(sres,'Animals.sresa1b.asc')

dif=avoid-sres

write.asc.gz(dif, 'Animals.difference.asc')
