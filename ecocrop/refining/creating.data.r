wd = '/home/22/jc148322/Ecocrop/webdata/'; setwd(wd) #set the working directory
species = read.csv('species.csv',as.is=TRUE)
for (sppcode in species$Code) { cat(sppcode,'\n')
	system(paste('wget http://ecocrop.fao.org/ecocrop/srv/en/dataSheet?id=',sppcode,sep=''))
	system(paste('mv dataSheet\\?id\\=',sppcode,' ',sppcode,'.html',sep=''))
}

n=list.files(pattern='.html')
length(n)

headings=c('NAME','SCIENTNAME','CODE','LIFESPAN','LATMN','LATMX','ALTMN','ALTMX','GMIN','GMAX','KTMP','TMIN','TOPMN','TOPMX','TMAX','RMIN','ROPMN','ROPMX','RMAX','PP','TEXT','TEXTR','DEP','DEPR','DRA','DRAR','PHMIN','PHOPMN','PHOPMX','PHMAX','SAL','SALR','FER','FERR','LIMITS','CLIMZONE')

ECOcrops= matrix(NA, nr = length(n), nc=length(headings), dimnames=list(1:length(n),headings))
ECOcrops=as.data.frame(ECOcrops)
###get the data
ECOcrops$CODE=species$Code

for (sppcode in ECOcrops$CODE) { cat(sppcode,'\n')

rowname=which(ECOcrops$CODE==sppcode)

c=1
next.col=function(col){col=col+1;return(col)}

sppdata=readLines(paste(sppcode,'.html',sep=''))

#get SCIENTNAME
scientname=strsplit(sppdata[104],'<')
scientname=strsplit(scientname[[1]][2],'>')
scientname=scientname[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=scientname

#get CODE
c=next.col(c)
ECOcrops[rowname,c]=sppcode

#get LIFESPAN
lifespan=strsplit(sppdata[116],'<')
lifespan=strsplit(lifespan[[1]][4],'>')
lifespan=lifespan[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=lifespan

#get LATMN & LATMX
lat=strsplit(sppdata[137],'<')
latmn=strsplit(lat[[1]][8],'>')
latmx=strsplit(lat[[1]][10],'>')
latmn=latmn[[1]][2]
latmx=latmx[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=latmn
c=next.col(c)
ECOcrops[rowname,c]=latmx

#get ALTMN & ALTMX
alt=strsplit(sppdata[144],'<')
altmn=strsplit(alt[[1]][3],'>')
altmx=strsplit(alt[[1]][5],'>')
altmn=altmn[[1]][2]
altmx=altmx[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=altmn
c=next.col(c)
ECOcrops[rowname,c]=altmx

#get GMIN & GMAX
g=strsplit(sppdata[177],'<')
gmin=strsplit(g[[1]][8],'>')
gmax=strsplit(g[[1]][10],'>')
gmin=gmin[[1]][2]
gmax=gmax[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=gmin
c=next.col(c)
ECOcrops[rowname,c]=gmax

#get KTMP
ktmp=strsplit(sppdata[159],'<')
ktmp=strsplit(ktmp[[1]][4],'>')
ktmp=ktmp[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=ktmp

#get TMIN, TOPMN, TOPMX, & TMAX
t=strsplit(sppdata[131],'<')
topmn=strsplit(t[[1]][4],'>')
topmx=strsplit(t[[1]][6],'>')
tmin=strsplit(t[[1]][8],'>')
tmax=strsplit(t[[1]][10],'>')

tmin=tmin[[1]][2]
topmn=topmn[[1]][2]
topmx=topmx[[1]][2]
tmax=tmax[[1]][2]

c=next.col(c)
ECOcrops[rowname,c]=tmin
c=next.col(c)
ECOcrops[rowname,c]=topmn
c=next.col(c)
ECOcrops[rowname,c]=topmx
c=next.col(c)
ECOcrops[rowname,c]=tmax

#get RMIN, ROPMN, ROPMX, & RMAX
r=strsplit(sppdata[134],'<')
ropmn=strsplit(r[[1]][4],'>')
ropmx=strsplit(r[[1]][6],'>')
rmin=strsplit(r[[1]][8],'>')
rmax=strsplit(r[[1]][10],'>')

rmin=rmin[[1]][2]
ropmn=ropmn[[1]][2]
ropmx=ropmx[[1]][2]
rmax=rmax[[1]][2]

c=next.col(c)
ECOcrops[rowname,c]=rmin
c=next.col(c)
ECOcrops[rowname,c]=ropmn
c=next.col(c)
ECOcrops[rowname,c]=ropmx
c=next.col(c)
ECOcrops[rowname,c]=rmax

#get PP
pp=strsplit(sppdata[156],'<')
pp=strsplit(pp[[1]][8],'>')
pp=pp[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=pp

#get TEXT & TEXTR
tx=strsplit(sppdata[131],'<')
tex=strsplit(tx[[1]][14],'>')
textr=strsplit(tx[[1]][16],'>')
tex=tex[[1]][2]
textr=textr[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=tex
c=next.col(c)
ECOcrops[rowname,c]=textr

#get DEP & DEPR
d=strsplit(sppdata[128],'<')
dep=strsplit(d[[1]][14],'>')
depr=strsplit(d[[1]][16],'>')
dep=dep[[1]][2]
depr=depr[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=dep
c=next.col(c)
ECOcrops[rowname,c]=depr

#get DRA & DRAR
d=strsplit(sppdata[147],'<')
dra=strsplit(d[[1]][14],'>')
drar=strsplit(d[[1]][16],'>')
dra=dra[[1]][2]
drar=drar[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=dra
c=next.col(c)
ECOcrops[rowname,c]=drar

#get PHMIN, PHOPMN, PHOPMX, & PHMAX
ph=strsplit(sppdata[147],'<')
phopmn=strsplit(ph[[1]][4],'>')
phopmx=strsplit(ph[[1]][6],'>')
phmin=strsplit(ph[[1]][8],'>')
phmax=strsplit(ph[[1]][10],'>')

phmin=phmin[[1]][2]
phopmn=phopmn[[1]][2]
phopmx=phopmx[[1]][2]
phmax=phmax[[1]][2]

c=next.col(c)
ECOcrops[rowname,c]=phmin
c=next.col(c)
ECOcrops[rowname,c]=phopmn
c=next.col(c)
ECOcrops[rowname,c]=phopmx
c=next.col(c)
ECOcrops[rowname,c]=phmax

#get SAL & SALR
s=strsplit(sppdata[144],'<')
sal=strsplit(s[[1]][9],'>')
salr=strsplit(s[[1]][11],'>')
sal=sal[[1]][2]
salr=salr[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=sal
c=next.col(c)
ECOcrops[rowname,c]=salr

#get FER & FERR
f=strsplit(sppdata[134],'<')
fer=strsplit(f[[1]][14],'>')
ferr=strsplit(f[[1]][16],'>')
fer=fer[[1]][2]
ferr=ferr[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=fer
c=next.col(c)
ECOcrops[rowname,c]=ferr

#get limits
l=strsplit(sppdata[162],'<')
l=strsplit(l[[1]][8],'>')
l=l[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=l

#get climzone
z=strsplit(sppdata[156],'<')
z=strsplit(z[[1]][4],'>')
z=z[[1]][2]
c=next.col(c)
ECOcrops[rowname,c]=z
}

ECOcrops$LATMN=as.numeric(ECOcrops$LATMN)
ECOcrops$LATMX=as.numeric(ECOcrops$LATMX)
ECOcrops$ALTMN=as.numeric(ECOcrops$ALTMN)
ECOcrops$ALTMX=as.numeric(ECOcrops$ALTMX)
ECOcrops$GMIN=as.numeric(ECOcrops$GMIN)
ECOcrops$GMAX=as.numeric(ECOcrops$GMAX)
ECOcrops$KTMP=as.numeric(ECOcrops$KTMP)
ECOcrops$TMIN=as.numeric(ECOcrops$TMIN)
ECOcrops$TOPMN=as.numeric(ECOcrops$TOPMN)
ECOcrops$TOPMX=as.numeric(ECOcrops$TOPMX)
ECOcrops$TMAX=as.numeric(ECOcrops$TMAX)
ECOcrops$RMIN=as.numeric(ECOcrops$RMIN)
ECOcrops$ROPMN=as.numeric(ECOcrops$ROPMN)
ECOcrops$ROPMX=as.numeric(ECOcrops$ROPMX)
ECOcrops$RMAX=as.numeric(ECOcrops$RMAX)
ECOcrops$PHMIN=as.numeric(ECOcrops$PHMIN)
ECOcrops$PHOPMN=as.numeric(ECOcrops$PHOPMN)
ECOcrops$PHOPMX=as.numeric(ECOcrops$PHOPMX)
ECOcrops$PHMAX=as.numeric(ECOcrops$PHMAX)

out.dir ="/home/jc148322/Ecocrop_output/"
write.csv(ECOcrops,paste(out.dir,'ecocrop_new.csv',sep=''))

ECOcrops=read.csv(paste(out.dir,'ecocrop_new.csv',sep=''))

#lifespan
unique(ECOcrops$LIFESPAN)
ECOcrops$LIFESPAN = as.character(ECOcrops$LIFESPAN)
ECOcrops$LIFESPAN[which(ECOcrops$LIFESPAN=='annual'|ECOcrops$LIFESPAN=='annual, perennial'|ECOcrops$LIFESPAN=='annual, biennial, perennial'|ECOcrops$LIFESPAN=='annual, biennial')] = 1
ECOcrops$LIFESPAN[which(ECOcrops$LIFESPAN=='perennial'|ECOcrops$LIFESPAN=='biennial, perennial'|ECOcrops$LIFESPAN=='biennial')] = 2
ECOcrops$LIFESPAN=as.numeric(ECOcrops$LIFESPAN)

#photoperiod
unique(ECOcrops$PP)
ECOcrops$PPmin=NA;ECOcrops$PPmax=NA
ECOcrops$PP=as.character(ECOcrops$PP)
ECOcrops$PPmin[which(ECOcrops$PP=='short day (&lt;12 hours)'|ECOcrops$PP=='short day (&lt;12 hours), neutral day (12-14 hours), long day (&gt;14 hours)'|ECOcrops$PP=='short day (&lt;12 hours), neutral day (12-14 hours)'|ECOcrops$PP=='short day (&lt;12 hours), long day (&gt;14 hours)'|ECOcrops$PP=='long day (&gt;14 hours), neutral day (12-14 hours), short day (&lt;12 hours)'|ECOcrops$PP=='not sensitive')]=1
ECOcrops$PPmin[which(ECOcrops$PP=='neutral day (12-14 hours), long day (&gt;14 hours)'|ECOcrops$PP=='neutral day (12-14 hours)')]=2
ECOcrops$PPmin[which(ECOcrops$PP=='long day (&gt;14 hours)')]=3
ECOcrops$PPmin=as.numeric(ECOcrops$PPmin)

ECOcrops$PPmax[which(ECOcrops$PP=='short day (&lt;12 hours)')]=1
ECOcrops$PPmax[which(ECOcrops$PP=='neutral day (12-14 hours)'|ECOcrops$PP=='short day (&lt;12 hours), neutral day (12-14 hours)')]=2
ECOcrops$PPmax[which(ECOcrops$PP=='short day (&lt;12 hours), neutral day (12-14 hours), long day (&gt;14 hours)'|ECOcrops$PP=='long day (&gt;14 hours), neutral day (12-14 hours), short day (&lt;12 hours)'|ECOcrops$PP=='not sensitive'|ECOcrops$PP=='long day (&gt;14 hours)'|ECOcrops$PP=='neutral day (12-14 hours), long day (&gt;14 hours)'|ECOcrops$PP=='short day (&lt;12 hours), long day (&gt;14 hours)')]=3
ECOcrops$PPmax=as.numeric(ECOcrops$PPmax)

#salinity
unique(ECOcrops$SAL)
ECOcrops$SAL = as.character(ECOcrops$SAL)
ECOcrops$SAL[which(ECOcrops$SAL=='low (&lt;4 dS/m)'|ECOcrops$SAL=='none')]=0
ECOcrops$SAL[which(ECOcrops$SAL=='medium (4-10 dS/m)')]=4
ECOcrops$SAL[which(ECOcrops$SAL=='high (&gt;10 dS/m))')]=10
ECOcrops$SAL=as.numeric(ECOcrops$SAL)

unique(ECOcrops$SALR)
ECOcrops$SALR = as.character(ECOcrops$SALR)
ECOcrops$SALR[which(ECOcrops$SALR=='low (&lt;4 dS/m)')]=3.99
ECOcrops$SALR[which(ECOcrops$SALR=='medium (4-10 dS/m)')]=9.99
ECOcrops$SALR[which(ECOcrops$SALR=='high (&gt;10 dS/m))'|ECOcrops$SALR=='none')]=100
ECOcrops$SALR=as.numeric(ECOcrops$SALR)





ECOcrops$SALMIN = as.character(ECOcrops$SAL)
ECOcrops$SALMAX = as.character(ECOcrops$SALR)

write.csv(ECOcrops,paste(out.dir,'ecocrop_new_edit.csv',sep=''))