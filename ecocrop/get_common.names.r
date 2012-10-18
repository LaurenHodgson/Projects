wd = '/home/jc148322/Ecocrop/webdata/common_names/'; dir.create(wd);setwd(wd) #set the working directory
species = read.csv('/home/jc148322/Ecocrop/webdata/species.csv',as.is=TRUE)
for (sppcode in species$Code) { cat(sppcode,'\n')
	system(paste('wget http://ecocrop.fao.org/ecocrop/srv/en/cropView?id=',sppcode,sep=''))
	system(paste('mv cropView\\?id\\=',sppcode,' ',sppcode,'.html',sep=''))
}

ECOcrops=read.csv('/home/jc148322/Ecocrop_output/ecocrop_new_edit.csv',as.is=TRUE)
library(Hmisc)
for (sppcode in species$Code[c(1:278,280:2208,2210:length(species$Code))]){ cat(sppcode,'\n')

sppdata=readLines(paste(sppcode,'.html',sep=''))
rowname=which(ECOcrops$CODE==sppcode)

common.name=strsplit(sppdata[119],'<td>')[[1]][2]
common.name=strsplit(common.name,',')[[1]][1]
common.name=strsplit(common.name,'</td>')[[1]][1]
common.name=capitalize(common.name)

column=3
ECOcrops[rowname,column]=common.name

}

out.dir ="/home/jc148322/Ecocrop_output/"
write.csv(ECOcrops,paste(out.dir,'ecocrop_new.csv',sep=''))
