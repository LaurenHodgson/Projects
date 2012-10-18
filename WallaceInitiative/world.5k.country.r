library(SDMTools); library(maptools)

wd='/home/jc148322/WallaceInitiative/countries_summary/';setwd(wd)
base.asc=read.asc.gz('base.asc.gz')
pos=read.csv('base.positions.csv')


#option 1
world.asc=read.asc('world.asc')
pos$country.code=extract.data(cbind(pos$lon,pos$lat),world.asc)

world.dbf=read.dbf('shapefile/world_country_admin_boundary_shapefile_with_fips_codes.dbf')
world.dbf$country.code=c(0:251)
world.dbf$country.code=as.numeric(world.dbf$country.code)
colnames(world.dbf)=gsub('CNTRY_NAME','country.name',colnames(world.dbf))

tdata=merge(world.dbf[,c('country.name','country.code')],pos)

write.csv(tdata,'world.countries.5k.csv')
