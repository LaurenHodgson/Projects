library(SDMTools)

wd = 'C:/Users/jc148322/Documents/Work Directory/Jeremy Work Directory/Ecocrop/land use/land use multi yrs/'; setwd(wd)

baseasc=read.asc('base.asc')
baseasc[which(is.finite(baseasc))] = 0
pos = read.csv('base.pos.csv',as.is=TRUE) 	#read in the base positions

#93
tasc=read.asc('1993.asc')
pos$crop93 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop93[which(is.na(pos$crop93))]=0
pos$crop93[which(pos$crop93==5 | pos$crop93==6 | pos$crop93==8 | pos$crop93==9 | pos$crop93==10 | pos$crop93==11 | pos$crop93==12)]=NA
pos$crop93[which(is.finite(pos$crop93))]=0

#94
tasc=read.asc('1994.asc')
pos$crop94 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop94[which(is.na(pos$crop94))]=0
pos$crop94[which(pos$crop94==310 | pos$crop94==320 | pos$crop94==330 | pos$crop94==331 | pos$crop94==333 | pos$crop94==334 | pos$crop94==335 | pos$crop94==336 | pos$crop94==338 | pos$crop94==340 | pos$crop94==341 | pos$crop94==343 | pos$crop94==344 | pos$crop94==354 | pos$crop94==420 | pos$crop94==430 | pos$crop94==431 | pos$crop94==433 | pos$crop94==434 | pos$crop94==435 | pos$crop94==436 | pos$crop94==438 | pos$crop94==440 | pos$crop94==441 | pos$crop94==443 | pos$crop94==444 | pos$crop94==454)]=NA
pos$crop94[which(is.finite(pos$crop94))]=0

#97
tasc=read.asc('1997.asc')
pos$crop97 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop97[which(is.na(pos$crop97))]=0
pos$crop97[which(pos$crop97==5 | pos$crop97==6 | pos$crop97==8 | pos$crop97==9 | pos$crop97==10 | pos$crop97==11 | pos$crop97==12)]=NA
pos$crop97[which(is.finite(pos$crop97))]=0

#99
tasc=read.asc('1999.asc')
pos$crop99 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop99[which(is.na(pos$crop99))]=0
pos$crop99[which(pos$crop99==5 | pos$crop99==6 | pos$crop99==8 | pos$crop99==9 | pos$crop99==10 | pos$crop99==11 | pos$crop99==12)]=NA
pos$crop99[which(is.finite(pos$crop99))]=0

#01
tasc=read.asc('2001.asc')
pos$crop01 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop01[which(is.na(pos$crop01))]=0
pos$crop01[which(pos$crop01==5 | pos$crop01==6 | pos$crop01==8 | pos$crop01==9 | pos$crop01==10 | pos$crop01==11 | pos$crop01==12)]=NA
pos$crop01[which(is.finite(pos$crop01))]=0


#02
tasc=read.asc('2002.asc')
pos$crop02 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop02[which(is.na(pos$crop02))]=0
pos$crop02[which(pos$crop02==107 | pos$crop02==187 | pos$crop02==188 | pos$crop02==193 | pos$crop02==195 | pos$crop02==198 | pos$crop02==201 | pos$crop02==206 | pos$crop02==207 | pos$crop02==210 | pos$crop02==213 | pos$crop02==236 | pos$crop02==237 | pos$crop02==257 | pos$crop02==258 | pos$crop02==259 | pos$crop02==260 | pos$crop02==262 | pos$crop02==263 | pos$crop02==264 | pos$crop02==266 | pos$crop02==271 | pos$crop02==299 | pos$crop02==300 | pos$crop02==313 | pos$crop02==327 | pos$crop02==341 | pos$crop02==364 | pos$crop02==392 | pos$crop02==393 | pos$crop02==396 | pos$crop02==397 | pos$crop02==398 | pos$crop02==399 | pos$crop02==401 | pos$crop02==402 | pos$crop02==404 | pos$crop02==405 | pos$crop02==406 | pos$crop02==411 | pos$crop02==412 | pos$crop02==423 | pos$crop02==429 | pos$crop02==431 | pos$crop02==447 | pos$crop02==456 | pos$crop02==457 | pos$crop02==458 | pos$crop02==459 | pos$crop02==462 | pos$crop02==464 | pos$crop02==465 | pos$crop02==466 | pos$crop02==469 | pos$crop02==470 | pos$crop02==472 | pos$crop02==477 | pos$crop02==479 | pos$crop02==481 | pos$crop02==482 | pos$crop02==483 | pos$crop02==485 | pos$crop02==486 | pos$crop02==488 | pos$crop02==490 | pos$crop02==492 | pos$crop02==493 | pos$crop02==523 | pos$crop02==525 | pos$crop02==542 | pos$crop02==555 | pos$crop02==566 | pos$crop02==569 | pos$crop02==571 | pos$crop02==572 | pos$crop02==573 | pos$crop02==576 | pos$crop02==580 | pos$crop02==583 | pos$crop02==597 | pos$crop02==599 | pos$crop02==601 | pos$crop02==602 | pos$crop02==603 | pos$crop02==604 | pos$crop02==611 | pos$crop02==615 | pos$crop02==617 | pos$crop02==618 | pos$crop02==619 | pos$crop02==620 | pos$crop02==621 | pos$crop02==622 | pos$crop02==628 | pos$crop02==635 | pos$crop02==640 | pos$crop02==644 | pos$crop02==647 | pos$crop02==649 | pos$crop02==650 | pos$crop02==656 | pos$crop02==657 | pos$crop02==661 | pos$crop02==679 | pos$crop02==680 | pos$crop02==695 | pos$crop02==699 | pos$crop02==719 | pos$crop02==722 | pos$crop02==726 | pos$crop02==727 | pos$crop02==736 | pos$crop02==740 | pos$crop02==743 | pos$crop02==746 | pos$crop02==747 | pos$crop02==748 | pos$crop02==756 | pos$crop02==757 | pos$crop02==802 | pos$crop02==806 | pos$crop02==807 | pos$crop02==854 | pos$crop02==870 | pos$crop02==876 | pos$crop02==878 | pos$crop02==884 | pos$crop02==887 | pos$crop02==891 | pos$crop02==894 | pos$crop02==895 | pos$crop02==897 | pos$crop02==898 | pos$crop02==904 | pos$crop02==906 | pos$crop02==929 | pos$crop02==936 | pos$crop02==943 | pos$crop02==951 | pos$crop02==955 | pos$crop02==966)]=NA
pos$crop02[which(is.finite(pos$crop02))]=0



#06
tasc=read.asc('2006.asc')
pos$crop06 = extract.data(cbind(pos$lon,pos$lat),tasc)

pos$crop06[which(is.na(pos$crop06))]=0
pos$crop06[which(pos$crop06==5 | pos$crop06==6 | pos$crop06==8 | pos$crop06==9 | pos$crop06==10 | pos$crop06==11 | pos$crop06==12)]=NA
pos$crop06[which(is.finite(pos$crop06))]=0

#add it up
allyrs=pos[5:ncol(pos)]
allyrs$crop=rowSums(allyrs)

allyears=baseasc
allyears[cbind(pos$row,pos$col)]=allyrs$crop
image(allyears)

write.asc(allyears, 'allyears.asc')


yr93=baseasc
yr93[cbind(pos$row,pos$col)]=pos$crop93
image(yr93) #okay

yr94=baseasc
yr94[cbind(pos$row,pos$col)]=pos$crop94
image(yr94) #okay

yr97=baseasc
yr97[cbind(pos$row,pos$col)]=pos$crop97
image(yr97) #okay

yr99=baseasc
yr99[cbind(pos$row,pos$col)]=pos$crop99
image(yr99) #okay

yr01=baseasc
yr01[cbind(pos$row,pos$col)]=pos$crop01
image(yr01) #okay

yr02=baseasc
yr02[cbind(pos$row,pos$col)]=pos$crop02
image(yr02) #okay

yr06=baseasc
yr06[cbind(pos$row,pos$col)]=pos$crop06
image(yr06) #okay




































