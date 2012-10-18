library(SDMTools)

wd = 'C:/Users/jc148322/Documents/Work Directory/Jeremy Work Directory/Ecocrop/land use'; setwd(wd)

tasc=read.asc('edit.asc')
baseasc=read.asc('base.asc')
baseasc[which(is.finite(baseasc))] = 0

pos = read.csv('base.pos.csv',as.is=TRUE) 	#read in the base positions
pos$crop = extract.data(cbind(pos$lon,pos$lat),tasc) 	#extract the soil id
tt = baseasc; tt[cbind(pos$row,pos$col)] = pos$crop 	#get the soil id

pos$crop[which(is.na(pos$crop))]=0
pos$crop[which(pos$crop==136 | pos$crop==186 | pos$crop==195 | pos$crop==196 | pos$crop==197 | pos$crop==200 | pos$crop==224 | pos$crop==230 | pos$crop==249 | pos$crop==253 | pos$crop==254 | pos$crop==257 | pos$crop==259 | pos$crop==260 | pos$crop==270 | pos$crop==273 | pos$crop==277 | pos$crop==292 | pos$crop==295 | pos$crop==297 | pos$crop==299 | pos$crop==301 | pos$crop==306 | pos$crop==381 | pos$crop==382 | pos$crop==383 | pos$crop==384 | pos$crop==388 | pos$crop==390 | pos$crop==391 | pos$crop==395 | pos$crop==397 | pos$crop==398 | pos$crop==400 | pos$crop==401 | pos$crop==404 | pos$crop==413 | pos$crop==419 | pos$crop==420 | pos$crop==430 | pos$crop==436 | pos$crop==437 | pos$crop==441 | pos$crop==445 | pos$crop==449 | pos$crop==450 | pos$crop==452 | pos$crop==454 | pos$crop==455 | pos$crop==459 | pos$crop==462 | pos$crop==465 | pos$crop==466 | pos$crop==468 | pos$crop==469 | pos$crop==471 | pos$crop==472 | pos$crop==473 | pos$crop==474 | pos$crop==476 | pos$crop==477 | pos$crop==480 | pos$crop==481 | pos$crop==484 | pos$crop==485 | pos$crop==489 | pos$crop==492 | pos$crop==499 | pos$crop==519 | pos$crop==521 | pos$crop==522 | pos$crop==523 | pos$crop==525 | pos$crop==528 | pos$crop==536 | pos$crop==541 | pos$crop==545 | pos$crop==546 | pos$crop==547 | pos$crop==554 | pos$crop==562 | pos$crop==568 | pos$crop==571 | pos$crop==572 | pos$crop==574 | pos$crop==576 | pos$crop==579 | pos$crop==582 | pos$crop==594 | pos$crop==599 | pos$crop==603 | pos$crop==604 | pos$crop==613 | pos$crop==614 | pos$crop==617 | pos$crop==620 | pos$crop==623 | pos$crop==625 | pos$crop==636 | pos$crop==637 | pos$crop==643 | pos$crop==644 | pos$crop==647 | pos$crop==648 | pos$crop==650 | pos$crop==654 | pos$crop==655 | pos$crop==656 | pos$crop==657 | pos$crop==665 | pos$crop==670 | pos$crop==681 | pos$crop==696 | pos$crop==697 | pos$crop==700 | pos$crop==722 | pos$crop==728 | pos$crop==729 | pos$crop==743 | pos$crop==782 | pos$crop==791 | pos$crop==802 | pos$crop==803 | pos$crop==828 | pos$crop==849 | pos$crop==853 | pos$crop==875 | pos$crop==884 | pos$crop==885 | pos$crop==886 | pos$crop==890 | pos$crop==891 | pos$crop==892 | pos$crop==894 | pos$crop==895 | pos$crop==899 | pos$crop==901 | pos$crop==904 | pos$crop==906 | pos$crop==907 | pos$crop==921 | pos$crop==937 | pos$crop==950 | pos$crop==974 | pos$crop==1034)]=NA
pos$crop[which(is.finite(pos$crop))]=0


baseasc[cbind(pos$row,pos$col)]=pos$crop


write.asc(baseasc, 'landuse.asc')
