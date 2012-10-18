




tmin = matrix(runif(144,0,30),nr=12,nc=12)

tmin[which(tmin<20 | tmin > 27)] = NA
tmin[which(is.finite(tmin))] = 1
tout = tmin; tout[,] = NA

for (ii in 1:12) {
	cols.of.interest = 1+(ii:(ii+5)%%12)
	tout[,ii] = rowSums(tmin[,cols.of.interest],na.rm=TRUE)
}
tout
tt = apply(tout,1,max)
