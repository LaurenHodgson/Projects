
################################################################################
#constants & equation from Glencross 2008
#gain equation
K = 0.539661; k = 0.4240000
x = -0.119917; y = 0.007434; z=-0.000119
gain = function(liveweight,Temperature) { #gain id grams per fish per day, live wieght is in grams, temperature in degrees celcius
return((K + x*Temperature + y*Temperature^2 + z*Temperature^3) * liveweight^k)
}

wt = seq(20, 3000, 10)
temp=c(15:40)
rate = function(temp,wt) { new=wt+gain(temp,wt); r = new/gain(temp,wt); r/100}
z = outer(temp, wt, rate)
z[is.na(z)] <- 1
op = par(bg='white')
persp(temp, wt, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
persp(temp, wt, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "Temperature", ylab = "Weight", zlab = "Rate gain %weight/d"
)


out=NULL
gain=NULL
rate=NULL
for (wt in liveweight){cat (wt, '\n')
for (temp in Temperature) {cat (temp, '\n')
	gain = rate.gain(wt, temp)
	rate = gain/wt
	rate = rate*100
	out = rbind(out, data.frame(temp, rate))
}
	if (ii==1) {out = rbind(out, data.frame(temp, rate))} else {out=cbind(out,rate)}
}
image(out$temp,out$gain,out$wt)


##########################################################################

x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
op <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "T", ylab = "Y", zlab = "Sinc( r )"
) -> res
round(res, 3)



