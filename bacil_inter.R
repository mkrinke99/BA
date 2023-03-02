
#INPUT: bacil_inter.csv file with phyto data

#OUTPUT: plot of data with all three interpolation methods
#        no Excel files!

library(lubridate)
library(dplyr)
library(polynom)
library(pracma)



bacil<- read.csv("bacil_inter.csv", sep= ";")
bacil$time<-  as.POSIXct(bacil$date, format= "%d.%m.%Y", tz= "UTC")
bacil<- bacil[,-1]


for(yy in 1994:2020){
  #png(file= paste0(yy,".png"))
  bacil_y<- subset(bacil, bacil$year== yy & month(bacil$time) %in% 1:5)
  x1<-plot(bacil_y$time, bacil_y$bacillariophyta, 
       col= "dodgerblue4", pch= 16, type= "p", cex= 1.2, main= yy, las= 1,
       ylim= c(0, max(bacil_y$spline_interpol_corrected)*1.1), ylab= "", xlab= "")
  lines(bacil_y$time, bacil_y$linear_interpol, col= "dodgerblue2")
  lines(bacil_y$time, bacil_y$spline_interpol_corrected, col= "chartreuse3")
  legend("topleft", legend= c("linear","spline"), fill= c("dodgerblue2","chartreuse3"))
  #dev.off()
  }



bacil$difftime<- as.numeric(difftime(bacil$time, bacil$time[1], units= "days"))

x<- bacil$difftime[!is.na(bacil$bacillariophyta)]
y<- bacil$bacillariophyta[!is.na(bacil$bacillariophyta)]
 
seq1<- min(x):max(x)
prac<- pchip(x, y, seq1)
bacil$prac<- c(rep(0,19), prac, rep(0,28))



#plot linear, cubic hermite and spline for every year
for(yy in 1994:2020){
  bacil_y<- subset(bacil, bacil$year== yy)
  d1<- as.POSIXct(paste0(yy,"-01-01"), tz="UTC")
  d6<- as.POSIXct(paste0(yy,"-05-31"), tz="UTC")
par(mfrow= c(3,1))
plot(bacil_y$time, bacil_y$linear_interpol,
     xlim= c(d1,d6), lty=2, col= "forestgreen", type= "l", lwd=3,
     main= paste0("Lineare Interpolation \n", yy), las= 1, xlab= "Zeit", ylab= "Biomasse",
     ylim= c(-440, max(bacil_y$spline_interpol_corrected *1.1)))
points(bacil_y$time, bacil_y$bacillariophyta, 
       pch=16, cex=2.1, col= "forestgreen")
abline(h=0, col= "grey55")

plot(bacil_y$time, bacil_y$prac,
     xlim= c(d1,d6), col= "dodgerblue3", lty=2, type= "l", las=1,lwd=3,
     main= paste0("Kubisch hermetische Interpolation \n", yy), xlab= "Zeit", 
     ylab= "Biomasse",
     ylim= c(-440, max(bacil_y$spline_interpol_corrected *1.1)))
points(bacil_y$time, bacil_y$bacillariophyta, 
       pch=16, cex= 2.1, col= "dodgerblue4")
abline(h=0, col= "grey55")

plot(bacil_y$time, bacil_y$spline_interpol,
     xlim= c(d1,d6),col= "chartreuse3", lty=2, type= "l", las=1,lwd=3,
      main= paste0("Spline Interpolation \n", yy), xlab= "Zeit", ylab= "Biomasse",
     ylim= c(-440, max(bacil_y$spline_interpol_corrected *1.1)))
points(bacil_y$time, bacil_y$bacillariophyta,
       pch=16, cex= 2.1, col= "chartreuse4")
abline(h=0, col= "grey55")
}









