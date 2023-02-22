
library(lubridate)
library(dplyr)

setwd("~/Uni/Praktikum/data/stechlin_martin")


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


library(polynom)
library(pracma)

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



#inter<- poly.calc(x[1:20], y[1:20])



#observations per year +gaps

#create column of gap between two measurements
obs<- subset(bacil, !is.na(bacil$bacillariophyta))
lag1<- obs$time[-1]
lag2<- obs$time[-nrow(obs)]
obs$gap<- c(0, as.numeric(difftime(lag1, lag2)))
obs1<- obs[,c(6,9)]

#merge with data set
bacil<- merge(bacil, obs1, all.x= T, by= "time")

par(mfrow= c(3,9)) 
#plot years
for(yy in 1994:2020){
  obs_y<- subset(obs, year(obs$time) %in% (yy-1):yy & month(obs$time) %in% 1:6)
  d1<- as.POSIXct(paste0(yy,"-01-01"), tz="UTC")
  d2<- as.POSIXct(paste0(yy,"-02-01"), tz="UTC")
  d3<- as.POSIXct(paste0(yy,"-03-01"), tz="UTC")
  d4<- as.POSIXct(paste0(yy,"-04-01"), tz="UTC")
  d5<- as.POSIXct(paste0(yy,"-05-01"), tz="UTC")
  d6<- as.POSIXct(paste0(yy,"-05-31"), tz="UTC")
  plot(obs_y$time, obs_y$gap,
       main= yy, cex.main= 1.2, 
       xlab= "Zeit", ylab= "Tage zwischen Stichproben",
       lwd= 2.3, col= "dodgerblue3", type= "b", pch= 16, axes= F, cex= 1.2,
       xlim= c(d1,d6),
       ylim= c(0, max(60, 1.1*obs[obs$year== yy,]$gap)))
  axis(1, c(d1,d2,d3,d4,d5,d6), c(month.abb[1:5],""))
  axis(2, seq(0,90,10), seq(0,90,10), las=1)
  text(as.POSIXct(paste0(yy,"-04-13"), tz="UTC"), max(60, 1.1*obs[obs$year== yy,]$gap),
        paste0(length(obs[month(obs$time) %in% 1:5 & obs$year== yy,]$gap),
               " Stichproben"), col= "dodgerblue4", cex= 1.2)
  abline(h= c(0,7,14,21,28), lty= 2, col= "grey76")
}


ve<- vector()
for(yy in 1994:2020){
  ve[yy-1993]<- length(obs[month(obs$time) %in% 1:5 &
                             obs$year== yy,]$gap)}

len<-data.frame(1994:2020, ve)
plot(1994:2020,ve, type= "l")

#mean days between samplings (Jan-May) 2002-2020
mean(obs$gap[obs$year %in% 2002:2020 & 
               month(obs$time) %in% 1:5])  #20.13







