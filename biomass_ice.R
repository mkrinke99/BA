
library(lubridate)
library(data.table)


#dataset with all vars and year+month
st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")
st$year<- year(st$time)
st$month<- month(st$time)

#subset: only data from jan-may
st<- subset(st, st$month %in% 1:5)


  
for (yy in 1994:1995){
  sty<- subset(st, st$year== yy)
  #icecover prediction
  #transform icecover to have it as third axis in time~biomass~icecover plot
  icedata<- subset(sty, !is.na(sty$icecover))
  icecov1<- icedata$icecover * max(sty$bacil_int)/100
  #icecov2<- sty$icecover_prediction* max(sty$bacil_int)/100
  #plot
  par(mar= c(5,5,4,5.5))
  plot(sty$time, sty$bacil_spline, 
       main= yy, xlab= "time", ylab= "biomass bacil_int",
       ylim= c(0, max(sty$bacil_spline)*1.1),
       type= "l", col= "chartreuse3", lwd= 2, axes= F)
  points(icedata$time, icecov1, col= "blue3", lwd=2, pch= 16, cex= 1.2)
  lines(icedata$time, icecov1, col= "cyan3", lty= 2)
  if(yy != 1994){
         axis(1, c(sty[day(sty$time)==1,]$time, last(sty$time)),  month.abb[1:6])
  }
  else axis(1, c(sty[day(sty$time)==1,]$time, last(sty$time)),  month.abb[2:6])
  axis(2, las=1, cex.axis= 0.8)
  axis(4, c(0,0.3*max(sty$bacil_spline), max(sty$bacil_spline)), 
       c(0,30,100), las=1, cex.axis= 0.8)
  #stratification begin
  abline(v=sty[sty$strat - shift(sty$strat)== 1 & !is.na(shift(sty$strat)),]$time, 
         lwd=3, col= "grey44")
}


































