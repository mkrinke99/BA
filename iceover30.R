




#add var icecover above 30%
library(dplyr)
library(lubridate)

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")

ice<- st[,c("time", "icecover")]
ice$icecover_int<-  approx(ice[!is.na(ice$icecover),]$time, 
                           ice[!is.na(ice$icecover),]$icecover, 
                           xout = ice$time, rule = 2, method = "linear")$y

ice$icecover30<- 0
ice[ice$icecover_int >= 30,]$icecover30<- 1



tdiff<- difftime(lead(ice[!is.na(ice$icecover),]$time), 
                ice[!is.na(ice$icecover),]$time, units= "days")
tdiff[length(tdiff)]<- 1
iceobs<- ice[!is.na(ice$icecover),]
iceobs$diff<- tdiff

obslast<- iceobs[iceobs$diff >50,]



nosample<- list()
for(i in 1: nrow(obslast)){
  nosample[[i]]<- seq(obslast$time[i] +days(1), obslast$time[i] + obslast$diff[i]-1, by= "days")
}
nodays<- as.POSIXct(as.numeric(unlist(nosample)), origin='1970-01-01') 


ice<- subset(ice, !(ice$time %in% nodays))
table(month(ice$time))

for(yy in c(1994:2002, 2009:2020)){
  sty= subset(ice, ice$time >= as.POSIXct(paste0((yy-1),"-10-01"), 
                                          format= "%Y-%m-%d", tz= "UTC") &
                ice$time < as.POSIXct(paste0(yy,"-06-01"), 
                                      format= "%Y-%m-%d", tz= "UTC"))
  plot(sty$time, sty$icecover, pch= 16, col= "royalblue", 
       main= yy, ylim= c(0,100))
  lines(sty$time, sty$icecover_int, col= "cyan2")
  lines(sty$time, sty$icecover30*30)
}


#approximated days with at least 30% Ice cover
ice30<- as.data.frame(aggregate(ice$icecover30, list(year(ice$time)), sum))
colnames(ice30)<- c("year", "ice30days")

#write.table(ice30, "ice30.csv", sep= ";")


