
library(lubridate)



#dataset---- 

#phytoplankton data------
phy1<- read.csv("phyto_int.csv", sep= ";")
phy1$time<- as.POSIXct(phy1$time, format= "%Y-%m-%d", tz= "UTC")
phy<- phy1[,c(1:8, 17:24)]


#add spline interpolation for bacil
bacil<- read.csv("bacil_inter.csv", sep= ";")
bacil$time<-  as.POSIXct(bacil$date, format= "%d.%m.%Y", tz= "UTC")
bacil<- bacil[,c(5,7)]
colnames(bacil)[1]<- "bacil_spline"

#merge dataset with pytodata with bacil spline interpolation
phy<- merge(phy, bacil, by= "time")



#add physical and chemical data------
physchem<- read.csv("st_vertmean.csv", sep= ";")
physchem$time<- as.POSIXct(physchem$time, format= "%d.%m.%Y", tz= "UTC")




#add secchi depth---------
meteo1<- read.csv("st_physchemmet.csv", sep= ";")
meteo1$time<- as.POSIXct(meteo1$time, format= "%d.%m.%Y", tz= "UTC")
#only keep data for 0m
secchi<- meteo1[meteo1$site.chem== "Main basin" & !is.na(meteo1$site.chem) &
                  meteo1$depth== 0 & !is.na(meteo1$depth),]

#interpolate secchi
days<- seq(as.POSIXct("1994-01-01", tz= "UTC"), 
           as.POSIXct("2020-12-31", tz= "UTC"), by= "days")
#days df -> keep 1993-11-15 for interpolation
days.df<- data.frame(time=  seq(as.POSIXct("1993-11-15", tz= "UTC"), 
                                as.POSIXct("2020-12-31", tz= "UTC"), by= "days"))
secchi<- aggregate(secchi$secchi, list(secchi$time), mean, na.rm= T)
colnames(secchi)<- c("time", "secchi")
secchi<- merge(days.df, secchi, by= "time", all.x= T)
secchi$secchi<- approx(secchi$time, secchi$secchi, xout= secchi$time,
                       rule= 2, method= "linear")$y

#add meteo data
meteo<- merge(days.df, meteo1[,c(1, 23, 27:36)], by= "time")
#remove duplicates
meteo<- meteo[!duplicated(meteo), ]
colnames(meteo)<- c("time", "icecover", "air_temp", "dewpoint_temp", 
                    "cloud_cover", "snowfall", "snowdepth", "air_pressure",
                    "solar_radiation", "thermal_radiation", "wind_speed",
                    "humidity")


#add approximated icecover data
#ice1<- read.csv("tp_ice_daily_predictions.csv", sep=";")
#ice1$time<-  as.POSIXct(ice1$Date, format= "%Y-%m-%d", tz= "UTC")
#ice<- ice1[,c(10,20)]
#ice[,1]<- round(ice[,1], 0)
#colnames(ice)<- c("icecover_prediction", "time")
#merge with meteo data
#meteo<- merge(meteo, ice, by= "time")



#add stratification data------
strat<- read.csv("st_strat.csv", sep= ";")
start<- as.POSIXct(strat$start_date, format= "%d.%m.%Y", tz= "UTC")
end  <- as.POSIXct(strat$end_date, format= "%d.%m.%Y", tz= "UTC")

stratdays<- list()
for(i in 1:26){
stratdays[[i]]<- seq(start[i], end[i], by= "days")
}

strat2<- data.frame(time= days, strat= 0)
for(i in 1:26){
strat2[strat2$time %in% stratdays[[i]],]$strat<- 1
}




#merge------
st<- merge(phy, physchem, by= "time") 
st<- merge(st, secchi, by= "time", all.x= T)
st<- merge(st, meteo,  by= "time", all.x= T)
st<- merge(st, strat2, by= "time", all.x= T)



#change NAs in hmix to 69
st[is.na(st$hmix),]$hmix= 69




#save------
#write.table(st, "ba_dataset.csv", sep= ";")

st$year<-   year(st$time)
st$month<-  month(st$time)

yday(as.Date("03-20", format= "%m-%d")) #spring start: 79
yday(as.Date("06-21", format= "%m-%d")) #spring start: 172
yday(as.Date("09-23", format= "%m-%d")) #autumn start: 266
yday(as.Date("12-22", format= "%m-%d")) #autumn start: 356

st$season<- "winter"
st[yday(st$time) %in% 79:171,]$season<- "spring"
st[yday(st$time) %in% 172:265,]$season<- "summer"
st[yday(st$time) %in% 266:355,]$season<- "autumn"

#change year of data from 22.12. - 31.12.  -> aggregate for winter of next year
st$year2<- st$year
st[yday(st$time) > 355,]$year2<- st[yday(st$time) > 355,]$year+1



#yearly means
st_y<- aggregate(st[,2:41], list(st$year), mean, na.rm= T)
colnames(st_y)[1]<- "year"
#save
#write.table(st_y, "st_y.csv", sep= ";")

#monthly means
st_m<- aggregate(st[,2:41], list(st$year, st$month), mean, na.rm= T)
colnames(st_m)[1:2]<- c("year", "month")
#save
#write.table(st_m, "st_m.csv", sep= ";")


#season means
st_s<- aggregate(st[,2:41], list(st$year2, st$season), mean, na.rm= T)
colnames(st_s)[1:2]<- c("year", "season")
#save
#write.table(st_s, "st_s.csv", sep= ";")












