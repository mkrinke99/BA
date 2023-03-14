
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























