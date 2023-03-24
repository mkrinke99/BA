
#Hypothesis 7: "This in turn decreases the availability of nutrients in 
#                surface water during the subsequent summer stratification.
#                Therefore the biomass of late-winter diatoms correlates negatively 
#                with the phytoplankton biomass in summer 
#                and positively with water transparency"


#Schritt 1: subsets der Gruppen nach 



library(lubridate)

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")

#Ziehe ein Jahr von Tagen ab, in Welchen die Stratification erst im 
#folgenden Jahr endet, sodass Werte beim Aggregieren dem richtigen Jahr  angehören

st$year<- year(st$time)
st<- subset(st, st$year != 2020)
st[st$strat==1 & month(st$time) %in% 1:2,]$year<- st[st$strat==1 & month(st$time) %in% 1:2,]$year-1

st0<- subset(st, st$strat== 0)
st1<- subset(st, st$strat== 1)

st0y<- aggregate(st0[,2:16], list(st0$year), mean)
st1y<- aggregate(st1[,2:16], list(st1$year), mean)













