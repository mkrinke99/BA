

library(lubridate)
library(tidyverse)

st_all<- read.csv("st_physchemmet.csv", sep= ";")
st_all$time<- as.POSIXct(st_all$time, format= "%d.%m.%Y", tz= "UTC")
st1<- subset(st_all, st_all$site.phys== "Main basin")
st1<- st1[,c("time", "depth", "secchi", "PAR", "solar_radiation_e5")]



st<- st1[!is.na(st1$secchi) & !is.na(st1$time),]
st$solar_inWatt<- st$solar_radiation_e5/3600

#Tage mit Secchi-Daten: 469
length(unique(st[!is.na(st$secchi),]$time))
#Tage mit PAR-Daten: 346
length(unique(st[!is.na(st$PAR),]$time))


Kd<- NULL
date<- unique(st$time)

for(i in date){
  dat<- st[st$time== i & st$depth <= 10, c("PAR", "depth", "secchi")]
  if(max(dat$depth) >2 & sum(!(is.na(dat$PAR))) >2){
    
  r1<- lm(log(PAR) ~ depth, data=dat)
  #plot(log(PAR)~depth, data= dat, las=1)
  #abline(r1)
  #text(9,7, paste0("Extinction coef.: ",round(Kd1,4)), cex= 0.8)
  Kd1<- c(-coef(r1)[2])
  #date1<- c(date, i)
  }
  else {
    Kd1<- 2/dat$secchi[1]
  }
  Kd<- c(Kd, Kd1)
}
Kd

ext<- data.frame(date, Kd)
ext$time<- as.POSIXct(as.numeric(ext$date), 
                      origin='1970-01-01', tz= "UTC") 

ext1<- ext[,3:2]
ext1


# radiation---------
# calculate daily means
# original units J/m2?
# convert to W/m2
# remember 1 W = 1 Joule /second

rad<- st_all[,c("time", "solar_radiation_e5")]
rad<- aggregate(rad$solar_radiation_e5, list(rad$time), mean)
names(rad)<- c("time", "solar_rad_e5")

rad <- mutate(rad,radiation=solar_rad_e5 / 3600)
rad<-  rad[year(rad$time) >= 1994,]
rad<- rad[,c("time","radiation")]
# plot(rad~time, rad)


#calculate daily underwater light from 1997 to 2020
#linearly interpolate Kd to daily
datenew <- seq(ISOdate(1994,1,1,0,tz="UTC"),
               ISOdate(2020,12,31,0,tz="UTC"), "day")
mydata1 <- data.frame(time=datenew, 
                      Kd=approx(x = ext$time, 
                               y = ext$Kd, xout = datenew, rule=2)$y)
mydata <- merge(mydata1,rad, by= "time")

#Add Mixed Layer and Bacil. data
bac1<- read.csv("ba_dataset.csv", sep= ";")
bac1$time<- as.POSIXct(bac1$time, format= "%Y-%m-%d", tz= "UTC")
bac<- bac1[,c("time", "bacil_int", "hmix", "strat")]

mydata<- merge(mydata, bac, by= "time")


# We need to know the mixed layer depth to calculate the mean light
# During mixing, the mixed layer depth is just the mean lake depth
# During stratification not. Let's assume that the mixed layer depth
# is 23 m, you can add a time series later


#change hmix from 69 to mean depth of 23 (not used yet)
hist(mydata[mydata$strat==0,]$hmix)
mydata[mydata$strat== 0,]$hmix <- 23

mydata$zmix<- 23

# Now convert radiation to PAR
# assuming 50% of radiationi is PAR, and 10% is reflected at the surface, 
# and the conversion is 1 Joule = 4.56 umol photons, then convert to mol/m2/day

mydata$I0 <- mydata$rad * 0.5 * 0.9 * 4.56 * # now umol photons /m2/s
  60*60*24 / 1000000 # now mol photons/m2/day


# Now calculate mean underwater light in mixed layer
# Integrating Lambert-Beer law over depth gives
# Imean = I0/(Kd * zmix) * (1 - exp(-Kd * zmix))
mydata <- mutate(mydata, PAR_new = I0 / (Kd * zmix) * (1 - exp(-Kd * zmix)))


#save-----
#write.table(mydata, "newPAR.csv", sep= ";")

#.---------
#add bacil data-----


#scaling factor to have PAR and bac biomass in same plot


dfbac<- mydata
dfbac$PAR_MAF<- MAF(14, dfbac$PAR_new)

par1_3<- vector()
maxday<- vector()


for(yy in 1994:2020){
dfbac1<- subset(dfbac, year(dfbac$time) %in% yy & month(dfbac$time) %in% 1:5)
scale_PAR_bac<- max(dfbac1$bacil_int)/max(dfbac1$PAR_new)
par1_3[yy-1993]<- dfbac1[dfbac1$PAR_MAF >1.3,][1,1]
maxday[yy-1993]<- which.max(dfbac1$bacil_int)
text<- c(dfbac1$time[which(day(dfbac1$time)==1)],  dfbac1$time[nrow(dfbac1)])
if(yy== 1994){text= c(min(dfbac1$time), text)}
par(mar=c(4, 4.51, 5.1, 5.1), xpd= F)
plot( PAR_new~time,      dfbac1,type="p", pch= 16, cex= 0.5, col= "thistle",
      main= yy, axes= F, ylim= c(0, 1.1*max(dfbac1$PAR_new)),
      xlab= "Zeit", ylab= expression("PAR [mol m"^"-2"*"d"^"-1"*"]"))
lines(PAR_MAF~time, dfbac1, col= "mediumpurple", lwd= 2)
lines(dfbac1$time, dfbac1$bacil_int/scale_PAR_bac, col= "chartreuse3", lwd= 3)
abline(h=1.3, lty= 2)
abline(h= 0, col= "grey66")
abline(v= par1_3[yy-1993], lwd= 2)
axis(1, text, month.abb[1:6])
axis(2, las= 1)
axis(4, las= 1, c(0, max(dfbac1$bacil_int)/scale_PAR_bac), 
     c(0, round(max(dfbac1$bacil_int),0)))
par(xpd= T)
text(max(dfbac1$time) +days(13), 0.4*max(dfbac1$PAR_new), 
     expression("Biomasse [µg L"^ "-1"* "]"), srt=90)
legend("topright", inset=c(0,-0.2), 
       legend=c("PAR (14 Tage Moving Average)",
                "Biomasse Bacillariophyta",
                "Moving Average von PAR >1.3"),
       fill= c("mediumpurple", "chartreuse3", "black"),
       title="Legende", cex= 0.6, box.lty= 0)
}


for(yy in 1994:2019){
  dfbac1<- subset(dfbac, year(dfbac$time) %in% yy & month(dfbac$time) %in% 1:5)
  par(mfrow= c(2,1))
  plot(dfbac1$time, dfbac1$bacil_int, type= "l", main=yy)
  plot(dfbac1$time, dfbac1$bacil_int, type= "l", main=yy)
}
  
  
par1_3<- as.POSIXct(par1_3, origin= "1970-01-01", tz= "UTC")


#dfbac$bac_MAF<- MAF2(14, dfbac$bacil_int)
dfbac[dfbac$bacil_int ==0,]$bacil_int<- 0.1
dfbac$gr<- (dfbac$bacil_int - lag(dfbac$bacil_int))/ (lag(dfbac$bacil_int))
dfbac$gr[1]<- 0
dfbac$gr_MAF<- MAF2(10, dfbac$gr)

for(yy in 1994:2019){
dfbac1<- subset(dfbac, year(dfbac$time)== yy & month(dfbac$time) %in% 1:5)
par(mfrow= c(2,1))
plot(dfbac1$time, dfbac1$bacil_int, type= "l", las= 1, main= yy)
plot(dfbac1$time, dfbac1$gr, type= "l", las= 1)
abline(h=0)
abline(v= par1_3[yy-1993])
}

















#MAF-----------
modulo<-function(x,a){		# gibt den Rest der Division x/a zurück
  #round() geht bei Rundung von 5 zur nächsten geraden Zahl!
  mod_dummy<-x-round(x/a)*a	
  if (mod_dummy<0) mod_dummy+a else mod_dummy
}

is.even<-function(x){
  if(modulo(x,2)==0) TRUE else FALSE
}
# moving average filter, argumente: ordnung "ord" (gerade zahlen!), zeitreihe "reihe"
MAF_even<-function(ord,reihe){		
  n<-ord/2	# funktioniert nur für gerade Zahlen
  result<-reihe
  leng<-length(reihe)
  # setze die ersten n Werte auf Mittelwert der ersten 2*n Werte
  for(i in 1:n)result[i]<-mean(reihe[1:(2*n)])	
  #letzte n Werte: Mittelwert der letzten ord Werte
  for(i in (leng-n+1):leng)result[i]<-mean(reihe[(leng-(2*n)+1):leng])	
  #Gleitender Durchschnitt
  for(i in (n+1):(leng-n)){
    result[i]<-(sum(reihe[(i-n+1):(i+n-1)])+0.5*(reihe[i+n]+reihe[i-n]))/(2*n)	
  }
  result		#Ausgabe
}
# moving average filter, argumente: ordnung "ord" (ungerade zahlen!), 
# zeitreihe "reihe"
MAF_uneven<-function(ord,reihe){	
  n<-(ord-1)/2	# funktioniert nur f�r gerade Zahlen
  result<-reihe
  leng<-length(reihe)
  # setze die ersten n Werte auf Mittelwert der ersten 2*n Werte
  for(i in 1:n)result[i]<-mean(reihe[1:(2*n)])	
  #letzte n Werte: Mittelwert der letzten ord Werte
  for(i in (leng-n+1):leng)result[i]<-mean(reihe[(leng-(2*n)+1):leng])	
  for(i in (n+1):(leng-n)){
    result[i]<-(sum(reihe[(i-n):(i+n)]))/(2*n+1)	#Gleitender Durchschnitt
  }
  result		#Ausgabe
}
MAF<-function(ord,reihe){
  if (is.even(ord)==TRUE) result<-MAF_even(ord,reihe) 
  else result<-MAF_uneven(ord,reihe) 
  result
}


MAF2<- function(ord, reihe){
  n<-ord	# funktioniert nur für gerade Zahlen
  result<-reihe
  leng<-length(reihe)
  # setze die ersten n Werte auf Mittelwert der ersten 2*n Werte
  for(i in 1:n) result[i]<- mean(reihe[1:(2*n)])	
  #Gleitender Durchschnitt
  for(i in (n+1):leng){
    #result2[i]<-(sum(reihe[(i-n+1):(i+n-1)])+0.5*(reihe[i+n]+reihe[i-n]))/(2*n)
    result[i]<- mean(result[i-ord]:result[i], rm.na=T)
  }
  result
}


