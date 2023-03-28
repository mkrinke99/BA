
#Hypothese 2: "Late-winter blooms of diatoms require a minimum amount of light 
#              (preliminary estimate >1.3 mol m-2 d-1 averaged over the water column)
#              for a certain duration before stratification begins. "

#Benötigte Daten: Bacil. Biomasse, PAR(mol m^-2 d^-1), strat)

#Aufgaben: plotte Bacil_Biomasse oder Wachstumsrate zusammen mit
#          PAR und Stratification Beginn


#Benötigte Pakete und Funtionen
library(dplyr)
library(lubridate)
library(ggplot2)

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


st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")
#Ändere Jahr, wenn Messung noch zur Schichtungsperiode des Vorjahres gehört
st$year<- year(st$time)
st[st$strat==1 & 
     month(st$time) %in% 1:2,]$year<- st[st$strat==1 & month(st$time) %in% 1:2,]$year-1


st<- st[,c("time","bacil_int","strat")]


stpar<- read.csv("newPAR.csv", sep= ";")
stpar$time<- as.POSIXct(stpar$time, format= "%d.%m.%Y", tz= "UTC")
stpar<- stpar[,c(1,6)]

#merge Phyto+Strat data with PAR data
st<- merge(st, stpar, by= "time")

#add Bacil sampling data
bacil<- read.csv("phyto_int.csv", sep= ";")
bacil$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")
bacil<- bacil[,c("time", "bacillariophyta")]
#merge mit restlichen Daten
st<- merge(st, bacil, by= "time")
st$par_maf<- MAF(7, st$PAR_new)
st$year<- year(st$time)

par_1.3<- vector()
stratstart<- vector()

#plotten-------
for(i in 1:26){
sty<- subset(st, year(st$time)== unique(year(st$time))[i] & month(st$time) %in% 1:5)
#labels for x-axis
dates<- c(min(sty$time), sty[day(sty$time)==1,]$time[-1], max(sty$time))
if(i==1){dates<- c(min(sty$time), sty[day(sty$time)==1,]$time, max(sty$time))}
#scaling facor for PAR
scalefactor<- max(sty$bacil_int)/max(sty$PAR_new)
#position on second y-axis where PAR is 1.3
scale2<- max(sty$bacil_int)*(1.3/max(sty$PAR_new))
#PAR with 14 day MAF
par_maf<- sty$par_maf*scalefactor
#plot
par(mar= c(4.3,5,4,5), xpd= F)
#Bacil. Biomass interpoliert
plot(sty$time, sty$bacil_int, type= "l", col= "chartreuse3", lwd=3,
     main= i+1993, xlab= "Zeit", ylab= expression("Biomasse [µg L"^ "-1"* "]"),
     axes= F, ylim= c(0, 1.1*max(sty$bacil_int)))
#Bacil. Biomasse raw
points(sty$time, sty$bacillariophyta, pch= 19, cex= 1.2, col= "chartreuse4")
#points PAR data
points(sty$time, sty$PAR_new*scalefactor, pch=19, col= "lightskyblue2",
       cex= 0.7)
#Achsen
axis(1, dates, month.abb[1:6])
axis(2, las=1, cex.axis= 0.8)
axis(4, c(0, scale2, round(0.8*max(sty$bacil_int),0)), 
     c(0, 1.3, round(max(sty$PAR_new),2)), las= 1)
#Abline Stratification start
stratstart[i]<- sty[sty$strat-lag(sty$strat)==1 & !is.na(lag(sty$strat)),]$time
abline(v= stratstart,
       col= "red3", lwd= 3)
abline(h= scale2, lty= 2, col= "grey44")
#Lines MAF PAR
lines(sty$time, par_maf, col= "royalblue", lwd= 2)
#Abline PAR_MAF > 1.3
par_1.3<- sty$time[min(which(par_maf >scale2))]
abline(v= par_1.3, col= "navy", lwd= 2)
par(xpd=T)
legend(min(sty$time), max(sty$bacil_int)*1.4, box.lty=0, cex= 0.6, inset= 0.02, 
       legend= c("Biomasse Bacillariophyta", "PAR", "PAR (7 Tage Moving Average)",
                 "PAR Moving Average >1.3 mol/m^2/d", "Beginn Stratifikation"),
       fill= c("chartreuse3", "lightskyblue2", "blue", "navy", "red3"))
text(max(sty$time)+days(15), 0.5* max(sty$bacil_int), 
     expression("PAR [mol m"^"-2"*"d"^"-1"*"]"), srt= 90)
}




stratstart<- as.POSIXct(stratstart, origin='1970-01-01', tz= "UTC")


par13days<- vector()
for(i in 1:26){
par13days[i]<- nrow(st[st$PAR_new> 1.3 & st$time < stratstart[i] &
                 st$year== unique(st$year)[i],])
} 
par13days





