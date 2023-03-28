
#Hypothesis 1: "The species that dominate during late winter circulation are diatoms 
#               with large cell size, 
#               (high silicate content and high sinking rates)
#               such as Aulacoseira islandica or A. subarctica, 
#               whereas blooms that occur during spring after stratification onsset
#               are dominated by small, fast-growing diatoms.

#Schritt 1: Lade Daten

#Schritt 2: Fasse Centrales als große (>= 20) und kleine (<20) Centrales zusammen

#Schritt 3: Interpoliere fehlende Werte aller Spezies (hermetische Interpolation) 

#Schritt 4: Erstelle Subsets für Winter Zirkulation und Stratification

#Schritt 5: Aggregiere Biomassen aller Kieselalgen Spezies für beide Subsets




#Schritt 6: Prüfe, ob im während der Winter Zirkulation große und während
#           der Stratification im Sommer kleine Spezies dominieren

#Schritt 7: Erstelle Plots, welche Ergebnisse visualisieren




#benötigte Pakete
library(dplyr)
library(lubridate)
library(ggplot2)
library(pracma)
library(data.table)

#Schritt 1: Lade Daten--------

#Phytoplankton Daten (inklusive Spezies)
bac<- read.csv("newphyto.csv", sep=";")
bac$time<- as.POSIXct(bac$date, format= "%d.%m.%Y", tz= "UTC")

#nur Kieselalgen
bac<- subset(bac, bac$division== "bacillariophyta")
bac<- bac[,c("time", "species", "biomass")]



#Schritt 3: Interpolation für alle Spezies-------

#change to wide format, add days with missing data:
bacwide<- reshape(bac, idvar= "time", timevar= "species", direction= "wide")
colnames(bacwide)[-1]<- gsub("biomass.","",colnames(bacwide)[-1])

bacall1<- data.frame(time= seq(min(bac$time), max(bac$time), by= "days"))
bacall1<- merge(bacall1, bacwide, by= "time", all.x= T)


#hermetische Interpolation

#letzte spalte mit spezies daten
splast<- (1+length(unique(bac$species)))
bacall2<- bacall1
bacall2$time<- as.numeric(difftime(bacall2$time, bacall2$time[1], units= "days"))
x<- bacall2[!is.na(bacall2$Achnanthes.cf..exigua),]$time
seq1<- min(x):max(x)
for(i in 2:splast){
  y<- bacall2[!is.na(bacall2[,i]),][,i]
  bacall2[,i]<- pchip(x,y,seq1)
}
bacall2$time<- bacall1$time


#Schritt 4: Erstelle Subsets für Winterzirkulation und nach Stratification (Frühling)----

#Stratification Daten und Jahreszeit
strat1<- read.csv("ba_dataset.csv", sep= ";")
strat1$time<- as.POSIXct(strat1$time, format= "%Y-%m-%d", tz= "UTC")
strat<- strat1[,c("time", "strat", "bacil_int")]




#merge
st<- merge(bacall2, strat, by= "time")
#Entferne 2020: Probleme bei Berechnung von Stratification Beginn
st<- st[year(st$time)!= 2020,]


st_no_centrales<- select(st, -c("Centrales","Centrales...5",
                                "Centrales.10.15","Centrales.5.10", 
                                "Centrales.5.15..medium", "Centrales.15.20", 
                                "Centrales..15..big", "Centrales.20.25",
                                "Centrales.20.30", "Centrales.25.30", 
                                "Centrales.30."))
  


centrales_full<- st[,c("time", "Centrales")]
cfull_y<- aggregate(centrales_full$Centrales, list(year(centrales_full$time)), mean)


centrales_size<- st[,c("time","Centrales...5",
                       "Centrales.10.15","Centrales.5.10", 
                       "Centrales.5.15..medium", "Centrales.15.20", 
                       "Centrales..15..big", "Centrales.20.25",
                       "Centrales.20.30", "Centrales.25.30", 
                       "Centrales.30.")]                         
centrales_size2<- centrales_size
centrales_size2$sum<- rowSums(centrales_size[,-1])
csize_y<- aggregate(centrales_size2$sum, list(year(centrales_size2$time)), mean)


            




st$year<- year(st$time)
st[st$strat==1 & 
     month(st$time) %in% 1:2,]$year<- st[st$strat==1 & month(st$time) %in% 1:2,]$year-1

#subsets: Daten während Winterzirkulation (Strat== 0)
st0<- subset(st, st$strat== 0)
#Daten im Frühling während Stratification (Strat== 1)
st1<- subset(st, st$strat== 1 & yday(st$time) %in% 79:171)


#Schritt 5: Aggregiere jährliche Mittelwerte beider Subsets-----

#aggregate yearly means
st0mean<- aggregate(st0[,2:splast], list(st0$year), mean, na.rm= T)
st1mean<- aggregate(st1[,2:splast], list(st1$year), mean, na.rm= T)
#(unwichtig, nur zum Vergleich)
stmean<- aggregate(st[,2:splast], list(st$year), mean, na.rm= T)


#Berechne relative Mittelwerte
years<- 1994:2019
df0<- NULL
df1<- NULL
for(i in 1:26){
  df0a<- data.frame(species= names(sort(100* st0mean[i,2:splast]/ rowSums(st0mean[i,2:splast]), 
                                        decreasing = T)),
                    biomass= as.numeric(round(sort(100* st0mean[i,2:splast]/ rowSums(st0mean[i,2:splast]),
                                                   decreasing = T),3)),
                    year= years[i]
  )
  df1a<- data.frame(species= names(sort(100* st1mean[i,2:splast]/ rowSums(st1mean[i,2:splast]), 
                                        decreasing = T)[1:5]),
                    biomass= as.numeric(round(sort(100* st1mean[i,2:splast]/ rowSums(st1mean[i,2:splast]),
                                                   decreasing = T)[1:5],3)),
                    year= years[i]
  )
  df0<- rbind(df0, df0a)
  df1<- rbind(df1, df1a)
}


#df0----
df0max<- aggregate(df0$biomass, list(df0$species), max)
colnames(df0max)<- c("species", "max_rel_biomass")
df0max<- df0max[order(df0max$max_rel_biomass, decreasing = T),]

df0topx<- df0max[df0max$max_rel_biomass >10,]$species
df0top<-  df0[df0$species %in% df0topx,]
ggplot(df0top, aes(x=year, y= biomass))   +
  geom_point(aes(colour = factor(species)), size= 4)   +
  geom_line(aes(colour = factor(species))) +
  labs(color= "Spezies",
       xlab= "Jahr",
       ylab= "Relative Biomasse",
       title= "Winter Zirkulation")


#df1----
df1max<- aggregate(df1$biomass, list(df1$species), max)
colnames(df1max)<- c("species", "max_rel_biomass")
df1max<- df1max[order(df1max$max_rel_biomass, decreasing = T),]

df1topx<- df1max[df1max$max_rel_biomass >10,]$species
df1top<-  df1[df1$species %in% df1topx,]
ggplot(df1top, aes(x=year, y= biomass))   +
  geom_point(aes(colour = factor(species)), size= 4)   +
  geom_line(aes(colour = factor(species))) 



