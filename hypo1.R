
#Hypothesis 1: "The species that dominate during late winter circulation are diatoms 
#               with large cell size, 
#               (high silicate content and high sinking rates)
#               such as Aulacoseira islandica or A. subarctica, 
#               whereas blooms that occur during spring after stratification onsset
#               are dominated by small, fast-growing diatoms.



#Schritt 1: Erstelle Subsets für Winter Zirkulation und Stratification

#Schritt 2: Aggregiere Biomassen aller Kieselalgen Spezies für beide Subsets

#Schritt 3: Prüfe, ob im während der Winter Zirkulation große und während
#           der Stratification im Sommer kleine Spezies dominieren

#Schritt 4: Erstelle Plots, welche Ergebnisse visualisieren



#Daten--------

library(dplyr)
library(lubridate)
library(ggplot2)
library(pracma)

#Phytoplankton Daten (inklusive Spezies)
bac<- read.csv("newphyto.csv", sep=";")
bac$time<- as.POSIXct(bac$date, format= "%d.%m.%Y", tz= "UTC")

#nur Kieselalgen
bac<- subset(bac, bac$division== "bacillariophyta")
bac<- bac[,c("time", "species", "biomass")]
#Entferne Centrales -> kommt im Datensatz mehrmals vor
centrales<- bac[bac$species == "Centrales",]
bac<- bac[bac$species != "Centrales",]

#change to wide format, add days with missing data:
bacwide<- reshape(bac, idvar= "time", timevar= "species", direction= "wide")
colnames(bacwide)[-1]<- gsub("biomass.","",colnames(bacwide)[-1])

bacall1<- data.frame(time= seq(min(bac$time), max(bac$time), by= "days"))
bacall1<- merge(bacall1, bacwide, by= "time", all.x= T)


#Lineare Interpolation
#bacall<-  bacall1
#for(i in 2:98){
#  bacall[,i]<- approx(bacwide$time, bacwide[,i], xout= bacall$time,
#                      rule= 2, method= "linear")$y
#}

#hermetische Interpolation
bacall2<- bacall1
bacall2$time<- as.numeric(difftime(bacall2$time, bacall2$time[1], units= "days"))
x<- bacall2[!is.na(bacall2$Achnanthes.cf..exigua),]$time
seq1<- min(x):max(x)
for(i in 2:98){
y<- bacall2[!is.na(bacall2[,i]),][,i]
bacall2[,i]<- pchip(x,y,seq1)
}
bacall2$time<- bacall1$time




#Stratification Daten
strat1<- read.csv("ba_dataset.csv", sep= ";")
strat1$time<- as.POSIXct(strat1$time, format= "%Y-%m-%d", tz= "UTC")
strat<- strat1[,c("time", "strat", "bacil_int")]

#merge
st<- merge(bacall2, strat, by= "time")
#Entferne 2020: Probleme bei Berechnung von Stratification Beginn
st<- st[year(st$time)!= 2020,]
st$year<- year(st$time)
st[st$strat==1 & 
   month(st$time) %in% 1:2,]$year<- st[st$strat==1 & month(st$time) %in% 1:2,]$year-1


#subsets: keine Stratification (==0) und Stratification (==1)
st0<- subset(st, st$strat== 0)
st1<- subset(st, st$strat== 1)


#aggregate yearly means
st0mean<- aggregate(st0[,2:98], list(st0$year), mean, na.rm= T)
st1mean<- aggregate(st1[,2:98], list(st1$year), mean, na.rm= T)
stmean<- aggregate(st[,2:98], list(st$year), mean, na.rm= T)

stmeanges<-  aggregate(st$bacil_int, list(st$year), mean)
years<- 1994:2019


df0<- NULL
df1<- NULL
for(i in 1:26){
df0a<- data.frame(species= names(sort(100* st0mean[i,2:98]/ rowSums(st0mean[i,2:98]), 
                               decreasing = T)[1:5]),
           biomass= as.numeric(round(sort(100* st0mean[i,2:98]/ rowSums(st0mean[i,2:98]),
                               decreasing = T)[1:5],3)),
           year= years[i]
          )
df1a<- data.frame(species= names(sort(100* st1mean[i,2:98]/ rowSums(st1mean[i,2:98]), 
                                      decreasing = T)[1:5]),
                  biomass= as.numeric(round(sort(100* st1mean[i,2:98]/ rowSums(st1mean[i,2:98]),
                                                 decreasing = T)[1:5],3)),
                  year= years[i]
)
df0<- rbind(df0, df0a)
df1<- rbind(df1, df1a)
}


#Centrales missing data
cent<- cbind(bacwide$time, bacwide[,which(substr(names(bacwide),1,4)== "Cent")])
plot(bacwide$time, rowSums(cent[,-1]), lwd=2, 
     col= "lightskyblue3", las= 1, type= "b", pch= 16, cex= 0.7)
points(centrales$time, centrales$biomass, col= "red", pch= 16, cex= 0.9)
legend("topleft", legend= c("Centrales", 
       "Centrales...5, 5-10, 5-15 medium, 10-15, 15-20, 15-big,  20-25. 20-30, 25-30, 30"),
       box.lty=0, fill= c("lightskyblue3","red"), cex= 0.8, inset= 0.02)
plot(rowSums(cent[,-1]) - centrales$biomass, ylim= c(-2500, 200))





for(yy in 1994:2019){
#  jpeg(file=paste0("species",yy,".jpeg"))
par(mfrow= c(1,2))
df0y<- subset(df0, df0$year== yy)
bp0<-barplot(df0y$biomass, ylim= c(0, 1.3*max(df0y$biomass)),
            las= 1, col= "chartreuse3", 
            main= paste("Winter Zirkulation",yy), ylab= "Relative Biomasse [%]")
text(bp0+0.3, df0y$biomass+4, df0y$species,
     cex= 0.75, srt= 45, pos= 3)

df1y<- subset(df1, df1$year== yy)
bp1<-barplot(df1y$biomass, ylim= c(0, 1.3*max(df1y$biomass)),
            las= 1, col= "chartreuse3", 
            main= paste("Stratification",yy), ylab= "Relative Biomasse [%]")
text(bp1+0.3, df1y$biomass+4, df1y$species,
     cex= 0.75, srt= 45, pos= 3)
#   dev.off()
}




