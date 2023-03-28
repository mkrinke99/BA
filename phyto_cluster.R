
library(vegan)
library(lubridate)
library(dplyr)
library(dendextend)
library(tidyr)
library(ggplot2)


phy1<- read.csv("phyto_int.csv", sep= ";")
phy1$time<- as.POSIXct(phy1$time, tz= "UTC")
phy<- subset(phy1, month(phy1$time) %in% 1:5)

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")


#yearly means absolute and relative biomass
mean<- aggregate(phy[,c( 2: 8)], list(phy$year), mean, na.rm=T)
rel<-  aggregate(phy[,c(18:24)], list(phy$year), mean, na.rm=T)


#boxplots yearly biomass vorbereitung

#names and cols------
divnames<- c( "Bacillariophyta", "Chlorophyta", "Chrysophyta",
              "Cryptophyta", "Cyanophyta","Dinophyta", "Sonstige")

divcols<- c("chartreuse3", "tan","lightskyblue1", "thistle", 
            "orange", "darkolivegreen2", "grey64")

#dendogram-----

#absolute values
x <- decostand(mean[,-1], "normalize")
dend <- as.dendrogram(hclust(dist(x), method= "complete"))
labels(dend) <- mean$Group.1[order.dendrogram(dend)]

dend <- dend %>%
  color_labels(k =  7) %>%
  color_branches(k= 7) %>%
  set("branches_lwd", 2)

par(cex= 0.67, mar= c(9,5,4,1), mfrow= c(1,1))
p1<- plot(dend, las= 1)


#relative values dend------
x <- decostand(rel[,-1], "normalize")
dend <- as.dendrogram(hclust(dist(x), method= "complete"))
labels(dend) <- rel$Group.1[order.dendrogram(dend)]

dend <- dend %>%
  color_labels(k =  4) %>%
  color_branches(k= 4) %>%
  set("branches_lwd", 2)

par(cex= 0.67, mar= c(9,5,4,1), mfrow= c(1,1))
p1<- plot(dend, las= 1, 
          main= "Dendogram relative Biomassen (Januar-Mai)",
          sub= "Methode: complete linkage")


#barplot means per species-----
relr<- cbind(year= rel[,1], round(rel[,2:8], 3))
relr<- relr[order.dendrogram(dend),]
relr$group<- c(rep(1,3), rep(2,8), rep(3,8), rep(4,8))
relr$groupcol<- c(rep("navy",3), rep("royalblue",8),
                  rep("forestgreen",8), rep("lightgreen",8))


sort(round(colMeans(rel[,2:8]), 3))

par(mfrow= c(2,4))
for(i in c(1,5,4,3,6,2,7)){
  b1<- barplot(relr[,(i+1)], ylim= c(0, 1.15*max(relr[,(i+1)])), 
               space= c(1,1,1, 3, 1,1,1,1,1,1,1, 3, 1,1,1,1,1,1,1, 3, 1,1,1,1,1,1,1),
               col= relr$groupcol, las= 1, main= divnames[i], cex.main= 1.8)
  abline(v= c(7.5,25.5, 43.5), lty= 2, col= "grey33")
  text(55, 1.05*max(relr[,(i+1)]), cex= 1.1,
       paste0("mean: ", round(mean(relr[,(i+1)]), 4)))
  text(b1, par("usr")[3], labels = relr$year, srt = 45, 
       adj = c(1.1,1.1), xpd = TRUE, cex= 0.86) 
}
plot(0,0, axes= F, xlab= "", ylab= "", col= "white")
legend("top", legend= c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4"),
       fill= unique(relr$groupcol), box.lty=0, cex= 1.8)







#bacil biomass for days-----


#baciladd<- data.frame(time= bacil$time[1],
#                     bacil_int= bacil$bacil_int[1],
#                      year= 1994, 
#                      day= c(paste0("01-0", 1:9), paste0("01-", 10:19)))
#bacil<- rbind(baciladd, bacil)
#bacil<- bacil[bacil$day!= "02-29",]#
#bacil_d<-reshape(bacil[,-1], idvar = "year", 
#                 timevar = "day", direction = "wide")
#dend
#x <- decostand(bacil_d[,-1], "normalize")
#dend <- as.dendrogram(hclust(dist(x), method= "complete"))
#labels(dend) <- bacil_d$year[order.dendrogram(dend)]
#dend <- dend %>%
#  color_labels(k =  4) %>%
#  color_branches(k= 4) %>%
#  set("branches_lwd", 2)
#par(mfrow= c(1,1), mar= c(7,5,5,4))
#p1<- plot(dend, las= 1)

#reshape to compare days
bacil<- phy[,1:2]
bacil$year<- year(bacil$time)
bacil$date<-format(bacil$time, format= "%m-%d")
bacil$day<- yday(bacil$time)





bacil$diff<- c(1,diff(bacil$bacil_int))
bacil$growth<- bacil$diff/bacil$bacil_int




bacil_y<- aggregate(bacil$bacil_int, list(bacil$year), max)
bacil_y$maxday<- 0
for(i in 1:27){
bacil_y$maxday[i]<- bacil[bacil$year== i+1993 &
                          bacil$bacil_int== bacil_y[i,2],]$day
}



x <- decostand(bacil_y[,-1], "normalize")
dend <- as.dendrogram(hclust(dist(x), method= "complete"))
labels(dend) <- bacil_y[,1][order.dendrogram(dend)]

dend <- dend %>%
  color_labels(k =  4) %>%
  color_branches(k= 4) %>%
  set("branches_lwd", 2)

par(mfrow= c(1,1), mar= c(7,5,5,4))
p1<- plot(dend, las= 1)


#.------
#monthly means
bacil$month<- month(bacil$time)
bacil_m<- aggregate(bacil$bacil_int, list(bacil$year, bacil$month), mean)
colnames(bacil_m)<- c("year", "month", "biomass")

mon_wide<- reshape(bacil_m, idvar = "year", timevar = "month", direction = "wide")
x <- decostand(mon_wide[,-1], "normalize")
dend <- as.dendrogram(hclust(dist(x), method= "complete"))
labels(dend) <- mon_wide[,1][order.dendrogram(dend)]

dend <- dend %>%
  color_labels(k =  4) %>%
  color_branches(k= 4) %>%
  set("branches_lwd", 2)

par(mfrow= c(1,1), mar= c(7,5,5,4))
p1<- plot(dend, las= 1)




#lines yearly means-----
colnames(mean)<- c("year", divnames)
ggplot(mean, aes(x = year)) +
  geom_line(aes(y = Bacillariophyta,  colour = divnames[1]), size= 1.9) +
  geom_line(aes(y = Chlorophyta,      colour = divnames[2]), size= 1.1) +
  geom_line(aes(y = Chrysophyta,      colour = divnames[3]), size= 1.1) +
  geom_line(aes(y = Cryptophyta,      colour = divnames[4]), size= 1.1) +
  geom_line(aes(y = Cyanophyta,       colour = divnames[5]), size= 2) +
  geom_line(aes(y = Dinophyta,        colour = divnames[6]), size= 1.1) +
  geom_line(aes(y = Sonstige,         colour = divnames[7]), size= 1.1) +
  scale_colour_manual("", breaks = divnames,values = divcols) +
  labs(x = "Jahr", y = expression("Biomasse [µg L"^ "-1"* "]"), 
       title = "Absolute Biomasse aller Spezies") +
  theme_bw() 


colnames(rel)<- c("year", divnames)
ggplot(rel, aes(x = year)) +
  geom_line(aes(y = Bacillariophyta,  colour = divnames[1]), size= 2) +
  geom_line(aes(y = Chlorophyta, colour = divnames[2]), size= 1.1) +
  geom_line(aes(y = Chrysophyta, colour = divnames[3]), size= 1.1) +
  geom_line(aes(y = Cryptophyta, colour = divnames[4]), size= 1.1) +
  geom_line(aes(y = Cyanophyta,  colour = divnames[5]), size= 2) +
  geom_line(aes(y = Dinophyta,   colour = divnames[6]), size= 1.1) +
  geom_line(aes(y = Sonstige,   colour = divnames[7]), size= 1.1) +
  scale_colour_manual("", breaks = divnames,values = divcols) +
  ylim(0, 1) +
  labs(x = "Jahr", y = "Relative Biomasse [%]", 
       title = "Relative Biomasse aller Spezies") +
  theme_bw() 






#.-----
#relr groop means-----

relmeans<- aggregate(relr[,2:8], list(relr$group), mean)
relmeans<- rbind(colMeans(rel[,2:8]), relmeans[,-1])
relmeans$group<- c("mean", "group1", "group2", "group3", "group4")
relmeans$groupcol<- c("darkorchid4", unique(relr$groupcol))

par(mfrow= c(2,4))
for(i in c(1,5,4,3,6,2,7)){
  b1<- barplot(relmeans[,(i+1)], ylim= c(0, 1.2*max(relmeans[,(i+1)])), 
               col= relmeans$groupcol, las= 1, main= colnames(relr)[i+1])
  text(b1, par("usr")[3], labels = relmeans$group, srt = 45, 
       adj = c(1.1,1.8), xpd = TRUE, cex= 0.9) 
}

#save table of group means------
df113<- cbind(relmeans[,8], round(relmeans[,1:7], 3))
#write.table(df113, "groupmeans_relbiomass.csv", sep= ";")


#plots correl----
relr34<- subset(relr, relr$group %in% 3:4)
#Correlation of Crypto and Cyano for years in Group 3 and Group 4: -0.636
cor(relr34$crypto_rel, relr34$cyano_rel)
#plot
ggplot(relr34, aes(x= cyano_rel, y= crypto_rel))+
  geom_point(aes(colour= factor(group)),size= 4.7) +
  scale_colour_manual(values = unique(relr34$groupcol))  +
  xlim(0, 0.5)+
  ylim(0, 0.4)+
  labs(color= "Gruppe",
       x= "Relative Biomasse Cyanophyta [%]",
       y= "Relative Biomasse Cryptophyta [%]",
       title= "Relative Biomasse Cyanophyta~Cryptophyta (Gruppe 3 und Gruppe 4)")+
  theme_light()


#Correlation for Bacil and Crypto: -0.729
cor(relr$bacil_rel, relr$cyano_rel)
#plot
ggplot(relr, aes(bacil_rel, cyano_rel)) +
  geom_point(aes(colour = factor(group)), size= 4)  +
  scale_colour_manual(values = unique(relr$groupcol))  +
  labs(color= "Gruppe",
       x= "Relative Biomasse Bacillariophyta [%]",
       y= "Relative Biomasse Cyanophyta [%]", color= relr$group,
       title= "Relative Biomasse Bacillariophyta~Cyanophyta")  +
  theme_light()






#add st data-----

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")

#approx missing PAR values
stpar<- st[,c("time", "PAR")]
stpar$date<- format(as.Date(st$time), format= "%m-%d")
stagg<- aggregate(stpar$PAR, list(stpar$date), mean, na.rm= T)
colnames(stagg)<- c("date", "PARmean")
stpar<- merge(stpar, stagg, by= "date")
stpar<- stpar[order(stpar$time),]
#change NAs to date means
st[is.na(st$PAR),]$PAR<- stpar[is.na(stpar$PAR),]$PARmean
#change 29.02.
st[st$time== as.POSIXct("1996-02-29", tz= "UTC"),]$PAR<- st[st$time== 
                           as.POSIXct("1996-02-28", tz= "UTC"),]$PAR


st_y<- aggregate(st[,c(18:30, 32:41)], list(year(st$time)), mean)
colnames(st_y)[1]<- "year"

ymeans<- merge(relr, st_y, by= "year")
ymeans$total_biomass<- rowSums(phy_y[,2:8])
ymeansg<- ymeans[order(ymeans$group, ymeans$year),]


for(i in 11:34){
di<- range(ymeansg[,i])
par(mfrow= c(1,1))
plot(ymeansg[,i],
     pch= 16, col= ymeansg$groupcol, axes= F,
     ylim= c(di[1] -diff(di)*0.05, di[2] +diff(di)*0.05),
     main= colnames(ymeansg)[i], xlab= "", ylab= "")
axis(2, cex.axis= 0.8, las= 1)
abline(v= c(3.5, 11.5, 19.5), lty= 2, col= "grey55")
segments(1, mean(ymeansg[ymeansg$group==1,][,i]), 
         3, mean(ymeansg[ymeansg$group==1,][,i]), 
         lty= 2, col= unique(ymeansg$groupcol)[1])
segments(4, mean(ymeansg[ymeansg$group==2,][,i]), 
         11, mean(ymeansg[ymeansg$group==2,][,i]), 
         lty= 2, col= unique(ymeansg$groupcol)[2])
segments(12, mean(ymeansg[ymeansg$group==3,][,i]), 
         19, mean(ymeansg[ymeansg$group==3,][,i]), 
         lty= 2, col= unique(ymeansg$groupcol)[3])
segments(20, mean(ymeansg[ymeansg$group==4,][,i]), 
         27, mean(ymeansg[ymeansg$group==4,][,i]), 
         lty= 2, col= unique(ymeansg$groupcol)[4])
text(1:27,  ymeansg[,i],
     pos= 3, col= ymeansg$groupcol,
     ymeansg$year, xpd= T, cex= 0.7)
text(c(2,7.5,15.5,23.5), min(ymeansg[,i])-diff(di)*0.1, 
     c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4"), 
     xpd=T, col= unique(ymeansg$groupcol))
}


library(ggpubr)
library(factoextra)
library(ggrepel)

df <- ymeans[,11:23]
res.km <- kmeans(scale(df), 4, nstart = 25)
res.km$cluster

fviz_cluster(res.km, data = df,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)+
geom_text_repel(aes(label= ymeans$group))


#kruskal-wallis-test------
for(i in c(2:8,11:33)){
print(paste0("Kruskal-Wallis Test ", colnames(ymeans)[i], 
             " ~ groups  p-value:  ",
  round(kruskal.test(ymeans[,i] ~ ymeans$group)$p.value,4)
))
  plot(ymeans$group, rank(ymeans[,i]), pch= 16, col= ymeans$groupcol,
       xlab= "Gruppe", ylab= "rang", main= colnames(ymeans)[i],
       cex= 1.3, axes= F)
  text(1:4, -1.1, 1:4, xpd= T, cex= 1.2)
  axis(2, c(1,10,20,27), c(1,10,20,27),las=1)
}








