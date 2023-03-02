
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














relr<- cbind(year= rel[,1], round(rel[,2:8], 3))
relr<- relr[order.dendrogram(dend),]
relr$group<- c(rep(1,3), rep(2,8), rep(3,8), rep(4,8))
relr$groupcol<- c(rep("navy",3), rep("royalblue",8),
                  rep("forestgreen",8), rep("lightgreen",8))

#barplot means per species-----
sort(round(colMeans(rel[,2:8]), 3))
par(mfrow= c(2,4))
for(i in c(1,5,4,3,6,2,7)){
b1<- barplot(relr[,(i+1)], ylim= c(0, 1.15*max(relr[,(i+1)])), 
        space= c(1,1,1, 3, 1,1,1,1,1,1,1, 3, 1,1,1,1,1,1,1, 3, 1,1,1,1,1,1,1),
     col= relr$groupcol, las= 1, main= divnames[i])
abline(v= c(7.5,25.5, 43.5), lty= 2, col= "grey33")
text(55, 1.05*max(relr[,(i+1)]), cex= 0.9,
     paste0("mean: ", round(mean(relr[,(i+1)]), 4)))
text(b1, par("usr")[3], labels = relr$year, srt = 45, 
     adj = c(1.1,1.1), xpd = TRUE, cex= 0.9) 
}


#relr groop means-----

relmeans<- aggregate(relr[,2:8], list(relr$group), mean)
relmeans<- rbind(colMeans(relmeans[1:4,]), relmeans)
relmeans[1,1]<- "mean"
relmeans[2:5,1]<- paste("group", relmeans[2:5,1])
colnames(relmeans)[1]<- "group"
relmeans$groupcol<- c("darkorchid4", unique(relr$groupcol))

par(mfrow= c(2,4))
for(i in c(1,5,4,3,6,2,7)){
  b1<- barplot(relmeans[,(i+1)], ylim= c(0, 1.2*max(relmeans[,(i+1)])), 
               col= relmeans$groupcol, las= 1, main= colnames(relr)[i+1])
  text(b1, par("usr")[3], labels = relmeans$group, srt = 45, 
       adj = c(1.1,1.8), xpd = TRUE, cex= 0.9) 
}










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



#plot all biomasses in one------
ggplot(phy_y_long, aes(x = year, y = biomass)) +
  geom_area(aes(color = species, fill = species, alpha= 0.3))+
  labs(x = "Jahr", y = expression("Biomasse [µg L"^ "-1"* "]"), 
       title = "Biomasse (Durchschnittswerte Januar-Mai)") +
  theme_bw() 



par(mfrow= c(3,9))
for(yy in 1994:2020){
  py<- subset(phy, year(phy$time)== yy)
  ymax= 4000
  if(yy %in% 2017:2019){ymax= 11000}
  plot(py$time, py$bacil_int, type= "l", col= "chartreuse3", lwd= 2,
       las= 1, ylim= c(0,ymax), cex.axis= 0.9,
       xlab= "", ylab= "", main= yy)
}








