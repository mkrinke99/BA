
library(vegan)
library(lubridate)

setwd("~/Uni/Praktikum/data/stechlin_martin")
phy1<- read.csv("phyto_int.csv", sep= ";")
phy1$time<- as.POSIXct(phy1$time, tz= "UTC")
phy<- subset(phy, month(phy$time) %in% 1:5)



mean<- aggregate(phy[,c( 2: 8)], list(phy$year), mean, na.rm=T)
rel<-  aggregate(phy[,c(18:24)], list(phy$year), mean, na.rm=T)

spe.norm <- decostand(mean[,-1], "normalize")
spe.ch <- vegdist(spe.norm, "euc", na.rm=T)
# Attach site names to object of class 'dist'
attr(spe.ch, "labels") <- mean$Group.1
# Compute single linkage agglomerative clustering
spe.ch.single <- hclust(spe.ch, method = "complete")
# Plot a dendrogram using the default options
plot(spe.ch.single,
     labels = mean$Group.1,
     main = "Chord - Single linkage")



spe.norm <- decostand(rel[,-1], "normalize")
spe.ch <- vegdist(spe.norm, "euc", na.rm=T)
# Attach site names to object of class 'dist'
attr(spe.ch, "labels") <- rel$Group.1
# Compute single linkage agglomerative clustering
spe.ch.single <- hclust(spe.ch, method= "complete")
# Plot a dendrogram using the default options
plot(spe.ch.single,
     labels = rel$Group.1,
     main = "Chord - Single linkage")




colvec<- c("dodgerblue2", "red3", "orange", "navy", "chartreuse3", "tan ", "ivory4")
for(yy in 1994:2020){
  int_y= subset(phy, phy$year== yy)
  plot(int_y$time, int_y$bacil_rel, 
       ylim= c(0,1), las=1, type= "l", lwd= 3, col= colvec[1],
       main= yy)
  lines(int_y$time, int_y$chloro_rel, col= colvec[2])
  lines(int_y$time, int_y$chryso_rel, col= colvec[3])
  lines(int_y$time, int_y$crypto_rel, col= colvec[4])
  lines(int_y$time, int_y$cyano_rel,  col= colvec[5], lwd= 3)
  lines(int_y$time, int_y$dino_rel,   col= colvec[6])
  lines(int_y$time, int_y$others_rel, col= colvec[7])
}


#samlings
table(!is.na(phy$bacillariophyta))
#samplings per year
table(!is.na(phy$bacillariophyta), year(phy$time))







