

library(tidyverse)
library(lubridate)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ggplot2)


setwd("~/Uni/Praktikum/data/stechlin_martin")
st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")

#approx missing PAR values----
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

#yearly means
st_y<- aggregate(st[,c(2:9, 18:28, 30)],
                 list(year(st$time)), mean)
colnames(st_y)[1]<- "year"

#plot yearly means------

lo<- predict(loess(st_y[,"all"] ~ st_y$year))
ggplot(data = st_y, aes(x = year, y = all)) +
    geom_line(color = "dodgerblue3", linetype = "solid") +
    geom_point(color = "dodgerblue3", shape = 16) +
    geom_line(aes(y = lo), color = "grey33", linetype = "solid") +
    labs(x = "year", y = "")+
    labs(title(var))+
    theme_classic()




#PCA------

#with yearly means
numerical_data <- st_y[,-1][1:26,]
data_normalized <- scale(numerical_data)
head(data_normalized)
#correl matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
#PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, 
             col.var = "grey11")





plot(st_y[,1], st_y$wt, type= "b", col= "dodgerblue3",las=1,pch=16)
lines(st_y[,1], lo, col= "grey33")

#with data
stx<- st[,c(1:8, 18:28, 30)]

par1<- aggregate(st$PAR, list(paste0(day(st$time),"-",month(st$time))),
                 mean, na.rm= T)
par1$date<- as.Date(paste0(par1[,1],"-2020"), format= "%d-%m-%Y")
par1[par1$Group.1== "29-2",]$x= par1[par1$Group.1== "28-2",]$x
par1<- par1[order(par1$date),]
stx[is.na(stx$PAR),]$PAR <- rep(par1$x, 4)[1:494]

numerical_data <- stx[,-1]
data_normalized <- scale(numerical_data)
head(data_normalized)
#correl matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
#PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, 
             col.var = "grey11")



#boxplots yearly biomass------
library(tidyr)
library(tidyverse)
library(hrbrthemes)

phy_y<- aggregate(st[,2:8], list(year(st$time)), mean)
colnames(phy_y)[1]<- c("year", "Bacillariophyta")


phyorder1<- sort(colSums(phy_y[,2:8]), decreasing = T)
phyorder<-  rownames(as.data.frame(phyorder1))


phy_y_long<- gather(phy_y, species, biomass, bacil_int:others_int)
phy_y_long$species <- factor(phy_y_long$species,
                            levels = phyorder,
                            ordered = TRUE)


#Boxplot Jahresmittelwerte mit log. y-Achse------
phy_y_long %>%
ggplot( aes(x= species, y= biomass, fill= species)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size= 1.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Jahresmittelwerte der Biomassen") +
  xlab("Spezies")+
  ylab(expression("Biomasse [µg L"^ "-1"* "]"))+
  scale_y_continuous(trans='log10')


#Berechne Mittelwert und Median
apply(phy_y, 2, FUN= median)
apply(phy_y, 2, FUN= mean)
#Bacil mit größtem Mittelwert und Median
#Cyano mit zweitgrößtem Mittelwert (etwa halb so groß wie Bacil), jedoch
#nur Drittgrößter Median da sehr linksschief
#Allgemein sehr linksschiefe Verteilung durch hohe Werte in den 2010ern























