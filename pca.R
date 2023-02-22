



library(corrr)
library(ggcorrplot)
library(FactoMineR)


setwd("~/Uni/Praktikum/data/stechlin_martin")
st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")

st97<- subset(st, !is.na(st$PAR))

#yearly means
st_y<- aggregate(st[,c(2:8, 18:28, 30)], list(year(st$time)), mean)
st_y$PAR[1:4]<- mean(st_y$PAR[5:7])

#PCA------

#with yearly means
numerical_data <- st_y[,-1]
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


#with data
stx<- st[,c(1:8, 18:28, 30)]
stx[is.na(stx$PAR),]$PAR <- rep(par1$x, 4)[1:494]

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



#boxplots yearly biomass
library(tidyr)

phy_y<- aggregate(st[,2:8], list(year(st$time)), mean)
colnames(phy_y)[1]<- "year"

phyorder1<- sort(colSums(phy_y[,2:8]), decreasing = T)
phyorder<-  rownames(as.data.frame(phyorder1))


phy_y_long<- gather(phy_y, species, biomass, bacil_int:others_int)
phy_y_long$species <- factor(phy_y_long$species,
                            levels = phyorder,
                            ordered = TRUE)


#Boxplot Jahresmittelwerte mit log. y-Achse
phy_y_long %>%
ggplot( aes(x= species, y= biomass, fill= species)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
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























