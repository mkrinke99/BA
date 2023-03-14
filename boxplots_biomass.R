


library(ggplot2)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(dplyr)
library(tidyr)

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")


#boxplots yearly biomass vorbereitung

#names and cols------
divnames<- c( "Bacillariophyta", "Chlorophyta", "Chrysophyta",
              "Cryptophyta", "Cyanophyta","Dinophyta", "Sonstige")

divcols<- c("chartreuse3", "tan","lightskyblue1", "thistle", 
            "orange", "darkolivegreen2", "grey64")



#data in long format-----
phy_y<- aggregate(st[,2:8], list(year(st$time)), mean)
colnames(phy_y)<- c("year", divnames)

phyorder1<- sort(colSums(phy_y[,2:8]), decreasing = T)
phyorder<-  rownames(as.data.frame(phyorder1))


phy_y_long<- gather(phy_y, species, biomass, Bacillariophyta:Sonstige)
phy_y_long$species <- factor(phy_y_long$species,
                             levels = phyorder,
                             ordered = TRUE)


#.--------
#Boxplot Jahresmittelwerte Biomasse normal und mit log. y-Achse------
phy_y_long %>%
  ggplot( aes(x= species, y= biomass, fill= species)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(lwd= 0.75, outlier.size= 2.5, outlier.color= "grey25") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Biomasse Mittelwerte (Januar-Mai) ") +
  xlab("Spezies")+
  ylab(expression("Biomasse [µg L"^ "-1"* "]"))




#y-Achse log
phy_y_long %>%
  ggplot( aes(x= species, y= biomass, fill= species)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(lwd= 0.8) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey25", size= 1.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Biomasse Mittelwerte (Januar-Mai) \n logarithmierte y-Achse") +
  xlab("Spezies")+
  ylab(expression("Biomasse [µg L"^ "-1"* "]"))+
  scale_y_continuous(trans='log10')





#Berechne Mittelwert und Median
round(apply(phy_y[,-1], 2, FUN= median),2)
round(apply(phy_y[,-1], 2, FUN= mean),2)
#Bacil mit größtem Mittelwert und Median
#Cyano mit zweitgrößtem Mittelwert (etwa halb so groß wie Bacil), jedoch
#nur Drittgrößter Median da sehr linksschief
#Allgemein sehr linksschiefe Verteilung durch hohe Werte in den 2010ern






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




#plot yearly means-----
df<- phy_y[,1:2]

ggplot(data= df, aes(x= year, y= bacil_int))+
  geom_line(col= "chartreuse3", lwd= 2)+
  geom_point(col= "chartreuse4", size= 3.8)+
  ggtitle("Biomasse (Mittelwerte Januar-Mai)") +
  xlab("Jahr")+
  ylab(expression("Biomasse [µg L"^ "-1"* "]"))+
  theme_light()



#plot all biomasses in one------
ggplot(phy_y_long, aes(x = year, y = biomass)) +
  geom_area(aes(color = species, fill = species, alpha= 1))+
  labs(x = "Jahr", y = expression("Biomasse [µg L"^ "-1"* "]"), 
       title = "Biomasse (Durchschnittswerte Januar-Mai)") +
  theme_light() 



