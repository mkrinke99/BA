

library(lubridate)
library(dplyr)


phy1<- read.csv("phyto_int.csv", sep= ";")
phy1$time<- as.POSIXct(phy1$time, tz= "UTC")
phy<- subset(phy1, month(phy1$time) %in% 1:5)

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")

#Spezies
spe1<- read.csv("newphyto.csv", sep= ";")
spe1$time<- as.POSIXct(spe1$date, format= "%d.%m.%Y", tz= "UTC")
spe<- subset(spe1, spe1$division== "bacillariophyta")
spe<- spe[,c("time","species","biomass")]
spe<- spe[spe$species!= "Centrales",]

specmean<- aggregate(spe$biomass, list(spe$species), mean, n.rm=T)
specmean$rel<- round(specmean[,2]/sum(specmean[,2]),5)

#monthly means
bac<- cbind(phy[,c("time", "bacil_int","bacil_rel")], st[,c("PAR","strat")])

bac_m<- aggregate(bac[,c("bacil_int","PAR")], 
                  list(year(bac$time), month(bac$time)), mean, na.rm=T)
colnames(bac_m)[1:2]<- c("year", "month")
bac_1<- bac_m[bac_m$month==1 & !is.na(bac_m$PAR),]
bac_2<- bac_m[bac_m$month==2 & !is.na(bac_m$PAR),]
bac_3<- bac_m[bac_m$month==3 & !is.na(bac_m$PAR),]
bac_4<- bac_m[bac_m$month==4 & !is.na(bac_m$PAR),]
bac_5<- bac_m[bac_m$month==5 & !is.na(bac_m$PAR),]


#cor biomas~Var for each month
for(i in 1:5){
  bacmon<- subset(bac_m, bac_m$month==i & !is.na(bac_m$PAR))
  print(paste0("cor biomass_bacil~PAR: ",
              round(cor(bacmon$bacil_int, bacmon$PAR),4), 
              " (", month.abb[i], ")"))}
  for(i in 1:5){
  bacmon<- subset(bac_m, bac_m$month==i & !is.na(bac_m$PAR))
  print(paste0("rank cor biomass_bacil~PAR: ",
               round(cor.test(bacmon$bacil_int, bacmon$PAR, method= "spearman")$estimate,4), 
               " (", month.abb[i], ")"))}


#4 vars maximum, time of maximum, PAR and strat begin
bacil4v<- data.frame(year<- 1994:2020,
                     max=  aggregate(bac$bacil_int, list(year(bac$time)), max)[,2],
                     maxday=  aggregate(bac$bacil_int, list(year(bac$time)), which.max)[,2],
                     strat=   c(yday(bac[bac$strat - lag(bac$strat) ==1,][-1,]$time),150),
                     PAR=    aggregate(bac$PAR, list(year(bac$time)), mean)[,2]
                        )
bacil4v$PAR[1:4]<- mean(bacil4v$PAR, na.rm=T)


panel.cor <- function(x, y, digits = 3, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- cor(x, y) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * abs(Cor)) # Resize the text by level of correlation
}
# Plotting the correlation matrix
pairs(bacil4v[,2:5], pch= 16, col= "grey16", lwd= 2,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) 


#dendogram---------
x <- decostand(bacil4v[,-1], "normalize")
dend <- as.dendrogram(hclust(dist(x), method= "complete"))
labels(dend) <- bacil4v[,1][order.dendrogram(dend)]

dend <- dend %>%
  color_labels(k =  5) %>%
  color_branches(k= 5) %>%
  set("branches_lwd", 2)

par(cex= 0.67, mar= c(9,5,4,1), mfrow= c(1,1))
p1<- plot(dend, las= 1)

