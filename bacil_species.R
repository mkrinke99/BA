

library(lubridate)
library(dplyr)
library(ggplot2)

phy1<- read.csv("phyto_int.csv", sep= ";")
phy1$time<- as.POSIXct(phy1$time, tz= "UTC")
phy<- subset(phy1, month(phy1$time) %in% 1:5)

st12<- read.csv("ba_dataset.csv", sep= ";")
st12$time<- as.POSIXct(st12$time, format= "%Y-%m-%d", tz= "UTC")
st<- st[month(st12$time) %in% 1:5,]

#Spezies
spe1<- read.csv("newphyto.csv", sep= ";")
spe1$time<- as.POSIXct(spe1$date, format= "%d.%m.%Y", tz= "UTC")
spe<- subset(spe1, spe1$division== "bacillariophyta")
spe<- spe[,c("time","species","biomass")]
spe<- spe[spe$species!= "Centrales",]

specmean<- aggregate(spe$biomass, list(spe$species), mean, n.rm=T)
specmean$rel<- round(specmean[,2]/sum(specmean[,2]),5)
colnames(specmean)<- c("species", "biomass", "biomass_rel")






spe6<- specmean[specmean$species %in% c("Asterionella.formosa",
                                        "Diatoma.elongatum",
                                        "Synedra.spp.",
                                        "Fragilaria.crotonensis"),]



#monthly means
bac<- cbind(phy[,c("time", "bacil_int","bacil_rel")], st[,c("strat")])

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


#bacil4
#4 vars maximum, time of maximum, PAR and strat begin------
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
  Cor <- cor(x, y) 
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * abs(Cor)) 
}


# Plotting the correlation matrix----
pairs(bacil4v[,2:6], pch= 16, col= "grey16", lwd= 2,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) 

#rank correl------
bacil4vrank<- data.frame(year= 1994:2020,  
                            max_rank= rank(bacil4v$max),
                            maxday_rank= rank(bacil4v$maxday),
                            strat_rank= rank(bacil4v$strat),
                            PAR_rank=rank(bacil4v$PAR))

pairs(bacil4vrank[,2:5], pch= 16, col= "grey16", lwd= 2,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) 


#function cor unterschiedliche monate, rank or no rank-----
cor4<- function(months, userank){
  bacm<- bac[month(bac$time) %in% months,]
  df<- data.frame(year<- 1994:2020,
            max=     aggregate(bacm$bacil_int, list(year(bacm$time)), max)[,2],
            maxday=  aggregate(bac$bacil_int, list(year(bac$time)), which.max)[,2],
            strat=   c(yday(bac[bac$strat - lag(bac$strat) ==1,][-1,]$time),150),
            PAR=     aggregate(bacm$PAR, list(year(bacm$time)), mean)[,2]
  )
  df$PAR[1:4]<- mean(df$PAR, na.rm=T)
  if(userank== T){
    df[,2]<- rank(df[,2])
    df[,3]<- rank(df[,3])
    df[,4]<- rank(df[,4])
    df[,5]<- rank(df[,5])
  }
  pairs(df[,2:5], pch= 16, col= "grey16", lwd= 2,
        upper.panel = panel.cor,    # Correlation panel
        lower.panel = panel.smooth,
        main= "")  
  colnames(df)[1]<- "year"
  return(df)
}

df<- cor4(1:5, F)


#time difference between maximum day and strat begin
df$maxday_strat<- df$maxday - df$strat

ggplot(df, aes(x= year, y= maxday_strat))+
  geom_line(lwd= 2, col= "royalblue")  +
  geom_hline(yintercept= 0, linetype="dashed")  +
  labs(x = "Jahr", y = expression("Tage"), 
       title = "Abstand zwischen Kieselalgen Maximum und Wasserzirkulation [Tage]") +
  theme_bw() 
  







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



library("hydroTSM")
st12$season<- time2season(st12$time, out.fmt = "seasons")

aglong<- aggregate(st12$all, list(year(st12$time), st12$season), mean)
ag<- reshape(aglong, timevar= "Group.2", idvar = "Group.1", direction = "wide")
colnames(ag)<- c("year","autumn","spring","summer","winter")
ag<- ag[,c("year", "winter", "spring", "summer", "autumn")]
ag$secchi<- aggregate(st12$secchi, list(year(st12$time)), mean)$x

#apply same method with winter biomass bacil 
# andtotal biomass in summer

#winter diatoms vs summer cyano

#detrend data -> decompose, divide trend component

#nutrients tp!, (si), no3, (tn)






#without rank
pairs(ag[,-1], pch= 16, col= "grey16", lwd= 2,
      upper.panel = panel.cor,    
      lower.panel = panel.smooth) 

#ranks
agr<- data.frame(year= 1994:2020,
                    winter_rank= rank(ag[,2]),
                    spring_rank= rank(ag[,3]),
                    summer_rank= rank(ag[,4]),
                    autumn_rank= rank(ag[,5]),
                    secchi_rank= rank(ag[,6]))

pairs(agr[,-1], pch= 16, col= "grey16", lwd= 2,
      upper.panel = panel.cor,    
      lower.panel = panel.smooth) 

decompose(st12$bacil_int)
 


library(forecast)





