
library(dplyr)
library(lubridate)
library(trend)
library(ggplot2)

st<- read.csv("st_s.csv", sep= ";")

#change PAR values



#aggregate monthly means
stwinter<- subset(st, st$season== "winter")
stspring<- subset(st, st$season== "spring")
stsummer<- subset(st, st$season== "summer")
stautumn<- subset(st, st$season== "autumn")
stall<-    aggregate(st[,-c(1:2)], list(st$year), mean, na.rm= T)


#plots yearly averages physchemmet------
ynames<- c("", "", rep(expression("Biomasse [µg L"^ "-1"* "]"), 8),
           rep("Relative Biomasse [%]", 7), "", "Wassertemperatur [°C]",
           expression("Konzentration [mg L"^ "-1"* "]"), "pH",
           rep(expression("Konzentration [mg L"^ "-1"* "]"), 7),
           expression("PAR µmol m"^"-2"*"s"^"-1"), "m","m")
mainnames<- c("", "", "Bacillariophyta", "Chlorophyta", "Chrysophyta",
              "Cryptophyta", "Cyanophyta", "Dinophyta", "Sonstige",
              "Biomasse Gesamt", "Relative Biomasse \n Bacillariophyta",
              "Relative Biomasse \n Chlorophyta", "Relative Biomasse \n Chtysophyta",
              "Relative Biomasse \n Cryptophyta", "Relative Biomasse \n Cyanophyta",
              "Relative Biomasse \n Dinophyta", "Relative Biomasse \n Sonstige", "",
              "Wassertemperatur", "O2 Konzentration", "PH Wert", 
              "Konzentration Phosphor", 
              "Konzentration gelöster reaktiver Phosphor",
              "Konzentration Stickstoff", 
              "Konzentration Nitrit", 
              "Konzentration Nitrat", 
              "Konzentration Ammonium", 
              "Konzentration Silicium",
              "PAR",
              "Mixed Layer Tiefe")

years<- 1994:2020


mkplot<- function(vari, showplot, seasn){
  pvalres<- data.frame(var= colnames(st)[vari], p.value_L= 0, p.value_R=0)
  resvec<- vector()
  st_s<- subset(st, st$season== seasn)
  if(seasn== "all"){
    st_s<- stall
    st_s<- st_s[c(1,1, 2:ncol(st_s))]
    }
  for(i in vari){
    i1<- min(vari)
  var<- st_s[,i]
  mid<- 2007
  if(colnames(st_s)[i]== "PAR") {
    var2<-   var[5:27]
    mid<-   2009
    pvalr<- round(mk.test(var2, alternative = "greater")$p.value,5)
    pvall<- round(mk.test(var2, alternative = "less")$p.value,5)
    pval<-  min(pvall, pvalr)
    }
  pvalr<- round(mk.test(var, alternative = "greater")$p.value,5)
  pvall<- round(mk.test(var, alternative = "less")$p.value,5)
  pval<-  min(pvall, pvalr)
  result<- ""
  if(pvalr <  0.05) {result=   " *"}
  if(pvalr <  0.01) {result=  " **"}
  if(pvalr < 0.001) {result= " ***"}
df<- data.frame(years= years, var= var)
#plot
p1<-ggplot(df, aes(x= years, y= var)) +
  geom_line(color = "chartreuse3", size = 1) +
  geom_point(color = "chartreuse3", size = 3, shape = 16) +
  labs(x = "Jahr", y = ynames[i],
       title = mainnames[i]) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 8), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 12, face = "bold")) +
  ylim(min(var), max(var) + 0.08 * diff(range(var))) +
  annotate("text", x = median(df$years), y = max(var) + 0.05 * diff(range(var)), 
           label = paste0("p-Wert MK-Test_s: ", pval, result))
pvalres[(i-(i1-1)),2]<- pvall
pvalres[(i-(i1-1)),3]<- pvalr
if(showplot== T) {return(p1)}
  }
  if(showplot== F)return(pvalres)
}



mkwinter<- mkplot(3:31, F, "winter")
mkwinter$season<- "winter"

mkspring<- mkplot(3:31, F, "spring")
mkspring$season<- "spring"

mksummer<- mkplot(3:31, F, "summer")
mksummer$season<- "summer"

mkautumn<- mkplot(3:31, F, "autumn")
mkautumn$season<- "autumn"

mkall<- mkplot(3:31, F, "all")
mkall$season<- "all"

mktests<- cbind(mkwinter, mkspring, mksummer, mkwinter, mkall)


#save test results
library("xlsx")
#write.xlsx(mktests, file= "MKtest.xlsx", col.names= T)



#mk-test bacil without 2020-----
#winter
mk.test(stwinter$bacil_int[1:26], alternative = "greater")$p.value #0.0858
#spring
mk.test(stspring$bacil_int[1:26], alternative = "greater")$p.value #0.0236
#summer
mk.test(stsummer$bacil_int[1:26], alternative = "greater")$p.value #0.0614
#autumn
mk.test(stautumn$bacil_int[1:26], alternative = "greater")$p.value #0.0122
#all seasons
mk.test(stall$bacil_int[1:26], alternative= "greater")$p.value #0.0041





#plot significant trends-------

ggplot(stwinter, aes(x= 1994:2020, y= tp)) +
  geom_line(color= "dodgerblue3") +
  geom_point(col= "dodgerblue4") +
  geom_line(aes(x= 1994:2020, y= no3)) +
  theme_light()




#significant trends:
#srp:    0.00001 *** increase
#tp:     0.00001 *** increase
#secchi: 0.0003  *** decrease
#Si:     0.0136    * decrease
#pH:     0.06668     decrease


#i in 10:22,24:33
#plots monthly averages
stall<- list(stmean, mean01, mean02, mean03, mean04, mean05)
par(mfrow= c(2,3))
for(i in  c(9)){
  for(j in 1:6){
  jtext<- c("Jan-May", "Jan", "Feb", "Mar", "Apr", "May")
  stmean<- stall[[j]]
  years<- 1994:2020
  var<- stmean[,i]
  mid<- 2007
  if(colnames(stmean)[i]== "PAR") {
    years<- 1998:2020
    var<-   var[5:27]
    mid<-   2009}
  pval<- round(min(mk.test(var, alternative = "greater")$p.value,
                   mk.test(var, alternative = "less")$p.value)  ,4)
  result<- ""
  if(pval <  0.05) {result=   " *"}
  if(pval <  0.01) {result=  " **"}
  if(pval < 0.001) {result= " ***"}
  #all
  plot(years, var,
       col= "chartreuse3", lwd=2, pch= 16, type= "b", las=1, cex.axis= 0.8,
       ylim= c(min(var), max(var)+0.11*diff(range(var))),
       xlab= "year", ylab= colnames(stmean)[i], 
       main= paste0(colnames(stmean)[i], " (", jtext[j], ")"))
  text(mid,max(var)+0.05*diff(range(var)),
       paste0("p-value MK-test: ", pval, result))
 }
}



#scaling MK-test--------
library(HKprocess)
library(trend)

mk.test(stmean$tp)
MannKendallLTP(stmean$tp)


for(i in c(2:22,24:33)){
  var<- stmean[,i]
  if(colnames(stmean)[i]== "PAR") {var<- var[5:27]}
  print(paste(colnames(stmean[i]), 
        "MK-test:", round(mk.test(var)$p.value, 4), "       ",
        "MK-scaled:", round(MannKendallLTP(var)$Mann_Kendall_LTP[2],4)))
}





#.-----------
#phyto----------

phy<- read.csv("phyto_int.csv", sep= ";")
phy$time<- as.POSIXct(phy$time, tz= "UTC")
#only jan-may
phy<- subset(phy, month(phy$time) %in% 1:5)
phy$Alle_Spezies<- rowSums(phy[,2:8])

phymean<- aggregate(phy[,c(2:8, 17:24)], list(phy$year), mean)
colnames(phymean)[1]<- "year"

phy01<- subset(phy, month(phy$time)== 1)
phy02<- subset(phy, month(phy$time)== 2)
phy03<- subset(phy, month(phy$time)== 3)
phy04<- subset(phy, month(phy$time)== 4)
phy05<- subset(phy, month(phy$time)== 5)

mean01p<- aggregate(phy01[,c(2:8, 17:24)], list(year(phy01$time)), mean, na.rm=T)
mean02p<- aggregate(phy02[,c(2:8, 17:24)], list(year(phy02$time)), mean, na.rm=T)
mean03p<- aggregate(phy03[,c(2:8, 17:24)], list(year(phy03$time)), mean, na.rm=T)
mean04p<- aggregate(phy04[,c(2:8, 17:24)], list(year(phy04$time)), mean, na.rm=T)
mean05p<- aggregate(phy05[,c(2:8, 17:24)], list(year(phy05$time)), mean, na.rm=T)



#plots monthly averages phyto------
phyall<- list(phymean, mean01p, mean02p, mean03p, mean04p, mean05p)
par(mfrow= c(2,3))
for(i in  c(2:16)){
  for(j in 1:6){
    jtext<- c("Jan-May", "Jan", "Feb", "Mar", "Apr", "May")
    phymean<- phyall[[j]]
    years<- 1994:2020
    var<- phymean[,i]
    mid<- 2007
    pval<- round(min(mk.test(var, alternative = "greater")$p.value,
                     mk.test(var, alternative = "less")$p.value)  ,4)
    result<- ""
    if(pval <  0.05) {result=   " *"}
    if(pval <  0.01) {result=  " **"}
    if(pval < 0.001) {result= " ***"}

    plot(years, var,
         col= "chartreuse3", lwd=2, pch= 16, type= "b", las=1, cex.axis= 0.8,
         ylim= c(min(var), max(var)+0.11*diff(range(var))),
         xlab= "year", ylab= colnames(phymean)[i], 
         main= paste0(colnames(phymean)[i], " (", jtext[j], ")"))
    text(mid,max(var)+0.05*diff(range(var)),
         paste0("p-value MK-tephy: ", pval, result))
  }
}
#results
#bacil:  increase in march (p-value 0.0333), decrease in May (p-value 0.0003)
#chloro: decrease in march (p-val 0.017) and april (p-val 0.03)
#cyano:  increase for ALL MONTHS overall p-val 0.0003
#dino:   increase in april and may (p-val < 0.0001 for both)

#bacil-rel:  decrease in may (p-val ***)
#chloro-rel: decrease in Feb-May (p-val ** for all)
#crypto-rel: decrease in Jan-Mar (p-val < 0.025 for all)
#cyano-rel:  increase for all months, Jan and May (***)
#dino-rel:   increase in april and may (p-val < 0.011 for both)





#bacil season means per year
plot(years, stwinter$bacil_int, type= "b", pch= 19, lwd= 2, col= "cyan3")
lines(years, stspring$bacil_int, type= "b", pch= 19, lwd= 2, col= "chartreuse3")
lines(years, stsummer$bacil_int, type= "b", pch= 19, lwd= 2, col= "gold")
lines(years, stautumn$bacil_int, type= "b", pch= 19, lwd= 2, col= "tomato1")



plot(years, stwinter$bacil_int, type= "b", pch= 19, lwd= 2, col= "cyan3",
     ylim= c(0, 4600), las= 1)
lines(years, stsummer$all, type= "b", col= "red2", lwd=2, pch=19)


#ts decompose-----------

library(forecast)

st12<- read.csv("ba_dataset.csv", sep= ";")
st12$time<- as.POSIXct(st12$time, format= "%Y-%m-%d", tz= "UTC")

tsp = ts(st12$bacil_int, frequency = 365)
decom<- decompose(tsp, "additive")

plot(st12$time, st12$bacil_int - decom$trend, type= "l", col= "royalblue")






#correl Biomassen ~ physchem ------

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
round(cor(stwinter[,c(3,11,18,10)], stwinter[,19:31]),4)







