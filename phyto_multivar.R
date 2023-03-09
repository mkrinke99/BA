
library(lubridate)
library(MASS)
library(olsrr)
library(tictoc)


phy<- read.csv("bacil_inter.csv", sep= ";")
phy$date<- as.POSIXct(phy$date, format= "%d.%m.%Y", tz= "UTC")

phy$day_nr<- yday(phy$date)




#maximum biomass per year
phymax<- aggregate(phy$spline_interpol_corrected, list(phy$year), max)
colnames(phymax)<- c("year", "max")

#day where maximum occurs for every year
ymax_day<- vector()
for(yy in 1994:2020){
  ymax_day[yy-1993]<- phy$day_nr[phy$spline_interpol_corrected== 
                                   max(phy$spline_interpol_corrected[phy$year== yy])]
}
phymax$max_day<- ymax_day

#first day biomass of 300 
over300<- vector()
for(yy in 1994:2019){
over300[yy-1993]<- yday(min(phy[phy$spline_interpol_corrected > 300 &
                             phy$year== yy,]$date))
}
phymax$over300<- c(over300, NA)

#first day any biomass
over_x<- function(x){
  over_x<- rep(NA, 27)
  for(yy in 1994:2019){
    if(max(phy$spline_interpol_corrected[phy$year== yy]) > x){
      over_x[yy-1993]<- yday(min(phy[phy$spline_interpol_corrected> x&
                                        phy$year== yy,]$date))
    }
  }
phymax$over_x<- over_x
plot(1994:2020, phymax$over_x, ylim= c(0,200), ylab= "day",
     type= "b", pch= 16, las= 1, main= paste0("first day of biomass over ", x))
}

#plot nach Dekaden--------
#colors
ycols<- c("red3", "yellow3", "dodgerblue3")
phy$ycol<-                           ycols[1]
phy$ycol[phy$year %in% 2000:2009]<-  ycols[2]
phy$ycol[phy$year %in% 2010:2020]<-  ycols[3]
#plot
plot(phy$day_nr, phy$bacillariophyta, 
     xlab= "day", ylab= "biomass", main= "all samplings",
     pch= 16, cex= 0.8, las= 1, col= phy$ycol)
legend(300, 10000, legend= c("1994-1999", "2000-2009", "2010-2020"),
       box.lty= 0, fill= ycols, cex= 0.8)


phy$Tag<- yday(phy$date)
phy$Dekade<- "1994-1999"
phy[phy$year %in% 2000:2009,]$Dekade<- "2000-2009"
phy[phy$year %in% 2010:2020,]$Dekade<- "2010-2020"




ggplot(phy, aes(x = Tag, y = bacillariophyta, col = Dekade)) +
  geom_point(pch = 16, cex = 3.3) +
  labs(x = "Tag", y = "Biomasse", title = "Stichproben nach Tag der Messung") +
  theme_bw() + 
  scale_color_manual(values=c("#00665E", "#FF8000", "#00A3E0")) #+
 # scale_y_continuous(trans='log10')








#plot day of maximum and maximum
plot(phymax$max_day, phymax$max, xlim= c(0,366), ylim= c(0,13000), pch=16,
     col= c(rep("red", 9), rep("orange", 9), rep("dodgerblue2", 9)), las=1,
     cex.axis= 0.8,
     xlab= "day of maximum biomass", ylab= "biomass")
text(phymax$max_day +8, phymax$max *1.05, phymax$year, cex= 0.5)
for (i in 1:26){
        segments(phymax$max_day[i], phymax$max[i],
         phymax$max_day[i+1], phymax$max[i+1], col= "grey73")}


#biomass maximum position
plot(1994:2020, phymax$max_day, ylim= c(0,220), 
     type= "b", pch= 16, las= 1, main= "Day of biomass maximum")
#maximum biomass
plot(1994:2020, phymax$max, ylim= c(0,12000), 
     type= "b", pch= 16, las= 1, main= "Biomass maximum")
#erster Tag mit Biomass über 300 
plot(1994:2020, phymax$over300, ylim= c(0,200), ylab= "day",
     type= "b", pch= 16, las= 1, main= "first day of biomass over 300")





#for tp peak
st<- read.csv("st_vertmean_samplings.csv", sep= ";")
st$time<-  as.POSIXct(st$time, format= "%d.%m.%Y", tz= "UTC")
st$year<-   year(st$time)
st$day_nr<- yday(st$time)
st<- st[st$year>1993,]

tpmax<- aggregate(st$tp, list(st$year), max, na.rm=T)
colnames(tpmax)<- c("year", "max")

tpmax_day<- vector()
for(yy in 1994:2020){
  tpmax_day[yy-1993]<- st$day_nr[st$tp==  max(st$tp[st$year== yy], na.rm=T)]
}
tpmax$max_day<- tpmax_day

library(dplyr)
st_tpmax1<- st %>% group_by(year) %>% top_n(1, tp)  
st_tpmax<-st_tpmax1$day_nr


for(yy in 1994:2020){
plot(st$time[st$year== yy], st$tp[st$year== yy], lwd=3, pch= 16,
     ylim= c(0, 1.1*max( st$tp[st$year== yy], na.rm=T)))
  abline( v= st$time[st$year== yy & 
                       st$tp== max( st$tp[st$year== yy], na.rm=T)]
         )
}

st_o2<- aggregate(st$o2, list(st$year), mean, na.rm= T)
plot(st_o2$Group.1, st_o2$x, type="b", lwd= 2, ylim= c( 7, 13))



#Multivar Regression--------

#get dataset with phytoplankton and physchem vars
phy$time<- phy$date
#stechlin data interpolated
st_full<- read.csv("st_vertmean.csv", sep= ";")
st_full$time<- as.POSIXct(st_full$time, format= "%d.%m.%Y", tz= "UTC")

#merge phytoplankton and physchem data
ste<- merge(phy[,c("time", "spline_interpol_corrected")],
            st_full, by= "time")
#add year column
ste$year<- year(ste$time)

#yearly means of all vars
ste_y<- aggregate(ste[,-c(1, 15)], list(ste$year), mean, na.rm=T)
#remove 2020 -> unplausible values (mistake?)
ste_y<- subset(ste_y, ste_y$Group.1 < 2020)

#add column for log(biomass) -> reduce impact of 2017-2019
ste_y$biomass_log<- ste_y$spline_interpol_corrected

#yearly mean, log(biomass)
for(i in 1:12){
  plot(ste_y$biomass_log, ste_y[,(i+2)],
       xlab= "log(biomass)", ylab= colnames(ste_y)[i+2], las= 1, 
       main= paste("Cor", "biomass", "~", colnames(ste_y)[i+2], ":",
                   round(cor(ste_y$biomass_log, ste_y[,(i+2)]), 3)),
       pch= 16, col= "chartreuse4")
  abline(lm(ste_y[,(i+2)] ~ ste_y$biomass_log))
}


#regression model
fit<- lm(formula = biomass_log ~ wt + o2+ ph + srp + tn + no2 + no3 + nh4 + si + PAR + hmix,
              data = ste_y)

step <- stepAIC(fit, direction="both")
step$anova # display results
#choose optimal number of vars
tic()
k <- ols_step_best_subset(fit)
plot(k)
toc()
#~70 sec

#4 vars with highest R^2_adj 0.8216 (with 2020: 4 vars and 0.317 pH, tp, si, hmix) 
fit8<- lm(formula= biomass_log ~ ph + srp + no2 + no3 + nh4 + si + PAR + hmix, data= ste_y)
summary(fit8)
#plot hist of residuals -> check if distribution is approximately normal
hist(residuals(fit8), col = "steelblue")    #approx. normalverteilt


par(mfrow= c(2,4))
for(i in 1:8){
  ste_y_8vars<- ste_y[,c("biomass_log", "srp", "si", "PAR", "hmix", "no3", "nh4", "no2", "ph")]
  plot(ste_y_8vars$biomass_log, ste_y_8vars[,(i+1)],
       xlab= "log(biomass)", ylab= colnames(ste_y_8vars)[i+1], las= 1, 
       main= paste("Cor", "biomass", "~", colnames(ste_y_8vars)[i+1], ":",
                   round(cor(ste_y_8vars$biomass_log, ste_y_8vars[,(i+1)]), 3)),
       pch= 16, col= "chartreuse4")
  abline(lm(ste_y_8vars[,(i+1)] ~ ste_y_8vars$biomass_log))
}



#seasons-------------

#subsets for seasons

ste$date<- format(ste$time, format= "%m-%d")

ste_winter1<- subset(ste, ste$date <  format("03-21" , format= "%m-%d") |
                         ste$date >= format("12-21" , format= "%m-%d"))
#add 1 year to december dates to aggregate yearly means
ste_winter1$year[ste_winter1$date >= format("12-21" , format= "%m-%d")]<- 
         ste_winter1$year[ste_winter1$date >= format("12-21" , format= "%m-%d")]+1
ste_winter1<- subset(ste_winter1, ste_winter1$year <2020)

ste_spring1<- subset(ste, ste$date >= format("03-21", format= "%m-%d") &
                         ste$date <  format("06-21", format= "%m-%d") & ste$year<2020)
ste_summer1<- subset(ste, ste$date >= format("06-21", format= "%m-%d") &
                         ste$date <  format("09-21", format= "%m-%d")& ste$year<2020)
ste_autumn1<- subset(ste, ste$date >= format("09-21", format= "%m-%d") &
                         ste$date <  format("12-21", format= "%m-%d")& ste$year<2020)


ste_spring<- aggregate(ste_spring1[,-c(1, 15, 16)], list(ste_spring1$year), mean, na.rm=T)
ste_summer<- aggregate(ste_summer1[,-c(1, 15, 16)], list(ste_summer1$year), mean, na.rm=T)
ste_autumn<- aggregate(ste_autumn1[,-c(1, 15, 16)], list(ste_autumn1$year), mean, na.rm=T)
ste_winter<- aggregate(ste_winter1[,-c(1, 15, 16)], list(ste_winter1$year), mean, na.rm=T)

#plot yearly means for each season
for(i in 2:14){
  plot(NA,NA, xlim= c(1994,2019), las= 1, cex.axis= 0.8,
       ylim= c(0.8*min(c(ste_spring[,i],ste_summer[,i],ste_autumn[,i],ste_winter[,i])),
               1.1*max(c(ste_spring[,i],ste_summer[,i],ste_autumn[,i],ste_winter[,i]))),
       xlab="year", ylab= colnames(ste_spring[i]))
  lines(ste_spring[,1], ste_spring[,i], lwd= 2, type= "b", pch=16, col= "chartreuse3")
  lines(ste_summer[,1], ste_summer[,i], lwd= 2, type= "b", pch=16, col= "gold2")
  lines(ste_autumn[,1], ste_autumn[,i], lwd= 2, type= "b", pch=16, col= "tomato2")
  lines(ste_winter[,1], ste_winter[,i], lwd= 4, type= "b", pch=16, col= "dodgerblue3")
}


#multiple linear regression for winter--------

#log(biomass) column
ste_winter$biomass_log<- log(ste_winter$spline_interpol_corrected)

fit_w<- lm(formula = biomass_log ~ wt + o2+ ph + srp + tn + no2 + no3 + nh4 + si + PAR + hmix,
           data = ste_winter)

step <- stepAIC(fit_w, direction="both")
step$anova # display results

#choose optimal number of vars
tic()
k <- ols_step_best_subset(fit)
plot(k)
toc()
#~70 sec

#4 vars with highest R^2_adj 0.8216 (with 2020: 4 vars and 0.317 pH, tp, si, hmix) 
fit7_w<- lm(formula= biomass_log ~ ph + srp + no2 + no3 + si + PAR + hmix, data= ste_y)
summary(fit7_w) #R^2_adj= 0.7941

#plot hist of residuals -> check if distribution is approximately normal
hist(residuals(fit7_w), col = "steelblue")    #approx. normalverteilt


par(mfrow= c(2,4))
for(i in 1:7){
  ste_w_7vars<- ste_winter[,c("biomass_log", "srp", "si", "PAR", "hmix", "no3", "no2", "ph")]
  plot(ste_w_7vars$biomass_log, ste_w_7vars[,(i+1)],
       xlab= "log(biomass)", ylab= colnames(ste_w_7vars)[i+1], las= 1, 
       main= paste("Cor", "biomass", "~", colnames(ste_w_7vars)[i+1], ":",
                   round(cor(ste_w_7vars$biomass_log, ste_w_7vars[,(i+1)]), 3)),
       pch= 16, col= "chartreuse4")
  abline(lm(ste_w_7vars[,(i+1)] ~ ste_w_7vars$biomass_log))
}


#.-------
#verschiebe jährliche plots: x-Richtung------

samplings<- subset(phy, !is.na(phy$bacillariophyta))

#only first 140 days -> local maximums
samplings<- subset(samplings, samplings$year <2020)

plot(NA,NA, xlim= c(0, 365), ylim=c(0, 13000))
for(yy in 1994:2019){
  lines(samplings[samplings$year== yy,]$day_nr, 
        samplings[samplings$year== yy,]$bacillariophyta, type= "b", cex= 0.6, pch= 16)
}

samplings_max<- aggregate(samplings$bacillariophyta[samplings$day_nr <=140],
                          list(samplings$year[samplings$day_nr <=140]), max, na.rm= T)
colnames(samplings_max)<- c("year", "bacillariophyta")

samplings_max<- left_join(samplings_max, samplings[,c(2,6,7)], by= c("year", "bacillariophyta"))

#mean day where maximum is located
mean(samplings_max$day_nr)  #91.73   (100.89 with global maximums)


#abweichung vom durchschnittlichen maximum
samplings_max$shift<- samplings_max$day_nr - 91.73

samplings$bp_shifted<- samplings$day_nr - 
                       rep(samplings_max$shift, as.vector(table(samplings$year)))


#vectors for col and pch
samplings$colvec<- rep(c(rep("red",8), rep("blue", 9), rep("chartreuse4",9)), 
                       as.vector(table(samplings$year)))
samplings$pchvec<- rep(c(rep(16, 8), rep(22, 9), rep(4, 9)), as.vector(table(samplings$year)))



for(i in 1994:2020){
plot(NA,NA, xlim= c(-40, 365), ylim=c(0, 7000),
     las= 1, cex.axis= 0.7,
     xlab= "day", ylab= "biomass", main= "biomass shifted")
for(yy in 1994:i){
  samplings_y<- subset(samplings, samplings$year== yy)
  lines(samplings_y$bp_shifted, 
        samplings_y$bacillariophyta,
        type= "b", cex= 0.6, lwd= 2,
        pch= samplings_y$pchvec, col= samplings_y$colvec)
  }
}


#verschiebe jährl. plots in y-Richtung









