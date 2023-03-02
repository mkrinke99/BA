


library(lubridate)

st1<- read.csv(file = "st_physchem.csv", header= T, sep= ";")
st1$time<- as.POSIXct(st1$time, format= "%d.%m.%Y", tz= "UTC")


st<- subset(st1, year(st1$time) >= 1994 & month(st1$time) %in% 1:5)


table(!is.na(st$wt))


#print number of observations and number of days per var
par(mfrow= c(2,2))
for(i in c(3:7, 11:17, 20)){
  d<- subset(st, !is.na(st[,i]))
  yvec<- 1994:2020
  if(i== 20){yvec= 1997:2020}
  #samplings per year
  sam<- aggregate(data = d, time ~ year(time),
                  function(time) length(unique(time)))
  mon<-  aggregate(data = d, time ~ month(time),
                   function(time) length(unique(time)))
  
  print(paste0("Var: ", colnames(d)[i],
               "   observations: ", table(!is.na(d[,i]))[1],
               "   days: ", length(unique(d$time)) ))
  #plot 1: year ~ depth
  plot(d$time, d$depth, 
       main= colnames(d)[i], xlab= "time", ylab= "depth", ylim= c(0,70),
       pch= 16, cex= 0.5, col= "seagreen3", las= 1)
  #plot 2: year ~ obs
  plot(yvec, as.data.frame(table(year(d$time), !is.na(d[,i])))[,3],
       main= "observations per year", las= 1, pch= 16, col= "seagreen4", 
       type= "b", xlab= "time", ylab= "total observations")
  #plot 3: year ~ sampling days
  plot(sam[,1], sam[,2],
       main= "samplings per year", las= 1, pch= 16, col= "seagreen4", 
       type= "b", xlab= "time", ylab= "number of samplings",
       ylim= c(0,10.6))
  text(1998, 1, round(mean(sam[,2], na.rm=T),2), cex=0.8)
  #plot 4: month ~ samplings total
  plot(mon[,1], mon[,2],
       main= "samplings per month", las= 1, pch= 16, col= "seagreen4", 
       type= "b", xlab= "month", ylab= "number of samplings",
       ylim= c(0, max(1.05*mon[,2])))
  text(1.2, 22, mon[1,2])
}



#observations per year------
ordervec<- c(11:17, 7,3,4,6, 20)
colvec<- rep("white", 20)
colvec[3:7]<- c("red", "palevioletred2", "white", "red4", "orange")
colvec[11:17]<- c("blue", "dodgerblue2", "dodgerblue4", "navy",
                  "steelblue3", "purple", "cyan2")
colvec[20]<- "seagreen3"

par(mfrow= c(1,2))
#plot 1: jähriche Messungen total
plot(NA,NA,xlim= c(1994,2020), ylim= c(0, 500),
     axes= F, xlab= "Zeit", ylab= "Messungen", main= "Jähriche Messungen")
for(i in ordervec){
  d<- subset(st, !is.na(st[,i]))
  yvec<- 1994:2020
  if(i== 20){yvec= 1997:2020}
  obs<- as.data.frame(table(year(d$time), !is.na(d[,i])))
  lines(yvec, obs[,3],
        lwd= 2, col= colvec[i])
  print(paste0(colnames(d)[i],": ",round(mean(obs[,3]),1)))
}
axis(1, 1994:2020, 1994:2020)
axis(2, las= 1)
legend(1994.5, 470, legend= colnames(st)[ordervec], 
       ncol=2, cex= 0.8, fill= colvec[ordervec], bty= "n")
#plot 2: jährliche Messungen (Tage)

plot(NA,NA,xlim= c(1994,2020), ylim= c(0, 12), axes= F, 
     xlab= "Zeit", ylab= "Messungen", main= "Jähriche Messungen (Tage)")
for(i in ordervec){
  d<- subset(st, !is.na(st[,i]))
  sam<- aggregate(data = d, time ~ year(time),
                  function(time) length(unique(time)))
  lines(sam[,1], sam[,2],
        lwd= 2, col= colvec[i])
  #print(paste0(colnames(d)[i],": ",round(mean(obs[,3]),1)))
}
axis(1, 1994:2020, 1994:2020)
axis(2, 0:10, 0:10, las= 1)
legend(1994.5, 12, legend= colnames(st)[ordervec], 
       ncol=2, cex= 0.8, fill= colvec[ordervec], bty= "n")



#observations per year +gaps--------

#create column of gap between two measurements
obs<- subset(bacil, !is.na(bacil$bacillariophyta))
lag1<- obs$time[-1]
lag2<- obs$time[-nrow(obs)]
obs$gap<- c(0, as.numeric(difftime(lag1, lag2)))
obs1<- obs[,c(6,9)]

#merge with data set
bacil<- merge(bacil, obs1, all.x= T, by= "time")

par(mfrow= c(3,9)) 
#plot years
for(yy in 1994:2020){
  obs_y<- subset(obs, year(obs$time) %in% (yy-1):yy & month(obs$time) %in% 1:6)
  d1<- as.POSIXct(paste0(yy,"-01-01"), tz="UTC")
  d2<- as.POSIXct(paste0(yy,"-02-01"), tz="UTC")
  d3<- as.POSIXct(paste0(yy,"-03-01"), tz="UTC")
  d4<- as.POSIXct(paste0(yy,"-04-01"), tz="UTC")
  d5<- as.POSIXct(paste0(yy,"-05-01"), tz="UTC")
  d6<- as.POSIXct(paste0(yy,"-05-31"), tz="UTC")
  plot(obs_y$time, obs_y$gap,
       main= yy, cex.main= 1.2, 
       xlab= "Zeit", ylab= "Tage zwischen Stichproben",
       lwd= 2.3, col= "dodgerblue3", type= "b", pch= 16, axes= F, cex= 1.2,
       xlim= c(d1,d6),
       ylim= c(0, max(60, 1.1*obs[obs$year== yy,]$gap)))
  axis(1, c(d1,d2,d3,d4,d5,d6), c(month.abb[1:5],""))
  axis(2, seq(0,90,10), seq(0,90,10), las=1)
  text(as.POSIXct(paste0(yy,"-04-13"), tz="UTC"), max(60, 1.1*obs[obs$year== yy,]$gap),
       paste0(length(obs[month(obs$time) %in% 1:5 & obs$year== yy,]$gap),
              " Stichproben"), col= "dodgerblue4", cex= 1.2)
  abline(h= c(0,7,14,21,28), lty= 2, col= "grey76")
}


#observations per year Phytoplankton data
ve<- vector()
for(yy in 1994:2020){
  ve[yy-1993]<- length(obs[month(obs$time) %in% 1:5 &
                             obs$year== yy,]$gap)}
par(mfrow= c(1,1))
plot(1994:2020,ve, type= "l")


#mean days between samplings (Jan-May) 2002-2020
mean(obs$gap[obs$year %in% 2002:2020 & 
               month(obs$time) %in% 1:5])  #20.13

#samlings
table(!is.na(phy$bacillariophyta))
#samplings per year
table(!is.na(phy$bacillariophyta), year(phy$time))

