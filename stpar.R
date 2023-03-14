
library(lubridate)

st<- read.csv("ba_dataset.csv", sep= ";")
st$time<- as.POSIXct(st$time, format= "%Y-%m-%d", tz= "UTC")



stpar<- st[,c("time", "PAR")]

stpar$PAR2<- stpar$PAR*86400/1000000


aggregate(stpar$PAR2, list(year(stpar$time)), mean)







