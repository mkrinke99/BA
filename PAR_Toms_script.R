# example of how to calculate PAR

rm(list=ls())
library(lubridate)
library(tidyr)
library(dplyr)
library(data.table)

# read data
datafolder <- "../data/stechlin/"

par <- read.table(paste0(datafolder,"PAR_Stechlin.csv"), 
                  header=FALSE,skip=3,sep=";",dec=".")
names(par) <- c("date","sensor","depth","PAR","device","comment")
par$date <- as.POSIXct(par$date,tz="UTC")

rad <- read.csv(paste0(datafolder,"meteo1_st.csv"))
rad$time <- as.POSIXct(rad$time,tz="UTC")

# Lambert-Beer law is Iz = I0 * exp(-Kd * z) 
# Iz is light at depth z, I0 is light at the surface, Kd is extinction coefficient.
# Rearrangig gives Kd = log(Iz/I0)/z
# Alternaively if you have pairs of simultaneous measurements of light at 2 depths, z1 and z2, 
# then Iz2 = Iz1 * exp(-Kd * (z2 - z1))
# Rerranging gives Kd = log(Iz1/Iz2)/(z2-z1)
# There will be periods where you don't have measurements of underwater PAR for extinction coefficient.
# Then you can estimate the extinction coefficient from the Secchi depth as:
# Kd = 2 / Secchi

# unfortunately the PAR dataset is a bit "broken" because the measurements were actually taken in pairs,
# but it is no longer possible to reconstruct the pairs reliably because they were grouped by day.
# Most likely the 1st pair was 0 & 0.5m, the second pair was 1 & 1.5 m , 3rd pair was 2 & 2.5 m etc.
# We can work around this by doing a regression of the pooled values, focus only on say the top 10m

# calculate extinction
Kd <- NULL
date <- unique(par$date)
for(i in date) {
  dat <- par[par$date==i & par$depth<=10 ,c("PAR","depth")]
  if(max(par$depth)>2 & sum(!is.na(dat$PAR))>2) {
    r1 <- lm(log(PAR)~depth,data=dat)
    # plot(log(PAR)~depth,dat)
    # abline(r1)
    Kd1 <- -coef(r1)[2]
  } else {
    Kd1 <- 2 / par$secchi
  }
  Kd <- c(Kd, Kd1)
  
}

ext <- data.frame(date,Kd)

# radiation
# calculate daily means
# original units??? guessing maybe hourly sums of J/m2?
# convert to W/m2
# remember 1 W = 1 Joule /second
# This is my first go at tidyverse ;-)

rad <- mutate(rad,rad=solar_rad_e5 / 3600)
# plot(rad~time,rad)
rad.d <- group_by(rad, date=floor_date(time, "day")) %>%
  summarise(rad=mean(rad))

# let's calculate daily underwater light from 1997 to 2020
# linearly interpolate Kd to daily
datenew <- seq(ISOdate(1997,1,1,0,tz="UTC"), ISOdate(2020,12,31,0,tz="UTC"), "day")
mydata <- data.frame(date=datenew, 
                     Kd=approx(x = ext$date, y = ext$Kd, xout = datenew, rule=2)$y)
mydata <- merge(mydata,rad.d)

# We need to know the mixed layer depth to calculate the mean light
# During mixing, the mixed layer depth is just the mean lake depth
# During stratification not. Let's assume that the mixed layer depth
# is 23 m, you can add a time series later

mydata$zmix <- 23

# Now convert radiation to PAR
# assuming 50% of radiationi is PAR, and 10% is reflected at the surface, 
# and the conversion is 1 Joule = 4.56 umol photons, then convert to mol/m2/day

mydata$I0 <- mydata$rad * 0.5 * 0.9 * 4.56 * # now umol photons /m2/s
  60*60*24 / 1000000 # now mol photons/m2/day

# Now calculate mean underwater light in mixed layer
# Integrating Lambert-Beer law over depth gives
# Imean = I0/(Kd * zmix) * (1 - exp(-Kd * zmix))

mydata <- mutate(mydata, PAR = I0 / (Kd * zmix) * (1 - exp(-Kd * zmix)))


plot(PAR~date,mydata,type="l")
abline(h=1.3)

plot(ext,type="o")

# check if you have the right ERA5 radiation data - it doesnt look cloudy enough to me?
