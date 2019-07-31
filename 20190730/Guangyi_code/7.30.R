#part1: grid box average temperature
rm(list=ls())
library(fields)
library(maps);library(ncdf4)

setwd("/Users/lulushen/Summer_tutor/For_Guangyi/20190730/Guangyi_code")
#---- load functions ----
source("Function.R")
#------------------------
ss=read_NCEP("air.mon.mean.nc",xlim=c(230,300),ylim=c(25,50))
names(ss)
data=ss$data
lon=ss$lon-360
lat=ss$lat
date=ss$date
# data=aperm(apply(data,c(1,2), cal.season.mean),c(2,3,1))
# data=aperm(apply(data,c(1,2), mov.detrend),c(2,3,1))

#Repeat all sites
setwd("/Users/lulushen/Summer_tutor/For_Guangyi")
ozone_data=read.csv("CASTNET_1990-2016.csv",header=TRUE)
ozone_data[ozone_data<=-999]=NA
o_lon=tapply(ozone_data[,"lon"],ozone_data[,"ID"],mean,na.rm=T)
o_lat=tapply(ozone_data[,"lat"],ozone_data[,"ID"],mean,na.rm=T)

sites_correlation = function(ID){  
  ind1=which.min(abs(lon-o_lon[ID]))
  ind2=which.min(abs(lat-o_lat[ID]))  
  monthly_mean_site= cal.season.mean(data[ind1,ind2,])
  
  ind3=(ozone_data[,"ID"]==ID)
  y= cal.season.mean(ozone_data[ind3,"mean"])
  # anormaly= linear.detrend(y)
  correlation=cor.test(linear.detrend(monthly_mean_site), linear.detrend (y))$estimate
  
  return(correlation)
}

all_sites_correlation=array(NA,52)
for(ID in 1:52){
  all_sites_correlation[ID]=sites_correlation(ID)
}
all_sites_correlation
# mean(all_sites_correlation)

plot.site(all_sites_correlation,cbind(o_lat,o_lon),xlim=c(-130,-60),ylim=c(25,50),type="def",zlim=c(-0.6,0.6))