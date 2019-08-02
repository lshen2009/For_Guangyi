#part1: grid box average temperature
rm(list=ls())
library(fields)
library(maps);library(ncdf4)
setwd("~/Summer_tutor/For_Guangyi/20190730/Guangyi_code")
#---- load functions ----
source("Function.R")
#-------parameters-------
m1=6;m2=8
yr1=1990;yr2=2016
#------------------------
ss=read_NCEP("air.mon.mean.nc",xlim=c(230,300),ylim=c(20,50))
met_data=ss$data
met_lon=ss$lon-360
met_lat=ss$lat
met_date=ss$date
met_data=cal.season_mean(met_data,met_date,m1,m2,yr1,yr2)

#Repeat all sites
setwd("~/Summer_tutor/For_Guangyi")
ozone_data=read.csv("CASTNET_1990-2016.csv",header=TRUE)
ozone_data[ozone_data<=-999]=NA
o_lon=tapply(ozone_data[,"lon"],ozone_data[,"ID"],mean,na.rm=T)
o_lat=tapply(ozone_data[,"lat"],ozone_data[,"ID"],mean,na.rm=T)

sites_correlation = function(ID){  
  ind1=which.min(abs(lon-o_lon[ID]))
  ind2=which.min(abs(lat-o_lat[ID]))    
  monthly_mean_site=data[ind1,ind2,]  
  ind3=(ozone_data[,"ID"]==ID)
  y= cal.season_mean(ozone_data[ind3,"mean"], ozone_data[ind3,c("year","month")],m1,m2,yr1,yr2)
  correlation=cor.test((monthly_mean_site), linear.detrend(y))$estimate  
  return(correlation)
}

all_sites_correlation=array(NA,52)
for(ID in 1:52){
  all_sites_correlation[ID]=sites_correlation(ID)
}


dev.new(width=5,height=2.8)
plot.site(all_sites_correlation,cbind(o_lat,o_lon),xlim=c(-130,-60),ylim=c(25,50),type="def",zlim=c(-0.6,0.6))