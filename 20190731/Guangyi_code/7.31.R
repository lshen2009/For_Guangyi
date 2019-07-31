rm(list=ls())
library(fields)
library(maps)
library(ncdf4)
read_NCEP=function(name,start_month=6,end_month=8,start_yr=1990,end_yr=2016,pressure=NULL,xlim=c(210,290),ylim=c(10,70)){
  #name="air.mon.mean.nc"
  #xlim is the range of longitude
  #ylim is the range of latitude	
  datadir='~/Summer_tutor/For_Guangyi/20190729/'
  datafile = nc_open(paste(datadir,name,sep=''))
  
  lon = ncvar_get(datafile, varid='lon') #read the longitude
  lat = ncvar_get(datafile, varid='lat') #read the latitude
  hours = ncvar_get(datafile, varid='time') #read the time
  
  days=hours/24#convert to days
  dtm <- as.Date("1800-01-01")#
  str_time=dtm + days
  date=cbind(as.numeric(format(str_time,"%Y")),as.numeric(format(str_time,"%m")))
  
  ind1=which(lon>=xlim[1] & lon<=xlim[2])
  ind2=which(lat>=ylim[1] & lat<=ylim[2])
  ind3=which(date[,1]>=start_yr & date[,1]<=end_yr)
  lon=lon[ind1]
  lat=lat[ind2]
  date=date[ind3,]
  
  #read the air temperature data
  output_data = ncvar_get(datafile,start=c(ind1[1],ind2[1],ind3[1]), count=c(length(ind1),length(ind2),length(ind3)), varid="air")
  
  #lat is in a decreasing order, now reorder it
  output_data=output_data[,length(lat):1,]#revert along the second order
  lat=rev(lat)
  
  ind4=which(date[,1]>=start_yr & date[,1]<=end_yr & date[,2]>=start_month & date[,2]<=end_month)
  output_data=output_data[,,ind4]
  date=date[ind4,]
  
  return(list('data'= output_data,'lon'=lon,'lat'=lat,'date'= date))
}

aa=read_NCEP("air.mon.mean.nc")
names(aa)
data=aa$data
data[1,1,]
lat=aa$lat
lon=aa$lon-360
date=aa$date

cal.season_mean=function(data, date, start_month=6, end_month=8, start_yr=1990, end_yr=2016){
  result=array(NA, c(dim(data)[1], dim(data)[2], (end_yr-start_yr+1)))
  for(year in start_yr: end_yr){
    ind=(date[,1]==year & date[,2]>=start_month & date[,2]<=end_month)
    result[,,year-start_yr+1]=apply(data[,,ind],c(1,2),mean,na.rm=TRUE)
  }
  return(result)
}
newdata=cal.season_mean(data,date)


#------ozone data--------
setwd("~/Summer_tutor/For_Guangyi")
ozone_data=read.csv("CASTNET_1990-2016.csv",header=TRUE)
ozone_data[ozone_data<=-999]=NA
ozone_season_mean=function(ozone_data,ID, start_yr=1990, end_yr=2016){
  mean_value=array(NA,end_yr-start_yr+1)
  for(year in start_yr:end_yr){
    ind = (ozone_data[,"year"]==year & ozone_data[,"ID"]==ID)
    newdata=ozone_data[ind,]
    mean_value[year-start_yr+1]=tapply(newdata[,"mean"],newdata[,"ID"],mean,na.rm=TRUE)
  }
  return(mean_value)
}
ozone_season_mean(ozone_data,1)

ozone_season_mean=function(ozone_data,ID, start_yr=1990, end_yr=2016){
  ind=(ozone_data[,'ID']==ID & ozone_data[,'year']>=start_yr & ozone_data[,'year']<=end_yr)
  spdata=ozone_data[ind,]
  y=tapply(spdata[,'mean'], spdata[,'year'],mean,na.rm=T)
  return(y)
}


#------the corresponding temperature
ID=1
o_lon=tapply(ozone_data[,"lon"],ozone_data[,"ID"],mean,na.rm=T)
o_lat=tapply(ozone_data[,"lat"],ozone_data[,"ID"],mean,na.rm=T)

ind1=which.min(abs(lon-o_lon[ID]))
ind2=which.min(abs(lat-o_lat[ID]))

monthly_mean_site=newdata[ind1,ind2,]

#------
y = ozone_season_mean(ozone_data,ID)
x=1:length(y)
fit<-lm(y~x)

missing.ind=is.na(y)
y.res=array(NA,length(y))

y.res[!missing.ind]=resid(fit)
anormaly=y.res

fit2<-lm(monthly_mean_site ~ anormaly)
correlation = cor.test(monthly_mean_site,anormaly)$estimate



#-------------all sites correlation
sites_correlation = function(ID){
  o_lon=tapply(ozone_data[,"lon"],ozone_data[,"ID"],mean,na.rm=T)
  o_lat=tapply(ozone_data[,"lat"],ozone_data[,"ID"],mean,na.rm=T)
  
  ind1=which.min(abs(lon-o_lon[ID]))
  ind2=which.min(abs(lat-o_lat[ID]))
  
  lon[ind1]
  lat[ind2]
  monthly_mean_site=newdata[ind1,ind2,]
  
  y = ozone_season_mean(ozone_data,ID)
  x=1:length(y)
  fit<-lm(y~x)
  
  missing.ind=is.na(y)
  y.res=array(NA,length(y))
  
  y.res[!missing.ind]=resid(fit)
  anormaly=y.res
  
  fit2<-lm(monthly_mean_site ~ anormaly)
  correlation = cor.test(monthly_mean_site,anormaly)$estimate
  return(correlation)
}

all_sites_correlation=array(NA,52)
for(ID in 1:52){
  all_sites_correlation[ID]=sites_correlation(ID)
}
all_sites_correlation

