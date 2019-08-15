rm(list=ls())
library(fields)
library(maps);library(ncdf4)
source("~/Desktop/Function.R")


#===========================================Temperature in Chengdu========================================
#=========================================================================================================
#=========================================================================================================
require(neuralnet)
setwd("~/Summer_tutor/For_Guangyi/20190809/Data")
files=Sys.glob("*.csv")

#===== read data in all years ======
#the year 2004 is missing
result=NULL
for(ifile in files){     
  met=read.csv(ifile)
  data=met[,c("DATE","TEMP","DEWP","SLP","WDSP","MAX","MIN","PRCP")]
  print(dim(data))
  result=rbind(result, data)
}

#===== now deal with missing data ======
strdate=as.Date(result[,1])
alldate=seq(as.Date("1999-01-01"),as.Date("2018-12-31"),by="day")
time.ind=match(strdate,alldate)

final_data=array(NA,c(length(alldate),dim(result)[2]))
dim(final_data)
final_data[time.ind,]=data.matrix(result)
final_data[,1]=alldate
final_data[final_data>=999.9]=NA

date=cbind(as.numeric(format(alldate,"%Y")),as.numeric(format(alldate,"%m")),as.numeric(format(alldate,"%d")))
colnames(date)=c("Year","Month","Day")
final=cbind(date,final_data[,2])
colnames(final)=c("Year","Month","Day","TEMP")
CD_T = (final[date[,3]+1,"TEMP"])

#=========================================Temperature in all grid boxes===================================
#=========================================================================================================
#=========================================================================================================
read_NCEP2=function(name,start_month=6,end_month=8,start_yr=1990,end_yr=2016,pressure=NULL,xlim=c(210,290),ylim=c(10,70)){
  #name="air.mon.mean.nc"
  #xlim is the range of longitude
  #ylim is the range of latitude	
  datadir='~/Desktop/'
  datafile = nc_open(paste(datadir,name,sep=''))
  lon = ncvar_get(datafile, varid='lon')#read longitude
  lat = ncvar_get(datafile, varid='lat')#read latitude
  hours = ncvar_get(datafile, varid='time')#read hours
  days=hours/24#convert to days
  dtm <- as.Date("1800-01-01")#
  str_time=dtm + days
  date=cbind(as.numeric(format(str_time,"%Y")),as.numeric(format(str_time,"%m")),as.numeric(format(str_time,"%d")))
  
  ind1=which(lon>=xlim[1] & lon<=xlim[2])
  ind2=which(lat>=ylim[1] & lat<=ylim[2])
  ind4=which(date[,1]>= start_yr & date[,1]<= end_yr)
  lon=lon[ind1]
  lat=lat[ind2]
  date=date[ind4,]
  if(!is.null(pressure)) {
    lev= ncvar_get(datafile, varid='level')	
    ind3=which(lev==pressure)
  }
  
  if(datafile$ndims==3){
    print("surface")		
    origin_data = ncvar_get(datafile,start=c(ind1[1],ind2[1],ind4[1]), count=c(length(ind1),length(ind2),length(ind4)))
  }
  if(datafile$ndims==4){
    print("presure")	
    origin_data = ncvar_get(datafile,start=c(ind1[1],ind2[1],ind3[1],ind4[1]), count=c(length(ind1),length(ind2),1,length(ind4)))
  }
  origin =origin_data[,length(lat):1,]
  lat=rev(lat)
  nc_close(datafile)
  
  ind=(date[,1]>=start_yr & date[,1]<=end_yr & date[,2]>=start_month & date[,2]<=end_month)
  output_data=origin[,,ind]
  
  output_date=date[ind,]
  
  return(list('data'= output_data,'lon'=lon,'lat'=lat,'date'= output_date))
}

ss = read_NCEP2("T.daily.1999-2018.nc",start_month=1,end_month=12,start_yr=1999,end_yr=2018,xlim=c(70,180),ylim=c(0,55))
met_data=ss$data-273.5
met_lon=ss$lon
met_lat=ss$lat
met_date=ss$date

correlation = cal.correlation(met_data,CD_T)
plot.field(correlation,met_lon,met_lat,Pacific.centric=F)


