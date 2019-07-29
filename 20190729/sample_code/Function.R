library(ncdf4)

read_NCEP=function(name,start_month=6,end_month=8,start_yr=1990,end_yr=2016,pressure=NULL,xlim=c(210,290),ylim=c(10,70)){
#name="air.mon.mean.nc"
#xlim is the range of longitude
#ylim is the range of latitude	
datadir='/Users/lulushen/Summer_tutor/For_Guangyi/20190729/'
datafile = nc_open(paste(datadir,name,sep=''))
lon = ncvar_get(datafile, varid='lon')#read longitude
lat = ncvar_get(datafile, varid='lat')#read latitude
hours = ncvar_get(datafile, varid='time')#read hours
days=hours/24#convert to days
dtm <- as.Date("1800-01-01")#
str_time=dtm + days
date=cbind(as.numeric(format(str_time,"%Y")),as.numeric(format(str_time,"%m")))

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

#=============================================
ss=read_NCEP("air.mon.mean.nc")
names(ss)
data=ss$data
lon=ss$lon
lat=ss$lat
date=ss$date
