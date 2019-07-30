library(ncdf4)
library(fields)
library(maps)
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

#read the air temperature data
origin_data = ncvar_get(datafile,start=c(ind1[1],ind2[1],ind4[1]), count=c(length(ind1),length(ind2),length(ind4)))

#lat is in a decreasing order, now reorder it
origin =origin_data[,length(lat):1,]
lat=rev(lat)
nc_close(datafile)#close the datafile

#subset the data
ind=(date[,1]>=start_yr & date[,1]<=end_yr & date[,2]>=start_month & date[,2]<=end_month)
output_data=origin[,,ind]
output_date=date[ind,]

return(list('data'= output_data,'lon'=lon,'lat'=lat,'date'= output_date))
}

plot.field = function(spdata, lon.map, lat.map, type=NULL, same=FALSE, zlim=NULL, col=NULL, nlevel=32, mai=c(0.2, 0.2, 0.2, 0.2), mgp=c(1.4, 0.5, 0), tcl=-0.3, ps=12, legend.mar=3, legend.width=1.2, xaxt="n", yaxt="n", map.region='world', Pacific.centric=FALSE, custom.breaks=NULL, map.xlim=NULL, map.ylim=NULL) {
   
   # This function plots spatial field data using function "image.plot" from package "fields".
   # Packages "fields" and "maps" must be pre-loaded.
   # "spdata" is a matrix or an array of spatial data.
   # "type" is a vector of intended types of presentation, as explained below.
   # Five types of presentation are supported:
   # 1. "sign": variable that has both positive and negative values, good for comparing signs of correlations/effects/deviations.
   # 2. "frac": variable that is a fraction or percentage, good for comparing proportions.
   # 3. "abs": variable that has absolute values only, good for comparing magtitudes.
   # 4. "def": user-defined scale and z-limits. (If chosen, must define the same same scale/limits for all plots.) 
   # 5. Default: no specification for display.
   # If you want all plots to have the same type, simply enter a string scalar for "type".
   
	if (lat.map[length(lat.map)] < lat.map[1]) {
		# 'lat.map' is in decreasing order. Need to reverse it to proceed further.
		lat.map = rev(lat.map)
		spdata = spdata[,length(lat.map):1]
	}	
	par(mai=mai, mgp=mgp, tcl=tcl, ps=ps)
	if (is.null(custom.breaks)) {
		if (is.null(type)) {
			zlim = c(min(na.omit(as.vector(spdata))), max(na.omit(as.vector(spdata))))
		} else if (type == 'sign') {
			if (is.null(zlim)) zlim = c(-max(abs(na.omit(as.vector(spdata)))), max(abs(na.omit(as.vector(spdata))))) else zlim = zlim
			if (is.null(col)) col = rwb.colors(nlevel)
		} else if (type == 'frac') {
			zlim = c(0,1)
		} else if (type == 'abs') {
			zlim = c(0, max(na.omit(as.vector(spdata))))
		} else if (type == 'def') {
			zlim = zlim
		} else {
			zlim = c(min(na.omit(as.vector(spdata))), max(na.omit(as.vector(spdata))))
		}
		if (is.null(col)) col = tim.colors(nlevel)
		spdata[which(spdata > zlim[2])] = zlim[2]
		spdata[which(spdata < zlim[1])] = zlim[1]
		image.plot(lon.map, lat.map, spdata, zlim=zlim, xlab='', ylab='', axis.args=list(mgp=c(0,0.5,0), tcl=-0.2), legend.mar=legend.mar, legend.width=legend.width, col=col, nlevel=nlevel, xaxt=xaxt, yaxt=yaxt)
	} else {
		nbreaks = length(custom.breaks)
		nlevel = nbreaks - 1
		if (!is.null(type)) {
			if (type == 'sign') col = rwb.colors(nlevel) else col = tim.colors(nlevel)
		} else col = tim.colors(nlevel)
		zval.breaks = 0:nlevel
		zval.center = 0:(nlevel - 1) + 0.5
		spdata.new = spdata
		for (n in 1:nlevel) spdata.new[which(spdata >= custom.breaks[n] & spdata <= custom.breaks[n + 1])] = zval.center[n]
		spdata.new[which(spdata < custom.breaks[1])] = zval.center[1]
		spdata.new[which(spdata > tail(custom.breaks, 1))] = tail(zval.center, 1)
		spdata = spdata.new
		zlim = c(0, nlevel)
		image.plot(lon.map, lat.map, spdata, zlim=zlim, xlab='', ylab='', axis.args=list(mgp=c(0,0.5,0), tcl=-0.2), legend.mar=legend.mar, legend.width=legend.width, col=col, nlevel=nlevel, xaxt=xaxt, yaxt=yaxt, breaks=zval.breaks, lab.breaks=custom.breaks)
	}
	if (Pacific.centric) map('world2', add=TRUE, xlim=map.xlim, ylim=map.ylim,col=1) else map(map.region, add=TRUE, xlim=map.xlim, ylim=map.ylim,col=1)
}

#=============================================
ss=read_NCEP("air.mon.mean.nc",xlim=c(230,300),ylim=c(25,50))
names(ss)
data=ss$data
lon=ss$lon
lat=ss$lat
date=ss$date

avg=apply(data,c(1,2),mean,na.rm=T)
plot.field(avg,lon,lat,Pacific.centric=T)

