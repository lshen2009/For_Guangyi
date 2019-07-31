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



read_NCEP=function(name,start_month=6,end_month=8,start_yr=1990,end_yr=2016,pressure=NULL,xlim=c(210,290),ylim=c(10,70)){
  #name="air.mon.mean.nc"
  #xlim is the range of longitude
  #ylim is the range of latitude	
  datadir='/Users/lulushen/Summer_tutor/For_Guangyi/20190729/'
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

plot.site = function(spdata, latlon, xlim=NULL, ylim=NULL, pch=19, type=NULL, zlim=NULL, col=NULL, nlevel=32, mai=c(0.5, 0.4, 0.2, 0.2), mgp=c(1.4, 0.5, 0), tcl=-0.3, ps=12, legend.mar=3, legend.width=1.2, xaxt="n", yaxt="n", Pacific.centric=FALSE,cex=1) {     
	MAT = na.omit(cbind(spdata, latlon))
	spdata = MAT[,1]; latlon = MAT[,2:3]
   if (is.null(xlim)) xlim = range(latlon[,2])
   if (is.null(ylim)) ylim = range(latlon[,1])
	latlon.uniq = unique(latlon)
	spdata.uniq = rep(0, times=nrow(latlon.uniq))
	col.uniq = rep(0, times=nrow(latlon.uniq))
	for (i in 1:length(spdata.uniq)) {
		spdata.uniq[i] = mean(spdata[which(latlon[,1] == latlon.uniq[i,1] & latlon[,2] == latlon.uniq[i,2])], na.rm=TRUE)
	}
	if (is.null(type)) {
      zlim = c(min(spdata.uniq, na.rm=TRUE), max(spdata.uniq, na.rm=TRUE))
   } else if (type == 'sign') {
      if (is.null(zlim)) zlim = c(-max(abs(spdata.uniq)), max(abs(spdata.uniq))) else zlim = zlim
      if (is.null(col)) col = rwb.colors(nlevel)
   } else if (type == "frac") {
      zlim = c(0, 1)
   } else if (type == "abs") {
      zlim = c(0, max(spdata.uniq, na.rm=TRUE))
   } else if (type == "def") {
      zlim = zlim
   } else {
      zlim = c(min(spdata.uniq, na.rm=TRUE), max(spdata.uniq, na.rm=TRUE))
   }
	if (is.null(col)) col = tim.colors(nlevel)
	zbreak = seq(zlim[1], zlim[2], length=(length(col) + 1))
	for (i in 1:length(col)) col.uniq[which(spdata.uniq >= zbreak[i] & spdata.uniq <= zbreak[i+1])] = col[i]
	par(mai=mai, mgp=mgp, tcl=tcl, ps=ps)
	image.plot(seq(xlim[1], xlim[2], length=10), seq(ylim[1], ylim[2], length=10), matrix(NA, nrow=10, ncol=10), zlim=zlim, xlab="", ylab="", axis.args=list(mgp=c(0,0.5,0), tcl=-0.2), legend.mar=legend.mar, legend.width=legend.width, col=col, nlevel=nlevel, xaxt=xaxt, yaxt=yaxt)
	points(latlon.uniq[,2], latlon.uniq[,1], col=col.uniq, pch=pch,cex=cex)
	if (Pacific.centric) map('world2', add=TRUE,col=8) else map('world', add=TRUE,col=8)
}

mov.avg.wgt=function(data, weights) {
	dum = length(weights)/2 - 0.5
	data.long = c(rep(NaN, times=dum), data, rep(NaN, times=dum))
	m.avg = rep(0, times=length(data))
	for (t in 1:length(data)) {
		sum.weights = sum(weights*(data.long[t:(t+dum*2)]/data.long[t:(t+dum*2)]), na.rm=TRUE)
		m.avg[t] = sum(data.long[t:(t+dum*2)]*weights/sum.weights, na.rm=TRUE)
	}
	return(m.avg)
}

mov.detrend=function(temp,len=7){
	  ap=mov.avg.wgt(temp, rep(1,len))	   	  
	  return(as.numeric(temp-ap))
}

linear.detrend=function(temp){
	xx=1:length(temp)
	MAT=na.omit(cbind(xx,temp))
	spdata=array(NA,c(length(xx)))
	if (length(MAT)>=8){
		fit<-lm(MAT[,2]~MAT[,1])
		yy=xx*fit$coefficients[2]+fit$coefficients[1]
		spdata=temp-yy
	}
	return(spdata)
}

cal.season.mean=function(y){
	y2=array(NA,length(y)/3)
	for(k in 1:length(y2)){
		y2[k]=mean(y[(3*k-2):(3*k)],na.rm=T)
	}
	return(y2)
}



cal.season_mean=function(spdata, date, start_month, end_month, start_yr, end_yr){
	result=array(NA, c(dim(spdata)[1], dim(spdata)[2], (end_yr-start_yr+1)))
	for(year in start_yr: end_yr){
		ind=(date[,1]==year & date[,2]>=start_month & date[,2]<=end_month)
		result[,,year-start_yr+1]=apply(spdata[,,ind],c(1,2),mean,na.rm=TRUE)
	}
	return(result)
}