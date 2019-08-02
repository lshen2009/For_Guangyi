plot_correlation = function(name){
  read_NCEP=function(name,start_month=6,end_month=8,start_yr=1990,end_yr=2016,pressure=NULL,xlim=c(210,290),ylim=c(10,70)){
    #xlim is the range of longitude
    #ylim is the range of latitude	
    datadir='~/Desktop/'
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
    
    output_data = ncvar_get(datafile,start=c(ind1[1],ind2[1],ind3[1]), count=c(length(ind1),length(ind2),length(ind3)))
    
    #lat is in a decreasing order, now reorder it
    output_data=output_data[,length(lat):1,]#revert along the second order
    lat=rev(lat)
    
    ind4=which(date[,1]>=start_yr & date[,1]<=end_yr & date[,2]>=start_month & date[,2]<=end_month)
    output_data=output_data[,,ind4]
    date=date[ind4,]
    
    return(list('data'= output_data,'lon'=lon,'lat'=lat,'date'= date))
  }
  #ozone data (season mean for 1 site)
  ozone_data=read.csv("CASTNET_1990-2016.csv",header=TRUE)
  ozone_data[ozone_data<=-999]=NA
  ozone_season_mean=function(ozone_data,ID,start_yr=1990, end_yr=2016){
    mean_value=array(NA,end_yr-start_yr+1)
    for(year in start_yr:end_yr){
      ind = (ozone_data[,"year"]==year & ozone_data[,"ID"]==ID)
      newdata=ozone_data[ind,]
      mean_value[year-start_yr+1]=tapply(newdata[,"mean"],newdata[,"ID"],mean,na.rm=TRUE)
    }
    return(mean_value)
  }
  y=ozone_season_mean
  
  #humidity (season mean)
  ss=read_NCEP(name)
  names(ss)
  data1=ss$data
  lat=ss$lat
  lon=ss$lon-360
  date=ss$date
  
  cal.season_mean1=function(data1, date, start_month=6, end_month=8, start_yr=1990, end_yr=2016){
    result=array(NA, c(dim(data1)[1], dim(data1)[2], (end_yr-start_yr+1)))
    for(year in start_yr: end_yr){
      ind=(date[,1]==year & date[,2]>=start_month & date[,2]<=end_month)
      result[,,year-start_yr+1]=apply(data1[,,ind],c(1,2),mean,na.rm=TRUE)
    }
    return(result)
  }
  data1=cal.season_mean1(data1,date)
  
  #correlation for one site 
  sites_correlation1 = function(ID){
    o_lon=tapply(ozone_data[,"lon"],ozone_data[,"ID"],mean,na.rm=T)
    o_lat=tapply(ozone_data[,"lat"],ozone_data[,"ID"],mean,na.rm=T)
    ind1=which.min(abs(lon-o_lon[ID]))
    ind2=which.min(abs(lat-o_lat[ID]))
    season_mean1=data1[ind1,ind2,] #the location's corresponding humidity
    
    ozone_season_mean=function(ozone_data,ID,start_yr=1990, end_yr=2016){
      mean_value=array(NA,end_yr-start_yr+1)
      for(year in start_yr:end_yr){
        ind = (ozone_data[,"year"]==year & ozone_data[,"ID"]==ID)
        newdata=ozone_data[ind,]
        mean_value[year-start_yr+1]=tapply(newdata[,"mean"],newdata[,"ID"],mean,na.rm=TRUE)
      }
      return(mean_value)
    }
    y=ozone_season_mean(ozone_data,ID)
    
    x=1:length(y)
    fit<-lm(y~x)
    missing.ind=is.na(y)
    y.res=array(NA,length(y))
    y.res[!missing.ind]=resid(fit)
    anormaly=y.res
    
    fit2<-lm(season_mean1 ~ anormaly)
    correlation = cor.test(season_mean1,anormaly)$estimate
    
    return(correlation)
  }
  
  #all sites correlation
  correlation1=array(NA,52)
  for(ID in 1:52){
    correlation1[ID]=sites_correlation1(ID)
  }
  correlation1
  
  #plot on the map
  #define a function
  plot.site = function(spdata, latlon, xlim=NULL, ylim=NULL, pch=19, type=NULL, zlim=NULL, col=NULL, nlevel=32, mai=c(0.5, 0.4, 0.2, 0.2), mgp=c(1.4, 0.5, 0), tcl=-0.3, ps=12, legend.mar=3, legend.width=1.2, xaxt="n", yaxt="n", Pacific.centric=FALSE,cex=1) {     
    MAT = na.omit(cbind(spdata, latlon))
    #spdata: ozone concentrations, 1D
    #latlon: latitude and longitude, 2D, nx2
    #xlim: range of x axis
    #ylim: range of y axis
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
  
  #correlation
  correlation1
  #Longitude by site
  lon=tapply(ozone_data[,"lon"],ozone_data[,"ID"],mean,na.rm=T)
  #Latitude by site
  lat=tapply(ozone_data[,"lat"],ozone_data[,"ID"],mean,na.rm=T)
  
  #dev.new(width=5,height=2.8)
  latlon=cbind(lat,lon)#combine lon and lat
  plot.site(correlation1,latlon,xlim=c(-125,-65),ylim=c(25,50),cex=1,xaxt=NULL,yaxt=NULL)
}