#part1
rm(list=ls())
#Step1, install packages
#install.packages("maps")
#install.packages("fields")
#Then load these two packages
library(maps)
library(fields)

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

setwd("~/Summer_tutor/For_Guangyi")
data=read.csv("CASTNET_1990-2016.csv",header=TRUE)
data[data<=-999]=NA

siteslope=function(ID){
  ind=(data[,"ID"]==ID)
  y = tapply(data[ind,'mean'], data[ind,'year'],mean,na.rm=T)
  x = c(1990:2016)
  sitedata= data.frame(x,y)
  site.lm = lm(y~x,data=sitedata)
  coeffs_site = coefficients(site.lm)
  return(coeffs_site[2])
}

slopes=array(NA,52)
for(ID in 1:52){
  slopes[ID] = siteslope(ID)
}
slopes[slopes <= -0.5] <- -0.5

newID=c(1:52)
newdata=cbind.data.frame(newID,slopes)
#slope by site
slope=tapply(newdata[,"slopes"],newdata[,"newID"],mean,na.rm=T)
#Longitude by site
lon=tapply(data[,"lon"],data[,"ID"],mean,na.rm=T)
#Latitude by site
lat=tapply(data[,"lat"],data[,"ID"],mean,na.rm=T)

#dev.new(width=5,height=2.8)
latlon=cbind(lat,lon)#combine lon and lat
plot.site(slope,latlon,xlim=c(-125,-65),ylim=c(25,50),type="def",zlim=c(-0.5,0.5),cex=1,xaxt=NULL,yaxt=NULL)


#part2-1
setwd("~/Desktop")
data=read.csv("CASTNET_1990-2016.csv",header=TRUE)
data[data<=-9999]=NA

yearmean=function(ID){
  ind = (data[,"ID"]==ID)
  mean=tapply(data[ind,"mean"],data[ind,"year"],mean)
  return(mean)
}

mean=array(NA,c(52,27))
for(ID in 1:52){
  mean[ID,] = yearmean(ID)
}
colnames(mean)<- c(1990:2016)

#number of sites that have three observations in one month
array(NA,27)
for(year in 1:27){
  sites_with_3data[year] = sum(!is.na(mean[,year]))
}

