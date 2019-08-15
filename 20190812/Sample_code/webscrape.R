library("rvest")
library(stringr)
library(maps)
library(mapdata)

url <- "http://aqicn.org/map/"
web <- read_html(url)

data <- web%>%html_nodes("script")
ss=data[14]
ap=html_text(ss)
substr(ap,1,500)
# ap2=gsub("\\","",ap)

splitdata=strsplit(ap,split="\\},\\{")[[1]]
	
latlon=NULL	
for (kk in 1:length(splitdata))	{
temp= splitdata[kk]
temp=gsub("\"","",temp)
aqi1=str_match(temp,"aqi:\\d+")
aqi2=str_match(aqi1,"\\d+")
loc1=str_match(temp,"g:\\[\\d+.\\d+,\\d+.\\d+\\]")
loc2=regmatches(loc1,gregexpr("\\d+.\\d+",loc1,perl=T))[[1]]
loc2=as.numeric(loc2)
latlon=rbind(latlon,loc2)
}
plot(NA,NA,xlim=c(70,150),ylim=c(15,50))
map("world",add=T)
points(latlon[,2],latlon[,1],pch=16,cex=0.5)