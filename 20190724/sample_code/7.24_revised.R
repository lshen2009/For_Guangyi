setwd("~/Summer_tutor/For_Guangyi")
data=read.csv("CASTNET_1990-2016.csv",header=TRUE)


sitedata = function(ID){
  ind = (data[,"ID"]==ID)
  sitedata = data[ind,]
  y=sitedata[,"mean"]
  y[y<=-999]=NA
  sitedata[,"mean"]=y
  return(sitedata)
}

cal_season_mean2=function(ID,yr1=1990,yr2=2000){
  data = sitedata(ID)
  ind=(data[,"year"]>=yr1 & data[,"year"]<=yr2 & data[,"month"]>=6 & data[,"month"]<=8)  
  season_mean=tapply(data[ind,"mean"],data[ind,"year"],mean,na.rm=TRUE)
  return(season_mean)
}

#line
dev.new(width=4.5,height=2.8)
par(mar=c(3,2,1,3))
par(mai=c(0.6, 0.6, 0.25, 0.2), mgp=c(1.4, 0.4, 0), tcl=-0.2, ps=11)
x = 1990:2000
# y = cal_season_mean2(ID,yr1,yr2)
plot(x,cal_season_mean2(7,1990,2000),ylab="Concentration",xlab="Year",pch=16,type="o",col=1 ,lwd=2,ylim=c(30,65))
lines(x,cal_season_mean2(8,1990,2000),col=2,lwd=2,pch=16,type="o")
lines(x,cal_season_mean2(9,1990,2000),col=3,lwd=2,pch=16,type="o")
lines(x,cal_season_mean2(10,1990,2000),col=4,lwd=2,pch=16,type="o")

legend("topleft", 
       legend = c("site7","site8","site9","site10"), 
       col = c(1,2,3,4),
       pch =16, 
       lty=1,
       bty = "n", 
       cex = 1, 
       text.col =c(1,2,3,4), 
       y.intersp=0.8)
title("Ozone concentrations in four sites",font.main=1,cex.main=1)

#每个站点的总平均数
y=data[,"mean"]
y[y<=-9999]=NA
data[,"mean"]=y

ID1 = 1
ID2 = 10

site_mean=array(NA,ID2-ID1+1)

for(ID in ID1:ID2){
  ind=(data[,"ID"]=ID)
  site_mean[ID-ID1+1]=mean(data[ind,"mean"],na.rm=TRUE)
}

#histogram
concentration=data[,"mean"]
dev.new(width=4.5,height=2.8)
par(mai=c(0.6, 0.6, 0.25, 0.2), mgp=c(1.4, 0.4, 0), tcl=-0.2, ps=11)
hist(concentration,20,main="Ozone Concentration across the US",xlab="Ozone Concentration",col=8,border =1,cex.main=1,font.main=1,breaks=30)
box()


#=========================
