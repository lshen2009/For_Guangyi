#change your working directory
setwd("~/Summer_tutor/For_Guangyi/20190722/sample_code")
data=read.csv("small_data.csv",header=TRUE)
y=data[,"mean"]
y[y<=-999]=NA
data[,"mean"]=y

#=== I define a function here====
cal_season_mean=function(data,yr1=1990,yr2=2000){#These are input parameters
   season_mean=array(NA,yr2-yr1+1)
   for(year in yr1:yr2){#loop over each year
	  ind=(data[,"year"]==year & data[,"month"]>=6 & data[,"month"]<=8)
	  season_mean[year-yr1+1]=mean(data[ind,"mean"],na.rm=TRUE)
   }
   return(season_mean)#We need to return a value
}

yr1=1990;yr2=1997
dev.new(width=5,height=2.8)
par(mai=c(0.6, 0.6, 0.25, 0.2), mgp=c(1.6, 0.4, 0), tcl=-0.2, ps=13)
x=yr1:yr2
y= cal_season_mean(data,yr1,yr2)#Call the function
plot(yr1:yr2,y,pch=16,type="o",xlab="",ylab="O3 concentrations")

#=== I define a function here but I use tapply====
cal_season_mean2=function(data,yr1=1990,yr2=2000){
   ind=(data[,"year"]>=yr1 & data[,"year"]<=yr2 & data[,"month"]>=6 & data[,"month"]<=8)
   #Here I use tapply to make my code succinct
   season_mean=tapply(data[ind,"mean"],data[ind,"year"],mean,na.rm=TRUE)
   return(season_mean)
}
y= cal_season_mean(data,yr1,yr2)#Call the function
y2= cal_season_mean2(data,yr1,yr2)#Call the function
#you can see y and y2 are the same.

#==== how to plot multiple figures in one plot? ===
dev.new(width=5,height=2.8)
par(mar=c(3,2,1,3))
par(mfrow=c(2,2))
par(mai=c(0.3, 0.6, 0.25, 0.2), mgp=c(1.4, 0.4, 0), tcl=-0.2, ps=11)
plot(cal_season_mean2(data,1990,2005),ylab="Concentration",xlab="",pch=16,type="o")
plot(cal_season_mean2(data,1990,2009),ylab="Concentration",xlab="",pch=16,type="o")
plot(cal_season_mean2(data,1993,2009),ylab="Concentration",xlab="",pch=16,type="o")
plot(cal_season_mean2(data,1993,2010),ylab="Concentration",xlab="",pch=16,type="o")
