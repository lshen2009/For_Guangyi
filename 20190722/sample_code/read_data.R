#change your working directory
setwd("~/Summer_tutor/For_Guangyi/20190722/sample_code")
#read the data
data=read.csv("small_data.csv",header=TRUE)
y=data[,"mean"]
y[y<=-999]=NA
data[,"mean"]=y

#====plot a simple figure ====
dev.new(width=5,height=2.8)
par(mai=c(0.6, 0.6, 0.25, 0.2), mgp=c(1.6, 0.4, 0), tcl=-0.2, ps=13)
plot(data[,'mean'],pch=16,type="o",xlab="",ylab="O3 concentrations")

#=== calculate the seasonal mean ===
yr1=1990
yr2=2000
season_mean=array(NA,yr2-yr1+1)
for(year in yr1:yr2){
	ind=(data[,"year"]==year & data[,"month"]>=6 & data[,"month"]<=8)
	season_mean[year-yr1+1]=mean(data[ind,"mean"],na.rm=TRUE)
}
dev.new(width=5,height=2.8)
par(mai=c(0.6, 0.6, 0.25, 0.2), mgp=c(1.6, 0.4, 0), tcl=-0.2, ps=13)
plot(yr1:yr2,season_mean,pch=16,type="o",xlab="",ylab="O3 concentrations")