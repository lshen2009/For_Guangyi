rm(list=ls())
library(neuralnet)
setwd("~/Summer_tutor/For_Guangyi/20190809/Data")
files=Sys.glob("*.csv")

#===== read data in all years ======
#the year 2004 is missing
result=NULL
for(ifile in files){
met=read.csv(ifile)
data=met[,c("DATE","TEMP","DEWP","SLP","WDSP","MAX","MIN","PRCP")]
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
final=cbind(date,final_data[,2:8])
colnames(final)=c("Year","Month","Day","TEMP","DEWP","SLP","WDSP","MAX","MIN","PRCP")
head(final)

#----- select summertime days -----
time.ind=which(date[,2]>=6 & date[,2]<=8)
y=final[time.ind+1,"TEMP"]#y is the temperature of the coming day
new=na.omit(cbind(final[time.ind,c("Year","Month","Day","TEMP","DEWP","WDSP","MAX","MIN","PRCP")],y))

#---- normalize the data -----
avg <- apply(new, 2, mean,na.rm=T) 
std <- apply(new, 2, sd,na.rm=T)
new <- as.data.frame(scale(new,center = avg,scale = std))

#---- fit a neural networks model ---
nn=neuralnet(y~TEMP+DEWP+WDSP+MAX+MIN+PRCP,data=new,hidden=4,act.fct="tanh",linear.output = T, threshold=0.05,stepmax=1e5)
plot(nn)

#--- test the R2 ----
test=data.frame(new[,c("TEMP","DEWP","WDSP","MAX","MIN","PRCP")])
Predict=compute(nn,test)
cor(Predict$net.result,new$y)^2