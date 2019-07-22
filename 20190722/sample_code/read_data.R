#change your working directory
setwd("~/Summer_tutor/For_Guangyi/20190722/sample_code")
#read the data
data=read.csv("small_data.csv",header=TRUE)

#====plot a simple figure ====
dev.new(width=5,height=2.8)
par(mai=c(0.6, 0.6, 0.25, 0.2), mgp=c(1.6, 0.4, 0), tcl=-0.2, ps=13)
plot(data[,'mean'],pch=16,type="o",xlab="",ylab="O3 concentrations")