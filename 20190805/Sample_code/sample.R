#== construct a 33x21x27 matrix
Temperature=array(NA,c(33,21,27))#lon x lat x time
for(i in 1:30){
	for(j in 1:21){
		Temperature[i,j,]=rnorm(27)#This is a random function
	}
}
ozone=rnorm(27)# Let's say this is ozone concentrations

#== correlation of gridbox (i,j) and ozone
i=10
j=5
cor(Temperature[i,j,],ozone)

#== if you use for loops ==============
#define a matrix
correlation=array(NA,c(33,21))
for (i in 1:33){
	for (j in 1:21){
		correlation[i,j]=cor(Temperature[i,j,],ozone)
	}
}

#== define a function ==============
haha=function(spdata,y){
correlation=array(NA,c(dim(spdata)[1],dim(spdata)[2]))
for (i in 1:dim(spdata)[1]){
	for (j in 1:dim(spdata)[2]){
		MAT=na.omit(cbind(spdata[i,j,],y))#remove rows with missing data
		if(length(MAT)>=5){
		correlation[i,j]= cor(spdata[i,j,],y)
		}
	}
}
return(correlation)
}

ss=haha(Temperature,ozone)
