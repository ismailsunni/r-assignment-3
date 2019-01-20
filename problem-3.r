# Clean memory and environment variables
rm(list=ls());
gc();

# Load library
source("functions.R")
# Load MASS library
library(MASS)

# Load data
data = read.csv("data.csv", header = TRUE)
attach(data);

# ignore the group column
data.components = data[,2:5]

# Create dendogram for the clustering using 3 methods
par(mfrow=c(1,3))
plclust(hclust(dist(data.components),method="single"),labels=row.names(data.components),ylab="Distance")
title("(a) Single linkage")
plclust(hclust(dist(data.components),method="complete"),labels=row.names(data.components),ylab="Distance")
title("(b) Complete linkage")
plclust(hclust(dist(data.components),method="average"),labels=row.names(data.components),ylab="Distance")
title("(c) Average linkage")

# Use h=2000 to cut the dendogram (example) and get 2 clusters
two<-cutree(hclust(dist(data.components),method="complete"),h=2000)
as.data.frame(two) # show the content vertically

data.clus<-lapply(1:2,function(nc) row.names(data)[two==nc])
data.mean<-lapply(1:2,function(nc) apply(data[two==nc,],2,mean))
data.mean
data.clus

dev.off()

pairs(data,panel=function(x,y) text(x,y,two))

dev.off()

rge<-apply(data.components,2,max)-apply(data.components,2,min) # Range = max - min
data.dat<-sweep(data.components,2,rge,FUN="/")  # naive normalization : dividing by the range
#
n<-length(data.dat[,1])  # number of observation
wss1<-(n-1)*sum(apply(data.dat,2,var))  # get variance from the column
wss<-numeric(0)
for(i in 2:5) {
  W<-sum(kmeans(data.dat,i)$withinss)
  wss<-c(wss,W)
}
wss<-c(wss1,wss)
plot(1:5,wss,type="l",xlab="Number of groups",ylab="Within groups sum of squares",lwd=2)

# New data
# Declare new data
newdata<-rbind(c(110,3320,0.240,39), c(120,3310,0.298,37))
colnames(newdata) <- colnames(data.components)
newdata <- data.frame(newdata)

# linear discriminant analysis
dis<-lda(two~HR+BW+Factor68+Gesage,data=data.components,prior=c(0.5,0.5))

# Predict the cluster for the new data
predict(dis,newdata = newdata)


