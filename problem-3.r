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

# Ignore the group column
data.components = data[,2:5]

# Create dendrogram for the clustering using 3 methods
par(mfrow=c(1,3))
plclust(hclust(dist(data.components),method="single"),labels=row.names(data.components),ylab="Distance")
title("(a) Single linkage")
plclust(hclust(dist(data.components),method="complete"),labels=row.names(data.components),ylab="Distance")
title("(b) Complete linkage")
plclust(hclust(dist(data.components),method="average"),labels=row.names(data.components),ylab="Distance")
title("(c) Average linkage")

# knn analysis to find optimum number of cluster
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
dev.off()

# Complete method
# Use h=2000 to cut the dendogram (example) and get 2 clusters
twoComplete<-cutree(hclust(dist(data.components),method="complete"),h=2000)
as.data.frame(twoComplete) # show the content vertically

data.clusComplete<-lapply(1:2,function(nc) row.names(data)[twoComplete==nc])
data.meanComplete<-lapply(1:2,function(nc) apply(data[twoComplete==nc,],2,mean))
data.meanComplete
data.clusComplete

# Compare to original data
accuracyComplete<-sum(abs(twoComplete-Group)) / nrow(data)
accuracyComplete

pairs(data,panel=function(x,y) text(x,y,twoComplete))
dev.off()

# Average method
# Use h=1200 to cut the dendogram (example) and get 2 clusters
twoAverage<-cutree(hclust(dist(data.components),method="average"),h=1200)
as.data.frame(twoAverage) # show the content vertically

data.clusAverage<-lapply(1:2,function(nc) row.names(data)[twoAverage==nc])
data.meanAverage<-lapply(1:2,function(nc) apply(data[twoAverage==nc,],2,mean))
data.meanAverage
data.clusAverage

# Compare to original data
accuracyAverage<-sum(abs(twoAverage-Group)) / nrow(data)
accuracyAverage

pairs(data,panel=function(x,y) text(x,y,twoAverage))
dev.off()

# New data
# Declare new data
newdata<-rbind(c(110,3320,0.240,39), c(120,3310,0.298,37))
colnames(newdata) <- colnames(data.components)
newdata <- data.frame(newdata)

# linear discriminant analysis
dis<-lda(twoComplete~HR+BW+Factor68+Gesage,data=data.components,prior=c(0.5,0.5))

# Predict the cluster for the new data
predict(dis,newdata = newdata)