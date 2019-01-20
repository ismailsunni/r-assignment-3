# Load library
source("functions.R")
# Load data
data = read.csv("data.csv", header = TRUE)
attach(data);

pairs(data,panel=function(x,y) {abline(lsfit(x,y)$coef,lwd=2)
lines(lowess(x,y),lty=2,lwd=2)
points(x,y)})

# Calculating Principal Component Value. 
# Calculating the Corelation Matrix of attribute 2 to 5 (HR to Gesage)
# cor=TRUE imples using Correlation Matrix
data.pc<-princomp(data[,2:5],cor=TRUE)
summary(data.pc,loadings=TRUE)

#From "Importance of Components"
#As the Cumulative proportion value is greater than (.70) i.e. 70%
#STOP at COMPONENT 2


#From the "Loadings" calculate the coefficient values ONLY for Comp.1, Comp. 2  
data.pc$scores[,1:3]

###Create Graph for Group aganist PC1 and Pc2
par(mfrow=c(1,2))
plot(data.pc$scores[,1],Group,xlab="PC1")
plot(data.pc$scores[,2],Group,xlab="PC2")

dev.off()

par(pty="s")
plot(data.pc$scores[,1],data.pc$scores[,2], ylim=range(data.pc$scores[,1]), xlab="PC1",ylab="PC2",type="n",lwd=2)
text(data.pc$scores[,1],data.pc$scores[,2], labels=abbreviate(row.names(data)),cex=0.7,lwd=2)