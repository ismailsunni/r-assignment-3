# Load library
source("functions.R")
# Load data
data = read.csv("data.csv", header = TRUE)
attach(data);

#  Show correlation matrix, without group column
cor(data[,2:5]);

# Choose BW and Gesage, 0.424 correlation

##### plotting scatter plot with abline ####
# Divide the plot area to 2x2
par(mfrow=c(2,2))
par(pty="s")
# Scatterplot BW and Gesage
plot(BW,Gesage,pch=1,lwd=2)
title("(a)",lwd=2)
# Scatterplot BW and Gesage with abline / linear regression fit
plot(BW,Gesage,pch=1,lwd=2)
abline(lm(Gesage~BW),lwd=2)
title("(b)",lwd=2)
# Jittered Scatterplot BW and Gesage
data1<-jitter(cbind(BW,Gesage))
plot(BW,Gesage,pch=1,lwd=2)
title("(c)",lwd=2)
# Plot BW and Gesage with jitter and histogram
plot(BW,Gesage,pch=1,lwd=2)
rug(jitter(BW),side=1)
rug(jitter(Gesage),side=2)
title("(d)",lwd=2) 

# Reset plot area
dev.off();

##### Scatter plot with abbreviation ####
names<-abbreviate(row.names(data))
par(mfrow=c(1,1))
plot(BW,Gesage,lwd=2,type="n")
text(BW,Gesage,labels=names,lwd=2)

# Reset plot area
dev.off();

##### Scatterplot BW against Gesage with linear regression and locally weighted fit, and marginal distribution ####
# set up plotting area for scatterplot
par(fig=c(0,0.7,0,0.7))
plot(BW,Gesage,lwd=2)
# add regression line
abline(lm(Gesage~BW),lwd=2)
# add locally weighted regression fit
lines(lowess(BW,Gesage),lwd=2)
# set up plotting area for histogram
par(fig=c(0,0.7,0.65,1),new=TRUE)
hist(BW,lwd=2)
# set up plotting area for boxplot
par(fig=c(0.65,1,0,0.7),new=TRUE)
boxplot(Gesage,lwd=2)

# Reset plot area
dev.off();

##### Convex hull ####
hull<-chull(BW,Gesage)
plot(BW,Gesage,pch=1)
polygon(BW[hull],Gesage[hull],density=15,angle=30)

# Reset plot area
dev.off();

##### Chi plots ####
chiplot(BW,Gesage,vlabs=c("BW","Gesage"))

# Reset plot area
dev.off();

#### Bivariate Boxplot####
# Create bvbox with robust estimator method (the default one)
bvbox(cbind(BW,Gesage),xlab="BW",ylab="Gesage")
# Create bvbox with other (non robust) method
bvbox(cbind(BW,Gesage),xlab="BW",ylab="Gesage",method="o")

# Reset plot area
dev.off();

##### perspective  plot of Bivariate Density ####
den1<-bivden(BW,Gesage)
persp(den1$seqx,den1$seqy,den1$den,xlab="BW",ylab="Gesage",
      zlab="Density",lwd=2)

# Reset plot area
dev.off();

##### contour plot of Bivariate Density ####
plot(BW,Gesage)
contour(den1$seqx,den1$seqy,den1$den,lwd=2,nlevels=20,add=TRUE)

# Reset plot area
dev.off();

##### matrix plots  ####
pairs(data,panel=function(x,y) {abline(lsfit(x,y)$coef,lwd=2)
  lines(lowess(x,y),lty=2,lwd=2)
  points(x,y)})

# Reset plot area
dev.off();

#### Conditioning Plot  ######
coplot(BW~Gesage|Factor68)
coplot(BW~Gesage|Factor68,panel=function(x,y,col,pch)
  panel.smooth(x,y,span=1))

# Reset plot area
dev.off()