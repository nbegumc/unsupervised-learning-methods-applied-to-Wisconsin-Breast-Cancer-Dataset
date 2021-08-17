# loading the libraries
library(car)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(gplots)


data=read.csv("Wdbc.data")
fix(data)
dim(data)

# Data Preprocessing

#setting the colnames
names=c('id', 'diagnosis', 'radius_mean', 
  'texture_mean', 'perimeter_mean', 'area_mean', 
  'smoothness_mean', 'compactness_mean', 
  'concavity_mean','concave_points_mean', 
  'symmetry_mean', 'fractal_dimension_mean',
  'radius_se', 'texture_se', 'perimeter_se', 
  'area_se', 'smoothness_se', 'compactness_se', 
  'concavity_se', 'concave_points_se', 
  'symmetry_se', 'fractal_dimension_se', 
  'radius_worst', 'texture_worst', 
  'perimeter_worst', 'area_worst', 
  'smoothness_worst', 'compactness_worst', 
  'concavity_worst', 'concave_points_worst', 
  'symmetry_worst', 'fractal_dimension_worst')

colnames(data)=names
rownames(data)=data$id
data=data[,-1]
fix(data)

#inspection of missing values in the dataset
sum(is.na(data)) # number of records with N/A values


########  Descriptives ##########
#descriptives:
means=apply(data[,-1],2,mean)
stand.dev.=apply(data[,-1],2,sd)
descriptives=round(cbind(means,stand.dev.),2)
descriptives
summary(data)



#correlations # check if the variables correlated
library(GGally)
ggcorr(data, method = c("everything", "pearson"),
       label_alpha= TRUE,
       label = TRUE, label_size = 2, layout.exp= 0) 

########################################################

# Principal Component Analysis


wdbc=data[,-c(1)] # dropping the categorical variables
wdbc= scale(wdbc)  # To standarize the variables
summary(wdbc)



wdbc_pc=prcomp(wdbc,scale. = TRUE,center=TRUE)
summary(wdbc_pc)

wdbc_pc$center #means
wdbc_pc$scale #sd

round(wdbc_pc$rotation,2) # loadings
round(wdbc_pc$x,4) # scores


###########################
library(lattice)
# see which variable contributes to PC1 the most
load    <- wdbc_pc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings <- load[order(load[, 2]), 1]
myTitle <- "Loadings Plot for PC2" 
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings <- load[order(load[, 3]), 1]
myTitle <- "Loadings Plot for PC3" 
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

#  BiPlot
cex.before <- par("cex")
par(cex = 0.7)
biplot(wdbc_pc) 
par(cex = cex.before)

# Change the direction
wdbc_pc$rotation=-wdbc_pc$rotation
wdbc_pc$x=-wdbc_pc$x

cex.before <- par("cex")
par(cex = 0.7)
biplot(wdbc_pc) 
par(cex = cex.before)

#####################
plot(wdbc_pc)
#The variance explained by each principal component is obtained by squaring
#these:
pr.var=wdbc_pc$sdev^2
round(pr.var,4)
#proportion of variance explained by each principal component,
pve=pr.var/sum(pr.var)
round(pve,2)

plot(pr.var,main="Scree Diagram",xlab = "Number of Components",
     ylab="Eigenvalues",
     type = 'b')
abline(h=1, lwd=3, col="red")

#plot the PVE explained by each component
plot(pve,xlab = "Principal Component",ylab="Proportion of Variance Explained",ylim = c(0,1),
     type = 'b')
abline(h=0, lwd=3, col="red")
#cumulative
plot(cumsum(pve),xlab = "Principal Component",ylab="Cumulative Proportion of Variance Explained",ylim = c(0,1),
     type = 'b')
abline(h=0.95, lwd=3, col="red")



biplot(wdbc_pc, xlabs = rep("", nrow(wdbc))) # to make it easier to show the vectors

#select how many components
screeplot(wdbc_pc)
pca_var <- wdbc_pc$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", 
        ylab = "Percentage Variance", ylim = c(0, 100))

library("factoextra") 
fviz_screeplot(wdbc_pc, addlabels = TRUE, ylim = c(0, 50))

#first 7 components explain 95% of the total variance
#components
components=round(cbind(wdbc_pc$rotation[,1]*wdbc_pc$sd[1],wdbc_pc$rotation[,2]*wdbc_pc$sd[2],
                       wdbc_pc$rotation[,3]*wdbc_pc$sd[3],wdbc_pc$rotation[,4]*wdbc_pc$sd[4],
                       wdbc_pc$rotation[,5]*wdbc_pc$sd[5],wdbc_pc$rotation[,6]*wdbc_pc$sd[6],
                       wdbc_pc$rotation[,7]*wdbc_pc$sd[7])
                 ,2)
colnames(components)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7")

communality<-components[,1]^2+components[,2]^2+components[,3]^2+
  components[,4]^2 + components[,5]^2 + components[,6]^2+components[,7]^2
  
components<-cbind(components,communality)
components

# standardized scores


sd <- wdbc_pc$sdev
scores<-round(cbind(wdbc_pc$x[,1]/sd[1],wdbc_pc$x[,2]/sd[2],wdbc_pc$x[,3]/sd[3],
                    wdbc_pc$x[,4]/sd[4],wdbc_pc$x[,5]/sd[5],wdbc_pc$x[,6]/sd[6],
                    wdbc_pc$x[,7]/sd[7]),2)
scores
plot(scores, main="Score plot",
     xlab="comp1",ylab="comp2")
text(scores, rownames(wdbc))
abline(v=0,h=0,col="red")
colnames(scores)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7")
scores

# loadings
par(mfrow=c(1,1))
plot(components[,1:2], main="Loadings plot",
     xlab="comp1",ylab="comp2", xlim=range(-1,1))
text(components, rownames(components))
abline(v=0,h=0,col="red")

plot(components[,2:3], main="Loadings plot",
     xlab="comp2",ylab="comp3", xlim=range(-1,1))
text(components, rownames(components))
abline(v=0,h=0,col="red")

plot(components[,1:3], main="Loadings plot",
     xlab="comp1",ylab="comp3", xlim=range(-1,1))
text(components, rownames(components))
abline(v=0,h=0,col="red")


#### different plottings
diagnosis <- factor(data$diagnosis)

pca_df <- as_tibble(wdbc_pc$x)
ggplot(pca_df, aes(x = PC1, y = PC2, col = data$diagnosis)) + geom_point()

library(ggfortify)
wdbc1=as.data.frame(cbind(wdbc,data$diagnosis),)
colnames(wdbc1['V31'])='diagnosis'

autoplot(wdbc_pc, data = wdbc1, colour ="V31", loadings = FALSE,loadings.label = TRUE,
         loadings.label.size = 3, loadings.colour="black")




##########

