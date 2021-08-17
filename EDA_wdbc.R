# loading the libraries
library(car)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(gplots)
library(GGally)
library(neuralnet)

#loading the data

data=read.csv("wdbc.data")

fix(data)


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
head(data)
attach(data)
dim(data)
sum(is.na(data)) # no N/A values

names(data)

names(wdbc_)[names(wdbc_) == 'V31'] <- 'diagnosis'
#data preprocessing



############# Summary Statistics of Variables ##############

summary(data) # all numeric, no outliers, 4 vars log transformed,scaled with target value

############# Visualising distributions ###################

## diagnosis ##
data %>%
  count(diagnosis)

ggplot(data = data) +
  geom_bar(mapping = aes(x = diagnosis))

## alcohol ##
ggplot(data = data[,-c(1)], mapping = aes(x = radius_mean)) +
  geom_histogram(binwidth = 0.1)
ggplot(data = data, aes(x = radius_mean)) + 
  stat_density()
data %>% 
  count(cut_width(radius_mean, 0.5))

ggplot(data = data, mapping = aes(x = radius_mean, colour = diagnosis)) +
  geom_freqpoly(binwidth = 0.5)



######## Visualising all variables ##

#density plots #

require(reshape2)

melt.data = melt(select(data,-c('diagnosis')))
head(melt.data)


ggplot(data = melt.data, aes(x = value)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  facet_wrap(~variable, scales = "free")

# some variables need to be transformed 
#' 
#' 
#' 
#boxplots # --- Identifying outliers ---

ggplot(data = melt.data, aes(y=value)) + 
  geom_boxplot(outlier.color="red") + 
  facet_wrap(~variable, scales = "free")

###bunu kullanma
par(cex.axis=0.45)
boxplot(data, col = 'orange')
####

#' almost all variables have outliers

###Removing Outliers ###

outliers = c()
for (i in 3:8){
  stats=boxplot.stats(data[[i]])$stats
  bottom_outlier_rows = which(data[[i]] < stats[1])
  top_outlier_rows = which(data[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
  outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% outliers ] )
  
}
data_no = data[-outliers,]
summary(data_no)
# example (extreme values of radius_mean)
ggplot(data) + 
  geom_histogram(mapping = aes(x = radius_mean), binwidth = 0.5)

ggplot(data) + 
  geom_histogram(mapping = aes(x = radius_mean), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

radius.mean_outliers = data %>% 
  filter(radius_mean > 24)
radius.mean_outliers


##Covariation###

ggplot(data = data, mapping = aes(group = diagnosis, y = radius_mean)) +
  geom_boxplot()

ggplot(data = data) +
  geom_boxplot(mapping = aes(x = reorder(diagnosis, radius_mean, FUN = mean), y = radius_mean))
#Higher radius.mean levels tend to be Malign

## correlations ##
cor(data[,-1])

###


ggcorr(data, method = c("everything", "pearson"),
       label_alpha= TRUE,
       label = TRUE, label_size = 2, layout.exp= 0) 

## scatter plots of correlated variables ##
library(ggplot2)
ggpairs(data, columns = c(2:10),
        title="correlogram with ggpairs()") 


####individual scatter plots####
ggplot(data = data) + 
  geom_point(mapping = aes(x =radius_mean , y =texture_mean ))
ggplot(data = data) + 
  geom_point(mapping = aes(x =perimeter_mean , y =area_mean))
ggplot(data = data) + 
  geom_point(mapping = aes(x =smoothness_mean , y =compactness_mean ))
ggplot(data = data) + 
  geom_point(mapping = aes(x =concavity_mean , y =concave_points_mean ))
ggplot(data = data) + 
  geom_point(mapping = aes(x =radius_mean , y =symmetry_mean ))

#########################################
## #Higher alcohol level tend to give better white wine quality

library(hrbrthemes)
ggplot(data = data, mapping = aes(x = radius_mean, colour = as.factor(diagnosis))) +
  geom_freqpoly(binwidth = 0.5)

ggplot(data=data, aes(x=perimeter_mean, group=as.factor(diagnosis), fill=as.factor(diagnosis))) +
  geom_density(adjust=1.5) +
  theme_ipsum()  

## Does median differ by ...?
plotmeans(radius_mean ~diagnosis, data=data) 

######################################### 

ggplot(data = data,mapping = aes(x = radius_mean,y =symmetry_mean,
                                     colour = as.factor(diagnosis),size=3)) + 
  geom_point(mapping = aes(x =radius_mean , y =symmetry_mean ))

#' checked distributions
#' checked outliers
#' checked correlations
#' density, residual sugar, alcohol, total and free sulfat dioxides

