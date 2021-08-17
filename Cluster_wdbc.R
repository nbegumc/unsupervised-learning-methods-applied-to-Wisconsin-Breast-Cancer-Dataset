
# k-means approach

wbdc=wdbc

summary(wbdc)

# K-Means
set.seed(123)
k.means.fit <- kmeans(wbdc, 2,nstart = 100) # k = 2
print(k.means.fit)
table(k.means.fit$cluster)


#cluster means
aggregate(wbdc, by=list(cluster=k.means.fit$cluster), mean)

#visualisation
library(factoextra)
fviz_cluster(k.means.fit, data = wbdc, 
             palette = c("#2E9FDF", "#E7B800"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = FALSE, 
             ggtheme = theme_minimal()
)


# how many clusters ?
## for K-means

summary(wdbc)
wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wbdc, nc=10) 

library(factoextra)
fviz_nbclust(wbdc, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")


fviz_cluster(kmeans(wbdc, centers = 2), geom = "point", data = wbdc)

# Elbow method
fviz_nbclust(wdbc, kmeans, method ="wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(wdbc, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#############################################
# hierarchical clustering 

#As part of the preparation for hierarchical clustering, the distance between all pairs
# of observations are computed. Furthermore, there are different ways to link clusters 
#together, with single, complete, and average being the most common linkage methods.

summary(wbdc)

#agglomerative clustering
d <- dist(scores, method = "euclidean") # Euclidean distance matrix.
d_matrix=as.matrix(d)
d_matrix
#complete linkage
H.fit <- hclust(d, method="complete")
fviz_dend(H.fit, cex = 0.5)
# Average linkage
H.fit.avg=hclust(d, method="average")
fviz_dend(H.fit.avg)
# Ward linkage
H.fit.ward=hclust(d, method="ward.D2")
fviz_dend(H.fit.ward)

# Cut tree into 2/4 groups
grp <- cutree(H.fit.ward, k = 2)
head(grp, n = 2)
# Number of members in each cluster
table(grp,diagnosis)
table(k.means.fit$cluster, grp)

fviz_dend(H.fit.ward, k = 2, 
          cex = 0.5,
          k_colors = c("#2E9FDF", "yellow","red","pink"),
          color_labels_by_k = TRUE,
          rect = TRUE 
)
fviz_cluster(list(data = wbdc, cluster = grp),
             palette = c("#2E9FDF", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", 
             repel = FALSE,
             show.clust.cent = FALSE, ggtheme = theme_minimal())




##########################################################
## validation
km2 = data.frame(wbdc, k.means.fit$cluster,data$diagnosis)
head(km2)

km2$data.diagnosis <- as.factor(mapvalues(km2$data.diagnosis,
                                              from=c("B", "M"),
                                              to=c("2","1")))

mean(km2$k.means.fit.cluster == km2$data.diagnosis)



hc2=data.frame(wbdc, grp,data$diagnosis)
hc2$data.diagnosis <- as.factor(mapvalues(hc2$data.diagnosis,
                                          from=c("B", "M"),
                                          to=c("2","1")))

mean(hc2$grp == hc2$data.diagnosis)
##########################################################


