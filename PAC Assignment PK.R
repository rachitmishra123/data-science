Library (readxls)
library(gdata)
wine_scaled_data<-scale(wine[,-1])
colnames(wine_scaled_data)

head(wine_scaled_data)

wine_pca<-princomp(wine_scaled_data,cor = TRUE,scores=TRUE,covmat = NULL)
summary(wine_pca)
im(wine_pca)

wine_pca$scores
wine_pca$loadings

plot(wine_pca$scores[1:3])

#Attaching the first 3 scores to the data
wine_newdata<-cbind(wine_scaled_data,wine_pca$scores[,1:3])
colnames(wine_newdata)

#Run Cluster analysis on 1st 3 PC's

d<-dist(wine_newdata[,14:16],method="euclidean")
fit<-hclust(d,method="average")
plot(fit)

# draw dendogram with red borders around 4 clusters

rect.hclust(fit, k=4, border = "red")

# K-means cluster

wss<-c()

for (i in 2:15) wss[i]<-sum(kmeans(wine_newdata[,14:16],centers = i)$withinss)
plot(1:15,wss,type="b")
# using K-means clusters, we can see 3 clusters

wine_c1 <- kmeans(wine_newdata[,14:16],3)
wine_c1$cluster
wine_c1$centers


library(animation)
windows()
wine_c1<-kmeans.ani(wine_newdata[,14:16],3)










