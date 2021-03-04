crime <- read.csv(file.choose())
View(crime)
scale_crime <- scale(crime[,-1])  
View(scale_crime)
install.packages("animation")
library(cluster)

#finding number of clusters
wss=(nrow(scale_crime)-1)*sum(apply(scale_crime,2,var))
for(i in 2:8) wss[i]=sum(kmeans(scale_crime,centers=i)$withinss)
plot(1:8,wss,type = "b",xlab = "clusters",ylab = "with in sum of squares",main = "K-means clustering")
#*****************2 is the number of clusters *******************

#to find optimal no of clusters
install.packages("factoextra")
library(factoextra)
fviz_nbclust(scale_crime,method = 'wss',FUNcluster = kmeans)
fviz_nbclust(scale_crime,method = 'silhouette',FUNcluster = kmeans)
fviz_nbclust(scale_crime,method = 'gap_stat',FUNcluster = kmeans)

# from all the elbow plots it is clear that optimal number of clusters is 2
library(animation)
final <- kmeans(scale_crime,2)
finalanim <- kmeans.ani(scale_crime,2)
fviz_cluster(final,data = crime[-1])
finaldata <- data.frame(final$cluster,crime)
View(finaldata)
aggregate(crime[,-1],by=list(final$cluster),FUN = mean)


#*******  Hclustering*************

#load libraries
library(data.table)
library(ggplot2)
library(fpc)

setDT(crime)
head(crime)
#lets check missing values
colSums(is.na(crime))
#some of the variables have missing values. Let's impute the missing values with median.
#scale the variables
scaled_cm<-scale(crime[,-1])

#we'll first calculate a distance matrix based on Euclidean measure. Then using the hclust function, we can implement hierarchical clustering.
#Hierarchical Clustering
d <- dist(scaled_cm,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward") #clustering
plot(h_clust,labels = crime$V1) #dendrogram
# how can you estimate the number of clusters? Going by the logic of horizontal cut, four clusters are evident. Let's see!

rect.hclust(h_clust,k=4)
#To look at which observation went into which cluster, you can write:

#extract clusters
groups <- cutree(h_clust,k=4)
groups

#o implement PCA, we'll use princomp base function. For our convenience, we'll take only the first two components.

#pca
pcmp <- princomp(scaled_cm)
pred_pc <- predict(pcmp, newdata=scaled_cm)[,1:2]
#Now, we'll create a data frame having pc values and their corresponding clusters. Then, using ggplot2 we'll create the plot.

comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = crime$V1)
ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)







