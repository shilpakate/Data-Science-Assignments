#Setting up Enviromnent for Clustering

list.of.packages <- c("datasets", "ggplot2","cluster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(cluster)
library(ggplot2)
library(caret)
#Importing Dataset
getwd()

library(openxlsx)
Airlines <- read.xlsx(file.choose(),2)
View(Airlines)
a <-Airlines[,-1]
View(a)
#************K-means clustering**************************
scale_a <- scale(a)
View(scale_a)
wss=(nrow(scale_a)-1)*sum(apply(scale_a,2,var))
for(i in 2:8)wss[i]=sum(kmeans(scale_a,centers = i)$withinss)
plot(1:8,wss,type = "o",xlab = "number of clusters",ylab = "within sum of squares")
title("scree plot")

twss <- NULL
for(i in 2:8){
   twss <- c(twss,kmeans(scale_a,i)$tot.withinss)
}
twss
plot(2:8,twss,type = "o")
#from elbow curve it is clear that k = 3 or 5
km1 <- kmeans(scale_a,3)
str(km1)        #twss=31001 ,bss=12977
km2 <- kmeans(scale_a,5)
str(km2)       #twss=26308,bss=17670

#the best cluster have less totwss and high betweenss
#hence choosing number of clusters as 5
library(animation)
km <- kmeans(scale_a,5)
kmanimation <- kmeans.ani(scale_a,5)
kmfinal <- data.frame(km$cluster,Airlines)
View(kmfinal)
aggregate(kmfinal[,-c(1,2)],by=list(km$cluster),FUN = mean)


#*******  Hclustering*************

      #load libraries
      library(data.table)
   library(ggplot2)
   library(fpc)
   
   setDT(Airlines)
   head(Airlines)
   #lets check missing values
   colSums(is.na(Airlines))
   #some of the variables have missing values. Let's impute the missing values with median.
   #scale the variables
   scaled_wd <-scale(Airlines[,-1])

   #we'll first calculate a distance matrix based on Euclidean measure. Then using the hclust function, we can implement hierarchical clustering.
   #Hierarchical Clustering
   d <- dist(scaled_wd,method = "euclidean") #distance matrix
   h_clust <- hclust(d, method = "ward") #clustering
   plot(h_clust,labels = Airlines$V1) #dendrogram
   # how can you estimate the number of clusters? Going by the logic of horizontal cut, four clusters are evident. Let's see!
   
   rect.hclust(h_clust,k=4)
   #To look at which observation went into which cluster, you can write:
   
   #extract clusters
   groups <- cutree(h_clust,k=4)
   groups
   
   #o implement PCA, we'll use princomp base function. For our convenience, we'll take only the first two components.
   
   #pca
   pcmp <- princomp(scaled_wd)
   pred_pc <- predict(pcmp, newdata=scaled_wd)[,1:2]
   #Now, we'll create a data frame having pc values and their corresponding clusters. Then, using ggplot2 we'll create the plot.
   
   comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = water_data$V1)
   ggplot(comp_dt,aes(Comp.1,Comp.2))+
      geom_point(aes(color = cluster),size=3)
   
   
   
   
   
   














