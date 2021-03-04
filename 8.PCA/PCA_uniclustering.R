install.packages("psych")
install.packages("NbClust")
install.packages("corrplot")
library(corrplot)
library(psych)
wine_data<-read.csv("D:\\Assignment__Rstudio\\PCA\\wine.csv") ## use read.csv for csv files
View(wine_data)
help(princomp) ## to understand the api for princomp

# wine_data[-1] -> Considering only numerical values for applying PCA
wine <- wine_data[,-1]
attach(wine)

corrplot(cor(wine),method = "number")
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wine_score<-cbind(wine_data,pcaObj$scores[,1:3])
View(wine_score)

#cluster analysis
############### hierarchial clustering for principle components scores ###############
install.packages("factoextra")
library(factoextra)
library(NbClust)
library(dendextend)
# preparing data for clustering (considering only pca scores as they represent the entire data)
clust_data <-wine_score[,8:10] 
norm_clust <- scale(clust_data)
clust <- eclust(norm_clust,"hclust",k=3,graph = FALSE)
fviz_dend(clust,rect = TRUE)
groups <- cutree(clust,k=3)
finalhclust <- data.frame(groups,wine)


aggregate(finalhclust,by=list(clust$cluster),FUN=mean)

#**************kmeans clustering for principle components scores*************************
wss <- (nrow(norm_clust)-1)*sum(apply(norm_clust,2,var))
for(i in 1:10){
  wss[i]=sum(kmeans(norm_clust,centers = i)$withinss)
}
plot(1:10,wss, type = "o") #from scree plot no of clusters is 3
#alternative method
noofclust <- NbClust(clust_data,distance = "euclidean",method = "kmeans",min.nc = 2,max.nc = 10,index = "all")
fviz_nbclust(noofclust)                                  #optimal number of clusters
km <- kmeans(norm_clust,3)
fviz_cluster(km,data = wine[-1])
final <- data.frame(km$cluster,wine)                    


aggregate(final,by=list(km$cluster),FUN = mean)

