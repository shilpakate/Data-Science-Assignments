
#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#book rating data
book_rate_data1 <- read.csv("book.csv")
View(book_rate_data1)
book_rate_data <- book_rate_data1[-c(1,2)]
View(book_rate_data)
#metadata about the variable
str(book_rate_data)



#rating distribution
hist(book_rate_data$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(book_rate_data, 'realRatingMatrix')
#Normalize the ratings matrix
#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, book_rate_data_matrix[413:414], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same books for all users , we need to improve our model using
# # Collaborative Filtering

#1.User Based Collaborative Filtering  Creation of the model  U(ser) B(ased) C(ollaborative) F(iltering)

book_recomm_model2 <- Recommender(book_rate_data_matrix, method="UBCF")


##*******Predictions for two users **********
recommended_items2 <- predict(book_recomm_model2, book_rate_data_matrix[413:414], n=5)
as(recommended_items2, "list")
Rec.model=Recommender(book_rate_data_matrix[413:414],method="UBCF", 
                      param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))


#2.Matrix factorization with LIBMF*************

book_recomm_model3 <- Recommender(book_rate_data_matrix, method="LIBMF")
##********Predictions for two users 
book_recomm_model3 <- predict(book_recomm_model3, book_rate_data_matrix[413:414], n=5)

###****3.	RANDOM recommendations
book_recomm_model4 <- Recommender(book_rate_data_matrix, method="RANDOM")
##***Predictions for two users ***********
 recommended_items4 <- predict(book_recomm_model4, book_rate_data_matrix[413:414], n=5)
as(recommended_items4, "list")


