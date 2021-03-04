#The Salary data set is being used for Support vector machine modelling by using the factors like age, occupation, workclas, marital status etc and predict the salary classification.
library(kernlab)
library(caret)

library(corrplot)
library(ggplot2)
library(kernlab)
library(plyr)
library(ggplot2)
library(psych)

# Data(Train)
doParallel::registerDoParallel(cores = 2)
train <- read.csv(file.choose())
View(train)
test <- read.csv(file.choose())
View(test)
str(test)
salary <- rbind(train,test)
View(salary)
table(salary$Salary)
plot(salary)

#creating dummies
level_work <- levels(salary$workclass)
leveledu <- levels(salary$education)
level_mari <- levels(salary$maritalstatus)
level_occ <- levels(salary$occupation)
level_rel <- levels(salary$relationship)
level_race <- levels(salary$race)



level_native <- levels(salary$native)
level_salary <- levels(salary$Salary)
salary$workclass <- as.integer(as.factor(salary$workclass))
salary$education <- as.integer(factor(salary$education))
salary$maritalstatus <- as.integer(factor(salary$maritalstatus))
salary$occupation <- as.integer(factor(salary$occupation))
salary$relationship <- as.integer(factor(salary$relationship))
salary$race <- as.integer(factor(salary$race))
salary$sex <- as.integer(factor(salary$sex))
salary$native <- as.integer(factor(salary$native))
salary$Salary <- as.integer(factor(salary$Salary))
corrplot(cor(salary),method = c("square"),type = "upper")
# normalising df
 normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
norm_salary <- normal(salary)

View(norm_salary)

#splitting of data to test and train
train_norm <- norm_salary[1:30161,]
View(train_norm)
test_norm <- norm_salary[30162:45221,]

#model building



####Model 1 : rbfdot 

modelrbfdot <- ksvm(train_norm $Salary~.,data=train_norm,kernel="rbfdot")
predrbfdot <- predict(modelrbfdot,newdata=test_norm)
cor(predrbfdot,test_norm$Salary)     

######Model 2 : polydot

modelpolydot <- ksvm(train_norm$Salary~.,data=train_norm,kernel="polydot")
predpolydot <- predict(modelpolydot,newdata=test_norm)
cor(predpolydot,test_norm$Salary)     


#####Model 3: vanilladot

modelvanilladot <- ksvm(train_norm$Salary~.,data=train_norm,kernel="vanilladot")
 predvanilladot <- predict(modelvanilladot,newdata=test_norm)
cor(predvanilladot,test_norm$Salary)   
#using bagging method

kernels <- c("rbfdot","vanilladot","polydot")
acc_bag <- list()
pred_info <- list()
table_info <- list()
for(i in kernels){
  model_bag <- ksvm(Salary~.,data=train_norm,kernel=i)
  pred_bag <- predict(model_bag,test_norm)
  pred_info[[i]] <- (pred_bag)
  acc_bag[[i]] <- cor(pred_bag,test_norm$Salary)
  table_info[[i]] <- table(pred_bag,test_norm$Salary)
}

acc_bag

table_info
plot(1:3,acc_bag)
plot(pred_info$rbfdot)
plot(pred_info$vanilladot)
plot(pred_info$polydot)

table_info$rbfdot





