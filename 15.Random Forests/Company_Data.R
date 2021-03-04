library(randomForest)
library(caret)
install.packages("e1071")
library(psych)
install.packages("smotefamily")
install.packages("DMwR")
library(DMwR)
##### Company data using Smote********************** 
data = read.csv(file.choose())
Data = data

median(Data$Sales)
Data$Sales <-factor(ifelse(Data$Sales<=7.49,'Low_sales','High_sales'))
summary(Data)
Data$ShelveLoc<- factor(Data$ShelveLoc)

Data$Urban<-factor(Data$Urban)
Data$US<-factor(Data$US)
library(DMwR)
?SMOTE()
x = data.frame(SMOTE(Sales~.,data=Data),perc.over = 600, perc.under =  80)
View(x)
table(x$Sales)

rand.forest <- randomForest(Sales ~., data=x, mtry = 7,
                            importance = TRUE, proximity=TRUE, ntree = 1000
)

rand.forest


############## splitting data to train and test ##########################
set.seed(100)
cutt <- createDataPartition(Data$Sales,p=0.7,list=F)
train_comp <-Data[cutt,]
test_comp <- Data[-cutt,]

############# model building ##############################
##Balancing by class-weight during training.
companyforest <- randomForest(Sales~.,ntree=500,mtry=3,data = train_comp,importnce=T)
companyforest

# prediction and accuracy based on train data
pred_train <- predict(companyforest,train_comp)
mean(pred_train==train_comp$Sales)                    
confusionMatrix(table(pred_train,train_comp$Sales))

# prediction and accuracy based on test data
pred_test <- predict(companyforest,test_comp)
mean(pred_test==test_comp$Sales)                       
confusionMatrix(table(pred_test,test_comp$Sales))

# visualisation 
plot(companyforest)
legend("topright",col = 2:11,colnames(companyforest$err.rate),fill = 2:11,cex = 0.5)

# variable importance
importance(companyforest)
varImpPlot(companyforest)
# price is the most significant variable

# bagging 
a <- c()
for(i in 3:10){
  set.seed(100)
  bag <- createDataPartition(Data$Sales,p=0.8,list = F)
  train_bag <- Data[bag,]
  test_bag <- Data[-bag,]
  bag_model <- randomForest(as.factor(Sales)~.,data = train_bag,mtry=i,importance=TRUE)
  pred_bag <- predict(bag_model,test_bag,type='class')
  a[i-2] <- mean(pred_bag==test_bag$Sales)
}
a
plot(3:10,a,xlab = "mtry",ylab = "acc")
# we get highest accuracy for mtry = 3
#****************

# choosing mtry as 9 with less OOB error


finalmodel <- randomForest(as.factor(Sales)~.,data = train_comp,mtry=9,importance=TRUE)
finalmodel
mean(predict(finalmodel,test_comp)==test_comp$Sales)