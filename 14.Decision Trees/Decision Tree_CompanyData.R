install.packages("party")
install.packages("caret")
install.packages("party")
install.packages("C50")
install.packages("rpart")
install.packages("rattle")
install.packages("gmodels")
install.packages("randomForest")
install.packages("tree",repos = "http://cran.us.r-project.org")
# Load CART packages
install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
library(rpart)
# install rpart package
install.packages("rpart.plot")
library(rpart.plot)
library(caret)
library(party)
library(C50)
library(rpart)
library(rattle)
library(gmodels)
install.packages("ROSE")
library(ROSE)

data <- read.csv(file.choose())
View(data)
str(data)
summary(data)

hist(data$Sales)
sort(data$Sales)
length(data$Sales)
mean(data$Sales)
sort(data$Sales)[400/3*2] #sales may be high,medium,low
#converting sales to categorical type
sales_cat<- ifelse(data$Sales>7.49,"high","low")
df <- data.frame(sales_cat,data[,-1])
View(df)

#splitting data to train and test data
set.seed(100)
CD = data.frame(data,sales_cat )
# View(CD)
CD_train <- CD[1:200,]
# View(CD_train)
CD_test <- CD[201:400,]

#building model
#Using Party Function 

#op_tree using party package

op_tree = ctree(as.factor(sales_cat)  ~ CompPrice + Income + Advertising + Population + Price
                + Age + Education, data = CD_train)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred1<- predict(op_tree,newdata=CD_test)
mean(pred1==CD$sales_cat) # Accuracy = 57.25 %                
CrossTable(CD_test$sales_cat,pred1)

#model2 using c50 package
model2 <- C5.0(train[,-1],as.factor(train$sales_cat),trails=100)
pred2 <- predict.C5.0(model2,test)
table(pred2,test$sales_cat)
mean(pred2==test$sales_cat)                #74.0% acc
plot(model2)
C5imp(model2)
CrossTable(test$sales_cat,pred2)
#model3 bagging method

acc <- c()
for(i in 1:100){
  print(i)
  splitting <- createDataPartition(sales_cat,p=0.85,list = F)
  training <- df[splitting,]
  testing <- df[-splitting,]
  
  modelfit <- C5.0(training[,-1],as.factor(training$sales_cat))
  predictfit <- predict(modelfit,testing)
  a <- table(predictfit,testing$sales_cat)
  acc <- c(acc,sum(diag(a))/sum(a))
}
acc
summary(acc)


#model using rpart package
model4 <- rpart(sales_cat~.,data = train)
plot(model4)
text(model4,pretty = 0)
fancyRpartPlot(model4,cex=0.5,type = 2)
text(model4)
pred4 <- predict(model4,test)
table(pred4,test$sales_cat)
CrossTable(test$sales_cat,pred4)
set.seed(100)

zz<-prune(model4,cp=0.23,best=10,minsplit=2,)
fancyRpartPlot(zz,cex=0.5,type = 2)
plot(zz)

install.packages("caretEnsemble")
library(caretEnsemble)

control_stacking <- trainControl(method="repeatedcv", number=2, repeats=2, savePredictions=TRUE, classProbs=TRUE)

algorithms_to_use <- c('rpart', 'knn')
stacked_models <- caretList(sales_cat~.,data = train, trControl=control_stacking, methodList=algorithms_to_use)

stacking_results <- resamples(stacked_models)

summary(stacking_results)
xyplot(resamples(stacked_models))

update.packages("rpart")
library(rpart.plot)

# Pruning

library(DMwR)
install.packages("fancyrpartplot")

 install.packages("tree")
 zp<-prune(model4,cp=0.1)
 
 
 fancyRpartPlot(zp, uniform=TRUE, main="Pruned Classification Tree")
 printcp(zp)

 
 