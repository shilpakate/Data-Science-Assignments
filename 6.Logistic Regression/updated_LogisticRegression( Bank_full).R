
install.packages("caret")
bankdata<-read.csv("D:\\Assignment__Rstudio\\Multilinear\\bank-full.csv",sep = ';')
library(caret)
set.seed(3456)
# Indicate the classification variable
bankdata$y=factor(bankdata$y)

# Use glm function, with y as dependent variable and all others as independent variables (IVs), written
#  in the usual manner with '+' sign.
logitModel=glm(y~poutcome+previous+pdays+campaign+duration+month+day+contact+loan+housing+balance+default+education+marital+job+age, data=bankdata,family=binomial(logit))
# As all variables besides, y, are IVs, we could have used a shorter form (with dot), as:
logitModel=glm(y~., data=bankdata,family=binomial(logit))
# Get model summary
summary(logitModel)

# Confusion matrix table 
prob <- predict(logitModel,type=c("response"),bankdata)
View(prob)
confusion<-table(prob>0.5,bankdata$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy#70.52
1-Accuracy
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)
?diag

# ROC Curve 
#install.packages("ROCR")
#install.packages("ROCR", dependencies = T)
library(ROCR)
rocrpred<-prediction(prob,bankdata$y)
eval <- performance(rocrpred,"acc")
plot(eval)


max <- which.max(slot(eval,"y.values")[[1]])
acc <- slot(eval,"y.values")[[1]][max]
cut <- slot(eval,"x.values")[[1]][max]
cut
print(c(Accuracy=acc, Cutoff=cut))

#Area under the curve"
rocrperf<-performance(rocrpred,'tpr','fpr')
#plot(rocrperf,colorize=T,text.adj=c(0.5,NA))
plot(rocrperf,colorize=T,main="ROC Curve",xlab="1-specificity",ylab="Sensitivity")
?plot
abline(a=0,b=1)
auc <- performance(rocrpred,"auc")
auc <- unlist(slot(auc,"y.values"))
auc <- round(auc,4)
auc


##Alternative
pred_values <- NULL
pred_values
yes_no <- NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

bankdata[,"prob"] <- prob
View(prob)
View(bankdata)
bankdata[,"pred_values"] <- pred_values
View(pred_values)
View(bankdata)
bankdata[,"yes_no"] <- yes_no
View(bankdata)

View(bankdata[,c(1,2,3, 4, 5, 6,7,8,9)])

# Accuracy 
acc <- table(bankdata$y,pred_values)
acc
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 70.62

#****************
install.packages("randomForest")
library(randomForest)
  bankdata.rf <- randomForest(y ~ ., bankdata, keep.forest=FALSE)
plot(margin(bankdata.rf))

#
install.packages("Boruta")
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(y ~ ., data=na.omit(bankdata), doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")]) 
print(boruta_signif) 
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance


#******************Blue boxplots correspond to minimal, average and maximum Z score of a shadow attribute. Red, yellow and green boxplots represent Z scores of rejected, tentative and confirmed attributes respectively.
finalModel=glm(y~prob+duration+month+age, data=bankdata,family=binomial(logit))

summary(finalModel)

# Confusion matrix table 
prob <- predict(finalModel,type=c("response"),bankdata)
View(prob)
confusion<-table(prob>0.5,bankdata$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy#70.52
1-Accuracy
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)
?diag
# ROC Curve 
#install.packages("ROCR")
#install.packages("ROCR", dependencies = T)
library(ROCR)
rocrpred<-prediction(prob,bankdata$y)
eval <- performance(rocrpred,"acc")
plot(eval)


max <- which.max(slot(eval,"y.values")[[1]])
acc <- slot(eval,"y.values")[[1]][max]
cut <- slot(eval,"x.values")[[1]][max]
cut
print(c(Accuracy=acc, Cutoff=cut))

#Area under the curve"
rocrperf<-performance(rocrpred,'tpr','fpr')
#plot(rocrperf,colorize=T,text.adj=c(0.5,NA))
plot(rocrperf,colorize=T,main="ROC Curve",xlab="1-specificity",ylab="Sensitivity")
?plot
abline(a=0,b=1)
auc <- performance(rocrpred,"auc")
auc <- unlist(slot(auc,"y.values"))
auc <- round(auc,4)
auc
