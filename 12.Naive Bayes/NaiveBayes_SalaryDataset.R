#1) Prepare a classification model using Naive Bayes  for salary data 
install.packages("e1071")
install.packages("corrplot")
install.packages("gmodels")
install.packages("naivebayes")
install.packages("ggplot2")
install.packages("psych")
install.packages('tidyverse')
install.packages("caret")
install.packages("ROSE")
library(dplyr) # for data manipulation
library(caret) # for model-building
install.packages("DMwR") # for smote implementation
install.packages("purrr") # for functional programming (map)
install.packages("pROC") # for AUC calculations

library(e1071)
library(corrplot)
library(gmodels)
library(naivebayes)
library(ggplot2)
library(psych)
install.packages("dplyr")
library(dplyr)
# Data(Train)
library(sos)
findFn("select")
install.packages("dplyr")

# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


train_sal$workclass <- as.factor(train_sal$workclass)
train_sal$Salary <- as.factor(train_sal$Salary)

plot(train_sal$workclass,train_sal$Salary)
plot(as.factor(train_sal$education),as.factor(train_sal$Salary))

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(as.factor(train_sal$native),as.factor(train_sal$Salary))
#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Age - Density Plot")
ggplot(data=train_sal,aes(x = train_sal$occupation, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("occupation Density Plot")

# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model
Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)
library("caret")


# Crosstable
table(salary_test$Salary,Model_pred)
CrossTable(salary_test$Salary,Model_pred)

barplot(table(salary_test$Salary), main = " salary in test")
barplot(table(salary_train$Salary) ,main = "salary in train")
# Confusion Matrix
confusionMatrix(Model_pred,as.factor(test_sal$Salary))
## Data Imbalancing


#check table
table(train_sal$native)
prop.table(table(train_sal$native))

#Balanced sampled naive Bayes
nsets <- 10 



#over sampling
library(imbalance)
head(test_sal, 10)
plotComparison(train_sal, rbind(train_sal, newMWMOTE), attrs = names(train_sal)[1:3])
data_balanced_over <- ovun.sample(train_sal$Salary ~ ., data = train_sal, method = "over",N = 1960)$data
 table(data_balanced_over$Salary)
data_balanced_under <- ovun.sample(train_sal$Salary ~ ., data = train_sal, method = "under", N = 40, seed = 1)$data
table(data_balanced_under$cls)
data_balanced_both <- ovun.sample(train_sal$Salary ~ ., data = train_sal, method = "both", p=0.5,                             N=1000, seed = 1)$data
table(data_balanced_both$cls)
data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data
 table(data.rose$cls)