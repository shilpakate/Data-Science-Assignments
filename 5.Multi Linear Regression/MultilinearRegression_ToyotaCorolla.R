#Multi Linear Regression
#Predict Toyota Corolla price
# load and look at your data!
install.packages("e1071")
install.packages("car")
install.packages("pbkrtest")
library(e1071)
library(car)
Data <- read.csv("E:/Data Science Asignments/Multilinear Regression/ToyotaCorolla.csv")

Corolla<-Data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(Corolla)

# First Moment Business Decision
summary(Corolla)
# Second Moment Business Decision
sd(Price)
sd(Age_08_04)
sd(KM)
sd(HP)
sd(cc)
var(HP)
var(cc)

var(Doors)

var(Gears)

var(Quarterly_Tax)

var(Weight)

skewness(Price)

skewness(Age_08_04)

skewness(KM)

skewness(HP)

skewness(cc)

skewness(Doors)

skewness(Gears)

skewness(Quarterly_Tax)

skewness(Weight)

kurtosis(Price)

kurtosis(Age_08_04)

kurtosis(KM)

kurtosis(HP)

kurtosis(cc)

kurtosis(Doors)

kurtosis(Gears)
kurtosis(Quarterly_Tax)

kurtosis(Weight)

plot(Age_08_04, Price) ## Newr the Car more expensive it is.
plot(KM, Price) ## The more miles a car has the cheaper it is
plot(HP, Price) ## More horsepower the more expensive.

plot(cc, Price) ##
plot(Doors, Price)
plot(Gears, Price)
plot(Quarterly_Tax, Price)
plot(Weight, Price)
# Cars of Fuels types 
summary(Data$Fuel_Type)
#summary(Data$Model)
summary(Data$Color)
# Find Correlation between input and output
pairs(Corolla)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Corolla)
##Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(Corolla))
## Building linear regression model
model <- lm(Price ~ ., data = Corolla)
summary(model)

# cc and Doors are influence to each other, predict the model based on individual records
model.carcc <- lm(Price ~ cc)
summary(model.carcc) # Its significat to output

model.cardoor <- lm(Price ~ Doors)
summary(model.cardoor) # It's also significatnt
## Build model with cc and Doors
model.car <- lm(Price ~ cc + Doors)
summary(model.car) # Both are significant to each other

# Find out the influencial record
influence.measures(model.car)


    

