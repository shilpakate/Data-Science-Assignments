# Load Calories_consumed.csv dataset
library(readr)
Calories_consumed <- read.csv("E:/Data Science Asignments/Simple regression/calories_consumed.csv")
View(Calories_consumed)

# Exploratory data analysis
summary(Calories_consumed)

var(Calories_consumed$Calories.Consumed)
sd(Calories_consumed$Calories.Consumed)
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)

#Scatter plot
plot(Calories_consumed$Calories.Consumed, Calories_consumed$Weight.gained..grams.)  # plot(X,Y)

?plot

attach(Calories_consumed)


#Correlation Coefficient (r)
cor(Calories.Consumed,Weight.gained..grams.)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Weight.gained..grams. ~ Calories.Consumed) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$Calories.Consumed
sum(reg$Calories.Consumed)

mean(reg$Calories.Consumed)
sqrt(sum(reg$Calories.Consumed^2)/nrow(Weight.gained..grams.))  #RMSE

sqrt(mean(reg$Calories.Consumed^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = Calories_consumed, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Calories_consumed, aes(x=Calories.Consumed, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(Calories_consumed,aes(Calories.Consumed,Weight.gained..grams.))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logarithmic Model

# x = log(Calories.Consumed); y = Weight.gained..grams.

plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.)

reg_log <- lm(Weight.gained..grams. ~ log(Calories.Consumed))   # lm(Y ~ log(X))

summary(reg_log)
predict(reg_log)

reg_log$Calories.Consumed
sqrt(sum(reg_log$Calories.Consumed^2)/nrow(Calories_consumed))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Calories.Consumed and y = log(Weight.gained..grams.)

plot(Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))

reg_exp <- lm(log(Weight.gained..grams.) ~ Calories.Consumed)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error =Calories_consumed $Weight.gained..grams. - at
error

sqrt(sum(error^2)/nrow(Calories_consumed))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Calories.Consumed,Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

cor(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

plot(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = Calories_consumed$Weight.gained..grams. - expy

sqrt(sum(err^2)/nrow(Calories_consumed))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = Calories_consumed, aes(x = Calories.Consumed + I(Calories.Consumed^2), y = log(Weight.gained..grams.))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Calories_consumed, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=logpol))


##############################

