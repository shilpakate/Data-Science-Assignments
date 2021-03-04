# Load delivery_time.csv dataset
library(readr)
delivery_time <- read.csv("E:/Data Science Asignments/Simple regression/delivery_time.csv")

View(delivery_time)

# Exploratory data analysis
summary(delivery_time)

var(delivery_time$Delivery.Time)
sd(delivery_time$Delivery.Time)
var(delivery_time$Sorting.Time)
sd(delivery_time$Sorting.Time)

#Scatter plot
plot(delivery_time$Sorting.Time, delivery_time$Delivery.Time)  # plot(X,Y)

?plot

attach(delivery_time)
#Correlation Coefficient (r)
cor(Sorting.Time,Delivery.Time)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Delivery.Time ~Sorting.Time) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$Sorting.Time
sum(reg$Sorting.Time)

mean(reg$Sorting.Time)
sqrt(sum(reg$Sorting.Time^2)/nrow(Delivery.Time))  #RMSE

sqrt(mean(reg$Sorting.Time^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = delivery_time, aes(x = Sorting.Time, y = Delivery.Time)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = delivery_time, aes(x=Sorting.Time, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(delivery_time,aes(Sorting.Time,Delivery.Time))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logarithmic Model

# x = log(Sorting.Time); y = Delivery.Time

plot(log(Sorting.Time), Delivery.Time)
cor(log(Sorting.Time), Delivery.Time)

reg_log <- lm(Delivery.Time ~ log(Sorting.Time))   # lm(Y ~ log(X))

summary(reg_log)
predict(reg_log)

reg_log$Sorting.Time
sqrt(sum(reg_log$Sorting.Time^2)/nrow(delivery_time))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Sorting.Time and y = log(Delivery.Time)

plot(Sorting.Time, log(Delivery.Time))

cor(Sorting.Time, log(Delivery.Time))

reg_exp <- lm(log(Delivery.Time) ~ Sorting.Time)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error =delivery_time $Delivery.Time - at
error

sqrt(sum(error^2)/nrow(delivery_time))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Sorting.Time,Delivery.Time)
plot(Sorting.Time*Calories.Consumed, Delivery.Time)

cor(Sorting.Time*Calories.Consumed, Delivery.Time)

plot(Sorting.Time*Sorting.Time, log(Delivery.Time))

cor(Sorting.Time, logDelivery.Time)
cor(Sorting.Time*Sorting.Time, log(Delivery.Time.))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Delivery.Time) ~ Sorting.Time + I(Delivery.Time*Delivery.Time))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time$Delivery.Time - expy

sqrt(sum(err^2)/nrow(delivery_time))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = delivery_time, aes(x = Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = delivery_time, aes(x=Sorting.Time+I(Sorting.Time^2), y=logpol))


##############################

