# Load Emp_data.csv dataset
library(readr)
Emp_data <- read.csv("E:/Data Science Asignments/Simple regression/Emp_data.csv")

View(Emp_data)

# Exploratory data analysis
summary(Emp_data)
# x=  Salary_hike   y= Churn_out_rate 

var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike)

#Scatter plot
plot(Emp_data$Salary_hike, Emp_data$Churn_out_rate)  # plot(X,Y)

?plot

attach(Emp_data)
#Correlation Coefficient (r)
cor(Salary_hike,Churn_out_rate)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~Salary_hike) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$Salary_hike
sum(reg$Salary_hike)

mean(reg$Salary_hike)
sqrt(sum(reg$Salary_hike^2)/nrow(Churn_out_rate))  #RMSE

sqrt(mean(reg$Salary_hike^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = Emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Emp_data, aes(x=Salary_hike, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(Emp_data,aes(Salary_hike,Churn_out_rate))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logarithmic Model

# x = log(Salary_hike); y = Churn_out_rate

plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ log(X))

summary(reg_log)
predict(reg_log)

reg_log$Salary_hike
sqrt(sum(reg_log$Salary_hike^2)/nrow(Emp_data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Salary_hike and y = log(Churn_out_rate)

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))

reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error =Emp_data $Churn_out_rate - at
error

sqrt(sum(error^2)/nrow(Emp_data))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike,Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate)

plot(Salary_hike*Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = Emp_data$Churn_out_rate - expy

sqrt(sum(err^2)/nrow(Emp_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = Emp_data, aes(x = Salary_hike + I(Salary_hike^2), y = log(Churn_out_rate))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=logpol))


##############################

