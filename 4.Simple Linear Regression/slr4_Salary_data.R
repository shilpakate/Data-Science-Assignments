# Load Salary_hike.csv dataset
library(readr)
Salary_hike <- read.csv("E:/Data Science Asignments/Simple regression/Salary_data.csv")

View(Salary_hike)

# Exploratory data analysis
summary(Salary_hike)
# x=  YearsExperience   y= Salary 

var(Salary_hike$Salary)
sd(Salary_hike$Salary)
var(Salary_hike$YearsExperience)
sd(Salary_hike$YearsExperience)

#Scatter plot
plot(Salary_hike$YearsExperience, Salary_hike$Salary)  # plot(X,Y)

?plot

attach(Salary_hike)
#Correlation Coefficient (r)
cor(YearsExperience,Salary)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Salary ~YearsExperience) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$YearsExperience
sum(reg$YearsExperience)

mean(reg$YearsExperience)
sqrt(sum(reg$YearsExperience^2)/nrow(Salary))  #RMSE

sqrt(mean(reg$YearsExperience^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = Salary_hike, aes(x = YearsExperience, y = Salary)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Salary_hike, aes(x=YearsExperience, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(Salary_hike,aes(YearsExperience,Salary))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logarithmic Model

# x = log(YearsExperience); y = Salary

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ log(X))

summary(reg_log)
predict(reg_log)

reg_log$YearsExperience
sqrt(sum(reg_log$YearsExperience^2)/nrow(Salary_hike))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = YearsExperience and y = log(Salary)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error =Salary_hike $Salary - at
error

sqrt(sum(error^2)/nrow(Salary_hike))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(YearsExperience,Salary)
plot(YearsExperience*YearsExperience, Salary)

cor(YearsExperience*YearsExperience, Salary)

plot(YearsExperience*YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = Salary_hike$Salary - expy

sqrt(sum(err^2)/nrow(Salary_hike))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = Salary_hike, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Salary_hike, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))


##############################

