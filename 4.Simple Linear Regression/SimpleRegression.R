Calories_consumed <- read.csv("E:/Data Science Asignments/Simple regression/calories_consumed.csv")
summary(Calories_consumed)
var(Calories_consumed$Calories.Consumed)
sd(Calories_consumed$Calories.Consumed)
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)
#Creating Linear Model for weight gain
WeightGainModel <- lm(Weight.gained..grams. ~ Calories.Consumed, data = Calories_consumed)
summary(WeightGainModel)
plot(Calories_consumed)


#*****************************************************************
delivery_time <- read.csv("E:/Data Science Asignments/Simple regression/delivery_time.csv")
summary(delivery_time)
# Variance and Standard deviation of Delivery.Time column
var(delivery_time$Delivery.Time)
sd(delivery_time$Delivery.Time)
# Variance and Standard deviation of Sorting.Time column
var(delivery_time$Sorting.Time)
sd(delivery_time$Sorting.Time)
#Creating Linear Model for delivery time
deliverTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time)
summary(deliverTimeModel)
plot(deliverTimeModel)    

#For Increasing R squared value Using mvinfluence in Linear Model to find the point which are creating problems
install.packages("mvinfluence")
library(mvinfluence)

influenceIndexPlot(deliverTimeModel)
deliverTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time[c(-5,-9,-21),])
summary(deliverTimeModel) 

plot(deliverTimeModel)
#************************************************
#*- Emp_data -> Build a prediction model for Churn_out_rate
Emp_data <- read.csv("E:/Data Science Asignments/Simple regression/emp_data.csv")
#Getting Summary of Import Data
summary(Emp_data)
# Variance and Standard deviation of Salary_hike column
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike)
# Variance and Standard deviation of Churn_out_rate column
var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)
#Creating Linear Model for Churn_out_rate
Churn_out_rate_Model <- lm(Churn_out_rate ~ Salary_hike, data = Emp_data)
summary(Churn_out_rate_Model)
plot(Churn_out_rate_Model)
#***************************************************************
#*4 - Salary_hike -> Build a prediction model for Salary_hike
Salary_hike <- read.csv("E:/Data Science Asignments/Simple regression/Salary_Data.csv") 
#Getting Summary of Import Data
summary(Salary_hike)
# Variance and Standard deviation of Salary_hike column
var(Salary_hike$YearsExperience)
sd(Salary_hike$YearsExperience)
# Variance and Standard deviation of Churn_out_rate column
var(Salary_hike$Salary)
sd(Salary_hike$Salary)
#Creating Linear Model for Salary_hike
Salary_hike_Model <- lm(Salary ~ YearsExperience, data = Salary_hike)
summary(Salary_hike_Model)
plot(Salary_hike_Model)
  