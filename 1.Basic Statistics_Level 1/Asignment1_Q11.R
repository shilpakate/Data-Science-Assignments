#Q11)  Suppose we want to estimate the average weight of an adult male in    Mexico. We draw a random sample of 2,000 men from a population of 3,000,000 men and weigh them.
#We find that the average person in our sample weighs 200 pounds, and the standard deviation of the sample is 30 pounds. 
#Calculate 94%,98%,96% confidence interval ?
a <- 200   #sample mean
s <- 30 #sd
n <- 2000 # sample size  
#conf interval =94%
#Step 1: Subtract the confidence level (Given as 94 percent in the question) from 1 and then divide the result by two. This is your alpha level, which represents the area in one tail.
#alpha level 
(1-0.94)/2  = 0.03
#Step 2: Plug the numbers into the second part of the formula and solve:  z* ?? / (???n)

error <- qnorm(0.03)*s/sqrt(n)
error
left <- a-error
right <- a+error
left
right

#for 98% 
   #at alpha level  
   (1-0.98)/2  = 0.01

error <- qnorm(0.01)*s/sqrt(n)
error
left <- a-error
right <- a+error
left
right
#for 96% 
#at alpha level  
(1-0.96)/2  = 0.02

error <- qnorm(0.02)*s/sqrt(n)
error
left <- a-error
right <- a+error
left
right