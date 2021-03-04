#Q 23) Calculate the t scores of 95% confidence interval, 
#96% confidence interval, 99% confidence interval for sample size of 25

# T scores -->   sample size =25
df= 25-1
#  for 95%   p= (1-0.95)/2 + 0.95
qt(0.975,24)
# for 96%   --> p= (1-0.96)/2 + 0.96
qt(0.98,24)   
#  for 99%  -->  p= (1-0.99)/2 + 0.99
qt(0.995,24) 