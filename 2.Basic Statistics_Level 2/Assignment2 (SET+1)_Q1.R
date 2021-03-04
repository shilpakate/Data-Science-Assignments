#Look at the data given below. Plot the data, find the outliers and find out  ??,??,??^2  
x<-c(24.23,25.53,25.41,24.14,29.62,28.25,25.81,24.39,40.26,32.95,91.36,25.99,39.42,26.71,35)
# for outlier draw box plot
boxplot(x)
OutVals = boxplot(x)$out
OutVals
  
result.mean<-mean(x)/100
result.mean          ##MEAN VALUE

result.sd<-sd(x)/100 ##Standard deviation 
result.sd

variance <- result.sd *result.sd  #Variance 
variance



0 * 0.2 + 1000 * 0.2 + 2000 * 0.3 + 3000* 0.1/6000