#Q12)  Below are the scores obtained by a student in tests 
#34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56
#1)	Find mean, median, variance, standard deviation.
x<-c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)

result.mean<-mean(x)
result.mean              #MEAN 
result.median<-median(x)
result.median            #MEDIAN

result.variance<-var(x)
result.variance          #VARIANCE
result.sd<- sd(x)
result.sd               #STANDARD DEVIATION

# Draw Boxplot
boxplot(x)