##Q7) Calculate Mean, Median, Mode, Variance, Standard Deviation, Range &     comment about the values / draw inferences, for the given dataset
#For Points,Score,Weigh
#Find Mean, Median, Mode, Variance, Standard Deviation, and Range and also Comment about the values/ Draw some inferences.

df <- read.csv(file.choose())
#weigh
df[,4]
##MEAN
weigh.mean<-mean(df[,4])
weigh.mean
##median
weigh.median<-median(df[,4])
weigh.median
##mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
weigh.mode<-mode(df[,4])
print(weigh.mode)
##Varience
var(df[,4])
##Standard Deviation
weigh.Standard_Deviation<-sd(df[,4])
weigh.Standard_Deviation
##Range
weigh.Range=(max(df[,4])-min(df[,4]))
weigh.Range




