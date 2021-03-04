#Calculate Skewness, Kurtosis & draw inferences on the following data
#SP and Weight(WT)  Use Q9_b.csv

df <- read.csv(file.choose())
names(df)
print(df)
##**************For SP**********************
SP=df['SP']
library(moments)
skewness(SP)
kurtosis(SP)
##***************For Weight*******************
Weight=df['WT']
skewness(Weight)
kurtosis(Weight)
##Draw Histogram
hist(df$SP)
hist(df$WT)