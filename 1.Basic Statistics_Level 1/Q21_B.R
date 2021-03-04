##b. Check Whether the Adipose Tissue (AT) and Waist Circumference(Waist)  from wc-at data set 
#follows Normal Distribution 
#  Dataset: wc-at.csv
wc<-read.csv(file.choose())
names(wc)
wc
##Q-Q Plot of waist

qqnorm(wc$Waist, pch = 1, frame = FALSE)
qqline(wc$Waist, col = "Blue")

##Q-Q Plot of AT
qqnorm(wc$AT, pch = 1, frame = FALSE)
qqline(wc$AT, col = "Blue")