#Calculate the probability of MPG  of Cars for the below cases.
#MPG <- Cars$MPG
#a.	P(MPG>38)
#b.	P(MPG<40)
#c.    P (20<MPG<50)

#####Find probability
df<-read.csv(file.choose())
df
names(df)
mpg<-df[,2]
##p(mpg>38)
pnorm(38, mean(mpg), sd(mpg), lower.tail=FALSE)
## p(mpg<40)
pnorm(40, mean(mpg), sd(mpg))
##P(20<MPG<50)
pnorm(20, mean(mpg), sd(mpg))-pnorm(50, mean(mpg), sd(mpg))
 




