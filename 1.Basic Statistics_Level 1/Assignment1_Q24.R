#A Government  company claims that an average light bulb lasts 270 days. A researcher randomly selects 18 bulbs for testing. 
#The sampled bulbs last an average of 260 days, with a standard deviation of 90 days. 
#If the CEO's claim were true, what is the probability that 18 randomly selected bulbs would have an average life of no more than 260 days

x=260  #Sample mean 
??= 270 #Population mean
s= 90  #Sample standard deviation 
n=18
df= 18-1 = 17  #Degrees of freedom

#t = [ x - ?? ] / [ s / sqrt( n ) ]
t = ( 270 - 260 ) / ( 90 / sqrt( 18) )
t= -(10)/21.2132   #t_score
pt(-0.4714045,17)
