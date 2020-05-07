#Q2
qt(1 - 0.02/2, 25-1)

#Q3, Q4, Q5, Q6
data = read.csv('INVQUAD.csv')
head(data)
electric = data[data$PLANT == 'Electric',2]
gas = data[data$PLANT == 'Gas',2]

var.test(electric, gas)


#Q8 
t.test(electric, gas, var.equal = T, conf.level = 0.95)

#Q9
t.test(electric, gas, var.equal = T, conf.level = 0.95, mu = -4)

#Q10
data2 = read.csv('KWHRS.csv')

sizeX = data2[,1]
usageY = data2[,3]

lm10 = lm(usageY ~ sizeX)
summary(lm10)

#Q11
library(s20x)
ciReg(lm10)

#Q12 , MSS/TSS = mult r2
summary(lm10)

#Q13
quadr = lm(usageY~sizeX + I(sizeX^2))
summary(quadr)

#Q20 
beta = 0.7
constant = 2*(25^0.5)/10

zbeta = 1 - qnorm(0.7,0,1)
alpha = (pnorm(zbeta, 0, 1) - 1)*2
alpha
