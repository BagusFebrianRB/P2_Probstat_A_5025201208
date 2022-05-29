#1
install.packages("BSDA")
install.packages("mosaic")
library(BSDA)
library(mosaic)

#a
Responden = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
n = 9

DataFrame = data.frame(Responden, x, y)

stddev = sd(DataFrame$x-DataFrame$y)
stddev

#b
mu = 0
xbar = mean(DataFrame$x-DataFrame$y)
tstastistik =( (xbar - mu) / (stddev / sqrt(n)))
pvalue = 2 * pt(-abs(tstastistik), df=n-1)
pvalue

#c
t.test(x=data$oksigen.sesudah, y=data$oksigen.sebelum,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

#2
stddev = 3900
xbar = 23500
mu = 20000
n = 100
stastistik =( (xbar - mu) / (stddev / sqrt(n)))
pvalue = pnorm(-abs(stastistik))
pvalue