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

#3
significantlevel = 0.05
nBandung = 19
meanBandung = 3.64
stdBandung = 1.67
nBali = 27
meanBali = 2.79
stdBali = 1.32

tsum.test(n.x = nBandung, mean.x = meanBandung, s.x = stdBandung, n.y = nBandung, mean.y = meanBandung ,s.y = stdBandung, var.equal = TRUE, alternative = "greater")

plotDist(dist = 't', df = 2)

qchisq(p = 0.05, df = 2, lower.tail = FALSE)

#4
group1 <- c(19,18.6,18.3,18,18.2,18.6,18.5,18.2,18.4,18.9,19.9,18.5,16.9,18,17.3,17.8,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.7,19.7,19.3,19,19.4,19.8,19.3,18.5)
group2 <- c(18.3,17.9,17.6,17.3,17.5,17.9,17.8,17,17.7,18.2,19.2,17.8,16.2,17.3,16.6,17.1,19.3,18.3,18.5,18,16.8,17.2,17.3,17.4,16.7,17.2,16.7,16.2,19,18.6,18.3,18.7,19.1,18.6,17.8)
group3 <- c(18,18.6,18.3,18,18.2,18.2,18.5,18.2,19.2,18.5,19.9,18.5,16.9,18,17,17.2,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.5,19.7,19,19,19.7,19.8,19.3,17)

lengths <- c(19,18.6,18.3,18,18.2,18.6,18.5,18.2,18.4,18.9,19.9,18.5,16.9,18,17.3,17.8,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.7,19.7,19.3,19,19.4,19.8,19.3,18.5,18.3,17.9,17.6,17.3,17.5,17.9,17.8,17,17.7,18.2,19.2,17.8,16.2,17.3,16.6,17.1,19.3,18.3,18.5,18,16.8,17.2,17.3,17.4,16.7,17.2,16.7,16.2,19,18.6,18.3,18.7,19.1,18.6,17.8,18,18.6,18.3,18,18.2,18.2,18.5,18.2,19.2,18.5,19.9,18.5,16.9,18,17,17.2,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.5,19.7,19,19,19.7,19.8,19.3,17)
groups <- c(1	,1,1,1	,1,1,1	,1	,1	,1	,1,1,1,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3)
dataframe <- data.frame(n4_groups, n4_lengths)

bartlett.test(lengths~groups, data= dataframe)

ggplot(data = dataframe, mapping = aes(groups,lengths))+ geom_point() + geom_smooth(method = 'lm')

model1 <- aov(lengths~factor(groups), data = dataframe)
anova(model1)

n4anova = aov(model1)
TukeyHSD(model1, conf.level = .95)
