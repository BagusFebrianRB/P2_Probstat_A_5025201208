# P2_Probstat_A_5025201208

# 1
a. mencari standar deviasi dengan `sd`
```r
Responden = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
y = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
n = 9

DataFrame = data.frame(Responden, x, y)

stddev = sd(DataFrame$x-DataFrame$y)
stddev
```

b. mencari nilai p-value
```r
mu = 0
xbar = mean(DataFrame$x-DataFrame$y)
tstastistik =( (xbar - mu) / (stddev / sqrt(n)))
pvalue = 2 * pt(-abs(tstastistik), df=n-1)
pvalue
```

# 2
```r
stddev = 3900
mean = 23500
mh0 = 20000
n = 100
tsum.test(mean.x = mean, s.x = stddev ,n.x = n, mu = mh0, alternative = "greater", var.equal =  TRUE)
2*pnorm(-abs((mean - mh0)/(stddev/sqrt(n)))
```

a. Karena confidence level berada di angka 95% pada interval 22852.45 sampai tak terhingga maka mean asli diatas 20000

b. p-value 9.437e-15, selain itu output menunjukkan interval confidence 95% diatas x = 22852.45

c. Karena p value dari tsum test lebih besar, maka kesimpulan yang diambil adalah mean asli lebih besar dari pada 20000

# 3
a. H0 = mean.bali=mean.bandung dan H1 = mean.bali != mean.bandung
b. 
```r
tsum.test(n.x = nBandung, mean.x = meanBandung, s.x = stdBandung, n.y = nBandung, mean.y = meanBandung ,s.y = stdBandung, var.equal = TRUE, alternative = "greater")
```
menghitung mean sampel

c. 
```r
plotDist(dist = 't', df = 2)
```
menghitung plot distribution dengan soal df =2

d. 
```r
qchisq(p = 0.05, df = 2, lower.tail = FALSE)
```
mendapat nilai kritis dengan qchisq

e. karena nilai kritis yang didapat lebih besar daripada hasil p-score pada tsum.test maka bisa tolak hipotesis bahwa bandung dan bali memiliki mean yang sama

# 4
a. 
```r
group1 <- c(19,18.6,18.3,18,18.2,18.6,18.5,18.2,18.4,18.9,19.9,18.5,16.9,18,17.3,17.8,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.7,19.7,19.3,19,19.4,19.8,19.3,18.5)
group2 <- c(18.3,17.9,17.6,17.3,17.5,17.9,17.8,17,17.7,18.2,19.2,17.8,16.2,17.3,16.6,17.1,19.3,18.3,18.5,18,16.8,17.2,17.3,17.4,16.7,17.2,16.7,16.2,19,18.6,18.3,18.7,19.1,18.6,17.8)
group3 <- c(18,18.6,18.3,18,18.2,18.2,18.5,18.2,19.2,18.5,19.9,18.5,16.9,18,17,17.2,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.5,19.7,19,19,19.7,19.8,19.3,17)
```

b. 
```r
lengths <- c(19,18.6,18.3,18,18.2,18.6,18.5,18.2,18.4,18.9,19.9,18.5,16.9,18,17.3,17.8,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.7,19.7,19.3,19,19.4,19.8,19.3,18.5,18.3,17.9,17.6,17.3,17.5,17.9,17.8,17,17.7,18.2,19.2,17.8,16.2,17.3,16.6,17.1,19.3,18.3,18.5,18,16.8,17.2,17.3,17.4,16.7,17.2,16.7,16.2,19,18.6,18.3,18.7,19.1,18.6,17.8,18,18.6,18.3,18,18.2,18.2,18.5,18.2,19.2,18.5,19.9,18.5,16.9,18,17,17.2,20,19,19.2,18.9,17.5,18.1,18,18.1,17.4,17.9,17.4,16.5,19.7,19,19,19.7,19.8,19.3,17)
groups <- c(1	,1,1,1	,1,1,1	,1	,1	,1	,1,1,1,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,1	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,2	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3	,3)
dataframe <- data.frame(groups, lengths)

bartlett.test(lengths~groups, data= dataframe)
```

c. 
```r
ggplot(data = dataframe, mapping = aes(groups,lengths))+ geom_point() + geom_smooth(method = 'lm')

model1 <- aov(lengths~factor(groups), data = dataframe)
anova(model1)
```
uji anova

d. 
```r
n4anova = aov(model1)
TukeyHSD(model1, conf.level = .95)
```
dari hasil yang didapat bisa disimpulkan bahwa terdapat perbedaan panjang yang cukup signifikan di antara grup 1 dengan grup 3
