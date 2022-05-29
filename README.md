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

