
<!-- README.md is generated from README.Rmd. Please edit that file -->
DACF
====

The goal of DACF is to implement methods to deal with challenges associated with ceiling/floor effects in the data using paramtric methods that assume normality for the true scores.

Installation
------------

You can install DACF from github with:

``` r
# install.packages("devtools")
devtools::install_github("QMmmmLiu/DACFD")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(DACF)
# Simulate healthy data for two groups
x.1=rnorm(300,2,4)
x.2=rnorm(300,3,5)
# check mean and variance for simulated healthy data
mean(x.1);var(x.1)
#> [1] 2.046914
#> [1] 12.34239
mean(x.2);var(x.2)
#> [1] 3.054255
#> [1] 19.49582
# induce ceiling effects of 20% in group 1
x.1.cf=induce.cfe(.2,0,x.1)
# induce floor effects of 10% in group 2
x.2.cf=induce.cfe(0,.1,x.2)
# recover the mean and variance for ceiling/floor data
rec.mean.var(x.1.cf)
#> $ceiling.percentage
#> [1] 0.003333333
#> 
#> $floor.percentage
#> [1] 0.24
#> 
#> $est.mean
#> [1] 2.141253
#> 
#> $est.var
#> [1] 11.79842
rec.mean.var(x.2.cf)
#> $ceiling.percentage
#> [1] 0.1
#> 
#> $floor.percentage
#> [1] 0.003333333
#> 
#> $est.mean
#> [1] 2.959067
#> 
#> $est.var
#> [1] 18.01588
# conduct a t test on healthy data
t.test(x.1,x.2)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  x.1 and x.2
#> t = -3.0922, df = 569.26, p-value = 0.002084
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -1.6472027 -0.3674792
#> sample estimates:
#> mean of x mean of y 
#>  2.046914  3.054255
t.test(x.1.cf,x.2.cf)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  x.1.cf and x.2.cf
#> t = -1.1283, df = 550.93, p-value = 0.2597
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.8717900  0.2356737
#> sample estimates:
#> mean of x mean of y 
#>  2.473312  2.791370
# conduct an adjusted t test on ceiling/floor data
lw.t.test(x.1.cf,x.2.cf,"a")
#> $statistic
#> [1] -2.37371
#> 
#> $p.value
#> [1] 0.0183786
#> 
#> $est.d
#> [1] -0.2099299
#> 
#> $conf.int
#> [1] -1.4964171 -0.1392114
lw.t.test(x.1.cf,x.2.cf,"b")
#> $statistic
#> [1] -2.594197
#> 
#> $p.value
#> [1] 0.009970491
#> 
#> $est.d
#> [1] -0.2118153
#> 
#> $conf.int
#> [1] -1.4383181 -0.1973104
# generate a dataframe for ANOVA demo
testdat=threeganova.sim(10000,.0625,1)
# induce ceiling/floor effects in the data
testdat.cf=testdat
testdat.cf[testdat.cf$group==2,]$y=induce.cfe(.2,0,testdat.cf[testdat.cf$group==2,]$y)
# conduct an adjusted F star test on ceiling/floor data
lw.f.star(testdat.cf,y~group,"a")
#> $statistic
#> [1] 868.0733
#> 
#> $p.value
#> [1] 0
#> 
#> $est.f.squared
#> [1] 0.05787155
lw.f.star(testdat.cf,y~group,"b")
#> $statistic
#> [1] 781.9596
#> 
#> $p.value
#> [1] 0
#> 
#> $est.f.squared
#> [1] 0.05591017
```
