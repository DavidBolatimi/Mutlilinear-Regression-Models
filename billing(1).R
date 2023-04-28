#load in billing.cvs and place data into a vector
library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
bill = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\billing.csv',header=TRUE)   
data <- bill$Total.Bill

#make a histogram of the data
hist(data)

#check for normality
#with Q-Q plot
qqnorm(data)
qqline(data)
#with Shapiro-Wilk test (null hypothesis is that data is normal)
 
#with Kolmogorov-Smirnov test
#ks.test(data, 'pnorm')

#calculate one sample t-test
t.test(data,mu=170,alt='greater')  #for one-sided t-test
#remove alt for two-sided

#calculate one sample chi-square test for variance
library(EnvStats)
varTest(data, alt='two.sided', conf.level=0.95,sigma.squared = 3600)

#calculate confidence interval
#use mean and standard deviation to calculate standard error
xbar <- mean(data)
s <- sd(data)
n <- length(data)
se <- s/sqrt(n)
conf.lev <- 0.95
#for one-sided critical value
t.score <- qt(1-conf.lev,n-1,lower.tail=FALSE)
#for two-sided critical value
#t.score <- qt((1-conf.lev)/2,n-1,lower.tail=FALSE)
me <- t.score*se
lcl <- xbar-me
ucl <- xbar+me
point <- formatC(xbar, digits=1, format="f")
uncert <- formatC(me, digits=1, format="f")
lclslim <- formatC(lcl, digits=1, format="f")
uclslim <- formatC(ucl, digits=1, format="f")
glue("Confidence interval for mean: {point} ± {uncert}, ({lclslim}, {uclslim})")
#for standard deviation
chilo <- qchisq((1-conf.lev)/2,n-1,lower.tail = TRUE)
chihi <- qchisq((1-conf.lev)/2,n-1,lower.tail = FALSE)
sdslim <- formatC(s, digits=1, format="f")
sdlo <- formatC((sqrt((n-1)*s^2/chihi)), digits=1, format="f")
sdhi <- formatC((sqrt((n-1)*s^2/chilo)), digits=1, format="f")
glue("Confidence interval for sd:  {sdslim}, ({sdlo}, {sdhi})")
