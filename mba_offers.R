#load in mba_offers.cvs and place data into a vector
offer = read.csv(file=file.choose(),header=TRUE)   
finance <- offer$Finance
market <- offer$Marketing

#make a histogram of the data
#define plotting region
par(mfrow=c(1,2)) 
hist(finance,main="Offers for Finance Majors")
hist(market,main="Offers for Marketing Majors")

#check Finance data for normality
#with Q-Q plot
qqnorm(finance,main="Q-Q Plot for Finance")
qqline(finance)
#with Shapiro-Wilk test (null hypothesis is that data is normal)
shapiro.test(finance)
#with Kolmogorov-Smirnov test
#ks.test(finance, 'pnorm')

#check Marketing data for normality
#with Q-Q plot
qqnorm(market,main="Q-Q Plot for Marketing")
qqline(market)
#with Shapiro-Wilk test (null hypothesis is that data is normal)
shapiro.test(market)
#with Kolmogorov-Smirnov test
#ks.test(market, 'pnorm')

#reset plot region
dev.off()

#F-test for variances
var.test(finance, market, alt='two.sided')

#calculate two-sample t-test
#for equal variances
t.test(finance, market, var.equal=TRUE)
#for unequal variance
t.test(finance, market, var.equal=FALSE)

#calculate confidence interval for difference of means
n1 <- length(finance)
n2 <- length(market)
s1 <- sd(finance)
s2 <- sd(market)
#for pooled variance with equal variances
sp.squared <- ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
se <- sqrt(sp.squared*(1/n1 + 1/n2))
conf.lev <- 0.95
df <- n1+n2-2
t.score <- qt((1-conf.lev)/2,df,lower.tail=FALSE)
me <- t.score*se
xbar1 <- mean(finance)
xbar2 <- mean(market)
lcl <- (xbar1-xbar2)-me
ucl <- (xbar1-xbar2)+me
point <- formatC(xbar1-xbar2, digits=2, format="f")
uncert <- formatC(me, digits=2, format="f")
lclslim <- formatC(lcl, digits=2, format="f")
uclslim <- formatC(ucl, digits=2, format="f")
glue("Confidence interval: {point} ± {uncert}, ({lclslim}, {uclslim})")
#for unequal variances
se <- sqrt(s1^2/n1 + s2^2/n2)
conf.lev <- 0.95
df <- (s1^2/n1 + s2^2/n2)^2/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
t.score <- qt((1-conf.lev)/2,df,lower.tail=FALSE)
me <- t.score*se
xbar1 <- mean(finance)
xbar2 <- mean(market)
lcl <- (xbar1-xbar2)-me
ucl <- (xbar1-xbar2)+me
point <- formatC(xbar1-xbar2, digits=2, format="f")
uncert <- formatC(me, digits=2, format="f")
lclslim <- formatC(lcl, digits=2, format="f")
uclslim <- formatC(ucl, digits=2, format="f")
glue("Confidence interval: {point} ± {uncert}, ({lclslim}, {uclslim})")
