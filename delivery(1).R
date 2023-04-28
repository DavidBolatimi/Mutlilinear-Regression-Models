#read in delivery.csv file
library(readr)
library(tidyverse)
library(ggpubr)
library(glue)
theme_set(theme_pubr())
data = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\apt_sales.csv',header=TRUE)   

#HW2 NO4



#correlation analysis
#make an initial scatter plot of Time vs Dist
ggplot(data, aes(x = Dist, y = Time)) +
  geom_point() #+stat_smooth()
#calculate correlation coefficient
print(r <- cor(data$Time, data$Dist))

#run linear model for time (in miles) as a function of distance (in hours)
model <- lm(cost ~ sqft + dist + age, data = data)
model

model2 <- lm(cost ~ sqft + age, data = data)
model2

anova(model, model2)

if data$Model == "Corolla" {
  Corolla = 1 And Civic = 0
}
if data$Model == "Civic" {
  Corolla = 0 And Civic = 1
}

#replot data with best-fit line
ggplot(data, aes(Dist, Time)) + 
  geom_point() + stat_smooth(method = lm)

#create an idealized model with normal residuals
df <- data
set.seed(123)
x <- rnorm(90, mean=mean(data$Time), sd=sd(data$Time))
df[,2]<-model$coefficients[1]+model$coefficients[2]*df[,1]+x
ggplot(df, aes(Dist, Time)) + geom_point() + stat_smooth(method = lm)
modelnorm <- lm(Time ~ Dist, data = df)
resnorm <- resid(modelnorm)
#check residuals for normality
summary(resnorm)
hist(resnorm, xlab="Residuals")
qqnorm(resnorm)
qqline(resnorm)
#create density plot of residuals
plot(density(resnorm))
#run shapiro.wilk test on residuals
shapiro.test(resnorm)
#create residuals vs fitted plot to check for homoscedasticity
plot(fitted(modelnorm), resnorm, ylab="Residuals for Normalized Data", xlab="Predicted Time")
abline(0,0)

#use residuals to check for required conditions for error variable
res <- resid(model)
#check residuals for normality
summary(res)
hist(res, xlab="Residuals")
qqnorm(res)
qqline(res)
#create density plot of residuals
plot(density(res))
#run shapiro.wilk test on residuals
shapiro.test(res)
#create residual vs fitted plot to check for heteroscedasticity
plot(fitted(model), res, ylab="Residuals", xlab="Predicted Time")
abline(0,0)
plot(model)

#use output to interpret...
summary(model)
#whether model is statistically significant using F-test
anova(model)
#how much of the variation in the data is explained by the model with R^2
print(Rsquared <- cor(data$Time, data$Dist)^2)
#calculate the percentage error
sigma(model)*100/mean(data$Time)
100 - Rsquared*100
#hypothesis test coefficient of x for statistical significance
#calculate the confidence intervals for the coefficient
confint(model)     #confint(model, level=0.99)
#report estimate for coefficient of x
point <- formatC(summary(model)$coefficients[2], digits=4, format="f")
uncert <- formatC(confint(model)[4]-summary(model)$coefficients[2], digits=4, format="f")
glue("Estimate of Beta1: {point} ± {uncert}")
#replot data with best-fit line
ggplot(data, aes(Dist, Time)) + geom_point() + stat_smooth(method = lm)

#predict the prediction interval for a single run
drive <- 30
attach(data)
newdata = data.frame(Dist=drive)
predict(model, newdata, interval="prediction", level=0.95)
predval1 <- predict(model, newdata, interval="prediction", level=0.95)
point1 <- formatC(predval1[1], digits=3, format="f")
uncert1 <- formatC(predval1[3] - predval1[1], digits=3, format="f")
glue("Estimate of time for one {drive} mile run (in hrs): {point1} ± {uncert1}")

#predict the confidence interval for the average of a series of runs
predict(model, newdata, interval="confidence", level=0.95)
predval2 <- predict(model, newdata, interval="confidence", level=0.95) 
point2 <- formatC(predval2[1], digits=3, format="f") 
uncert2 <- formatC(predval2[3] - predval2[1], digits=3, format="f") 
glue("Estimate of time for the average of {drive} mile runs (in hrs): {point2} ± {uncert2}")
